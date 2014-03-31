#include <algorithm>

#include <wx/datstrm.h>
#include <wx/gauge.h>
#include <wx/progdlg.h>

#include <lk_absyn.h>
#include <lk_stdlib.h>
#include <lk_eval.h>

#include <ssc/sscapi.h>

#include "simulation.h"
#include "main.h"
#include "equations.h"
#include "case.h"


static bool VarValueToSSC( VarValue *vv, ssc_data_t pdata, const wxString &sscname )
{
	switch( vv->Type() )
	{
	case VV_NUMBER:
		ssc_data_set_number( pdata, sscname.c_str(), (ssc_number_t)vv->Value() );
		break;
	case VV_ARRAY:
	{
		size_t n;
		float *p = vv->Array( &n );
		if ( sizeof(ssc_number_t) == sizeof( float ) )
			ssc_data_set_array( pdata, sscname.c_str(), p, n );
		else
		{
			ssc_number_t *pp = new ssc_number_t[n];
			for( size_t i=0;i<n;i++ )
				pp[i] = p[i];

			ssc_data_set_array( pdata, sscname.c_str(), pp, n );

			delete [] pp;
		}
	}
		break;
	case VV_MATRIX:
	{
		matrix_t<float> &fl = vv->Matrix();
		if ( sizeof(ssc_number_t) == sizeof(float) )
		{
			ssc_data_set_matrix( pdata, sscname.c_str(), fl.data(), fl.nrows(), fl.ncols() );
		}
		else
		{
			ssc_number_t *pp = new ssc_number_t[ fl.nrows() * fl.ncols() ];
			size_t n = 0;
			for( size_t r = 0; r < fl.nrows(); r++ )
				for( size_t c=0;c<fl.ncols();c++)
					pp[n++] = (ssc_number_t)fl(r,c);

			ssc_data_set_matrix( pdata, sscname.c_str(), pp, fl.nrows(), fl.ncols() );
			delete [] pp;
		}
	}
		break;
	case VV_STRING:
		ssc_data_set_string( pdata, sscname.c_str(), vv->String().c_str() );
		break;
	case VV_TABLE:
	{
		ssc_data_t tab = ssc_data_create();
		VarTable &vt = vv->Table();
		for( VarTable::iterator it = vt.begin();
			it != vt.end();
			++it )
		{
			VarValueToSSC( it->second, tab, it->first );
		}

		ssc_data_set_table( pdata, sscname.c_str(), tab );

		ssc_data_free( tab ); // ssc_data_set_table above makes a deep copy, so free this here
	}
		break;


	case VV_INVALID:
	default:
		return false;
	}

	return true;
}


Simulation::Simulation( Case *cc, const wxString &name )
	: m_case( cc ), m_name( name )
{
}


static void write_array_string( wxDataOutputStream &out, wxArrayString &list )
{
	out.Write32( list.size() );
	for( size_t i=0;i<list.size();i++ )
		out.WriteString( list[i] );
}

static void read_array_string( wxDataInputStream &in, wxArrayString &list )
{
	list.Clear();
	size_t n = in.Read32();
	for( size_t i=0;i<n;i++ )
		list.Add( in.ReadString() );
}

void Simulation::Write( wxOutputStream &os )
{
	wxDataOutputStream out( os );
	out.Write8( 0x9c );
	out.Write8( 1 ); // version

	out.WriteString( m_name );

	write_array_string( out, m_overrides );

	m_inputs.Write( os );
	m_outputs.Write( os );
	
	write_array_string( out, m_errors );
	write_array_string( out, m_warnings );

	m_outputLabels.Write( os );
	m_outputUnits.Write( os );

	out.Write8( 0x9c );
}

bool Simulation::Read( wxInputStream &is )
{
	Clear();
	wxDataInputStream in( is );

	wxUint8 code = in.Read8(); // code
	in.Read8(); // ver

	m_name = in.ReadString();

	read_array_string( in, m_overrides );

	m_inputs.Read( is );
	m_outputs.Read( is );

	read_array_string( in, m_errors );
	read_array_string( in, m_warnings );
		
	m_outputLabels.Read( is );
	m_outputUnits.Read( is );
	
	
	return ( code == in.Read8() );	
}

void Simulation::Copy( const Simulation &rh )
{
	Clear();
	m_name = rh.m_name;
	m_overrides = rh.m_overrides;
	m_inputs = rh.m_inputs;
	m_outputs = rh.m_outputs;
	m_errors = rh.m_errors;
	m_warnings = rh.m_warnings;
	m_outputLabels = rh.m_outputLabels;
	m_outputUnits = rh.m_outputUnits;
}

void Simulation::Clear()
{
	m_overrides.clear();
	m_inputs.clear();
	m_outputs.clear();
	m_errors.clear();
	m_warnings.clear();
	m_outputLabels.clear();
	m_outputUnits.clear();
}

void Simulation::Override( const wxString &name, const VarValue &val )
{
	if ( VarValue *vv = m_inputs.Create( name, val.Type() ) )
	{
		m_overrides.Add( name );
		vv->Copy( val );
	}
}


wxString Simulation::GetOverridesLabel( bool with_labels )
{
	wxString tag;
	for( size_t i=0;i<m_overrides.size();i++ )
	{
		if ( VarValue *vv = m_inputs.Get( m_overrides[i] ) )
		{
			wxString label = m_overrides[i];
			
			if ( with_labels )
				if ( VarInfo *vi = m_case->Variables().Lookup( m_overrides[i] ) )
					if ( !vi->Label.IsEmpty() )
						label = vi->Label;
			
			tag += label + "=" + vv->AsString();
			if ( i < m_overrides.size()-1 )
				tag += ";";
		}
	}

	return tag;
}

VarValue *Simulation::GetInput( const wxString &name )
{
	if ( VarValue *val = m_inputs.Get( name ) )
		return val;

	return m_case->Values().Get( name );
}

bool Simulation::Ok()
{
	return m_errors.size() == 0;
}

wxArrayString &Simulation::GetErrors()
{
	return m_errors;
}
wxArrayString &Simulation::GetWarnings()
{
	return m_warnings;
}

VarTable &Simulation::Outputs()
{
	return m_outputs;
}


wxArrayString Simulation::GetOutputs()
{
	return Outputs().ListAll();
}

VarValue *Simulation::GetOutput( const wxString &var )
{
	return Outputs().Get( var );
}

VarValue *Simulation::GetValue( const wxString &name )
{
	if ( VarValue *vv = Outputs().Get( name ) ) return vv;
	else return GetInput( name );
}

wxString Simulation::GetLabel( const wxString &var )
{
	return m_outputLabels[ var ];
}

wxString Simulation::GetUnits( const wxString &var )
{
	return m_outputUnits[ var ];
}

class SimulationContext
{
public:
	wxProgressDialog *progdlg;
	wxArrayString *errors;
	wxArrayString *warnings;
};

static ssc_bool_t ssc_invoke_handler( ssc_module_t p_mod, ssc_handler_t p_handler,
	int action_type, float f0, float f1, 
	const char *s0, const char *s1,
	void *user_data )
{
	SimulationContext *sc = (SimulationContext*) user_data;
	if (!sc) return 0;

	if (action_type == SSC_LOG)
	{		
		switch( (int)f0 )
		{
		case SSC_NOTICE:
		case SSC_WARNING:
			sc->warnings->Add( s0 );
			break;
		case SSC_ERROR:
			sc->errors->Add( s0 );
			break;
		}
		return sc->progdlg->WasCancelled() ? 0 : 1;
	}
	else if (action_type == SSC_UPDATE)
	{
		sc->progdlg->Update( (int)f0, s0 );
		return !sc->progdlg->WasCancelled();
	}
	else
		return 0;
}


bool Simulation::Invoke()
{
	
	ConfigInfo *cfg = m_case->GetConfiguration();
	if ( !cfg )
	{
		m_errors.Add("no valid configuration");
		return false;
	}
	
	m_outputLabels.clear();
	m_outputUnits.clear();

	// transfer all the values except for ones that have been 'overriden'

	for( VarTableBase::const_iterator it = m_case->Values().begin();
		it != m_case->Values().end();
		++it )
		if ( 0 == m_inputs.Get( it->first ) )
			m_inputs.Set( it->first, *it->second );

	// recalculate all the equations

	CaseEvaluator eval( m_case, m_inputs, m_case->Equations() );
	int n = eval.CalculateAll();

	if ( n < 0 )
	{
		wxArrayString &errs = eval.GetErrors();
		for( size_t i=0;i<errs.size();i++ )
			m_errors.Add( errs[i] );

		return false;
	}

	SimulationContext sc;
	sc.progdlg = new wxProgressDialog( "Simulation", "in progress", 100, 0, wxPD_APP_MODAL|wxPD_SMOOTH|wxPD_CAN_ABORT );
	sc.progdlg->Show();
	sc.errors = &m_errors;
	sc.warnings = &m_warnings;

	ssc_data_t p_data = ssc_data_create();


	for( size_t kk=0;kk<cfg->Simulations.size();kk++ )
	{
		ssc_module_t p_mod = ssc_module_create( cfg->Simulations[kk].c_str() );
		if ( !p_mod )
		{
			m_errors.Add( "could not create ssc module: " + cfg->Simulations[kk] );
			continue;
		}

		int pidx=0;
		while( const ssc_info_t p_inf = ssc_module_var_info( p_mod, pidx++ ) )
		{
			int var_type = ssc_info_var_type( p_inf );   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
			int data_type = ssc_info_data_type( p_inf ); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX		
			wxString name( ssc_info_name( p_inf ) ); // assumed to be non-null
			wxString reqd( ssc_info_required( p_inf ) );

			if ( var_type == SSC_INPUT )
			{
				// handle ssc variable names
				// that are explicit field accesses"shading:mxh"
				wxString field;
				int pos = name.Find( ':' );
				if ( pos != wxNOT_FOUND )
				{
					field = name.Mid(pos+1);
					name = name.Left(pos);
				}

				VarValue *vv = GetInput(name);
				if ( !vv && reqd == "*" )
				{
					int existing_type = ssc_data_query( p_data, ssc_info_name( p_inf ) );
					if ( existing_type == SSC_INVALID )
						m_errors.Add( "SSC requires input '" + name + "', but was not found in the SAM UI or from previous simulations" );
					else if ( existing_type != data_type )
						m_errors.Add( "SSC requires input '" + name + "', but variable from a previous simulation had an incompatible data type");
				}
				else if ( vv != 0 )
				{

					if ( !field.IsEmpty() )
					{
						if ( vv->Type() != VV_TABLE )
							m_errors.Add( "SSC variable has table:field specification, but '" + name + "' is not a table in SAM" );

						bool do_copy_var = false;
						if ( reqd.Left(1) == "?" )
						{
							// if the SSC variable is optional, check for the 'en_<field>' element in the table
							if ( VarValue *en_flag = vv->Table().Get( "en_" + field ) )
								if ( en_flag->Boolean() )
									do_copy_var = true;
						}
						else do_copy_var = true;
						
						if ( do_copy_var )
						{
							if ( VarValue *vv_field = vv->Table().Get( field ) )
							{
								if ( !VarValueToSSC( vv_field, p_data, name + ":" + field ) )
									m_errors.Add( "Error translating table:field variable from SAM UI to SSC for '" + name + "':" + field );
							}
						}
						
					}

					if ( !VarValueToSSC( vv, p_data, name ))
						m_errors.Add( "Error translating data from SAM UI to SSC for " + name );
					
				}
			}
		}

		wxString dbgfile( wxGetHomeDir() + "/ssc-" + cfg->Simulations[kk] + ".lk" );
		if( FILE *fp = fopen( dbgfile.c_str(), "w" ) )
		{
			ssc_number_t value;
			ssc_number_t *p;
			int len, nr, nc;
			pidx = 0;
			wxString str_value;
			double dbl_value;
			int dbgidx = 0;
			while( const ssc_info_t p_inf = ssc_module_var_info( p_mod, dbgidx++ ) )
			{
				const char *name = ::ssc_info_name( p_inf );
				const char *desc = ::ssc_info_label( p_inf );
				const char *units = ::ssc_info_units( p_inf );
				int type = ::ssc_data_query( p_data, name );
				switch( type )
				{
				case SSC_STRING:
					str_value = wxString::FromUTF8(::ssc_data_get_string( p_data, name ));
					str_value.Replace("\\", "/" );
					fprintf(fp, "var( '%s', '%s' );\n", name, str_value.c_str() );
					break;
				case SSC_NUMBER:
					::ssc_data_get_number( p_data, name, &value );
					dbl_value = (double)value;
					if ( dbl_value > DBL_MAX ) dbl_value = 1e38;
					fprintf(fp, "var( '%s', %lg );\n", name, dbl_value );
					break;
				case SSC_ARRAY:
					p = ::ssc_data_get_array( p_data, name, &len );
					fprintf(fp, "var( '%s', [", name);
					for ( int i=0;i<(len-1);i++ )
					{
						dbl_value = (double)p[i];
						if ( dbl_value > DBL_MAX ) dbl_value = 1e38;
						fprintf(fp, " %lg,", dbl_value );
					}
					dbl_value = (double)p[len-1];
					if ( dbl_value > DBL_MAX ) dbl_value = 1e38;
					fprintf(fp, " %lg ] );\n", dbl_value );
					break;
				case SSC_MATRIX:
					p = ::ssc_data_get_matrix( p_data, name, &nr, &nc );
					len = nr*nc;
					fprintf( fp, "var( '%s', \n[ [", name );					
					for (int k=0;k<(len-1);k++)
					{
						dbl_value = (double)p[k];
						if ( dbl_value > DBL_MAX ) dbl_value = 1e38;
						if ( (k+1)%nc == 0 ) 
							fprintf(fp, " %lg ], \n[", dbl_value);
						else
							fprintf(fp, " %lg,", dbl_value);
					}
					dbl_value = (double)p[len-1];
					if ( dbl_value > DBL_MAX ) dbl_value = 1e38;
					fprintf(fp, " %lg ] ] );\n", dbl_value);
				}
			}
			fclose( fp );
		}


		if ( !ssc_module_exec_with_handler( p_mod, p_data, ssc_invoke_handler, &sc ))
		{
			m_errors.Add(wxString::Format("simulation did not succeed - compute module %s failed", cfg->Simulations[kk].c_str() ));
			if (sc.warnings->Count() > 0)
				for (size_t i=0; i<sc.warnings->Count();i++)
					m_errors.Add(wxString::Format("compute module %s warning[%d] = %s", cfg->Simulations[kk].c_str(), i, sc.warnings->Item(i).c_str() ));
		}
		else
		{
			pidx = 0;
			while( const ssc_info_t p_inf = ssc_module_var_info( p_mod, pidx++ ) )
			{
				int var_type = ssc_info_var_type( p_inf );   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
				int data_type = ssc_info_data_type( p_inf ); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX		
				const char *name = ssc_info_name( p_inf ); // assumed to be non-null
				wxString label( ssc_info_label( p_inf ) );
				wxString units( ssc_info_units( p_inf ) );
				
				if ( var_type == SSC_OUTPUT && data_type == SSC_NUMBER )
				{
					ssc_number_t vval;
					if ( ssc_data_get_number( p_data, name, &vval ) )
					{
						VarValue *vv = m_outputs.Create( name, VV_NUMBER );
						vv->Set( (float) vval );
						
						m_outputLabels[ name ] = label;
						m_outputUnits[ name ] = units;
					}
				}
				else if ( var_type == SSC_OUTPUT && data_type == SSC_ARRAY )
				{
					int len;
					if ( ssc_number_t *varr = ssc_data_get_array( p_data, name, &len ) )
					{
						VarValue *vv = m_outputs.Create( name, VV_ARRAY );
						float *ff = new float[len];
						for( int i=0;i<len;i++ )
							ff[i] = (float)(varr[i]);

						vv->Set( ff, (size_t)len );
						delete [] ff;
						
						m_outputLabels[ name ] = label;
						m_outputUnits[ name ] = units;
					}		
				}
			}
		}

		ssc_module_free( p_mod );
	}

	ssc_data_free( p_data );

	delete sc.progdlg;

	return m_errors.size() == 0;

}

void Simulation::ListByCount( size_t n, wxArrayString &list )
{
	VarTable &vt = Outputs();
	for ( VarTable::iterator it = vt.begin();
		it != vt.end();
		++it )
	{
		size_t len = 0;
		if ( it->second->Type() == VV_NUMBER )
			len = 1;
		else if ( it->second->Type() == VV_ARRAY )
			len = it->second->Length();

		if ( len == n )
			list.Add( it->first );
	}
}

void Simulation::GetVariableLengths( std::vector<size_t> &varlengths )
{	
	varlengths.clear();
	
	VarTable &vt = Outputs();
	if( vt.size() == 0 ) return;
	
	bool has_single_value = false;
	for ( VarTable::iterator it = vt.begin();
		it != vt.end();
		++it )
	{
		if ( it->second->Type() == VV_ARRAY )
		{
			size_t n = 0;
			float *f = it->second->Array( &n );

			if ( n > 1 && std::find( varlengths.begin(), varlengths.end(), n ) == varlengths.end() )
				varlengths.push_back( n );
		}
		else if ( it->second->Type() == VV_NUMBER )
			has_single_value = true;
	}

	if ( has_single_value ) 
		varlengths.push_back( 1 );

	// sort variable lengths
	std::stable_sort( varlengths.begin(), varlengths.end() );
}


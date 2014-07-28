#include <algorithm>

#include <wx/datstrm.h>
#include <wx/gauge.h>
#include <wx/progdlg.h>
#include <wx/thread.h>
#include <wx/statline.h>
#include <wx/stattext.h>

#include <wex/metro.h>

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
	m_outputs.Write( os, SamApp::Project().GetSaveHourlyData() ? 0 : 1024 );
	
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
	m_outputList.clear();
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

wxArrayString Simulation::ListOutputs()
{
	return m_outputList;
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

class SingleThreadHandler : public ISimulationHandler
{
	wxArrayString errors, warnings;
	wxProgressDialog *progdlg;
public:
	SingleThreadHandler() {
		progdlg = 0;
	};

	virtual int GetErrorCount() { return errors.size(); }
	void SetProgressDialog( wxProgressDialog *d ) { progdlg = d; }
	virtual wxArrayString GetErrors() { return errors; }
	virtual wxArrayString GetWarnings() { return warnings; }
	
	virtual void Warn( const wxString &s ) { 
		warnings.Add( s );
	}
	virtual void Error( const wxString &s ) { 
		errors.Add( s );
	}
	virtual void Update( float percent, const wxString &s ) {
		if( progdlg) progdlg->Update( (int)percent, s );
	}
	virtual bool IsCancelled() {
		if ( progdlg) return progdlg->WasCancelled();
		else return false;
	}

	virtual bool WriteDebugFile( const wxString &sim, ssc_module_t p_mod, ssc_data_t p_data )
	{
		wxString dbgfile( wxGetHomeDir() + "/ssc-" + sim + ".lk" );
		if( FILE *fp = fopen( dbgfile.c_str(), "w" ) )
		{
			ssc_number_t value;
			ssc_number_t *p;
			int len, nr, nc;
			int pidx = 0;
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
					fprintf(fp, "var( '%s', '%s' );\n", name, (const char*)str_value.c_str() );
					break;
				case SSC_NUMBER:
					::ssc_data_get_number( p_data, name, &value );
					dbl_value = (double)value;
					if ( dbl_value > 1e38 ) dbl_value = 1e38;
					fprintf(fp, "var( '%s', %lg );\n", name, dbl_value );
					break;
				case SSC_ARRAY:
					p = ::ssc_data_get_array( p_data, name, &len );
					fprintf(fp, "var( '%s', [", name);
					for ( int i=0;i<(len-1);i++ )
					{
						dbl_value = (double)p[i];
						if ( dbl_value > 1e38 ) dbl_value = 1e38;
						fprintf(fp, " %lg,", dbl_value );
					}
					dbl_value = (double)p[len-1];
					if ( dbl_value > 1e38 ) dbl_value = 1e38;
					fprintf(fp, " %lg ] );\n", dbl_value );
					break;
				case SSC_MATRIX:
					p = ::ssc_data_get_matrix( p_data, name, &nr, &nc );
					len = nr*nc;
					fprintf( fp, "var( '%s', \n[ [", name );					
					for (int k=0;k<(len-1);k++)
					{
						dbl_value = (double)p[k];
						if ( dbl_value > 1e38 ) dbl_value = 1e38;
						if ( (k+1)%nc == 0 ) 
							fprintf(fp, " %lg ], \n[", dbl_value);
						else
							fprintf(fp, " %lg,", dbl_value);
					}
					dbl_value = (double)p[len-1];
					if ( dbl_value > 1e38 ) dbl_value = 1e38;
					fprintf(fp, " %lg ] ] );\n", dbl_value);
				}
			}
			fclose( fp );
			return true;
		}
		else
			return false;
	}

};

static ssc_bool_t ssc_invoke_handler( ssc_module_t p_mod, ssc_handler_t p_handler,
	int action_type, float f0, float f1, 
	const char *s0, const char *s1,
	void *user_data )
{
	ISimulationHandler *hh = (ISimulationHandler*) user_data;
	if (!hh) return 0;

	if (action_type == SSC_LOG)
	{		
		switch( (int)f0 )
		{
		case SSC_NOTICE:
		case SSC_WARNING:
			hh->Warn( s0 );
			break;
		case SSC_ERROR:
			hh->Error( s0 );
			break;
		}
		
		return hh->IsCancelled() ? 0 : 1;		
	}
	else if (action_type == SSC_UPDATE)
	{
		hh->Update( f0, s0 );
		return hh->IsCancelled() ? 0 : 1;
	}
	else
		return 0;
}

bool Simulation::Invoke( bool silent, bool prepare )
{
	SingleThreadHandler sc;
	wxProgressDialog *prog = 0;
	if ( !silent )
	{
		prog = new wxProgressDialog("Simulation", "in progress", 100,
			SamApp::CurrentActiveWindow(),  // progress dialog parent is current active window - works better when invoked scripting
			wxPD_APP_MODAL | wxPD_SMOOTH | wxPD_CAN_ABORT);
		prog->Show();

		sc.SetProgressDialog( prog );
	}

	if ( prepare && !Prepare() )
		return false;
	
	bool ok =  InvokeWithHandler( &sc );

	if ( prog ) prog->Destroy();

	return ok;
}

bool Simulation::Prepare()
{	
	ConfigInfo *cfg = m_case->GetConfiguration();
	if ( !cfg )
	{
		m_errors.Add("no valid configuration for this case");
		return false;
	}

	m_simlist = cfg->Simulations;

	m_outputList.clear();
	m_outputLabels.clear();
	m_outputUnits.clear();

	// transfer all the values except for ones that have been 'overriden'

	for( VarTableBase::const_iterator it = m_case->Values().begin();
		it != m_case->Values().end();
		++it )
		if ( 0 == m_inputs.Get( it->first ) )
			m_inputs.Set( it->first, *(it->second) );

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
	
	//wxLogStatus("Simulation preparation time: %d copy, %d eval", (int)time_copy, (int)time_eval);

	return true;
}

bool Simulation::InvokeWithHandler( ISimulationHandler *ih )
{
	ssc_data_t p_data = ssc_data_create();

	if ( m_simlist.size() == 0 )
		m_errors.Add("No simulation compute modules defined for this configuration.");
	
	for( size_t kk=0;kk<m_simlist.size();kk++ )
	{
		ssc_module_t p_mod = ssc_module_create( m_simlist[kk].c_str() );
		if ( !p_mod )
		{
			m_errors.Add( "could not create ssc module: " + m_simlist[kk] );
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

		// write a debug input file if using a single threaded
		ih->WriteDebugFile( m_simlist[kk], p_mod, p_data );
		
		ssc_bool_t ok = ssc_module_exec_with_handler( p_mod, p_data, ssc_invoke_handler, ih );

		
		// copy over warnings and errors from the simulation
		wxArrayString list = ih->GetErrors();
		for( size_t k=0;k<list.size();k++ )
			m_errors.Add( list[k] );

		list = ih->GetWarnings();
		for( size_t k=0;k<list.size();k++ )
			m_warnings.Add( list[k] );

		if ( !ok )
		{
			m_errors.Add( "Simulation " + m_simlist[kk] + " failed." );
			if ( m_warnings.Count() > 0)
				for (size_t i=0; i<m_warnings.Count();i++)
					m_errors.Add(wxString::Format("Warning %d: %s", (int)(i+1), (const char*)m_warnings[i].c_str() ));
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
						m_outputList.Add( name );
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
						m_outputList.Add( name );
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


bool Simulation::ListAllOutputs( ConfigInfo *cfg, 
	wxArrayString *names, 
	wxArrayString *labels, 
	wxArrayString *units, 
	wxArrayString *groups,
	bool single_values )
{
	if ( !cfg ) return false;

	for( size_t kk=0;kk<cfg->Simulations.size();kk++ )
	{
		ssc_module_t p_mod = ssc_module_create( cfg->Simulations[kk].c_str() );
		if ( !p_mod )
			return false;

		int pidx=0;
		while( const ssc_info_t p_inf = ssc_module_var_info( p_mod, pidx++ ) )
		{
			int var_type = ssc_info_var_type( p_inf );   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
			int data_type = ssc_info_data_type( p_inf ); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX		
			
			if ( var_type == SSC_OUTPUT || var_type == SSC_INOUT )
			{
				if ( !single_values || (single_values && data_type == SSC_NUMBER ) )
				{
					if ( names ) names->Add( wxString(ssc_info_name( p_inf )) );
					if ( labels ) labels->Add( wxString(ssc_info_label( p_inf )) );
					if ( units ) units->Add( wxString(ssc_info_units( p_inf )) );
					if ( groups ) groups->Add( wxString(ssc_info_group( p_inf )) );
				}
			}
		}
	}

	return true;
}


class SimulationThread : public wxThread, ISimulationHandler
{
	std::vector<Simulation*> m_list;
	wxMutex m_currentLock, m_cancelLock, m_nokLock, m_logLock, m_percentLock;
	size_t m_current;
	bool m_canceled;
	size_t m_nok;
	wxArrayString m_messages;
	wxString m_update;
	wxString m_curName;
	float m_percent;
	int m_threadId;
	wxArrayString m_errors, m_warnings;
public:

	SimulationThread( int id )
		: wxThread( wxTHREAD_JOINABLE ) {
		m_canceled = false;
		m_threadId = id;
		m_nok = 0;
		m_percent = 0;
		m_current = 0;
	}
	
	void Add( Simulation *s ) {
		m_list.push_back( s );
	}

	size_t Size() { return m_list.size(); }
	size_t Current() { 
		wxMutexLocker _lock( m_currentLock );
		return m_current;
	}
	float GetPercent() {
		size_t ns = Size();
		size_t cur = Current();
		wxMutexLocker _lock(m_percentLock);
		float curper = m_percent;

		if ( ns == 0 ) return 0.0f;

		float each = 100/ns;
		float overall = cur*each + 0.01*curper*each;
		return overall;
	}

	void Cancel()
	{
		wxMutexLocker _lock(m_cancelLock);
		m_canceled = true;
	}

	size_t NOk() {
		wxMutexLocker _lock(m_nokLock);
		return m_nok;
	}
	
	virtual wxArrayString GetErrors() { return m_errors; }
	virtual wxArrayString GetWarnings() { return m_warnings; }
	virtual int GetErrorCount() { return m_errors.size(); }

	void Message( const wxString &text )
	{
		wxMutexLocker _lock(m_logLock);
		wxString L( m_curName );
		if ( !L.IsEmpty() ) L += ": ";
		m_messages.Add( L + text );
	}

	virtual void Warn( const wxString &text )
	{
		m_warnings.Add( text );
		Message( text );
	}

	virtual void Error( const wxString &text )
	{
		m_errors.Add( text );		
		Message( text );
	}

	virtual void Update( float percent, const wxString &text )
	{
		wxMutexLocker _lock(m_percentLock);
		m_percent = percent;
		m_update = text;
	}


	virtual bool IsCancelled() {
		wxMutexLocker _lock(m_cancelLock);
		return m_canceled;
	}

	virtual bool WriteDebugFile( const wxString &, ssc_module_t, ssc_data_t )
	{
		// don't write a debug file here... although we could
		// write to different files given the simulation ID #?
		return false;
	}
	
	wxArrayString GetNewMessages()
	{
		wxMutexLocker _lock(m_logLock);
		wxArrayString list = m_messages;
		m_messages.Clear();
		return list;
	}

	virtual void *Entry()
	{
		m_canceled = false;
		for( size_t i=0;i<m_list.size();i++ )
		{
			m_curName = m_list[i]->GetName();
			m_errors.Clear();
			m_warnings.Clear();
			if ( m_list[i]->InvokeWithHandler( this ) )
			{
				wxMutexLocker _lock(m_nokLock);
				m_nok++;
			}

			m_currentLock.Lock();
			m_current++;
			m_currentLock.Unlock();

			wxMutexLocker _lock(m_cancelLock);
			if (m_canceled) break;
		}

		return 0;
	}
};

int Simulation::DispatchThreads( SimulationDialog &tpd, 
	std::vector<Simulation*> &sims, 
	int nthread )
{
	return DispatchThreads( tpd.Dialog(), sims, nthread );
} 

int Simulation::DispatchThreads( ThreadProgressDialog &tpd, 
	std::vector<Simulation*> &sims, 
	int nthread )
{	
	wxStopWatch sw;

	// no need to create extra unnecessary threads 
	if (nthread > sims.size()) nthread = sims.size();

	std::vector<SimulationThread*> threads;
	for( int i=0;i<nthread;i++)
	{
		SimulationThread *t = new SimulationThread( i );
		threads.push_back( t );
		t->Create();
	}

	// round robin assign each simulation to a thread
	size_t ithr = 0;
	for( size_t i=0;i<sims.size();i++ )
	{
		threads[ithr++]->Add( sims[i] );
		if ( ithr == threads.size() )
			ithr = 0;
	}

	sw.Start();
	
	// start the threads
	for ( int i=0;i<nthread;i++ )
		threads[i]->Run();

	while (1)
	{
		size_t i, num_finished = 0;
		for (i=0;i<threads.size();i++)
			if (!threads[i]->IsRunning())
				num_finished++;

		if (num_finished == threads.size())
			break;

		// threads still running so update interface
		for (i=0;i<threads.size();i++)
		{
			float per = threads[i]->GetPercent();
			tpd.Update(i, per);
			wxArrayString msgs = threads[i]->GetNewMessages();
			tpd.Log( msgs );
		}

		wxGetApp().Yield();

		// if dialog's cancel button was pressed, send cancel signal to all threads
		if (tpd.IsCanceled())
		{
			for (i=0;i<threads.size();i++)
				threads[i]->Cancel();
		}

		::wxMilliSleep( 100 );
	}

	
	size_t nok = 0;
	// wait on the joinable threads
	for (size_t i=0;i<threads.size();i++)
	{
		threads[i]->Wait();
		nok += threads[i]->NOk();

		// update final progress
		float per = threads[i]->GetPercent();
		tpd.Update(i, per);

		// get any final simulation messages
		wxArrayString msgs = threads[i]->GetNewMessages();
		tpd.Log( msgs );
	}
	
	// delete all the thread objects
	for (size_t i=0;i<threads.size();i++)
		delete threads[i];

	threads.clear();
	
	return nok;
}



BEGIN_EVENT_TABLE( ThreadProgressDialog, wxDialog )
	EVT_BUTTON( wxID_CANCEL, ThreadProgressDialog::OnCancel )
	EVT_CLOSE( ThreadProgressDialog::OnDialogClose )
END_EVENT_TABLE( )

ThreadProgressDialog::ThreadProgressDialog(wxWindow *parent, int nthreads)
	: wxDialog( parent, wxID_ANY, wxString("Simulation Progress"), wxDefaultPosition, 
	wxSize(625, 475), wxBORDER_NONE )
{
	SetBackgroundColour( *wxWHITE );
	m_canceled = false;
	m_button = new wxMetroButton(this, wxID_CANCEL, "Cancel");//, wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);
	
	wxBoxSizer *szv = new wxBoxSizer(wxVERTICAL);

	m_status = new wxStaticText( this, wxID_ANY, "Processing..." );
	m_status->SetFont( wxMetroTheme::Font( wxMT_LIGHT, 15 ) );
	m_status->SetForegroundColour( wxMetroTheme::Colour( wxMT_TEXT ) );

	szv->Add( m_status, 0, wxALL|wxEXPAND, 20 );

	for (int i=0;i<nthreads;i++)
	{
			
		wxStaticText *label = new wxStaticText(this, wxID_ANY, wxString::Format("Process %d", i+1));
		label->SetForegroundColour( wxMetroTheme::Colour( wxMT_TEXT ) );

		wxGauge *gauge = new wxGauge(this, wxID_ANY, 100);
		wxTextCtrl *text = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_READONLY|wxBORDER_NONE);
		text->SetBackgroundColour( *wxWHITE );
		text->SetForegroundColour( wxMetroTheme::Colour( wxMT_TEXT ) );
		
		wxBoxSizer *sizer = new wxBoxSizer( wxHORIZONTAL );
		sizer->Add( label, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
		sizer->Add( gauge, 1, wxALL|wxEXPAND, 5 );
		sizer->Add( text, 0, wxALL|wxEXPAND, 5 );

		m_labels.push_back(label);
		m_progbars.push_back(gauge);
		m_percents.push_back(text);
			
		szv->Add( sizer, 0, wxEXPAND|wxALL, 5 );
	}
	
	m_log = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxBORDER_NONE );
	m_log->SetForegroundColour( wxMetroTheme::Colour( wxMT_TEXT ) );		
	szv->Add( m_log, 1, wxALL|wxEXPAND, 10 );
	szv->Add( m_button, 0, wxALIGN_CENTER_VERTICAL|wxCENTER|wxLEFT|wxRIGHT|wxBOTTOM, 10);

	SetSizer(szv);
}

void ThreadProgressDialog::SetButtonText( const wxString &text )
{
	m_button->SetLabel( text );
	Layout();
	wxYield();
}

void ThreadProgressDialog::Status( const wxString &s )
{
	m_status->SetLabel( s );
}

void ThreadProgressDialog::Reset()
{
	for( size_t i=0;i<m_progbars.size();i++ )
	{
		m_labels[i]->SetLabel( wxString::Format("Process %d", i+1) );
		m_progbars[i]->SetValue( 0 );
		m_percents[i]->ChangeValue( wxEmptyString );
	}

	Layout();
}

void ThreadProgressDialog::ShowBars( int n )
{
	if ( n < 0 ) n = m_progbars.size();
	for( size_t i=0;i<m_progbars.size();i++ )
	{
		bool show = (i < n);
		m_labels[i]->Show( show );
		m_progbars[i]->Show( show );
		m_percents[i]->Show( show );
	}

	Layout();
}

void ThreadProgressDialog::Log( const wxArrayString &list )
{
	for (size_t i=0;i<list.Count();i++)
		Log(list[i]);
}

bool ThreadProgressDialog::HasMessages()
{
	return ( m_log->GetValue().Len() > 0 );
}

wxString ThreadProgressDialog::GetMessages()
{
	return m_log->GetValue();
}

void ThreadProgressDialog::Log( const wxString &text )
{
	m_log->AppendText( text + "\n" );
}

void ThreadProgressDialog::Update(int ThreadNum, float percent, const wxString &text)
{
	if (ThreadNum >= 0 && ThreadNum < m_progbars.size())
	{
		m_progbars[ThreadNum]->SetValue( (int)percent );
		m_percents[ThreadNum]->ChangeValue( wxString::Format("%.1f %%", percent) );
		if ( !text.IsEmpty() )
		{
			m_labels[ThreadNum]->SetLabel( text );
			Layout();
		}
	}
}

	
void ThreadProgressDialog::OnCancel(wxCommandEvent &evt)
{
	m_canceled = true;
	if ( IsModal() )
		EndModal( wxID_CANCEL );
}

void ThreadProgressDialog::OnDialogClose(wxCloseEvent &evt)
{
	m_canceled = true;
	evt.Skip();
}


SimulationDialog::SimulationDialog( const wxString &message, int nthread )
{
	if ( nthread < 1 )
		nthread = wxThread::GetCPUCount();

	m_transp = CreateTransparentOverlay( SamApp::Window() );
	m_tpd = new ThreadProgressDialog( m_transp, nthread );
	m_tpd->CenterOnParent();
	m_tpd->Show();
	if ( message.IsEmpty() )
		m_tpd->Status( "Simulating...");
	else
		m_tpd->Status( message );
	m_tpd->ShowBars( 1 );
	wxYield();
}

SimulationDialog::~SimulationDialog()
{
	m_tpd->Destroy();
	m_transp->Destroy();
}

void SimulationDialog::Finalize( const wxString &title )
{			
	if ( m_tpd->HasMessages() )
	{
		if ( title.IsEmpty() ) m_tpd->Status( "Simulations finished with notices." );
		else m_tpd->Status( title );

		m_tpd->ShowBars( 0 );
		m_tpd->SetButtonText( "Close" );
		m_tpd->Hide();
		m_tpd->ShowModal();
	}
}

void SimulationDialog::Update(int ThreadNum, float percent, const wxString &label )
{
	m_tpd->Update(ThreadNum, percent, label);
	wxYield();
}

void SimulationDialog::NewStage( const wxString &title, int nbars_to_show )
{
	m_tpd->Reset();
	m_tpd->Status( title );
	m_tpd->ShowBars( nbars_to_show );
	wxYield();
}

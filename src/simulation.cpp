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

VarTable &Simulation::Results()
{
	return m_results;
}


struct SimulationContext
{
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
				VarValue *vv = GetInput(name);
				if ( !vv && strcmp(reqd,"*")==0)
				{
					m_errors.Add( "SSC requires input '" + name + "', but was not found in the SAM UI" );
				}
				else if ( vv != 0 )
				{
					if (!VarValueToSSC( vv, p_data, name ))
						m_errors.Add( "Error translating data from SAM UI to SSC for " + name );
				}
			}
		}

		if ( !ssc_module_exec_with_handler( p_mod, p_data, ssc_invoke_handler, &sc ))
		{
			m_errors.Add("simulation did not succeed.");
		}
		else
		{
			pidx = 0;
			while( const ssc_info_t p_inf = ssc_module_var_info( p_mod, pidx++ ) )
			{
				int var_type = ssc_info_var_type( p_inf );   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
				int data_type = ssc_info_data_type( p_inf ); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX		
				const char *name( ssc_info_name( p_inf ) ); // assumed to be non-null
				
				if ( var_type == SSC_OUTPUT && data_type == SSC_NUMBER )
				{
					ssc_number_t vval;
					if ( ssc_data_get_number( p_data, name, &vval ) )
					{
						VarValue *vv = m_results.Create( name, VV_NUMBER );
						vv->Set( (float) vval );
					}
				}
				else if ( var_type == SSC_OUTPUT && data_type == SSC_ARRAY )
				{
					int len;
					if ( ssc_number_t *varr = ssc_data_get_array( p_data, name, &len ) )
					{
						VarValue *vv = m_results.Create( name, VV_ARRAY );
						float *ff = new float[len];
						for( int i=0;i<len;i++ )
							ff[i] = (float)(varr[i]);

						vv->Set( ff, (size_t)len );
						delete [] ff;
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

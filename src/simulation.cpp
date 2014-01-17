#include <wx/gauge.h>

#include <lk_absyn.h>
#include <lk_stdlib.h>
#include <lk_eval.h>

#include <ssc/sscapi.h>

#include "simulation.h"
#include "main.h"
#include "equations.h"
#include "case.h"


/*


class SimulationContext
{
	Simulation *m_sim;
	ssc_data_t m_sscData;
	bool m_canceled;
public:
	SimulationContext( Simulation *sim )
		: m_sim(sim), m_canceled(false)
	{
		m_sscData = ssc_data_create();
	}
	
	virtual ~SimulationContext()
	{
		ssc_data_free( m_sscData );
		m_sscData = 0;
	}
	
	ssc_data_t GetSSCData() { return m_sscData; }
	Simulation *GetSimulation() { return m_sim; }
	VarValue *GetInput( const wxString &name ) { return m_sim->GetInput( name ); }
	void Cancel() { m_canceled = true; }
	bool IsCanceled() { return m_canceled; }

	virtual bool OnProgressUpdate( float percent_done, const wxString &message )
	{
		// push the update to the UI
		return !m_canceled;
	}
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
			sc->GetSimulation()->GetWarnings().Add( s0 );
			break;
		case SSC_ERROR:
			sc->GetSimulation()->GetErrors().Add( s0 );
			break;
		}
		return sc->IsCanceled() ? 0 : 1;
	}
	else if (action_type == SSC_UPDATE)
	{
		return sc->OnProgressUpdate( f0, s0 ) ? 1 : 0;
	}
	else
		return 0;
}

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

static void fcall_ssc_invoke( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_invoke", "Run an ssc compute module", "(string:name):boolean" );
	
	cxt.result().assign( 0.0 );

	SimulationContext *simcxt = (SimulationContext*)cxt.user_data();
	wxString module_name = cxt.arg(0).as_string();
	wxArrayString &errors = simcxt->GetSimulation()->GetErrors();

	ssc_module_t pmod = ssc_module_create( module_name.c_str() );
	if ( !pmod )
	{
		errors.Add( "could not create ssc module instance: " + module_name );
		return;
	}

	if ( ssc_module_exec_with_handler( pmod, simcxt->GetSSCData(), ssc_invoke_handler, simcxt ) )
		cxt.result().assign( 1.0 );
	
	ssc_module_free( pmod );
}

static void fcall_ssc_copy( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_copy", "Copy a SAM variable to SSC", "(string:ssc var, string:sam var):boolean" );

	cxt.result().assign( 0.0 );
	SimulationContext *simcxt = (SimulationContext*)cxt.user_data();
	wxString sscvar = cxt.arg(0).as_string();
	wxString samvar = cxt.arg(1).as_string();

	if ( VarValue *vv = simcxt->GetInput( samvar ) )
		if ( VarValueToSSC( vv, simcxt->GetSSCData(), sscvar ) )
			cxt.result().assign( 1.0 );
}

static void fcall_ssc_assign( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_assign", "Assign an ssc variable with the given value", "(string:name, variant:value):void" );
	
	SimulationContext *simcxt = (SimulationContext*)cxt.user_data();
	VarValue vv;
	if ( vv.Read( cxt.arg(1), true ) )
		VarValueToSSC( &vv, simcxt->GetSSCData(), cxt.arg(0).as_string() );
}

static void fcall_sam_copy( lk::invoke_t &cxt )
{
	LK_DOC( "sam_copy", "Assign an SSC number or array to the SAM results table with optional scaling", "(string:resultvar, string:sscvar[, number:scale]):void");
	
	SimulationContext *simcxt = (SimulationContext*)cxt.user_data();
	wxString samvar = cxt.arg(0).as_string();
	wxString sscvar = cxt.arg(1).as_string();
	
	float scale = 1.0f;
	if ( cxt.arg_count() > 2 )
		scale = (float) cxt.arg(2).as_number();

	int type = ssc_data_query( simcxt->GetSSCData(), sscvar.c_str() );

	if ( type == SSC_NUMBER )
	{
		ssc_number_t vval;
		ssc_data_get_number( simcxt->GetSSCData(), sscvar.c_str(), &vval );
		VarValue *vv = simcxt->GetSimulation()->Results().Create( samvar, VV_NUMBER );
		vv->Set( (float) (vval*scale) );
	}
	else if ( type == SSC_ARRAY )
	{
		int len;
		if ( ssc_number_t *varr = ssc_data_get_array( simcxt->GetSSCData(), sscvar.c_str(), &len ) )
		{
			VarValue *vv = simcxt->GetSimulation()->Results().Create( samvar, VV_ARRAY );
			float *ff = new float[len];
			for( int i=0;i<len;i++ )
				ff[i] = (float)(scale*varr[i]);

			vv->Set( ff, (size_t)len );
			delete [] ff;
		}		
	}
}

static void fcall_sam_assign( lk::invoke_t &cxt )
{
	LK_DOC( "sam_assign", "Assign a value to a SAM result variable", "(string:name, variant:value):void");
	
	SimulationContext *simcxt = (SimulationContext*)cxt.user_data();
	wxString samvar = cxt.arg(0).as_string();
	
	if ( VarValue *vv = simcxt->GetSimulation()->Results().Create( samvar ) )
		vv->Read( cxt.arg(1), true );
}



lk::fcall_t* invoke_simulation_stubs()
{
	static const lk::fcall_t vec[] = {
		fcall_ssc_invoke,
		fcall_ssc_copy,
		fcall_ssc_assign,
		fcall_sam_copy,
		fcall_sam_assign,
		0 };
	return (lk::fcall_t*)vec;
}



Simulation::Simulation( Case *cc, const wxString &name )
	: m_case( cc ), m_name( name )
{
	m_case->GetConfiguration( &m_tech, &m_fin );
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
				if ( VarInfo *vi = SamApp::Variables().Lookup( m_overrides[i] ) )
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

bool Simulation::PrepareInputs()
{
	// transfer all the values except for ones that have been 'overriden'

	for( VarTableBase::const_iterator it = m_case->Values().begin();
		it != m_case->Values().end();
		++it )
		if ( 0 == m_inputs.Get( it->first ) )
			m_inputs.Set( it->first, *it->second );

	// recalculate all the equations

	CaseEqnEvaluator eval( m_case, m_inputs, m_case->Equations() );
	int n = eval.CalculateAll();

	if ( n < 0 )
	{
		wxArrayString errs = eval.GetErrors();
		for( size_t i=0;i<errs.size();i++ )
			m_errors.Add( errs[i] );

		return false;
	}

	return true;

}

bool Simulation::Invoke( SimulationContext *context )
{
	if ( Invoke( m_tech, context ) )
		if ( Invoke( m_fin, context ) )
			return true;

	return false;
}

bool Simulation::Invoke( const wxString &name, SimulationContext *context )
{	
	// lookup and run any callback functions.
	if ( lk::node_t *root = SamApp::Callbacks().Lookup( "on_simulate", name ) )
	{
		lk::env_t local_env( SamApp::Callbacks().GetEnv() );

		// add other callback environment functions
		local_env.register_funcs( lk::stdlib_basic() );
		local_env.register_funcs( lk::stdlib_math() );
		local_env.register_funcs( lk::stdlib_string() );
			
		local_env.register_func( fcall_ssc_invoke, context );
		local_env.register_func( fcall_ssc_copy, context );
		local_env.register_func( fcall_ssc_assign, context );
		local_env.register_func( fcall_sam_copy, context );
		local_env.register_func( fcall_sam_assign, context );
	
		try {
			VarTableScriptInterpreter e( root, &local_env, &m_inputs );
			if ( !e.run() )
			{
				m_errors.Add( "could not evaluate simulation script: " + name );
				for (size_t i=0;i<e.error_count();i++)
					m_errors.Add( e.get_error(i) );

				return false;
			}
		
		} catch(std::exception &e ){

			m_errors.Add( "exception in simulation script: " + name );
			m_errors.Add( wxString(e.what()) );
			return false;
		}

		return true;
	}
	else
	{
		m_errors.Add("on_simulate->" + name + " function not found");
		return false;
	}
}



*/
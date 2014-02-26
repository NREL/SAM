#include <wx/datstrm.h>

#include <wex/utils.h>

#include "case.h"
#include "equations.h"
#include "main.h"
#include "library.h"
#include "invoke.h"

#ifndef MAX
#define MAX(a, b) ((a) > (b) ? a : b)
#endif

static void fcall_technology_pCase( lk::invoke_t &cxt )
{
	LK_DOC( "technology", "Return the current technology option name", "(void):string" );
	if ( Case *cc = static_cast<Case*>( cxt.user_data() ) ) 
		cxt.result().assign( cc->GetTechnology() );
}

static void fcall_financing_pCase( lk::invoke_t &cxt )
{
	LK_DOC( "financing", "Return the current financing option name", "(void):string" );
	if ( Case *cc = static_cast<Case*>( cxt.user_data() ) )
		cxt.result().assign( cc->GetFinancing() );
}

static void fcall_substance_density_pCase(lk::invoke_t &cxt)
{
	LK_DOC("substance_density", "Return the density given a substance ID and temperature in C", "(variant:substanceID, variant:tempC):variant");
	if (Case *cc = static_cast<Case*>(cxt.user_data()))
		cxt.result().assign(cc->GetSubstanceDensity(cxt.arg(0).as_unsigned(), cxt.arg(1).as_number()));
	else
		cxt.result().assign(0.0);
}


CaseEvaluator::CaseEvaluator( Case *cc, VarTable &vars, EqnFastLookup &efl )
	: EqnEvaluator( vars, efl )
{
	m_case = cc;
}

void CaseEvaluator::SetupEnvironment( lk::env_t &env )
{
	// call base version first to register standard functions
	EqnEvaluator::SetupEnvironment( env );

	env.register_func( fcall_technology_pCase, m_case );
	env.register_func( fcall_financing_pCase, m_case );
	env.register_funcs( invoke_ssc_funcs() );
	env.register_func( fcall_substance_density_pCase, m_case);
}
	
int CaseEvaluator::CalculateAll()
{
	int nlibchanges = 0;
	for ( VarInfoLookup::iterator it = m_case->Variables().begin();
		it != m_case->Variables().end();
		++it )
	{
		if ( it->second->Flags & VF_LIBRARY
			&& it->second->Type == VV_STRING )
		{
			wxArrayString changed;
			if ( !UpdateLibrary( it->first, changed ) )
				return -1;
			else
				nlibchanges += changed.size();
		}
	}
	
	int nevals = EqnEvaluator::CalculateAll();
	if ( nevals >= 0 ) nevals += nlibchanges;

	return nevals;	
}

int CaseEvaluator::Changed( const wxArrayString &vars )
{
	int nlibchanges=0;
	wxArrayString trigger_list;
	for( size_t i=0;i<vars.size();i++ )
	{
		trigger_list.Add( vars[i] );

		wxArrayString changed;
		bool ok = UpdateLibrary( vars[i], changed );
		if ( ok && changed.size() > 0 )
		{
			for( size_t j=0;j<changed.size();j++ )
			{
				m_updated.Add( changed[j] );
				trigger_list.Add( changed[j] );
				nlibchanges++;
			}
		}
		else if ( !ok )
			return -1;
	}
	
	int nevals = EqnEvaluator::Changed( trigger_list );
	if ( nevals >= 0 ) nevals += nlibchanges;

	return nevals;
}

int CaseEvaluator::Changed( const wxString &trigger )
{
	wxArrayString list;
	list.Add(trigger);
	return Changed( list );
}

bool CaseEvaluator::UpdateLibrary( const wxString &trigger, wxArrayString &changed )
{
	size_t nerrors = 0;
	VarInfo *vi = m_case->Variables().Lookup( trigger );
	VarValue *vv = m_case->Values().Get( trigger );
	if( vv && vv->Type() == VV_STRING && vi && vi->Flags & VF_LIBRARY )
	{
		if ( vi->IndexLabels.size() == 2 )
		{
			// lookup the library name in vi->IndexLabels
			wxString name = vi->IndexLabels[0];
			int varindex = wxAtoi( vi->IndexLabels[1] );
		
			if ( Library *lib = Library::Find( name ) )
			{
				// find the entry
				int entry = lib->FindEntry( vv->String() );
				if ( entry >= 0 && lib->ApplyEntry( entry, varindex, m_case->Values(), changed ) )
				{
					wxLogStatus( "applied " + name + ":" + vv->String() + " = " + wxJoin(changed,',') );
//					SendEvent( CaseEvent( CaseEvent::VARS_CHANGED, changed ) );
				}
				else
				{
					nerrors++;
					m_errors.Add("error applying library entry " + vv->String() + "\n\n" + wxJoin( lib->GetErrors(), wxChar('\n')) );
				}
			}
			else
			{
				nerrors++;
				m_errors.Add( "Could not locate referenced library: " + name);
			}
		}
		else
		{
			nerrors++;
			m_errors.Add( "invalid library specification: " + wxJoin(vi->IndexLabels, ',') );
		}
	}

	return nerrors == 0;
}


Case::Case()
	: m_config(0)
{

}

Case::~Case()
{
	ClearListeners();
}
	
Object *Case::Duplicate()
{
	Case *c = new Case();
	c->Copy(this);
	return c;
}

bool Case::Copy( Object *obj )
{
	if ( Case *rhs = dynamic_cast<Case*>( obj ) )
	{
		m_vals.Copy( rhs->m_vals );
		m_baseCase.Copy( rhs->m_vals );
		m_properties = rhs->m_properties;
		m_notes = rhs->m_notes;
		
		m_config = 0;
		if ( rhs->m_config )
			SetConfiguration( rhs->m_config->Technology, rhs->m_config->Financing );

		return true;
	}
	else
		return false;
}

wxString Case::GetTypeName()
{
	return "sam.case";
}

void Case::Write( wxOutputStream &_o )
{
	wxDataOutputStream out(_o);

	out.Write8( 0x9b );
	out.Write8( 1 );

	wxString tech, fin;
	if ( m_config != 0 )
	{
		tech = m_config->Technology;
		fin = m_config->Financing;
	}

	// write data
	out.WriteString( tech );
	out.WriteString( fin );
	m_vals.Write( _o );
	m_baseCase.Write( _o );
	m_properties.Write( _o );
	m_notes.Write( _o );

	out.Write8( 0x9b );
}

bool Case::Read( wxInputStream &_i )
{
	wxDataInputStream in(_i);

	wxUint8 code = in.Read8();
	in.Read8(); // version

	// read data
	wxString tech = in.ReadString();
	wxString fin = in.ReadString();
	if ( !m_vals.Read( _i ) ) wxLogStatus("error reading m_vals in Case::Read");
	if ( !m_baseCase.Read( _i ) ) wxLogStatus("error reading m_baseCase in Case::Read");
	if ( !m_properties.Read( _i ) ) wxLogStatus("error reading m_properties in Case::Read");
	if ( !m_notes.Read( _i ) ) wxLogStatus("error reading m_notes in Case::Read");

	SetConfiguration( tech, fin );

	return (in.Read8() == code);
}


void Case::SetConfiguration( const wxString &tech, const wxString &fin )
{
	// erase results
	m_baseCase.clear();

	m_config = SamApp::Config().Find( tech, fin );
		
	if ( !m_config )
	{
		wxMessageBox("Case error: could not find configuration information for " + tech + ", " + fin );
		return;
	}

	// erase all input variables that are no longer in the current configuration
	wxArrayString to_remove;
	VarInfoLookup &vars = m_config->Variables;

	for( VarTable::iterator it = m_vals.begin(); it != m_vals.end(); ++it )
		if ( vars.find( it->first ) == vars.end() )
			to_remove.Add( it->first );

	m_vals.Delete( to_remove );

	// set up any new variables with internal default values
	for( VarInfoLookup::iterator it = vars.begin(); it != vars.end(); ++it )
		if ( !m_vals.Get( it->first ) )
			m_vals.Set( it->first, it->second->DefaultValue ); // will create new variable if it doesnt exist
		
	// reevalute all equations
	CaseEvaluator eval( this, m_vals, m_config->Equations );
	int n = eval.CalculateAll();
	if ( n < 0 )
		::wxShowTextMessageDialog( wxJoin( eval.GetErrors(), wxChar('\n') ) );

	// setup the local callback environment
	// by merging all the functions defined
	// in the various input page callback scripts
	// into one runtime environment
	// the parse trees of the actual function implementations
	// are not copied - they just reference those stored in the
	// scriptdatabase(s) that are members of inputpagedata
	m_cbEnv.clear_objs();
	m_cbEnv.clear_vars();

	lk::vardata_t *vdt_on_load = new lk::vardata_t;
	vdt_on_load->empty_hash();
	m_cbEnv.assign( "on_load", vdt_on_load );

	lk::vardata_t *vdt_on_change = new lk::vardata_t;
	vdt_on_change->empty_hash();
	m_cbEnv.assign( "on_change", vdt_on_change );
	
	for( InputPageDataHash::iterator it = m_config->InputPages.begin();
		it != m_config->InputPages.end();
		++it )
	{
		lk::env_t *env = it->second->Callbacks().GetEnv();
		lk_string key;
		lk::vardata_t *val;
		bool has_more = env->first( key, val );
		while( has_more )
		{
			if ( val->type() == lk::vardata_t::FUNCTION )
				m_cbEnv.assign( key, new lk::vardata_t( *val ) );
			else if ( val->type() == lk::vardata_t::HASH
				&& (key == "on_load" || key == "on_change") )
			{
				lk::vardata_t *target = (key=="on_load") ? vdt_on_load : vdt_on_change;
				lk::varhash_t *hh = val->hash();
				for( lk::varhash_t::iterator ihh = hh->begin();
					ihh != hh->end();
					++ihh )
					target->hash_item( ihh->first, *ihh->second );
			}

			has_more = env->next( key, val );
		}
	}
	
	// update UI
	SendEvent( CaseEvent( CaseEvent::CONFIG_CHANGED, tech, fin ) );
}

lk::env_t &Case::CallbackEnvironment()
{
	return m_cbEnv;
}

lk::node_t *Case::QueryCallback( const wxString &method, const wxString &object )
{
	
	lk::vardata_t *cbvar = m_cbEnv.lookup( method, true);

	if (!cbvar || cbvar->type() != lk::vardata_t::HASH )
	{
		//wxLogStatus("ScriptDatabase::Invoke: could not find " + method_name + " variable or not a hash");
		return 0;
	}

	lk::vardata_t *cbref = cbvar->lookup( object );
	if ( cbref == 0 
		|| cbref->type() != lk::vardata_t::FUNCTION
		|| cbref->deref().func() == 0 )
	{
		// wxLogStatus("ScriptDatabase::Invoke: could not find function entry for '%s'", (const char*)obj_name.c_str() );
		return 0;
	}
	
	lk::expr_t *p_define = cbref->deref().func();
	if ( p_define->oper != lk::expr_t::DEFINE )
	{
		wxLogStatus("Case::QueryCallback: improper function structure, must be a 'define' for %s, instead: %s", (const char*)object.c_str(), cbref->func()->operstr() );
		return 0;
	}
	
	if ( p_define->right == 0 )
	{
		wxLogStatus("Case::QueryCallback: function block nonexistent for '%s'\n", (const char*)object.c_str());
		return 0;
	}

	return p_define->right;
}

void Case::GetConfiguration( wxString *tech, wxString *fin )
{
	if ( m_config )
	{
		if ( tech ) *tech = m_config->Technology;
		if ( fin ) *fin = m_config->Financing;
	}
}

VarInfoLookup &Case::Variables()
{
static VarInfoLookup sg_emptyVars;
	return m_config ? m_config->Variables : sg_emptyVars;
}

EqnFastLookup &Case::Equations()
{
static EqnFastLookup sg_emptyEqns;
	return m_config ? m_config->Equations : sg_emptyEqns;
}

wxString Case::GetTechnology() const
{
	return m_config ? m_config->Technology : wxEmptyString;
}

wxString Case::GetFinancing() const
{
	return m_config ? m_config->Financing : wxEmptyString;
}

void Case::VariableChanged( const wxString &var )
{
	// Send the additional case event that this variable
	// was programmatically changed and needs to be updated
	CaseEvent ce( CaseEvent::VARS_CHANGED );
	ce.GetVars().Add( var );
	SendEvent( ce );

	// issue the request for any calculations to be updated as needed
	Recalculate( var );
}

int Case::Recalculate( const wxString &trigger )
{
	if ( !m_config )
	{
		wxLogStatus( "cannot recalculate: no active configuration" );
		return -1;
	}

	
	CaseEvaluator eval( this, m_vals, m_config->Equations );
	int n = eval.Changed( trigger );	
	if ( n > 0 ) SendEvent( CaseEvent( CaseEvent::VARS_CHANGED, eval.GetUpdated() ) );
	else if ( n < 0 ) wxShowTextMessageDialog( wxJoin( eval.GetErrors(), wxChar('\n') )  );
	return n;

}

int Case::RecalculateAll()
{
	if ( !m_config )
	{
		wxLogStatus( "cannot recalculate all, no valid configuration information" );
		return -1;
	}

	CaseEvaluator eval( this, m_vals, m_config->Equations );
	int n = eval.CalculateAll();
	if ( n > 0 ) SendEvent( CaseEvent( CaseEvent::VARS_CHANGED, eval.GetUpdated() ) );
	else if ( n < 0 ) wxShowTextMessageDialog( wxJoin( eval.GetErrors(), wxChar('\n') )  );
	return n;
}

void Case::AddListener( CaseEventListener *cel )
{
	m_listeners.push_back( cel );
}

void Case::RemoveListener( CaseEventListener *cel )
{
	std::vector<CaseEventListener*>::iterator it = std::find( m_listeners.begin(), m_listeners.end() , cel );
	if ( it != m_listeners.end() )
		m_listeners.erase( it );
}

void Case::ClearListeners()
{
	m_listeners.clear();
}

void Case::SendEvent( CaseEvent e )
{
	for( size_t i=0;i<m_listeners.size();i++ )
		m_listeners[i]->OnCaseEvent( this, e );
}

wxString Case::GetProperty( const wxString &id )
{
	StringHash::iterator it = m_properties.find( id );
	if ( it != m_properties.end() )
		return it->second;
	else
		return wxEmptyString;
}
void Case::SetProperty( const wxString &id, const wxString &value )
{
	m_properties[id] = value;
}

wxString Case::RetrieveNote( const wxString &id )
{
	StringHash::iterator it = m_notes.find( id );
	if ( it != m_notes.end() )
		return it->second;
	else return wxEmptyString;
}

void Case::SaveNote( const wxString &id, const wxString &text )
{
	m_notes[id] = text;
}

double Case::GetSubstanceDensity(size_t substanceID, double tempC) const
{
	/*
	This function accepts as inputs temperature [K] and pressure [Pa]
	This function outputs density in units of [kg/m^3]
	*/
	double T = tempC + 273.15; // C to K
	double v, R_air;
	double P = 1.0; // default pressure used 1Pa
	double density = 1.0; // default - Type 229=0, Type 805=1

	switch (substanceID)
	{
	case 1: //   1.) Air
		R_air = 287; // Gas constant [J/kg-K]
		v = R_air*T / P;
		density = 1.0 / v;
		break;
	case 2: //   2.) Stainless_AISI316
		density = 8349.38 - 0.341708*T - 0.0000865128*T*T; // !EES
		break;
	case 3: //   3.) Water (liquid)
		break;
	case 4: //   4.) Steam
		break;
	case 5: //   5.) CO2
		break;
	case 6: //   6.) Salt (68% KCl, 32% MgCl2)
		density = 1E-10*T*T*T - 3E-07*T*T - 0.4739*T + 2384.2;
		break;
	case 7: //   7.) Salt (8% NaF, 92% NaBF4)
		density = 8E-09*T*T*T - 2E-05*T*T - 0.6867*T + 2438.5;
		break;
	case 8: //   8.) Salt (25% KF, 75% KBF4)
		density = 2E-08*T*T*T - 6E-05*T*T - 0.7701*T + 2466.1;
		break;
	case 9: //   9.) Salt (31% RbF, 69% RbBF4)
		density = -1E-08*T*T*T + 4E-05*T*T - 1.0836*T + 3242.6;
		break;
	case 10: //   10.) Salt (46.5% LiF, 11.5%NaF, 42%KF)
		density = -2E-09*T*T*T + 1E-05*T*T - 0.7427*T + 2734.7;
		break;
	case 11: //   11.) Salt (49% LiF, 29% NaF, 29% ZrF4)
		density = -2E-11*T*T*T + 1E-07*T*T - 0.5172*T + 3674.3;
		break;
	case 12: //   12.) Salt (58% KF, 42% ZrF4)
		density = -6E-10*T*T*T + 4E-06*T*T - 0.8931*T + 3661.3;
		break;
	case 13: //   13.) Salt (58% LiCl, 42% RbCl)
		density = -8E-10*T*T*T + 1E-06*T*T - 0.689*T + 2929.5;
		break;
	case 14: //   14.) Salt (58% NaCl, 42% MgCl2)
		density = -5E-09*T*T*T + 2E-05*T*T - 0.5298*T + 2444.1;
		break;
	case 15: //   15.) Salt (59.5% LiCl, 40.5% KCl)
		density = 1E-09*T*T*T - 5E-06*T*T - 0.864*T + 2112.6;
		break;
	case 16: //   16.) Salt (59.5% NaF, 40.5% ZrF4)
		density = -5E-09*T*T*T + 2E-05*T*T - 0.9144*T + 3837.0;
		break;
	case 17: //   17.) Salt (60% NaNO3, 40% KNO3)
		density = -1E-07*T*T*T + 0.0002*T*T - 0.7875*T + 2299.4;
		density = MAX(density, 1000.0);
		break;
		/*
		case(18:25) !  18-25.) Call trough properties
		density = Dens_fluid((T-273.15),int(Fnumd)) !Trough calcs take fluid properties in degC, not K
		*/
	case 18: //   18.) Nitrate Salt** type 805 dens_salt(Tc)
		density = 2090 - 0.636 * tempC;
		density = MAX(density, 1000.0);
		break;
	case 19: //   19.) Caloria HT 43** type 805 dens_caloria(tempC)
		density = 885 - 0.6617 * tempC - 0.0001265 * tempC*tempC;
		density = MAX(density, 100.0);
		break;
	case 20: //   20.) Hitec XL** type 805 dens_salt_xl(tempC)
		density = 2240 - 0.8266 * tempC;
		density = MAX(density, 800.0);
		break;
	case 21: //   21.) Therminol VP-1** type 805 dens_therminol(tempC)
		density = 1074.0 - 0.6367 * tempC - 0.0007762 * tempC*tempC;
		density = MAX(density, 400.0);
		break;
	case 22: //   22.) Hitec** type 805 dens_salt_hitec(tempC)
		density = 2080 - 0.733 * tempC;
		density = MAX(density, 1000.0);
		break;
	case 23: //   23.) Dowtherm Q** type 805 dens_Dowtherm_Q(tempC)
		density = -0.757332 * tempC + 980.787;
		density = MAX(density, 100.0);
		break;
	case 24: //   24.) Dowtherm RP** type 805 dens_Dowtherm_RP(tempC)
		//density = -0.000186495 * tempC*tempC - 0.668337 * T + 1042.11;
		density = -0.000186495 * tempC*tempC - 0.668337 * tempC + 1042.11; // changed from above when the function was added to SAMnt (as per email from Ty, Feb 23, 2014)
		density = MAX(density, 200.0);
		break;
	case 25: //   25.) Salt XL** type 805 dens_salt_xl(tempC)
		density = 2240 - 0.8266 * tempC;
		density = MAX(density, 800.0);
		break;
	case 26: //   26.) Argon (ideal gas properties)
		density = P / (208.13*T);
		density = MAX(density, 1e-10);
		break;
	case 27: //   27.) Hydrogen (ideal gas properties)
		density = P / (4124.*T);
		density = MAX(density, 1e-10);
		break;
	case 28: //   28.) T-91 Steel: "Thermo hydraulic optimisation of the EURISOL DS target" - Paul Scherrer Institut
		density = -0.3289*tempC + 7742.5;
		break;
	case 29: //   29.) Therminol 66: Reference: Therminol Reference Disk by Solutia: www.therminol.com/pages/tools/toolscd.asp
		density = -0.7146*tempC + 1024.8;
		break;
	case 30: //   30.) Therminol 59: Reference: Therminol Reference Disk by Solutia: www.therminol.com/pages/tools/toolscd.asp
		density = -0.0003*tempC*tempC - 0.6963*tempC + 988.44;
		break;
	case 31: //   31.) -blank-
	case 32: //   32.) -blank-
	case 33: //   33.) -blank-
	case 34: //   34.) -blank-
	case 35: //   35.) -blank-
		break;
	case 36: //   36+) User specified (lookup tables)
		/*
		!Call the user-defined property table
		lb=fl_bounds(fnum-35)
		ub=fl_bounds(fnum-35+1)-1
		if(ub.lt.lb) ub=size(fprop(1,:))
		dxx(:)=fprop(1,lb:ub)
		dyy(:)=fprop(3,lb:ub)
		call interp(T,size(dxx),dxx,dyy,Gjsav,Density)
		if((Gjsav.eq.ub).or.(Gjsav.eq.lb)) dum=t_warn(T,dxx(lb),dxx(ub),"User-specified fluid")
		*/
		break;
	}
	return density;
}

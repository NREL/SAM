#include <wx/datstrm.h>

#include <wex/utils.h>

#include "case.h"
#include "equations.h"
#include "main.h"
#include "library.h"
#include "invoke.h"

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
	env.register_funcs( invoke_other_funcs() );
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

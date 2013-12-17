#include <wx/datstrm.h>

#include "case.h"
#include "equations.h"
#include "main.h"

Case::Case()
	: m_eqns( &SamApp::Equations() ) // initialize fast lookup with global all equations
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
		
		m_technology.Clear();
		m_financing.Clear();
		SetConfiguration( rhs->m_technology, rhs->m_financing );
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

	// write data
	out.WriteString( m_technology );
	out.WriteString( m_financing );
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
	m_technology = tech;
	m_financing = fin;

	// erase results
	m_baseCase.clear();
	
	// update equation lookup and variable info caches
	m_vars = SamApp::Config().GetVariables( m_technology, m_financing );
	m_eqns = SamApp::Config().GetEquations( m_technology, m_financing );
	
	// erase all input variables that are no longer in the current configuration
	wxArrayString to_remove;
	for( VarTable::iterator it = m_vals.begin(); it != m_vals.end(); ++it )
		if ( m_vars.find( it->first ) == m_vars.end() )
			to_remove.Add( it->first );

	m_vals.Delete( to_remove );

	// set up any new variables with internal default values
	for( VarInfoLookup::iterator it = m_vars.begin(); it != m_vars.end(); ++it )
		if ( !m_vals.Get( it->first ) )
			m_vals.Set( it->first, it->second->DefaultValue ); // will create new variable if it doesnt exist
		
	// reevalute all equations
	EqnEvaluator eval( m_vals, m_eqns );
	eval.CalculateAll();
	
	// update UI
	SendEvent( CaseEvent( CaseEvent::CONFIG_CHANGED, tech, fin ) );
}

void Case::GetConfiguration( wxString *tech, wxString *fin )
{
	if ( tech ) *tech = m_technology;
	if ( fin ) *fin = m_financing;
}

int Case::Changed( const wxString &var )
{
	EqnEvaluator eval( m_vals, m_eqns );
	int n = eval.Changed( var );	
	if ( n > 0 ) SendEvent( CaseEvent( CaseEvent::VARS_CHANGED, eval.GetUpdated() ) );
	else if ( n < 0 ) wxLogStatus( wxJoin( eval.GetErrors(), '\n' )  );
	return n;

}

int Case::CalculateAll()
{
	EqnEvaluator eval( m_vals, m_eqns );
	int n = eval.CalculateAll();
	if ( n > 0 ) SendEvent( CaseEvent( CaseEvent::VARS_CHANGED, eval.GetUpdated() ) );
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
#include <wx/datstrm.h>
#include "case.h"

Case::Case()
{
	m_caseWin = 0;
	m_name = wxT( "untitled" );
}

Case::~Case()
{
	// nothing to do
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
		m_name = rhs->m_name;
		m_technology = rhs->m_technology;
		m_financing = rhs->m_financing;
		m_vars.Copy( rhs->m_vars );
		m_baseCase.Copy( rhs->m_vars );
		m_properties = rhs->m_properties;
		m_notes = rhs->m_notes;
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
	out.WriteString( m_name );
	out.WriteString( m_technology );
	out.WriteString( m_financing );
	m_vars.Write( _o );
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
	m_name = in.ReadString();
	m_technology = in.ReadString();
	m_financing = in.ReadString();
	m_vars.Read( _i );
	m_baseCase.Read( _i );
	m_properties.Read( _i );
	m_notes.Read( _i );

	return (in.Read8() == code);
}

void Case::SetConfiguration( const wxString &tech, const wxString &fin )
{
	if ( m_technology == tech && m_financing == fin )
		return;

	m_technology = tech;
	m_financing = fin;

	// erase all input variables
	m_vars.clear();

	// erase results
	m_baseCase.clear();

	// look up configuration

	// set up all default variables and values

	// reevalute all equations
}

void Case::GetConfiguration( wxString *tech, wxString *fin )
{
	if ( tech ) *tech = m_technology;
	if ( fin ) *fin = m_financing;
}
#include <wx/datstrm.h>
#include "project.h"
#include "main.h"


Case::Case()
{
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
		m_vars.Copy( rhs->m_vars, true );
		m_baseCase.Copy( rhs->m_vars, true );
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
	m_vars.Clear();

	// erase results
	m_baseCase.Clear();

	// look up configuration

	// set up all default variables and values

	// reevalute all equations
}

void Case::GetConfiguration( wxString *tech, wxString *fin )
{
	if ( tech ) *tech = m_technology;
	if ( fin ) *fin = m_financing;
}

ProjectFile::ProjectFile()
{
}

ProjectFile::~ProjectFile()
{
	Clear();
}

void ProjectFile::Clear()
{
	m_cases.Clear();
	m_objects.Clear();
	m_properties.clear();
}


// managing cases
void ProjectFile::AddCase( const wxString &name, Case *c )
{
	m_cases.Add( name, c );
}

bool ProjectFile::DeleteCase( const wxString &name )
{
	return m_cases.Delete( name );
}

wxArrayString ProjectFile::GetCases()
{
	return m_cases.GetNames();
}

Case *ProjectFile::GetCase( const wxString &name )
{
	return dynamic_cast<Case*>( m_cases.Lookup( name ) );
}

// simple project file properties
wxString ProjectFile::GetProperty( const wxString &name )
{
	if ( m_properties.find( name ) != m_properties.end() )
		return m_properties[ name ];
	else
		return wxEmptyString;
}

void ProjectFile::SetProperty( const wxString &name, const wxString &value )
{
	m_properties[ name ] = value;
}

wxArrayString ProjectFile::GetProperties()
{
	wxArrayString list;
	for ( StringHash::iterator it = m_properties.begin();
		it != m_properties.end();
		++it )
		list.Add( it->first );
	return list;
}

// general purpose data storage objects
void ProjectFile::AddObject( const wxString &name, Object *obj )
{
	m_objects.Add( name, obj );
}

void ProjectFile::DeleteObject( const wxString &name )
{
	m_objects.Delete( name );
}

Object *ProjectFile::GetObject( const wxString &name )
{
	return m_objects.Lookup( name );
}

wxArrayString ProjectFile::GetObjects()
{
	return m_objects.GetNames();
}

void ProjectFile::Write( wxOutputStream &output )
{
	wxArrayString list;
	wxDataOutputStream out(output);

	out.Write16( 0x3c ); // identifier code
	out.Write16( 1 ); // data format version
	out.Write16( SamApp::VersionMajor() );
	out.Write16( SamApp::VersionMinor() );
	
	m_properties.Write( output );
	m_cases.Write( output );
	m_objects.Write( output );
	
	out.Write16( 0x3c ); // identifier code to finish
} 

bool ProjectFile::Read( wxInputStream &input )
{
	Clear();

	wxDataInputStream in(input);

	wxUint16 code = in.Read16();
	wxUint16 ver = in.Read16();

	int major = (int)in.Read16();
	int minor = (int)in.Read16();

	// todo: check version numbers of file

	m_properties.Read( input );
	m_cases.Read( input );
	m_objects.Read( input );

	return (in.Read16() == code );
}

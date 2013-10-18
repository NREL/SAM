#include <wx/datstrm.h>

#include "project.h"
#include "case.h"
#include "main.h"

ProjectFile::ProjectFile()
{
	m_modified = false;
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
	m_modified = true;
}

Case *ProjectFile::AddCase( const wxString &name )
{
	Case *c = new Case;
	AddCase( name, c );
	return c;
}

bool ProjectFile::DeleteCase( const wxString &name )
{
	if ( m_cases.Delete( name ) )
	{
		m_modified = true;
		return true;
	}
	else return false;
}

bool ProjectFile::RenameCase( const wxString &old_name, const wxString &new_name )
{
	return m_cases.Rename( old_name, new_name );
}

wxArrayString ProjectFile::GetCases()
{
	return m_cases.GetNames();
}

Case *ProjectFile::GetCase( const wxString &name )
{
	return dynamic_cast<Case*>( m_cases.Lookup( name ) );
}

wxString ProjectFile::GetCaseName( Case *c )
{
	for( ObjectCollection::iterator it = m_cases.begin();
		it != m_cases.end();
		++it )
		if ( it->second == c )
			return it->first;

	return wxEmptyString;
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
	m_modified = true;
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
	m_modified = true;
}

void ProjectFile::DeleteObject( const wxString &name )
{
	m_objects.Delete( name );
	m_modified = true;
}

bool ProjectFile::RenameObject( const wxString &old_name, const wxString &new_name )
{
	return m_cases.Rename( old_name, new_name );
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
	out.Write16( SamApp::VersionMicro() );
	
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
	int micro = (int)in.Read16();

	// todo: check version numbers of file

	m_properties.Read( input );
	m_cases.Read( input );
	m_objects.Read( input );

	m_modified = false;
	return (in.Read16() == code );
}

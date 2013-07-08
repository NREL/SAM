#include <wx/datstrm.h>
#include "project.h"
#include "main.h"

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
	for ( pfStringHash::iterator it = m_properties.begin();
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
	
	out.Write32( m_properties.size() );
	for ( pfStringHash::iterator it = m_properties.begin();
		it != m_properties.end();
		++it )
	{
		out.WriteString( it->first );
		out.WriteString( it->second );
	}

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

	size_t n = in.Read32();
	for (size_t i=0;i<n;i++)
	{
		wxString name = in.ReadString();
		wxString value = in.ReadString();
		m_properties[name] = value;
	}

	m_cases.Read( input );
	m_objects.Read( input );

	return (in.Read16() == code );
}

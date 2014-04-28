#include <wx/datstrm.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>

#include "project.h"
#include "case.h"
#include "main.h"

ProjectFile::ProjectFile()
{
	m_modified = false;
}

ProjectFile::~ProjectFile()
{
	ClearListeners();
	Clear();
}

void ProjectFile::Clear()
{
	m_cases.Clear();
	m_objects.Clear();
	m_properties.clear();
}


void ProjectFile::AddListener(ProjectFileEventListener *pel)
{
	m_listeners.push_back(pel);
}

void ProjectFile::RemoveListener(ProjectFileEventListener *pel)
{
	std::vector<ProjectFileEventListener*>::iterator it = std::find(m_listeners.begin(), m_listeners.end(), pel);
	if (it != m_listeners.end())
		m_listeners.erase(it);
}


void ProjectFile::ClearListeners()
{
	m_listeners.clear();
}

void ProjectFile::SendEvent(ProjectFileEvent e)
{
	for (size_t i = 0; i<m_listeners.size(); i++)
		m_listeners[i]->OnProjectFileEvent(this, e);
}


// managing cases
void ProjectFile::AddCase( const wxString &name, Case *c )
{
	m_cases.Add( name, c );
	SendEvent(ProjectFileEvent(ProjectFileEvent::CASE_ADDED, name));
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
	SendEvent(ProjectFileEvent(ProjectFileEvent::CASE_DELETED, name));
	if (m_cases.Delete(name))
	{
		m_modified = true;
		return true;
	}
	else return false;
}

bool ProjectFile::RenameCase( const wxString &old_name, const wxString &new_name )
{
	if (m_cases.Rename(old_name, new_name))
	{
		SendEvent(ProjectFileEvent(ProjectFileEvent::CASE_RENAMED, old_name, new_name));
		return true;
	}
	else return false;
}

std::vector<Case*> ProjectFile::GetCases()
{
	return m_cases.GetObjects<Case>();
}

wxArrayString ProjectFile::GetCaseNames()
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
	m_lastError.Clear();
	Clear();

	wxDataInputStream in(input);

	wxUint16 code = in.Read16();
	wxUint16 ver = in.Read16();
	int major = (int)in.Read16();
	int minor = (int)in.Read16();
	int micro = (int)in.Read16();

	// todo: check version numbers of file

	if ( !m_properties.Read( input ) ) m_lastError = "could not read project properties";
	if ( !m_cases.Read( input ) ) m_lastError = "could not read case data" ;
	if ( !m_objects.Read( input ) ) m_lastError = "could not read objects" ;

	m_modified = false;
	return (in.Read16() == code );
}

bool ProjectFile::WriteArchive( const wxString &file )
{
	wxFFileOutputStream out( file );
	if ( ! out.IsOk() ) return false;
	wxZlibOutputStream zout( out );
	Write( out );
	return true;
}

bool ProjectFile::ReadArchive( const wxString &file )
{
	wxFFileInputStream in( file );
	if ( !in.IsOk() ) return false;
	wxZlibInputStream zin( in );
	return Read( in );
}
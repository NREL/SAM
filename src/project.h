#ifndef __project_h
#define __project_h

#include <vector>

#include "object.h"

class Case;
class ProjectFile;

class ProjectFileEvent
{
private:
	int m_type;
	wxString m_str, m_str2;
public:
	enum { CASE_ADDED, CASE_DELETED, CASE_RENAMED };

	ProjectFileEvent(int type) : m_type(type) { }
	ProjectFileEvent(int type, const wxString &str) : m_type(type), m_str(str) { }
	ProjectFileEvent(int type, const wxString &str, const wxString &str2) : m_type(type), m_str(str), m_str2(str2) { }

	int GetType() { return m_type; }
	wxString GetString() { return m_str; }
	wxString GetString2() { return m_str2; }
};

class ProjectFileEventListener
{
public:
	virtual void OnProjectFileEvent(ProjectFile *, ProjectFileEvent &) = 0;
};


class ProjectFile
{
public:
	ProjectFile();
	virtual ~ProjectFile();

	void Clear();
	
	// managing cases
	void AddCase( const wxString &name, Case *c );
	Case *AddCase( const wxString &name );
	bool DeleteCase( const wxString &name );
	Case *GetCase( const wxString &name );
	wxString GetCaseName( Case *c );
	wxArrayString GetCaseNames();
	std::vector<Case*> GetCases();
	bool RenameCase( const wxString &old_name, const wxString &new_name );

	// simple project file properties
	wxString GetProperty( const wxString &name );
	void SetProperty( const wxString &name, const wxString &value );
	wxArrayString GetProperties();

	// general purpose data storage objects
	void AddObject( const wxString &name, Object * );
	void DeleteObject( const wxString &name );
	bool RenameObject( const wxString &old_name, const wxString &new_name );
	Object *GetObject( const wxString &name );
	wxArrayString GetObjects();

	void Write( wxOutputStream &out );
	bool Read( wxInputStream &in );

	bool WriteArchive( const wxString &file );
	bool ReadArchive( const wxString &file );

	wxString GetLastError() { return m_lastError; }

	bool IsModified() { return m_modified; }
	void SetModified( bool b ) { m_modified = b; }

	void AddListener(ProjectFileEventListener *pel);
	void RemoveListener(ProjectFileEventListener *pel);
	void ClearListeners();
	void SendEvent(ProjectFileEvent e);

private:
	ObjectCollection m_cases;
	ObjectCollection m_objects;
	StringHash m_properties;
	wxString m_lastError;
	bool m_modified;
	std::vector<ProjectFileEventListener*> m_listeners;
};


#endif


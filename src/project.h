#ifndef __project_h
#define __project_h

#include <vector>

#include "object.h"

class Case;

class ProjectFile
{
public:
	ProjectFile();
	virtual ~ProjectFile();

	void Clear();

	// managing cases
	void AddCase( const wxString &name, Case *c );
	bool DeleteCase( const wxString &name );
	wxArrayString GetCases();
	Case *GetCase( const wxString &name );

	// simple project file properties
	wxString GetProperty( const wxString &name );
	void SetProperty( const wxString &name, const wxString &value );
	wxArrayString GetProperties();

	// general purpose data storage objects
	void AddObject( const wxString &name, Object * );
	void DeleteObject( const wxString &name );
	Object *GetObject( const wxString &name );
	wxArrayString GetObjects();

	void Write( wxOutputStream &out );
	bool Read( wxInputStream &in );

	wxString LastError() { return m_lastError; }

	bool IsModified() { return m_modified; }
	void SetModified( bool b ) { m_modified = b; }

private:
	ObjectCollection m_cases;
	ObjectCollection m_objects;
	StringHash m_properties;
	wxString m_lastError;
	bool m_modified;
};


#endif


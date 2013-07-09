#ifndef __project_h
#define __project_h

#include <vector>

#include "object.h"


class Case : public Object
{
public:
	Case() { }
	virtual ~Case() { }

	wxString GetName() { return "no name"; }
	
	virtual Object *Duplicate() = 0;
	virtual bool Copy( Object *obj ) = 0;
	virtual wxString GetTypeName() = 0;
	virtual void Write( wxOutputStream & ) = 0;
	virtual bool Read( wxInputStream & ) = 0;

private:
};

class ProjectFile
{
	typedef unordered_map<wxString, wxString, wxStringHash, wxStringEqual> pfStringHash;

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

private:
	ObjectCollection m_cases;
	ObjectCollection m_objects;
	pfStringHash m_properties;
	wxString m_lastError;
};


#endif


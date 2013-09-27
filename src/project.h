#ifndef __project_h
#define __project_h

#include <vector>

#include "object.h"
#include "variables.h"


class Case : public Object
{
public:
	Case();
	virtual ~Case();

	virtual Object *Duplicate();
	virtual bool Copy( Object *obj );
	virtual wxString GetTypeName();
	virtual void Write( wxOutputStream & );
	virtual bool Read( wxInputStream & );


	wxString GetName() { return m_name; }
	void SetName( const wxString &s ) { m_name = s; }
	void SetConfiguration( const wxString &tech, const wxString &fin );
	void GetConfiguration( wxString *tech, wxString *fin );	
	VarTable &Vars() { return m_vars; }

	int Changed( const wxString &name );

	VarTable &BaseCase();

	StringHash &Properties() { return m_properties; }
	StringHash &Notes() { return m_notes; }

private:
	wxString m_name;
	wxString m_technology;
	wxString m_financing;
	VarTable m_vars;
	VarTable m_baseCase;
	StringHash m_properties;
	StringHash m_notes;

};

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

private:
	ObjectCollection m_cases;
	ObjectCollection m_objects;
	StringHash m_properties;
	wxString m_lastError;
};


#endif


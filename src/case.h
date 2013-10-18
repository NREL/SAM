#ifndef __case_h
#define __case_h

#include <vector>

#include "object.h"
#include "variables.h"

// case events allow the user interface to be updated
// when something internal in the case changes that needs to be reflected
// to the user, i.e. variables are recalculated...

class Case;

class CaseEvent
{
private:
	int m_type;
	wxArrayString m_vars;
	wxString m_str, m_str2;
public:
	enum { VARS_CHANGED, CONFIG_CHANGED };

	CaseEvent(  int type ) : m_type(type) { }
	CaseEvent( int type, const wxString &str ) : m_type(type), m_str(str) { }
	CaseEvent( int type, const wxString &str1, const wxString &str2 ) : m_type(type), m_str(str1), m_str2(str2) { }
	CaseEvent( int type, const wxArrayString &vars ) : m_type(type), m_vars(vars) { }

	int GetType() { return m_type; }
	wxString GetString() { return m_str; }
	wxString GetString2() { return m_str2; }
	wxArrayString &GetVars() { return m_vars; }
};

class CaseEventListener
{
public:
	virtual void OnCaseEvent( Case *, CaseEvent & ) = 0;
};

class Case : public Object
{
public:
	Case();
	Case( const wxString &tech, const wxString &fin );
	virtual ~Case();

	virtual Object *Duplicate();
	virtual bool Copy( Object *obj );
	virtual wxString GetTypeName();
	virtual void Write( wxOutputStream & );
	virtual bool Read( wxInputStream & );
	
	void SetConfiguration( const wxString &tech, const wxString &fin );
	void GetConfiguration( wxString *tech, wxString *fin );	
	VarTable &Vars() { return m_vars; }

	int Changed( const wxString &name );
	int CalculateAll();

	VarTable &BaseCase();

	StringHash &Properties() { return m_properties; }
	StringHash &Notes() { return m_notes; }

	void AddListener( CaseEventListener *cel );
	void ClearListeners();

private:
	wxString m_technology;
	wxString m_financing;
	VarTable m_vars;
	VarTable m_baseCase;
	StringHash m_properties;
	StringHash m_notes;
	std::vector<CaseEventListener*> m_listeners;
	void SendEvent( CaseEvent e );

};

#endif
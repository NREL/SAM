#ifndef __case_h
#define __case_h

#include <vector>

#include "object.h"
#include "variables.h"

class CaseWindow;

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

	void SetCaseWindow( CaseWindow *cw ) { m_caseWin = cw; }
	CaseWindow *GetCaseWindow() { return m_caseWin; }


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
	CaseWindow *m_caseWin;
	wxString m_name;
	wxString m_technology;
	wxString m_financing;
	VarTable m_vars;
	VarTable m_baseCase;
	StringHash m_properties;
	StringHash m_notes;

};

#endif
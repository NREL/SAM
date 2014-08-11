#ifndef __defmgr_h
#define __defmgr_h

#include <wx/panel.h>
#include <wx/arrstr.h>

class wxTextCtrl;
class wxCheckListBox;
class wxCheckBox;

class DefaultsManager : public wxPanel
{
public:
	DefaultsManager( wxWindow *parent );

private:
	wxTextCtrl *m_varName;
	wxTextCtrl *m_value;
	wxTextCtrl *m_output;
	wxCheckListBox *m_configList;
	wxArrayString m_techList, m_finList;
	wxChoice *m_dataType;
	wxCheckBox *m_enableAdd;

	
	void ClearLog();
	void Log(const wxString &s);

	void OnQuery(wxCommandEvent &evt);
	void OnModify(wxCommandEvent &evt);
	void OnLookupVar( wxCommandEvent &evt );
	void OnDeleteVar( wxCommandEvent &evt );

	wxString LookupVariable();

	DECLARE_EVENT_TABLE();
};

#endif

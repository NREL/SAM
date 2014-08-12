#ifndef __defmgr_h
#define __defmgr_h

#include <wx/panel.h>
#include <wx/arrstr.h>

class wxTextCtrl;
class wxCheckListBox;
class wxCheckBox;
class wxButton;
class wxStaticTextCtrl;
class wxChoice;
class AFDataMatrixCtrl;

class ValueEditor : public wxPanel
{
public:
	ValueEditor( wxWindow *parent );

	int GetType();
	void Set( const VarValue &vv );
	VarValue Get();

private:
	wxChoice *m_type;
	wxTextCtrl *m_text;
	AFDataMatrixCtrl *m_matrix;
	wxListBox *m_fields;
	wxButton *m_addField, *m_removeField, *m_editField, *m_clearTable;
	wxStaticText *m_valLabel, *m_tabLabel;
	
	void ValueToForm();
	void UpdateFormUI();
	void OnCommand( wxCommandEvent & );
	void OnEditField( wxCommandEvent & );

	VarValue m_val;

	DECLARE_EVENT_TABLE();
};


class DefaultsManager : public wxPanel
{
public:
	DefaultsManager( wxWindow *parent );

private:
	wxTextCtrl *m_varName;
	ValueEditor *m_value;
	
	wxTextCtrl *m_output;
	wxCheckListBox *m_configList;
	wxArrayString m_techList, m_finList;
	wxCheckBox *m_changeType;
	wxCheckBox *m_enableAdd;

	
	void ClearLog();
	void Log(const wxString &s);

	void OnQuery(wxCommandEvent &evt);
	void OnModify(wxCommandEvent &evt);
	void OnLoad(wxCommandEvent &evt);
	void OnLookupVar( wxCommandEvent &evt );
	void OnDeleteVar( wxCommandEvent &evt );

	wxString LookupVariable();

	DECLARE_EVENT_TABLE();
};

#endif

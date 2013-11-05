#ifndef __samide_h
#define __samide_h

#include <wx/frame.h>
#include <wex/uiform.h>

#include "variables.h"

class wxListBox;
class wxMetroNotebook;
class wxExtTextCtrl;
class wxLKScriptCtrl;

class IDEWindow : public wxFrame
{
public:
	IDEWindow( wxWindow *parent );
	virtual ~IDEWindow();

private:
	void OnClose( wxCloseEvent & );
	void OnCommand( wxCommandEvent & );

	void LoadFormList( const wxString &sel = wxEmptyString );

	wxString m_formName;
	wxUIFormData m_formData;
	VarDatabase m_varData;
	bool WriteForm( const wxString &name );
	bool LoadForm( const wxString &name );

	wxListBox *m_varList;
	wxExtTextCtrl *m_varName;
	wxChoice *m_varType;
	wxExtTextCtrl *m_varLabel, *m_varUnits, *m_varGroup, *m_varIndexLabels, *m_varDefaultValue;
	wxCheckBox *m_varFlagHideLabels, *m_varFlagParametric, *m_varFlagIndicator;

	VarInfo *m_curVar;
	void FormToVarInfo( VarInfo * );
	void VarInfoToForm( VarInfo * );
	void LoadVarList( const wxString &sel = wxEmptyString );



	wxLKScriptCtrl *m_scriptCtrl;

	wxListBox *m_formList;
	wxUIFormDesigner *m_uiFormEditor;
	wxUIPropertyEditor *m_uiPropEditor;

	wxMetroNotebook *m_notebook;

	DECLARE_EVENT_TABLE();
};

#endif

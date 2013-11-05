#ifndef __samide_h
#define __samide_h

#include <wx/frame.h>
#include <wex/uiform.h>

#include "variables.h"

class wxListBox;
class wxMetroNotebook;
class wxExtTextCtrl;
class wxLKScriptCtrl;

class StartupScriptPanel : public wxPanel
{
public:
	StartupScriptPanel( wxWindow *parent );
private:
	void OnCommand( wxCommandEvent & );

	wxLKScriptCtrl *m_scriptCtrl;

	DECLARE_EVENT_TABLE();
};

class UIEditorPanel : public wxPanel
{
public:
	UIEditorPanel( wxWindow *parent );

private:
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

	void SyncFormUIToDataBeforeWriting();



	wxListBox *m_formList;
	wxUIFormDesigner *m_uiFormEditor;
	wxUIPropertyEditor *m_uiPropEditor;
	
	DECLARE_EVENT_TABLE();
};

class IDEWindow : public wxFrame
{
public:
	IDEWindow( wxWindow *parent );
	virtual ~IDEWindow();

private:
	void OnClose( wxCloseEvent & );
	wxMetroNotebook *m_notebook;

	StartupScriptPanel *m_startupPanel;
	UIEditorPanel *m_uiPanel;

	DECLARE_EVENT_TABLE();
};

#endif

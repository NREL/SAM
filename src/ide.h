#ifndef __samide_h
#define __samide_h

#include <wx/frame.h>
#include <wex/uiform.h>

#include "variables.h"
#include "main.h"
#include "defaultsmanager.h"

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

/*
class SimulationScriptPanel : public wxPanel
{
public:
	SimulationScriptPanel( wxWindow *parent );
private:
	void OnCommand( wxCommandEvent & );
	void SaveCurrent();
	void RefreshList();

	wxString m_currentFile;
	wxChoice *m_simList;
	wxLKScriptCtrl *m_scriptCtrl;

	DECLARE_EVENT_TABLE();
};*/


class ExFormData : public wxUIFormData
{
private:
	VarDatabase *m_vdb;
public:
	ExFormData( VarDatabase *vdb );	
	virtual ~ExFormData();
	virtual bool GetMetaData( const wxString &name,
		wxString *label, wxString *units, wxColour *colour );
};

class UIEditorPanel : public wxPanel
{
public:
	UIEditorPanel( wxWindow *parent );
	
	wxUIFormData *GetFormData() { return &m_exForm; }
	VarDatabase *GetVars() { return &m_ipd.Variables(); }
	wxString GetCallbacks() { return m_callbackScript->GetText(); }
	void SetCallbacks( const wxString &t ) { m_callbackScript->SetText( t ); }
	wxString GetEquations() { return m_equationScript->GetText(); }
	void SetEquations( const wxString &e ) { m_equationScript->SetText( e ); }
	
	bool Write( const wxString &name );
	bool Load( const wxString &name );
	void LoadFormList( const wxString &sel = wxEmptyString );
	void LoadVarList( const wxString &sel = wxEmptyString );
	wxUIFormDesigner *GetDesigner() { return m_uiFormEditor; }
	wxUIPropertyEditor *GetPropertyEditor() { return m_uiPropEditor; }

	void FormToVarInfo( );
	void VarInfoToForm( const wxString &name );

private:
	wxArrayString m_callbackGotoList;
	void OnCallbackGoto( wxCommandEvent & );
	void OnFormTest( wxCommandEvent & );
	void OnCommand( wxCommandEvent & );
	void OnFormSelectObject( wxUIFormEvent & );

	wxString m_formName;
	ExFormData m_exForm;
	InputPageData m_ipd;
	

	VarDatabase m_varCopyBuffer;

	wxCheckListBox *m_varList;
	wxExtTextCtrl *m_varName;
	wxChoice *m_varType;
	wxExtTextCtrl *m_varLabel, *m_varUnits, *m_varGroup, *m_varIndexLabels, *m_varDefaultValue;
	wxCheckBox *m_varFlagHideLabels, *m_varFlagParametric, *m_varFlagIndicator, 
		*m_varFlagCalculated, *m_varFlagLibrary;

	wxString m_curVarName;

	void SyncFormUIToDataBeforeWriting();

	wxLKScriptCtrl *m_callbackScript;
	wxLKScriptCtrl *m_equationScript;


	wxListBox *m_formList;
	wxUIFormDesigner *m_uiFormEditor;
	wxUIPropertyEditor *m_uiPropEditor;
	wxUIObjectCopyBuffer m_uiCopyBuffer;
	
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

	DefaultsManager *m_defaultsmanager;

	StartupScriptPanel *m_startupPanel;
	UIEditorPanel *m_uiPanel;
	//SimulationScriptPanel *m_simPanel;

	DECLARE_EVENT_TABLE();
};

#endif

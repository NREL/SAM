/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef __samide_h
#define __samide_h

#include <wx/frame.h>
#include <wex/uiform.h>

#include "variables.h"
#include "main.h"

class wxListBox;
class wxMetroNotebook;
class wxExtTextCtrl;
class wxLKScriptCtrl;

class SamReportWindow;

class ScriptPanel : public wxPanel
{
public:
	ScriptPanel( wxWindow *parent, const wxString &script_file_name );
	void AddLibrary( lk::fcall_t *, const wxString &name );
private:
	void OnCommand( wxCommandEvent & );
	wxString m_fileName;
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
	
	bool Write(const wxString &name);
	bool Load(const wxString &name);
	bool Write_text(const wxString &name);
	bool Load_text(const wxString &name);
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
	void OnTextFind( wxCommandEvent & );

	wxString m_formName;
	InputPageData m_ipd;
	ExFormData m_exForm;
	

	VarDatabase m_varCopyBuffer;

	wxCheckListBox *m_varList;
	wxExtTextCtrl *m_varName;
	wxChoice *m_varType;
	wxChoice *m_varUIObject;
	wxExtTextCtrl *m_varLabel, *m_varUnits, *m_varGroup, *m_varIndexLabels, *m_varDefaultValue;
	wxCheckBox *m_varFlagHideLabels, *m_varFlagParametric, *m_varFlagIndicator, 
		*m_varFlagCalculated, *m_varFlagLibrary, *m_varFlagChangeModel;

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

class DefaultsManager;

class IDEWindow : public wxFrame
{
public:
	IDEWindow( wxWindow *parent );
	virtual ~IDEWindow();

private:
	void OnClose( wxCloseEvent & );
	wxMetroNotebook *m_notebook;

	ScriptPanel *m_startupPanel;
	UIEditorPanel *m_uiPanel;
	ScriptPanel *m_metricsPanel;
	ScriptPanel *m_cashFlowPanel;
	ScriptPanel *m_autoGraphPanel;
	ScriptPanel *m_lossDiagramPanel;
	SamReportWindow *m_reportEditorPanel;
	DefaultsManager *m_defaultsMgr;
	ScriptPanel *m_versionPanel;

	//SimulationScriptPanel *m_simPanel;

	DECLARE_EVENT_TABLE();
};

#endif

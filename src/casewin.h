/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef __casewin_h
#define __casewin_h

#include <vector>

#include <wx/splitter.h>
#include <wx/minifram.h>
#include <wx/notebook.h>
#include <wx/treectrl.h>

#include "case.h"
#include "inputpage.h"
#include "main.h"

class wxPanel;
class wxMetroButton;
class wxMetroNotebook;
class wxSimplebook;
class wxLKScriptCtrl;
class InputPageList;
class ResultsViewer;
class Simulation;

class PageNote;
class CollapsePaneCtrl;
class wxUIObject;
class ParametricViewer;
class StochasticPanel;
class P50P90Form;
class MacroPanel;
struct InputPageGroup;

class CaseWindow : public wxSplitterWindow, CaseEventListener
{
public:
	CaseWindow( wxWindow *parent, Case *c );
	virtual ~CaseWindow();

	Case *GetCase() { return m_case; }

	void UpdateConfiguration();

	bool SwitchToInputPage( const wxString &name );
	wxArrayString GetInputPages();
	wxUIObject *FindActiveObject( const wxString &name, ActiveInputPage **page = 0 );

	bool SwitchToPage( const wxString &name ); // can navigate to results, parametrics, as well as input pages

	wxString GetCurrentContext();
	void UpdatePageNote();
	bool HasPageNote( const wxString &id );
	void ShowPageNote( );
	void SetPageNote( const wxString &text );

	void SaveCurrentViewProperties();

	bool RunBaseCase( bool silent = false, wxString *messages = 0 );
	void UpdateResults();

	bool GenerateReport( 
		wxString pdffile = wxEmptyString, 
		wxString templfile = wxEmptyString,
		VarValue *metadata = 0
		);
	
private:
	Case *m_case;
	
	InputPageList *m_inputPageList;
	std::vector<InputPageGroup*> m_pageGroups;
	UIFormDatabase m_forms;
	InputPageGroup *m_currentGroup;
	std::vector<wxUIFormData*> m_currentForms;

	struct PageDisplayState {
		PageDisplayState() {
			Form = 0;
			ActivePage = 0;
			CollapseCheck = 0;
			Collapsible = false;
			HeaderPage = false;
		}

		wxUIFormData *Form;
		ActiveInputPage *ActivePage;
		bool Collapsible;
		wxString CollapsibleVar;
		CollapsePaneCtrl *CollapseCheck;
		bool HeaderPage;
	};

	std::vector<PageDisplayState*> m_currentActivePages;
	wxScrolledWindow *m_inputPageScrollWin;
	wxPanel *m_exclPanel;
	wxBoxSizer *m_exclPanelSizer;
	wxMetroButton *m_exclPageButton;
	wxMetroTabList *m_exclPageTabList;
	void UpdatePageListForConfiguration( const std::vector<PageInfo> &pages, ConfigInfo *cfg );
	void LoadPageList( const std::vector<PageInfo> &list, bool header );
	void SetupActivePage();
	void LayoutPage();
	void DetachCurrentInputPage();

	wxStaticText *m_configLabel;
	wxMetroButton *m_simButton, *m_resultsButton;

	wxSimplebook *m_pageFlipper;
	ResultsViewer *m_baseCaseResults;
	ParametricViewer *m_parametrics;
	StochasticPanel *m_stochastic;
	P50P90Form *m_p50p90;
	MacroPanel *m_macros;

	PageNote *m_pageNote;
	wxString m_lastPageNoteId;

	void OnCommand( wxCommandEvent & );	
	virtual void OnCaseEvent( Case *, CaseEvent & );
	void OnSubNotebookPageChanged( wxNotebookEvent &evt );


	DECLARE_EVENT_TABLE();
};


class PageNote : public wxMiniFrame
{
public:
	PageNote(CaseWindow *cwin);
	void SetText(const wxString &t);
	wxString GetText();
	wxTextCtrl *GetTextCtrl();
private:
	wxTextCtrl *m_text;
	void OnHideClose(wxCloseEvent &evt);
	DECLARE_EVENT_TABLE();
};

class wxExtTextCtrl;
class wxExtTreeCtrl;

class SelectVariableDialog : public wxDialog
{
private:	
	wxExtTextCtrl *txtSearch;
	wxExtTreeCtrl *tree;
	wxButton *btnOk;
	wxButton *btnCancel;
	wxButton *btnUncheckAll;
	wxButton *btnExpandAll;

public:
	SelectVariableDialog( wxWindow *parent, const wxString &title );
	
	void SetItems(const wxArrayString &names, const wxArrayString &labels);
	void SetCheckedNames(const wxArrayString &list);
	wxArrayString GetCheckedNames();
	void ShowAllItems();
	void UpdateTree();

	
	static bool Run( const wxString &title, 
		const wxArrayString &names, const wxArrayString &labels, wxArrayString &list,
		bool expand_all=false );

private:
	void OnUncheckAll(wxCommandEvent &evt);
	void OnExpandAll(wxCommandEvent &evt);
	void OnSearch( wxCommandEvent & );
	void OnTree(wxTreeEvent &evt);

	struct item_info
	{
		wxString name;
		wxString label;
		wxTreeItemId tree_id;
		bool shown;
		bool checked;
	};

	wxTreeItemId m_root;
	std::vector<item_info> m_items;

protected:
	wxSizer *m_sizer;

	DECLARE_EVENT_TABLE()
};




class VarSelectDialog : public SelectVariableDialog
{
	wxChoice *m_cfglist;
public:
	VarSelectDialog( wxWindow *parent, const wxString &title );
	void UpdateVariables();
	void SetConfiguration( const wxString &tech, const wxString &fin );

protected:
	void OnConfig( wxCommandEvent &evt );
	DECLARE_EVENT_TABLE()
};



class NumericRangeDialog : public wxDialog
{
public:
	NumericRangeDialog( wxWindow *parent, const wxString &title );
	
	void SetValues( const wxArrayString &values, bool int_only=false );
	wxArrayString GetValues();
	void GenerateValues();
	bool CheckRanges();
	
private:	
	void OnCommand(wxCommandEvent &evt);

	bool m_intOnly;	
	wxStaticText *m_notification;
	wxListBox *m_values;
	wxNumericCtrl *m_numIncr;
	wxNumericCtrl *m_numEnd;
	wxNumericCtrl *m_numStart;

	DECLARE_EVENT_TABLE()
};

#endif

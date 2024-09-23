/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/SAM/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#ifndef __casewin_h
#define __casewin_h

#include <vector>

#include <wx/splitter.h>
#include <wx/minifram.h>
#include <wx/notebook.h>
#include <wx/treectrl.h>
#include <wx/dataview.h>

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
class PVUncertaintyForm;
class MacroPanel;
struct InputPageGroup;

class CaseWindow : public wxSplitterWindow, CaseEventListener
{
public:
	CaseWindow( wxWindow *parent, Case *c );
	virtual ~CaseWindow();

	Case *GetCase() { return m_case; }
	ParametricViewer* GetParametricViewer() { return m_parametrics; }

	void UpdateConfiguration();

	bool SwitchToInputPage(const wxString& name);
	bool SwitchToNavigationMenu(const wxString& name);
	wxString GetInputPage();
	wxArrayString GetInputPages();
	wxUIObject* FindActiveObject(const wxString& name, ActiveInputPage** page = 0);
	wxUIObject* FindObject(const wxString& name, ActiveInputPage** page = 0);

	bool SwitchToPage( const wxString &name ); // can navigate to results, parametrics, as well as input pages

	wxString GetCurrentContext();
	void UpdatePageNote();
	void CheckAndUpdateNotes(const wxArrayString& inputPageHelpContext);
	bool HasPageNote( const wxString &id );
	void ShowPageNote( );
	void SetPageNote( const wxString &text );

	void SaveCurrentViewProperties();

	bool RunBaseCase( bool silent = false, wxString *messages = 0 );
	void UpdateResults();

	bool RunSSCBaseCase(wxString& fn, bool silent = false, wxString* messages = 0);

	bool GenerateReport( 
		wxString pdffile = wxEmptyString, 
		wxString templfile = wxEmptyString,
		VarValue *metadata = 0
		);
	
private:
	Case *m_case;
	
	wxPanel* m_left_panel;

	InputPageList *m_inputPageList;
	std::vector<InputPageGroup*> m_pageGroups;
	std::vector<UIFormDatabase> m_forms;
	InputPageGroup *m_currentGroup;
	std::vector<wxUIFormData*> m_currentForms;

	struct PageDisplayState {
		PageDisplayState() {
			Form = 0;
			ActivePage = 0;
			CollapseCheck = 0;
			Collapsible = false;
			HeaderPage = false;
			ndxHybrid = 0;
		}

		wxUIFormData *Form;
		ActiveInputPage *ActivePage;
		bool Collapsible;
		wxString CollapsibleVar;
		CollapsePaneCtrl *CollapseCheck;
		bool HeaderPage;
		size_t ndxHybrid;
	};

	std::vector<PageDisplayState*> m_currentActivePages;
	wxScrolledWindow *m_inputPageScrollWin;
	wxPanel *m_exclPanel;
	wxBoxSizer *m_exclPanelSizer;
	wxMetroButton *m_exclPageButton;
    wxMetroListBox *m_exclRadioButton;
	wxMetroTabList *m_exclPageTabList;
	void UpdatePageListForConfiguration( const std::vector<PageInfo> &pages, ConfigInfo *cfg, size_t ndxHybrid );
	void LoadPageList( const std::vector<PageInfo> &list, bool header );
	void SetupActivePage();
	void LayoutPage();
	void DetachCurrentInputPage();

	wxStaticText *m_techLabel;
    wxStaticText* m_finLabel;
	wxMetroButton *m_simButton, *m_resultsButton;
	wxMetroDataViewTreeCtrl *m_navigationMenu;
    wxDataViewItem m_currentSelection;
    
    // to allow switching case configurations with P50/P90 and PVUncertainty
    wxGridSizer *m_szsims;
 //   wxMetroButton *m_parametricsButton, *m_stochasticButton, *m_pvuncertaintyButton, *m_p50p90Button, *m_macrosButton;

	wxSimplebook *m_pageFlipper;
	ResultsViewer *m_baseCaseResults;
	ParametricViewer *m_parametrics;
	StochasticPanel *m_stochastic;
	P50P90Form* m_p50p90;
	PVUncertaintyForm* m_pvuncertainty;
	MacroPanel *m_macros;

	PageNote *m_pageNote;
	wxString m_lastPageNoteId;

	void OnCommand( wxCommandEvent & );
    void OnTree(wxDataViewEvent&);
    void OnTreeCollapsing(wxDataViewEvent &evt );
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

public:
	SelectVariableDialog( wxWindow *parent, const wxString &title );
	
	static wxString PrettyPrintLabel(const wxString name, const wxString label, const wxString type, 
								const wxString units, const wxString group, bool ssc_variable=false);
	static wxString PrettyPrintLabel(const wxString name, const VarInfo vi);
	void SetItems(const wxArrayString &names, const wxArrayString &labels);
	void SetCheckedNames(const wxArrayString &list);
	wxArrayString GetCheckedNames();
	wxArrayInt GetCheckedIndices();
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

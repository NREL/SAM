#ifndef __casewin_h
#define __casewin_h

#include <vector>

#include <wx/splitter.h>
#include <wx/minifram.h>
#include <wx/notebook.h>
#include <wx/treectrl.h>

#include "case.h"
#include "inputpage.h"

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

	wxString GetCurrentContext();
	void UpdatePageNote();
	bool HasPageNote( const wxString &id );
	void ShowPageNote();

	void SaveCurrentViewProperties();

	bool RunBaseCase( bool silent = false, wxString *messages = 0 );
	void UpdateResults();

	void GenerateReport();
	
private:
	Case *m_case;
	
	wxPanel *m_inputPagePanel;
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
		}

		wxUIFormData *Form;
		ActiveInputPage *ActivePage;
		bool Collapsible;
		wxString CollapsibleVar;
		CollapsePaneCtrl *CollapseCheck;
	};

	std::vector<PageDisplayState*> m_currentActivePages;
	wxScrolledWindow *m_inputPageScrollWin;
	wxPanel *m_exclPanel;
	wxMetroButton *m_exclPageButton;

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

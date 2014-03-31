#ifndef __casewin_h
#define __casewin_h

#include <wx/splitter.h>
#include <wx/minifram.h>


#include "case.h"

class wxPanel;
class wxMetroButton;
class wxMetroNotebook;
class wxSimpleNotebook;
class wxLKScriptCtrl;

class InputPageList;
class ResultsViewer;
class Simulation;

class PageNote;
class CollapsePaneCtrl;

class CaseWindow : public wxSplitterWindow, CaseEventListener
{
public:
	CaseWindow( wxWindow *parent, Case *c );
	virtual ~CaseWindow();

	Case *GetCase() { return m_case; }

	void UpdateConfiguration();

	bool SwitchToInputPage( const wxString &name );
	wxUIObject *FindActiveObject( const wxString &name, ActiveInputPage **page = 0 );

	wxString GetCurrentContext();
	void UpdatePageNote();
	bool HasPageNote( const wxString &id );
	void ShowPageNote();

	void SaveCurrentViewProperties();

	bool RunBaseCase();
	void UpdateResults();

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

	//wxStaticText *m_techLabel, *m_finLabel;
	wxMetroButton *m_simButton, *m_resultsButton;

	wxSimplebook *m_pageFlipper;
	ResultsViewer *m_baseCaseResults;
	wxLKScriptCtrl *m_scriptCtrl;

	PageNote *m_pageNote;
	wxString m_lastPageNoteId;

	void OnCommand( wxCommandEvent & );	
	virtual void OnCaseEvent( Case *, CaseEvent & );


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

#endif

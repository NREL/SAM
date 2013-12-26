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
class BaseCase;

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
	wxUIObject *FindActiveObject( const wxString &name, InputPageBase **page = 0 );

	wxString GetCurrentContext();
	void UpdatePageNote();
	bool HasPageNote( const wxString &id );
	void ShowPageNote();

	void SaveCurrentViewProperties();

private:
	Case *m_case;
	
	wxPanel *m_inputPagePanel;
	InputPageList *m_inputPageList;
	std::vector<ConfigDatabase::InputPageGroup*> m_pageGroups;
	FormDatabase m_forms;
	ConfigDatabase::InputPageGroup *m_currentGroup;
	std::vector<wxUIFormData*> m_currentForms;

	struct PageDisplayState {
		PageDisplayState() {
			Form = 0;
			ActivePage = 0;
			CollapseCheck = 0;
			Collapsible = false;
		}

		wxUIFormData *Form;
		InputPageBase *ActivePage;
		bool Collapsible;
		wxString CollapsibleVar;
		CollapsePaneCtrl *CollapseCheck;
	};

	std::vector<PageDisplayState*> m_currentActivePages;
	wxScrolledWindow *m_inputPageScrollWin;
	wxPanel *m_exclPanel;
	wxStaticText *m_exclPageLabel;
	wxMetroButton *m_exclPageButton;

	void SetupActivePage();
	void LayoutPage();
	void DetachCurrentInputPage();

	wxMetroButton *m_simButton, *m_resultsButton;

	wxSimplebook *m_pageFlipper;
	wxMetroNotebook *m_resultsTab;
	BaseCase *m_baseCase;
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

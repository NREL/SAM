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

class CaseWindow : public wxSplitterWindow, CaseEventListener
{
public:
	CaseWindow( wxWindow *parent, Case *c );
	virtual ~CaseWindow();

	Case *GetCase() { return m_case; }

	void UpdateConfiguration();

	bool SwitchToInputPage( const wxString &name );
	void OrganizeCurrentPages();
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
	ConfigDatabase::InputPageGroup *m_currentGroup;
	FormDatabase m_currentForms;
	std::vector<InputPageBase*> m_currentShownPages;
	wxScrolledWindow *m_inputPageScrollWin;
	wxPanel *m_exclPanel;
	wxStaticText *m_exclPageLabel;
	wxMetroButton *m_exclPageButton;

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

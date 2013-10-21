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

class CaseWindow : public wxSplitterWindow, CaseEventListener
{
public:
	CaseWindow( wxWindow *parent, Case *c );
	virtual ~CaseWindow();

	Case *GetCase() { return m_case; }
	bool HasPageNote( const wxString &page ) { return false; }

private:
	Case *m_case;

	InputPageList *m_inputPageList;
	wxMetroButton *m_simButton, *m_resultsButton;

	wxPanel *m_inputPage;
	wxSimplebook *m_pageFlipper;
	wxMetroNotebook *m_resultsTab;
	BaseCase *m_baseCase;
	wxLKScriptCtrl *m_scriptCtrl;

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
private:
	wxTextCtrl *mText;
	void OnHideClose(wxCloseEvent &evt);
	DECLARE_EVENT_TABLE();
};

#endif

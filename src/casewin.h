#ifndef __casewin_h
#define __casewin_h

#include <wx/splitter.h>

class wxPanel;
class Case;
class wxMetroButton;
class wxMetroNotebook;
class wxSimpleNotebook;

class CaseWindow : public wxSplitterWindow
{
public:
	CaseWindow( wxWindow *parent, Case *c );
	virtual ~CaseWindow();

	Case *GetCase() { return m_case; }

private:
	Case *m_case;

	wxListBox *m_inputPageList;
	wxMetroButton *m_simButton, *m_resultsButton;

	wxPanel *m_inputPage;
	wxSimplebook *m_pageFlipper;
	wxMetroNotebook *m_resultsTab;

	void OnCommand( wxCommandEvent & );


	DECLARE_EVENT_TABLE();
};

#endif

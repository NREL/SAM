#include <wx/wx.h>
#include <wx/splitter.h>
#include <wx/simplebook.h>
#include <wex/metro.h>
#include <wex/lkscript.h>

#include "basecase.h"
#include "main.h"
#include "case.h"
#include "casewin.h"
#include "ipagelist.h"

#include "../resource/graph.cpng"


enum { ID_INPUTPAGELIST = wxID_HIGHEST + 142,
	ID_SIMULATE, ID_RESULTSPAGE };

BEGIN_EVENT_TABLE( CaseWindow, wxSplitterWindow )
	EVT_BUTTON( ID_SIMULATE, CaseWindow::OnCommand )
	EVT_BUTTON( ID_RESULTSPAGE, CaseWindow::OnCommand )
	EVT_LISTBOX( ID_INPUTPAGELIST, CaseWindow::OnCommand )
END_EVENT_TABLE()

CaseWindow::CaseWindow( wxWindow *parent, Case *c )
	: wxSplitterWindow( parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_NOBORDER | wxSP_LIVE_UPDATE ),
	m_case( c )
{
	m_case->AddListener( this );

	wxPanel *left_panel = new wxPanel( this );
	left_panel->SetBackgroundColour( *wxWHITE );
	m_inputPageList = new InputPageList( left_panel, ID_INPUTPAGELIST );
	m_inputPageList->SetBackgroundColour( wxColour(243,243,243) );
	for ( int i=0;i<10;i++ ) m_inputPageList->Add( wxString::Format("Input page %d", i+1) );

	m_simButton = new wxMetroButton( left_panel, ID_SIMULATE, "Simulate", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW );
	m_simButton->SetFont( wxMetroTheme::Font( wxMT_NORMAL, 14) );
	m_resultsButton = new wxMetroButton( left_panel, ID_RESULTSPAGE, wxEmptyString, wxBITMAP_PNG_FROM_DATA( graph ) );

	wxBoxSizer *szhl = new wxBoxSizer( wxHORIZONTAL );
	szhl->Add( m_simButton, 1, wxALL|wxEXPAND, 0 );
	szhl->Add( m_resultsButton, 0, wxALL|wxEXPAND, 0 );

	wxBoxSizer *szvl = new wxBoxSizer( wxVERTICAL );
	szvl->Add( m_inputPageList, 1, wxALL|wxEXPAND, 0 );
	szvl->Add( szhl, 0, wxALL|wxEXPAND, 0 );

	left_panel->SetSizer( szvl );

	m_pageFlipper = new wxSimplebook( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE );

	m_inputPage = new wxPanel( m_pageFlipper );
	m_inputPage->SetBackgroundColour( *wxWHITE );

	m_resultsTab = new wxMetroNotebook( m_pageFlipper, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxMT_LIGHTTHEME );
	
	m_baseCase = new BaseCase( m_resultsTab, 0 );
	m_resultsTab->AddPage( m_baseCase, "Base Case", true );

	m_resultsTab->AddPage( new wxPanel( m_resultsTab ), "Parametric", false );
	m_resultsTab->AddPage( new wxPanel( m_resultsTab ), "Sensitivity", false );
	m_resultsTab->AddPage( new wxPanel( m_resultsTab ), "P50/P90", false );

	m_scriptCtrl = new wxLKScriptCtrl( m_resultsTab, wxID_ANY );
	m_resultsTab->AddPage( m_scriptCtrl, "Scripting", false );

	m_pageFlipper->AddPage( m_inputPage, "Input Pages", true );
	m_pageFlipper->AddPage( m_resultsTab, "Output Pages", false );

	SplitVertically( left_panel, m_pageFlipper, 210 );
}

CaseWindow::~CaseWindow()
{
	m_case->RemoveListener( this );
}

void CaseWindow::OnCommand( wxCommandEvent &evt )
{
	if ( evt.GetId() == ID_SIMULATE )
	{
		m_inputPageList->Select( -1 );
		m_pageFlipper->SetSelection( 1 );
		m_resultsTab->SetSelection( 0 ); // show base case
		// run simulations for this case
	}
	else if (evt.GetId() == ID_RESULTSPAGE )
	{
		m_inputPageList->Select( -1 );
		m_pageFlipper->SetSelection( 1 );
	}
	else if ( evt.GetId() == ID_INPUTPAGELIST )
	{
		m_pageFlipper->SetSelection( 0 );
	}
}

void CaseWindow::OnCaseEvent( Case *c, CaseEvent &evt )
{
	if ( evt.GetType() == CaseEvent::VARS_CHANGED )
	{
		// to do
	}
	else if ( evt.GetType() == CaseEvent::CONFIG_CHANGED )
	{
		 // to do
	}
}

/* ********* SAM Page Notes ************** */

BEGIN_EVENT_TABLE(PageNote, wxMiniFrame)
EVT_CLOSE( PageNote::OnHideClose )
END_EVENT_TABLE()

PageNote::PageNote(CaseWindow *cwin)
	: wxMiniFrame(cwin, -1, "Notes", wxDefaultPosition, wxDefaultSize,
			  wxCLOSE_BOX|wxSYSTEM_MENU|wxCAPTION/*|wxSTAY_ON_TOP*/|wxRESIZE_BORDER)
{
	mText = new wxTextCtrl(this,
		-1, "", wxDefaultPosition ,wxDefaultSize,
		wxTE_MULTILINE);
	wxFont f = mText->GetFont();
	f.SetPointSize( f.GetPointSize() + 2 );
	mText->SetFont( f );
	mText->SetBackgroundColour( wxColour(255,255,168) );
}

void PageNote::SetText(const wxString &t)
{
	mText->SetValue(t);
}

wxString PageNote::GetText()
{
	return mText->GetValue();
}

void PageNote::OnHideClose(wxCloseEvent &evt)
{
	evt.Veto();
	Hide();
}

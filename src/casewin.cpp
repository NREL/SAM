#include <wx/wx.h>
#include <wx/splitter.h>
#include <wx/simplebook.h>
#include <wex/metro.h>

#include "main.h"
#include "case.h"
#include "casewin.h"

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
	m_inputPageList = new wxListBox( left_panel, ID_INPUTPAGELIST, wxDefaultPosition, wxDefaultSize, 0, 0, wxLB_SINGLE|wxBORDER_NONE );
	m_inputPageList->SetBackgroundColour( wxColour(243,243,243) );
	m_inputPageList->SetFont( wxMetroTheme::Font( wxMT_LIGHT, 18 ) );
	for ( int i=0;i<10;i++ ) m_inputPageList->Append( wxString::Format("Input page %d", i+1) );

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
	m_inputPage->SetBackgroundColour( *wxLIGHT_GREY );

	m_resultsTab = new wxMetroNotebook( m_pageFlipper, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxMT_LIGHTTHEME );
	m_resultsTab->AddPage( new wxPanel( m_resultsTab ), "Base Case", true );
	m_resultsTab->AddPage( new wxPanel( m_resultsTab ), "Parametric", true );
	m_resultsTab->AddPage( new wxPanel( m_resultsTab ), "Sensitivity", true );
	m_resultsTab->AddPage( new wxPanel( m_resultsTab ), "P50/P90", true );
	m_resultsTab->AddPage( new wxPanel( m_resultsTab ), "Scripting", true );

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
		m_pageFlipper->SetSelection( 1 );
		m_resultsTab->SetSelection( 0 ); // show base case
		// run simulations for this case
	}
	else if (evt.GetId() == ID_RESULTSPAGE )
	{
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
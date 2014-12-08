#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <wx/wx.h>
#include <wx/mdi.h>
#include <wx/config.h>
#include <wx/print.h>
#include <wx/printdlg.h>
#include <wx/radiobox.h>
#include <wx/dcbuffer.h>
#include <wx/protocol/http.h>
#include <wx/html/htmlwin.h>
#include <wx/hyperlink.h>
#include <wx/stdpaths.h>

#include <wex/icons/time.cpng>
#include <wex/metro.h>


#include "../resource/nrel.cpng"

#include "main.h"
#include "registration.h"
#include "script.h"
#include "welcome.h"

enum { ID_CREATE_PROJECT=wxID_HIGHEST+556, ID_OPEN_EXISTING, ID_RECENT_FILES,
	ID_MESSAGES_HTML, ID_MESSAGE_THREAD, ID_DOWNLOAD_TIMER, ID_GET_STARTED,
ID_NEW_SCRIPT, ID_OPEN_SCRIPT, ID_REGISTRATION };

BEGIN_EVENT_TABLE(WelcomeScreen, wxPanel)
	EVT_PAINT(WelcomeScreen::OnPaint)
	EVT_SIZE(WelcomeScreen::OnResize)
	
	EVT_BUTTON( ID_CREATE_PROJECT, WelcomeScreen::OnCommand )
	EVT_BUTTON( ID_OPEN_EXISTING, WelcomeScreen::OnCommand )
	EVT_BUTTON( ID_NEW_SCRIPT, WelcomeScreen::OnCommand )
	EVT_BUTTON( ID_OPEN_SCRIPT, WelcomeScreen::OnCommand )
	EVT_BUTTON( wxID_ABOUT, WelcomeScreen::OnCommand )
	EVT_BUTTON( wxID_HELP, WelcomeScreen::OnCommand )
	EVT_BUTTON( wxID_EXIT, WelcomeScreen::OnCommand )
	EVT_BUTTON( ID_GET_STARTED, WelcomeScreen::OnCommand )
	EVT_BUTTON( ID_REGISTRATION, WelcomeScreen::OnCommand )

	EVT_LISTBOX_DCLICK( ID_RECENT_FILES, WelcomeScreen::OnCommand )
	
	EVT_HTML_LINK_CLICKED( ID_MESSAGES_HTML, WelcomeScreen::OnMessagesLinkClicked )

	EVT_SIMPLECURL( ID_MESSAGE_THREAD, WelcomeScreen::OnMessageDownloadThread )
	EVT_TIMER( ID_DOWNLOAD_TIMER, WelcomeScreen::OnDownloadTimeout )
END_EVENT_TABLE();

enum { DOWNLOADING, FAILED, RETRIEVED };

WelcomeScreen::WelcomeScreen(wxWindow *parent)
	: wxPanel(parent, wxID_ANY),
	  m_downloadTimer( this, ID_DOWNLOAD_TIMER ),
	  m_ssCurlMessage( this, ID_MESSAGE_THREAD )
{
	SetBackgroundStyle(wxBG_STYLE_CUSTOM);
	SetBackgroundColour( *wxWHITE );

	m_messageStatus = DOWNLOADING;

	m_nrelLogo = wxBITMAP_PNG_FROM_DATA( nrel );
	
	m_htmlWin = new wxHtmlWindow(this, ID_MESSAGES_HTML, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE);
	m_htmlWin->SetFont( *wxNORMAL_FONT );
	m_htmlWin->SetFonts( wxNORMAL_FONT->GetFaceName(), "courier" );
	m_htmlWin->SetPage( "<html><body><font color=#a9a9a9 face=\"Segoe UI Light\" size=10>Loading news...</font></body></html>" );


	m_createCase = new wxMetroButton(this, ID_CREATE_PROJECT, "Start a new project", wxNullBitmap, 
		wxPoint(459,51), wxSize(208,21), wxMB_RIGHTARROW);
	m_createCase->SetFont( wxMetroTheme::Font( wxMT_NORMAL, 14) );

	m_openExisting = new wxMetroButton( this, ID_OPEN_EXISTING, "Open a project file", wxNullBitmap );
	m_openExisting->SetFont( wxMetroTheme::Font( wxMT_NORMAL, 14) );

	m_openScript = new wxMetroButton( this, ID_OPEN_SCRIPT, "Open script" );
	m_newScript = new wxMetroButton( this, ID_NEW_SCRIPT, "New script" );
	
	m_btnRegistration = new wxMetroButton( this, ID_REGISTRATION, "Registration" );
	m_btnGetStarted = new wxMetroButton( this, ID_GET_STARTED, "Getting started for new users", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW);
	m_btnHelp = new wxMetroButton( this, wxID_HELP, "Help contents" );
	m_btnAbout = new wxMetroButton( this, wxID_ABOUT, "About" );
	m_btnQuit = new wxMetroButton( this, wxID_EXIT, "Quit" );
	
	m_recent = new wxMetroListBox(this, ID_RECENT_FILES, wxPoint(15,327), wxSize(650,150) );
	

	LayoutWidgets();

	m_ssCurlMessage.Start( SamApp::WebApi("messages") );
	
	m_downloadTimer.Start(15000, true);

	UpdateRecentList();
}

WelcomeScreen::~WelcomeScreen()
{
	AbortDownloadThreads();
}


void WelcomeScreen::AbortDownloadThreads()
{
	m_ssCurlMessage.Abort();
}

void WelcomeScreen::OnDownloadTimeout( wxTimerEvent & )
{
	wxLogStatus("timeout: aborting download threads if they are still running...");
	AbortDownloadThreads();
}

void WelcomeScreen::OnMessagesLinkClicked(wxHtmlLinkEvent &e)
{
	wxLaunchDefaultBrowser( e.GetLinkInfo().GetHref(), wxBROWSER_NEW_WINDOW );
}

void WelcomeScreen::OnMessageDownloadThread(wxSimpleCurlEvent &e)
{
	wxLogStatus("OnMessageDownloadThread: " + e.GetMessage());
	if (e.GetStatusCode() == wxSimpleCurlEvent::FINISHED)
		UpdateMessagesHtml( m_ssCurlMessage.GetDataAsString() );
}

void WelcomeScreen::UpdateMessagesHtml(const wxString &html)
{
	if (!html.IsEmpty())
	{
		m_htmlWin->SetPage( html );
		m_messageStatus = RETRIEVED;
	}
	else
	{
		
		m_htmlWin->SetPage( "<html><body><font color=#a9a9a9 face=\"Segoe UI Light\" size=10>"
			"Could not connect to the SAM news feed."
			"</font></body></html>" );
		m_messageStatus = FAILED;
	}

	Refresh();
}


void WelcomeScreen::UpdateRecentList()
{
	m_recent->Clear();
	wxArrayString list = SamApp::RecentFiles();
	for( size_t i=0;i<list.size();i++ )
		m_recent->Add( list[i] );

	m_recent->Refresh();
}

#define YSTART 100
#define SPACER 30
#define BORDER 40
#define LEFTWIDTH 300

void WelcomeScreen::LayoutWidgets()
{
	int cw,ch,top = YSTART;
	GetClientSize(&cw,&ch);
	ch -= top;

	int ht = 2*ch/3; // top section height
	int hb = ch-ht; // bottom section height

	m_htmlWin->SetSize( BORDER+LEFTWIDTH, top, cw-BORDER-BORDER-LEFTWIDTH, ht );

	m_recent->SetSize( BORDER+LEFTWIDTH, top+ht, cw-BORDER-BORDER-LEFTWIDTH, hb);


	wxSize size2 = m_createCase->GetBestSize();
	size2.y+=5;
	m_createCase->SetSize( BORDER, top, LEFTWIDTH, size2.GetHeight()  );
	m_openExisting->SetSize( BORDER, top+size2.GetHeight(), LEFTWIDTH, size2.GetHeight() );

	int y = top+size2.GetHeight()*2;

	wxSize size3 = m_newScript->GetBestSize();
	m_newScript->SetSize( BORDER, y, LEFTWIDTH/2, size3.GetHeight() );
	m_openScript->SetSize( BORDER + LEFTWIDTH/2, y, LEFTWIDTH/2, size3.GetHeight() );
	
	m_btnGetStarted->SetSize( BORDER, top+ch-3*size3.y, LEFTWIDTH, size3.GetHeight() );
	m_btnHelp->SetSize( BORDER, top+ch-2*size3.y, LEFTWIDTH, size3.GetHeight() );
	m_btnRegistration->SetSize( BORDER, top+ch-size3.y, LEFTWIDTH/3, size3.y );
	m_btnAbout->SetSize( BORDER+ LEFTWIDTH/3, top+ch-size3.y, LEFTWIDTH/3, size3.y );
	m_btnQuit->SetSize( BORDER+LEFTWIDTH/3+LEFTWIDTH/3,top+ch-size3.y, LEFTWIDTH/3, size3.y );
}

void WelcomeScreen::OnPaint(wxPaintEvent &)
{
	m_recent->Invalidate();  // TFF, Feb 10 2014: added this line to force m_recent control to redraw after user closes a file. Glitch in wxMetroListBox?

	wxAutoBufferedPaintDC dc(this);
	wxSize sz = GetClientSize();

	wxColour grey(200, 200, 200);

	dc.SetBackground( wxBrush( *wxWHITE ) );
	dc.Clear();
	
	dc.SetFont( wxMetroTheme::Font( wxMT_LIGHT, 28 ) );	
	dc.SetTextForeground( grey );
	dc.DrawText( wxString::Format("System Advisor %d", SamApp::VersionMajor()), BORDER, 30 );

	dc.DrawBitmap( m_nrelLogo, sz.GetWidth()-m_nrelLogo.GetWidth()-BORDER, 33 );

	int y = 100;
		
	dc.SetPen( *wxTRANSPARENT_PEN );
	dc.SetBrush( wxBrush( grey ) );
	dc.DrawRectangle( BORDER, y, LEFTWIDTH, sz.GetHeight() - y+1 );

	//dc.SetPen( wxPen(wxMetroTheme::Colour( wxMT_ACCENT ), 1) );
	//dc.DrawLine( BORDER, y, sz.GetWidth()-BORDER, y );

	y += 20;	
	dc.SetFont( wxMetroTheme::Font() );	
	
	if (m_messageStatus == FAILED)
	{
		dc.SetTextForeground( *wxRED );
		wxString stat_text = "Could not connect.";
		int tw = dc.GetTextExtent( stat_text ).GetWidth();
		dc.DrawText( stat_text, sz.GetWidth()-BORDER-tw-2, y );		
	}

}

void WelcomeScreen::OnResize(wxSizeEvent &)
{
	LayoutWidgets();
	Refresh();
}
extern void foo(int);
void WelcomeScreen::OnCommand( wxCommandEvent &evt )	
{
	switch( evt.GetId() )
	{
	case ID_NEW_SCRIPT:
		ScriptWindow::CreateNewWindow();
		break;
	case ID_GET_STARTED:
		SamApp::ShowHelp( "getting_started");
		break;
	case ID_OPEN_SCRIPT:
		ScriptWindow::OpenFiles();
		break;
	case ID_CREATE_PROJECT:
		SamApp::Window()->CreateProject();
		break;
	case ID_OPEN_EXISTING:
	{
		wxFileDialog dlg( this, "Open a SAM file", wxEmptyString, wxEmptyString, "SAM Project Files (*.sam)|*.sam" );
		if ( wxID_OK == dlg.ShowModal() && SamApp::Window()->CloseProject())
			SamApp::Window()->LoadProject( dlg.GetPath() );
	}
		break;
	case ID_RECENT_FILES:
	{
		wxString fn = m_recent->GetSelectionString();
		if ( !wxFileExists(fn) )
		{
			wxMessageBox( "The file you selected no longer exists:\n\n" + fn );
			size_t n = SamApp::FileHistory().GetCount();
			for( size_t i=0;i<n;i++ )
			{
				if ( fn == SamApp::FileHistory().GetHistoryFile( i ) )
				{
					SamApp::FileHistory().RemoveFileFromHistory( i );
					m_recent->Delete( m_recent->GetSelection() );
					m_recent->SetSelection( -1 );
					m_recent->Invalidate();
					break;
				}
			}
			return;
		}
		if ( SamApp::Window()->CloseProject())
			if ( !SamApp::Window()->LoadProject( fn ) )
				wxMessageBox("The selected file could not be opened:\n\n" + fn );
	}
		break;

	case wxID_ABOUT:
		SamApp::ShowHelp( ":about" );
		break;

	case wxID_HELP:
		SamApp::ShowHelp( "welcome_page" );
		break;

	case wxID_EXIT:
		SamApp::Window()->Close();
		break;

	case ID_REGISTRATION:
		SamRegistration::ShowDialog();
		break;
	}
}

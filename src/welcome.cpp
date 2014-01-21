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
#include "welcome.h"

enum { ID_CREATE_PROJECT=wxID_HIGHEST+556, ID_OPEN_EXISTING, ID_RECENT_FILES,
	ID_MESSAGES_HTML, ID_MESSAGE_THREAD, ID_USAGE_THREAD, ID_DOWNLOAD_TIMER };

BEGIN_EVENT_TABLE(WelcomeScreen, wxPanel)
	EVT_PAINT(WelcomeScreen::OnPaint)
	EVT_SIZE(WelcomeScreen::OnResize)
	
	EVT_BUTTON( ID_CREATE_PROJECT, WelcomeScreen::OnCommand )
	EVT_BUTTON( ID_OPEN_EXISTING, WelcomeScreen::OnCommand )
	EVT_LISTBOX_DCLICK( ID_RECENT_FILES, WelcomeScreen::OnCommand )
	
	EVT_HTML_LINK_CLICKED( ID_MESSAGES_HTML, WelcomeScreen::OnMessagesLinkClicked )

	EVT_SIMPLECURL( ID_MESSAGE_THREAD, WelcomeScreen::OnMessageDownloadThread )
	EVT_SIMPLECURL( ID_USAGE_THREAD, WelcomeScreen::OnUsageDownloadThread )
	EVT_TIMER( ID_DOWNLOAD_TIMER, WelcomeScreen::OnDownloadTimeout )
END_EVENT_TABLE();

enum { DOWNLOADING, FAILED, RETRIEVED };

WelcomeScreen::WelcomeScreen(wxWindow *parent)
	: wxPanel(parent, wxID_ANY),
	  m_downloadTimer( this, ID_DOWNLOAD_TIMER ),
	  m_ssCurlMessage( this, ID_MESSAGE_THREAD ),
	  m_ssCurlUsage( this, ID_USAGE_THREAD )
{
	SetBackgroundStyle(wxBG_STYLE_CUSTOM);
	SetBackgroundColour( *wxWHITE );

	m_messageStatus = DOWNLOADING;

	m_nrelLogo = wxBITMAP_PNG_FROM_DATA( nrel );
	
	m_htmlWin = new wxHtmlWindow(this, ID_MESSAGES_HTML, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE);
	m_htmlWin->SetFont( *wxNORMAL_FONT );
	m_htmlWin->SetFonts( wxNORMAL_FONT->GetFaceName(), "courier" );


	m_createCase = new wxMetroButton(this, ID_CREATE_PROJECT, "Start a new project", wxNullBitmap, 
		wxPoint(459,51), wxSize(208,21), wxMB_RIGHTARROW);
	m_createCase->SetFont( wxMetroTheme::Font( wxMT_NORMAL, 14) );

	m_openExisting = new wxMetroButton( this, ID_OPEN_EXISTING, "Open an existing file", wxNullBitmap );
	m_openExisting->SetFont( wxMetroTheme::Font( wxMT_NORMAL, 14) );
	
	
	m_recent = new wxMetroListBox(this, ID_RECENT_FILES, wxPoint(15,327), wxSize(650,150) );
	

	LayoutWidgets();

	wxString msg_url = "https://sam.nrel.gov/files/content/updates/messages.html";
	m_ssCurlMessage.Start( msg_url );
	
	wxString usage_url = "https://nreldev.nrel.gov/analysis/sam/usage/samnt/startup.php?action=increment";
	m_ssCurlUsage.Start( usage_url );

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
	m_ssCurlUsage.Abort();
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
		UpdateMessagesHtml( m_ssCurlMessage.GetData() );
}

void WelcomeScreen::UpdateMessagesHtml(const wxString &html)
{
	if (!html.IsEmpty())
	{
		m_htmlWin->SetPage( html );
		m_messageStatus = RETRIEVED;
	}
	else
		m_messageStatus = FAILED;

	FILE *fp = fopen(GetLocalMessagesFile().c_str(), "w");
	if (fp)
	{
		wxLogStatus("updated local messages file: %s\n", GetLocalMessagesFile().c_str());
		fputs( html.c_str(), fp );
		fclose(fp);
	}

	Refresh();
}

wxString WelcomeScreen::GetLocalMessagesFile()
{	
	wxString path = wxStandardPaths::Get().GetUserLocalDataDir(); 
	
	if (!wxDirExists( path ))
		wxFileName::Mkdir( path, 511, wxPATH_MKDIR_FULL );

	return path + "/messages.html";
}

void WelcomeScreen::OnUsageDownloadThread( wxSimpleCurlEvent &evt )
{
	if (evt.GetStatusCode() == wxSimpleCurlEvent::FINISHED)
	{
		wxLogStatus("Logged usage download thread request: " + evt.GetMessage() );
		// nothing to do here - discard whatever data we get back from this url.
	}
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

void WelcomeScreen::LayoutWidgets()
{
	int cw,ch,top = YSTART;
	GetClientSize(&cw,&ch);
	ch -= top;

	int ht = 2*ch/3; // top section height
	int hb = ch-ht; // bottom section height

	m_htmlWin->SetSize( BORDER+350, top, cw-BORDER-BORDER-350, ht );

	m_recent->SetSize( BORDER+350, top+ht, cw-BORDER-BORDER-350, hb);


	wxSize size2 = m_createCase->GetBestSize();
	m_createCase->SetSize( BORDER+SPACER, top+SPACER, 350-SPACER-SPACER, size2.GetHeight()  );
	m_openExisting->SetSize( BORDER+SPACER, top+SPACER+size2.GetHeight()+10, 350-SPACER-SPACER, size2.GetHeight() );
}

void WelcomeScreen::OnPaint(wxPaintEvent &)
{
	wxAutoBufferedPaintDC dc(this);
	wxSize sz = GetClientSize();

	dc.SetBackground( wxBrush( *wxWHITE ) );
	dc.Clear();
	
	dc.SetFont( wxMetroTheme::Font( wxMT_LIGHT, 28 ) );	
	dc.SetTextForeground( wxColour(180, 180, 180) );
	dc.DrawText( wxString::Format("System Advisor %d", SamApp::VersionMajor()), BORDER, 30 );

	dc.DrawBitmap( m_nrelLogo, sz.GetWidth()-m_nrelLogo.GetWidth()-BORDER, 33 );

	int y = 100;
		
	dc.SetPen( *wxTRANSPARENT_PEN );
	dc.SetBrush( wxBrush( wxMetroTheme::Colour( wxMT_ACCENT ) ) );
	dc.DrawRectangle( BORDER, y, 350, sz.GetHeight() - y+1 );

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

void WelcomeScreen::OnCommand( wxCommandEvent &evt )	
{
	switch( evt.GetId() )
	{
	case ID_CREATE_PROJECT:
		SamApp::Window()->CreateProject();
		break;
	case ID_OPEN_EXISTING:
	{
		wxFileDialog dlg( this, "Open a SAM file", wxEmptyString, wxEmptyString, "SAM Project Files (*.sam)|*.sam" );
		if ( dlg.ShowModal() && SamApp::Window()->CloseProject())
			SamApp::Window()->LoadProject( dlg.GetPath() );
	}
		break;
	case ID_RECENT_FILES:
	{
		wxString fn = m_recent->GetSelectionString();
		if ( SamApp::Window()->CloseProject())
			SamApp::Window()->LoadProject( fn );
	}
		break;
	}
}

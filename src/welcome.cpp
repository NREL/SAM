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

DEFINE_EVENT_TYPE( wxEVT_WELCOME_CLOSE )

enum { ID_m_openExisting=wxID_HIGHEST+556, ID_m_createCase, ID_m_recent, 
ID_htmlViewer, ID_messageDownloadThread, ID_updateDownloadThread, ID_usageDownloadThread,
ID_m_onlineForum, ID_m_helpSystem, ID_downloadTimer
};

BEGIN_EVENT_TABLE(WelcomeScreen, wxPanel)
	EVT_PAINT(WelcomeScreen::OnPaint)
	EVT_SIZE(WelcomeScreen::OnResize)
	EVT_BUTTON( ID_m_createCase, WelcomeScreen::OnCreateProject)
	EVT_BUTTON( ID_m_openExisting, WelcomeScreen::OnOpenExisting)
	EVT_LISTBOX_DCLICK( ID_m_recent, WelcomeScreen::OnOpenRecent)

	EVT_BUTTON( ID_m_onlineForum, WelcomeScreen::OnHyperlink )
	EVT_BUTTON( ID_m_helpSystem, WelcomeScreen::OnHyperlink )

	EVT_HTML_LINK_CLICKED( ID_htmlViewer, WelcomeScreen::OnLinkClicked )

	EVT_SIMPLECURL( ID_messageDownloadThread, WelcomeScreen::OnMessageDownloadThread )
	EVT_SIMPLECURL( ID_updateDownloadThread, WelcomeScreen::OnUpdateDownloadThread )
	EVT_SIMPLECURL( ID_usageDownloadThread, WelcomeScreen::OnUsageDownloadThread )
	EVT_TIMER( ID_downloadTimer, WelcomeScreen::OnDownloadTimeout )
END_EVENT_TABLE();

enum { DOWNLOADING, FAILED, RETRIEVED };

WelcomeScreen::WelcomeScreen(wxWindow *parent)
	: wxPanel(parent, wxID_ANY),
	  m_downloadTimer( this, ID_downloadTimer ),
	  m_ssCurlMessage( this, ID_messageDownloadThread ),
	  m_ssCurlUpdate( this, ID_updateDownloadThread ),
	  m_ssCurlUsage( this, ID_usageDownloadThread )
{
	SetBackgroundStyle(wxBG_STYLE_CUSTOM);
	SetBackgroundColour( *wxWHITE );

	m_messageStatus = DOWNLOADING;

	m_nrelLogo = wxBITMAP_PNG_FROM_DATA( nrel );
	
	m_htmlWin = new wxHtmlWindow(this, ID_htmlViewer, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE);
	m_htmlWin->SetFont( *wxNORMAL_FONT );
	m_htmlWin->SetFonts( wxNORMAL_FONT->GetFaceName(), "courier" );


	m_createCase = new wxMetroButton(this, ID_m_createCase, "Start a new project", wxNullBitmap, 
		wxPoint(459,51), wxSize(208,21), wxMB_RIGHTARROW);
	m_createCase->SetFont( wxMetroTheme::Font( wxMT_NORMAL, 12) );

	m_openExisting = new wxMetroButton( this, ID_m_openExisting, "Open an existing file", wxNullBitmap );
	m_openExisting->SetFont( wxMetroTheme::Font( wxMT_NORMAL, 12) );
	
	
	m_recent = new wxListBox(this, ID_m_recent, wxPoint(15,327), wxSize(650,150), 0, 0, wxLB_SINGLE);
	m_recent->SetFont( wxMetroTheme::Font( wxMT_NORMAL ) );

	//m_onlineForum = new wxMetroButton(this, ID_m_onlineForum, "Support forum");
	//m_onlineForum->SetFont( wxMetroTheme::NormalFont(14) );

	//m_helpSystem = new wxMetroButton(this, ID_m_helpSystem, "Help system" );
	//m_helpSystem->SetFont( wxMetroTheme::NormalFont(14) );

	LayoutWidgets();

	wxString msg_url = "https://sam.nrel.gov/files/content/updates/messages.html";
	m_ssCurlMessage.Start( msg_url );
	
	wxString update_url = "https://sam.nrel.gov/files/content/updates/notification_2013.1.15.html";
	//m_ssCurlUpdate.Start( update_url );
	
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
	m_ssCurlUpdate.Abort();
	m_ssCurlUsage.Abort();
}

void WelcomeScreen::OnDownloadTimeout( wxTimerEvent & )
{
	wxLogStatus("timeout: aborting download threads if they are still running...");
	AbortDownloadThreads();
}

void WelcomeScreen::OnLinkClicked(wxHtmlLinkEvent &e)
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



enum { ID_UpdateDialogHtml = wxID_HIGHEST+142 };

class UpdateDialog : public wxDialog
{
private:
	wxHtmlWindow *m_htmlWindow;
	wxCheckBox *m_chkDoNotShowAgain;
public:
	UpdateDialog( wxWindow *parent, const wxString &title, const wxString &html )
		: wxDialog (parent, wxID_ANY, title, wxDefaultPosition, wxSize(500,400), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)
	{
		m_htmlWindow = new wxHtmlWindow(this, ID_UpdateDialogHtml );
		m_htmlWindow->SetPage( html );

		m_chkDoNotShowAgain = new wxCheckBox( this, wxID_ANY, "Do not show this notification again" );

		wxBoxSizer *sz_bottom = new wxBoxSizer(wxHORIZONTAL);
		sz_bottom->Add( m_chkDoNotShowAgain, 1, wxALL|wxEXPAND, 4 );
		sz_bottom->Add( CreateButtonSizer(wxOK), 0, wxALL|wxEXPAND, 4 );

		wxBoxSizer *sz_main = new wxBoxSizer(wxVERTICAL );
		sz_main->Add( m_htmlWindow, 1, wxALL|wxEXPAND, 0 );
		sz_main->Add( sz_bottom, 0, wxALL|wxEXPAND, 4 );

		SetSizer( sz_main );
	}

	bool DoNotShowAgain() { return m_chkDoNotShowAgain->GetValue(); }
	void OnLink( wxHtmlLinkEvent &evt )
	{
		wxLaunchDefaultBrowser( evt.GetLinkInfo().GetHref(), wxBROWSER_NEW_WINDOW );
	}
	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( UpdateDialog, wxDialog )
	EVT_HTML_LINK_CLICKED( ID_UpdateDialogHtml, UpdateDialog::OnLink )
END_EVENT_TABLE()

void WelcomeScreen::OnUpdateDownloadThread( wxSimpleCurlEvent &evt )
{
	if (evt.GetStatusCode() == wxSimpleCurlEvent::FINISHED )
	{
		wxString html = m_ssCurlUpdate.GetData();
		bool do_not_show = false;
		wxString cfgkey = "update_notification_" + SamApp::VersionStr();
		SamApp::Settings().Read( cfgkey, &do_not_show );
		if ( !html.IsEmpty() && !do_not_show )
		{
			UpdateDialog dlg( SamApp::Window(), "Update Notification", html );
			dlg.CenterOnParent();
			dlg.ShowModal();

			if (dlg.DoNotShowAgain())
				SamApp::Settings().Write( cfgkey, true );
		}
	}
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
	m_recent->Append( SamApp::RecentFiles() );
}

#define SPACER 30
#define BORDER 40
#define CCB_WIDTH 155
#define REMINDER_HEIGHT 121

void WelcomeScreen::LayoutWidgets()
{

	int cw,ch,top = 100 + SPACER;
	GetClientSize(&cw,&ch);
	ch -= top;

	int ht = 2*ch/3; // top section height
	int hb = ch-ht; // bottom section height

	m_recent->SetSize( BORDER+350+BORDER, top+ht, cw-BORDER-BORDER-BORDER-350, hb-BORDER);

	m_htmlWin->SetSize( BORDER+350+BORDER, top+BORDER+BORDER+BORDER, cw-BORDER-BORDER-BORDER-350, ht-BORDER );

	wxSize size2 = m_createCase->GetBestSize();
	m_createCase->SetSize( BORDER+SPACER, top, 350-SPACER-SPACER, size2.GetHeight()  );

	m_openExisting->SetSize( BORDER+SPACER, top+size2.GetHeight()+10, 350-SPACER-SPACER, size2.GetHeight() );

//	int y = ch*2/3;
//	wxSize size3 = m_onlineForum->GetBestSize();
//	m_onlineForum->SetSize( BORDER+SPACER, y, 350-SPACER-SPACER, size3.GetHeight() );

//	wxSize size4 = m_helpSystem->GetBestSize();
//	m_helpSystem->SetSize( BORDER+SPACER, y + size3.GetHeight()+SPACER, 350-SPACER-SPACER, size4.GetHeight() );
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
	dc.DrawRectangle( BORDER, y, sz.GetWidth() - BORDER-BORDER, SPACER+SPACER );

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

void WelcomeScreen::OnCreateProject(wxCommandEvent &)	
{
	SamApp::Window()->CreateProject();
}

void WelcomeScreen::OnOpenExisting( wxCommandEvent &)
{
	wxFileDialog dlg( this, "Open a SAM file", wxEmptyString, wxEmptyString, "SAM Project Files (*.sam)|*.sam" );
	if ( dlg.ShowModal() && SamApp::Window()->CloseProject())
		SamApp::Window()->LoadProject( dlg.GetPath() );
}


void WelcomeScreen::OnOpenRecent(wxCommandEvent &)
{
	wxString fn = m_recent->GetStringSelection();
	if ( SamApp::Window()->CloseProject())
		SamApp::Window()->LoadProject( fn );
}

void WelcomeScreen::OnHyperlink(wxCommandEvent &evt)
{
	switch(evt.GetId())
	{
	case ID_m_onlineForum:
		wxLaunchDefaultBrowser( "https://sam.nrel.gov/forums/support-forum", wxBROWSER_NEW_WINDOW);
		break;
	case ID_m_helpSystem:
		SamApp::ShowHelp("main");
		break;
	}
}

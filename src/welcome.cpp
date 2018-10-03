#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <memory>

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
#include <wx/dir.h>
#include <wx/stattext.h>

#include <wex/icons/time.cpng>
#include <wex/metro.h>
#include <wex/utils.h>
#include <wex/lkscript.h>

#include <lk/lex.h>
#include <lk/absyn.h>
#include <lk/eval.h>
#include <lk/stdlib.h>
#include <lk/invoke.h>
#include <lk/vm.h>
#include <lk/codegen.h>
#include <lk/env.h>
#include <lk/parse.h>

#ifdef __WXMSW__
#include <wex/mswfatal.h>
#endif

//#include "../resource/nrel.cpng"

#include "main.h"
#include "invoke.h"
#include "script.h"
#include "welcome.h"
#include "macro.h"


#define MAX_SCRIPTS 100

enum { ID_CREATE_PROJECT=wxID_HIGHEST+556, ID_OPEN_EXISTING, ID_RECENT_FILES,
	ID_MESSAGES_HTML, ID_MESSAGE_THREAD, ID_DOWNLOAD_TIMER, ID_GET_STARTED,
ID_NEW_SCRIPT, ID_OPEN_SCRIPT, ID_TEST_SEGFAULT, 
ID_QUICK_START, ID_QSTART_SCRIPTS, ID_QSTART_SCRIPTS_MAX=ID_QSTART_SCRIPTS+MAX_SCRIPTS };

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
	EVT_BUTTON( ID_TEST_SEGFAULT, WelcomeScreen::OnCommand )

	
	EVT_BUTTON( ID_QUICK_START, WelcomeScreen::OnCommand )
	EVT_MENU( ID_GET_STARTED, WelcomeScreen::OnCommand )
	EVT_MENU_RANGE( ID_QSTART_SCRIPTS, ID_QSTART_SCRIPTS_MAX, WelcomeScreen::OnQStartScript )

	EVT_LISTBOX_DCLICK( ID_RECENT_FILES, WelcomeScreen::OnCommand )
	
	EVT_HTML_LINK_CLICKED( ID_MESSAGES_HTML, WelcomeScreen::OnMessagesLinkClicked )

	EVT_EASYCURL( ID_MESSAGE_THREAD, WelcomeScreen::OnMessageDownloadThread )
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

#ifdef _DEBUG
	new wxButton( this, ID_TEST_SEGFAULT, "!!!", wxPoint(0,0), wxSize(25,25) );
#endif

	m_messageStatus = DOWNLOADING;

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
	
	m_btnGetStarted = new wxMetroButton( this, ID_QUICK_START, "Quick start for new users", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW);
	m_btnHelp = new wxMetroButton( this, wxID_HELP, "Help contents" );
	m_btnAbout = new wxMetroButton( this, wxID_ABOUT, "About" );
	m_btnQuit = new wxMetroButton( this, wxID_EXIT, "Quit" );
	
	m_recent = new wxMetroListBox(this, ID_RECENT_FILES );
	
	LayoutWidgets();

	m_ssCurlMessage.Start( SamApp::WebApi("welcome") );
	
	m_downloadTimer.Start(15000, true);

	UpdateRecentList();
}

WelcomeScreen::~WelcomeScreen()
{
	AbortDownloadThreads();
}


void WelcomeScreen::AbortDownloadThreads()
{
	m_ssCurlMessage.Cancel();
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

void WelcomeScreen::OnMessageDownloadThread(wxEasyCurlEvent &e)
{
	wxLogStatus("OnMessageDownloadThread: " + e.GetMessage());
	if (e.GetStatusCode() == wxEasyCurlEvent::FINISHED)
	{
		RunWelcomeScript( m_ssCurlMessage.GetDataAsString() );
	//	UpdateMessagesHtml( m_ssCurlMessage.GetDataAsString() );
	}
}

static void fcall_out( lk::invoke_t &cxt )
{
	LK_DOC( "out", "Output textual data.", "(string):none" );
	if ( wxString *s = (wxString*)cxt.user_data() )
	{
		for( size_t i=0;i<cxt.arg_count();i++ )
			(*s) += cxt.arg(i).as_string();
	}
}

void WelcomeScreen::RunWelcomeScript( const wxString &script )
{
	// wxShowTextMessageDialog( script );

	lk::input_string data( script );
	lk::parser parse( data );
	smart_ptr<lk::node_t>::ptr root( parse.script() );

	if ( parse.error_count() != 0
		|| parse.token() != lk::lexer::END)
	{
		wxString text = "parsing did not reach end of input\n";
		for (int x=0; x < parse.error_count(); x++)
			text += "parse: " + parse.error(x) + "\n";

		text.Replace( "<", "&lt" );
		text.Replace( ">", "&gt" );
		text.Replace( "&", "&amp" );

		UpdateMessagesHtml( "<pre>" + text + "</pre>" );
		return;
	}

	lk::env_t env;
	env.register_funcs( lk::stdlib_basic() );

	// note: do not register SYSIO functions to improve security when running
	// LK code downloaded directly from sam.nrel.gov in the welcome window

	env.register_funcs( lk::stdlib_string() );
	env.register_funcs( lk::stdlib_math() );
	env.register_funcs( lk::stdlib_wxui() );
	env.register_funcs( wxLKPlotFunctions() );
	env.register_funcs( wxLKMiscFunctions() );

	// note: don't register wxLKFileFunctions (csv i/o and decompress)
	// to improve security too

	//std::vector<lk_string> list = env.list_funcs();
	//wxShowTextMessageDialog( "welcome functions=\n" + lk::join( list, "\n" ) );
	
	wxString html;
	env.register_func( fcall_out, &html );
	env.register_func( fcall_samver );

	lk::codegen cg;
	if  ( !cg.generate( root.get() ) )
	{
		UpdateMessagesHtml( "<pre>lkcg: " + cg.error() + "</pre>" );
		return;
	}

	lk::bytecode bc;
	cg.get( bc );

	lk::vm vm;
	vm.load( &bc );
	
	UpdateMessagesHtml( "<html><body><font color=#a9a9a9 face=\"Segoe UI Light\" size=11>"
		"Loading..."
			"</font></body></html>" );

	if ( !vm.initialize( &env ) || !vm.run() )
	{
		if (vm.error().Lower() == "[0] no bytecode loaded")
		{
			if (strlen(sam_api_key) == 0)
				UpdateMessagesHtml( "<html><body><font color=#a9a9a9 face=\"Segoe UI Light\" size=10>Please setup API keys, see private.h for details...</font></body></html>" );
			else
				UpdateMessagesHtml( "<pre></pre>" );
		}
		else
			UpdateMessagesHtml( "<pre>lkvm: " + vm.error() + "</pre>" );
		return;
	}	

	UpdateMessagesHtml( html );
}

void WelcomeScreen::UpdateMessagesHtml(const wxString &html)
{
	//wxShowTextMessageDialog( html );
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

#define YSTART0 100
#define LEFTWIDTH0 300
#define SPACER 30
#define BORDER 40

void WelcomeScreen::LayoutWidgets()
{
	double xScale, yScale;
	wxDevicePPIToScale( wxClientDC(this).GetPPI(), &xScale, &yScale );

	int YSTART = (int)(YSTART0*yScale);
	int LEFTWIDTH = (int)(LEFTWIDTH0*xScale);

	int cw,ch,top = YSTART;
	GetClientSize(&cw,&ch);
	ch -= top;
	

	int ht = 2*ch/3; // top section height
	int hb = ch-ht; // bottom section height
	
	int verht = 0;
	/*if ( m_versionLabel->IsShown() )
	{
		wxSize vsz( m_versionLabel->GetBestSize() );
		verht = vsz.GetHeight();
		m_versionLabel->SetSize( BORDER+LEFTWIDTH, top, cw-BORDER-BORDER-LEFTWIDTH, verht );
	}*/

	m_htmlWin->SetSize( BORDER+LEFTWIDTH, top + verht, cw-BORDER-BORDER-LEFTWIDTH, ht-verht );

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
	m_btnAbout->SetSize( BORDER, top+ch-size3.y, LEFTWIDTH/2, size3.y );
	m_btnQuit->SetSize( BORDER+LEFTWIDTH/2,top+ch-size3.y, LEFTWIDTH/2, size3.y );
}

void WelcomeScreen::OnPaint(wxPaintEvent &)
{
	m_recent->Invalidate();  // TFF, Feb 10 2014: added this line to force m_recent control to redraw after user closes a file. Glitch in wxMetroListBox?

	wxAutoBufferedPaintDC dc(this);
	double xScale, yScale;
	wxDevicePPIToScale( dc.GetPPI(), &xScale, &yScale );

	wxSize sz = GetClientSize();

	wxColour grey(210, 210, 210);

	dc.SetBackground( wxBrush( *wxWHITE ) );
	dc.Clear();
	
	int y = (int)(YSTART0*yScale);
	
	dc.SetFont( wxMetroTheme::Font( wxMT_LIGHT, 28 ) );	
	dc.SetTextForeground( grey );
	wxString title(wxString::Format("System Advisor Model (Open Source) %d", SamApp::VersionMajor() ));
	wxSize tsz( dc.GetTextExtent( title ) );
	dc.DrawText( title, BORDER, y/2-tsz.y/2 );

	dc.SetPen( *wxTRANSPARENT_PEN );
	dc.SetBrush( wxBrush( grey ) );
	dc.DrawRectangle( BORDER, y, ((int)LEFTWIDTH0*xScale), sz.GetHeight() - y+1 );

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
	case ID_TEST_SEGFAULT:
#ifdef __WXMSW__
		wxMSWSegmentationFault();
#endif
		break;
	case ID_NEW_SCRIPT:
		SamScriptWindow::CreateNewWindow();
		break;
	case ID_QUICK_START:
	{
		wxPoint pos(wxDefaultPosition);
		if ( wxWindow *win = dynamic_cast<wxWindow*>(evt.GetEventObject()) )
		{
			pos = win->GetScreenPosition();
			pos.x += win->GetClientSize().x;
		}

		wxMetroPopupMenu menu;
		menu.Append( ID_GET_STARTED, "Getting started guide..." );
		
		m_qstartScripts.clear();
		wxDir::GetAllFiles( SamApp::GetRuntimePath() + "/quickstart", &m_qstartScripts, "*.lk" );
		m_qstartScripts.Sort();

		if (m_qstartScripts.size() > 0 ) menu.AppendSeparator();
		for( size_t i=0;i<m_qstartScripts.size();i++ )
			menu.Append( ID_QSTART_SCRIPTS+i, wxFileName(m_qstartScripts[i]).GetName() );
		
		menu.Popup( this, pos, wxTOP|wxLEFT );
	}
		break;
	case ID_GET_STARTED:
		SamApp::ShowHelp( "getting_started");
		break;
	case ID_OPEN_SCRIPT:
		SamScriptWindow::OpenFiles();
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

	}
}

void WelcomeScreen::OnQStartScript( wxCommandEvent &evt )
{
	int iscript = evt.GetId() - ID_QSTART_SCRIPTS;
	if ( iscript < 0 || iscript >= (int)m_qstartScripts.size() )
		return;

	wxString file( m_qstartScripts[iscript] );

	wxFile fp( file );
	if ( fp.IsOpened() )
	{
		wxString buf;
		fp.ReadAll( &buf );

		MacroEngine me;
		if ( !me.Run( buf ) )
			wxMessageBox("Quick start script failed.\n\n", file);
	}
	else
		wxMessageBox("Could not open quick start script.\n\n", file);
	
}

#include <wx/wx.h>
#include <wx/frame.h>
#include <wx/stc/stc.h>
#include <wx/webview.h>
#include <wx/simplebook.h>
#include <wx/panel.h>

#include <wex/metro.h>
#include <wex/icons/cirplus.cpng>
#include <wex/icons/qmark.cpng>

#include "../resource/nrel_small.cpng"
#include "../resource/main_menu.cpng"


#include "main.h"
#include "welcome.h"
#include "project.h"


// application globals
static MainWindow *g_mainWindow = 0;
static wxConfig *g_config = 0;
static const int g_verMajor = 2014;
static const int g_verMinor = 1;
static const int g_verMicro = 1;

enum { __idFirst = wxID_HIGHEST+592,

	ID_MAIN_MENU, ID_CASE_TABS, ID_CONTEXT_HELP, ID_PAGE_NOTES,
	__idCaseMenuFirst,
	ID_CASE_CREATE,
	ID_CASE_CONFIG,
	ID_CASE_RENAME,
	ID_CASE_DUPLICATE,
	ID_CASE_DELETE,
	ID_CASE_REPORT,
	ID_CASE_SIMULATE,
	ID_CASE_RESET_DEFAULTS,
	ID_CASE_CLEAR_RESULTS,
	ID_CASE_COMPARE,
	ID_CASE_VARIABLE_LIST,
	ID_CASE_IMPORT,
	__idCaseMenuLast
};

BEGIN_EVENT_TABLE( MainWindow, wxFrame )
	EVT_CLOSE( MainWindow::OnClose )
	EVT_MENU( wxID_NEW, MainWindow::OnCommand )
	EVT_MENU( wxID_OPEN, MainWindow::OnCommand )
	EVT_MENU( wxID_SAVE, MainWindow::OnCommand )
	EVT_MENU( wxID_SAVEAS, MainWindow::OnCommand )
	EVT_MENU( wxID_CLOSE, MainWindow::OnCommand )
	EVT_MENU( wxID_EXIT, MainWindow::OnCommand )
	EVT_BUTTON( ID_MAIN_MENU, MainWindow::OnCommand )
	EVT_LISTBOX( ID_CASE_TABS, MainWindow::OnCaseTabChange )
	EVT_BUTTON( ID_CASE_TABS, MainWindow::OnCaseTabButton )
	EVT_MENU_RANGE( __idCaseMenuFirst, __idCaseMenuLast, MainWindow::OnCaseMenu )
END_EVENT_TABLE()

MainWindow::MainWindow()
	: wxFrame( 0, wxID_ANY, wxT("SAM") + wxString(" ") + SamApp::VersionStr(), 
		wxDefaultPosition, wxSize( 1100, 700 ) )
{
#ifdef __WXMSW__
	SetIcon( wxICON( appicon ) );
#endif	

#ifdef __WXOSX__
	m_fileMenu = new wxMenu;
	m_fileMenu->Append( wxID_NEW );
	m_fileMenu->AppendSeparator();
	m_fileMenu->Append( wxID_OPEN );
	m_fileMenu->Append( wxID_SAVE );
	m_fileMenu->Append( wxID_SAVEAS );
	m_fileMenu->AppendSeparator();
	m_fileMenu->Append( wxID_CLOSE );
	m_fileMenu->AppendSeparator();
	m_fileMenu->Append( wxID_EXIT, "Quit SAM");

	m_caseMenu = new wxMenu;

	m_toolsMenu = new wxMenu;

	m_helpMenu = new wxMenu;
	m_helpMenu->Append( wxID_HELP );
	m_helpMenu->AppendSeparator();
	m_helpMenu->Append( wxID_ABOUT );
		
	m_menuBar = new wxMenuBar;
	m_menuBar->Append( m_fileMenu, wxT("&File") );
	m_menuBar->Append( m_caseMenu, wxT("&Case")  );
	m_menuBar->Append( m_toolsMenu, wxT("&Tools")  );
	m_menuBar->Append( m_helpMenu, wxT("&Help")  );
	SetMenuBar( m_menuBar );
#endif

	m_topBook = new wxSimplebook( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE );

	m_welcomeScreen = new WelcomeScreen( m_topBook );
	m_topBook->AddPage( m_welcomeScreen, "Welcome to SAM" );


	m_caseTabPanel = new wxPanel( m_topBook );
	m_topBook->AddPage( m_caseTabPanel, "Main project window" );

	wxBoxSizer *tools = new wxBoxSizer( wxHORIZONTAL );
	tools->Add( new wxMetroButton( m_caseTabPanel, ID_MAIN_MENU, wxEmptyString, wxBITMAP_PNG_FROM_DATA( main_menu ), wxDefaultPosition, wxDefaultSize /*, wxMB_DOWNARROW */), 0, wxALL|wxEXPAND, 0 );
	tools->Add( new wxMetroButton( m_caseTabPanel, ID_CASE_CREATE, "New", wxBITMAP_PNG_FROM_DATA( cirplus ), wxDefaultPosition, wxDefaultSize), 0, wxALL|wxEXPAND, 0 );
	m_caseTabList = new wxMetroTabList( m_caseTabPanel, ID_CASE_TABS, wxDefaultPosition, wxDefaultSize, wxMT_MENUBUTTONS );
	m_caseTabList->Append( "photovoltaic #1" );
	m_caseTabList->Append( "solar water" );
	m_caseTabList->Append( "power tower steam" );
	tools->Add( m_caseTabList, 1, wxALL|wxEXPAND, 0 );		
	tools->Add( new wxMetroButton( m_caseTabPanel, ID_CONTEXT_HELP, wxEmptyString, wxBITMAP_PNG_FROM_DATA(qmark), wxDefaultPosition, wxDefaultSize), 0, wxALL|wxEXPAND, 0 );
	
	m_caseNotebook = new wxSimplebook( m_caseTabPanel, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE );
		
	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( tools, 0, wxALL|wxEXPAND, 0 );
	sizer->Add( m_caseNotebook, 1, wxALL|wxEXPAND, 0 );
	m_caseTabPanel->SetSizer(sizer);

	m_topBook->SetSelection( 0 );
}

void MainWindow::CreateProject()
{
	m_project.Clear();
	m_project.SetModified( false );
	m_topBook->SetSelection( 1 );
}

void MainWindow::CloseProject()
{
	m_project.Clear();
	m_project.SetModified( false );
	m_topBook->SetSelection( 0 );
}


void MainWindow::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_MAIN_MENU:
		{
			wxMenu menu;
			menu.Append( wxID_NEW, "New project" );
			menu.AppendSeparator();
			menu.Append( wxID_OPEN, "Open project" );
			menu.Append( wxID_SAVE, "Save project" );
			menu.Append( wxID_SAVEAS, "Save project as" );
			menu.AppendSeparator();
			menu.Append( wxID_CLOSE, "Close project" );
			menu.Append( wxID_EXIT );
			PopupMenu( &menu );
		}
		break;
	case wxID_NEW:
		CreateProject();
		break;
	case wxID_OPEN:
	case wxID_SAVE:
	case wxID_SAVEAS:
		wxMessageBox("not possible yet");
		break;
	case wxID_CLOSE:
		CloseProject();
		break;
	case wxID_EXIT:
		Close();
		break;
	}
}

void MainWindow::OnCaseTabChange( wxCommandEvent &evt )
{
	//wxMessageBox( wxString::Format("Case tab changed: %d", evt.GetSelection() ) );
}

void MainWindow::OnCaseTabButton( wxCommandEvent &evt )
{
	wxMenu menu;
	
	menu.Append( ID_CASE_CONFIG, "Change configuration..." );
	menu.AppendSeparator();
	menu.Append( ID_CASE_RENAME, "Rename" );
	menu.Append( ID_CASE_DUPLICATE, "Duplicate" );
	menu.Append( ID_CASE_DELETE, "Delete" );
	menu.AppendSeparator();
	menu.Append( ID_CASE_SIMULATE, "Simulate" );
	menu.Append( ID_CASE_CLEAR_RESULTS, "Clear all results" );
	menu.Append( ID_CASE_REPORT, "Generate report" );
	menu.Append( ID_CASE_COMPARE, "Compare to..." );
	menu.AppendSeparator();
	menu.Append( ID_CASE_RESET_DEFAULTS, "Reset inputs to default values" );
	menu.Append( ID_CASE_VARIABLE_LIST, "Input variable list");
	menu.AppendSeparator();
	menu.Append( ID_CASE_IMPORT, "Import" );

	PopupMenu( &menu );
}

void MainWindow::OnCaseMenu( wxCommandEvent &evt )
{
	int sel = m_caseTabList->GetSelection();
	//wxMessageBox( wxString::Format("case id: %d, command %d", sel, evt.GetId() ) );
}

void MainWindow::OnClose( wxCloseEvent & )
{
	CloseProject();
	Destroy();
}


class SplashScreen : public wxDialog
{
	wxBitmap m_nrelLogo;
public:

	SplashScreen()
		: wxDialog( 0, wxID_ANY, wxEmptyString, wxDefaultPosition, wxSize(515,385), wxBORDER_NONE )
	{
		m_nrelLogo = wxBITMAP_PNG_FROM_DATA( nrel_small );
	}

	void OnPaint( wxPaintEvent & )
	{
		wxPaintDC dc(this);

		int width, height;
		GetClientSize( &width, &height );

		dc.SetBackground( wxBrush( wxMetroTheme::Colour( wxMT_ACCENT ) ) );
		dc.Clear();

		dc.SetBrush( *wxWHITE_BRUSH );
		dc.SetPen( *wxWHITE_PEN );
		dc.DrawRectangle( 0, height-50, width, 50 );

		dc.SetTextForeground( *wxWHITE );
		dc.SetFont( wxMetroTheme::Font( wxMT_LIGHT, 30 ) );
		dc.DrawText( "System Advisor Model", 35, 65 );

		dc.SetFont( wxMetroTheme::Font( wxMT_LIGHT, 18 ) );
		dc.DrawText( "Version " + SamApp::VersionStr(), 35, 135 );
		dc.DrawText( "Starting up... please wait", 35, 275 );

		dc.SetTextForeground( wxMetroTheme::Colour( wxMT_TEXT ) );
		dc.SetFont( wxMetroTheme::Font( wxMT_LIGHT, 10 ) );
		dc.DrawText( wxString::Format("Copyright %d, National Renewable Energy Laboratory", SamApp::VersionMajor()),
			35, height-25-dc.GetCharHeight()/2 );

		dc.DrawBitmap( m_nrelLogo, width-m_nrelLogo.GetWidth()-10, height-25-m_nrelLogo.GetHeight()/2 );
	}

	void OnSize( wxSizeEvent & )
	{
		Refresh();
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( SplashScreen, wxDialog )
	EVT_PAINT( SplashScreen::OnPaint )
	EVT_SIZE( SplashScreen::OnSize )
END_EVENT_TABLE()


bool SamApp::OnInit()
{
	wxInitAllImageHandlers();
	wxSimpleCurlInit();
	
	SplashScreen splash;
	splash.CenterOnScreen();
	splash.Show();
	splash.Update();

	Yield(true);
	wxMilliSleep( 500 );

	g_config = new wxConfig( "SAMnt", "NREL" );
	FileHistory().Load( Config() );


	g_mainWindow = new MainWindow();
	g_mainWindow->Show();

	return true;
}

int SamApp::OnExit()
{
	FileHistory().Save( Config() );

	if ( g_config != 0 ) delete g_config;

	wxSimpleCurlShutdown();
	return 0;
}

wxConfig &SamApp::Config()
{
	if ( g_config == 0 ) throw SamException( "g_config = NULL: internal error" );
	return *g_config;
}

MainWindow *SamApp::Window()
{
	return g_mainWindow;
}

wxFileHistory &SamApp::FileHistory()
{
static wxFileHistory s_fileHistory;
	return s_fileHistory;
}
wxArrayString SamApp::RecentFiles()
{
	wxArrayString files;
	size_t n = FileHistory().GetCount();
	for ( size_t i=0;i<n;i++ )
		files.Add( FileHistory().GetHistoryFile( i ) );

	return files;
}

void SamApp::ShowHelp( const wxString &id )
{
	wxMessageBox("no help system yet: " + id);
}

wxString SamApp::VersionStr() { return wxString::Format("%d.%d.%d", VersionMajor(), VersionMinor(), VersionMicro());};
int SamApp::VersionMajor() { return g_verMajor; }
int SamApp::VersionMinor() { return g_verMinor; }
int SamApp::VersionMicro() { return g_verMicro; }


IMPLEMENT_APP( SamApp );

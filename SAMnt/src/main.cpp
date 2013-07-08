#include <wx/wx.h>
#include <wx/frame.h>
#include <wx/stc/stc.h>
#include <wx/webview.h>

#include <wex/dview/dvplotctrl.h>

#include "main.h"
#include "welcome.h"


// application globals
static MainWindow *g_mainWindow = 0;
static wxConfig *g_config = 0;
static const int g_verMajor = 5;
static const int g_verMinor = 1;

BEGIN_EVENT_TABLE( MainWindow, wxFrame )
	EVT_CLOSE( MainWindow::OnClose )
END_EVENT_TABLE()

MainWindow::MainWindow()
	: wxFrame( 0, wxID_ANY, wxT("SAM") + wxString(" ") + SamApp::VersionStr(), wxDefaultPosition, wxSize( 800, 600 ) )
{
#ifdef __WXMSW__
	SetIcon( wxICON( appicon ) );
#endif

	m_welcomeScreen = new WelcomeScreen( this );
}

void MainWindow::OnClose( wxCloseEvent &evt )
{
	Destroy();
}


bool SamApp::OnInit()
{
	g_config = new wxConfig( "SAMnt", "NREL" );
	FileHistory().Load( Config() );

	wxInitAllImageHandlers();

	g_mainWindow = new MainWindow();
	g_mainWindow->Show();

	return true;
}

int SamApp::OnExit()
{
	FileHistory().Save( Config() );

	if ( g_config != 0 ) delete g_config;

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

wxString SamApp::VersionStr() { return wxString::Format("%d.%d", VersionMajor(), VersionMinor());};
int SamApp::VersionMajor() { return g_verMajor; }
int SamApp::VersionMinor() { return g_verMinor; }


IMPLEMENT_APP( SamApp );

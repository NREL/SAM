#include <wx/wx.h>
#include <wx/frame.h>
#include <wx/stc/stc.h>
#include <wx/webview.h>
#include <wx/simplebook.h>
#include <wx/panel.h>

#include <wex/metro.h>
#include <wex/icons/cirplus.cpng>
#include <wex/icons/qmark.cpng>
#include <wex/utils.h>

#include "../resource/nrel_small.cpng"
#include "../resource/main_menu.cpng"

#include <lk_absyn.h>
#include <lk_parse.h>
#include <lk_eval.h>

#include "main.h"
#include "welcome.h"
#include "project.h"
#include "variables.h"
#include "case.h"
#include "casewin.h"


// application globals
static MainWindow *g_mainWindow = 0;
static wxConfig *g_config = 0;
static const int g_verMajor = 2014;
static const int g_verMinor = 1;
static const int g_verMicro = 1;
static VarDatabase g_varDatabase;
static ConfigDatabase g_cfgDatabase;
static wxLogWindow *g_logWindow = 0;


enum { __idFirst = wxID_HIGHEST+592,

	ID_MAIN_MENU, ID_CASE_TABS, ID_CONTEXT_HELP, ID_PAGE_NOTES,
	ID_CASE_CREATE,
	__idCaseMenuFirst,
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
	__idCaseMenuLast,
	__idInternalFirst,
	ID_INTERNAL_IDE, ID_INTERNAL_RESTART,
	__idInternalLast
};

BEGIN_EVENT_TABLE( MainWindow, wxFrame )
	EVT_CLOSE( MainWindow::OnClose )
	EVT_MENU( wxID_NEW, MainWindow::OnCommand )
	EVT_MENU( wxID_OPEN, MainWindow::OnCommand )
	EVT_MENU( wxID_SAVE, MainWindow::OnCommand )
	EVT_MENU( wxID_SAVEAS, MainWindow::OnCommand )
	EVT_MENU( wxID_CLOSE, MainWindow::OnCommand )
	EVT_MENU( wxID_EXIT, MainWindow::OnCommand )
	EVT_BUTTON( ID_CASE_CREATE, MainWindow::OnCommand )
	EVT_BUTTON( ID_MAIN_MENU, MainWindow::OnCommand )
	EVT_LISTBOX( ID_CASE_TABS, MainWindow::OnCaseTabChange )
	EVT_BUTTON( ID_CASE_TABS, MainWindow::OnCaseTabButton )
	EVT_MENU_RANGE( __idCaseMenuFirst, __idCaseMenuLast, MainWindow::OnCaseMenu )
	EVT_MENU_RANGE( __idInternalFirst, __idInternalLast, MainWindow::OnInternalCommand )
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
	tools->Add( m_caseTabList, 1, wxALL|wxEXPAND, 0 );		
	tools->Add( new wxMetroButton( m_caseTabPanel, ID_CONTEXT_HELP, wxEmptyString, wxBITMAP_PNG_FROM_DATA(qmark), wxDefaultPosition, wxDefaultSize), 0, wxALL|wxEXPAND, 0 );
	
	m_caseNotebook = new wxSimplebook( m_caseTabPanel, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE );
		
	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( tools, 0, wxALL|wxEXPAND, 0 );
	sizer->Add( m_caseNotebook, 1, wxALL|wxEXPAND, 0 );
	m_caseTabPanel->SetSizer(sizer);

	m_topBook->SetSelection( 0 );
	
	wxAcceleratorEntry entries[20];
	entries[0].Set( wxACCEL_SHIFT, WXK_F7,  ID_INTERNAL_IDE );
	entries[1].Set( wxACCEL_SHIFT, WXK_F8,  ID_INTERNAL_RESTART );
	SetAcceleratorTable( wxAcceleratorTable(2,entries) );
}

bool MainWindow::CreateProject()
{
	if ( !CloseProject()) return false;

	m_topBook->SetSelection( 1 );

	CreateNewCase();
	return true;
}

bool MainWindow::CloseProject()
{
	wxArrayString cases = m_project.GetCases();
	for( size_t i=0;i<cases.size();i++ )
		DeleteCaseWindow( m_project.GetCase( cases[i] ) );

	m_project.Clear();
	m_project.SetModified( false );
	m_topBook->SetSelection( 0 );
	return true;
}

wxString MainWindow::GetUniqueCaseName( wxString base )
{	
	if ( base.IsEmpty() ) base = "untitled";
	int unique_num = 0;
	wxString suffix;
	while ( m_project.GetCases().Index( base + suffix ) >= 0 )
		suffix = wxString::Format(" (%d)", ++unique_num);

	return base + suffix;
}

void MainWindow::CreateNewCase( const wxString &_name )
{
	Case *c = m_project.AddCase( GetUniqueCaseName(_name ) );
	CreateCaseWindow( c );
}

CaseWindow *MainWindow::CreateCaseWindow( Case *c )
{
	if( CaseWindow *cw = GetCaseWindow(c) )
		return cw;

	wxString name = m_project.GetCaseName( c );	
	CaseWindow *win = new CaseWindow( m_caseNotebook, c );
	m_caseNotebook->AddPage( win, name, true );
	m_caseTabList->Append( name );
	m_caseTabList->SetSelection( m_caseTabList->Count()-1 );
	m_caseTabList->Refresh();

	return win;
}

void MainWindow::DeleteCaseWindow( Case *c )
{
	CaseWindow *cw = GetCaseWindow( c );
	if ( cw == 0 ) return;

	m_caseNotebook->DeletePage( m_caseNotebook->FindPage( cw ) );
	m_caseTabList->Remove( m_project.GetCaseName( c ) );
	m_caseTabList->Refresh();
}


void MainWindow::OnInternalCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_INTERNAL_IDE:
		wxMessageBox("No ide yet - soon though!");
		break;
	case ID_INTERNAL_RESTART:
		SamApp::Restart();
		wxMessageBox("Configuration and variable databases reloaded from startup.lk");
		break;
	}
}

void MainWindow::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_CASE_CREATE:
		CreateNewCase();
		break;
	case ID_MAIN_MENU:
		{
			wxMenu *add_obj_menu = new wxMenu;
			add_obj_menu->Append( wxID_ANY, "Weather data file" );
			add_obj_menu->Append( wxID_ANY, "3D Shading scene" );
			add_obj_menu->Append( wxID_ANY, "Script" );
			add_obj_menu->Append( wxID_ANY, "User defined variables" );
			add_obj_menu->Append( wxID_ANY, "Text file" );
			add_obj_menu->Append( wxID_ANY, "Report template" );

			wxMenu *edit_obj_menu = new wxMenu;
			edit_obj_menu->Append( wxID_ANY, "site measured (Weather File)" );
			edit_obj_menu->Append( wxID_ANY, "shading scene 1 (3D Shading)" );
			edit_obj_menu->Append( wxID_ANY, "shading scene 2 (3D Shading)" );
			edit_obj_menu->Append( wxID_ANY, "batch processor (Script)" );
			
			wxMenu *del_obj_menu = new wxMenu;
			del_obj_menu->Append( wxID_ANY, "site measured (Weather File)" );
			del_obj_menu->Append( wxID_ANY, "shading scene 1 (3D Shading)" );
			del_obj_menu->Append( wxID_ANY, "shading scene 2 (3D Shading)" );
			del_obj_menu->Append( wxID_ANY, "batch processor (Script)" );


			wxMenu menu;
			menu.Append( wxID_ANY, "Add to project", add_obj_menu );
			menu.Append( wxID_ANY, "Edit", edit_obj_menu );
			menu.Append( wxID_ANY, "Delete", del_obj_menu );
			menu.AppendSeparator();
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

CaseWindow *MainWindow::GetCaseWindow( Case *c )
{
	for( size_t i=0;i<m_caseNotebook->GetPageCount();i++ )
		if ( CaseWindow *cw = dynamic_cast<CaseWindow*>(m_caseNotebook->GetPage( i ) ) )
			if ( cw->GetCase() == c )
				return cw;

	return 0;
}

void MainWindow::OnCaseTabChange( wxCommandEvent &evt )
{
	int sel = evt.GetSelection();
	wxString name = m_caseTabList->GetLabel( sel );

	if ( Case *c = m_project.GetCase( name ) )
	{
		sel = m_caseNotebook->FindPage( GetCaseWindow( c ) );
		if ( sel >= 0 ) m_caseNotebook->SetSelection( sel );
	}
	//wxMessageBox( wxString::Format("Case tab changed: %d", evt.GetSelection() ) );
}

void MainWindow::OnCaseTabButton( wxCommandEvent &evt )
{
	wxMenu menu;
	
	menu.Append( ID_CASE_CONFIG, "Select technology and market" );
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
	size_t tab_sel = m_caseTabList->GetSelection();
	wxString case_name = m_caseTabList->GetLabel( tab_sel );
	Case *c = m_project.GetCase( case_name );
	CaseWindow *cw = GetCaseWindow( c );

	if ( c == 0 || cw == 0 ) return; // error
	
	switch( evt.GetId() )
	{
	case ID_CASE_RENAME:
		{
			wxString new_name = case_name;
			while( 1 )
			{
				new_name = wxGetTextFromUser( "Please enter a new name for the case:", "Query", case_name, this );
				if ( new_name == case_name || new_name.IsEmpty() ) return;

				if ( m_project.RenameCase( case_name, new_name ) )
				{
					m_caseTabList->SetLabel( tab_sel, new_name );
					m_caseTabList->Refresh();
					break;
				}

				if ( wxNO == wxMessageBox("A case with that name already exists in the project. Try again?", "Query", wxYES_NO, this ) )
					break;
			}
		}
	case ID_CASE_DELETE:
		if ( wxYES == wxMessageBox("Really delete case " + case_name + "?  This action cannot be reversed.", "Query", wxYES_NO, this ) )
		{
			DeleteCaseWindow( c );
			m_project.DeleteCase( case_name );
		}
		break;
	case ID_CASE_DUPLICATE:
		if ( Case *dup = dynamic_cast<Case*>(c->Duplicate()) )
		{
			m_project.AddCase( GetUniqueCaseName( case_name ), dup );
			CreateCaseWindow( dup );
		}
		break;
	};

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



ConfigDatabase::ConfigDatabase()
{
	m_curConfig = 0;
}

ConfigDatabase::~ConfigDatabase()
{
	Clear();
}

void ConfigDatabase::Clear()
{
	m_techList.clear();
	for( size_t i=0;i<m_configList.size();i++ )
		delete m_configList[i];
	m_configList.clear();
	m_curConfig = 0;
}

void ConfigDatabase::Add( const wxString &tech, const wxArrayString &fin )
{
	TechInfo x;
	x.Name = tech;
	x.FinancingOptions = fin;
	m_techList.push_back( x );
}

void ConfigDatabase::SetConfig( const wxString &t, const wxString &f )
{
	m_curConfig = Find( t, f );
}

void ConfigDatabase::AddPage( const wxString &name, const wxString &caption, const wxString &hlpcxt, 
		const wxArrayString &subpages, bool exclusive, 
		const wxString &exclvar )
{
	if ( m_curConfig == 0 ) return;

	InputPageInfo *ip = new InputPageInfo;
	ip->Name = name;
	ip->Caption = caption;
	ip->HelpContext = hlpcxt;
	ip->SubPages = subpages;
	ip->OrganizeAsExclusivePages = exclusive;
	ip->ExclusivePageVar = exclvar;

	m_curConfig->InputPages.push_back( ip );
}

wxArrayString ConfigDatabase::GetTechnologies()
{
	wxArrayString list;
	for( size_t i=0;i<m_techList.size();i++ )
		list.Add( m_techList[i].Name );
	return list;
}

wxArrayString ConfigDatabase::GetFinancingForTech(const wxString &tech)
{
	for( size_t i=0;i<m_techList.size();i++ )
		if ( m_techList[i].Name == tech )
			return m_techList[i].FinancingOptions;

	return wxArrayString();

}
	
std::vector<ConfigDatabase::InputPageInfo*> ConfigDatabase::GetInputPageList(const wxString &tech,
			const wxString &financing )
{
	if ( ConfigInfo *ci = Find( tech, financing ) ) return ci->InputPages;
	else return std::vector<InputPageInfo*>();
}
		
ConfigDatabase::ConfigInfo *ConfigDatabase::Find( const wxString &t, const wxString &f )
{
	for( size_t i=0;i<m_configList.size();i++ )
		if ( m_configList[i]->Technology == t
			&& m_configList[i]->Financing == f )
			return m_configList[i];

	return 0;
}


class SamLogWindow : public wxLogWindow
{
public:
	SamLogWindow( )	: wxLogWindow( 0, "sam-log" ) { 
		GetFrame()->SetPosition( wxPoint( 5, 5 ) );
		GetFrame()->SetClientSize( wxSize(1100,200) );
	}
	virtual bool OnFrameClose( wxFrame *frame ) {
		return false; // don't delete frame when closed by the user
	}
};

bool SamApp::OnInit()
{
#ifdef _DEBUG
	g_logWindow = new SamLogWindow;
	wxLog::SetActiveTarget( g_logWindow );
#endif

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

	Restart(); // loads and runs startup scripts, sets up variable databases

	g_mainWindow = new MainWindow();
	g_mainWindow->Show();

	return true;
}

int SamApp::OnExit()
{
	FileHistory().Save( Config() );

	if ( g_config != 0 ) delete g_config;

	wxSimpleCurlShutdown();

	wxLog::SetActiveTarget( 0 );
	return 0;
}

void SamApp::Restart()
{
	SamApp::VarDB().Clear();
	SamApp::CfgDB().Clear();

	// load configuration map
	wxString startup_script = GetRuntimePath() + "/startup.lk";
	wxLogStatus("loading startup script: " + startup_script );
	wxArrayString errors;
	if ( !LoadAndRunScriptFile( startup_script, &errors ) )
		wxLogStatus( wxJoin( errors, '\n' ) );
}

wxString SamApp::GetAppPath()
{
	if ( wxGetApp().argc > 0 ) return wxPathOnly( wxGetApp().argv[0] );
	else return wxEmptyString;
}

wxString SamApp::GetRuntimePath()
{
	return GetAppPath() + "/../runtime/";
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
VarDatabase &SamApp::VarDB() { return g_varDatabase; }
ConfigDatabase &SamApp::CfgDB() { return g_cfgDatabase; }

void fcall_outln( lk::invoke_t &cxt )
{
	LK_DOC("outln", "Output a data line to the console.", "(...):none");	
	wxString output;
	for (size_t i=0;i<cxt.arg_count();i++)
		output += cxt.arg(i).as_string();
	wxLogStatus( output );
}

void fcall_resetdb( lk::invoke_t &cxt )
{
	LK_DOC("resetdb", "Resets variable and configuration databases to empty", "(none):none");
	SamApp::CfgDB().Clear();
	SamApp::VarDB().Clear();
}

void fcall_addconfig( lk::invoke_t &cxt )
{
	LK_DOC("addconfig", "Add a technology+financing options", "( string:tech, array:financings ):none" );

	wxArrayString finlist;
	lk::vardata_t &fins = cxt.arg(1);
	for( size_t i=0;i<fins.length();i++ )
		finlist.Add( fins.index(i)->as_string() );
	SamApp::CfgDB().Add( cxt.arg(0).as_string(), finlist );

	wxLogStatus( "Configuration: " + cxt.arg(0).as_string() + "  -> [ " + wxJoin(finlist,';') + " ]" );
}

void fcall_setconfig( lk::invoke_t &cxt )
{
	LK_DOC("setconfig", "Sets the currently active configuration for editing", "(string:Tech, string:Financing):none");
	SamApp::CfgDB().SetConfig( cxt.arg(0).as_string(), cxt.arg(1).as_string() );
}

void fcall_addpage( lk::invoke_t &cxt )
{
	LK_DOC("addpage", "Add an page section to the currently active configuration (may have multiple sub pages).", "(string:name, string:caption, string:helpcxt, array:subpages, [boolean:exclusive, string:exclusive var name]" );
	
	wxString name = cxt.arg(0).as_string();
	wxString capt = cxt.arg(1).as_string();
	wxString help = cxt.arg(2).as_string();
	wxArrayString groups;
	lk::vardata_t &grps = cxt.arg(3);
	for( size_t i=0;i<grps.length();i++ )
		groups.Add( grps.index(i)->as_string() );

	bool subgrps = false;
	if ( cxt.arg_count() > 4 )
		subgrps = cxt.arg(4).as_boolean();
	wxString subgrpvar;
	if ( cxt.arg_count() > 5 )
		subgrpvar = cxt.arg(5).as_string();

	SamApp::CfgDB().AddPage( name, capt, help, groups, subgrps, subgrpvar );
}

static wxString s_defaultContext;

void fcall_setcontext( lk::invoke_t &cxt )
{
	LK_DOC( "setcontext", "Changes the current default context when adding variables using 'addvar'", "(string:context):none");
	s_defaultContext = cxt.arg(0).as_string();
}

void fcall_addvar( lk::invoke_t &cxt )
{
	LK_DOC( "addvar", "Adds a variable to the common database", "(string:name, integer:type, string:label, string:units, "
		"[string:context, string:indexlabels, array:flags, variant:default_value] )");

	wxString name = cxt.arg(0).as_string();
	int type = cxt.arg(1).as_integer();
	wxString label = cxt.arg(2).as_string();
	wxString units = cxt.arg(3).as_string();
	wxString context = s_defaultContext;
	wxString idxlabels;
	unsigned long flags = 0;

	VarValue defval;
	defval.SetType( type );

	if ( cxt.arg_count() > 4 && cxt.arg(4).as_string().Len() > 0 ) context = cxt.arg(4).as_string();
	if ( cxt.arg_count() > 5 ) idxlabels = cxt.arg(5).as_string();
	if ( cxt.arg_count() > 6 )
	{
		lk::vardata_t &fl = cxt.arg(6);
		for( size_t i=0;i<fl.length();i++ )
		{
			wxString flag = fl.index(i)->as_string().Lower();
			if ( flag == "hide_labels" ) flags |= VF_HIDE_LABELS;
			else if ( flag == "parameteric" ) flags |= VF_PARAMETRIC;
			else if ( flag == "indicator" ) flags |= VF_INDICATOR;
		}
	}

	if ( cxt.arg_count() > 7 )
		defval.Read( cxt.arg(7), false );
	
	SamApp::VarDB().Add( name, type, label, units, context, idxlabels, flags, defval );
}

void fcall_addeqn( lk::invoke_t &cxt )
{
	LK_DOC( "addeqn", "Adds an equation to the variable database", "(string:inputs, string:outputs, string:equation code):none");
	wxArrayString errors;
	if (!SamApp::VarDB().AddEquation( cxt.arg(0).as_string(), cxt.arg(1).as_string(), cxt.arg(2).as_string(), &errors ))
	{
		wxLogStatus("error adding equation via 'addeqn':");
		for( size_t i=0;i<errors.size();i++ ) wxLogStatus( errors[i] );
	}
}

bool SamApp::LoadAndRunScriptFile( const wxString &script_file, wxArrayString *errors )
{
	FILE *fp = fopen( script_file.c_str(), "r" );
	if ( !fp )
	{
		if (errors) errors->Add( "could not read: " + script_file );
		return false;
	}

	lk::input_stream p( fp );
	lk::parser parse( p );
	lk::node_t *tree = parse.script();

	fclose( fp );
		
	if ( parse.error_count() != 0 
		|| parse.token() != lk::lexer::END)
	{
		if ( errors )
		{
			for( size_t i=0;i<parse.error_count();i++ )
				errors->Add( parse.error(i) );
			errors->Add( "parsing did not reach end of input" );
		}
		return false;
	}
	else
	{
		lk::env_t lkenv;
		lkenv.register_func( fcall_outln, 0 );
		lkenv.register_func( fcall_resetdb, 0 );
		lkenv.register_func( fcall_addconfig, 0 );
		lkenv.register_func( fcall_setconfig, 0 );
		lkenv.register_func( fcall_addpage, 0 );		
		lkenv.register_func( fcall_setcontext, 0 );
		lkenv.register_func( fcall_addvar, 0 );
		lkenv.register_func( fcall_addeqn, 0 );
		lk::vardata_t lkvar;
		std::vector<lk_string> lkerrs;
		unsigned int lkctl = lk::CTL_NONE;
		bool ok = lk::eval( tree, &lkenv, lkerrs, lkvar, 0, lkctl, 0, 0 );
		if ( tree ) delete tree;
		
		if ( !ok && errors )
			for( size_t i=0;i<lkerrs.size();i++ )
				errors->Add( lkerrs[i] );

		return ok;
	}
}

IMPLEMENT_APP( SamApp );

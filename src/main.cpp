#include <wx/wx.h>
#include <wx/frame.h>
#include <wx/stc/stc.h>
#include <wx/webview.h>
#include <wx/simplebook.h>
#include <wx/panel.h>
#include <wx/busyinfo.h>

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
static wxArrayString g_appArgs;
static MainWindow *g_mainWindow = 0;
static wxConfig *g_config = 0;
static const int g_verMajor = 2014;
static const int g_verMinor = 1;
static const int g_verMicro = 1;
static ConfigDatabase g_cfgDatabase;
static VarDatabase g_varDatabase;
static EqnDatabase g_eqnDatabase;
static CallbackDatabase g_cbDatabase;
static FormDatabase g_formDatabase;
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
	ID_CASE_MOVE_LEFT,
	ID_CASE_MOVE_RIGHT,
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
	m_topBook->AddPage( m_welcomeScreen, wxT("Welcome to SAM") );


	m_caseTabPanel = new wxPanel( m_topBook );
	m_topBook->AddPage( m_caseTabPanel, wxT("Main project window") );

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
	entries[2].Set( wxACCEL_CMD, 'o', wxID_OPEN );
	entries[3].Set( wxACCEL_CMD, 's', wxID_SAVE );
	entries[4].Set( wxACCEL_CMD, 'w', wxID_CLOSE );
	entries[5].Set( wxACCEL_NORMAL, WXK_F2, ID_CASE_RENAME );
	SetAcceleratorTable( wxAcceleratorTable(6,entries) );
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
	if ( m_project.IsModified() )
	{
		int ret = wxMessageBox("The project '" + GetProjectDisplayName() + "' has been modified.  Save changes?", "Query", wxICON_EXCLAMATION|wxYES_NO|wxCANCEL, this );
		if (ret == wxYES)
		{
			Save( );
			if ( m_project.IsModified() ) // if failed to save, cancel
				return false;
		}
		else if (ret == wxCANCEL)
			return false;
	}

	wxArrayString cases = m_project.GetCaseNames();
	for( size_t i=0;i<cases.size();i++ )
		DeleteCaseWindow( m_project.GetCase( cases[i] ) );

	m_project.Clear();
	m_project.SetModified( false );

	m_projectFileName.Clear();
	UpdateFrameTitle();
	return true;
}

wxString MainWindow::GetProjectDisplayName()
{
	if ( m_projectFileName.IsEmpty() ) return wxT("untitled");
	else return m_projectFileName;
}

wxString MainWindow::GetUniqueCaseName( wxString base )
{	
	if ( base.IsEmpty() ) base = wxT("untitled");
	int unique_num = 0;
	wxString suffix;
	while ( m_project.GetCaseNames().Index( base + suffix ) >= 0 )
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

extern void ShowIDEWindow();

void MainWindow::OnInternalCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_INTERNAL_IDE:
		ShowIDEWindow();
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
			menu.Append( wxID_OPEN, "Open\tCtrl-O" );
			menu.Append( wxID_SAVE, "Save\tCtrl-S" );
			menu.Append( wxID_SAVEAS, "Save as" );
			menu.AppendSeparator();
			menu.Append( wxID_CLOSE, "Close" );
			menu.Append( wxID_EXIT );
			PopupMenu( &menu );
		}
		break;
	case wxID_NEW:
		CreateProject();
		break;
	case wxID_OPEN:
		{
			if ( !CloseProject() ) return;			
			wxFileDialog dlg(this, "Open SAM file", wxEmptyString, wxEmptyString, "SAM Project Files (*.sam)|*.sam", wxFD_OPEN );
			if (dlg.ShowModal() == wxID_OK)
				if( !LoadProject( dlg.GetPath() ) )
					wxMessageBox("Error loading project file:\n\n" 
						+ dlg.GetPath() + "\n\n" + m_project.GetLastError(), "Notice", wxOK, this );
		}
		break;
	case wxID_SAVEAS:
		SaveAs();
		break;
	case wxID_SAVE:
		Save();
		break;
	case wxID_CLOSE:
		CloseProject();
		m_topBook->SetSelection( 0 );
		break;
	case wxID_EXIT:
		Close();
		break;
	}
}


void MainWindow::Save()
{
	if ( m_projectFileName.IsEmpty() )
	{
		SaveAs();
		return;
	}

	if ( !SaveProject( m_projectFileName ) )
		wxMessageBox("Error writing project to disk:\n\n" + m_projectFileName, "Notice", wxOK, this );
			
	UpdateFrameTitle();
}

void MainWindow::SaveAs()
{

	wxFileDialog dlg( this, "Save SAM file as", wxEmptyString, wxEmptyString, "SAM Project File (*.sam)|*.sam", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
	if ( dlg.ShowModal() == wxID_OK )
	{
		m_projectFileName = dlg.GetPath();
		Save();
	}
	else
		return;
}


bool MainWindow::LoadProject( const wxString &file )
{
	m_project.Clear();
	if ( m_project.ReadArchive( file ) )
	{
		m_topBook->SetSelection( 1 );
		std::vector<Case*> cases = m_project.GetCases();
		for( size_t i=0;i<cases.size();i++ )
			CreateCaseWindow( cases[i] );

		// restore UI view properties
		wxArrayString ordered_tabs = wxSplit( m_project.GetProperty( "ui.case_tab_order" ), '|' );
		wxArrayString tabs = m_caseTabList->GetLabels();
		
		// re-add tabs in order that they were saved
		m_caseTabList->Clear();
		for( size_t i=0;i<ordered_tabs.size();i++ )
		{
			if ( tabs.Index( ordered_tabs[i] ) >= 0 ) 
			{ // only add a tab from the ordered list if it actually is a case
				m_caseTabList->Append( ordered_tabs[i] );
				tabs.Remove( ordered_tabs[i] );
			}
		}

		// add any tabs originally in the case list that the ordered list
		// did not have for some strange reason
		for( size_t i=0;i<tabs.size();i++)
			m_caseTabList->Append( tabs[i] );

		SwitchToCaseWindow( m_project.GetProperty( "ui.selected_case") );

		m_caseTabList->Refresh();

		m_projectFileName = file;
		UpdateFrameTitle();
		SamApp::FileHistory().AddFileToHistory( file );
		m_welcomeScreen->UpdateRecentList();
		return true;
	}
	else return false;
}

bool MainWindow::SaveProject( const wxString &file )
{
	wxBusyInfo busy( "Writing project data... " + wxFileNameFromPath(file), this );
	wxGetApp().Yield();
	wxMilliSleep( 100 );

	// save UI view properties before writing to disk
	wxArrayString tabs = m_caseTabList->GetLabels();
	m_project.SetProperty( "ui.case_tab_order", wxJoin( tabs, '|' ) );
	m_project.SetProperty( "ui.selected_case", m_caseTabList->GetStringSelection() );

	bool ok = m_project.WriteArchive( file );
	if ( ok ) m_project.SetModified( false );

	UpdateFrameTitle();
	return ok;
}

CaseWindow *MainWindow::GetCaseWindow( Case *c )
{
	for( size_t i=0;i<m_caseNotebook->GetPageCount();i++ )
		if ( CaseWindow *cw = dynamic_cast<CaseWindow*>(m_caseNotebook->GetPage( i ) ) )
			if ( cw->GetCase() == c )
				return cw;

	return 0;
}

void MainWindow::SwitchToCaseWindow( const wxString &case_name )
{
	if ( Case *c = m_project.GetCase( case_name ) )
	{
		int sel = m_caseNotebook->FindPage( GetCaseWindow( c ) );
		if ( sel >= 0 ) m_caseNotebook->SetSelection( sel );

		// update tab list too if needed
		if ( m_caseTabList->GetStringSelection() != case_name )
		{
			int idx = m_caseTabList->Find( case_name );
			if ( idx >= 0 )
			{
				m_caseTabList->SetSelection( idx );
				m_caseTabList->Refresh();
			}
		}
	}
}

void MainWindow::OnCaseTabChange( wxCommandEvent &evt )
{
	int sel = evt.GetSelection();
	SwitchToCaseWindow( m_caseTabList->GetLabel(sel) );
	//wxMessageBox( wxString::Format("Case tab changed: %d", evt.GetSelection() ) );
}

void MainWindow::OnCaseTabButton( wxCommandEvent &evt )
{
	wxMenu menu;
	
	menu.Append( ID_CASE_CONFIG, "Select technology and market" );
	menu.AppendSeparator();
	menu.Append( ID_CASE_RENAME, "Rename\tF2" );
	menu.Append( ID_CASE_DUPLICATE, "Duplicate" );
	menu.Append( ID_CASE_DELETE, "Delete" );
	menu.AppendSeparator();
	menu.Append( ID_CASE_MOVE_LEFT, "Move left" );
	menu.Append( ID_CASE_MOVE_RIGHT, "Move right" );
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
	case ID_CASE_CONFIG:
		{
			bool reset = true;
			wxString tech, fin;
			ShowConfigurationDialog( this, &tech, &fin, &reset );
		}
		break;
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
		break;
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
	case ID_CASE_MOVE_LEFT:
		m_caseTabList->ReorderLeft( tab_sel );
		break;
	case ID_CASE_MOVE_RIGHT:
		m_caseTabList->ReorderRight( tab_sel );
		break;
	};

	//wxMessageBox( wxString::Format("case id: %d, command %d", sel, evt.GetId() ) );
}

void MainWindow::OnClose( wxCloseEvent &evt )
{
	if ( !CloseProject() )
	{
		evt.Veto();
		return;
	}

	Destroy();
}

void MainWindow::UpdateFrameTitle()
{
	wxString title = wxT("SAM") + wxString(" ") + SamApp::VersionStr();
	if ( !m_projectFileName.IsEmpty() )	title += ": " + m_projectFileName;
	SetTitle( title );
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


ConfigDatabase::ConfigInfo::ConfigInfo()
	: Equations( &g_eqnDatabase )
{
}

ConfigDatabase::ConfigInfo::~ConfigInfo()
{
	for( size_t i=0;i<InputPages.size();i++) delete InputPages[i];
}


ConfigDatabase::ConfigDatabase()
	: m_emptyEqnList( &g_eqnDatabase )
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

void ConfigDatabase::AddInputPageGroup( const wxArrayString &pages, const wxString &caption, const wxString &hlpcxt, 
		bool exclusive, const wxString &exclvar )
{
	if ( m_curConfig == 0 ) return;

	InputPageGroup *ip = new InputPageGroup;
	ip->Pages = pages;
	ip->Caption = caption;
	ip->HelpContext = hlpcxt;
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
	
std::vector<ConfigDatabase::InputPageGroup*> &ConfigDatabase::GetInputPages(const wxString &tech,
			const wxString &financing )
{
	if ( ConfigInfo *ci = Find( tech, financing ) ) return ci->InputPages;
	else return std::vector<InputPageGroup*>();
}

VarInfoLookup &ConfigDatabase::GetVariables( const wxString &tech, const wxString &financing )
{
	if ( ConfigInfo *ci = Find( tech, financing ) ) return ci->Variables;
	else return m_emptyVarList;
}

EqnFastLookup &ConfigDatabase::GetEquations( const wxString &tech, const wxString &financing )
{
	if ( ConfigInfo *ci = Find( tech, financing ) ) return ci->Equations;
	else return m_emptyEqnList;
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

extern void RegisterInputPageObjects(); // decl of function in "inputpage.h"

bool SamApp::OnInit()
{
	SetAppName( "SAM" );
	SetVendorName( "NREL" );

	for( int i=0;i<argc;i++ )
		g_appArgs.Add( argv[i] );

	if ( g_appArgs.Count() < 1 )
	{
		wxMessageBox("Internal error - cannot determine runtime folder from startup argument 0" );
		return false;
	}

#ifdef _DEBUG
	g_logWindow = new SamLogWindow;
	wxLog::SetActiveTarget( g_logWindow );
#endif

	// register all the object types that can
	// be read or written to streams.
	ObjectTypes::Register( new StringHash );
	ObjectTypes::Register( new Case );

	// register all input page UI objects 
	RegisterInputPageObjects();

	wxInitAllImageHandlers();
	wxSimpleCurlInit();
	
	SplashScreen splash;
	splash.CenterOnScreen();
	splash.Show();
	splash.Update();

	Yield(true);
	wxMilliSleep( 500 );

	g_config = new wxConfig( "SAMnt", "NREL" );
	FileHistory().Load( Settings() );

	Restart(); // loads and runs startup scripts, sets up variable databases

	g_mainWindow = new MainWindow();
	g_mainWindow->Show();

	ShowIDEWindow();

	return true;
}

int SamApp::OnExit()
{
	FileHistory().Save( Settings() );

	if ( g_config != 0 ) delete g_config;

	wxSimpleCurlShutdown();

	wxLog::SetActiveTarget( 0 );
	return 0;
}

void SamApp::Restart()
{
	SamApp::Config().Clear();

	// load configuration map
	wxString startup_script = GetRuntimePath() + "/startup.lk";
	wxLogStatus("loading startup script: " + startup_script );
	wxArrayString errors;
	if ( !LoadAndRunScriptFile( startup_script, &errors ) )
		wxLogStatus( wxJoin( errors, '\n' ) );
}

wxString SamApp::GetAppPath()
{
	return wxPathOnly( g_appArgs[0] );
}

wxString SamApp::GetRuntimePath()
{
	return GetAppPath() + "/../runtime/";
}

wxConfig &SamApp::Settings()
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

ConfigDatabase &SamApp::Config() { return g_cfgDatabase; }
VarDatabase &SamApp::Variables() { return g_varDatabase; }
EqnDatabase &SamApp::Equations() { return g_eqnDatabase; }
CallbackDatabase &SamApp::Callbacks() { return g_cbDatabase; }
FormDatabase &SamApp::Forms() { return g_formDatabase; }

static void fcall_dbgoutln( lk::invoke_t &cxt )
{
	LK_DOC("outln", "Output a data line to the console.", "(...):none");	
	wxString output;
	for (size_t i=0;i<cxt.arg_count();i++)
		output += cxt.arg(i).as_string();
	wxLogStatus( output );
}

static void fcall_resetdb( lk::invoke_t &cxt )
{
	LK_DOC("resetdb", "Resets variable and configuration databases to empty", "(none):none");
	SamApp::Config().Clear();
}

static void fcall_addconfig( lk::invoke_t &cxt )
{
	LK_DOC("addconfig", "Add a technology+financing options", "( string:tech, array:financings ):none" );

	wxArrayString finlist;
	lk::vardata_t &fins = cxt.arg(1);
	for( size_t i=0;i<fins.length();i++ )
		finlist.Add( fins.index(i)->as_string() );
	SamApp::Config().Add( cxt.arg(0).as_string(), finlist );

	wxLogStatus( "Configuration: " + cxt.arg(0).as_string() + "  -> [ " + wxJoin(finlist,';') + " ]" );
}

static void fcall_setconfig( lk::invoke_t &cxt )
{
	LK_DOC("setconfig", "Sets the currently active configuration for editing", "(string:Tech, string:Financing):none");
	SamApp::Config().SetConfig( cxt.arg(0).as_string(), cxt.arg(1).as_string() );
}

static void fcall_addpage( lk::invoke_t &cxt )
{
	LK_DOC("addpage", "Add an input page group to the currently active configuration (may have multiple pages).", "(array:pages, [string:caption, string:helpcxt, boolean:exclusive, string:exclusive var name]" );
	
	wxArrayString pages;
	lk::vardata_t &grps = cxt.arg(0);
	for( size_t i=0;i<grps.length();i++ )
		pages.Add( grps.index(i)->as_string() );

	if ( pages.size() == 0 ) return;

	wxString capt = pages[0];
	if ( cxt.arg_count() > 1 )
		capt = cxt.arg(1).as_string();

	wxString help = pages[0];
	if (cxt.arg_count() > 2 )
		wxString help = cxt.arg(2).as_string();

	bool subgrps = false;
	if ( cxt.arg_count() > 3 )
		subgrps = cxt.arg(3).as_boolean();

	wxString subgrpvar;
	if ( cxt.arg_count() > 4 )
		subgrpvar = cxt.arg(4).as_string();

	SamApp::Config().AddInputPageGroup( pages, capt, help, subgrps, subgrpvar );
}

static wxString s_defaultContext;

static void fcall_setcontext( lk::invoke_t &cxt )
{
	LK_DOC( "setcontext", "Changes the current default context when adding variables using 'addvar'", "(string:context):none");
	s_defaultContext = cxt.arg(0).as_string();
}

lk::fcall_t* startup_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_dbgoutln,
		fcall_resetdb,
		fcall_addconfig,
		fcall_setconfig,
		fcall_addpage,
		fcall_setcontext,
		0 };
	return (lk::fcall_t*)vec;
}

/*
static void fcall_cfgtree( lk::invoke_t &cxt )
{
	LK_DOC("cfgtree", "Adds an item to a configuration tree", "(string:which tree 'tech' or 'fin', string:label, string:caption, string:bitmap name, [string:cfg type name], [string:parent]");
	
	std::vector<ConfigDatabase::TreeItem*> &tree = (cxt.arg(0).as_string() == "tech") ? SamApp::Config().TechTree() : SamApp::Config().FinTree();
	wxString label = cxt.arg(1).as_string();
	wxString capt = cxt.arg(2).as_string();
	wxString bmp = cxt.arg(3).as_string();
	wxString type;
	if ( cxt.arg_count() > 4 ) 
		type = cxt.arg(4).as_string();
	wxString parent_name;
	if ( cxt.arg_count() > 5 )
		parent_name = cxt.arg(5).as_string();

	ConfigDatabase::TreeItem *parent = NULL;
	if ( !parent_name.IsEmpty() )
		for( size_t i=0;i<tree.size();i++ )
			if ( tree[i]->Label == parent_name )
				parent = tree[i];

	ConfigDatabase::TreeItem *item = new ConfigDatabase::TreeItem;
	item->Label = label;
	item->Description = capt;
	item->BmpName = bmp;
	item->TypeTag = type;

	if ( parent != 0 )
		parent->Children.push_back( item );
	else
		tree.push_back( item );
}
*/

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
		lkenv.register_funcs( startup_funcs(), 0 );
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



enum { ID_TechTree = wxID_HIGHEST+98, ID_FinTree };

BEGIN_EVENT_TABLE(ConfigDialog, wxDialog)
	EVT_LISTBOX( ID_TechTree, ConfigDialog::OnTechTree)
	EVT_BUTTON( wxID_HELP, ConfigDialog::OnHelp )
END_EVENT_TABLE()

ConfigDialog::ConfigDialog( wxWindow *parent, const wxSize &size )
	: wxDialog( parent, wxID_ANY, wxEmptyString, wxDefaultPosition, size, wxBORDER_NONE )
{
	CenterOnParent();
	SetEscapeId(wxID_CANCEL);
	
	m_pTech = new wxMetroListBox( this, ID_TechTree );
	m_pFin = new wxMetroListBox( this, ID_FinTree );

	wxBoxSizer *choice_sizer = new wxBoxSizer( wxHORIZONTAL );
	choice_sizer->Add( m_pTech, 1, wxALL|wxEXPAND, 0 );
	choice_sizer->Add( m_pFin, 1, wxALL|wxEXPAND, 0 );

	wxStaticText *label = new wxStaticText( this, wxID_ANY,
		"Project configuration: select a technology and then a financing option." );
	wxFont font = label->GetFont();
	font.SetWeight( wxFONTWEIGHT_BOLD );
	font.SetPointSize( font.GetPointSize() + 1 );
	label->SetFont( font );

	m_pChkUseDefaults = new wxCheckBox(this, wxID_ANY, "Reset new inputs to Tech/Market-specific default values" );	
	m_pChkUseDefaults->SetValue(true);

	wxBoxSizer *hbox = new wxBoxSizer (wxHORIZONTAL );
	hbox->Add( new wxButton(this, wxID_HELP, "Help..." ), 0, wxALL|wxEXPAND, 4 );
	hbox->Add( m_pChkUseDefaults, 0, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, 4 );
	hbox->AddStretchSpacer();
	hbox->Add( CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL, 4 );
	
	wxBoxSizer *vbox = new wxBoxSizer( wxVERTICAL );
	vbox->Add( label, 0, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, 8 );
	vbox->Add( choice_sizer, 1, wxALL|wxEXPAND, 0 );
	vbox->Add( hbox, 0, wxALL|wxEXPAND, 0 );	
	SetSizer( vbox );
	
	wxAcceleratorEntry entries[1];
	entries[0].Set( ::wxACCEL_NORMAL, WXK_F1, wxID_HELP );
	SetAcceleratorTable( wxAcceleratorTable(1,entries) );

	PopulateTech();

}

bool ConfigDialog::ResetToDefaults()
{
	return m_pChkUseDefaults->GetValue();
}
void ConfigDialog::SetConfiguration(const wxString &t, const wxString &f)
{
	m_t = t;
	m_f = f;
}

void ConfigDialog::ShowResetCheckbox(bool b)
{
	m_pChkUseDefaults->Show(b);
}

bool ConfigDialog::GetConfiguration(wxString &t, wxString &f)
{
	t = m_pTech->GetValue();
	f = m_pFin->GetValue();
	return true;
}


void ConfigDialog::OnDoubleClick(wxCommandEvent &evt)
{
	EndModal( wxID_OK );
}

void ConfigDialog::PopulateTech()
{
	m_pTech->Clear();
	wxArrayString list = SamApp::Config().GetTechnologies();
	for( size_t i=0;i<list.Count();i++)
		m_pTech->Add(  list[i] );
	
	m_pTech->Invalidate();
}

void ConfigDialog::OnTechTree( wxCommandEvent &evt )
{
	m_pFin->Clear();
	wxArrayString list = SamApp::Config().GetFinancingForTech( m_pTech->GetValue() );
	for( size_t i=0;i<list.Count();i++)
		m_pFin->Add( list[i] );

	m_pFin->Invalidate();
}


void ConfigDialog::OnHelp(wxCommandEvent &evt)
{
	SamApp::ShowHelp("Technology Market");
}

bool ShowConfigurationDialog( wxWindow *parent, wxString *tech, wxString *fin, bool *reset )
{
	if ( parent == 0 ) return false;

	wxPoint pos = parent->ClientToScreen( wxPoint(0,0) );
	wxSize size = parent->GetClientSize();

	wxFrame *trans = new wxFrame( parent, wxID_ANY, wxEmptyString,  pos, size, 
		wxBORDER_NONE | wxFRAME_FLOAT_ON_PARENT | wxFRAME_NO_TASKBAR );
	trans->SetBackgroundColour( *wxLIGHT_GREY );
	trans->SetTransparent( 200 );
	trans->Show();


	ConfigDialog *dlg = new ConfigDialog( parent );
	dlg->ShowResetCheckbox( *reset );
	if ( !tech->IsEmpty() && !fin->IsEmpty() )
		dlg->SetConfiguration( *tech, *fin );

	bool result = false;
	if ( dlg->ShowModal() == wxID_OK )
	{
		dlg->GetConfiguration( *tech, *fin );
		*reset = dlg->ResetToDefaults();
		result = true;
	}

	dlg->Destroy();
	trans->Destroy();
	return result;
}


IMPLEMENT_APP( SamApp );

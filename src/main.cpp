//#define __BETARELEASE__ 1  // comment this line out to disable beta option
//#define __BETAWILLEXPIRE__ 1 // comment this line out to disable expiration of beta
#define __BETAEXPIRE_DAY__ 26
#define __BETAEXPIRE_MONTH__ wxDateTime::Nov
#define __BETAEXPIRE_YEAR__ 2014

static const char *beta_disclaimer =
"Notice: Beta versions of SAM are provided as-is and may change without notice."
	"  Beta software may not work in the same way as a final version, and features and functionality may be changed, enhanced, or removed without notice."
	"  The software is considered generally stable and useful but may produce incorrect results, crash, or behave otherwise unexpectedly."
	"  There is no guarantee that files opened, saved, or created with this beta software will be usable with other versions of SAM."
	"  This notice appears in addition to the full disclaimer of warranty, accessible in Help/About."
	"\n\nThank you for trying SAM Beta.  We look forward to your feedback.";


#include <set>

#include <wx/wx.h>
#include <wx/frame.h>
#include <wx/stc/stc.h>
#include <wx/webview.h>
#include <wx/simplebook.h>
#include <wx/panel.h>
#include <wx/busyinfo.h>
#include <wx/dir.h>
#include <wx/wfstream.h>
#include <wx/datstrm.h>
#include <wx/grid.h>
#include <wx/stdpaths.h>
#include <wx/webview.h>
#include <wx/txtstrm.h>
#include <wx/buffer.h>
#include <wx/display.h>

#include <wex/metro.h>
#include <wex/icons/cirplus.cpng>
#include <wex/icons/qmark.cpng>
#include <wex/utils.h>

#include <ssc/sscapi.h>

#include "../resource/nrel_small.cpng"
#include "../resource/main_menu.cpng"
#include "../resource/notes_white.cpng"

#include <lk_absyn.h>
#include <lk_parse.h>
#include <lk_eval.h>
#include <lk_stdlib.h>

#include "main.h"
#include "registration.h"
#include "welcome.h"
#include "project.h"
#include "variables.h"
#include "case.h"
#include "casewin.h"
#include "invoke.h"
#include "library.h"
#include "uiobjects.h"
#include "variablegrid.h"
#include "script.h"

// application globals
static wxArrayString g_appArgs;
static MainWindow *g_mainWindow = 0;
static wxConfig *g_config = 0;
static const int g_verMajor = 2014;
static const int g_verMinor = 10;
static const int g_verMicro = 1;
static ConfigDatabase g_cfgDatabase;
static InputPageDatabase g_uiDatabase;
static wxLogWindow *g_logWindow = 0;
static ScriptDatabase g_globalCallbacks;

class SamLogWindow : public wxLogWindow
{
public:
	SamLogWindow( )	: wxLogWindow( 0, "sam-log" ) { 
		GetFrame()->SetPosition( wxPoint( 5, 5 ) );
		GetFrame()->SetClientSize( wxSize(1100,200) );
	}
	virtual bool OnFrameClose( wxFrame *frame ) {
		g_logWindow = 0; // clear the global pointer, then delete the frame
		return true;
	}
	
	static void Setup()
	{
		if ( g_logWindow != 0 )
			delete g_logWindow;

		g_logWindow = new SamLogWindow;
		wxLog::SetActiveTarget( g_logWindow );
		g_logWindow->Show();
	}
};


enum { __idFirst = wxID_HIGHEST+592,

	ID_MAIN_MENU, ID_CASE_TABS, ID_PAGE_NOTES,
	ID_CASE_CREATE, ID_RUN_ALL_CASES, ID_SAVE_HOURLY,
	ID_NEW_SCRIPT, ID_OPEN_SCRIPT, ID_BROWSE_INPUTS,
	__idCaseMenuFirst,
	ID_CASE_CONFIG,
	ID_CASE_RENAME,
	ID_CASE_DUPLICATE,
	ID_CASE_DELETE,
	ID_CASE_REPORT,
	ID_CASE_EXCELEXCH,
	ID_CASE_SIMULATE,
	ID_CASE_RESET_DEFAULTS,
	ID_CASE_CLEAR_RESULTS,
	ID_CASE_IMPORT,
	ID_CASE_MOVE_LEFT,
	ID_CASE_MOVE_RIGHT,
	__idCaseMenuLast,
	__idInternalFirst,
		ID_INTERNAL_IDE, ID_INTERNAL_RESTART, ID_INTERNAL_SHOWLOG, 
		ID_INTERNAL_DATAFOLDER, ID_INTERNAL_CASE_VALUES, ID_SAVE_CASE_DEFAULTS, ID_INTERNAL_INVOKE_SSC_DEBUG,
	__idInternalLast
};

BEGIN_EVENT_TABLE( MainWindow, wxFrame )
	EVT_CLOSE( MainWindow::OnClose )
	EVT_MENU( wxID_HELP, MainWindow::OnCommand )
	EVT_MENU( ID_SAVE_HOURLY, MainWindow::OnCommand )
	EVT_MENU( wxID_NEW, MainWindow::OnCommand )
	EVT_MENU( ID_NEW_SCRIPT, MainWindow::OnCommand )
	EVT_MENU( ID_OPEN_SCRIPT, MainWindow::OnCommand )
	EVT_MENU(ID_BROWSE_INPUTS, MainWindow::OnCommand)
	EVT_MENU(wxID_OPEN, MainWindow::OnCommand)
	EVT_MENU( wxID_SAVE, MainWindow::OnCommand )
	EVT_MENU( wxID_SAVEAS, MainWindow::OnCommand )
	EVT_MENU( ID_SAVE_HOURLY, MainWindow::OnCommand )
	EVT_MENU( wxID_CLOSE, MainWindow::OnCommand )
	EVT_MENU( wxID_EXIT, MainWindow::OnCommand )
	EVT_BUTTON(ID_CASE_CREATE, MainWindow::OnCommand)
	EVT_MENU(ID_RUN_ALL_CASES, MainWindow::OnCommand)
	EVT_BUTTON(ID_MAIN_MENU, MainWindow::OnCommand)
	EVT_LISTBOX( ID_CASE_TABS, MainWindow::OnCaseTabChange )
	EVT_BUTTON( ID_CASE_TABS, MainWindow::OnCaseTabButton )
	EVT_BUTTON( wxID_HELP, MainWindow::OnCommand )
	EVT_BUTTON( ID_PAGE_NOTES, MainWindow::OnCommand )
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

	wxMetroButton *metbut = 0;

	wxBoxSizer *tools = new wxBoxSizer( wxHORIZONTAL );
	tools->Add( m_mainMenuButton = new wxMetroButton( m_caseTabPanel, ID_MAIN_MENU, wxEmptyString, wxBITMAP_PNG_FROM_DATA( main_menu ), wxDefaultPosition, wxDefaultSize /*, wxMB_DOWNARROW */), 0, wxALL|wxEXPAND, 0 );
	tools->Add( new wxMetroButton( m_caseTabPanel, ID_CASE_CREATE, "New", wxBITMAP_PNG_FROM_DATA( cirplus ), wxDefaultPosition, wxDefaultSize), 0, wxALL|wxEXPAND, 0 );
	m_caseTabList = new wxMetroTabList( m_caseTabPanel, ID_CASE_TABS, wxDefaultPosition, wxDefaultSize, wxMT_MENUBUTTONS );
	tools->Add( m_caseTabList, 1, wxALL|wxEXPAND, 0 );		
	tools->Add( metbut = new wxMetroButton( m_caseTabPanel, ID_PAGE_NOTES, wxEmptyString, wxBITMAP_PNG_FROM_DATA( notes_white ), wxDefaultPosition, wxDefaultSize), 0, wxALL|wxEXPAND, 0 );
	metbut->SetToolTip( "Add a page note" );
	tools->Add( new wxMetroButton( m_caseTabPanel, wxID_HELP, wxEmptyString, wxBITMAP_PNG_FROM_DATA(qmark), wxDefaultPosition, wxDefaultSize), 0, wxALL|wxEXPAND, 0 );
	
	m_caseNotebook = new wxSimplebook( m_caseTabPanel, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE );
		
	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( tools, 0, wxALL|wxEXPAND, 0 );
	sizer->Add( m_caseNotebook, 1, wxALL|wxEXPAND, 0 );
	m_caseTabPanel->SetSizer(sizer);

	m_topBook->SetSelection( 0 );
	
	std::vector<wxAcceleratorEntry> entries;
	entries.push_back( wxAcceleratorEntry( wxACCEL_SHIFT, WXK_F5,  ID_INTERNAL_INVOKE_SSC_DEBUG ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_SHIFT, WXK_F7,  ID_INTERNAL_IDE ) ) ;
	entries.push_back( wxAcceleratorEntry( wxACCEL_SHIFT, WXK_F8,  ID_INTERNAL_RESTART ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_SHIFT, WXK_F4,  ID_INTERNAL_SHOWLOG ) );
	entries.push_back(wxAcceleratorEntry(wxACCEL_SHIFT, WXK_F12, ID_INTERNAL_CASE_VALUES));
	entries.push_back(wxAcceleratorEntry(wxACCEL_SHIFT, WXK_F11, ID_RUN_ALL_CASES));
	entries.push_back(wxAcceleratorEntry(wxACCEL_SHIFT, WXK_F10, ID_SAVE_CASE_DEFAULTS));
	entries.push_back(wxAcceleratorEntry(wxACCEL_SHIFT, WXK_F9, ID_INTERNAL_DATAFOLDER));
	entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 'o', wxID_OPEN ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 's', wxID_SAVE ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 'w', wxID_CLOSE ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_NORMAL, WXK_F2, ID_CASE_RENAME ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_NORMAL, WXK_F5, ID_CASE_SIMULATE ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_NORMAL, WXK_F6, ID_CASE_REPORT ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_NORMAL, WXK_F1, wxID_HELP ) );
	SetAcceleratorTable( wxAcceleratorTable( entries.size(), &entries[0] ) );
}

bool MainWindow::CreateProject()
{
	if ( !CloseProject()) return false;

	// This will switch tabs to case panel from welcome screen if needed
	CreateNewCase();

	//CreateNewCase( wxEmptyString, "Flat Plate PV", "Residential" );
	//CreateNewCase( wxEmptyString, "PVWatts", "None" );
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

wxString MainWindow::GetProjectFileName()
{
	return m_projectFileName;
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

bool MainWindow::CreateNewCase( const wxString &_name, wxString tech, wxString fin )
{
	if ( tech.IsEmpty() || fin.IsEmpty() )
	{
		bool reset = false;
		if (!ShowConfigurationDialog( this, &tech, &fin, &reset ))
			return false;
	}
	
	if ( 0 == SamApp::Config().Find( tech, fin ) )
	{
		wxMessageBox("Internal error: could not locate configuration information for " + tech + "/" + fin );
		return false;
	}

	if ( m_topBook->GetSelection() != 1 )
		m_topBook->SetSelection( 1 ); // switch to cases view if currently in welcome window

	Case *c = m_project.AddCase( GetUniqueCaseName(_name ) );
	c->SetConfiguration( tech, fin );
	c->LoadDefaults();
	CreateCaseWindow( c );
	return true;
}

CaseWindow *MainWindow::CreateCaseWindow( Case *c )
{
	if( CaseWindow *cw = GetCaseWindow(c) )
		return cw;

	wxString name = m_project.GetCaseName( c );	
	
	wxBusyCursor bc;

	m_caseNotebook->Freeze();
	CaseWindow *win = new CaseWindow( m_caseNotebook, c );
	m_caseNotebook->AddPage( win, name, true );
	m_caseNotebook->Thaw();

	m_caseTabList->Append( name );
	m_caseTabList->SetSelection( m_caseTabList->Count()-1 );
	m_caseTabList->Refresh();
	wxGetApp().Yield();

	// when creating a new case, at least 
	// show the first input page
	wxArrayString pages = win->GetInputPages();
	if ( pages.size() > 0 )
		win->SwitchToInputPage( pages[0] );

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
	switch (evt.GetId())
	{
	case ID_INTERNAL_INVOKE_SSC_DEBUG:
		if ( Case *cc = GetCurrentCase() )
		{
			cc->BaseCase().Clear();
			if (!cc->BaseCase().Invoke())
				wxShowTextMessageDialog( wxJoin( cc->BaseCase().GetAllMessages(), '\n') );
		}
		break;
	case ID_INTERNAL_IDE:
		ShowIDEWindow();
		break;
	case ID_INTERNAL_RESTART:
		SamApp::Restart();
		wxMessageBox("Configuration and variable databases reloaded from startup.lk");
		break;
	case ID_INTERNAL_SHOWLOG:
		SamLogWindow::Setup();
		break;
	case ID_INTERNAL_DATAFOLDER:
		wxLaunchDefaultBrowser(SamApp::GetUserLocalDataDir());
		break;
	case ID_INTERNAL_CASE_VALUES:
		
		if (Case *cc = GetCurrentCase())
		{
			//CaseVarGrid(cases);
			VariableGridFrame *var_frame = new VariableGridFrame(this, &m_project, cc);
		}
		break;
	case ID_SAVE_CASE_DEFAULTS:
		if (Case *cc = GetCurrentCase())
		{
			cc->SaveDefaults();
		}
		break;
	}
}

void MainWindow::CaseVarGrid(std::vector<Case*> &cases)
{
	if (cases.size() > 0)
	{
		wxArrayString col_hdrs;
		wxString title;
		std::vector<VarTable> var_table_vec;
		std::vector<VarInfoLookup> var_info_lookup_vec;
		if (cases.size() == 1)
		{
			col_hdrs.push_back("Variable");
			col_hdrs.push_back("Label");
			wxString case_name = m_project.GetCaseName(cases[0]);
			col_hdrs.push_back(case_name);
			title = "Current Case Values: " + case_name;
			var_table_vec.push_back(cases[0]->Values());
			var_info_lookup_vec.push_back(cases[0]->Variables());
		}
		else
		{
			title = "Case comparison";
			col_hdrs = m_project.GetCaseNames();
			col_hdrs.Insert("Label", 0);
			col_hdrs.Insert("Variable", 0);
			for (std::vector<Case*>::iterator it = cases.begin(); it != cases.end(); ++it)
			{
				var_table_vec.push_back((*it)->Values());
				var_info_lookup_vec.push_back((*it)->Variables());
			}
		}

		wxFrame *frame = new wxFrame(this, wxID_ANY, title, wxDefaultPosition, wxSize(400, 700));
		wxGrid *grid = new wxGrid(frame, wxID_ANY);

		size_t num_cols = col_hdrs.Count();

		// variable names
		std::set<wxString> var_names;

		for (std::vector<VarTable>::iterator it = var_table_vec.begin(); it != var_table_vec.end(); ++it)
		{
			wxArrayString as = it->ListAll();
			for (size_t i = 0; i < as.Count();i++)
				var_names.insert(as[i]);
		}
		size_t num_rows = var_names.size();

		// variable labels
		wxArrayString var_labels;

		for (std::set<wxString>::iterator idx = var_names.begin(); idx != var_names.end(); ++idx)
		{
			wxString str_label = " ";
			for (std::vector<VarInfoLookup>::iterator it = var_info_lookup_vec.begin(); it != var_info_lookup_vec.end(); ++it)
			{
				if ((*it).Lookup(*idx)) 
					str_label = (*it).Label(*idx);
			}
			var_labels.push_back(str_label);
		}

		grid->CreateGrid(num_rows, num_cols);
		size_t idx = 0;
		grid->Freeze();
		// wxGrid only support 6500 characters per cell (empirically determined) - use 1024 for display
		size_t col = 0, row=0;
		int width, height;
		std::vector<int> col_width(num_cols, 60);
		for (std::set<wxString>::iterator idx = var_names.begin(); idx != var_names.end(); ++idx)
		{
			GetTextExtent(*idx, &width, &height);
			if ((width + 10) > col_width[col]) col_width[col] = width + 10;
			grid->SetCellValue(row, col++, *idx); //name
			if (row < var_labels.Count())
			{
				GetTextExtent(var_labels[row], &width, &height);
				if ((width + 10) > col_width[col]) col_width[col] = width + 10;
				grid->SetCellValue(row, col++, var_labels[row]); //label
			}
			for (std::vector<VarTable>::iterator it = var_table_vec.begin(); it != var_table_vec.end(); ++it)
			{
				wxString str_val = "";
				if (it->Get(*idx)) str_val = it->Get(*idx)->AsString();
				if (str_val.Length() > 1024) str_val = str_val.Left(1024) + "...";
				grid->SetCellValue(row, col++, str_val);
			}
			row++;
			col = 0;
		}

		// go through all rows for case comparison and only show unequal values
		if (cases.size() > 1)
		{
			for (row = 0; row < num_rows; row++)
			{
				wxString str_val = grid->GetCellValue(row, 2);
				bool same_val = true;
				for (col = 3; col < num_cols; col++)
					same_val = same_val && (str_val == grid->GetCellValue(row, col));
				if (same_val) grid->HideRow(row);
			}
		}
		//grid->AutoSizeColumns();
		// column headers
		for (col = 0; col < col_hdrs.Count(); col++)
		{
			grid->SetColLabelValue(col, col_hdrs[col]);
			GetTextExtent(col_hdrs[col], &width, &height);
			if ((width + 10) > col_width[col]) col_width[col] = width + 10;
			grid->SetColumnWidth(col, col_width[col]);
		}
		grid->Thaw();

		frame->Show();
	}

}

void MainWindow::OnCommand( wxCommandEvent &evt )
{
	CaseWindow *cwin = GetCurrentCaseWindow();

	switch( evt.GetId() )
	{
	case wxID_HELP:
		SamApp::ShowHelp( cwin ? cwin->GetCurrentContext() : wxString("welcome_page") );
		break;
	case ID_PAGE_NOTES:
		if ( cwin != 0 )
			cwin->ShowPageNote();
		break;
	case ID_CASE_CREATE:
		CreateNewCase();
		break;
	case ID_RUN_ALL_CASES:
		if (m_project.GetCases().size() > 0)
		{
			std::vector<Case*> cases = m_project.GetCases();
			for (std::vector<Case*>::iterator it = cases.begin(); it != cases.end(); ++it)
			{
				CaseWindow *cw = GetCaseWindow(*it);
				if ( cw ) cw->RunBaseCase();
			}
		}
		break;
	case ID_MAIN_MENU:
		{
			wxPoint p = m_mainMenuButton->ClientToScreen( wxPoint( 0, m_mainMenuButton->GetClientSize().y ) );
			wxMetroPopupMenu menu;
			menu.Append( wxID_NEW, "New project\tCtrl-N" );
			menu.Append( ID_NEW_SCRIPT, "New script" );
			menu.AppendSeparator();
			menu.Append( wxID_OPEN, "Open project\tCtrl-O" );
			menu.Append( ID_OPEN_SCRIPT, "Open script" );
			menu.AppendSeparator();
			menu.Append( wxID_SAVE, "Save\tCtrl-S" );
			menu.Append( wxID_SAVEAS, "Save as..." );
			menu.Append( ID_SAVE_HOURLY, "Save with hourly results");
			menu.AppendSeparator();
			menu.Append( ID_BROWSE_INPUTS, "Browse case inputs...");
			menu.AppendSeparator();
			menu.Append( wxID_CLOSE, "Close\tCtrl-W" );
			menu.Append( wxID_EXIT, "Quit" );
			menu.Popup( this, p );
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
	case ID_NEW_SCRIPT:
		ScriptWindow::CreateNewWindow();
		break;
	case ID_OPEN_SCRIPT:
		ScriptWindow::OpenFiles();
		break;
	case ID_BROWSE_INPUTS:
		if (m_project.GetCases().size() > 0)
			new VariableGridFrame(this, &m_project);
		break;
	case wxID_SAVEAS:
		SaveAs();
		break;
	case wxID_SAVE:
		Save();
		break;
	case ID_SAVE_HOURLY:
		m_project.SetSaveHourlyData( true );
		Save();
		m_project.SetSaveHourlyData( false );
		break;
	case wxID_CLOSE:
		if ( CloseProject() )
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
	if ( file.IsEmpty() || !wxFileExists( file ) )
		return false;

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

	for( size_t i=0;i<m_caseNotebook->GetPageCount();i++ )
		if ( CaseWindow *cw = dynamic_cast<CaseWindow*>(m_caseNotebook->GetPage(i)) )
			cw->SaveCurrentViewProperties();

	bool ok = m_project.WriteArchive( file );
	if ( ok ) m_project.SetModified( false );

	UpdateFrameTitle();
	return ok;
}

CaseWindow *MainWindow::GetCaseWindow( Case *c )
{
	if ( !c ) return 0;

	for( size_t i=0;i<m_caseNotebook->GetPageCount();i++ )
		if ( CaseWindow *cw = dynamic_cast<CaseWindow*>(m_caseNotebook->GetPage( i ) ) )
			if ( cw->GetCase() == c )
				return cw;

	return 0;
}

bool MainWindow::SwitchToCaseWindow( const wxString &case_name )
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

		// update all the page notes so they get
		// hidden/shown appropriately
		for( size_t i=0;i<m_caseNotebook->GetPageCount();i++ )
			if ( CaseWindow *cw = dynamic_cast<CaseWindow*>( m_caseNotebook->GetPage(i) ) )
				cw->UpdatePageNote();

		return true;
	}
	else return false;
}

void MainWindow::OnCaseTabChange( wxCommandEvent &evt )
{
	int sel = evt.GetSelection();
	SwitchToCaseWindow( m_caseTabList->GetLabel(sel) );
	//wxMessageBox( wxString::Format("Case tab changed: %d", evt.GetSelection() ) );
}

void MainWindow::OnCaseTabButton( wxCommandEvent &evt )
{
	wxMetroPopupMenu menu;
	menu.Append( ID_CASE_SIMULATE, "Simulate\tF5" );
	menu.Append( ID_CASE_REPORT, "Create report\tF6" );
	menu.Append( ID_CASE_CLEAR_RESULTS, "Clear all results" );
	menu.AppendSeparator();	
	menu.Append( ID_CASE_RENAME, "Rename\tF2" );
	menu.Append( ID_CASE_DUPLICATE, "Duplicate" );
	menu.Append( ID_CASE_DELETE, "Delete" );
	menu.AppendSeparator();
	menu.Append( ID_CASE_MOVE_LEFT, "Move left" );
	menu.Append( ID_CASE_MOVE_RIGHT, "Move right" );
	menu.AppendSeparator();
	menu.Append( ID_CASE_CONFIG, "Select technology and market..." );
	menu.Append( ID_CASE_RESET_DEFAULTS, "Reset inputs to default values" );
#ifdef __WXMSW__
	menu.AppendSeparator();
	menu.Append( ID_CASE_EXCELEXCH, "Setup Excel exchange...");
#endif
	//menu.AppendSeparator();
	//menu.Append( ID_CASE_IMPORT, "Import" );

	menu.Popup( this, m_caseTabList->GetPopupMenuPosition( m_caseTabList->GetSelection() ) );
}

CaseWindow *MainWindow::GetCurrentCaseWindow()
{
	return GetCaseWindow( GetCurrentCase() );
}

Case *MainWindow::GetCurrentCase()
{
	size_t tab_sel = m_caseTabList->GetSelection();
	wxString case_name = m_caseTabList->GetLabel( tab_sel );
	return m_project.GetCase( case_name );
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
			wxString tech, fin;
			c->GetConfiguration( &tech, &fin );
			wxString t2(tech), f2(fin);
			if( ShowConfigurationDialog( this, &t2, &f2, NULL ) 
				&& (t2 != tech || f2 != fin) )
				c->SetConfiguration( t2, f2 ); // this will cause case window to update accordingly
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
	case ID_CASE_RESET_DEFAULTS:
		{
			// load default values from config from disk
			c->LoadDefaults();
			
			// update ui
			c->SendEvent( CaseEvent( CaseEvent::VARS_CHANGED, c->Values().ListAll() ) );
		}
		break;
	case ID_CASE_EXCELEXCH:
		ExcelExchange::ShowExcelExchangeDialog( c->ExcelExch(), cw );
		break;
	case ID_CASE_REPORT:
		cw->GenerateReport();
		break;
	case ID_CASE_SIMULATE:
		cw->RunBaseCase();
		break;
	case ID_CASE_CLEAR_RESULTS:
		c->BaseCase().Clear();
		cw->UpdateResults();		
		break;
	};

	//wxMessageBox( wxString::Format("case id: %d, command %d", sel, evt.GetId() ) );
}

void MainWindow::OnClose( wxCloseEvent &evt )
{
	Raise();
	if ( !CloseProject() )
	{
		evt.Veto();
		return;
	}

	if ( !ScriptWindow::CloseAll() )
	{
		evt.Veto();
		return;
	}
	
	// save window position to settings
	wxRect rr;
	GetPosition( &rr.x,&rr.y );
	GetClientSize( &rr.width, &rr.height );	
	SamApp::Settings().Write( "window_x", rr.x);
	SamApp::Settings().Write( "window_y", rr.y);
	SamApp::Settings().Write( "window_width", rr.width);
	SamApp::Settings().Write( "window_height", rr.height);
	SamApp::Settings().Write( "window_maximized", IsMaximized() );


	// destroy the window
	wxGetApp().ScheduleForDestruction( this );
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
	wxString m_message;
public:

	SplashScreen()
		: wxDialog( 0, wxID_ANY, wxEmptyString, wxDefaultPosition, wxSize(515,385), wxBORDER_NONE ),
		m_message( "Starting up... please wait" )
	{
		m_nrelLogo = wxBITMAP_PNG_FROM_DATA( nrel_small );
	}

	void SetMessage( const wxString &msg )
	{
		m_message = msg;
		Refresh();
		Update();
		wxGetApp().Yield( true );
	}

	void OnPaint( wxPaintEvent & )
	{
		wxPaintDC dc(this);

		int width, height;
		GetClientSize( &width, &height );

		
		// dc.SetBackground( wxBrush( wxMetroTheme::Colour( wxMT_ACCENT ) ) ); // metro blue
		 dc.SetBackground( wxBrush( wxColour(219, 192, 4) ) ); // bright yellow/orange
		// dc.SetBackground( wxBrush( wxColour(2, 152, 152) ) ); // bright teal <- obviously not the best color ever.
		// dc.SetBackground( wxBrush( wxColour(120, 67, 163) ) ); // violet
		// dc.SetBackground( wxBrush( wxColour(191, 38, 96) ) ); // reddish pink
		// dc.SetBackground( wxBrush( wxColour(15,79,34) ) ); // dark forest green
		//dc.SetBackground(wxBrush(wxColour(241, 47, 144))); // hot pink <- then how about this one?
		dc.Clear();

		dc.SetBrush( *wxWHITE_BRUSH );
		dc.SetPen( *wxWHITE_PEN );
		dc.DrawRectangle( 0, height-50, width, 50 );

		dc.SetTextForeground( *wxWHITE );
		dc.SetFont( wxMetroTheme::Font( wxMT_LIGHT, 30 ) );
		dc.DrawText( "System Advisor Model", 35, 65 );

		dc.SetFont( wxMetroTheme::Font( wxMT_LIGHT, 18 ) );
		dc.DrawText( "Version " + SamApp::VersionStr(), 35, 135 );
		dc.DrawText( m_message, 35, 275 );

		dc.SetTextForeground( wxMetroTheme::Colour( wxMT_TEXT ) );
		dc.SetFont( wxMetroTheme::Font( wxMT_LIGHT, 10 ) );
		dc.DrawText( wxString::Format("Copyright %d National Renewable Energy Laboratory", SamApp::VersionMajor()),
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

ScriptDatabase::ScriptDatabase()
{
}

ScriptDatabase::~ScriptDatabase()
{
	ClearAll();
}

bool ScriptDatabase::LoadFile( const wxString &file )
{
	wxFile fp( file );
	if ( fp.IsOpened() )
	{
		//wxLogStatus("uicb: processing callback script file: " + file);
		wxString buf;
		fp.ReadAll( &buf );
		return LoadScript( buf );
	}

	return true;
}

bool ScriptDatabase::LoadScript( const wxString &source )
{
	lk::input_string data( source );
	lk::parser parse( data );
	lk::node_t *tree = parse.script();

	if ( parse.error_count() != 0
		|| parse.token() != lk::lexer::END)
	{
		wxLogStatus("fail: callback script load: parsing did not reach end of input ");				
		for (int x=0; x < parse.error_count(); x++)
			wxLogStatus( parse.error(x));

		return false;
	}
	else if ( tree != 0 )
	{							
		cb_data *cbf = new cb_data;
		cbf->source = source;
		cbf->tree = tree;
		m_cblist.push_back(cbf);

		lk::eval e( tree, &m_cbenv );

		if ( !e.run() )
		{
			wxLogStatus("uicb script eval fail" );
			for (size_t i=0;i<e.error_count();i++)
				wxLogStatus( e.get_error(i) );

			return false;
		}
	}

	return true;
}

void ScriptDatabase::ClearAll()
{
	for( size_t i=0;i<m_cblist.size();i++) 
	{
		delete m_cblist[i]->tree;
		delete m_cblist[i];
	}
	
	m_cblist.clear();
	m_cbenv.clear_objs();
	m_cbenv.clear_vars();
}

lk::node_t *ScriptDatabase::Lookup( const wxString &method_name, const wxString &obj_name )
{	
	lk::vardata_t *cbvar = m_cbenv.lookup( method_name, true);

	if (!cbvar || cbvar->type() != lk::vardata_t::HASH )
	{
		//wxLogStatus("ScriptDatabase::Invoke: could not find " + method_name + " variable or not a hash");
		return 0;
	}

	lk::vardata_t *cbref = cbvar->lookup( obj_name );
	if ( cbref == 0 
		|| cbref->type() != lk::vardata_t::FUNCTION
		|| cbref->deref().func() == 0 )
	{
		// wxLogStatus("ScriptDatabase::Invoke: could not find function entry for '%s'", (const char*)obj_name.c_str() );
		return 0;
	}
	
	lk::expr_t *p_define = cbref->deref().func();
	if ( p_define->oper != lk::expr_t::DEFINE )
	{
		wxLogStatus("ScriptDatabase::Invoke: improper function structure, must be a 'define' for %s, instead: %s", (const char*)obj_name.c_str(), cbref->func()->operstr() );
		return 0;
	}
	
	if ( p_define->right == 0 )
	{
		wxLogStatus("ScriptDatabase::Invoke: function block nonexistent for '%s'\n", (const char*)obj_name.c_str());
		return 0;
	}

	return p_define->right;
}


InputPageData::InputPageData() {
	// nothing to do
}

void InputPageData::Clear()
{ 
	m_form.DeleteAll();
	m_vars.clear();
	m_eqns.Clear();
	m_cbs.ClearAll();
}

void InputPageData::Write( wxOutputStream &os )
{
	wxDataOutputStream out( os );
	out.Write8( 0x48 );
	out.Write8( 1 );

	m_form.Write( os );
	m_vars.Write( os );
	out.WriteString( m_eqnScript );
	out.WriteString( m_cbScript );

	out.Write8( 0x48 );
}

bool InputPageData::Read( wxInputStream &is )
{
	wxDataInputStream in(is);
	wxUint8 code = in.Read8();
	wxUint8 ver = in.Read8();

	bool ok = true; 
	ok = ok && m_form.Read( is );
	ok = ok && m_vars.Read( is );
	m_eqnScript = in.ReadString();
	m_cbScript = in.ReadString();

	return in.Read8() == code && ok;
}

bool InputPageData::BuildDatabases()
{
	m_eqns.Clear();
	m_cbs.ClearAll();

	if ( !m_eqns.LoadScript( m_eqnScript ) )
		return false;

	if ( !m_cbs.LoadScript( m_cbScript ) )
		return false;

	return true;
}

InputPageDatabase::InputPageDatabase()
{
}

InputPageDatabase::~InputPageDatabase()
{
	Clear();
}

void InputPageDatabase::Clear()
{
	for ( InputPageDataHash::iterator it = m_hash.begin();
		it != m_hash.end();
		++it )
		delete (*it).second;

	m_hash.clear();
}

bool InputPageDatabase::LoadFile( const wxString &file )
{
	wxFileName ff(file);
	wxString name( ff.GetName() );
	
	InputPageData *pd = new InputPageData;
	
	bool ok = true;
	wxFFileInputStream is( file );
	if ( !is.IsOk() || !pd->Read( is ) )
		ok = false;
	
	pd->Form().SetName( name );

	if ( ok ) Add( name, pd );
	else delete pd;

	if ( !pd->BuildDatabases() )
	{
		wxLogStatus( "failed to build equation and script databases for: " + name );
		ok = false;
	}

	return ok;
}

void InputPageDatabase::Add( const wxString &name, InputPageData *ui )
{
	InputPageDataHash::iterator it = m_hash.find( name );
	if ( it != m_hash.end() )
	{
		delete it->second;
		it->second = ui;
	}
	else
		m_hash[ name ] = ui;
}

InputPageData *InputPageDatabase::Lookup( const wxString &name )
{
	InputPageDataHash::iterator it = m_hash.find( name );
	return ( it != m_hash.end() ) ? it->second : NULL;
}



ConfigInfo::ConfigInfo()
{
}

ConfigInfo::~ConfigInfo()
{
	for( size_t i=0;i<InputPageGroups.size();i++) delete InputPageGroups[i];
}


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

	for( size_t i=0;i<fin.size();i++ )
	{
		if ( !Find( tech, fin[i] ) )
		{
			ConfigInfo *ci = new ConfigInfo;
			ci->Technology = tech;
			ci->Financing = fin[i];
			m_configList.push_back( ci );
		}
	}
}

void ConfigDatabase::SetConfig( const wxString &t, const wxString &f )
{
	m_curConfig = Find( t, f );
}

void ConfigDatabase::SetModules( const wxArrayString &list )
{
	if ( m_curConfig != 0 ) m_curConfig->Simulations = list;
}

void ConfigDatabase::AddInputPageGroup( const std::vector< std::vector<PageInfo> > &pages, const wxString &sidebar, 
	const wxString &hlpcxt, const wxString &exclvar )
{
	if ( m_curConfig == 0 ) return;

	InputPageGroup *ip = new InputPageGroup;
	ip->Pages = pages;
	ip->SideBarLabel = sidebar;
	ip->HelpContext = hlpcxt;
	ip->OrganizeAsExclusivePages = !exclvar.IsEmpty();
	ip->ExclusivePageVar = exclvar;

	m_curConfig->InputPageGroups.push_back( ip );
}

void ConfigDatabase::RebuildCaches()
{
	for( std::vector<ConfigInfo*>::iterator it0 = m_configList.begin();
		it0 != m_configList.end(); ++it0 )
	{
		ConfigInfo *ci = *it0;

		ci->Variables.clear();
		ci->Equations.Clear();
		ci->AutoVariables.clear();
		ci->InputPages.clear();
		
		for( std::vector<InputPageGroup*>::iterator it1 = ci->InputPageGroups.begin();
			it1 != ci->InputPageGroups.end(); ++it1 )
		{
			InputPageGroup *igrp = *it1;
			for( size_t k=0;k<igrp->Pages.size();k++ )
			{
				size_t nstack = igrp->Pages[k].size();
				for( size_t l=0;l<nstack;l++ )
				{
					PageInfo &pi = igrp->Pages[k][l];
					if ( InputPageData *ipd = SamApp::InputPages().Lookup( pi.Name ) )
					{
						ci->InputPages[ pi.Name ] = ipd;
						
						ci->Equations.AddDatabase( &ipd->Equations() );
						ci->Equations.Add( ipd->Equations().GetEquations() );
					
						VarDatabase &vars = ipd->Variables();
						for( VarDatabase::iterator it = vars.begin();
							it != vars.end();
							++it )
						{
							if ( !ci->Variables.Add( it->first, it->second ) )
							{
								wxMessageBox("Internal error in configuration.\n\n" + ci->Technology + ", " + ci->Financing + "   [ " + pi.Name + " ]\n\n"
									"An error occured when attempting to instantiate variable: '" + it->first + "'\n"
									"Duplicate variables within a configuration are not allowed.", "sam-engine", wxICON_ERROR|wxOK );
							}

						//	if ( EqnData *ed = ipd->Equations()..GetEquationData( it->first ))
//								ci->Equations.Add( ed );
						}

						if ( pi.Collapsible && !pi.CollapsiblePageVar.IsEmpty() )
						{
							VarInfo *vv = ci->AutoVariables.Lookup( pi.CollapsiblePageVar );
							if( vv == 0 )
							{
								vv = ci->AutoVariables.Create( pi.CollapsiblePageVar, VV_NUMBER,
									"Current selection for " + pi.Caption );

								vv->Flags |= VF_COLLAPSIBLE_PANE;
								vv->DefaultValue.Set( pi.CollapsedByDefault ? 0 : 1 );
							}
							else
								wxLogStatus( "AutoVariable error: collapsible page variable already exists in configuration: " + pi.CollapsiblePageVar );

							ci->Variables.Add( pi.CollapsiblePageVar, vv );
						}
					}
					else
						wxLogStatus("could not find data for referenced input page: " + pi.Name );
				}
			}

			if ( igrp->OrganizeAsExclusivePages && !igrp->ExclusivePageVar.IsEmpty() )
			{
				VarInfo *vv = ci->AutoVariables.Lookup( igrp->ExclusivePageVar );
				if ( vv == 0 )
				{
					vv = ci->AutoVariables.Create( igrp->ExclusivePageVar, VV_NUMBER, 
						"Current selection for " + igrp->SideBarLabel );

					vv->Flags |= VF_EXCLUSIVE_PAGES;
					vv->DefaultValue.Set( 0 );
				}
				else
					wxLogStatus( "AutoVariable error: exclusive page variable already exists in configuration: " + igrp->ExclusivePageVar );

				ci->Variables.Add( igrp->ExclusivePageVar, vv );
			}
		}
	}
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
		
ConfigInfo *ConfigDatabase::Find( const wxString &t, const wxString &f )
{
	for( size_t i=0;i<m_configList.size();i++ )
		if ( m_configList[i]->Technology == t
			&& m_configList[i]->Financing == f )
			return m_configList[i];

	return 0;
}

ConfigOptions &ConfigDatabase::Options( const wxString &name )
{
static unordered_map<wxString,ConfigOptions, wxStringHash, wxStringEqual> m_opts;
	return m_opts[name];
}

bool SamApp::OnInit()
{
	m_locale.Init(); // necessary for comma-formatting

	/*
	SamLogWindow::Setup();
	extern void loss_diagram_test();
	loss_diagram_test();
	return true;
	*/
	
	SetAppName( "SAM" );
	SetVendorName( "NREL" );

	// register all the object types that can
	// be read or written to streams.
	ObjectTypes::Register( new StringHash );
	ObjectTypes::Register( new Case );

	// register all input page UI objects 
	wxUIObjectTypeProvider::RegisterBuiltinTypes();
	RegisterUIObjectsForSAM();

	// register standard sam report objects for report generation
extern void RegisterReportObjectTypes();
	RegisterReportObjectTypes();


	wxInitAllImageHandlers();
	wxSimpleCurl::Init();
	
	for( int i=0;i<argc;i++ )
		g_appArgs.Add( argv[i] );
	
	if ( g_appArgs.Count() < 1 || !wxDirExists( wxPathOnly(g_appArgs[0]) ) )
	{

		wxMessageBox("Startup error - cannot determine application runtime folder from startup argument.\n\n"
			"Try running " + g_appArgs[0] + " by specifying the full path to the executable.");
		return false;
	}
	
	wxString proxy_file = wxPathOnly(g_appArgs[0]) + "/proxy.txt";
	if (wxFileExists(proxy_file))
	{
		if (FILE *fp = fopen(proxy_file.c_str(), "r"))
		{
			char buf[512];
			fgets(buf, 511, fp);
			fclose(fp);
			wxSimpleCurl::SetupProxy(wxString::FromAscii(buf));
		}
	}


#ifdef _DEBUG
	SamLogWindow::Setup();
#endif

	
	g_config = new wxConfig( "SAMnt", "NREL" );
	
	
	wxLogStatus( "startup version %d.%d.%d with SSC version %d, %s", 
		g_verMajor, g_verMinor, g_verMicro,ssc_version(), ssc_build_info() );
	
	SplashScreen splash;
	splash.CenterOnScreen();
	splash.Show();
	splash.Update();
	Yield(true);


	
#if defined(__BETAWILLEXPIRE__)&&defined(__BETARELEASE__)
	wxDateTime now = wxDateTime::Now();
	wxDateTime BetaEndDate( __BETAEXPIRE_DAY__, __BETAEXPIRE_MONTH__, __BETAEXPIRE_YEAR__ );
	if (now.IsEarlierThan(BetaEndDate))
	{
		int ndays = BetaEndDate.Subtract(now).GetDays()+1;
		wxString datestr = BetaEndDate.Format("%A, %d %B %Y");

		wxString expire_text = wxString::Format( "This beta software will expire %d days from now, on ",ndays) +  datestr + ".";
		if ( ndays == 1 )
			expire_text = "This beta software will expire tomorrow!";

		wxMessageBox(wxString::Format("Thank you for using SAM Beta Version %d.%d.%d.\n\n",
			SamApp::VersionMajor(), SamApp::VersionMinor(), SamApp::VersionMicro()) + expire_text + "\n\n" + wxString(beta_disclaimer));
	}
	else if (!wxFileExists( SamApp::GetRuntimePath() + "/expireoverride.txt" ))
	{
		if (wxYES==wxMessageBox("The SAM beta software has expired.  Please download the latest release from the SAM website.\n\nWould you like to visit the SAM website now?", "Notice", wxYES_NO))
		{
			wxString url = SamApp::WebApi("samwebsite");
			if (url.IsEmpty()) url = "http://sam.nrel.gov";
			wxLaunchDefaultBrowser( url );
		}
		return false;
	}
#endif
	
	wxString devoverride;
	SamApp::Settings().Read( "developer-registration", &devoverride );

	if ( devoverride != "09332s" )
	{
		splash.SetMessage( "Verifying registration..." );

		wxString email = SamRegistration::GetEmail();
		wxString key = SamRegistration::GetKey();
	
		if ( email.IsEmpty() || key.IsEmpty() )
		{
			splash.Hide();
			SamRegistration::ShowDialog();
		}
		
		if ( !SamRegistration::IncrementUsage() )
		{
			splash.Hide();

			if ( !SamRegistration::CanStart() )
			{
				SamRegistration::ShowDialog( "You have reached the limit for the number of times you can run SAM without verifying your registration key.\n\n"
					"Please check your email address, verification code, and internet connection." );

				if ( !SamRegistration::CanStart() )
					return false;
			}
			else 
			{
				wxString text = ("SAM was not able to verify your registration key with NREL servers.\n"
					"This can be caused by your internet connection being unavailable, or an invalid proxy. See help for more information.\n\n" );
				int nstarts = SamRegistration::AllowedStartsRemaining();
				if ( nstarts == 1 )	text += "This is the last time you may run SAM without your registration being verified.\n\n";
				else text += wxString::Format( "You may run SAM %d more times without your registration being verified.\n\n", nstarts );

				if ( !SamRegistration::ShowDialog( text, wxString::Format("Skip for now (%d left)", nstarts) ) )
				{
					// since app is not going to start, decrement usage count (it was incremented already for this start above)
					SamRegistration::DecrementUsage();
					return false;
				}
			}
		}
	}
	
	splash.Show();
	splash.SetMessage( "Starting up... please wait" );

	//wxMessageBox( wxString::Format("sizeof(VarValue)=%d, sizeof(wxMemoryBuffer)=%d, sizeof(matrix_t<float>)=%d, sizeof(VarTable)=%d, sizeof(wxString)=%d", 
	//	sizeof(VarValue), sizeof(wxMemoryBuffer), sizeof(matrix_t<float>), sizeof(VarTable), sizeof(wxString) ) );

	FileHistory().Load( Settings() );

	Restart(); // loads and runs startup scripts, sets up variable databases

	g_mainWindow = new MainWindow();
	SetTopWindow( g_mainWindow );
	g_mainWindow->Show();
	
	bool first_load = true;
	wxString fl_key = wxString::Format("first_load_%d", VersionMajor()*10000+VersionMinor()*100+VersionMicro() );
	Settings().Read(fl_key, &first_load, true);
		
	if ( first_load )
	{
		// register the first load
		Settings().Write(fl_key, false);

		// enable web update app 
		wxConfig cfg("SamUpdate", "NREL");
		cfg.Write("allow_web_updates", true);
				
		// after installing a new version, always show the reminders again until the user turns them off
		Settings().Write( "show_reminder", true );
	}
	else
	{
		// restore window position
		bool b_maximize = false;
		int f_x,f_y,f_width,f_height;
		Settings().Read("window_x", &f_x, -1);
		Settings().Read("window_y", &f_y, -1);
		Settings().Read("window_width", &f_width, -1);
		Settings().Read("window_height", &f_height, -1);
		Settings().Read("window_maximized", &b_maximize, false);
		
		if (b_maximize)
			g_mainWindow->Maximize();
		else
		{
			if ( wxDisplay::GetFromPoint( wxPoint(f_x,f_y) ) != wxNOT_FOUND )
			{
				if (f_width > 100 && f_height > 100)
					g_mainWindow->SetClientSize(f_width, f_height);

				if (f_x > 0 && f_y > 0)
					g_mainWindow->SetPosition(wxPoint(f_x,f_y));
			}
			else // place default here...
				g_mainWindow->Maximize();
		}
	}

	return true;
}


#ifdef __BETARELEASE__

class SurveyDialog : public wxDialog
{
public:
	SurveyDialog( wxWindow *parent, const wxString &title )
		: wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE )
	{
		wxStaticText *text = new wxStaticText( this, wxID_ANY, 
			"Thank you for using this beta release of SAM.  "
			"The SAM development team would greatly appreciate any feedback you might provide.  "
			"Please click on the link below to help us improve future versions of SAM.  ");
		text->Wrap( 500 );
		
	/*	wxHyperlinkCtrl *hyp1 = new wxHyperlinkCtrl( this, wxID_ANY, 
			"Click to fill out the SAM Beta online survey... (recommended)",  
			"https://www.surveymonkey.com/s/sam-beta-survey" );*/

		wxHyperlinkCtrl *hyp2 = new wxHyperlinkCtrl( this, wxID_ANY,
			"Email feedback directly to the SAM team...",
			"mailto:sam.support@nrel.gov?subject=SAM Beta Feedback" );

		wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
		sizer->Add( text, 0, wxALIGN_CENTER|wxALL, 10 );
		//sizer->Add( hyp1, 0, wxALIGN_CENTER|wxALL, 10 );
		sizer->Add( hyp2, 0, wxALIGN_CENTER|wxALL, 10 );
		sizer->Add( CreateButtonSizer( wxOK ), 0, wxEXPAND|wxALL, 10 );

		SetSizer( sizer );
		Fit();
	}
};

#endif

int SamApp::OnExit()
{
	FileHistory().Save( Settings() );

	if ( g_config != 0 ) delete g_config;

	
#ifdef __BETARELEASE__
	SurveyDialog dlg( 0, "Feedback" );
	dlg.CenterOnScreen();
	dlg.ShowModal();
#endif
	

	wxSimpleCurl::Shutdown();
	
	wxLog::SetActiveTarget( 0 );
	return 0;
}

bool SamApp::OnExceptionInMainLoop()
{
	wxMessageBox("SAM unhandled exception occurred.");
	return false;
}

void SamApp::Restart()
{
	// reload all forms, variables, callbacks, equations
	SamApp::InputPages().Clear();
	
	wxDir dir( SamApp::GetRuntimePath() + "/ui" );
	if ( dir.IsOpened() )
	{
		wxString file;
		bool has_more = dir.GetFirst( &file, "*.ui", wxDIR_FILES  );
		while( has_more )
		{
			wxLogStatus( "loading .ui: " + wxFileName(file).GetName() );
			if (!SamApp::InputPages().LoadFile( SamApp::GetRuntimePath() + "/ui/" + file ))
				wxLogStatus( " --> error loading .ui for " + wxFileName(file).GetName() );
			
			has_more = dir.GetNext( &file );
		}
	}
	dir.Close();
	
	// reload configuration map
	SamApp::Config().Clear();
	wxString startup_script = GetRuntimePath() + "/startup.lk";
	wxLogStatus("loading startup script: " + startup_script );
	wxArrayString errors;
	if ( !LoadAndRunScriptFile( startup_script, &errors ) )
		wxLogStatus( wxJoin( errors, '\n' ) );


	wxLogStatus("rebuilding caches for each configuration's variables and equations");
	SamApp::Config().RebuildCaches();

	wxLogStatus("loading libraries...");
	Library::UnloadAll();
	if ( dir.Open( SamApp::GetRuntimePath() + "../libraries" ) )
	{
		wxString file;
		bool has_more = dir.GetFirst( &file, "*.csv", wxDIR_FILES  );
		while( has_more )
		{
			if ( Library *ll = Library::Load( SamApp::GetRuntimePath() + "../libraries/" + file ) )
			{
				wxLogStatus( "loaded '" + ll->GetName() + "' from " + file );
				wxLogStatus( "\t %d entries, %d fields", (int)ll->NumEntries(), (int)ll->GetFields().size() );
			}
			else
				wxLogStatus( "error loading library " + file );
			has_more = dir.GetNext( &file );
		}
	}


	g_globalCallbacks.ClearAll();
	if ( !g_globalCallbacks.LoadFile( SamApp::GetRuntimePath() + "/metrics.lk" )) 
		wxLogStatus( "error loading metrics.lk" );

	if ( !g_globalCallbacks.LoadFile( SamApp::GetRuntimePath() + "/cashflow.lk" ))
		wxLogStatus( "error loading cashflow.lk" );

	if ( !g_globalCallbacks.LoadFile( SamApp::GetRuntimePath() + "/autographs.lk" ))
		wxLogStatus( "error loading autographs.lk" );

	if ( !g_globalCallbacks.LoadFile( SamApp::GetRuntimePath() + "/lossdiag.lk" ))
		wxLogStatus( "error loading lossdiag.lk" );

	wxString solar_resource_db = SamApp::GetUserLocalDataDir() + "/SolarResourceData.csv";
	if ( !wxFileExists( solar_resource_db ) ) ScanSolarResourceData( solar_resource_db );
	Library::Load( solar_resource_db );

	wxString wind_resource_db  = SamApp::GetUserLocalDataDir() + "/WindResourceData.csv";
	if ( !wxFileExists( wind_resource_db ) ) ScanWindResourceData( wind_resource_db );
	Library::Load( wind_resource_db );
}

wxString SamApp::WebApi( const wxString &name )
{
static StringHash s_apis;
	if ( s_apis.size() == 0 )
		if ( ! s_apis.ReadKeyValueFile( GetRuntimePath() + "/webapis.conf" ) || s_apis.size() == 0 )
			wxMessageBox( "error loading webapis.conf, or no apis defined");

	if ( name.IsEmpty() )
	{
		wxArrayString names;
		for( StringHash::iterator it = s_apis.begin(); it != s_apis.end(); ++it )
			names.Add( it->first );
		return wxJoin(names, ',');
	}
	if ( s_apis.find( name ) != s_apis.end() ) return s_apis[ name ];
	else return wxEmptyString;
}

wxString SamApp::GetAppPath()
{
	return wxPathOnly( g_appArgs[0] );
}

wxString SamApp::GetRuntimePath()
{
	return GetAppPath() + "/../runtime/";
}

wxString SamApp::GetUserLocalDataDir()
{	
	wxString path = wxStandardPaths::Get().GetUserLocalDataDir() + "/sam-" + SamApp::VersionStr();
	path.Replace("\\","/");
	
	if (!wxDirExists( path ))
		wxFileName::Mkdir( path, 511, wxPATH_MKDIR_FULL );

	return path;
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

ProjectFile &SamApp::Project()
{
	return g_mainWindow->Project();
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

class HelpWin;
static HelpWin *gs_helpWin = 0;

enum { ID_BACK = wxID_HIGHEST+439, ID_BROWSER, ID_HOME, ID_EMAIL_SUPPORT, ID_WEBSITE, ID_FORUM, ID_RELEASE_NOTES, ID_SCRIPT_REFERENCE };


class HelpWin : public wxFrame
{
	wxWebView *m_webView;
	wxString m_aboutHtml;
public:
	HelpWin( wxWindow *parent )
		: wxFrame( parent, wxID_ANY, "System Advisor Model Help", wxDefaultPosition, wxSize(800,600) )
	{
		CreateAboutHtml();

#ifdef __WXMSW__
		SetIcon( wxICON( appicon ) );
#endif
		SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );
		m_webView = wxWebView::New( this, ID_BROWSER, ::wxWebViewDefaultURLStr, wxDefaultPosition, wxDefaultSize, 
			::wxWebViewBackendDefault, wxBORDER_NONE );
		m_webView->SetPage( m_aboutHtml, "About SAM" );

		wxBoxSizer *tools = new wxBoxSizer( wxHORIZONTAL );
		tools->Add( new wxMetroButton( this, ID_BACK, "Back" ), 0, wxALL|wxEXPAND, 0 );
		tools->Add( new wxMetroButton( this, ID_HOME, "Home" ), 0, wxALL|wxEXPAND, 0 );
		tools->Add( new wxMetroButton( this, ID_WEBSITE, "Web site" ), 0, wxALL|wxEXPAND, 0 );
		tools->Add( new wxMetroButton( this, ID_FORUM, "Forum" ), 0, wxALL|wxEXPAND, 0 );
		tools->Add( new wxMetroButton( this, ID_EMAIL_SUPPORT, "Email support" ), 0, wxALL|wxEXPAND, 0 );
		tools->Add( new wxMetroButton( this, ID_RELEASE_NOTES, "Release notes" ), 0, wxALL|wxEXPAND, 0 );
		tools->Add( new wxMetroButton( this, ID_SCRIPT_REFERENCE, "Scripting reference" ), 0, wxALL|wxEXPAND, 0 );
		tools->AddStretchSpacer();
		tools->Add( new wxMetroButton( this, wxID_ABOUT, "About" ), 0, wxALL|wxEXPAND, 0 ); 
		tools->Add( new wxMetroButton( this, wxID_CLOSE, "Close" ), 0, wxALL|wxEXPAND, 0 ); 

		wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
		sizer->Add( tools, 0, wxALL|wxEXPAND, 0 );
		sizer->Add( m_webView, 1, wxALL|wxEXPAND, 0 );
		SetSizer( sizer );
	}

	void CreateAboutHtml()
	{
		static char *s_samDisclaimerHtml = "DISCLAIMER<br><br>"
				"The System Advisor Model (\"Model\") is provided by the National Renewable Energy Laboratory (\"NREL\"), which is operated by the Alliance for Sustainable Energy, LLC (\"Alliance\") for the U.S. Department Of Energy (\"DOE\") and may be used for any purpose whatsoever.<br><br>"
				"The names DOE/NREL/ALLIANCE shall not be used in any representation, advertising, publicity or other manner whatsoever to endorse or promote any entity that adopts or uses the Model.  DOE/NREL/ALLIANCE shall not provide any support, consulting, training or assistance of any kind with regard to the use of the Model or any updates, revisions or new versions of the Model.<br><br>"
				"YOU AGREE TO INDEMNIFY DOE/NREL/ALLIANCE, AND ITS AFFILIATES, OFFICERS, AGENTS, AND EMPLOYEES AGAINST ANY CLAIM OR DEMAND, INCLUDING REASONABLE ATTORNEYS' FEES, RELATED TO YOUR USE, RELIANCE, OR ADOPTION OF THE MODEL FOR ANY PURPOSE WHATSOEVER.  THE MODEL IS PROVIDED BY DOE/NREL/ALLIANCE \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE EXPRESSLY DISCLAIMED.  IN NO EVENT SHALL DOE/NREL/ALLIANCE BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER, INCLUDING BUT NOT LIMITED TO CLAIMS ASSOCIATED WITH THE LOSS OF DATA OR PROFITS, WHICH MAY RESULT FROM ANY ACTION IN CONTRACT, NEGLIGENCE OR OTHER TORTIOUS CLAIM THAT ARISES OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THE MODEL.<br><br>";
		
		int nbit = (sizeof(void*) == 8) ? 64 : 32;
		m_aboutHtml = "<html><body bgcolor=#ffffff>"
			"<font color=#a9a9a9 face=\"Segoe UI Light\" size=10>System Advisor Model</font><br>"
				"<font color=#a9a9a9 face=\"Segoe UI Light\" size=5>Version " + SamApp::VersionStr() + wxString::Format(", %d bit</font><br><br>", nbit )
				+ "<font color=#999999 face=\"Segoe UI Light\" size=3>" 
				+ wxString::Format("SSC Version %d:  %s", ssc_version(), ssc_build_info() ) + "<br>"
				+ wxString::Format("wxWidgets %d.%d.%d", wxMAJOR_VERSION, wxMINOR_VERSION, wxRELEASE_NUMBER )  + " on " + wxGetOsDescription() + "<br><br><br>"				
				+ wxString(s_samDisclaimerHtml) + "</font>"
				"</body></html>";
	}
	void LoadPage( wxString url )
	{
		if ( url == ":about" )
		{
			m_webView->SetPage( m_aboutHtml, "About SAM" );
			return;
		}
		else if ( url == ":release_notes" )
		{
			url = SamApp::WebApi("release_notes");
		}
		else if ( url == ":email_support" )
		{
			wxLaunchDefaultBrowser( SamApp::WebApi("support_email") );
			return;
		}
		else if ( url == ":script_ref" )
		{
			wxFileName file( SamApp::GetRuntimePath() + "/help/lk_guide.pdf" );
			file.Normalize();
			wxLaunchDefaultBrowser( file.GetFullPath() );
			return;
		}
		else if ( url == ":forum" )
			url = SamApp::WebApi( "forum" );
		else if ( url == ":website" )
			url = SamApp::WebApi( "website" );
		
		m_webView->LoadURL( url );
	}

	void OnClose( wxCloseEvent &evt )
	{
		Hide();
		evt.Veto();
	}

	void OnCommand( wxCommandEvent &evt )
	{
		switch( evt.GetId() )
		{
		case ID_BACK:
			if ( m_webView->CanGoBack() ) m_webView->GoBack();
			break;
		case ID_WEBSITE:
			LoadPage( ":website" );
			break;
		case ID_FORUM:
			LoadPage( ":forum" );
			break;
		case ID_EMAIL_SUPPORT:
			LoadPage( ":email_support" );
			break;
		case ID_RELEASE_NOTES:
			LoadPage( ":release_notes" );
			break;
		case ID_SCRIPT_REFERENCE:
			LoadPage( ":script_ref" );
			break;
		case ID_HOME:
		{
			wxFileName fn( SamApp::GetRuntimePath() + "/help/html/index.html" );
			fn.MakeAbsolute();
			LoadPage( "file:///" + fn.GetFullPath() );
		}
			break;
		case wxID_ABOUT:
			LoadPage( ":about" );
			break;
		case wxID_CLOSE:
			Close();
			break;
		}
	}

	void OnNewWindow( wxWebViewEvent &evt )
	{
		wxLaunchDefaultBrowser( evt.GetURL() );
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( HelpWin, wxFrame )
	EVT_BUTTON( ID_BACK, HelpWin::OnCommand )
	EVT_BUTTON( ID_HOME, HelpWin::OnCommand )
	EVT_BUTTON( ID_WEBSITE, HelpWin::OnCommand )
	EVT_BUTTON( ID_FORUM, HelpWin::OnCommand )
	EVT_BUTTON( ID_RELEASE_NOTES, HelpWin::OnCommand )
	EVT_BUTTON( ID_SCRIPT_REFERENCE, HelpWin::OnCommand )
	EVT_BUTTON( ID_EMAIL_SUPPORT, HelpWin::OnCommand )
	EVT_BUTTON( wxID_CLOSE, HelpWin::OnCommand )
	EVT_BUTTON( wxID_ABOUT, HelpWin::OnCommand )
	EVT_WEBVIEW_NEWWINDOW( ID_BROWSER, HelpWin::OnNewWindow )
	EVT_CLOSE( HelpWin::OnClose )
END_EVENT_TABLE()


void SamApp::ShowHelp( const wxString &context )
{	
	wxString url; 
	if ( context.Left(1) == ":" )
		url = context; // for things like :about, etc
	else
	{
		wxFileName fn( SamApp::GetRuntimePath() + "/help/html/" );
		fn.MakeAbsolute();
		url = "file:///" + fn.GetFullPath( wxPATH_NATIVE ) + "index.html";
		if ( ! context.IsEmpty() )
			url += "?" + context + ".htm";		
	}
	
	wxWindow *modal_active = 0;
	wxWindow *nonmodal_tlw = 0;
	for( wxWindowList::iterator wl = wxTopLevelWindows.begin();
		wl != wxTopLevelWindows.end();
		++wl )
	{
		wxTopLevelWindow *tlw = dynamic_cast<wxTopLevelWindow*>( *wl );
		wxDialog *dia = dynamic_cast<wxDialog*>( *wl );

		if ( tlw != 0 && (dia == 0  || !dia->IsModal()) )
			nonmodal_tlw = tlw;

		if ( dia != 0 && dia->IsActive() && dia->IsModal() )
			modal_active = dia;
	}

	// try several different parent windows for the help window
	// if possible, use the SAM main window
	// otherwise, choose any top level window that is not modal
	// last resort, choose a currently modal dialog box
	wxWindow *parent = SamApp::Window();
	if ( !parent ) parent = nonmodal_tlw;
	if ( !parent ) parent = modal_active;

	if ( modal_active && gs_helpWin != 0 && gs_helpWin->IsShown() )
	{
		wxRect h_rect = gs_helpWin->GetRect();

		if (gs_helpWin->Destroy())
		{
			gs_helpWin = new HelpWin( parent );
			gs_helpWin->SetSize(h_rect);
		}
	}
	else if ( 0 == gs_helpWin )			
		gs_helpWin = new HelpWin( parent );
		
	gs_helpWin->Show( );
	gs_helpWin->LoadPage( url );
	gs_helpWin->Raise();
	gs_helpWin->SetTitle( "System Advisor Model Help {" + context + " --> " + url + "}" );
}

wxString SamApp::VersionStr() { return wxString::Format("%d.%d.%d", VersionMajor(), VersionMinor(), VersionMicro());};
int SamApp::VersionMajor() { return g_verMajor; }
int SamApp::VersionMinor() { return g_verMinor; }
int SamApp::VersionMicro() { return g_verMicro; }

wxWindow *SamApp::CurrentActiveWindow()
{
	wxWindowList &wl = ::wxTopLevelWindows;
	for( wxWindowList::iterator it = wl.begin(); it != wl.end(); ++it )
		if ( wxTopLevelWindow *tlw = dynamic_cast<wxTopLevelWindow*>( *it ) )
			if ( tlw->IsActive() )
				return tlw;

	return 0;
}

ConfigDatabase &SamApp::Config() { return g_cfgDatabase; }
InputPageDatabase &SamApp::InputPages() { return g_uiDatabase; }
ScriptDatabase &SamApp::GlobalCallbacks() { return g_globalCallbacks; }

bool SamApp::LoadAndRunScriptFile( const wxString &script_file, wxArrayString *errors )
{
	wxFile fp( script_file );
	if ( !fp.IsOpened() )
	{
		if (errors) errors->Add( "could not read: " + script_file );
		return false;
	}

	wxString buf;
	fp.ReadAll( &buf );
	lk::input_string p( buf );
	lk::parser parse( p );
	lk::node_t *tree = parse.script();
			
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
		lk::env_t env;
		env.register_funcs( lk::stdlib_basic() );
		env.register_funcs( lk::stdlib_math() );
		env.register_funcs( lk::stdlib_string() );
		env.register_funcs( lk::stdlib_wxui() );		
		env.register_funcs( invoke_general_funcs(), 0 );
		env.register_funcs( invoke_config_funcs(), 0 );

		lk::eval e( tree, &env );
		bool ok = e.run();
		if ( tree ) delete tree;
		
		if ( !ok && errors )
			for( size_t i=0;i<e.error_count();i++ )
				errors->Add( e.get_error(i) );

		return ok;
	}
}



enum { ID_TechTree = wxID_HIGHEST+98, ID_FinTree };

BEGIN_EVENT_TABLE(ConfigDialog, wxDialog)
	EVT_LISTBOX( ID_TechTree, ConfigDialog::OnTechTree)
	EVT_LISTBOX_DCLICK( ID_FinTree, ConfigDialog::OnDoubleClick )
	EVT_BUTTON( wxID_HELP, ConfigDialog::OnHelp )
	EVT_BUTTON( wxID_OK, ConfigDialog::OnOk )
	EVT_BUTTON( wxID_CANCEL, ConfigDialog::OnCancel )
	EVT_MENU( wxID_HELP, ConfigDialog::OnHelp )
	EVT_CHAR_HOOK( ConfigDialog::OnCharHook )
END_EVENT_TABLE()

ConfigDialog::ConfigDialog( wxWindow *parent, const wxSize &size )
	: wxDialog( parent, wxID_ANY, wxEmptyString, wxDefaultPosition, size, wxBORDER_NONE
#ifdef __WXOSX__
	| wxSTAY_ON_TOP  // on OSX for some reason, we need this for the dialog show up on top of the transparent pane which is the parent
#endif
	)
{
	SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );
	CenterOnParent();

	m_pTech = new wxMetroListBox( this, ID_TechTree );
	m_pFin = new wxMetroListBox( this, ID_FinTree );

	wxBoxSizer *choice_sizer = new wxBoxSizer( wxHORIZONTAL );
	choice_sizer->Add( m_pTech, 1, wxALL|wxEXPAND, 0 );
	choice_sizer->Add( m_pFin, 1, wxALL|wxEXPAND, 0 );

	wxStaticText *label = new wxStaticText( this, wxID_ANY,
		"Choose a performance model, and then choose from the available financial models." );
	wxFont font( wxMetroTheme::Font( wxMT_NORMAL, 12  ) );
	label->SetFont( font );
	label->SetForegroundColour( *wxWHITE );

	m_pChkUseDefaults = new wxCheckBox(this, wxID_ANY, "Reset new inputs to default values" );	
	m_pChkUseDefaults->SetValue(true);

	wxBoxSizer *hbox = new wxBoxSizer (wxHORIZONTAL );
	hbox->Add( new wxMetroButton(this, wxID_HELP, "Help" ), 0, wxALL|wxEXPAND, 0 );
	hbox->Add( m_pChkUseDefaults, 0, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, 4 );
	m_pChkUseDefaults->SetForegroundColour( *wxWHITE );
	hbox->AddStretchSpacer();
	//hbox->Add( CreateButtonSizer( wxOK|wxCANCEL ) );
	hbox->Add( new wxMetroButton(this, wxID_OK, "   OK   "), 0, wxALL, 0 );
	hbox->Add( new wxMetroButton(this, wxID_CANCEL, "Cancel"), 0, wxALL, 0 );
	
	wxBoxSizer *vbox = new wxBoxSizer( wxVERTICAL );
	vbox->Add( label, 0, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, 8 );
	vbox->Add( choice_sizer, 1, wxALL|wxEXPAND, 0 );
	vbox->Add( hbox, 0, wxALL|wxEXPAND, 0 );	
	SetSizer( vbox );

	
	std::vector<wxAcceleratorEntry> entries;
	entries.push_back( wxAcceleratorEntry( ::wxACCEL_NORMAL, WXK_F1, wxID_HELP ) );
	entries.push_back( wxAcceleratorEntry( ::wxACCEL_NORMAL, WXK_ESCAPE, wxID_CANCEL ) );
	SetAcceleratorTable( wxAcceleratorTable( entries.size(), &entries[0] ) );
	
	PopulateTech();

}

bool ConfigDialog::ResetToDefaults()
{
	return m_pChkUseDefaults->GetValue();
}

void ConfigDialog::SetConfiguration(const wxString &t, const wxString &f)
{
	int sel = m_tnames.Index( t );
	m_pTech->SetSelection( sel );
	m_pTech->Invalidate();

	if ( sel >= 0 )
		UpdateFinTree();
	
	m_pFin->SetSelection( m_fnames.Index(f) );
	m_pFin->Invalidate();
}

void ConfigDialog::ShowResetCheckbox(bool b)
{
	m_pChkUseDefaults->Show(b);
}

void ConfigDialog::GetConfiguration(wxString &t, wxString &f)
{
	int tsel = m_pTech->GetSelection();
	int fsel = m_pFin->GetSelection();
	t = tsel >= 0 && tsel < (int)m_tnames.size() ? m_tnames[tsel] : wxEmptyString;
	f = fsel >= 0 && fsel < (int)m_fnames.size() ? m_fnames[fsel] : wxEmptyString;
}


void ConfigDialog::OnDoubleClick(wxCommandEvent &evt)
{
	EndModal( wxID_OK );
}

void ConfigDialog::PopulateTech()
{
	m_pTech->Clear();

	m_tnames = SamApp::Config().GetTechnologies();
	
	for( size_t i=0;i<m_tnames.Count();i++)
	{
		wxString L( SamApp::Config().Options( m_tnames[i] ).LongName );
		if ( L.IsEmpty() ) L = m_tnames[i];
		m_pTech->Add( L );
	}
	
	m_pTech->Invalidate();
}

void ConfigDialog::UpdateFinTree()
{
	m_pFin->Clear();
	int tsel = m_pTech->GetSelection();
	m_fnames = SamApp::Config().GetFinancingForTech( tsel >= 0 && tsel < (int)m_tnames.size() ? m_tnames[tsel] : wxEmptyString );
	for( size_t i=0;i<m_fnames.Count();i++)
	{
		wxString L( SamApp::Config().Options( m_fnames[i] ).LongName );
		if ( L.IsEmpty() ) L = m_fnames[i];
		m_pFin->Add( L );
	}

	m_pFin->Invalidate();
}

void ConfigDialog::OnTechTree( wxCommandEvent &evt )
{
	UpdateFinTree();
}


void ConfigDialog::OnHelp(wxCommandEvent &evt)
{
	SamApp::ShowHelp( "choose_performance_financial" );
}

void ConfigDialog::OnOk( wxCommandEvent & )
{
	wxString t, f;
	GetConfiguration( t, f );
	if ( t.IsEmpty() || f.IsEmpty() )
	{
		wxMessageBox( "Please select both a technology and a financing option.", "Notice", wxOK, this );
		return;
	}
	EndModal( wxID_OK );
}

void ConfigDialog::OnCancel( wxCommandEvent & )
{
	EndModal( wxID_CANCEL );
}

int ConfigDialog::ShowModal()
{
#ifdef __WXMSW__
	ShowWithEffect( wxSHOW_EFFECT_SLIDE_TO_RIGHT );
	wxYield();
	Refresh();
	SetFocus();
#endif
	return wxDialog::ShowModal();
}

void ConfigDialog::EndModal( int ret )
{
#ifdef __WXMSW__
	HideWithEffect( wxSHOW_EFFECT_SLIDE_TO_LEFT );
	 // kludge to make hide with effect work for closing the modal dialog below
	Show();  // happens so fast it's not really visible
#endif
	wxDialog::EndModal( ret );
}

void ConfigDialog::OnCharHook( wxKeyEvent &evt )
{
	if ( evt.GetKeyCode() == WXK_ESCAPE )
		EndModal( wxID_CANCEL );
	else if ( evt.GetKeyCode() == WXK_RETURN )
	{
		wxCommandEvent _evt;
		OnOk( _evt );
	}
}

wxFrame *CreateTransparentOverlay( wxWindow *parent )
{
	wxPoint pos = parent->ClientToScreen( wxPoint(0,0) );
	wxSize size = parent->GetClientSize();

	wxFrame *trans = new wxFrame( parent, wxID_ANY, wxEmptyString,  pos, size, 
		wxBORDER_NONE | wxFRAME_FLOAT_ON_PARENT | wxFRAME_NO_TASKBAR );
	trans->SetBackgroundColour( *wxLIGHT_GREY );
	trans->SetTransparent( 230 );
	trans->Show();

	return trans;
}

bool ShowConfigurationDialog( wxWindow *parent, wxString *tech, wxString *fin, bool *reset )
{
	if ( parent == 0 ) return false;

	wxFrame *trans = CreateTransparentOverlay( parent );
	wxPoint pt( trans->GetPosition() );
	wxSize size( trans->GetClientSize() );
	
	ConfigDialog *dlg = new ConfigDialog( trans );
	dlg->SetPosition( pt );
	dlg->SetClientSize( 700, size.y );
	
	if ( reset != 0 ) dlg->ShowResetCheckbox( *reset );
	else dlg->ShowResetCheckbox( false );

	if ( !tech->IsEmpty() && !fin->IsEmpty() )
		dlg->SetConfiguration( *tech, *fin );
    	
	bool result = false;
	if ( dlg->ShowModal() == wxID_OK )
	{
		dlg->GetConfiguration( *tech, *fin );

		if ( reset != 0 ) *reset = dlg->ResetToDefaults();

		result = true;
	}

	dlg->Destroy();
	trans->Destroy();
	return result;
}


IMPLEMENT_APP( SamApp );

/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/SAM/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#include <set>
//#include <chrono>

#include <wx/wx.h>
#include <wx/frame.h>
#include <wx/stc/stc.h>
#include <fstream>

#if defined(__WXMSW__)||defined(__WXOSX__)
#include <wx/webview.h>
#else
#include <wx/html/htmlwin.h> // for linux - avoid webkitgtk dependencies
#endif

#include <wx/simplebook.h>
#include <wx/panel.h>
#include <wx/busyinfo.h>
#include <wx/dynlib.h>
#include <wx/dir.h>
#include <wx/wfstream.h>
#include <wx/datstrm.h>
#include <wx/grid.h>
#include <wx/stdpaths.h>
#include <wx/webview.h>
#include <wx/txtstrm.h>
#include <wx/buffer.h>
#include <wx/display.h>
#include <wx/utils.h>
#include <wx/platform.h>
#include <wx/txtstrm.h>

#include <wex/metro.h>
#include <wex/icons/cirplus.cpng>
#include <wex/icons/qmark.cpng>
#include <wex/utils.h>


#include "../resource/menu.cpng"
#include "../resource/notes_white.cpng"

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

#include <ssc/sscapi.h>

#include <rapidjson/writer.h>
#include <rapidjson/stringbuffer.h>
#include <rapidjson/prettywriter.h> // for stringify JSON
#include <rapidjson/filereadstream.h>
#include <rapidjson/filewritestream.h>
#include <rapidjson/istreamwrapper.h>


//#include "private.h"
//#include "welcome.h"
#include "main.h"
#include "project.h"
#include "variables.h"
#include "case.h"
#include "casewin.h"
#include "invoke.h"
#include "library.h"
#include "uiobjects.h"
#include "variablegrid.h"
#include "script.h"
#include "pythonhandler.h"
//#include "main_add.h"
#include <../src/main_add.h>

#define __SAVE_AS_JSON__ 1
#define __LOAD_AS_JSON__ 1

static PythonConfig pythonConfig;


enum { __idFirst = wxID_HIGHEST+592,

	ID_MAIN_MENU, ID_CASE_TABS, ID_PAGE_NOTES,
	ID_CASE_CREATE, ID_RUN_ALL_CASES, ID_SAVE_HOURLY,
	ID_IMPORT_CASES,
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
	ID_CASE_GENERATE_CODE,
	ID_CASE_MOVE_LEFT,
	ID_CASE_MOVE_RIGHT,
	__idCaseMenuLast,
	__idInternalFirst,
		ID_INTERNAL_IDE, ID_INTERNAL_RESTART, ID_INTERNAL_SHOWLOG, ID_INTERNAL_SEGFAULT,
		ID_INTERNAL_DATAFOLDER, ID_INTERNAL_CASE_VALUES, ID_SAVE_CASE_DEFAULTS, ID_SAVE_CASE_AS_JSON, ID_LOAD_CASE_FROM_JSON, 
		ID_SAVE_CASE_AS_SSC_JSON, ID_LOAD_CASE_FROM_SSC_JSON, ID_LOAD_RUN_CASE_FROM_SSC_JSON, ID_INTERNAL_INVOKE_SSC_DEBUG,
	__idInternalLast
};

BEGIN_EVENT_TABLE( MainWindow, wxFrame )
	EVT_CLOSE( MainWindow::OnClose )
	EVT_MENU( wxID_ABOUT, MainWindow::OnCommand )
	EVT_MENU( wxID_HELP, MainWindow::OnCommand )
	EVT_MENU( ID_SAVE_HOURLY, MainWindow::OnCommand )
	EVT_MENU( ID_IMPORT_CASES, MainWindow::OnCommand )
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
	: wxFrame( 0, wxID_ANY, wxString(MAIN_WINDOW_TITLE) + SamApp::VersionStr(),
		wxDefaultPosition, wxScaleSize( 1100, 700 ) )
{
#ifdef __WXMSW__
	SetIcon( wxICON( appicon ) );
#endif

#ifdef __WXOSX__
	wxMenu *fileMenu = new wxMenu;
	fileMenu->Append( wxID_NEW, "New project\tCtrl-N" );
	fileMenu->Append( ID_NEW_SCRIPT, "New script" );
	fileMenu->AppendSeparator();
	fileMenu->Append( wxID_OPEN, "Open project\tCtrl-O" );
	fileMenu->Append( ID_OPEN_SCRIPT, "Open script" );
	fileMenu->AppendSeparator();
	fileMenu->Append( wxID_SAVE, "Save\tCtrl-S" );
	fileMenu->Append( wxID_SAVEAS, "Save as..." );
	fileMenu->Append( ID_SAVE_HOURLY, "Save with hourly results");
	fileMenu->AppendSeparator();
	fileMenu->Append( ID_IMPORT_CASES, "Import cases..");
	fileMenu->AppendSeparator();
	fileMenu->Append( ID_BROWSE_INPUTS, "Inputs browser...");
	fileMenu->AppendSeparator();
	fileMenu->Append( wxID_EXIT, "Quit SAM");

	wxMenu *caseMenu = new wxMenu;
	caseMenu->Append( ID_CASE_SIMULATE, "Simulate\tF5" );
	caseMenu->Append( ID_CASE_REPORT, "Create report\tF6" );
	caseMenu->Append( ID_CASE_CLEAR_RESULTS, "Clear all results" );
	caseMenu->AppendSeparator();
	caseMenu->Append( ID_CASE_RENAME, "Rename\tF2" );
	caseMenu->Append( ID_CASE_DUPLICATE, "Duplicate" );
	caseMenu->Append( ID_CASE_DELETE, "Delete" );
	caseMenu->AppendSeparator();
	caseMenu->Append( ID_CASE_MOVE_LEFT, "Move left" );
	caseMenu->Append( ID_CASE_MOVE_RIGHT, "Move right" );
	caseMenu->AppendSeparator();
	caseMenu->Append( ID_CASE_CONFIG, "Change model..." );
	caseMenu->Append( ID_CASE_RESET_DEFAULTS, "Reset inputs to default values" );

	wxMenu *helpMenu = new wxMenu;
	helpMenu->Append( wxID_HELP );
	helpMenu->AppendSeparator();
	helpMenu->Append( wxID_ABOUT );

	wxMenuBar *menuBar = new wxMenuBar;
	menuBar->Append( fileMenu, wxT("&File") );
	menuBar->Append( caseMenu, wxT("&Case")  );
	menuBar->Append( helpMenu, wxT("&Help")  );
	SetMenuBar( menuBar );
#endif

	m_topBook = new wxSimplebook( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE );

	m_welcomeScreen = new WelcomeScreen( m_topBook );
	m_topBook->AddPage( m_welcomeScreen, wxT("Welcome to SAM") );


	m_caseTabPanel = new wxPanel( m_topBook );
	m_topBook->AddPage( m_caseTabPanel, wxT("Main project window") );

	wxMetroButton *metbut = 0;

	wxBoxSizer *tools = new wxBoxSizer( wxHORIZONTAL );
	//tools->Add( m_mainMenuButton = new wxMetroButton( m_caseTabPanel, ID_MAIN_MENU, wxEmptyString, wxBITMAP_PNG_FROM_DATA( menu ), wxDefaultPosition, wxDefaultSize /*, wxMB_DOWNARROW */), 0, wxALL|wxEXPAND, 0 );
	tools->Add( m_mainMenuButton = new wxMetroButton( m_caseTabPanel, ID_MAIN_MENU, "File", wxNullBitmap/*wxBITMAP_PNG_FROM_DATA( menu )*/, wxDefaultPosition, wxDefaultSize, wxMB_DOWNARROW ), 0, wxALL|wxEXPAND, 0 );
	tools->Add( metbut = new wxMetroButton( m_caseTabPanel, ID_CASE_CREATE, "Add", wxBITMAP_PNG_FROM_DATA( cirplus ), wxDefaultPosition, wxDefaultSize), 0, wxALL|wxEXPAND, 0 );
	metbut->SetToolTip( "Add case" );
	m_caseTabList = new wxMetroTabList( m_caseTabPanel, ID_CASE_TABS, wxDefaultPosition, wxDefaultSize, wxMT_MENUBUTTONS );

	tools->Add( m_caseTabList, 1, wxALL|wxEXPAND, 0 );
	tools->Add( metbut = new wxMetroButton( m_caseTabPanel, ID_PAGE_NOTES, wxEmptyString, wxBITMAP_PNG_FROM_DATA( notes_white ), wxDefaultPosition, wxDefaultSize), 0, wxALL|wxEXPAND, 0 );
	metbut->SetToolTip( "Add a page note" );

	tools->Add( new wxMetroButton( m_caseTabPanel, wxID_HELP, "Help",/* wxBITMAP_PNG_FROM_DATA(qmark) */ wxNullBitmap, wxDefaultPosition, wxDefaultSize), 0, wxALL|wxEXPAND, 0 );

	m_caseNotebook = new wxSimplebook( m_caseTabPanel, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE );

	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( tools, 0, wxALL|wxEXPAND, 0 );
	sizer->Add( m_caseNotebook, 1, wxALL|wxEXPAND, 0 );
	m_caseTabPanel->SetSizer(sizer);

	m_topBook->SetSelection( 0 );

	std::vector<wxAcceleratorEntry> entries;
	entries.push_back( wxAcceleratorEntry( wxACCEL_SHIFT, WXK_F4, ID_INTERNAL_SHOWLOG));
	entries.push_back( wxAcceleratorEntry( wxACCEL_SHIFT, WXK_F5,  ID_INTERNAL_INVOKE_SSC_DEBUG ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_SHIFT, WXK_F7,  ID_INTERNAL_IDE ) ) ;
	entries.push_back( wxAcceleratorEntry( wxACCEL_SHIFT, WXK_F8,  ID_INTERNAL_RESTART ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_SHIFT, WXK_F9, ID_INTERNAL_DATAFOLDER));
	entries.push_back( wxAcceleratorEntry( wxACCEL_SHIFT, WXK_F10, ID_SAVE_CASE_DEFAULTS));
	entries.push_back( wxAcceleratorEntry( wxACCEL_SHIFT, WXK_F11, ID_RUN_ALL_CASES));
	entries.push_back( wxAcceleratorEntry( wxACCEL_SHIFT, WXK_F12, ID_INTERNAL_CASE_VALUES));
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL, WXK_F1, ID_INTERNAL_SEGFAULT));
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL, WXK_F5, ID_SAVE_CASE_AS_JSON));
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL, WXK_F6, ID_LOAD_CASE_FROM_JSON));
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL, WXK_F7, ID_SAVE_CASE_AS_SSC_JSON));
	entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL, WXK_F8, ID_LOAD_CASE_FROM_SSC_JSON));
	entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL, WXK_F9, ID_LOAD_RUN_CASE_FROM_SSC_JSON));
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL, WXK_F10, ID_CASE_DUPLICATE));
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL, WXK_F11, ID_BROWSE_INPUTS));
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL | wxACCEL_SHIFT, 'n', ID_NEW_SCRIPT ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL, 'o', wxID_OPEN ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL | wxACCEL_SHIFT, 'o', ID_OPEN_SCRIPT ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL, 's', wxID_SAVE ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL, 'w', wxID_CLOSE ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_NORMAL, WXK_F1, wxID_HELP));
	entries.push_back( wxAcceleratorEntry( wxACCEL_NORMAL, WXK_F2, ID_CASE_RENAME ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_NORMAL, WXK_F5, ID_CASE_SIMULATE ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_NORMAL, WXK_F6, ID_CASE_REPORT ) );
	SetAcceleratorTable( wxAcceleratorTable( entries.size(), &entries[0] ) );
}


class CaseImportDialog : public wxDialog
{
private:
	wxCheckListBox *m_cklList;
public:
	CaseImportDialog( wxWindow *parent, const wxString &title )
		: wxDialog( parent, wxID_ANY, title, wxDefaultPosition,
			wxScaleSize(350,300), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)
	{
		m_cklList = new wxCheckListBox( this, wxID_ANY );

		wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
		sizer->Add( m_cklList, 1, wxALL|wxEXPAND, 8 );
		sizer->Add( CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 8 );

		SetSizer(sizer);

		SetEscapeId( wxID_CANCEL );
	}

	void SetItems( const wxArrayString &list )
	{
		m_cklList->Clear();
		m_cklList->Append( list );
	}

	wxArrayString GetChecked()
	{
		wxArrayString list;
		for (size_t i=0;i<m_cklList->GetCount();i++)
			if ( m_cklList->IsChecked(i) )
				list.Add( m_cklList->GetString(i) );
		return list;
	}
};

void MainWindow::ImportCases()
{
	wxFileDialog fdlg( this, "Select a SAM project file", wxEmptyString, wxEmptyString, "SAM Project Files (*.sam)|*.sam", wxFD_OPEN );
	if ( fdlg.ShowModal() != wxID_OK )
		return;

	wxString file( fdlg.GetPath() );
	if ( wxFileName(file).SameAs( GetProjectFileName() ) )
	{
		wxMessageBox("The file you selected is currently open.");
		return;
	}


	int prj_major, prj_minor, prj_micro;
	int sam_major, sam_minor, sam_micro;

	ProjectFile prj;
	if ( !prj.ReadArchive( file ) )
	{
		wxMessageBox( "Could not read project file:\n\n" + file );
		return;
	}

	size_t file_ver = prj.GetVersionInfo( &prj_major, &prj_minor, &prj_micro );
	size_t sam_ver = SamApp::Version( &sam_major, &sam_minor, &sam_micro );

	if ( file_ver > sam_ver )
	{
		wxMessageBox( "The file '" + wxFileNameFromPath(file) + "' was saved using a newer SAM version "
			+ wxString::Format( "%d.%d.%d.\n"
				"You are currently using SAM version %d.%d.%d.\n\n"
				"Please upgrade to the latest version of SAM to import a case from this file.",
				prj_major, prj_minor, prj_micro, sam_major, sam_minor, sam_micro), "Version Error", wxICON_ERROR);
		return;
	}

	if ( file_ver < sam_ver )
	{
		VersionUpgrade upgd;

		{ // scope to show busy info dialog
//			wxBusyInfo info( "Upgrading project file to current SAM version..." );
			if ( !upgd.Run( prj ) )
				wxMessageBox("Error upgrading older project file:\n\n", file );
		}

		upgd.ShowReportDialog( file, true );
	}

	CaseImportDialog cdlg(this, "Select case(s) to import:");
	cdlg.CenterOnParent();
	cdlg.SetItems( prj.GetCaseNames() );
	if ( cdlg.ShowModal() != wxID_OK)
		return;

	wxArrayString selections = cdlg.GetChecked();
	for (size_t i=0;i<selections.Count();i++)
	{
		wxString cname( selections[i] );

		if ( Case *to_import = prj.GetCase( cname ) )
		{
			// duplicate case and append it to current project
			wxBusyInfo info("Importing case: " + cname );

			if( Case *dup = dynamic_cast<Case*>(to_import->Duplicate()) )
			{
				cname = GetUniqueCaseName(cname);
				m_project.AddCase( cname, dup );
				CreateCaseWindow( dup );
				SwitchToCaseWindow( cname );
			}
		}
	}
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
	if (pages.size() > 0) {
		win->Freeze();
		// go through and load all pages to trigger on-load events - addresses SAM issue 1520 and other requests
		for (int i = 0; i <= c->GetConfiguration()->Technology.size() - 1; i++) {
			for (int j=0; j< c->GetConfiguration()->InputPageGroups[i].size(); j++)
	            win->SwitchToInputPage(c->GetConfiguration()->InputPageGroups[i][j]->SideBarLabel);
        }
		// load first page of hybrid and non-hybrid configurations
		if (c->GetConfiguration()->Technology.size() > 1) // hybrid	
			win->SwitchToInputPage(c->GetConfiguration()->InputPageGroups[c->GetConfiguration()->Technology.size() - 1][0]->SideBarLabel);
		else
			win->SwitchToInputPage(pages[0]);

		// reevaluate all equations address SAM #1583
		c->EvaluateEquations();
		//for (size_t ndxHybrid = 0; ndxHybrid < c->GetConfiguration()->Technology.size(); ndxHybrid++)
		//	c->RecalculateAll(ndxHybrid, true);

		win->Thaw();
	} //mp trying to not overwrite first page switch at start
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
	{
		size_t tab_sel = m_caseTabList->GetSelection();
		wxString case_name = m_caseTabList->GetLabel(tab_sel);
		Case *c = m_project.GetCase(case_name);
		CaseWindow *cw = GetCaseWindow(c);
		if (c == 0 || cw == 0) return; // error

		CodeGen_Base::ShowCodeGenDialog(cw);
		// code generator
		/*
		if ( Case *cc = GetCurrentCase() )
		{
		// initialize properties
		wxString folder = SamApp::Settings().Read("SSCDebugFolder");
		if (folder.IsEmpty()) folder = ::wxGetHomeDir();
		// get folder
		wxDirDialog dlg(SamApp::Window(), "Select an output folder", folder, wxDD_DEFAULT_STYLE | wxDD_DIR_MUST_EXIST);
		if (dlg.ShowModal() == wxID_OK)
		{
		folder = dlg.GetPath();
		folder.Replace("\\", "/");
		SamApp::Settings().Write("SSCDebugFolder", folder);
		}
		else // user cancelled.
		return;

		if (wxDir::Exists(folder))
		{
		cc->BaseCase().Clear();
		if (!cc->BaseCase().Invoke(false, true, folder))
		wxShowTextMessageDialog(wxJoin(cc->BaseCase().GetAllMessages(), '\n'));
		if (CaseWindow *cw = GetCaseWindow(cc))
		cw->UpdateResults();
		wxLaunchDefaultApplication(folder);
		}
		}
		*/
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
			new VariableGridFrame(this, &m_project, cc);
		}
		break;
	case ID_SAVE_CASE_DEFAULTS:
		if (Case* cc = GetCurrentCase())
		{
			cc->SaveDefaults();
		}
		break;
	case ID_SAVE_CASE_AS_JSON:
		if (Case* cc = GetCurrentCase())
		{
			size_t tab_sel = m_caseTabList->GetSelection();
			wxString case_name = m_caseTabList->GetLabel(tab_sel);

			wxFileDialog fdlg(this, "Save the active case as a JSON file", wxEmptyString,
				case_name + ".json", "JSON (*.json)|*.json", wxFD_SAVE | wxFD_OVERWRITE_PROMPT);

			if (fdlg.ShowModal() == wxID_OK) {
				cc->SaveAsJSON(true, fdlg.GetPath(), case_name);
			}
		}
		break;
	case ID_LOAD_CASE_FROM_JSON:
	{
		// Read JSON file and get case_name and config_info
		wxFileDialog fdlg(this, "Load a case from a JSON file", wxEmptyString,
			".json", "JSON (*.json)|*.json", wxFD_OPEN);

		if (fdlg.ShowModal() == wxID_OK) {
			rapidjson::Document doc;

			wxString sfn = fdlg.GetPath();
			wxFileName fn(sfn);
			wxFileInputStream fis(sfn);

			if (!fis.IsOk()) {
				wxLogError(wxS("Couldn't open the file '%s'."), sfn);
				break;
			}
			wxStringOutputStream os;

			wxString case_name, tech, fin;

			fis.Read(os);
			rapidjson::StringStream is(os.GetString().c_str());
			doc.ParseStream(is);
			if (doc.HasParseError()) {
				wxLogError(wxS("Could not read the json file string conversion '%s'."), sfn);
				break;
			}
			else {
				for (rapidjson::Value::ConstMemberIterator itr = doc.MemberBegin(); itr != doc.MemberEnd(); ++itr) {
					// can search for more efficiency
					wxString str_name = itr->name.GetString();
					if (str_name == "Case_name")
						case_name = itr->value.GetString();
					else if (str_name == "Technology")
						tech = itr->value.GetString();
					else if (str_name == "Financing")
						fin = itr->value.GetString();
				}
			}

			if (0 == SamApp::Config().Find(tech, fin)) {
				wxMessageBox("Internal error: could not locate configuration information for " + tech + "/" + fin);
				break;
			}

			if (m_topBook->GetSelection() != 1)
				m_topBook->SetSelection(1); // switch to cases view if currently in welcome window

			Case* c = m_project.AddCase(GetUniqueCaseName(case_name));
			c->SetConfiguration(tech, fin);
			wxString error = "";
//			c->LoadFromJSON(sfn, &error);
			CreateCaseWindow(c);
			if (error.Len() > 0) wxMessageBox(error);
		}
	}
		break;
	case ID_SAVE_CASE_AS_SSC_JSON:
		if (Case* cc = GetCurrentCase())
		{
			size_t tab_sel = m_caseTabList->GetSelection();
			wxString case_name = m_caseTabList->GetLabel(tab_sel);

			wxFileDialog fdlg(this, "Save the active case as a SSC JSON file", wxEmptyString,
				case_name + ".json", "JSON (*.json)|*.json", wxFD_SAVE | wxFD_OVERWRITE_PROMPT);

			if (fdlg.ShowModal() == wxID_OK) {
				cc->SaveAsSSCJSON(fdlg.GetPath());
			}
		}
		break;
	case ID_LOAD_CASE_FROM_SSC_JSON:
	{
		// Read JSON file and get case_name and config_info
		wxFileDialog fdlg(this, "Load a case from a ssc JSON file", wxEmptyString,
			".json", "JSON (*.json)|*.json", wxFD_OPEN);

		if (fdlg.ShowModal() == wxID_OK) {
			wxString case_name, tech, fin;


			wxString sfn = fdlg.GetPath();

			/* For reading in "Case_name", "Technology", and "Financing"
			rapidjson::Document doc;

			wxFileName fn(sfn);
			wxFileInputStream fis(sfn);

			if (!fis.IsOk()) {
				wxLogError(wxS("Couldn't open the file '%s'."), sfn);
				break;
			}
			wxStringOutputStream os;


			fis.Read(os);
			rapidjson::StringStream is(os.GetString().c_str());
			doc.ParseStream(is);
			if (doc.HasParseError()) {
				wxLogError(wxS("Could not read the json file string conversion '%s'."), sfn);
				break;
			}
			else {
				for (rapidjson::Value::ConstMemberIterator itr = doc.MemberBegin(); itr != doc.MemberEnd(); ++itr) {
					// can search for more efficiency
					wxString str_name = itr->name.GetString();
					if (str_name == "Case_name")
						case_name = itr->value.GetString();
					else if (str_name == "Technology")
						tech = itr->value.GetString();
					else if (str_name == "Financing")
						fin = itr->value.GetString();
				}
			}
			*/

			// present configuraiton selection dialog to set technology and financing (reads directly from JSON for Inputs code generation file)
			bool reset = false;
			if (!ShowConfigurationDialog(this, &tech, &fin, &reset))
				break;

			case_name = "SSC inputs";

			if (0 == SamApp::Config().Find(tech, fin)) {
				wxMessageBox("Internal error: could not locate configuration information for " + tech + "/" + fin);
				break;
			}

			if (m_topBook->GetSelection() != 1)
				m_topBook->SetSelection(1); // switch to cases view if currently in welcome window

			Case* c = m_project.AddCase(GetUniqueCaseName(case_name));
			c->SetConfiguration(tech, fin);
			wxString error = "";
//			c->LoadFromSSCJSON(sfn, &error);
			CreateCaseWindow(c);
			if (error.Len() > 0) wxMessageBox(error);
		}
	}
	break;

	case ID_LOAD_RUN_CASE_FROM_SSC_JSON:
	{
		// Read JSON file and get case_name and config_info
		wxFileDialog fdlg(this, "Load and run a case from a ssc JSON file", wxEmptyString,
			".json", "JSON (*.json)|*.json", wxFD_OPEN);

		if (fdlg.ShowModal() == wxID_OK) {
			wxString case_name, tech, fin;

			wxString sfn = fdlg.GetPath();

			// present configuraiton selection dialog to set technology and financing (reads directly from JSON for Inputs code generation file)
			bool reset = false;
			if (!ShowConfigurationDialog(this, &tech, &fin, &reset))
				break;

			case_name = "SSC run inputs";

			if (0 == SamApp::Config().Find(tech, fin)) {
				wxMessageBox("Internal error: could not locate configuration information for " + tech + "/" + fin);
				break;
			}

			if (m_topBook->GetSelection() != 1)
				m_topBook->SetSelection(1); // switch to cases view if currently in welcome window

			Case* c = m_project.AddCase(GetUniqueCaseName(case_name));

			wxString error = "";
			if (c->PreRunSSCJSON(tech, fin, sfn, &error)) {
				if (wxMessageBox("Continue and create case loading any missing defaults?", "Create Case", wxYES_NO) == wxYES) {
					c->SetConfiguration(tech, fin); // loads defaults
					error = "";
					// overwrite defaults with JSON values
//					c->LoadFromSSCJSON(sfn, &error);
					auto* cw = CreateCaseWindow(c);
					//cw->RunSSCBaseCase(sfn, false, &error); // optionally run base case
					//if (error.Len() > 0) wxMessageBox(error);
				}
			}
			else {
				if (error.Len() > 0) wxMessageBox(error);
			}
		}
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
			title = "Inputs Browser"; //: " + case_name;
			var_table_vec.push_back(cases[0]->Values(0)); // TODO - handle hybrid technologies
			var_info_lookup_vec.push_back(cases[0]->Variables(0));
		}
		else
		{
			title = "Inputs Browser"; //Case comparison";
			col_hdrs = m_project.GetCaseNames();
			col_hdrs.Insert("Label", 0);
			col_hdrs.Insert("Variable", 0);
			for (std::vector<Case*>::iterator it = cases.begin(); it != cases.end(); ++it)
			{
				var_table_vec.push_back((*it)->Values(0));  // TODO - handle hybrid technologies
				var_info_lookup_vec.push_back((*it)->Variables(0));
			}
		}

		wxFrame *frame = new wxFrame(this, wxID_ANY, title, wxDefaultPosition, wxScaleSize(400, 700));
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
			grid->SetColSize(col, col_width[col]);
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
	case wxID_ABOUT:
		SamApp::ShowHelp( ":about" );
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
			menu.Append( ID_IMPORT_CASES, "Import cases..." );
			menu.AppendSeparator();
			menu.Append( ID_BROWSE_INPUTS, "Inputs browser...");
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
		SamScriptWindow::CreateNewWindow();
		break;
	case ID_OPEN_SCRIPT:
		SamScriptWindow::OpenFiles();
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
	case ID_IMPORT_CASES:
		ImportCases();
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

bool MainWindow::CheckVersionBeforeSaving( const wxString &file )
{
	int major, minor, micro;
	if ( m_project.GetVersionInfo(&major,&minor,&micro) < SamApp::Version() && wxFileExists(file) )
		return wxYES == wxMessageBox( wxString::Format(
				"This project was originally created with a previous version of SAM, version %d.%d.%d.\n\n"
				"After saving, you will not be able to open this project in the previous version of SAM.\n\n", major, minor, micro )
				+ "Overwrite " + file + "?",
			"Query", wxYES_NO|wxICON_WARNING, this );
	else
		return true;
}

void MainWindow::Save()
{
	if ( m_projectFileName.IsEmpty() )
	{
		SaveAs();
		return;
	}
	else if ( !CheckVersionBeforeSaving( m_projectFileName ) )
	{
		return;
	}

	if ( !SaveProject( m_projectFileName ) )
		wxMessageBox("Error writing project to disk:\n\n" + m_projectFileName, "Notice", wxOK, this );

	UpdateFrameTitle();
}

void MainWindow::SaveAs()
{
	wxFileDialog dlg( this, "Save SAM file as", wxPathOnly(m_projectFileName),
		m_projectFileName, "SAM Project File (*.sam)|*.sam",
		wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
	if ( dlg.ShowModal() == wxID_OK )
	{
		if ( m_projectFileName == dlg.GetPath()
			&& !CheckVersionBeforeSaving( dlg.GetPath() ) )
			return;

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

	ProjectFile pf;
	// tell user to save and check file if issue
	if (!pf.ReadArchive(file)) {
//		wxMessageBox(wxString::Format("Problem reading file!\n\n%s\n\n%sTo fix the problem, click OK to open the file and then save it.", file, pf.GetLastError()));
		wxMessageBox(wxString::Format("Problem reading file!\n\n%s\n\n%s.", file, pf.GetLastError()));
		return false;
	}

	int major, minor, micro;
	size_t file_ver = pf.GetVersionInfo( &major, &minor, &micro );

	int sammajor, samminor, sammicro;
	size_t sam_ver = SamApp::Version( &sammajor, &samminor, &sammicro );

	if ( file_ver > sam_ver )
	{
		wxMessageBox( wxString::Format("The file '%s' was last saved using SAM version %d.%d.%d.\n"
				"You are currently running SAM version %d.%d.%d.\n\n"
				"Please upgrade to the latest version of SAM to open this file.",
				(const char*)wxFileNameFromPath(file).c_str(),
				major, minor, micro,
				sammajor, samminor, sammicro ),
				"Version Error", wxICON_ERROR );

		return false;
	}


	if ( file_ver < sam_ver )
	{
		wxMessageBox( wxString::Format("The file you are opening was created with an older version of SAM, Version %d.%d.%d.\n\n%s\n\n"
			"There may be changes between versions that cause simulation results to be different. The Version Upgrade Report in the next "
			"window lists any input variables that have changed between versions.\n\n",
			major, minor, micro, (const char*)wxFileNameFromPath(file).c_str()),
			"Notice", wxICON_INFORMATION, this);

//		wxBusyInfo info( "Upgrading project file to current SAM version..." );

		VersionUpgrade upgd;
		upgd.Run( pf );
		upgd.ShowReportDialog( file );
		if (pf.GetCases().size() == 0) {
			wxMessageBox("Cannot open a file that contains only retired cases.", "Information", wxOK);
			pf.Clear();
			return false;
		}
	}

	// copy over project file data,
	// but don't copy PF event listeners
	m_project.Copy( pf, false );

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


	// update version information in project to current SAM
	m_project.SetVersionInfo(
		SamApp::VersionMajor(),
		SamApp::VersionMinor(),
		SamApp::VersionMicro(),
		SamApp::RevisionNumber() );

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

void MainWindow::OnCaseTabButton( wxCommandEvent & )
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
	menu.Append( ID_CASE_CONFIG, "Change model..." );
	menu.Append( ID_CASE_RESET_DEFAULTS, "Reset inputs to default values" );
#ifdef __WXMSW__
	menu.AppendSeparator();
	menu.Append( ID_CASE_EXCELEXCH, "Excel exchange...");
#endif
	//menu.AppendSeparator();
	//menu.Append( ID_CASE_IMPORT, "Import" );
	menu.AppendSeparator();
	menu.Append(ID_CASE_GENERATE_CODE, "Generate code...");

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
		if (m_project.GetCases().size() > 1) {
			if (wxYES == wxMessageBox("Really delete case " + case_name + "?  This action cannot be reversed.", "Query", wxYES_NO, this)) {
				DeleteCaseWindow(c);
				m_project.DeleteCase(case_name);
				if (m_project.GetCases().size() == 0) {
					m_topBook->SetSelection(0);
				}
			}
		}
		else {
			wxMessageBox("Cannot delete case when there is only one case in the file.", "Information", wxOK);
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
			for (size_t i = 0; i < c->GetConfiguration()->Technology.Count(); i++)
				c->SendEvent( CaseEvent( CaseEvent::VARS_CHANGED, c->Values(i).ListAll() ) ); // TODO - test hybrids UI update
		}
		break;
	case ID_CASE_EXCELEXCH:
		ExcelExchange::ShowExcelExchangeDialog(c->ExcelExch(), cw);
		break;
	case ID_CASE_GENERATE_CODE:
		CodeGen_Base::ShowCodeGenDialog( cw);
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

	if ( !SamScriptWindow::CloseAll() )
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
	wxString title = wxString(MAIN_WINDOW_TITLE) + SamApp::VersionStr();
	if ( !m_projectFileName.IsEmpty() )	title += ": " + m_projectFileName;
	SetTitle( title );
}



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
	wxDataOutputStream out(os);
	out.Write8( 0x48 );
	out.Write8( 1 );

	m_form.Write( os );
	m_vars.Write( os );
	out.WriteString( m_eqnScript );
	out.WriteString( m_cbScript );

	out.Write8( 0x48 );
}


bool InputPageData::Read(wxInputStream &is)
{
	wxDataInputStream in(is);
	wxUint8 code = in.Read8();
	in.Read8(); // wxUint8 ver

	bool ok = true;
	ok = ok && m_form.Read(is);
	ok = ok && m_vars.Read(is);
	m_eqnScript = in.ReadString();
	m_cbScript = in.ReadString();

	return in.Read8() == code && ok;
}

void InputPageData::Write_text(wxOutputStream &os, wxString &ui_path)
{
	wxTextOutputStream out(os, wxEOL_UNIX);
	m_form.Write_text(os,ui_path);
	m_vars.Write_text(os);
	size_t n = m_eqnScript.Len();
	out.PutChar('\n');
	wxString x = m_eqnScript;
	x.Replace("\r", "");
	n = x.Len();
	out.Write32((wxUint32)n);
	if (n > 0)
	{
		out.PutChar('\n');
		for (size_t i = 0; i < n; i++)
		{
			out.PutChar(x[i]);
		}
	}
	out.PutChar('\n');
	n = m_cbScript.Len();
	x = m_cbScript;
	x.Replace("\r", "");
	n = x.Len();
	out.Write32((wxUint32)n);
	if (n > 0)
	{
		out.PutChar('\n');
		for (size_t i = 0; i < n; i++)
		{
				out.PutChar(x[i]);
		}
	}
}

bool InputPageData::Read_text(wxInputStream &is, wxString &ui_path)
{
	wxTextInputStream in(is, "\n", wxConvAuto(wxFONTENCODING_UTF8));
	bool ok = true;
	ok = ok && m_form.Read_text(is, ui_path);
	ok = ok && m_vars.Read_text(is);
	m_eqnScript.Clear();
	size_t n = in.Read32();
	if (n > 0)
	{
		for (size_t i = 0; i < n; i++)
				m_eqnScript.Append(in.GetChar());
	}
	m_cbScript.Clear();
	n = in.Read32();
	if (n > 0)
	{
		for (size_t i = 0; i < n; i++)
			m_cbScript.Append(in.GetChar());
	}
	return ok;
}

bool InputPageData::Write_JSON(const std::string& file, wxString& ui_path)
{
	rapidjson::Document doc;
	Write_JSON(doc, ui_path);

	rapidjson::StringBuffer os;
	rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(os);
	doc.Accept(writer);

	if (doc.HasParseError()) {
		wxLogError(wxS("Could not read the json file '%s'.\nError: %d"), file.c_str(), doc.GetParseError());
		return false;
	}

	wxFFileOutputStream out(file);
	out.Write(os.GetString(), os.GetSize());
	out.Close();
	return true;
}

void InputPageData::Write_JSON(rapidjson::Document& doc, wxString& ui_path)
{
	doc.SetObject();

	m_form.Write_JSON(doc, ui_path); 
	m_vars.Write_JSON(doc); 
	Write_JSON_multiline_value(doc, "Equations", m_eqnScript);
	Write_JSON_multiline_value(doc, "Callbacks", m_cbScript);
}

bool InputPageData::Read_JSON(const std::string& file, wxString& ui_path)
{
	rapidjson::Document doc;
	std::ifstream ifs(file);
	rapidjson::IStreamWrapper is(ifs);

	doc.ParseStream(is);

	if (doc.HasParseError()) {
		wxLogError(wxS("Could not read the json file '%s'.\nError: %d"), file.c_str(), doc.GetParseError());
		return false;
	}
	else {
		Read_JSON(doc, ui_path);
		return true;
	}
}

bool InputPageData::Read_JSON(const rapidjson::Document& doc, wxString& ui_path)
{
	bool ok = true;
	ok = ok && m_form.Read_JSON(doc, ui_path);
	ok = ok && m_vars.Read_JSON(doc);
    m_eqnScript.Clear();
	m_eqnScript = Read_JSON_multiline_value(doc, "Equations");
	m_cbScript.Clear();
	m_cbScript = Read_JSON_multiline_value(doc, "Callbacks");
	return ok;
}



bool InputPageData::BuildDatabases()
{
	m_eqns.Clear();
	m_cbs.ClearAll();

	// TODO - add m_eqnScript preprocess to expand compute module calls per Github SAM Issue#634 and PR#687

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


bool InputPageDatabase::LoadFileText(const wxString &file)
{
	wxFileName ff(file);
	wxString name(ff.GetName());

	InputPageData *pd = new InputPageData;
	wxString ui_path = SamApp::GetRuntimePath() + "/ui/";
	bool ok = true;
//	wxFFileInputStream is(file);
//	if (!is.IsOk() || !pd->Read_text(is))
	wxFFileInputStream is(file, "r");
	bool bff = is.IsOk();
	bool bread = pd->Read_text(is, ui_path);
	if (!bff && !bread)
		ok = false;

	pd->Form().SetName(name);

	if (ok) Add(name, pd);
	else delete pd;

	if (!pd->BuildDatabases())
	{
		wxLogStatus("failed to build equation and script databases for: " + name);
		ok = false;
	}

	return ok;
}


bool InputPageDatabase::LoadFileJSON(const wxString& file)
{
	wxFileName ff(file);
	wxString name(ff.GetName());

	InputPageData* pd = new InputPageData;
	wxString ui_path = SamApp::GetRuntimePath() + "/ui/";
	bool ok = true;
	bool bread = pd->Read_JSON(file.ToStdString(), ui_path);
	if (!bread)
		ok = false;

	pd->Form().SetName(name);

	if (ok) Add(name, pd);
	else delete pd;

	if (!pd->BuildDatabases())
	{
		wxLogStatus("failed to build equation and script databases for: " + name);
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
	for( size_t i=0;i<InputPageGroups.size();i++) 
		for (size_t j=0; j< InputPageGroups[i].size(); j++)
			delete InputPageGroups[i][j];
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

			size_t vector_sizes = 1;
			// hybrid handling
			ci->TechnologyFullName = tech;
			auto as = wxSplit(tech, ' ');
			if (as.Count() > 1 && as[as.size() - 1].Lower() == "hybrid") {
				vector_sizes = as.size();
				for (size_t j = 0; j < as.size(); j++)
					ci->Technology.push_back(as[j]);
			}
			else {
				ci->Technology.push_back(tech);
			}
			ci->InputPageGroups.resize(vector_sizes);
			ci->InputPages.resize(vector_sizes);
			ci->Variables.resize(vector_sizes);
			ci->AutoVariables.resize(vector_sizes);
			ci->Equations.resize(vector_sizes);

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
	if (m_curConfig != NULL) {
		m_curConfig->Simulations = list;
	}
}

void ConfigDatabase::SetHybridVariableDependencies(const std::vector<HybridVariableDependencies>& dependencies)
{
	if (m_curConfig != NULL) {
		m_curConfig->HybridVariables = dependencies;
	}
}


void ConfigDatabase::AddInputPageGroup( const std::vector< std::vector<PageInfo> > &pages, const wxString &sidebar,
	const wxString &hlpcxt, const wxString &exclvar,
	const std::vector<PageInfo> &exclhdr_pages,
	bool excl_tabs, bool excl_hide, wxString bin_name, bool excl_top)
{
	if ( m_curConfig == 0 ) return;

	InputPageGroup *ip = new InputPageGroup;
	ip->Pages = pages;
	ip->SideBarLabel = sidebar;
	ip->HelpContext = hlpcxt;
	ip->OrganizeAsExclusivePages = !exclvar.IsEmpty();
	ip->ExclusivePageVar = exclvar;
	ip->ExclusiveHeaderPages = exclhdr_pages;
	ip->ExclusiveTabs = excl_tabs;
    ip->ExclusiveHide = excl_hide;
    ip->BinName = bin_name;
    ip->ExclTop = excl_top;

	int ndx = 0;
	// assumption is that bin_name is one of hybrid techs, e.g. for "PVWatts Wind Battery Hybrid" technology, the bin_name = PVWatts, Wind or Battery
	if (m_curConfig->Technology.size() > 1)  { // do not process for m_curConfig.Technology.size() == 1 e.g. "PV Battery" does not end with "Hybrid"
		ndx = m_curConfig->Technology.Index(bin_name);
        if (ndx == wxNOT_FOUND) { // no bin name - place in "Hybrid" technology and all technologies so that vartables contain dependencies like "analysisperiod" and "salestax"
			ndx = (int)m_curConfig->Technology.size() - 1; // TODO: check that values are updated on other forms.
            m_curConfig->InputPageGroups[ndx].push_back(ip);
        }
		else if (ndx < 0 || ndx >(int)m_curConfig->InputPageGroups.size()) {
			wxMessageBox("Internal error in configuration.\n\n" + m_curConfig->TechnologyFullName + ", " + m_curConfig->Financing + "   [ " + ip->BinName + " ]\n\n"
				"An error occurred when attempting to add input pages.", "sam-engine", wxICON_ERROR | wxOK);
			return;
		}
		else {
			m_curConfig->InputPageGroups[ndx].push_back(ip);
		}
	}
	else { // InputPageGroups sized during Add call - non-hybrid use first element
		m_curConfig->InputPageGroups[ndx].push_back(ip);
	}
	// tracks which vartable, vardatabase and eqndatabase to use - 0 for non-hybrids
	// pagegroup
	ip->ndxHybrid = ndx;
	// pages
	for (size_t i = 0; i < ip->Pages.size(); i++)
		for (size_t j = 0; j < ip->Pages[i].size(); j++)
			ip->Pages[i][j].ndxHybrid = ndx;
	//exclusive header pages e.g. Utility Rate - Enable
	for (size_t i = 0; i < ip->ExclusiveHeaderPages.size(); i++)
			ip->ExclusiveHeaderPages[i].ndxHybrid = ndx;

}

void ConfigDatabase::CachePagesInConfiguration( std::vector<PageInfo> &Pages, ConfigInfo *ci, size_t ndx )
{
	for( size_t i=0;i<Pages.size();i++ )
	{
		PageInfo &pi = Pages[i];
		if ( InputPageData *ipd = SamApp::InputPages().Lookup( pi.Name ) )
		{
			ci->InputPages[ndx][ pi.Name ] = ipd;

			ci->Equations[ndx].AddDatabase(&ipd->Equations());
			ci->Equations[ndx].Add(ipd->Equations().GetEquations());

			VarDatabase &vars = ipd->Variables();
			for( VarDatabase::iterator it = vars.begin();it != vars.end();++it )	{
				if ( !ci->Variables[ndx].Add(it->first, it->second))	{
					wxMessageBox("Internal error in configuration.\n\n" + ci->TechnologyFullName  + ", " + ci->Financing + "   [ " + pi.Name + " ]\n\n"
						"An error occurred when attempting to instantiate variable: '" + it->first + "'\n"
						"Duplicate variables within a configuration are not allowed.", "sam-engine", wxICON_ERROR|wxOK );
				}
				/*
                // TODO: hybrid additional variables - need general way to handle - in startup.lk
				if (ci->Technology.size() > 1) { // hybrids
					if ((it->first.Lower() == "analysis_period") || (it->first.Lower() == "sales_tax_rate")) { // in financial pages ndx==3
						if (ci->Variables.size() > 1) ci->Variables[0].Add(it->first, it->second);// add to pvwatts
						if (ci->Variables.size() > 2) ci->Variables[1].Add(it->first, it->second);// add to wind
						if (ci->Variables.size() > 3) ci->Variables[2].Add(it->first, it->second);// add to battery
					}
					if (it->first.Lower() == "solar_resource_file") { // in pvwatts pages ndx==0
						if (ci->Variables.size() > 3) ci->Variables[2].Add(it->first, it->second);// add to battery
					}
					// cannot add now that Hybrid Summary page contains variables
					if (it->first.Lower() == "system_capacity") { // use pvwatts initially ndx==0
						if (ndx == 0 && ci->Variables.size() > 3) ci->Variables[3].Add(it->first, it->second);// add to financials
					}
					if (ndx == 0 && it->first.Lower() == "total_installed_cost") { // use pvwatts initially ndx==0
						if (ci->Variables.size() > 3) ci->Variables[3].Add(it->first, it->second);// add to financials
					}
				}
				*/
			}

			if ( pi.Collapsible && !pi.CollapsiblePageVar.IsEmpty() )	{
				VarInfo *vv = ci->AutoVariables[ndx].Lookup(pi.CollapsiblePageVar);
				if( vv == 0 ){
					vv = ci->AutoVariables[ndx].Create(pi.CollapsiblePageVar, VV_NUMBER,
						"Current selection for " + pi.Caption );

					vv->Flags |= VF_COLLAPSIBLE_PANE;
					vv->DefaultValue.Set( pi.CollapsedByDefault ? 0 : 1 );
				}
				else
					wxLogStatus( "AutoVariable error: collapsible page variable already exists in configuration: " + pi.CollapsiblePageVar );

				ci->Variables[ndx].Add(pi.CollapsiblePageVar, vv);
			}
		}
		else
			wxLogStatus("could not find data for referenced input page: " + pi.Name );
	}
}

void ConfigDatabase::RebuildCaches()
{
	for( std::vector<ConfigInfo*>::iterator it0 = m_configList.begin();
		it0 != m_configList.end(); ++it0 )
	{
		ConfigInfo *ci = *it0;
		size_t i;

		// all set to same size in Add method
		for (i=0;i<ci->Variables.size(); i++)
			ci->Variables[i].clear();
		for (i = 0; i < ci->Equations.size(); i++)
			ci->Equations[i].Clear();
		for (i = 0; i < ci->AutoVariables.size(); i++)
			ci->AutoVariables[i].clear();
		for (i = 0; i < ci->InputPages.size(); i++)
			ci->InputPages[i].clear();

		for (i = 0; i < ci->Technology.size(); i++) {

			for (std::vector<InputPageGroup*>::iterator it1 = ci->InputPageGroups[i].begin();
				it1 != ci->InputPageGroups[i].end(); ++it1)
			{
				InputPageGroup* igrp = *it1;
				for (size_t k = 0; k < igrp->Pages.size(); k++)
					CachePagesInConfiguration(igrp->Pages[k], ci, i);

				if (igrp->OrganizeAsExclusivePages && !igrp->ExclusivePageVar.IsEmpty())
				{
					VarInfo* vv = ci->AutoVariables[i].Lookup(igrp->ExclusivePageVar);
					if (vv == 0)
					{
						vv = ci->AutoVariables[i].Create(igrp->ExclusivePageVar, VV_NUMBER,
							"Current selection for " + igrp->SideBarLabel);

						vv->Flags |= VF_EXCLUSIVE_PAGES;
						vv->DefaultValue.Set(0);
					}
					else
						wxLogStatus("AutoVariable error: exclusive page variable already exists in configuration: " + igrp->ExclusivePageVar);

					ci->Variables[i].Add(igrp->ExclusivePageVar, vv);
				}

				CachePagesInConfiguration(igrp->ExclusiveHeaderPages, ci, i);
			}
		}
		// after variables are initially populated, add any hybrid dependencies, if necessary
		for (i = 0; i < ci->HybridVariables.size(); i++) {
			if (VarInfo* vi = ci->Variables[ci->HybridVariables[i].IndependentVariableVarTable].Lookup(ci->HybridVariables[i].IndependentVariableName))
				ci->Variables[ci->HybridVariables[i].DependentVariableVarTable].Add(ci->HybridVariables[i].DependentVariableName, vi);
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
		if ( m_configList[i]->TechnologyFullName == t
			&& m_configList[i]->Financing == f )
			return m_configList[i];

	return 0;
}

ConfigOptions &ConfigDatabase::Options( const wxString &name )
{
static unordered_map<wxString,ConfigOptions, wxStringHash, wxStringEqual> m_opts;
	return m_opts[name];
}

SamApp::SamApp()
{
}

class SAMThemeProvider : public wxMetroThemeProvider
{
public:
	virtual ~SAMThemeProvider() { }
	virtual wxColour Colour( int id )
	{
		switch( id )
		{
		case wxMT_FOREGROUND: return wxColour( 130,186,0 );
		case wxMT_HOVER: return wxColour( 0, 138, 23 );
		case wxMT_DIMHOVER : return wxColour( 0, 102, 18 );
		default:
			return wxMetroThemeProvider::Colour( id );
		/*
		case wxMT_BACKGROUND:  return *wxWHITE;
		case wxMT_HOVER: return wxColour( 0, 88, 153 );
		case wxMT_DIMHOVER: return wxColour( 0, 107, 186 );
		case wxMT_LIGHTHOVER: return wxColour( 231, 232, 238 );
		case wxMT_ACCENT: return wxColour( 255, 143, 50 );
		case wxMT_TEXT: return wxColour( 135, 135, 135 );
		case wxMT_ACTIVE: return wxColour( 0, 114, 198 );
		case wxMT_SELECT:  return wxColour(193,210,238);
		case wxMT_HIGHLIGHT: return wxColour(224,232,246);
		*/
		}
	}
};


void SamApp::OnFatalException()
{
#ifdef __WXMSW__
	//wxMSWHandleApplicationFatalException();
#endif
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
			SamApp::WebApi("support_email") + "?subject=SAM Beta Feedback" );

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


	wxEasyCurl::Shutdown();

	wxLog::SetActiveTarget( 0 );
	return 0;
}

void SamApp::Restart()
{
	// reload all forms, variables, callbacks, equations
	SamApp::InputPages().Clear();

//	std::chrono::system_clock::time_point start = std::chrono::system_clock::now();
	size_t forms_loaded = 0;
	wxDir dir( SamApp::GetRuntimePath() + "/ui" );
	if ( dir.IsOpened() )
	{
		wxString file;
#ifdef UI_BINARY
		bool has_more = dir.GetFirst(&file, "*.ui", wxDIR_FILES);
#elif defined(__LOAD_AS_JSON__)
		bool has_more = dir.GetFirst(&file, "*.json", wxDIR_FILES);
#else
		bool has_more = dir.GetFirst(&file, "*.txt", wxDIR_FILES);
#endif // UI_BINARY
		while( has_more )
		{
#ifdef UI_BINARY
//			wxLogStatus("loading .ui: " + wxFileName(file).GetName());
			if (!SamApp::InputPages().LoadFile(SamApp::GetRuntimePath() + "/ui/" + file))
				wxLogStatus(" --> error loading .ui for " + wxFileName(file).GetName());
#elif defined(__LOAD_AS_JSON__)
			//			wxLogStatus("loading .json: " + wxFileName(file).GetName());
			if (!SamApp::InputPages().LoadFileJSON(SamApp::GetRuntimePath() + "/ui/" + file))
				wxLogStatus(" --> error loading .json for " + wxFileName(file).GetName());
#else
//			wxLogStatus("loading .txt: " + wxFileName(file).GetName());
			if (!SamApp::InputPages().LoadFileText(SamApp::GetRuntimePath() + "/ui/" + file))
				wxLogStatus(" --> error loading .txt for " + wxFileName(file).GetName());
#endif
			else
				forms_loaded++;

			has_more = dir.GetNext( &file );
		}
	}
	dir.Close();

//	auto end = std::chrono::system_clock::now();
//	auto diff = std::chrono::duration_cast < std::chrono::milliseconds > (end - start).count();
//	wxString ui_time(std::to_string(diff) + "ms ");
//#ifdef UI_BINARY
//	wxLogStatus(wxString::Format(" %d forms loaded as binary in %s", (int)forms_loaded, (const char*)ui_time.c_str()));
//#else
//	wxLogStatus(wxString::Format(" %d forms loaded as text in %s", (int)forms_loaded, (const char*)ui_time.c_str()));
//#endif

	// reload configuration map
	SamApp::Config().Clear();
	wxString startup_script = GetRuntimePath() + "/startup.lk";
	wxLogStatus("loading startup script: " + startup_script );
	wxArrayString errors;
	if ( !LoadAndRunScriptFile( startup_script, &errors ) )
		wxShowTextMessageDialog( "error during startup:\n\n" + wxJoin( errors, '\n' ) );


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
	  {
		wxLogStatus( "error loading metrics.lk" );
	  }

	if ( !g_globalCallbacks.LoadFile( SamApp::GetRuntimePath() + "/cashflow.lk" ))
	  {
		wxLogStatus( "error loading cashflow.lk" );
	  }

	if ( !g_globalCallbacks.LoadFile( SamApp::GetRuntimePath() + "/autographs.lk" ))
	  {
		wxLogStatus( "error loading autographs.lk" );
	  }

	if ( !g_globalCallbacks.LoadFile( SamApp::GetRuntimePath() + "/lossdiag.lk" ))
	  {
		wxLogStatus( "error loading lossdiag.lk" );
	  }

	wxString solar_resource_db = SamApp::GetUserLocalDataDir() + "/SolarResourceData.csv";
	if ( !wxFileExists( solar_resource_db ) ) ScanSolarResourceData( solar_resource_db );
	Library::Load( solar_resource_db );

	wxString wind_resource_db  = SamApp::GetUserLocalDataDir() + "/WindResourceData.csv";
	if ( !wxFileExists( wind_resource_db ) ) ScanWindResourceData( wind_resource_db );
	Library::Load( wind_resource_db );

	wxString wave_resource_db = SamApp::GetUserLocalDataDir() + "/WaveResourceData.csv";
	if (!wxFileExists(wave_resource_db)) ScanWaveResourceData(wave_resource_db);
	Library::Load(wave_resource_db);

    wxString wave_resource_ts_db = SamApp::GetUserLocalDataDir() + "/WaveResourceTSData.csv";
    if (!wxFileExists(wave_resource_ts_db)) ScanWaveResourceTSData(wave_resource_ts_db);
    Library::Load(wave_resource_ts_db);

    wxString tidal_resource_db = SamApp::GetUserLocalDataDir() + "/TidalResourceData.csv";
    if (!wxFileExists(tidal_resource_db)) ScanTidalResourceData(tidal_resource_db);
    Library::Load(tidal_resource_db);
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
	wxFileName path( g_appArgs[0] );
	if ( !path.IsAbsolute() )
		path.MakeAbsolute();

	return wxPathOnly( path.GetFullPath() );
}

wxString SamApp::GetRuntimePath()
{
	wxFileName path( GetAppPath() + "/../runtime/" );
	path.Normalize();
	return path.GetFullPath();
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
#ifdef __WXGTK__
		if ( ! context.IsEmpty() )
			url = "file:///" + fn.GetFullPath( wxPATH_NATIVE ) + context + ".html";
		wxLaunchDefaultBrowser( url );
		return;
#else
		if ( ! context.IsEmpty() )
			url += "?" + context + ".html";
#endif
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
}

int SamApp::RevisionNumber()
{
	int patch = 0;
#if defined(__WXMSW__)
	wxString path = SamApp::GetRuntimePath() + "/patches/patch_msw.txt";
#elif defined(__WXOSX__)
	wxString path = SamApp::GetRuntimePath() + "/patches/patch_osx.txt";
#else
	wxString path = SamApp::GetRuntimePath() + "/patches/patch_lnx.txt";
#endif
	if ( FILE *fp = fopen( (const char*)path.c_str(), "r" ) )
	{
		char buf[32];
		fgets( buf, 31, fp );
		fclose(fp);
		patch = atoi( buf );
	}
	return patch;
}

wxString SamApp::VersionStr( bool with_patches, bool short_style )
{
	wxString vs( wxString::Format("%d.%d.%d", VersionMajor(), VersionMinor(), VersionMicro()) );
	if ( version_label != 0 && strlen(version_label) > 0 )
		vs += "-" + wxString(version_label);

	if ( with_patches )
	{
		int patch = RevisionNumber();
		if ( patch > 0 )
			vs += short_style ? wxString::Format(" r%d", patch )
					: wxString::Format(", updated to Revision %d", patch );
	}

	return vs;
}

int SamApp::VersionMajor() { return releases[0].major; }
int SamApp::VersionMinor() { return releases[0].minor; }
int SamApp::VersionMicro() { return releases[0].micro; }
size_t SamApp::Version( int *maj, int *min, int *mic, int nr )
{
	if ( nr >= NumReleases() ) return 0;
	if( maj ) *maj = releases[nr].major;
	if( min ) *min = releases[nr].minor;
	if( mic ) *mic = releases[nr].micro;
	return VERSION_VALUE( releases[nr].major, releases[nr].minor, releases[nr].micro );
}

size_t sam_version( int *maj, int *min, int *mic )
{
	return SamApp::Version( maj, min, mic, 0 );
}

int SamApp::NumReleases()
{
	int n=0;
	while( releases[n++].major != 0 );
	return n;
}

wxWindow *SamApp::CurrentActiveWindow()
{
	wxWindowList &wl = ::wxTopLevelWindows;
	for( wxWindowList::iterator it = wl.begin(); it != wl.end(); ++it )
		if ( wxTopLevelWindow *tlw = dynamic_cast<wxTopLevelWindow*>( *it ) )
			if ( tlw->IsActive() )
				return tlw;

	return 0;
}


wxString SamApp::ReadProxyFile()
{
	wxString proxy_file = SamApp::GetAppPath() + "/proxy.txt";
	if ( wxFileExists( proxy_file ) )
	{
		if ( FILE *f = fopen(proxy_file.c_str(), "r") )
		{
			char buf[512];
			fgets(buf,511,f);
			fclose(f);
			return wxString::FromAscii(buf).Trim().Trim(false);
		}
	}

	return wxEmptyString;
}

bool SamApp::WriteProxyFile( const wxString &proxy )
{
	wxString proxy_file = SamApp::GetAppPath() + "/proxy.txt";
	if ( FILE *f = fopen(proxy_file.c_str(), "w") )
	{
		fprintf(f, "%s\n", (const char*)proxy.ToAscii() );
		fclose(f);
		return true;
	}
	else
		return false;
}

ConfigDatabase &SamApp::Config() { return g_cfgDatabase; }

bool SamApp::VarTablesFromJSONFile(ConfigInfo* ci, std::vector<VarTable>& vt, const std::string& file)
{
	if (!ci || (vt.size() < 1) || (ci->Technology.size() < 1))
		return false;
	else if (ci->Technology.size() < 2)
		return vt[0].Read_JSON(file);
	else { // hybrid
		rapidjson::Document doc, table;
		wxFileInputStream fis(file);

		if (!fis.IsOk()) {
			wxLogError(wxS("Couldn't open the file '%s'."), file);
			return false;
		}
		wxStringOutputStream os;
		fis.Read(os);

		rapidjson::StringStream is(os.GetString().c_str());

		doc.ParseStream(is);
		if (doc.HasParseError()) {
			wxLogError(wxS("Could not read the json file string conversion '%s'."), file);
			return false;
		}
		else {
			bool ret = true;
			for (size_t i = 0; i < ci->Technology.size(); i++) {
				table.CopyFrom(doc[ci->Technology[i].ToStdString().c_str()], doc.GetAllocator());
				ret = ret && vt[i].Read_JSON(table);
			}
			return ret;
		}
	}
}


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
			for( int i=0;i<parse.error_count();i++ )
				errors->Add( parse.error(i) );
			errors->Add( "parsing did not reach end of input" );
		}
		return false;
	}
	else
	{
		lk::env_t env;
		env.register_funcs( lk::stdlib_basic() );
		env.register_funcs( lk::stdlib_sysio() );
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

std::string SamApp::GetPythonConfigPath(){
    wxFileName path( GetAppPath() + "/../runtime/python" );
    path.Normalize();
    return path.GetFullPath().ToStdString();
}

void SamApp::LoadPythonConfig(){
    pythonConfig = ReadPythonConfig(GetPythonConfigPath() + "/python_config.json");
    if (CheckPythonInstalled(pythonConfig)){
        std::string python_path = GetPythonConfigPath();
		set_python_path(python_path.c_str());
        return;
    }
}

bool SamApp::CheckPythonPackage(const std::string& pip_name){
    if (CheckPythonInstalled(pythonConfig)){
        if (CheckPythonPackageInstalled(pip_name, pythonConfig))
            return true;
    }
    return false;
}

void SamApp::InstallPython() {
    if (pythonConfig.pythonVersion.empty() && pythonConfig.minicondaVersion.empty())
        LoadPythonConfig();

    auto python_path = GetPythonConfigPath();
    // already installed and correctly configured
    if (CheckPythonInstalled(pythonConfig)){
		set_python_path(python_path.c_str());
        return;
    }

#ifdef __WXMSW__
    // windows
    bool errors = InstallPythonWindows(python_path, pythonConfig);
#else
    bool errors = InstallPythonUnix(python_path, pythonConfig);
#endif
    if (errors)
        throw std::runtime_error("Error installing python.");
    LoadPythonConfig();
}

void SamApp::InstallPythonPackage(const std::string& pip_name) {
    if (CheckPythonPackageInstalled(pip_name, pythonConfig))
        return;
    auto packageConfig = ReadPythonPackageConfig(pip_name, GetPythonConfigPath() + "/" + pip_name + ".json");

#ifdef __WXMSW__
	bool retval = InstallFromPipWindows(GetPythonConfigPath() + "\\" + pythonConfig.pipPath, packageConfig);
#else
	std::string pip_exec = GetPythonConfigPath() + "/" + pythonConfig.pipPath;
	bool retval = InstallFromPip(pip_exec, packageConfig);
#endif
    if (retval == 0){
        pythonConfig.packages.push_back(pip_name);
        WritePythonConfig(GetPythonConfigPath() + "/python_config.json", pythonConfig);
    }
    else {
        throw std::runtime_error("Error installing " + pip_name);
    }
}

enum { ID_TechTree = wxID_HIGHEST+98, ID_FinTree };

BEGIN_EVENT_TABLE(ConfigDialog, wxDialog)
	EVT_DATAVIEW_ITEM_COLLAPSING(ID_TechTree, ConfigDialog::OnTechTreeCollapsing)
	EVT_DATAVIEW_ITEM_COLLAPSING(ID_FinTree, ConfigDialog::OnFinTreeCollapsing)
	EVT_DATAVIEW_ITEM_ACTIVATED(ID_FinTree, ConfigDialog::OnFinTreeDoubleClick)
	EVT_DATAVIEW_SELECTION_CHANGED(ID_TechTree, ConfigDialog::OnTechTree)
	EVT_DATAVIEW_SELECTION_CHANGED(ID_FinTree, ConfigDialog::OnFinTree)
	EVT_BUTTON( wxID_HELP, ConfigDialog::OnHelp )
	EVT_BUTTON( wxID_OK, ConfigDialog::OnOk )
	EVT_BUTTON( wxID_CANCEL, ConfigDialog::OnCancel )
	EVT_MENU( wxID_HELP, ConfigDialog::OnHelp )
	EVT_CHAR_HOOK( ConfigDialog::OnCharHook )
END_EVENT_TABLE()

ConfigDialog::ConfigDialog( wxWindow *parent, const wxSize &size )
	: wxDialog( parent, wxID_ANY, wxEmptyString, wxDefaultPosition, size, wxBORDER_NONE
#if defined(__WXOSX__)||defined(__WXGTK__)
	| wxSTAY_ON_TOP  // on OSX/GTK for some reason, we need this for the dialog show up on top of the transparent pane which is the parent
#endif
	)
{
	SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );
	CenterOnParent();

	m_pTech = new wxMetroDataViewTreeCtrl(this, ID_TechTree);
	m_pFin = new wxMetroDataViewTreeCtrl(this, ID_FinTree);

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
//	hbox->Add(m_pChkUseDefaults, 0, wxALL | wxEXPAND | wxALIGN_CENTER_VERTICAL, 4);
	hbox->Add(m_pChkUseDefaults, 0, wxALL | wxEXPAND, 4);
	m_pChkUseDefaults->SetForegroundColour(*wxWHITE);
	hbox->AddStretchSpacer();
	//hbox->Add( CreateButtonSizer( wxOK|wxCANCEL ) );
	hbox->Add( new wxMetroButton(this, wxID_OK, "   OK   "), 0, wxALL, 0 );
	hbox->Add( new wxMetroButton(this, wxID_CANCEL, "Cancel"), 0, wxALL, 0 );

	wxBoxSizer *vbox = new wxBoxSizer( wxVERTICAL );
//	vbox->Add(label, 0, wxALL | wxEXPAND | wxALIGN_CENTER_VERTICAL, 8);
	vbox->Add(label, 0, wxALL | wxEXPAND , 8);
	vbox->Add(choice_sizer, 1, wxALL | wxEXPAND, 0);
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
	int seltech = m_tnames.Index( t );
	if (seltech >= 0)
	{
//		PopulateTech();

		m_techname = m_tnames[seltech];
		wxString L(SamApp::Config().Options(m_tnames[seltech]).LongName);

		wxDataViewItemArray dvia;

		m_pTech->GetModel()->GetChildren(wxDataViewItem(0), dvia);
		bool foundTech = false;
		for (size_t i = 0; (i < dvia.Count()) && !foundTech; i++)
		{
			wxDataViewItem dvi = dvia[i];
			m_pTech->SetCurrentItem(dvi);
			if (m_pTech->IsContainer(dvi))
			{
				m_pTech->GetModel()->GetChildren(dvi, dvia);
				for (size_t ic = 0; (ic < dvia.Count()) && !foundTech; ic++)
				{
					const wxDataViewItem dvic = dvia[ic];
					wxString s = m_pTech->GetItemText(m_pTech->GetCurrentItem());
					if (s == L)
					{
						m_pTech->SetCurrentItem(dvic);
						foundTech = true;
					}
				}
			}
			else
			{
				if (m_pTech->GetItemText(dvi) == L)
				{
					m_pTech->SetCurrentItem(dvi);
					foundTech = true;
				}
			}
		}
		UpdateFinTree();
		int selfin = m_fnames.Index(f);
		if (selfin >= 0)
		{
			m_finname = m_fnames[selfin];
			L = (SamApp::Config().Options(m_fnames[selfin]).LongName);
			bool foundFin = false;

			m_pFin->GetModel()->GetChildren(wxDataViewItem(0), dvia);

			for (size_t i = 0; (i < dvia.Count()) && !foundFin; i++)
			{
				wxDataViewItem dvi = dvia[i];
				if (m_pFin->IsContainer(dvi))
				{
					m_pFin->GetModel()->GetChildren(dvi, dvia);
					for (size_t ic = 0; (ic < dvia.Count()) && !foundFin; ic++)
					{
						wxDataViewItem dvic = dvia[i];
						if (m_pFin->GetItemText(dvic) == L)
						{
							m_pFin->SetCurrentItem(dvic);
							foundFin = true;
						}
					}
				}
				else
				{
					if (m_pFin->GetItemText(dvi) == L)
					{
						m_pFin->SetCurrentItem(dvi);
						foundFin = true;
					}
				}
			}
		}
		m_pTech->Update();
	}
}

void ConfigDialog::ShowResetCheckbox(bool b)
{
	m_pChkUseDefaults->Show(b);
}

void ConfigDialog::GetConfiguration(wxString &t, wxString &f)
{
	t = m_techname;
	f = m_finname;
}

void ConfigDialog::PopulateTech()
{
	// clear tree
	m_pTech->DeleteAllItems();
	// list all technologies
	m_tnames = SamApp::Config().GetTechnologies();

	// tree containers and nodes
	wxArrayString containers, nodes, added;
	for (int j = 0; j < m_tnames.Count(); j++) {
		wxString node(SamApp::Config().Options(m_tnames[j]).LongName);
		wxString container(SamApp::Config().Options(m_tnames[j]).TreeParent);
		if (node.IsEmpty()) node = m_tnames[j];
		if (container != "" && containers.Index(container) == wxNOT_FOUND &&  container != "Retired") 
			containers.Add(container);
		else if (node.Find("Retired") == wxNOT_FOUND)
			nodes.Add(node);
	}

	wxDataViewItemArray dvia{containers.Count()};

	// order from startup.lk configopt("TechnologyTreeOrder", ...
	wxString TreeOrder = SamApp::Config().Options("TechnologyTreeOrder").Description;
    wxArrayString order = wxSplit(TreeOrder, ',');
    for (auto& item : order) {
        if (containers.Index(item) != wxNOT_FOUND)
            dvia[containers.Index(item)] = m_pTech->AppendContainer(wxDataViewItem(0), item);
        else if (nodes.Index(item) != wxNOT_FOUND)
            m_pTech->AppendItem(wxDataViewItem(0), item);
    }
    // append remaining nodes to appropriate containers
    for (auto& item : m_tnames) {
        wxString node(SamApp::Config().Options(item).LongName);
        wxString container(SamApp::Config().Options(item).TreeParent);
        if (node.IsEmpty()) node = item;
    // skip those already added
        if (order.Index(node) != wxNOT_FOUND) continue;
        if (order.Index(container) != wxNOT_FOUND)
            m_pTech->AppendItem(dvia[containers.Index(container)], node);
        else {
            if (added.Index(container) == wxNOT_FOUND && container != "" && container != "Retired") {
                added.Add(container);
                dvia[containers.Index(container)] = m_pTech->AppendContainer(wxDataViewItem(0), container);
            }
            if (container.Find("Retired") != wxNOT_FOUND); //do nothing for Retired technologies
            else if (containers.Index(container) != wxNOT_FOUND) {
                m_pTech->AppendItem(dvia[containers.Index(container)], node);
            }
            else {
                m_pTech->AppendItem(wxDataViewItem(0), node);
            }

        }
	}

}

void ConfigDialog::UpdateFinTree()
{
	m_finDVI = wxDataViewItem(0);
	m_pFin->DeleteAllItems();
	m_fnames = SamApp::Config().GetFinancingForTech(m_techname);
	m_finname = "";

	wxDataViewItem cont_ppa;
	wxDataViewItem cont_dist;
	//wxDataViewItem cont_tpo; //TPO

	for (size_t i = 0; i < m_fnames.Count(); i++) {
		wxString TP(SamApp::Config().Options(m_fnames[i]).TreeParent);
		if (TP.Find("PPA") != wxNOT_FOUND)	{
			cont_ppa = m_pFin->AppendContainer(wxDataViewItem(0), "Power Purchase Agreement");
			break;
		}
	}
	for (size_t i = 0; i < m_fnames.Count(); i++) {
		wxString TP(SamApp::Config().Options(m_fnames[i]).TreeParent);
		if (TP.Find("DISTRIBUTED") != wxNOT_FOUND) {
			cont_dist = m_pFin->AppendContainer(wxDataViewItem(0), "Distributed");
			break;
		}
	}
	/*for (size_t i = 0; i < m_fnames.Count(); i++) //TPO
	{
		wxString TP(SamApp::Config().Options(m_fnames[i]).TreeParent);
		if (TP.Find("TPO") != wxNOT_FOUND)
		{
			cont_tpo = m_pFin->AppendContainer(wxDataViewItem(0), "Third Party Ownership");
			break;
		}
	}*/


	for (size_t i = 0; i < m_fnames.Count(); i++) {
		wxString L(SamApp::Config().Options(m_fnames[i]).LongName);
		if (L.IsEmpty()) L = m_fnames[i];
		wxString TP(SamApp::Config().Options(m_fnames[i]).TreeParent);
		if (TP.Find("PPA") != wxNOT_FOUND)
			m_pFin->AppendItem(cont_ppa, L);
		else if (TP.Find("DISTRIBUTED") != wxNOT_FOUND)
			m_pFin->AppendItem(cont_dist, L);
		/*else if (TP.Find("TPO") != wxNOT_FOUND) //TPO
			m_pFin->AppendItem(cont_tpo, L);*/
		else
			m_pFin->AppendItem(wxDataViewItem(0), L);
	}
}



void ConfigDialog::OnTechTreeCollapsing(wxDataViewEvent& evt)
{
	wxDataViewItem dvi = evt.GetItem();
	if (dvi.IsOk() && m_pTech->IsContainer(dvi)) {
		auto selectedDVI = m_pTech->GetCurrentItem();
		if (selectedDVI.IsOk()) {
			for (size_t i = 0; i < m_pTech->GetChildCount(dvi); i++) {
				if (selectedDVI == m_pTech->GetNthChild(dvi, i))
					evt.Veto();
			}
		}
	}
}

void ConfigDialog::OnFinTreeCollapsing(wxDataViewEvent& evt)
{
	wxDataViewItem dvi = evt.GetItem();
	if (dvi.IsOk() && m_pFin->IsContainer(dvi)) {
		auto selectedDVI = m_pFin->GetCurrentItem();
		if (selectedDVI.IsOk()) {
			for (size_t i = 0; i < m_pFin->GetChildCount(dvi); i++) {
				if (selectedDVI == m_pFin->GetNthChild(dvi, i))
					evt.Veto();
			}
		}
	}
}


void ConfigDialog::OnFinTreeDoubleClick(wxDataViewEvent &evt)
{
	if (SamApp::Config().Find(m_techname, m_finname) != NULL)
		EndModal(wxID_OK);
	else
		evt.Veto();
}


void ConfigDialog::OnTechTree(wxDataViewEvent &evt)
{
	wxDataViewItem dvi = evt.GetItem();
	if (!dvi.IsOk())
		return;

	if (m_pTech->IsContainer(dvi))
	{
		if (m_pTech->IsExpanded(dvi))
			m_pTech->Collapse(dvi);
		else
			m_pTech->Expand(dvi);
		if (m_techDVI.IsOk()) {// keep current selection 
			m_pTech->SetCurrentItem(m_techDVI);
			return;
		}
		else {// select first child
			m_techDVI = m_pTech->GetNthChild(dvi, 0);
			m_pTech->SetCurrentItem(m_techDVI);
		}
	}
	else {
		m_techDVI = evt.GetItem();
	}
	wxString title = m_pTech->GetItemText(m_techDVI);
	if (title.empty())
		title = "None";
	m_techname = title;
	for (size_t i = 0; i < m_tnames.Count(); i++) {
		if (SamApp::Config().Options(m_tnames[i]).LongName == m_techname) {
			m_techname = m_tnames[i];
			break;
		}
	}
	//	wxMessageBox(wxString::Format("wxEVT_DATAVIEW_SELECTION_CHANGED, First selected Item: %s", title));
	UpdateFinTree();
}

void ConfigDialog::OnFinTree(wxDataViewEvent &evt)
{
	wxDataViewItem dvi = evt.GetItem();
	if (!dvi.IsOk())
		return;

	if (m_pFin->IsContainer(dvi))
	{
		if (m_pFin->IsExpanded(dvi))
			m_pFin->Collapse(dvi);
		else
			m_pFin->Expand(dvi);
		if (m_finDVI.IsOk()) {// keep current selection 
			m_pFin->SetCurrentItem(m_finDVI);
			return;
		}
		else {// select first child
			m_finDVI = m_pFin->GetNthChild(dvi, 0);
			m_pFin->SetCurrentItem(m_finDVI);
		}
	}
	else {
		m_finDVI = evt.GetItem();
	}
	wxString title = m_pFin->GetItemText(m_finDVI);
	if (title.empty())
		title = "None";
	m_finname = title;
	for (size_t i = 0; i < m_fnames.Count(); i++) {
		if (SamApp::Config().Options(m_fnames[i]).LongName == m_finname) {
			m_finname = m_fnames[i];
			break;
		}
	}

	//	wxMessageBox(wxString::Format("wxEVT_DATAVIEW_SELECTION_CHANGED, First selected Item: %s", title));
}

void ConfigDialog::OnHelp(wxCommandEvent &)
{
	SamApp::ShowHelp( "choose_models" );
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
#ifndef __WXGTK__
	ShowWithEffect( wxSHOW_EFFECT_SLIDE_TO_RIGHT );
	wxYield();
	Refresh();
	SetFocus();
#endif
	return wxDialog::ShowModal();
}

void ConfigDialog::EndModal( int ret )
{
#ifndef __WXGTK__
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

bool ShowConfigurationDialog( wxWindow *parent, wxString *tech, wxString *fin, bool *reset )
{
	if ( parent == 0 ) return false;

	wxWindow *trans = wxCreateTransparentOverlay( parent );
	wxPoint pt( trans->GetPosition() );
	wxSize size( trans->GetClientSize() );

	ConfigDialog *dlg = new ConfigDialog( trans );
	dlg->SetPosition( pt );
	dlg->SetClientSize( (int)(700*wxGetScreenHDScale()), size.y );

	if ( reset != 0 ) dlg->ShowResetCheckbox( *reset );
	else dlg->ShowResetCheckbox( false );

	if ( !tech->IsEmpty() && !fin->IsEmpty() )
		dlg->SetConfiguration( *tech, *fin );

	dlg->Raise();

	bool result = false;
	if ( dlg->ShowModal() == wxID_OK )
	{
		dlg->GetConfiguration( *tech, *fin );

		if ( reset != 0 ) *reset = dlg->ResetToDefaults();

		result = true;
	}

	// config dialog is child of trans so will be destroyed too.
	trans->Destroy();
	return result;
}


IMPLEMENT_APP( SamApp );

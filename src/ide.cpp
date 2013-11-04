#include <wx/splitter.h>
#include <wx/busyinfo.h>
#include <wx/wfstream.h>
#include <wx/datstrm.h>
#include <wx/dir.h>
#include <wx/filename.h>

#include <wex/lkscript.h>
#include <wex/metro.h>
#include <wex/uiform.h>

#include "ide.h"
#include "main.h"

static IDEWindow *g_ideWin=0;

void ShowIDEWindow()
{
	if ( !g_ideWin )
		g_ideWin = new IDEWindow( SamApp::Window() );

	g_ideWin->Show();
	g_ideWin->Raise();
}

enum { ID_STARTUP_EDITOR = wxID_HIGHEST+124,
	ID_STARTUP_SAVE,
	ID_STARTUP_FIND,
	ID_STARTUP_HELP,
	ID_STARTUP_RESTART,
	ID_FORM_LIST,
	ID_FORM_LIST_REFRESH,
	ID_FORM_ADD,
	ID_FORM_SAVE,
	ID_FORM_DELETE,
	ID_FORM_PROPERTIES
};

BEGIN_EVENT_TABLE( IDEWindow, wxFrame )
	EVT_BUTTON( ID_STARTUP_SAVE, IDEWindow::OnCommand )
	EVT_BUTTON( ID_STARTUP_FIND, IDEWindow::OnCommand )
	EVT_BUTTON( ID_STARTUP_HELP, IDEWindow::OnCommand )
	EVT_BUTTON( ID_STARTUP_RESTART, IDEWindow::OnCommand )
	EVT_LISTBOX( ID_FORM_LIST, IDEWindow::OnCommand )
	EVT_BUTTON( ID_FORM_LIST_REFRESH, IDEWindow::OnCommand )
	EVT_BUTTON( ID_FORM_ADD, IDEWindow::OnCommand )
	EVT_BUTTON( ID_FORM_SAVE, IDEWindow::OnCommand )
	EVT_BUTTON( ID_FORM_DELETE, IDEWindow::OnCommand )
	EVT_BUTTON( ID_FORM_PROPERTIES, IDEWindow::OnCommand )
	EVT_CLOSE( IDEWindow::OnClose )
END_EVENT_TABLE()

extern lk::fcall_t *startup_funcs();

IDEWindow::IDEWindow( wxWindow *parent )
	: wxFrame( parent, wxID_ANY, "sam-ide", wxDefaultPosition, wxSize(900, 700))
{
	wxUIObjectTypeProvider::RegisterBuiltinTypes();

	m_notebook = new wxMetroNotebook( this, wxID_ANY );

	wxPanel *startup_panel = new wxPanel( m_notebook );	
	wxBoxSizer *sz_startup_tools = new wxBoxSizer( wxHORIZONTAL );
	sz_startup_tools->Add( new wxButton( startup_panel, ID_STARTUP_SAVE, "Save"), 0, wxALL|wxEXPAND, 2 );
	sz_startup_tools->Add( new wxButton( startup_panel, ID_STARTUP_FIND, "Find..."), 0, wxALL|wxEXPAND, 2 );
	sz_startup_tools->Add( new wxButton( startup_panel, ID_STARTUP_HELP, "Help"), 0, wxALL|wxEXPAND, 2 );
	sz_startup_tools->Add( new wxButton( startup_panel, ID_STARTUP_RESTART, "Restart SAM"), 0, wxALL|wxEXPAND, 2 );	
	m_scriptCtrl = new wxLKScriptCtrl( startup_panel, ID_STARTUP_EDITOR );
	m_scriptCtrl->RegisterLibrary( startup_funcs(), "Startup Functions", 0 );
	wxBoxSizer *sz_startup_main = new wxBoxSizer( wxVERTICAL );
	sz_startup_main->Add( sz_startup_tools, 0, wxALL|wxEXPAND, 2 );
	sz_startup_main->Add( m_scriptCtrl, 1, wxALL|wxEXPAND, 0 );
	startup_panel->SetSizer( sz_startup_main );
	m_notebook->AddPage( startup_panel, "Startup Script" );

	if (!m_scriptCtrl->ReadAscii( SamApp::GetRuntimePath() + "/startup.lk" ) )
		wxMessageBox("Error loading startup.lk");

	wxPanel *form_panel = new wxPanel( m_notebook );
	wxBoxSizer *sz_form_tools = new wxBoxSizer( wxHORIZONTAL );
	sz_form_tools->Add( new wxButton( form_panel, ID_FORM_LIST_REFRESH, "Refresh list"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( form_panel, ID_FORM_ADD, "Add..."), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( form_panel, ID_FORM_SAVE, "Save"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( form_panel, ID_FORM_DELETE, "Delete"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( form_panel, ID_FORM_PROPERTIES, "Properties..."), 0, wxALL|wxEXPAND, 2 );
	
	wxSplitterWindow *split = new wxSplitterWindow( form_panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_LIVE_UPDATE );

	wxPanel *form_panel_left = new wxPanel( split );
	m_uiPropEditor = new wxUIPropertyEditor( form_panel_left, wxID_ANY );
	m_formList = new wxListBox( form_panel_left, ID_FORM_LIST, wxDefaultPosition, wxDefaultSize, 0, 0, wxLB_SINGLE|wxBORDER_NONE );
	wxBoxSizer *sz_forms = new wxBoxSizer( wxVERTICAL );
	sz_forms->Add( m_formList, 1, wxALL|wxEXPAND, 0 );
	sz_forms->Add( m_uiPropEditor, 1, wxALL|wxEXPAND, 0 );
	form_panel_left->SetSizer( sz_forms );
;
	m_uiFormEditor = new wxUIFormDesigner( split, wxID_ANY );

	m_uiFormEditor->SetPropertyEditor( m_uiPropEditor );
	m_uiFormEditor->SetFormData( &m_formData );

	split->SplitVertically( form_panel_left, m_uiFormEditor, 200 );

	wxBoxSizer *sz_form_main = new wxBoxSizer( wxVERTICAL );
	sz_form_main->Add( sz_form_tools, 0, wxALL|wxEXPAND, 2 );
	sz_form_main->Add( split, 1, wxALL|wxEXPAND, 0 );
	form_panel->SetSizer( sz_form_main );

	m_notebook->AddPage( form_panel, "User Interface" );

	LoadFormList();
}

IDEWindow::~IDEWindow()
{
	g_ideWin = 0;
}

void IDEWindow::OnClose( wxCloseEvent &evt )
{
	Hide();
	evt.Veto();
}

void IDEWindow::LoadFormList( const wxString &sel )
{
	m_formList->Freeze();
	m_formList->Clear();

	wxDir dir( SamApp::GetRuntimePath() + "/ui" );
	if ( dir.IsOpened() )
	{
		wxString file;
		bool has_more = dir.GetFirst( &file, "*.ui", wxDIR_FILES  );
		while( has_more )
		{
			wxFileName fn( file );
			m_formList->Append( fn.GetName() );
			has_more = dir.GetNext( &file );
		}
	}


	m_formList->Thaw();
	
	if ( !sel.IsEmpty() )
		m_formList->SetStringSelection( sel );
}

void IDEWindow::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_STARTUP_SAVE:
		{
			wxBusyInfo savemsg( "Writing startup.lk to disk" );
			wxYield();
			wxMilliSleep(300);
			if ( !m_scriptCtrl->WriteAscii( SamApp::GetRuntimePath() + "/startup.lk" ))
				wxMessageBox("Error writing startup.lk to disk" );
			break;
		}
	case ID_STARTUP_FIND:
		m_scriptCtrl->ShowFindReplaceDialog();
		break;
	case ID_STARTUP_HELP:
		m_scriptCtrl->ShowHelpDialog( this );
		break;
	case ID_STARTUP_RESTART:
		{
			if ( m_scriptCtrl->IsModified() 
				&& wxYES == wxMessageBox( "startup.lk is modified. Save first?", "Query", wxYES_NO) )
				m_scriptCtrl->WriteAscii( SamApp::GetRuntimePath() + "/startup.lk" );
		
			wxBusyInfo restartmsg( "Restarting SAM... messages in debug output screen" );
				wxYield();
				wxMilliSleep(300);
			SamApp::Restart();
		}
		break;
	case ID_FORM_LIST:
		if ( !m_formName.IsEmpty() )
			WriteForm( m_formName );

		if (!LoadForm( m_formList->GetStringSelection() ))
			wxMessageBox("error loading form: " + m_formList->GetStringSelection() );
		break;
	case ID_FORM_LIST_REFRESH:
		LoadFormList();
		break;
	case ID_FORM_ADD:
		{
			wxString name = wxGetTextFromUser( "Enter new form name:" );
			if ( wxFileExists(SamApp::GetRuntimePath()  + "/ui/" + name + ".ui" ) )
			{
				wxMessageBox("that form already exists.");
				return;
			}
			m_uiFormEditor->SetFormData( 0 ) ;
			m_formData.DeleteAll();
			m_varData.Clear();
			WriteForm( name );
			if ( !LoadForm( name ) )
				wxMessageBox("error loading form: " + name );
			LoadFormList( name );
		}
		break;
	case ID_FORM_SAVE:
		{
			wxBusyInfo info( "Saving form and variable data: " + m_formName );
			wxYield();
			wxMilliSleep( 300 );
			if (!WriteForm( m_formName ))
				wxMessageBox("error writing form: " + m_formName );
		}
		break;
	}
}

bool IDEWindow::WriteForm( const wxString &name )
{
	wxFFileOutputStream fout( SamApp::GetRuntimePath() + "/ui/" + name + ".ui" );
	if ( fout.IsOk() )
	{
		wxDataOutputStream ddo( fout );
		ddo.Write8( 0x1f ); // code
		ddo.Write8( 1 ); // version
		m_formData.Write( fout );
		ddo.Write8( 0x2f ); // midpoint code
		m_varData.Write( fout );
		ddo.Write8( 0x1f );	
		return true;
	}
	else return false;
}

bool IDEWindow::LoadForm( const wxString &name )
{
	m_uiFormEditor->SetFormData( 0 );
	m_uiFormEditor->Refresh();
	m_formName.Clear();

	wxFFileInputStream fin( SamApp::GetRuntimePath() + "/ui/" + name + ".ui" );
	if ( fin.IsOk() )
	{
		wxDataInputStream ddi( fin );
		wxUint8 code = ddi.Read8();
		ddi.Read8();
		if ( !m_formData.Read( fin ) ) return false;
		ddi.Read8();
		if ( !m_varData.Read( fin ) ) return false;
		if ( ddi.Read8() != code ) return false;

		m_uiFormEditor->SetFormData( &m_formData );
		m_uiFormEditor->Refresh();
		m_formName = name;
		return true;
	}
	else
	{
		wxMessageBox("error loading /ui/" + name + ".ui");
		return false;
	}
}

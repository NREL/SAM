#include <wx/splitter.h>
#include <wx/textctrl.h>
#include <wx/busyinfo.h>

#include <wex/lkscript.h>
#include <wex/metro.h>

#include "main.h"
#include "script.h"

class SamScriptCtrl : public wxLKScriptCtrl
{
	ScriptWindow *m_scriptwin;
public:
	SamScriptCtrl( wxWindow *parent, int id, ScriptWindow *scriptwin )
		: wxLKScriptCtrl( parent, id, wxDefaultPosition, wxDefaultSize,
			(wxLK_STDLIB_BASIC|wxLK_STDLIB_STRING|wxLK_STDLIB_MATH|wxLK_STDLIB_WXUI|wxLK_STDLIB_PLOT|wxLK_STDLIB_MISC|wxLK_STDLIB_BIOS) ),
		 m_scriptwin( scriptwin )
	{
		// register SAM-specific invoke functions here
	}
	
	virtual bool OnEval( int line )
	{
		wxGetApp().Yield( true );
		return true;
	}

	virtual void OnOutput( const wxString &out )
	{
		m_scriptwin->AddOutput( out );
	}
};

enum { ID_SCRIPT = wxID_HIGHEST+494 };

BEGIN_EVENT_TABLE( ScriptWindow, wxFrame )
	EVT_BUTTON( wxID_NEW, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_OPEN, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_SAVE, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_SAVEAS, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_FIND, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_EXECUTE, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_STOP, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_CLOSE, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_HELP, ScriptWindow::OnCommand )
	
	EVT_MENU( wxID_NEW, ScriptWindow::OnCommand )
	EVT_MENU( wxID_OPEN, ScriptWindow::OnCommand )
	EVT_MENU( wxID_SAVE, ScriptWindow::OnCommand )
	EVT_MENU( wxID_FIND, ScriptWindow::OnCommand )
	EVT_MENU( wxID_EXECUTE, ScriptWindow::OnCommand )
	EVT_MENU( wxID_CLOSE, ScriptWindow::OnCommand )
	EVT_MENU( wxID_HELP, ScriptWindow::OnCommand )

	EVT_STC_MODIFIED( ID_SCRIPT, ScriptWindow::OnModified )
	EVT_CLOSE( ScriptWindow::OnClose )
END_EVENT_TABLE()


ScriptWindow::ScriptWindow( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size )
	: wxFrame( parent, id, wxT("untitled"), pos, size ) 
{
#ifdef __WXMSW__
	SetIcon( wxICON( appicon ) );
#endif	
	SetBackgroundColour( wxMetroTheme::Colour(wxMT_FOREGROUND) );
	
#ifdef __WXOSX__
	wxMenu *file = new wxMenu;
	file->Append( wxID_NEW );
	file->AppendSeparator();
	file->Append( wxID_OPEN );
	file->Append( wxID_SAVE );
	file->Append( wxID_SAVEAS );
	file->AppendSeparator();
	file->Append( wxID_EXECUTE );
	file->AppendSeparator();
	file->Append( wxID_CLOSE );

	wxMenuBar *menuBar = new wxMenuBar;
	menuBar->Append( file, wxT("&File") );
	SetMenuBar( menuBar );
#endif

	

	wxBoxSizer *toolbar = new wxBoxSizer( wxHORIZONTAL );
	toolbar->Add( new wxMetroButton( this, wxID_NEW, "New" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( new wxMetroButton( this, wxID_OPEN, "Open" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( new wxMetroButton( this, wxID_SAVE, "Save" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( new wxMetroButton( this, wxID_SAVEAS, "Save as" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( new wxMetroButton( this, wxID_FIND, "Find" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( m_runBtn=new wxMetroButton( this, wxID_EXECUTE, "Run", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( m_stopBtn=new wxMetroButton( this, wxID_STOP, "Stop" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->AddStretchSpacer();
	toolbar->Add( new wxMetroButton( this, wxID_HELP, "Help" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( new wxMetroButton( this, wxID_CLOSE, "Close" ), 0, wxALL|wxEXPAND, 0 );

	m_stopBtn->Hide();

	wxSplitterWindow *split = new wxSplitterWindow( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_LIVE_UPDATE|wxSP_3DSASH|wxBORDER_NONE );

	m_script = new SamScriptCtrl( split, ID_SCRIPT, this );

	m_output = new wxTextCtrl( split, wxID_ANY, wxT("Ready."), wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY|wxBORDER_NONE );
	
	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( toolbar, 0, wxALL|wxEXPAND, 0 );
	sizer->Add( split, 1, wxALL|wxEXPAND, 0 );
	SetSizer( sizer );
	
	split->SetMinimumPaneSize( 100 );
	split->SplitHorizontally( m_script, m_output, -150 );
	split->SetSashGravity( 1.0 );
		
	std::vector<wxAcceleratorEntry> entries;
	entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 'n', wxID_NEW ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 'o', wxID_OPEN ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 's', wxID_SAVE ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 'f', wxID_FIND ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 'w', wxID_CLOSE ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_NORMAL, WXK_F1, wxID_HELP ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_NORMAL, WXK_F5, wxID_EXECUTE ) );
	SetAcceleratorTable( wxAcceleratorTable( entries.size(), &entries[0] ) );

	UpdateWindowTitle();
}


ScriptWindow *ScriptWindow::CreateNewWindow( bool show )
{
	ScriptWindow *sw = new ScriptWindow( SamApp::Window(), wxID_ANY, wxDefaultPosition, wxSize( 760, 800 ) );
	sw->Show( show );
	return sw;
}

void ScriptWindow::OpenFiles( ScriptWindow *current )
{
	wxWindow *parent = current;
	if ( parent == 0 ) parent = SamApp::Window();

	wxFileDialog dlg( parent, "Open script", 
		wxEmptyString, wxEmptyString, "SAM Script Files (*.lk)|*.lk", wxFD_OPEN|wxFD_MULTIPLE );
	if ( wxID_OK == dlg.ShowModal() )
	{
		wxArrayString files;
		dlg.GetPaths( files );
		for( size_t i=0;i<files.size();i++ )
		{
			if ( ScriptWindow *sw = FindOpenFile( files[i] ) )
			{
				sw->Raise();
				sw->SetFocus();
				continue;
			}

			if ( wxFileExists( files[i] ) )
			{
				ScriptWindow *sw = ( current != 0 && current->GetFileName().IsEmpty() ) ? current : CreateNewWindow( false );
				if ( !sw->Load( files[i] ) )
				{
					wxMessageBox( "Failed to load script.\n\n" + files[i] );
					if ( sw != current ) sw->Destroy();
				}
				else
					sw->Show();
			}
		}
	}
}



std::vector<ScriptWindow*> ScriptWindow::GetWindows()
{
	std::vector<ScriptWindow*> list;
	wxFrame *top = SamApp::Window();
	wxWindowList &wl = top->GetChildren();
	for( wxWindowList::iterator it = wl.begin();
		it != wl.end();
		++it )
		if ( ScriptWindow *scrip = dynamic_cast<ScriptWindow*>( *it ) )
			list.push_back( scrip );

	return list;
}

bool ScriptWindow::CloseAll()
{
	bool closed = true;
	std::vector<ScriptWindow*> list = GetWindows();
	for( size_t i=0;i<list.size();i++ )
		if ( !list[i]->Close() )
			closed = false;

	return closed;
}

ScriptWindow *ScriptWindow::FindOpenFile( const wxString &file )
{
	std::vector<ScriptWindow*> list = GetWindows();
	for( size_t i=0;i<list.size();i++ )
		if ( list[i]->GetFileName() == file )
			return list[i];

	return 0;
}

void ScriptWindow::AddOutput( const wxString &out )
{
	m_output->AppendText( out );
}

void ScriptWindow::ClearOutput()
{
	m_output->Clear();
}

bool ScriptWindow::Save()
{
	if ( m_fileName.IsEmpty() )
		return SaveAs();
	else
		return Write( m_fileName );
}

bool ScriptWindow::SaveAs()
{
	wxFileDialog dlg( this, "Save as...",
		wxPathOnly(m_fileName),
		wxFileNameFromPath(m_fileName),
		"SAM Script Files (*.lk)|*.lk", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
	if (dlg.ShowModal() == wxID_OK)
		return Write( dlg.GetPath() );
	else
		return false;
}

bool ScriptWindow::Load( const wxString &file )
{
	if( m_script->ReadAscii( file ) )
	{
		m_fileName = file;
		UpdateWindowTitle();
		return true;
	}
	else
		return false;
}

bool ScriptWindow::Write( const wxString &file )
{
	wxBusyInfo info( "Saving: " + file, this );
	wxMilliSleep( 120 );

	if ( m_script->WriteAscii( file ) )
	{
		m_fileName = file;
		UpdateWindowTitle();
		return true;
	}
	else return false;
}

void ScriptWindow::UpdateWindowTitle()
{
	wxString title( m_fileName );
	if ( title.IsEmpty() ) title = "untitled";
	if ( m_script->IsModified() ) title += " *";
	if ( m_lastTitle != title )
	{
		SetTitle( title );
		m_lastTitle = title;
	}
}

wxString ScriptWindow::GetFileName()
{
	return m_fileName;
}

void ScriptWindow::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case wxID_NEW:
		CreateNewWindow();
		break;

	case wxID_OPEN:
		OpenFiles( this );
		break;

	case wxID_SAVE:
		Save();
		break;

	case wxID_SAVEAS:
		SaveAs();
		break;

	case wxID_FIND:
		m_script->ShowFindReplaceDialog();
		break;

	case wxID_EXECUTE:
		m_output->Clear();
		m_runBtn->Hide();
		m_stopBtn->Show();
		Layout();
		wxGetApp().Yield( true );
		m_script->Execute( wxPathOnly(m_fileName), SamApp::Window() );
		m_stopBtn->Hide();
		m_runBtn->Show();
		Layout();
		break;

	case wxID_HELP:
		m_script->ShowHelpDialog();
		break;

	case wxID_STOP:
		m_script->Stop();
		break;

	case wxID_CLOSE:
		Close();
		break;
	};
}

void ScriptWindow::OnModified( wxStyledTextEvent & )
{
	UpdateWindowTitle();
}

void ScriptWindow::OnClose( wxCloseEvent &evt )
{	
	if ( m_script->IsModified() )
	{
		Raise();
		int ret = wxMessageBox("The script '" + m_fileName + "' has been modified.  Save changes?", "Query", 
			wxICON_EXCLAMATION|wxYES_NO|wxCANCEL, this );
		if (ret == wxYES)
		{
			Save( );
			if ( m_script->IsModified() ) // if failed to save, cancel
			{
				evt.Veto();
				return;
			}
		}
		else if (ret == wxCANCEL)
		{
			evt.Veto();
			return;
		}
	}

	Destroy();
}
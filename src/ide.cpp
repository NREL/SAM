#include <wx/splitter.h>

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



static bool LoadAscii( wxLKScriptCtrl *editor, const wxString &file )
{
	FILE *fp = fopen(file.c_str(), "r");
	if ( fp )
	{
		wxString str;
		char buf[1024];
		while(fgets( buf, 1023, fp ) != 0)
			str += buf;

		fclose(fp);
		editor->SetText(str);
		editor->EmptyUndoBuffer();
		editor->SetSavePoint();
		return true;
	}
	else return false;
}

BEGIN_EVENT_TABLE( IDEWindow, wxFrame )
	EVT_CLOSE( IDEWindow::OnClose )
END_EVENT_TABLE()

extern lk::fcall_t *startup_funcs();

IDEWindow::IDEWindow( wxWindow *parent )
	: wxFrame( parent, wxID_ANY, "sam-ide", wxDefaultPosition, wxSize(900, 700))
{
	wxUIObjectTypeProvider::RegisterBuiltinTypes();

	m_notebook = new wxMetroNotebook( this, wxID_ANY );

	m_scriptCtrl = new wxLKScriptCtrl( m_notebook );
	m_scriptCtrl->RegisterLibrary( startup_funcs(), "Startup Functions", 0 );
	m_notebook->AddPage( m_scriptCtrl, "Startup Script" );

	LoadAscii( m_scriptCtrl, SamApp::GetRuntimePath() + "/startup.lk" );

	wxSplitterWindow *split = new wxSplitterWindow( m_notebook, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_LIVE_UPDATE );
	m_uiPropEditor = new wxUIPropertyEditor( split, wxID_ANY );
	m_uiFormEditor = new wxUIFormDesigner( split, wxID_ANY );

	m_uiFormEditor->SetPropertyEditor( m_uiPropEditor );
	m_uiFormEditor->SetFormData( &m_formData );

	split->SplitVertically( m_uiPropEditor, m_uiFormEditor, 200 );

	m_notebook->AddPage( split, "User Interface" );
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

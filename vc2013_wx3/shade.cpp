#include <wx/wx.h>
#include <wx/frame.h>
#include <wx/stc/stc.h>
#include <wx/webview.h>

#include <wex/dview/dvplotctrl.h>

#include "../src/simplecurl.h"
#include "../src/s3tool.h"
#include "../src/s3view.h"

static wxArrayString g_appArgs;

class MyFrame : public wxFrame
{
public:
	ShadeTool *m_shade;

	MyFrame() : wxFrame( 0, wxID_ANY, "Shading Tool", wxDefaultPosition, wxSize(900,700) )
	{
		SetIcon( wxICON( appicon ) );
		m_shade = new ShadeTool( this, wxID_ANY );
	}

	void OnClose( wxCloseEvent &evt )
	{
		/*if ( wxNO == wxMessageBox("Are you sure about quitting?", "Query", wxYES_NO, this ) )
		{
			evt.Veto();
			return;
		}*/
		
		Destroy();
	}

	DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE( MyFrame, wxFrame )
	EVT_CLOSE( MyFrame::OnClose )
END_EVENT_TABLE()

class MyApp : public wxApp
{
	wxLocale m_locale;
public:
	bool OnInit()
	{
		wxInitAllImageHandlers();
		wxSimpleCurlInit();
		
		if ( !wxApp::OnInit() )
			return false;

		m_locale.Init();

		SetAppName( "SAM" );
		SetVendorName( "NREL" );

		for( int i=0;i<argc;i++ )
			g_appArgs.Add( argv[i] );

		if ( g_appArgs.Count() < 1 )
		{
			wxMessageBox("Internal error - cannot determine runtime folder from startup argument 0" );
			return false;
		}

		//wxMessageBox( wxT("Hello, \x01dc\x03AE\x03AA\x00C7\x00D6\x018C\x01dd in wxWidgets 3.0!") );

		
		MyFrame *frame = new MyFrame;
		frame->Show();

		if ( g_appArgs.Count() > 1 )
			frame->m_shade->LoadFromFile( g_appArgs[1] );

	
		return true;
	}
};

IMPLEMENT_APP( MyApp );

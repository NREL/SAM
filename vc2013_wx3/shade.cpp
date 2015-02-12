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

	MyFrame() : wxFrame( 0, wxID_ANY, "SAM Shade Calculator (2015.2.12)", wxDefaultPosition, 
		wxSize(1100,700) )
	{
		SetIcon( wxICON( appicon ) );
		m_shade = new ShadeTool( this, wxID_ANY, wxPathOnly( g_appArgs[0] ) );
		
		std::vector<wxAcceleratorEntry> entries;
		entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 's', wxID_SAVE ) );
		entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 'o', wxID_OPEN ) );
		SetAcceleratorTable( wxAcceleratorTable( entries.size(), &entries[0] ) );
	}

	void OnCommand( wxCommandEvent &evt )
	{
		switch( evt.GetId() )
		{
		case wxID_SAVE: m_shade->Save(); break;
		case wxID_OPEN: m_shade->Load(); break;
		}
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
	EVT_MENU( wxID_SAVE, MyFrame::OnCommand )
	EVT_MENU( wxID_OPEN, MyFrame::OnCommand )
END_EVENT_TABLE()

class MyApp : public wxApp
{
	wxLocale m_locale;
public:
	bool OnInit()
	{
		wxInitAllImageHandlers();
		wxSimpleCurl::Init();
		
		m_locale.Init();

		SetAppName( "SAM Shade Calculator" );
		SetVendorName( "NREL" );

		for( int i=0;i<argc;i++ )
			g_appArgs.Add( argv[i] );

		if ( g_appArgs.Count() < 1 || !wxDirExists( wxPathOnly(g_appArgs[0]) ) )
		{

			wxMessageBox("Startup error - cannot determine application runtime folder from startup argument.\n\n"
				"Try running " + g_appArgs[0] + " by specifying the full path to the executable." );
			return false;
		}

		//wxMessageBox( wxT("Hello, \x01dc\x03AE\x03AA\x00C7\x00D6\x018C\x01dd in wxWidgets 3.0!") );
		
		wxString proxy_file = wxPathOnly(g_appArgs[0]) + "/proxy.txt";
		if ( wxFileExists(proxy_file)  )
		{
			if ( FILE *fp = fopen( proxy_file.c_str(), "r" ) )
			{
				char buf[512];
				fgets( buf, 511, fp );
				fclose(fp);
//				wxSimpleCurl::SetupProxy( wxString::FromAscii( buf ) );
			}
		}

		MyFrame *frame = new MyFrame;
		frame->Show();

		if ( g_appArgs.Count() > 1 )
			if( frame->m_shade->LoadFromFile( g_appArgs[1] ) )
				frame->m_shade->SwitchTo( PG_SCENE );

	
		return true;
	}
};

IMPLEMENT_APP( MyApp );

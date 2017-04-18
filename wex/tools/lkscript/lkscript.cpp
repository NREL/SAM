#include <wx/wx.h>
#include <wx/frame.h>
#include <wx/stc/stc.h>
#include <wx/webview.h>
#include <wx/dynlib.h>

#include <wx/app.h>

#include <vector>
#include <wx/frame.h>
#include <wx/stc/stc.h>
#include <wx/splitter.h>
#include <wx/textctrl.h>
#include <wx/busyinfo.h>
#include <wx/stdpaths.h>

#include <wex/plot/plplot.h>
#include <wex/lkscript.h>
#include <wex/metro.h>
#include <wex/utils.h>


class MyApp : public wxApp
{
public:
	bool OnInit()
	{
		
#ifdef __WXMSW__
		typedef BOOL (WINAPI *SetProcessDPIAware_t)(void); 
		wxDynamicLibrary dllUser32(wxT("user32.dll")); 
		SetProcessDPIAware_t pfnSetProcessDPIAware = 
			(SetProcessDPIAware_t)dllUser32.RawGetSymbol(wxT("SetProcessDPIAware")); 
		if ( pfnSetProcessDPIAware ) 
			pfnSetProcessDPIAware(); 
#endif

		wxArrayString args;
		for( int i=0;i<argc;i++ )
			args.Add( argv[i] );

		bool run = args.Index("-run") >= 0 ;

		wxInitAllImageHandlers();
	
		wxString wexdir;
		if ( wxGetEnv("WEXDIR", &wexdir) )
		{
			if (!wxPLPlot::AddPdfFontDir( wexdir + "/pdffonts" ))
				wxMessageBox("Could not add font dir: " + wexdir + "/pdffonts" );
			if (!wxPLPlot::SetPdfDefaultFont( "ComputerModernSansSerif" ) )
				wxMessageBox("Could not set default pdf font to Computer Modern Sans Serif" );
		}
		
		if ( args.size() > 1 )
		{
			for( size_t i=1;i<args.size();i++ )
			{
				if ( args[i] == "-run" ) continue;

				if ( !wxFileExists( args[i] ) )
				{
					wxMessageBox("The script does not exist:\n\n" + args[i] );
					continue;
				}

				wxLKScriptWindow *sw = wxLKScriptWindow::CreateNewWindow( !run );
				if ( sw->Load( args[i] ) )
				{
			
					if ( run )
					{
						if ( sw->RunScript() ) // if run OK, close window
							sw->Destroy();
						else
						{
							wxMessageBox( "Script error.");
							sw->Show(); // otherwise, show script & errors
						}

					}
					else
						sw->Show();
				}
				else
				{
					wxMessageBox( "Error loading script file:\n\n" + args[i] );
					sw->Destroy();
				}
			}
		}
		else
			wxLKScriptWindow::CreateNewWindow();

		return wxTopLevelWindows.size() > 0 ;
	}
};

IMPLEMENT_APP( MyApp )
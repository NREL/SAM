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


#include "main.h"
#include "ide.h"
#include "script.h"
#include "private.h"
#include "welcome.h"
#include "uiobjects.h"
#include <wx/dynlib.h>
#include <wx/display.h>
#include <wx/webview.h>
#include <wex/metro.h>
#include <wex/easycurl.h>


// application globals

static wxArrayString g_appArgs;
static MainWindow* g_mainWindow = 0;
static wxConfig* g_config = 0;
static ConfigDatabase g_cfgDatabase;
static InputPageDatabase g_uiDatabase;
static wxLogWindow* g_logWindow = 0;
static ScriptDatabase g_globalCallbacks;

void ShowIDEWindow()
{
	if (!g_ideWin)
	{
		g_ideWin = new IDEWindow(SamApp::Window());
	}

	g_ideWin->Show();
	g_ideWin->Raise();
}



enum { ID_BACK = wxID_HIGHEST + 439, ID_BROWSER, ID_HOME, ID_EMAIL_SUPPORT, ID_WEBSITE, ID_FORUM, ID_RELEASE_NOTES, ID_SCRIPT_REFERENCE };


static SamApp::ver releases[] = {
	//intermediate version numbers are required in this list in order for the version upgrade script (versions.lk) to work correctly
	//please clarify the reason for the new version in a comment. Examples: public release, variable changes, internal release, public beta release, etc.
	//the top version should always be the current working version
			{ 2024, 12, 12},	// 2024.12.12 ssc 298 public release
			{ 2024, 11, 14},	// 2024.11.14 ssc 295 beta release for IPH configurations expires 1/31/2025
			{ 2024, 9, 15},		// 2024.09.15 Placeholder version number for updated NT variable names (replace with release version number)		
			{ 2024, 9, 11},		// 2024.09.11 Final Norwich beta		
			{ 2024, 5, 28},		// 2024.05.28 Change trough mass flow rate limits 
			{ 2023, 12, 17 },  // 2023.12.17 ssc 288 release candidate from develop - no expiration first built 12/11/2023, 2023.12.17.r1.ssc.290 3/7/2024, 2023.12.17.r2.ssc.292 8/12/2024 
			{ 2023, 12, 1 },  // 2023.12.1 ssc 287 2023 release candidate from develop expires 12/17/2023
			{ 2023, 10, 31 },  // 2023.10.31 ssc 286 public beta release from develop expires 11/30/2023
			{ 2023, 10, 6 },  // 2023.10.6 ssc 285 beta release from develop with hybrids expires 1/6/2024
			{ 2023, 10, 2 },  // 2023.10.2 ssc 284 beta release from develop with hybrids expires 1/2/2024
			{ 2023, 9, 19 },  // 2023.9.19 ssc 283 beta release from develop with hybrids expires 12/19/2023
			{ 2023, 7, 24 },  // 2023.7.24 Add new molten salt linear fresnel model and IPH
			{ 2022, 11, 21 }, // 2022.11.21 ssc 278 public release and ssc 279 revision 1
			{ 2021, 12, 02 }, // 2021.12.02 ssc 267 public release
			{ 2021, 11, 30 }, // 2021.11.30 ssc 265 release candidate beta expires 11/30/2022
			{ 2021, 11, 27 }, // 2021.11.27 ssc 264 release candidate beta expires 11/27/2022
			{ 2021, 11, 22 }, // 2021.11.22 ssc 263 release candidate beta expires 11/22/2022
			{ 2021, 10, 27 }, // 2021.10.27 ssc 262 Cambium beta expires 10/27/2022
			{ 2021, 9, 10 }, // 2021.9.10 ssc 261 Community Solar Beta expires 9/10/2022
			{ 2021, 9, 7 }, // 2021.9.7 ssc 260 Community Solar Beta expires 9/7/2022
			{ 2021, 7, 27 }, // 2021.7.27 ssc 259 EPRI Beta expires 7/27/2022
			{ 2021, 7, 12 }, // 2021.7.12 ssc 258 EPRI Beta expires 7/12/2022
			{ 2021, 6, 2 }, // 2021.6.2 ssc 257 EPRI Beta expires 6/2/2022
			{ 2021, 4, 19 }, // 2021.4.19 ssc 255 EPRI Beta expires 4/19/2022
			{ 2021, 3, 25 }, // 2021.3.25 ssc 254 EPRI Beta expires 3/25/2022
			{ 2021, 3, 8 }, // 2021.3.8 ssc 253 EPRI Beta expires 3/8/2022
			{ 2021, 2, 24 }, // 2021.2.24 ssc 251 release
			{ 2020, 11, 29 }, // 2020.11.29 ssc 250 release r1 ssc 252 2/25/2021 and r2 ssc 256 on 5/12/2021
			{ 2020, 11, 17 }, // 2020.11.17 ssc 247 beta  - expires 11/17/2021
			{ 2020, 11, 12 }, // 2020.11.12 ssc 246 beta  - expires 11/12/2021
			{ 2020, 11, 5 }, // 2020.11.5 ssc 245 beta for Ty - expires 11/5/2021
			{ 2020, 11, 3 }, // 2020.11.3 ssc 244 beta for the 2021 release - expires 11/3/2021
			{ 2020, 2, 29 }, //2020.2.29 release
			{ 2020, 02, 24 }, //2020.2.24 beta
			{ 2020, 02, 17 }, //VS2019 beta release
			{ 2020, 02, 14 }, //CSP beta release
			{ 2020, 1, 17 }, //Updated Beta for release testing - expires 1/17/2021 ssc version 232
			{ 2020, 1, 14 }, //Updated Beta for release testing - expires 1/14/2021 ssc version 231
			{ 2020, 1, 6 }, //Updated Beta for release testing - expires 1/6/2021 ssc version 230
			{ 2020, 1, 3 }, //Updated Beta for release testing - expires 1/3/2021 ssc version 229
			{ 2019, 12, 31 }, //Updated Beta for release testing - expires 12/31/2020.
			{ 2019, 12, 26 }, //Updated Beta for internal release testing - no expiration.
			{ 2019, 12, 19 }, //Updated Beta for internal release testing - no expiration.
			{ 2019, 12, 16 }, //Updated Beta for internal release testing - no expiration.
			{ 2019, 12, 9 }, //Updated Beta for internal release testing - no expiration.
			{ 2019, 12, 2 }, //Updated Beta for ME and Fuel Cells expires 12/2/2020
			{ 2019, 11, 27 }, //Beta for ME and Fuel Cells expires 11/27/2020
			{ 2019, 11, 11 }, //Beta for ME 11/11/2020
			{ 2019, 10, 14 }, //Beta for MHK and Wind_PRUF ssc 220 expires 10/14/2020
			{ 2019, 10, 7 }, //Beta for MHK ssc 218 expires 10/7/2020
			{ 2019, 10, 4 }, //Beta for MHK ssc 217 expires 10/4/2020
			{ 2019, 9, 26 }, //Beta for MHK ssc 215 expires 9/26/2020
			{ 2019, 7, 15 }, //Beta for Wind PRUF project expires 7/15/2020
			{ 2019, 7, 11 }, //Beta for MHK ssc 211 expires 7/11/2020
			{ 2019, 4, 3 }, //Beta for fuel cells and batteries 4/3/2020
			{ 2019, 3, 4 }, //Beta for fuel cells 3/4/2020
			{ 2019, 1, 21 }, //Beta for fuel cells 1/21/2020
			{ 2018, 12, 20 }, //Beta for fuel cells 12/20/2019
			{ 2018, 11, 29 }, //Beta for fuel cells 11/29/2019
		{ 2018, 11, 11 }, // public Veteran's Day release !
			{ 2018, 11, 8 }, //Release candidate for testing expires 11/8/2019
			{ 2018, 11, 5 }, //Beta version for testing expires 11/5/2019
			{ 2018, 10, 29 }, //Beta version for testing expires 10/29/2019
			{ 2018, 10, 17 }, //Beta version for defaults expires 10/17/2019
			{ 2018, 9, 20 }, //new version number for MPPT upgrades
			{ 2018, 9, 13 }, // Beta for Webinar - expires 9/13/2019.
			{ 2018, 9, 10 }, // Beta for Webinar - expires 9/10/2019.
			{ 2018, 8, 29 }, // Beta for Bifacial - expires 8/29/2019.
			{ 2018, 8, 20 }, // Beta for testing - internal with no expiration.
			{ 2018, 8, 13 }, // Beta for Bifacial - expires 8/13/2019
			{ 2018, 7, 17 }, // Beta for Bifacial - expires 7/17/2019
			{ 2018, 7, 11 }, // Beta for Bifacial - expires 7/11/2019
			{ 2018, 4, 3 }, // Beta for MHK - expires 5/31/2018
			{ 2018, 4, 2 }, // Beta for Southern company - expires 4/2/2019
			{ 2018, 1, 29 }, // Beta release for OEA/OEI
			{ 2018, 1, 3}, // Beta release for Host Developer
		{ 2017, 9, 5 }, // public Labor Day release !
			{ 2017, 8, 28 }, // Beta release candidate - expires 12/30/17
			{ 2017, 8, 18 }, // Beta release - expires 12/30/17
			{ 2017, 8, 11 }, // Beta release - expires 12/30/17
			{ 2017, 7, 28 }, // Beta release - expires 12/30/17
			{ 2017, 5, 15 }, // Beta release - expires 7/31/17
			{ 2017, 5, 11 }, // Beta release - no expiration
			{ 2017, 4, 11 }, // Beta release
			{ 2017, 2, 28 }, // Beta release
			{ 2017, 2, 14 }, // Beta release
		{ 2017, 1, 17 }, // public 'ones and sevens' release !
			{ 2016, 12, 29 }, // Beta release - expires 2/28/17
			{ 2016, 10, 25 }, // Beta release
			{ 2016, 7, 21 }, // Beta release - expires 12/31/16
			{ 2016, 5, 4 }, //dc adjustment factor added, internal release
		{ 2016, 3, 14 }, // public pi-day release!
			{ 2016, 3, 2 }, // Beta release - expires 4/15/16
			{ 2016, 2, 29 }, // internal release
			{ 2016, 2, 26 }, // utility rate changes
			{ 2016, 2, 22 }, // self-shading update
			{ 2016, 2, 19 }, // PV variable changes
			{ 2016, 2, 16 }, // new versioning scheme
			{ 2016, 1, 21 }, // internal release
			{ 2015, 11, 16 }, // utility rate variable changes
			{ 2015, 10, 29 }, // battery model variable changes
			{ 2015, 10, 16 }, // internal release
			{ 2015, 9, 30 }, // internal release
			{ 2015, 9, 9 }, // CSP and net metering changes
			{ 2015, 8, 17 }, // CSP variable changes
		{ 2015, 6, 30 }, // public release
			{ 2015, 5, 27 }, // CSP variable changes
			{ 2015, 4, 10 }, // CSP variable changes
		{ 2015, 1, 30 }, // public release
		{ 2014, 11, 24 }, // public release
		{    0,  0,  0 } };
#pragma once



		class SamLogWindow : public wxLogWindow
		{
		public:
			SamLogWindow() : wxLogWindow(0, "sam-log") {
				GetFrame()->SetPosition(wxPoint(5, 5));
				GetFrame()->SetClientSize(wxScaleSize(1000, 200));
			}
			virtual bool OnFrameClose(wxFrame*) {
				g_logWindow = 0; // clear the global pointer, then delete the frame
				return true;
			}

			static void Setup()
			{
				if (g_logWindow != 0)
					delete g_logWindow;

				g_logWindow = new SamLogWindow;
				wxLog::SetActiveTarget(g_logWindow);
				g_logWindow->Show();
			}
		};




class SplashScreen : public wxDialog
{
	wxString m_message;
public:

	SplashScreen()
		: wxDialog( 0, wxID_ANY, wxEmptyString, wxDefaultPosition,
		wxScaleSize( 515, 385 ), wxBORDER_NONE ),
		m_message( "Starting up...please wait" )
	{
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
		// dc.SetBackground( wxBrush( wxColour(219, 192, 4) ) ); // bright yellowish orange
		// dc.SetBackground( wxBrush( wxColour(2, 152, 152) ) ); // bright teal
		// dc.SetBackground( wxBrush( wxColour(120, 67, 163) ) ); // violet
		// dc.SetBackground( wxBrush( wxColour(191, 38, 96) ) ); // reddish pink
		// dc.SetBackground( wxBrush( wxColour(15,79,34) ) ); // dark forest green
		// dc.SetBackground( wxBrush( wxColour(130,186,0) ) ); // pale lime green
		// dc.SetBackground(wxBrush(wxColour(241, 47, 144))); // hot pink, making development more fun for everyone!
		// dc.SetBackground(wxBrush(wxColour(23, 26, 33))); // dark gray 2017.1.17
		//dc.SetBackground(wxBrush(wxColour(62, 121, 123))); // blue green 2017.9.5
		//dc.SetBackground(wxBrush(wxColour(83, 76, 173))); // dark lavender 2018.10.10
		// dc.SetBackground(wxBrush(wxColour(197, 5, 12))); // Wisconsin Badgers #c5055c = rgb(197, 5, 12) from https://www.rapidtables.com/convert/color/hex-to-rgb.html and https://brand.wisc.edu/web/colors/
		//dc.SetBackground(wxBrush(wxColour(197, 5, 12))); // Wisconsin Badgers #c5055c = rgb(197, 5, 12) from https://www.rapidtables.com/convert/color/hex-to-rgb.html and https://brand.wisc.edu/web/colors/
		// dc.SetBackground(wxBrush(wxColour(255, 117, 24))); // Testing Autumn (Pumpkin) color
		//dc.SetBackground(wxBrush(wxColour(151, 69, 21))); // Burnt Orange from Brian 11/12/2020
		//dc.SetBackground(wxBrush(wxColour(4, 16, 96))); // Navy Blue (Matt's birthday 4/16/96) 11/22/21
		//dc.SetBackground(wxBrush(wxColour(182, 86, 42))); // Thanksgiving color palette https://www.color-hex.com/color-palette/27134
		//dc.SetBackground(wxBrush(wxColour(49, 212, 179))); // 10/31/2023
		dc.SetBackground(wxBrush(wxColour(255, 200, 45))); // 11/1/2024 University of Kansas KU Jayhawks

		dc.Clear();

		double scaleX, scaleY;
		wxDevicePPIToScale( dc.GetPPI(), &scaleX, &scaleY );

		dc.SetBrush( *wxWHITE_BRUSH );
		dc.SetPen( *wxWHITE_PEN );
		dc.DrawRectangle( 0, (int)(height-50*scaleY), width, (int)(50*scaleY) );

		dc.SetTextForeground( *wxWHITE );
		dc.SetFont( wxMetroTheme::Font( wxMT_LIGHT, 30 ) );
		dc.DrawText( "System Advisor Model", wxScalePoint( wxPoint(35, 65), scaleX, scaleY ) );

		dc.SetFont( wxMetroTheme::Font( wxMT_LIGHT, 18 ) );
				dc.DrawText("(Open Source) " + SamApp::VersionStr(), wxScalePoint(wxPoint(35, 135), scaleX, scaleY));
		dc.DrawText( m_message, wxScalePoint( wxPoint(35, 275), scaleX, scaleY) );

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




bool SamApp::OnInit()
{

#ifdef __WXMSW__
	/*wxMSWSetupExceptionHandler(
		wxString("SAM"),
		SamApp::VersionStr(),
		wxString("sam.support@nrel.gov") );
		*/
#endif

	// apd : On windows, make sure process is DPI aware, regardless
	// of whether wxWidgets does this.  ref: http://trac.wxwidgets.org/ticket/16116
	// We don't use built-in icons or AUI, and rather have clean lines and text
	// rather than blurry look, now that UI pages can be made to scale (as of 8/24/2015)
#ifdef __WXMSW__
    typedef BOOL (WINAPI *SetProcessDPIAware_t)(void);
    wxDynamicLibrary dllUser32(wxT("user32.dll"));
    SetProcessDPIAware_t pfnSetProcessDPIAware =
        (SetProcessDPIAware_t)dllUser32.RawGetSymbol(wxT("SetProcessDPIAware"));
    if ( pfnSetProcessDPIAware )
        pfnSetProcessDPIAware();
#endif


	// note: DO NOT CALL wxApp::Init() here, because
	// we want to do our own handling of command line
	// arguments.

//	wxMetroTheme::SetTheme( new SAMThemeProvider );
	// set app and vendor
	SetAppName("");
	SetVendorName("");

#ifdef _DEBUG
	SamLogWindow::Setup();
#endif

	wxLogStatus( "startup version %d.%d.%d with SSC version %d, %s",
		releases[0].major,
		releases[0].minor,
		releases[0].micro,
		ssc_version(),
		ssc_build_info() );

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

	for( int i=0;i<argc;i++ )
		g_appArgs.Add( argv[i] );

	if ( g_appArgs.Count() < 1 || !wxDirExists( wxPathOnly(g_appArgs[0]) ) )
	{
		wxMessageBox("Startup error - cannot determine application runtime folder from startup argument.\n\n"
			"Try running " + g_appArgs[0] + " by specifying the full path to the executable.");
		return false;
	}

	g_config = new wxConfig("SystemAdvisorModel", "NREL");

	wxInitAllImageHandlers();


	wxEasyCurl::Initialize();
	//wxEasyCurl::SetApiKeys( GOOGLE_API_KEY, BING_API_KEY, DEVELOPER_API_KEY );
	wxEasyCurl::SetUrlEscape("<SAMAPIKEY>", wxString(sam_api_key));
	wxEasyCurl::SetUrlEscape("<GEOCODEAPIKEY>", wxString(geocode_api_key));
	wxEasyCurl::SetUrlEscape("<BINGAPIKEY>", wxString(bing_api_key));
	wxEasyCurl::SetUrlEscape("<GOOGLEAPIKEY>", wxString(google_api_key));

	wxEasyCurl::SetUrlEscape("<USEREMAIL>", wxString(user_email));


	wxPLPlot::AddPdfFontDir( GetRuntimePath() + "/pdffonts" );
	wxPLPlot::SetPdfDefaultFont( "ComputerModernSansSerif" );

	wxString proxy = SamApp::ReadProxyFile();
	if ( ! proxy.IsEmpty() )
		wxEasyCurl::SetProxyAddress( proxy );

	SplashScreen splash;
	splash.CenterOnScreen();
	splash.Show();
	splash.Update();
	Yield(true);
	splash.Show();
	splash.SetMessage( "Starting up...please wait" );

	FileHistory().Load( Settings() );

	Restart(); // loads and runs startup scripts, sets up variable databases

	g_mainWindow = new MainWindow();
	SetTopWindow( g_mainWindow );
	g_mainWindow->Show();

	// so that script windows are specialized to SAM, not the base generic one
	SamScriptWindow::SetFactory( new SamScriptWindowFactory );

	bool first_load = true;
	wxString fl_key = wxString::Format("first_load_%d", VersionMajor()*10000+VersionMinor()*100+VersionMicro() );
	Settings().Read(fl_key, &first_load, true);

	if ( first_load )
	{
		// register the first load
		Settings().Write(fl_key, false);

		// enable web update app
		wxConfig cfg("SamUpdate3", "NREL");
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

	if ( argc == 2 )
		g_mainWindow->LoadProject( argv[1] );
	else if (argc == 3)	{
		wxLKScriptWindow* lksw = SamScriptWindow::CreateNewWindow(true);
		if (lksw != NULL) {
			if (lksw->Load(argv[2])) lksw->RunScript();
		}
	}

	LoadPythonConfig();

	return true;
}



class HelpWin : public wxFrame
{
#if defined(__WXMSW__)||defined(__WXOSX__)
	wxWebView *m_webView;
#else
	wxHtmlWindow *m_htmlView;
#endif

	wxString m_aboutHtml;
public:
	HelpWin( wxWindow *parent )
		: wxFrame(parent, wxID_ANY, "System Advisor Model (Open Source) Help", wxDefaultPosition, wxScaleSize(1000, 600))
	{
		CreateAboutHtml();

#ifdef __WXMSW__
		SetIcon( wxICON( appicon ) );
#endif
		SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

#if defined(__WXMSW__)||defined(__WXOSX__)
		m_webView = wxWebView::New( this, ID_BROWSER, ::wxWebViewDefaultURLStr, wxDefaultPosition, wxDefaultSize,
			::wxWebViewBackendDefault, wxBORDER_NONE );
		m_webView->SetPage( m_aboutHtml, "About SAM" );
#else
		m_htmlView = new wxHtmlWindow( this, ID_BROWSER );
		m_htmlView->SetPage( m_aboutHtml );
#endif

		wxBoxSizer *tools = new wxBoxSizer( wxHORIZONTAL );
//#if defined(__WXMSW__)||defined(__WXOSX__)
//		tools->Add( new wxMetroButton( this, ID_BACK, "Back" ), 0, wxALL|wxEXPAND, 0 );
//#endif
		tools->Add( new wxMetroButton( this, ID_HOME, "Home" ), 0, wxALL|wxEXPAND, 0 );
//		tools->Add( new wxMetroButton( this, ID_WEBSITE, "Web site" ), 0, wxALL|wxEXPAND, 0 );
//		tools->Add( new wxMetroButton( this, ID_FORUM, "Forum" ), 0, wxALL|wxEXPAND, 0 );
//		tools->Add( new wxMetroButton( this, ID_EMAIL_SUPPORT, "Email support" ), 0, wxALL|wxEXPAND, 0 );
//		tools->Add( new wxMetroButton( this, ID_RELEASE_NOTES, "Release notes" ), 0, wxALL|wxEXPAND, 0 );
//		tools->Add( new wxMetroButton( this, ID_SCRIPT_REFERENCE, "Scripting reference" ), 0, wxALL|wxEXPAND, 0 );
		tools->AddStretchSpacer();
		tools->Add( new wxMetroButton( this, wxID_ABOUT, "About" ), 0, wxALL|wxEXPAND, 0 );
		tools->Add( new wxMetroButton( this, wxID_CLOSE, "Close" ), 0, wxALL|wxEXPAND, 0 );

		wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
		sizer->Add( tools, 0, wxALL|wxEXPAND, 0 );
#if defined(__WXMSW__)||defined(__WXOSX__)
		sizer->Add( m_webView, 1, wxALL|wxEXPAND, 0 );
#else
		sizer->Add( m_htmlView, 1, wxALL|wxEXPAND, 0 );
#endif
		SetSizer( sizer );
	}

	void CreateAboutHtml()
	{

		wxString proxy( wxEasyCurl::GetProxyForURL( SamApp::WebApi("website") ));
		if ( proxy.IsEmpty() ) proxy = "default";
		else proxy = "proxy: " + proxy;

		int patch = SamApp::RevisionNumber();
		wxString patchStr;
		if ( patch > 0 )
			patchStr.Printf( ", updated to revision %d", patch );

		// int nbit = (sizeof(void*) == 8) ? 64 : 32;
		m_aboutHtml = "<html><body bgcolor=#ffffff>"
			"<font color=#a9a9a9 face=\"Segoe UI Light\" size=10>System Advisor Model (Open Source)</font><br><p>"
			"BSD 3-Clause License<br><br>Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/SAM/blob/develop/LICENSE <br>	All rights reserved. <br>"

			"Redistribution and use in source and binary forms, with or without	modification, are permitted provided that the following conditions are met :<br><br>"

			"1. Redistributions of source code must retain the above copyright notice, this	list of conditions and the following disclaimer.<br><br>"

			"2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation	and /or other materials provided with the distribution.<br><br>"

			"3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.<br><br>"

			"THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,	OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
			"</p></font></body></html>";
	}
	void LoadPage( wxString url )
	{
		if ( url == ":about" )
		{
#if defined(__WXMSW__)||defined(__WXOSX__)
			m_webView->SetPage( m_aboutHtml, "About SAM" );
#else
			m_htmlView->SetPage( m_aboutHtml );
#endif

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
			url = SamApp::WebApi( "website" ) + "/forum.html";
		else if ( url == ":website" )
			url = SamApp::WebApi( "website" );

#if defined(__WXMSW__)||defined(__WXOSX__)
		m_webView->LoadURL( url );
#else
		wxLaunchDefaultBrowser( url );
#endif
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
#if defined(__WXMSW__)||defined(__WXOSX__)
			if ( m_webView->CanGoBack() ) m_webView->GoBack();
#endif
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

#if defined(__WXMSW__)||defined(__WXOSX__)
	void OnNewWindow( wxWebViewEvent &evt )
	{
		wxLaunchDefaultBrowser( evt.GetURL() );
	}
#endif

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
#if defined(__WXMSW__)||defined(__WXOSX__)
	EVT_WEBVIEW_NEWWINDOW( ID_BROWSER, HelpWin::OnNewWindow )
#endif
	EVT_CLOSE( HelpWin::OnClose )
END_EVENT_TABLE()


class HelpWin;
static HelpWin* gs_helpWin = 0;


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
	typedef BOOL(WINAPI* SetProcessDPIAware_t)(void);
	wxDynamicLibrary dllUser32(wxT("user32.dll"));
	SetProcessDPIAware_t pfnSetProcessDPIAware =
		(SetProcessDPIAware_t)dllUser32.RawGetSymbol(wxT("SetProcessDPIAware"));
	if (pfnSetProcessDPIAware)
		pfnSetProcessDPIAware();
#endif


	// note: DO NOT CALL wxApp::Init() here, because
	// we want to do our own handling of command line
	// arguments.

//	wxMetroTheme::SetTheme( new SAMThemeProvider );
	// set app and vendow
	SetAppName("");
	SetVendorName("");

#ifdef _DEBUG
	SamLogWindow::Setup();
#endif

	wxLogStatus("startup version %d.%d.%d with SSC version %d, %s",
		releases[0].major,
		releases[0].minor,
		releases[0].micro,
		ssc_version(),
		ssc_build_info());

	// register all the object types that can
	// be read or written to streams.
	ObjectTypes::Register(new StringHash);
	ObjectTypes::Register(new Case);

	// register all input page UI objects
	wxUIObjectTypeProvider::RegisterBuiltinTypes();
	RegisterUIObjectsForSAM();

	// register standard sam report objects for report generation
	extern void RegisterReportObjectTypes();
	RegisterReportObjectTypes();

	for (int i = 0; i < argc; i++)
		g_appArgs.Add(argv[i]);

	if (g_appArgs.Count() < 1 || !wxDirExists(wxPathOnly(g_appArgs[0])))
	{
		wxMessageBox("Startup error - cannot determine application runtime folder from startup argument.\n\n"
			"Try running " + g_appArgs[0] + " by specifying the full path to the executable.");
		return false;
	}

	g_config = new wxConfig("SystemAdvisorModel", "NREL");

	wxInitAllImageHandlers();


	wxEasyCurl::Initialize();
	wxEasyCurl::SetApiKeys(GOOGLE_API_KEY, BING_API_KEY, DEVELOPER_API_KEY);
	wxEasyCurl::SetUrlEscape("<SAMAPIKEY>", wxString(sam_api_key));

	wxPLPlot::AddPdfFontDir(GetRuntimePath() + "/pdffonts");
	wxPLPlot::SetPdfDefaultFont("ComputerModernSansSerif");

	wxString proxy = SamApp::ReadProxyFile();
	if (!proxy.IsEmpty())
		wxEasyCurl::SetProxyAddress(proxy);

	SplashScreen splash;
	splash.CenterOnScreen();
	splash.Show();
	splash.Update();
	Yield(true);
	splash.Show();
	splash.SetMessage("Starting up...please wait");

	FileHistory().Load(Settings());

	Restart(); // loads and runs startup scripts, sets up variable databases

	g_mainWindow = new MainWindow();
	SetTopWindow(g_mainWindow);
	g_mainWindow->Show();

	// so that script windows are specialized to SAM, not the base generic one
	SamScriptWindow::SetFactory(new SamScriptWindowFactory);

	bool first_load = true;
	wxString fl_key = wxString::Format("first_load_%d", VersionMajor() * 10000 + VersionMinor() * 100 + VersionMicro());
	Settings().Read(fl_key, &first_load, true);

	if (first_load)
	{
		// register the first load
		Settings().Write(fl_key, false);

		// enable web update app
		wxConfig cfg("SamUpdate3", "NREL");
		cfg.Write("allow_web_updates", true);

		// after installing a new version, always show the reminders again until the user turns them off
		Settings().Write("show_reminder", true);
	}
	else
	{
		// restore window position
		bool b_maximize = false;
		int f_x, f_y, f_width, f_height;
		Settings().Read("window_x", &f_x, -1);
		Settings().Read("window_y", &f_y, -1);
		Settings().Read("window_width", &f_width, -1);
		Settings().Read("window_height", &f_height, -1);
		Settings().Read("window_maximized", &b_maximize, false);

		if (b_maximize)
			g_mainWindow->Maximize();
		else
		{
			if (wxDisplay::GetFromPoint(wxPoint(f_x, f_y)) != wxNOT_FOUND)
			{
				if (f_width > 100 && f_height > 100)
					g_mainWindow->SetClientSize(f_width, f_height);

				if (f_x > 0 && f_y > 0)
					g_mainWindow->SetPosition(wxPoint(f_x, f_y));
			}
			else // place default here...
				g_mainWindow->Maximize();
		}
	}

	if (argc == 2)
		g_mainWindow->LoadProject(argv[1]);
	else if (argc == 3) {
		wxLKScriptWindow* lksw = SamScriptWindow::CreateNewWindow(true);
		if (lksw != NULL) {
			if (lksw->Load(argv[2])) lksw->RunScript();
		}
	}

	LoadPythonConfig();

	return true;
}


class SplashScreen : public wxDialog
{
	wxString m_message;
public:

	SplashScreen()
		: wxDialog(0, wxID_ANY, wxEmptyString, wxDefaultPosition,
			wxScaleSize(515, 385), wxBORDER_NONE),
		m_message("Starting up...please wait")
	{
	}

	void SetMessage(const wxString& msg)
	{
		m_message = msg;
		Refresh();
		Update();
		wxGetApp().Yield(true);
	}

	void OnPaint(wxPaintEvent&)
	{
		wxPaintDC dc(this);

		int width, height;
		GetClientSize(&width, &height);


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
		dc.SetBackground(wxBrush(wxColour(4, 16, 96))); // Navy Blue (Matt's birthday 4/16/96) 11/22/21

		dc.Clear();

		double scaleX, scaleY;
		wxDevicePPIToScale(dc.GetPPI(), &scaleX, &scaleY);

		dc.SetBrush(*wxWHITE_BRUSH);
		dc.SetPen(*wxWHITE_PEN);
		dc.DrawRectangle(0, (int)(height - 50 * scaleY), width, (int)(50 * scaleY));

		dc.SetTextForeground(*wxWHITE);
		dc.SetFont(wxMetroTheme::Font(wxMT_LIGHT, 30));
		dc.DrawText("System Advisor Model", wxScalePoint(wxPoint(35, 65), scaleX, scaleY));

		dc.SetFont(wxMetroTheme::Font(wxMT_LIGHT, 18));
		dc.DrawText("(Open Source) " + SamApp::VersionStr(), wxScalePoint(wxPoint(35, 135), scaleX, scaleY));
		dc.DrawText(m_message, wxScalePoint(wxPoint(35, 275), scaleX, scaleY));

	}

	void OnSize(wxSizeEvent&)
	{
		Refresh();
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(SplashScreen, wxDialog)
EVT_PAINT(SplashScreen::OnPaint)
EVT_SIZE(SplashScreen::OnSize)
END_EVENT_TABLE()



class HelpWin;
static HelpWin* gs_helpWin = 0;

enum { ID_BACK = wxID_HIGHEST + 439, ID_BROWSER, ID_HOME, ID_EMAIL_SUPPORT, ID_WEBSITE, ID_FORUM, ID_RELEASE_NOTES, ID_SCRIPT_REFERENCE };


class HelpWin : public wxFrame
{
#if defined(__WXMSW__)||defined(__WXOSX__)
	wxWebView* m_webView;
#else
	wxHtmlWindow* m_htmlView;
#endif

	wxString m_aboutHtml;
public:
	HelpWin(wxWindow* parent)
		: wxFrame(parent, wxID_ANY, "System Advisor Model (Open Source) Help", wxDefaultPosition, wxScaleSize(1000, 600))
	{
		CreateAboutHtml();

#ifdef __WXMSW__
		SetIcon(wxICON(appicon));
#endif
		SetBackgroundColour(wxMetroTheme::Colour(wxMT_FOREGROUND));

#if defined(__WXMSW__)||defined(__WXOSX__)
		m_webView = wxWebView::New(this, ID_BROWSER, ::wxWebViewDefaultURLStr, wxDefaultPosition, wxDefaultSize,
			::wxWebViewBackendDefault, wxBORDER_NONE);
		m_webView->SetPage(m_aboutHtml, "About SAM");
#else
		m_htmlView = new wxHtmlWindow(this, ID_BROWSER);
		m_htmlView->SetPage(m_aboutHtml);
#endif

		wxBoxSizer* tools = new wxBoxSizer(wxHORIZONTAL);
#if defined(__WXMSW__)||defined(__WXOSX__)
		tools->Add(new wxMetroButton(this, ID_BACK, "Back"), 0, wxALL | wxEXPAND, 0);
#endif
		tools->Add(new wxMetroButton(this, ID_HOME, "Home"), 0, wxALL | wxEXPAND, 0);
		tools->Add(new wxMetroButton(this, ID_WEBSITE, "Web site"), 0, wxALL | wxEXPAND, 0);
		tools->Add(new wxMetroButton(this, ID_FORUM, "Forum"), 0, wxALL | wxEXPAND, 0);
		tools->Add(new wxMetroButton(this, ID_EMAIL_SUPPORT, "Email support"), 0, wxALL | wxEXPAND, 0);
		tools->Add(new wxMetroButton(this, ID_RELEASE_NOTES, "Release notes"), 0, wxALL | wxEXPAND, 0);
		tools->Add(new wxMetroButton(this, ID_SCRIPT_REFERENCE, "Scripting reference"), 0, wxALL | wxEXPAND, 0);
		tools->AddStretchSpacer();
		tools->Add(new wxMetroButton(this, wxID_ABOUT, "About"), 0, wxALL | wxEXPAND, 0);
		tools->Add(new wxMetroButton(this, wxID_CLOSE, "Close"), 0, wxALL | wxEXPAND, 0);

		wxBoxSizer* sizer = new wxBoxSizer(wxVERTICAL);
		sizer->Add(tools, 0, wxALL | wxEXPAND, 0);
#if defined(__WXMSW__)||defined(__WXOSX__)
		sizer->Add(m_webView, 1, wxALL | wxEXPAND, 0);
#else
		sizer->Add(m_htmlView, 1, wxALL | wxEXPAND, 0);
#endif
		SetSizer(sizer);
	}

	void CreateAboutHtml()
	{

		wxString proxy(wxEasyCurl::GetProxyForURL("https://sam.nrel.gov"));
		if (proxy.IsEmpty()) proxy = "default";
		else proxy = "proxy: " + proxy;

		int patch = SamApp::RevisionNumber();
		wxString patchStr;
		if (patch > 0)
			patchStr.Printf(", updated to revision %d", patch);

		// int nbit = (sizeof(void*) == 8) ? 64 : 32;
		m_aboutHtml = "<html><body bgcolor=#ffffff>"
			"<font color=#a9a9a9 face=\"Segoe UI Light\" size=10>System Advisor Model (Open Source)</font><br><p>"
			"Copyright 2019 Alliance for Sustainable Energy, LLC<br>"

			"Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met : <br><br>"

			"1. Redistributions of source code must retain the above copyright notice, the above government rights notice, this list of conditions and the following disclaimer.<br><br>"

			"2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and /or other materials provided with the distribution.<br><br>"

			"3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.<br><br>"

			"THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ""AS IS"" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
			"</p></font></body></html>";
	}
	void LoadPage(wxString url)
	{
		if (url == ":about")
		{
#if defined(__WXMSW__)||defined(__WXOSX__)
			m_webView->SetPage(m_aboutHtml, "About SAM");
#else
			m_htmlView->SetPage(m_aboutHtml);
#endif

			return;
		}
		else if (url == ":release_notes")
		{
			url = SamApp::WebApi("release_notes");
		}
		else if (url == ":email_support")
		{
			wxLaunchDefaultBrowser(SamApp::WebApi("support_email"));
			return;
		}
		else if (url == ":script_ref")
		{
			wxFileName file(SamApp::GetRuntimePath() + "/help/lk_guide.pdf");
			file.Normalize();
			wxLaunchDefaultBrowser(file.GetFullPath());
			return;
		}
		else if (url == ":forum")
			url = SamApp::WebApi("forum");
		else if (url == ":website")
			url = SamApp::WebApi("website");

#if defined(__WXMSW__)||defined(__WXOSX__)
		m_webView->LoadURL(url);
#else
		wxLaunchDefaultBrowser(url);
#endif
	}


	void OnClose(wxCloseEvent& evt)
	{
		Hide();
		evt.Veto();
	}

	void OnCommand(wxCommandEvent& evt)
	{
		switch (evt.GetId())
		{
		case ID_BACK:
#if defined(__WXMSW__)||defined(__WXOSX__)
			if (m_webView->CanGoBack()) m_webView->GoBack();
#endif
			break;
		case ID_WEBSITE:
			LoadPage(":website");
			break;
		case ID_FORUM:
			LoadPage(":forum");
			break;
		case ID_EMAIL_SUPPORT:
			LoadPage(":email_support");
			break;
		case ID_RELEASE_NOTES:
			LoadPage(":release_notes");
			break;
		case ID_SCRIPT_REFERENCE:
			LoadPage(":script_ref");
			break;
		case ID_HOME:
		{
			wxFileName fn(SamApp::GetRuntimePath() + "/help/html/index.html");
			fn.MakeAbsolute();
			LoadPage("file:///" + fn.GetFullPath());
		}
		break;
		case wxID_ABOUT:
			LoadPage(":about");
			break;
		case wxID_CLOSE:
			Close();
			break;
		}
	}

#if defined(__WXMSW__)||defined(__WXOSX__)
	void OnNewWindow(wxWebViewEvent& evt)
	{
		wxLaunchDefaultBrowser(evt.GetURL());
	}
#endif

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(HelpWin, wxFrame)
EVT_BUTTON(ID_BACK, HelpWin::OnCommand)
EVT_BUTTON(ID_HOME, HelpWin::OnCommand)
EVT_BUTTON(ID_WEBSITE, HelpWin::OnCommand)
EVT_BUTTON(ID_FORUM, HelpWin::OnCommand)
EVT_BUTTON(ID_RELEASE_NOTES, HelpWin::OnCommand)
EVT_BUTTON(ID_SCRIPT_REFERENCE, HelpWin::OnCommand)
EVT_BUTTON(ID_EMAIL_SUPPORT, HelpWin::OnCommand)
EVT_BUTTON(wxID_CLOSE, HelpWin::OnCommand)
EVT_BUTTON(wxID_ABOUT, HelpWin::OnCommand)
#if defined(__WXMSW__)||defined(__WXOSX__)
EVT_WEBVIEW_NEWWINDOW(ID_BROWSER, HelpWin::OnNewWindow)
#endif
EVT_CLOSE(HelpWin::OnClose)
END_EVENT_TABLE()


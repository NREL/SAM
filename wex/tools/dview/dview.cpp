#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <wx/wx.h>
#include <wx/config.h>
#include <wx/scrolbar.h>
#include <wx/print.h>
#include <wx/printdlg.h>
#include <wx/accel.h>
#include <wx/image.h>
#include <wx/fs_zip.h>
#include <wx/html/htmlwin.h>
#include <wx/snglinst.h>
#include <wx/progdlg.h>
#include <wx/busyinfo.h>
#include <wx/dir.h>
#include <wx/stdpaths.h>
#include <wx/generic/helpext.h>
#include <wx/clipbrd.h>
#include <wx/aui/aui.h>
#include <wx/wfstream.h>
#include <wx/txtstrm.h>
#include <wx/cmdline.h>
#include <wx/tokenzr.h>
#include <wx/msgdlg.h>

#include "wex/dview/dvplotctrl.h"
#include "wex/dview/dvfilereader.h"

#include "wex/plot/plplotctrl.h"
#include "wex/plot/pllineplot.h"
#include "wex/plot/plscatterplot.h"

#define MAX_RECENT 25
enum{ 
		ID_RECENT_FILES = wxID_HIGHEST+1233,			
		// up to 100 recent items can be accommodated
		ID_RECENT,
		ID_RECENT_LAST = ID_RECENT+MAX_RECENT,
};

class DViewFrame : public wxFrame
{
private:
	wxDVPlotCtrl *mPlotCtrl;
	int mRecentCount;
	wxString mLastDir;
	wxMenu *mFileMenu, *mRecentMenu;
	wxString mRecentFiles[MAX_RECENT];
	wxArrayString mFileNames;

public:

	DViewFrame()
	 : wxFrame( 0, wxID_ANY, "Data Viewer", wxDefaultPosition, wxSize(800,600) )
	{	
		mRecentCount = 0;

#ifdef __WXMSW__
		SetIcon( wxIcon("appicon") );
#endif

		wxMenuBar *menubar = new wxMenuBar;
		mRecentMenu = new wxMenu;
		mFileMenu = new wxMenu;
		mFileMenu->Append(wxID_OPEN, "Open...\tCtrl-O");
		mFileMenu->Append(wxID_CLEAR, "Clear\tCtrl-W");
		mFileMenu->AppendSeparator();
		mFileMenu->Append(ID_RECENT_FILES, "Recent", mRecentMenu);
	
	#ifndef __WXMAC__
		mFileMenu->AppendSeparator();
		mFileMenu->Append(wxID_EXIT);
	#endif
		menubar->Append(mFileMenu, "&File");
	
		wxMenu *help_menu = new wxMenu;
		help_menu->Append(wxID_ABOUT);
		menubar->Append(help_menu, "&Help");

		SetMenuBar( menubar );
		
		mPlotCtrl = new wxDVPlotCtrl(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, 0 );
		mPlotCtrl->DisplayTabs();
	
		wxConfig cfg( "DView", "NREL" );
		long ct = 0;
		if (cfg.Read("RecentCount", &ct))
			mRecentCount = (int)ct;

		if (mRecentCount > MAX_RECENT)
			mRecentCount = MAX_RECENT;

		for (int i=0;i<mRecentCount;i++)
		{
			wxString key;
			key.Printf("RecentFile_%d", i);
			wxString fn;
			if (cfg.Read(key, &fn))
			{
				fn.Replace("\\","/");
				mRecentFiles[i] = fn;
			}
		}

		cfg.Read("LastDirectory", &mLastDir);

		
		int x, y, width, height;
		bool maximized;

		if (cfg.Read("FrameX", &x)
			&& cfg.Read("FrameY", &y)
			&& cfg.Read("FrameWidth", &width)
			&& cfg.Read("FrameHeight", &height)
			&& cfg.Read("FrameMaximized", &maximized))
		{

			if ( width < 700 ) width = 700;
			if ( height < 450 ) height = 450;

			SetPosition( wxPoint(x, y) );
			SetClientSize( width, height );
			if (maximized)
				Maximize();
		}

		UpdateRecentMenu();
	}
	
	void UpdateRecentMenu()
	{
		int i;
		for (i=0;i<MAX_RECENT;i++)
		{
			if (mRecentMenu->FindItem(ID_RECENT+i) != NULL)
				mRecentMenu->Destroy(ID_RECENT+i);
		}

		for (i=0;i<mRecentCount;i++)
		{
			wxString name;
			name.Printf("%d ", i+1);
			name += mRecentFiles[i];
			mRecentMenu->Append(ID_RECENT+i, name);
		}


		mFileMenu->Enable(ID_RECENT_FILES, mRecentCount > 0);
	}

	void OnRecent(wxCommandEvent &evt)
	{
		int id = evt.GetId() - ID_RECENT;
		if (id < 0 || id >= MAX_RECENT)
			return;

		if ( !mRecentFiles[id].IsEmpty() )
		{
			wxArrayString files;
			files.Add( mRecentFiles[id] );
			Load( files );
		}
	}

	wxArrayString GetRecentFiles()
	{
		wxArrayString list;
		for (int i=0;i<mRecentCount;i++)
			list.Add( mRecentFiles[i] );
		return list;
	}

	void AddRecent(const wxString &fn)
	{
		wxString norm_fn = fn;
		norm_fn.Replace("\\","/");

		int i;
		int index = -1;
		// find the file in the recent list
		for (i=0;i<mRecentCount;i++)
		{
			if (norm_fn == mRecentFiles[i])
			{
				index = i;
				break;
			}
		}

		if (index >= 0)
		{
			// bring this file to the front of the
			// recent file list

			for (i=index;i>0;i--)
				mRecentFiles[i] = mRecentFiles[i-1];
		}
		else // not found in recent list
		{
			// add this to the front of the recent list
			// and increment the recent count if its 
			// less than MAX_RECENT

			for (i=MAX_RECENT-1;i>0;i--)
				mRecentFiles[i] = mRecentFiles[i-1];

			if (mRecentCount < MAX_RECENT)
				mRecentCount++;
		}
	
		mRecentFiles[0] = norm_fn;
		UpdateRecentMenu();
	}

	void RemoveRecent(const wxString &fn)
	{
		wxString norm_fn = fn;
		norm_fn.Replace("\\","/");

		int i;
		int index = -1;
		// find the file in the recent list
		for (i=0;i<mRecentCount;i++)
		{
			if (norm_fn == mRecentFiles[i])
			{
				index = i;
				break;
			}
		}

		if (index >= 0)
		{
			for (i=index;i<MAX_RECENT-1;i++)
				mRecentFiles[i] = mRecentFiles[i+1];

			mRecentCount--;
			UpdateRecentMenu();
		}
	}

	void OnCloseFrame( wxCloseEvent &evt )
	{	

		/* save window position */
		bool b_maximize = this->IsMaximized();
		int f_x,f_y,f_width,f_height;

		this->GetPosition(&f_x,&f_y);
		this->GetClientSize(&f_width, &f_height);
	
		long ct = (long)mRecentCount;

		wxConfig cfg( "DView", "NREL" );
		cfg.Write("RecentCount", ct);
		for (int i=0;i<mRecentCount;i++)
		{
			wxString key;
			key.Printf("RecentFile_%d", i);
			cfg.Write(key, mRecentFiles[i]);
		}

		cfg.Write("LastDirectory", mLastDir);
		cfg.Write("FrameX", f_x);
		cfg.Write("FrameY", f_y);
		cfg.Write("FrameWidth", f_width);
		cfg.Write("FrameHeight", f_height);
		cfg.Write("FrameMaximized", b_maximize);

	
		Destroy();
	}
	
	bool Load(const wxArrayString& filenames)
	{
		bool FileExists = false;

		wxBeginBusyCursor();
		for(size_t i=0; i<filenames.GetCount(); i++)
		{	
			for (size_t j = 0; j < mFileNames.GetCount(); j++)
			{
				if (filenames[i] == mFileNames[j])
				{
					FileExists = true;
					break;
				}
			}

			if (!FileExists)
			{
				if(!wxDVFileReader::FastRead(mPlotCtrl, filenames[i]))
				{
					wxMessageBox( wxT("The selected file is not of the correct format, is corrupt, no longer exists, or you do not have permission to open it."), wxT("Error opening file."), wxICON_ERROR);
					RemoveRecent(filenames[i]);
				}
				else
				{
					AddRecent(filenames[i]);
					mFileNames.Add(filenames[i]);
					mPlotCtrl->DisplayTabs();
				}
			}
		}
		
		UpdateRecentMenu();
		wxEndBusyCursor();
		return true;
	}

	void Open()
	{
		wxFileDialog fdlg(this, "Open Data File", mLastDir, "", "All Files|*.*|CSV Files(*.csv)|*.csv|TXT Files(*.txt)|*.txt|TMY3 Files(*.tmy3)|*.tmy3|EPW Files(*.epw)|*.epw", wxFD_OPEN | wxFD_MULTIPLE);
		wxArrayString myFilePaths;
		if (fdlg.ShowModal() == wxID_OK)
		{
			fdlg.GetPaths(myFilePaths);
			Load(myFilePaths);
		}
	}
	
	void OnCommand(wxCommandEvent &evt)
	{
		switch( evt.GetId() )
		{
		case wxID_OPEN:
			Open();
			break;
		case wxID_CLEAR:
			mPlotCtrl->RemoveAllDataSets();
			mFileNames.Clear();
			break;
		case wxID_ABOUT:
		case wxID_HELP:
			wxMessageBox( wxT("DView (" + wxGetLibraryVersionInfo().GetVersionString() + ") Version " __DATE__) );
			break;
		case wxID_EXIT:
			Close( false );
			break;
		}
	}

	wxDVPlotCtrl *GetPlot()
	{
		return mPlotCtrl;
	}

	DECLARE_EVENT_TABLE();
};


BEGIN_EVENT_TABLE(DViewFrame, wxFrame)

	EVT_MENU( wxID_OPEN,     DViewFrame::OnCommand )
	EVT_MENU( wxID_CLEAR,    DViewFrame::OnCommand )
	EVT_MENU( wxID_EXIT,     DViewFrame::OnCommand )
	EVT_MENU( wxID_ABOUT,    DViewFrame::OnCommand )
	EVT_CLOSE( DViewFrame::OnCloseFrame )
	EVT_MENU_RANGE( ID_RECENT, ID_RECENT+MAX_RECENT, DViewFrame::OnRecent)

END_EVENT_TABLE()


class DViewApp : public wxApp
{
private:
	
	bool m_arg_showLog;
	int m_arg_tab, m_arg_data;
	double m_startHour, m_endHour;
	wxArrayString m_variables;
	wxArrayString m_arg_filenames;
	long m_lineMode;
	double m_ylmin, m_ylmax;
	double m_yrmin, m_yrmax;

public:
	bool OnInit()
	{
		//wxApp::OnInit handles all of our command line argument stuff.
		if (!wxApp::OnInit())
			return false;
		
		::wxInitAllImageHandlers();
		wxFileSystem::AddHandler(new wxZipFSHandler);
			
		DViewFrame *frame = new DViewFrame;
		
		if (m_arg_filenames.Count() > 0)
			frame->Load(m_arg_filenames);

		if (m_arg_tab != -1)
			frame->GetPlot()->SelectTabIndex(m_arg_tab);

		if (m_arg_data != -1)
			frame->GetPlot()->SelectDataIndex(m_arg_data);

		if ( m_startHour >= 0 && m_endHour >= 0 && m_startHour < m_endHour )
			frame->GetPlot()->SetTimeSeriesRange( m_startHour, m_endHour );

		if ( m_variables.size() > 0 )
			frame->GetPlot()->SetSelectedNames( m_variables );

		if ( m_lineMode >= 0 && m_lineMode <= 2 )
			frame->GetPlot()->SetTimeSeriesMode( m_lineMode );

		if ( m_ylmin < m_ylmax )
			frame->GetPlot()->SetupTopYLeft( m_ylmin, m_ylmax );

		if ( m_yrmin < m_yrmax )
			frame->GetPlot()->SetupTopYRight( m_yrmin, m_yrmax );

		frame->Show();

		return true;
	}
	
	void OnInitCmdLine(wxCmdLineParser& parser)
	{
		wxApp::OnInitCmdLine(parser);

		parser.AddSwitch(wxT("l"), wxT("log"), wxT("show log window"));
		parser.AddOption(wxT("t"), wxT("tab"), wxT("initial tab number (zero-indexed)"), wxCMD_LINE_VAL_NUMBER);
		parser.AddOption(wxT("i"), wxT("index"), wxT("variable to display initially (zero-indexed)"), wxCMD_LINE_VAL_NUMBER);
		parser.AddOption(wxT("v"), wxT("variables"), wxT("comma separated list of column names to display initially"), wxCMD_LINE_VAL_STRING);
		parser.AddOption(wxT("s"), wxT("start"), wxT("starting hour to display"), wxCMD_LINE_VAL_DOUBLE);
		parser.AddOption(wxT("e"), wxT("end"), wxT("ending hour to display"), wxCMD_LINE_VAL_DOUBLE );
		parser.AddOption(wxT("m"), wxT("mode"), wxT("time series mode: 0=normal,1=stepped,2=stacked"), wxCMD_LINE_VAL_NUMBER);
		parser.AddOption(wxT("y1"), wxT("y1"), wxT("string min,max range for left y axis on top plot. if not specified, autoscaled."), wxCMD_LINE_VAL_STRING );
		parser.AddOption(wxT("y2"), wxT("y2"), wxT("string min,max range for right y axis on top plot. 'lock' is also allowed to lock y2 to y1. if not specified, autoscaled."), wxCMD_LINE_VAL_STRING );
		parser.AddParam(wxT("files to load"), wxCMD_LINE_VAL_STRING, wxCMD_LINE_PARAM_OPTIONAL | wxCMD_LINE_PARAM_MULTIPLE);
	}

	bool OnCmdLineParsed(wxCmdLineParser& parser)
	{
		wxApp::OnCmdLineParsed(parser);

		m_arg_filenames.Alloc(parser.GetParamCount());
		for (size_t i=0; i<parser.GetParamCount(); i++)
			m_arg_filenames.Add(parser.GetParam(i));

		double dd;
		long tabNumber, varNumber;
		if (parser.Found(wxT("l")))
			m_arg_showLog = true;
		else
			m_arg_showLog = false;
		
		m_arg_tab = -1;
		if (parser.Found(wxT("t"), &tabNumber) && tabNumber >= 0)
			m_arg_tab = tabNumber;

		m_arg_data = -1;
		if (parser.Found(wxT("i"), &varNumber) && varNumber >= 0)
			m_arg_data = varNumber;

		m_startHour = -1;
		if ( parser.Found("s", &dd) )
			m_startHour = dd;
			
		m_endHour = -1;
		if ( parser.Found("e", &dd ) )
			m_endHour = dd;

		wxString ss;
		if ( parser.Found("v", &ss) )
			m_variables = wxStringTokenize( ss, ",;|" );

		m_lineMode = -1;
		if ( parser.Found("m", &varNumber )  && varNumber >= 0 && varNumber <= 2 )
			m_lineMode = varNumber;

		m_ylmin = m_ylmax = 0;
		if ( parser.Found("y1", &ss) )
		{
			m_ylmin = wxAtof( ss );
			m_ylmax = wxAtof( ss.Mid( ss.Find(',')+1 ) );
		}

		m_yrmin = m_yrmax = 0;
		if ( parser.Found("y2", &ss) )
		{
			m_yrmin = wxAtof( ss );
			m_yrmax = wxAtof( ss.Mid( ss.Find(',')+1 ) );
		}

		return true;
	}


};

IMPLEMENT_APP( DViewApp );

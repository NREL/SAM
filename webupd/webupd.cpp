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
#include <wx/ffile.h>
#include <wx/wfstream.h>
#include <wx/progdlg.h>
#include <wx/zipstrm.h>
#include <wx/txtstrm.h>
#include <wx/datstrm.h>
#include <wx/tokenzr.h>

#include <wex/metro.h>
#include <wex/md5.h>
#include <wex/utils.h>

#include "../src/simplecurl.h"

#include "pkgdlg.h"

static wxString g_updateURL("https://sam.nrel.gov/sites/sam.nrel.gov/files/content/updates/");
static wxString g_samVerIdStr("2015.3.4");
static wxString g_appPath;
static wxString g_icmStr; // internet connection method string

#ifdef __WXMSW__
static const char *g_platStr = "msw";
#else
static const char *g_platStr = "osx";
#endif

#define NRWBUFBYTES 4096
enum { ID_download  = 12359 };

struct PatchInfo
{
	int level;
	wxString desc;
	wxString date;
	wxString file;
	wxString md5;
};

static int GetCurrentPatchLevel()
{
	 wxString path = g_appPath + "/runtime/patches/patch_" + wxString(g_platStr) + ".txt";
	 FILE *fp = fopen( (const char*)path.c_str(), "r" );
	 if (!fp) return -1;
	 char buf[32];
	 fgets( buf, 31, fp );
	 fclose(fp);
	 return atoi( buf );
}

static bool RecordPatch( int patch, const wxString &desc )
{
	 wxString path = g_appPath + "/runtime/patches/patch_" + wxString(g_platStr) + ".txt";
	 FILE *fp = fopen( (const char*)path.c_str(), "w");
	 if (!fp) return false;
	 fprintf(fp, "%d\n", patch);
	 fclose(fp);

	 path = g_appPath + "/runtime/patches/" + wxString::Format("%d", patch);
	 fp = fopen( (const char*)path.c_str(), "w");
	 if (!fp) return false;
	 fprintf(fp, "%d ", patch);
	 fputs( (const char*)wxNow().c_str(), fp );
	 fputs( "\n", fp );
	 fputs( (const char*)desc.c_str(), fp );
	 fclose(fp);
	 return true;
}

class UpdateDialog : public wxDialog
{
private:
	wxGauge *m_gauge;
	wxTextCtrl *m_text;
	wxString m_url;
	wxMetroButton *m_button;
public:
	UpdateDialog(wxString &url) 
		: wxDialog( 0, wxID_ANY, "SAM Web Update", wxDefaultPosition, wxSize(650,550), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
	{
		SetBackgroundColour( *wxWHITE );

#ifdef __WXMSW__
		SetIcon( wxICON( appicon ) );
#endif		
		m_text = new wxTextCtrl( this, wxID_ANY, wxEmptyString, 
			wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY|wxBORDER_NONE);
		
		m_button = new wxMetroButton( this, wxID_OK, "Close" );
		m_button->Hide();

		m_gauge = new wxGauge( this, wxID_ANY, 100 );
		m_url = url;
		wxBoxSizer *sz = new wxBoxSizer(wxVERTICAL);
		sz->Add( m_text, 1, wxALL|wxEXPAND, 10 );
		sz->Add( m_gauge, 0, wxALL|wxEXPAND, 10  );
		sz->Add( m_button, 0, wxALL|wxALIGN_CENTER, 10 );
		SetSizer(sz);
	}

	bool ApplyPatches( std::vector<PatchInfo> &plist, int begin, int end )
	{
		for ( int ipatch=begin;ipatch<=end;ipatch++ )
		{
			PatchInfo *pi = 0;
			for (int j=0;j<(int)plist.size();j++)
				if ( ipatch == plist[j].level )
					pi = &plist[j];

			if (!pi)
			{
				m_text->AppendText(wxString::Format("Invalid revision: %d\n", ipatch));
				continue;
			}
			
			m_text->AppendText("Internet connection method: " +g_icmStr+"\n");
			
			wxString patchArchive = wxFileName::GetTempDir() + "/sam_dl_patch.zip";
			wxString url =  m_url + "/" + pi->file;
			wxSimpleCurl dl( this, ID_download );
			m_text->AppendText("Downloading: " + url + "\n");
			wxYield();
			
			// start in asynchronous mode so that
			// we can have our own loop below that
			// enables UI updates
			dl.Start( url );
			while( 1 )
			{
				if ( dl.IsStarted() && !dl.Finished() ) 
				{
					wxYield();
					wxMilliSleep( 30 );
				}
				else break;
			}

			if ( dl.Ok() && dl.WriteDataToFile( patchArchive ) )
			{
				wxString md5 = wxMD5::GetFileMD5( patchArchive );
				if (md5.Trim().Trim(true).Lower() != pi->md5.Trim().Trim(true).Lower())
				{
					wxMessageBox("Revision archive did not download correctly (MD5 checksum error).  Please try the update process again.\n\n" + md5 + " != " + pi->md5, "SAM Web Update");
					return false;
				}
					
				m_text->AppendText("MD5: " + md5 + " ok.\n");
				
				if ( !UnzipFile( patchArchive, g_appPath + "/" ) )
				{
					wxMessageBox("Failed to process archive.", "SAM Web Update");
					return false;
				}
				
				wxRemoveFile( patchArchive );

				if (!RecordPatch( ipatch, pi->desc ))
				{
					wxMessageBox("Failed to record revision.","SAM Web Update");
					return false;
				}

				m_text->AppendText(wxString::Format("Recorded revision %d.\n", ipatch));
			}
			else
			{
				wxMessageBox("Could not download update. Please contact SAM user support.","SAM Web Update");
				return false;
			}
		}

		return true;
		
	}

	void OnCloseEvent( wxCloseEvent &ev )
	{
		ev.Veto();
	}
		
	void OnDownloadProgressEvent(wxSimpleCurlEvent &e)
	{
		double n = e.GetBytesTransferred();
		double len = e.GetBytesTotal();	
		m_gauge->SetValue( (int)( 100.0 * n / len ) );
		m_gauge->Refresh();
		wxYield();
	}
	
	bool UnzipFile(const wxString &archive, const wxString &target)
	{
		char rwbuf[NRWBUFBYTES];
		wxFFileInputStream in( archive );
		if (!in.IsOk()) return false;

		wxZipInputStream zip(in);
		if (!zip.IsOk()) return false;

		int nfiles = 0;
		wxZipEntry *zf = zip.GetNextEntry();
		while (zf)
		{
			wxFileName ffn(target + "/" + zf->GetName());
			ffn.Normalize();
			wxString fn = ffn.GetFullPath();
			m_text->AppendText("Processing: " + fn + "\n");
			wxYield();
			if (!zf->IsDir())
			{
				// create the directory if needed
				wxString dirpath = wxPathOnly(fn);
				if (!wxDirExists(dirpath))
					wxFileName::Mkdir( dirpath, 511, wxPATH_MKDIR_FULL );

				nfiles++;
				wxFFileOutputStream out( fn );
				if (!out.IsOk())
				{
					wxDELETE(zf);
					return false;
				}

				while (!zip.Eof())
				{
					zip.Read(rwbuf, NRWBUFBYTES);
					out.Write(rwbuf, zip.LastRead());
				}				
			}
			wxDELETE(zf);
			zf = zip.GetNextEntry();
		}

		m_text->AppendText(wxString::Format("%d files processed.\n", nfiles));
		
		return true;
	}

	void Log( const wxString &text )
	{
		m_text->AppendText( text );
	}

	bool WriteLog( const wxString &file )
	{
		if ( FILE *fp = fopen( file.c_str(), "w" ) )
		{
			fputs( m_text->GetValue().c_str(), fp );
			fclose(fp);
			return true;
		}
		else
			return false;
	}

	void Finalize()
	{
		m_gauge->Hide();
		m_button->Show();
		Layout();
		ShowModal();
	}

	void OnCommand( wxCommandEvent & )
	{
		EndModal(0);
	}

	DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(UpdateDialog, wxDialog)
	EVT_CLOSE( UpdateDialog::OnCloseEvent )
	EVT_BUTTON( wxID_OK, UpdateDialog::OnCommand )
	EVT_SIMPLECURL( ID_download, UpdateDialog::OnDownloadProgressEvent )
END_EVENT_TABLE();

class MyHtmlWindow : public wxHtmlWindow
{
public:
	MyHtmlWindow( wxWindow *parent, int id )
		: wxHtmlWindow( parent, id, wxDefaultPosition, wxDefaultSize, wxHW_DEFAULT_STYLE|wxBORDER_NONE )
	{
	}

	void OnLinkClicked( wxHtmlLinkEvent &evt )
	{
		wxString url( evt.GetLinkInfo().GetHref() );
		wxLaunchDefaultBrowser( url );
	}

	DECLARE_EVENT_TABLE();
};
BEGIN_EVENT_TABLE( MyHtmlWindow, wxHtmlWindow )
	EVT_HTML_LINK_CLICKED( wxID_ANY, MyHtmlWindow::OnLinkClicked )
END_EVENT_TABLE()


class SamUpdateApp : public wxApp
{
public:
	bool OnInit()
	{
		wxSimpleCurl::Init();

		
		wxString proxy_file = wxPathOnly(argv[0]) + "/proxy.txt";
		if (wxFileExists(proxy_file))
		{
			if (FILE *fp = fopen(proxy_file.c_str(), "r"))
			{
				char buf[512];
				fgets(buf, 511, fp);
				fclose(fp);
				wxSimpleCurl::SetProxy(wxString::FromAscii(buf));
			}
		}

		
		g_icmStr = wxSimpleCurl::GetProxy();
		if ( !g_icmStr.IsEmpty() )	g_icmStr = "via proxy, url is " + g_icmStr;
		else g_icmStr = "direct, no proxy";


		bool allow = true;
		if (wxConfig *cfg = new wxConfig("SamUpdate3", "NREL")) 
		{
			cfg->Read("allow_web_updates", &allow);
			delete cfg;
		}
		
		wxFileName binary( argv[0] );
		binary.MakeAbsolute();

		wxFileName relpath( wxPathOnly(binary.GetFullPath()) + "/../" );
		relpath.Normalize();
		g_appPath = relpath.GetFullPath();

		bool quiet = false;
		wxString url = g_updateURL + g_samVerIdStr;
		
		if ( argc > 1 )
		{
			wxString arg = argv[1];
			if ( arg == "-quiet" )
				quiet = true;
			else if ( arg == "-package" )
			{
				PackageDialog dlg( 0, "Create update package: " + g_samVerIdStr, g_samVerIdStr, g_appPath );
				dlg.ShowModal();
				return false;
			}
			else if ( arg == "-url" )
			{
				url = argv[2];
			}
			else if ( arg == "-md5" )
			{
				wxFileDialog dlg( 0, "Select file to get MD5SUM for" );
				if ( wxID_OK == dlg.ShowModal() )
					::wxShowTextMessageDialog( "MD5SUM for " + dlg.GetPath() + "\n\n" + wxMD5::GetFileMD5( dlg.GetPath() ) );
				else
					wxMessageBox("no file selected, quitting");
				return false;
			}
		}

		// check that we can write to a file
		wxString path = g_appPath + "/.check-" + wxString(g_platStr);
		FILE *fp = fopen( (const char*)path.c_str(), "w" );
		if (!fp)
		{
			if (!quiet) wxMessageBox("SAM cannot be updated because you do not have write permissions to the installation folder\n\n" + path,"SAM Web Update");
			return false;
		}
		fclose(fp);	
		wxRemoveFile( path );
				
		wxSimpleCurl dl_notice;
		if ( dl_notice.Start( url + "/newversion_" + wxString(g_platStr) + ".txt", true ) )
		{
			wxString text( dl_notice.GetDataAsString() );
			if ( text.Left(17) == "<!--sam-notice-->" && text.Len() > 18 )
			{
				wxDialog dlg( 0, wxID_ANY, "Notice", wxDefaultPosition, wxSize(550,500), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER );
				dlg.SetBackgroundColour( *wxWHITE );
				MyHtmlWindow *html = new MyHtmlWindow( &dlg, wxID_ANY );
				html->SetPage( text );
				wxBoxSizer *szm = new wxBoxSizer(wxVERTICAL);
				szm->Add( html, 1, wxALL|wxEXPAND, 10 );
				szm->Add( new wxMetroButton( &dlg, wxID_OK, "OK"), 0, wxALL|wxCENTER, 10);
				dlg.SetSizer( szm );
				dlg.CenterOnScreen();
				dlg.ShowModal();
			}
		}

		// download current patch information for this platform
		wxSimpleCurl dl;
		if ( !dl.Start( url + "/patch_" + wxString(g_platStr) + ".txt", true ) )
		{
			if (!quiet) 
				wxMessageBox("Could not obtain update information for SAM " + wxString(g_platStr) + ".\n\nInternet connection method: " + g_icmStr, "SAM Web Update");
			return false;
		}


		// parse the patch information
		long latest_ver = 0;
		wxArrayString lines = wxStringTokenize( dl.GetDataAsString(), "\n");		
		std::vector<PatchInfo> pdat;
		for (size_t i=0;i<lines.Count();i++)
		{
			// skip comments
			if (lines[i].Left(1) == "#")
				continue;

			// patch line format:
			// <int:level> \t date \t html description \t file to download \t md5sum of file\n
			wxArrayString patch = wxStringTokenize(lines[i], "\t");			
			if (patch.Count() == 5)
			{
				PatchInfo p;
				p.level = atoi( patch[0].c_str() );
				p.date = patch[1].c_str();
				p.desc = patch[2].c_str();
				p.file = patch[3].c_str();
				p.md5 = patch[4].c_str();

				if (p.level > latest_ver)
					latest_ver = p.level;

				pdat.push_back(p);
			}
		}
				
		long curver = ::GetCurrentPatchLevel();
		if(curver < 0)
		{
			RecordPatch( 0, "Baseline SAM " + g_samVerIdStr + " Installation" );
			curver = 0;
		}

		if (latest_ver > curver)
		{
			wxDialog dlg( 0, wxID_ANY, "SAM Web Update (" + g_samVerIdStr + ")",
				wxDefaultPosition, wxSize(650,550), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER);
			dlg.SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );
			MyHtmlWindow *html = new MyHtmlWindow( &dlg, wxID_ANY );
			wxString h="An update is available for SAM " + g_samVerIdStr + ". To install the update, click <b>Update</b>. To ignore the update and start SAM, click <b>Cancel</b>. You can update later by clicking <b>Check for Updates</b> on SAM's Help menu.<br><br>";
			for (int i=((int)pdat.size())-1;i>=0;i--)
			{
				if ( pdat[i].level > curver )
				{
					h += wxString::Format("<b>Revision %d</b>", pdat[i].level) + " [<i>" + pdat[i].date + " </i>]<br><br>" + pdat[i].desc + "<br>";
					if (i > 0) h += "<hr>";
				}
			}
			html->SetPage( h );
			wxBoxSizer *btnsz = new wxBoxSizer(wxHORIZONTAL);
			wxCheckBox *chkAllowUpdates = new wxCheckBox(&dlg, wxID_ANY, "Allow SAM to check for updates at startup");
			chkAllowUpdates->SetForegroundColour( *wxWHITE );
			chkAllowUpdates->SetValue(allow);
			btnsz->Add( chkAllowUpdates, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5);
			btnsz->AddStretchSpacer();
			btnsz->Add( new wxMetroButton(&dlg, wxID_OK, "Update"), 0, wxALL|wxEXPAND, 0);
			btnsz->Add( new wxMetroButton(&dlg, wxID_CANCEL, "Cancel"), 0, wxALL|wxEXPAND, 0);
			wxBoxSizer *szm = new wxBoxSizer(wxVERTICAL);
			szm->Add( html, 1, wxALL|wxEXPAND, 0 );
			szm->Add( btnsz, 0, wxALL|wxEXPAND, 0 );
			dlg.SetSizer( szm );
			dlg.CenterOnScreen();
			int code = dlg.ShowModal();
		
			if (allow != chkAllowUpdates->GetValue() && allow == true)
				wxMessageBox("From the Welcome page in SAM, you can check for updates manually.", "SAM Web Update", wxICON_INFORMATION|wxOK);

			if (wxConfig *cfg = new wxConfig("SamUpdate3", "NREL"))
			{
				cfg->Write("allow_web_updates", chkAllowUpdates->GetValue());
				delete cfg;
			}

			if ( code != wxID_OK)
				return false;

			// make sure SAM is not running while we patch it
			wxArrayString binaries;
			wxString sam_exe;
#ifdef __WXMSW__
			binaries.Add( g_appPath + "/win32/sam.exe" );
			binaries.Add( g_appPath + "/x64/sam.exe" );

#ifdef _WIN64
			sam_exe = g_appPath + "/x64/sam.exe";
#else
			sam_exe = g_appPath + "/win32/sam.exe";
#endif

#else
			sam_exe = g_appPath + "/MacOS/SAM";
			binaries.Add( sam_exe );
#endif

#ifdef __WXMSW__
			// check whether application is running on Windows.  Not sure how to do this on OSX.			
			while (1)
			{
				bool all_ok = true;
				for( size_t i=0;i<binaries.size();i++ )
				{
					FILE *fsam = fopen( binaries[i].c_str(), "a" );
					if (fsam == 0)
						all_ok = false;
					else
						fclose( fsam );
				}

				if ( all_ok ) // all SAM binaries are not running.
					break;
				
				if (wxCANCEL == wxMessageBox("Close all instances of SAM and then click OK to continue with the update.\n\nSAM Web Update will not work if SAM is running.  This window will stay open until you click OK or Cancel.", "SAM Web Update", wxOK|wxCANCEL))
					return false;
			}
#else
			// on OSX, kindly ask the user to quit SAM before proceeding with the update
			wxMessageBox("Please quit SAM before proceeding with the web update.  The update may fail if SAM is still running.  Click OK to continue once SAM is closed.");
#endif
						
			UpdateDialog upd(url);
			upd.CenterOnScreen();
			upd.Show();
			upd.Raise();
			wxYield();
			wxString now(wxNow());
			now.Replace( ":", " " );
			now.Replace( " ", "_" );
			now.MakeLower();
			wxFileName log_path(  g_appPath + "/runtime/patches/patchlog_" + now  + ".txt" );
			log_path.Normalize();

			if (upd.ApplyPatches( pdat, curver+1, latest_ver ))
			{
				if ( upd.WriteLog( log_path.GetFullPath() ) )
					upd.Log("Log saved at " + log_path.GetFullPath() + "\n" );
				else
					upd.Log("Error saving log to " + log_path.GetFullPath() );

				upd.Log("\nUpdate successful.\n");
			}
			else
			{
				upd.Hide();
				upd.Log("\nThere was a problem applying one or more updates.  Please try updating again later.\n");
			}
			
			upd.Finalize();
			wxExecute( sam_exe );
		}
		else
		{
			if (!quiet) wxMessageBox("SAM " + g_samVerIdStr + " is up to date.","SAM Web Update");
		}
		
		wxSimpleCurl::Shutdown();
		
		return false;
	}
};

IMPLEMENT_APP( SamUpdateApp );

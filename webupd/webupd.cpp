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

#include <wex/md5.h>
#include <wex/utils.h>

#include "../src/simplecurl.h"

#include "pkgdlg.h"

static wxString g_updateURL("https://sam.nrel.gov/sites/sam.nrel.gov/files/content/updates/");
static wxString g_samVerIdStr("2014.11.24");
static wxString g_appPath;

#ifdef __WXMSW__
static const char *g_platStr = "win32";
#else
static const char *g_platStr = "macosx";
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
	 wxString path = g_appPath + "/runtime/patch_" + wxString(g_platStr) + ".dat";
	 FILE *fp = fopen( (const char*)path.c_str(), "r" );
	 if (!fp) return -1;
	 char buf[256];
	 fgets( buf, 255, fp );
	 fclose(fp);
	 return atoi( buf );
}

static bool RecordPatch( int patch, const wxString &desc )
{
	 wxString path = g_appPath + "/runtime/patches/patch_" + wxString(g_platStr) + ".dat";
	 FILE *fp = fopen( (const char*)path.c_str(), "w");
	 if (!fp) return false;
	 fprintf(fp, "%d ", patch);
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
public:
	UpdateDialog(wxString &url) : wxDialog( 0, wxID_ANY, "SAM Update", wxDefaultPosition, wxSize(500,500) )
	{
#ifdef __WXMSW__
		SetIcon( wxICON( appicon ) );
#endif		
		m_text = new wxTextCtrl( this, wxID_ANY, wxEmptyString, 
			wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY);
		
		m_gauge = new wxGauge( this, wxID_ANY, 100 );
		m_url = url;
		wxBoxSizer *sz = new wxBoxSizer(wxVERTICAL);
		sz->Add( m_text, 1, wxALL|wxEXPAND );
		sz->Add( m_gauge, 0, wxALL|wxEXPAND );
		SetSizer(sz);

	}

	bool ApplyPatches( std::vector<PatchInfo> &plist, size_t begin, size_t end )
	{
		for ( size_t ipatch=begin;ipatch<=end;ipatch++ )
		{
			PatchInfo *pi = 0;
			for (int j=0;j<(int)plist.size();j++)
				if (ipatch == (int)plist[j].level)
					pi = &plist[j];

			if (!pi)
			{
				m_text->AppendText(wxString::Format("Invalid revision: %d\n", ipatch));
				continue;
			}

			
			wxString patchArchive = wxFileName::GetTempDir() + "/sampatch.zip";
				//wxString::Format("c:/sam/patch_%d.zip", ipatch);
			m_text->AppendText("\n");
//				wxString url =  g_updateURL + g_samVerIdStr + "/" + pi->file;
			wxString url =  m_url + "/" + pi->file;
			wxSimpleCurl dl( this, ID_download );
			dl.Start( url );

			m_text->AppendText("Downloading: " + url + "\n");
			wxYield();
			if (dl.WriteDataToFile( patchArchive ))
			{
				wxString md5 = wxMD5::GetFileMD5( patchArchive );
				if (md5.Trim().Trim(true).Lower() != pi->md5.Trim().Trim(true).Lower())
				{
					wxMessageBox("Revision archive did not download correctly (MD5 checksum error).  Please try the update process again.\n\n" + md5 + " != " + pi->md5, "SAM Update");
					return false;
				}
					
				m_text->AppendText("MD5: " + md5 + " ok.\n");
					
#ifdef __WXMSW__
				bool ok = UnzipFile( patchArchive, g_appPath + "/" );
#else 
				// Mac extraction point is SAM.app/Contents
				// Updated 3/3/13 so that ssc.dylib can be updated
				// Allows for updating of Frameworks, MacOS and Resources folder in SAM.app
				// g_appPath is location of application and is SAM.app/Contents/MacOS
				bool ok = UnzipFile( patchArchive, g_appPath + "/../" );
#endif
				wxRemoveFile( patchArchive );

				if (!ok)
				{
					wxMessageBox("Failed to process archive.", "SAM Update");
					return false;
				}

				if (!RecordPatch( ipatch, pi->desc ))
				{
					wxMessageBox("Failed to record revision.","SAM Update");
					return false;
				}

				m_text->AppendText(wxString::Format("Recorded revision %d.\n", ipatch));
			}
			else
			{
				wxMessageBox("Could not download update. Contact SAM user support.","SAM Update");
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
			wxString fn = target + "/" + zf->GetName();
			m_text->AppendText("Processing: " + target + "/" + zf->GetName() + "\n");
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

	DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(UpdateDialog, wxDialog)
	EVT_CLOSE( UpdateDialog::OnCloseEvent )
	EVT_SIMPLECURL( ID_download, UpdateDialog::OnDownloadProgressEvent )
END_EVENT_TABLE();


class SamUpdateApp : public wxApp
{
public:
	bool OnInit()
	{
		bool is64 = (sizeof(void*)==8);

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


		bool allow = true;
		if (wxConfig *cfg = new wxConfig("SamUpdate3", "NREL")) 
		{
			cfg->Read("allow_web_updates", &allow);
			delete cfg;
		}
		
		wxFileName relpath( wxPathOnly(argv[0]) + "/.." );
		relpath.MakeAbsolute();
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
		}

		// check that we can write to a file
		wxString path = g_appPath + "/.testWrite" + wxString(g_platStr) + ".dat";
		FILE *fp = fopen( (const char*)path.c_str(), "w" );
		if (!fp)
		{
			if (!quiet) wxMessageBox("SAM cannot be updated because you do not have write permissions to the install directory.","SAM Update");
			return false;
		}
		fclose(fp);	
		wxRemoveFile( path );

		// download current patch information for this platform
		wxSimpleCurl dl;
		if ( !dl.Start( url + "/patchinfo_" + wxString(g_platStr) + ".txt", true ) )
		{
			if (!quiet) 
				wxMessageBox("Could not obtain update information.", "SAM Update");
			return false;
		}


		// parse the patch information
		long latver = 0;
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

				if (p.level > latver)
					latver = p.level;

				pdat.push_back(p);
			}
		}
				
		long curver = ::GetCurrentPatchLevel();
		if(curver < 0)
		{
			RecordPatch( 0, "Baseline SAM " + g_samVerIdStr + " Installation" );
			curver = 0;
		}

		if (latver > curver)
		{
			wxDialog *dlg = new wxDialog( 0, wxID_ANY, "SAM Update (" + g_samVerIdStr + ")",
				wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER);
			wxHtmlWindow *html = new wxHtmlWindow( dlg );
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
			wxCheckBox *chkAllowUpdates = new wxCheckBox(dlg, wxID_ANY, "Allow SAM to check for updates at startup");
			chkAllowUpdates->SetValue(allow);
			btnsz->Add( chkAllowUpdates, 0, wxALL|wxEXPAND, 3);
			btnsz->AddStretchSpacer();
			btnsz->Add( new wxButton(dlg, wxID_OK, "Update"), 0, wxALL|wxEXPAND, 3);
			btnsz->Add( new wxButton(dlg, wxID_CANCEL, "Cancel"), 0, wxALL|wxEXPAND, 3);
			wxBoxSizer *szm = new wxBoxSizer(wxVERTICAL);
			szm->Add( html, 1, wxALL|wxEXPAND );
			szm->Add( btnsz, 0, wxALL|wxEXPAND, 3 );
			dlg->SetSizer( szm );
			int code = dlg->ShowModal();
		
			if (allow != chkAllowUpdates->GetValue() && allow == true)
				wxMessageBox("From the Help menu in SAM, you can check for updates manually, as well as re-enable the automatic check at startup.", "SAM Update", wxICON_INFORMATION|wxOK);

			if (wxConfig *cfg = new wxConfig("SamUpdate3", "NREL"))
			{
				cfg->Write("allow_web_updates", chkAllowUpdates->GetValue());
				delete cfg;
			}

			if ( code != wxID_OK)
			{

				delete dlg;
				return false;
			}	

#ifdef __WXMSW__
			wxString sam_exe;
			if ( is64 ) sam_exe = g_appPath + "/x64/sam.exe";
			else sam_exe = g_appPath + "/win32/sam.exe";

#else
			wxString sam_exe = g_appPath + "/SAM";
#endif
			
			while (1)
			{
				if (wxCANCEL == wxMessageBox("SAM Update will not work if SAM is running.\n\nClose all instances of SAM and then click OK to continue with the update. (This window will stay open until you click OK or Cancel.)", "SAM Update", wxOK|wxCANCEL))
					return false;

				FILE *fsam = fopen(sam_exe.c_str(), "a");
				if (fsam != 0)
				{
					fclose(fsam);
					break;
				}
			}


			
			UpdateDialog upd(url);
			upd.Show();
			upd.Raise();
			wxYield();
			if (upd.ApplyPatches( pdat, curver+1, latver ))
				wxMessageBox("Update complete.", "SAM Update", wxOK|wxICON_INFORMATION);
			else
				wxMessageBox("There was a problem applying one or more updates.  Please try updating again later.", "SAM Update", wxOK|wxICON_WARNING);

			wxExecute( sam_exe );
		}
		else
		{
			if (!quiet) wxMessageBox("SAM " + g_samVerIdStr + " is up to date.","SAM Update");
		}
		
		wxSimpleCurl::Shutdown();
		
		return false;
	}
};

IMPLEMENT_APP( SamUpdateApp );

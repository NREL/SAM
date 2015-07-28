#include "pkgdlg.h"

#include <wx/dir.h>
#include <wx/filename.h>
#include <wx/filepicker.h>
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
#include <wx/splitter.h>
#include <wx/progdlg.h>
#include <wx/zipstrm.h>
#include <wx/txtstrm.h>
#include <wx/datstrm.h>
#include <wx/tokenzr.h>

#include <wex/utils.h>
#include <wex/md5.h>

#include <wex/extgrid.h>


BEGIN_EVENT_TABLE( MyHtmlWindow, wxHtmlWindow )
	EVT_HTML_LINK_CLICKED( wxID_ANY, MyHtmlWindow::OnLinkClicked )
END_EVENT_TABLE()


MyHtmlWindow::MyHtmlWindow( wxWindow *parent, int id )
	   : wxHtmlWindow( parent, id, wxDefaultPosition, wxDefaultSize, 
	   		wxHW_DEFAULT_STYLE|wxBORDER_NONE )
{
}

void MyHtmlWindow::OnLinkClicked( wxHtmlLinkEvent &evt )
{
   wxString url( evt.GetLinkInfo().GetHref() );
   wxLaunchDefaultBrowser( url );
}

enum { ID_PATCH_NOTES = wxID_HIGHEST+494 };

class PatchFileHelper : public wxDialog
{
	wxString m_archive, m_platStr, m_verStr, m_patchLevel, m_md5;
	MyHtmlWindow *m_html;
	wxTextCtrl *m_notes, *m_line;
	public:
		PatchFileHelper( wxWindow *parent, 
				const wxString &archive,
				const wxString &plat,
				const wxString &ver,
				const wxString &level ,
				const wxString &md5  )
			: wxDialog(parent, -1, "Create Patch File Line: patch_"+plat+".txt", 
					wxDefaultPosition, wxSize(750,450), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER),
			m_archive(archive),
			m_platStr(plat),
			m_verStr(ver),
			m_patchLevel( level ),
			m_md5( md5 )
		{
			m_html = 0;
			m_line = 0;

			if ( m_patchLevel.IsEmpty()
					|| m_md5.IsEmpty()
					|| m_archive.IsEmpty() )
				wxMessageBox("patch level, md5, or archive data doesn't exist. create a archive package first.  in the meantime you can still edit the notes html.");
			
			m_notes = new wxTextCtrl( this, ID_PATCH_NOTES, 
				"<b>List of Changes:</b> <br><br>\n"
				"<ol>\n"
				" <li>\n"
				" <li>\n"
				"</ol><br><br>\n\n"
				"<p>For more information, see the <a href=\"https://sam.nrel.gov/sites/sam.nrel.gov/files/content/updates/releasenotes.html\">release notes</a>.</p>",
				wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE);
				
			m_html = new MyHtmlWindow( this, wxID_ANY );
			
			wxBoxSizer *hsizer = new wxBoxSizer( wxHORIZONTAL );
			hsizer->Add( m_notes, 1, wxALL|wxEXPAND, 0 );
			hsizer->Add( m_html, 1, wxALL|wxEXPAND, 0 );
			
			m_line = new wxTextCtrl( this, wxID_ANY );
			
			wxBoxSizer *vsizer = new wxBoxSizer( wxVERTICAL );
			vsizer->Add( hsizer, 1, wxALL|wxEXPAND, 4 );
			vsizer->Add( m_line, 0, wxALL|wxEXPAND, 4 );

			wxBoxSizer *hsizer2 = new wxBoxSizer( wxHORIZONTAL );
			hsizer2->Add( new wxButton( this, wxID_OK ), 0, wxALL, 4 );
			hsizer2->Add( new wxButton( this, wxID_SAVE ), 0, wxALL, 4 );
			vsizer->Add( hsizer2, 0, wxALL, 1 );
			
			SetSizer( vsizer );
		}
		
		void SetNotes( const wxString &n )
		{
			m_notes->SetValue( n );
		}
		wxString GetNotes()
		{
			return m_notes->GetValue();
		}
		
		void OnNotesChange( wxCommandEvent & )
		{
			if ( !m_html || !m_line ) return;
			
			wxString nn( m_notes->GetValue() );
			nn.Replace( "\t", "" );
			nn.Replace( "\r", "" );
			nn.Replace( "\n", "" );
			
			m_html->SetPage( nn );
			
			m_line->ChangeValue( m_patchLevel + "\t" 
					+ wxDateTime::Now().FormatISODate() 
					+ "\t" + nn + "\t" + wxFileNameFromPath(m_archive) + "\t" + m_md5 );
		}

		void OnSave( wxCommandEvent & )
		{
			wxFileDialog dlg( this, "Save patch file line", wxEmptyString, "patch_" + m_platStr + ".txt", "Text Files (*.txt)|*.txt", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
			if ( dlg.ShowModal() == wxID_OK )
			{
				if ( FILE *f = fopen( dlg.GetPath().c_str(), "w" ) )
				{
					fputs( m_line->GetValue().c_str(), f );
					fclose( f );
				}
				else
					wxMessageBox("Could not write to file:\n\n" + dlg.GetPath());
			}
		}
		
		DECLARE_EVENT_TABLE();
		
};

BEGIN_EVENT_TABLE( PatchFileHelper, wxDialog )
	EVT_TEXT( ID_PATCH_NOTES, PatchFileHelper::OnNotesChange )
	EVT_BUTTON( wxID_SAVE, PatchFileHelper::OnSave )
END_EVENT_TABLE()


enum { ID_GRID = wxID_HIGHEST + 941,
	ID_OUTPUT, 
	ID_DIFF,
	ID_PACKAGE,
	ID_FILTER,
	ID_DEFFILT,
	ID_PATCHHELPER
};

BEGIN_EVENT_TABLE( PackageDialog, wxDialog )
	EVT_BUTTON( ID_DIFF, PackageDialog::OnCommand )
	EVT_BUTTON( ID_PACKAGE, PackageDialog::OnCommand )
	EVT_BUTTON( ID_FILTER, PackageDialog::OnCommand )
	EVT_BUTTON( ID_DEFFILT, PackageDialog::OnCommand )
	EVT_BUTTON( ID_PATCHHELPER, PackageDialog::OnCommand )

END_EVENT_TABLE()

#define DEFAULT_FILTER "*d.exe;*.ilk;*.tlog;*d.pdb;*.log;*.lastbuildstate;*msvc*.dll;*lib*.lib;*webupd*;./SAM;*.iss"

static const char *help_text =

"When building the installers, don't forget to enter the patch level in the /runtime/patches/patch_<plat>.txt file!\n"
"On Windows and Linux, the folder path should be <path-to-sam-folder>, i.e. c:\\sam\\2015.6.30.  "
"On OSX, the folder path should point to the 'Contents' folder in the bundle, i.e. /Users/xyz/SAM.app/Contents";

PackageDialog::PackageDialog( wxWindow *parent, const wxString &title,
	const wxString &ver, const wxString &basepath, const wxString &plat)
	: wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxSize(1100, 800), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)
{
	m_appPath = basepath;
	m_verStr = ver;
	m_platStr = plat;

	m_curDir = new wxDirPickerCtrl( this, wxID_ANY, m_appPath, "Choose current (new) path", wxDefaultPosition, wxDefaultSize, wxDIRP_DEFAULT_STYLE|wxDIRP_USE_TEXTCTRL|wxDIRP_SMALL );
	m_oldDir = new wxDirPickerCtrl( this, wxID_ANY, m_appPath, "Choose old path", wxDefaultPosition, wxDefaultSize, wxDIRP_DEFAULT_STYLE|wxDIRP_USE_TEXTCTRL|wxDIRP_SMALL );
	
	wxString dir;
	wxConfig cfg( "SamUpdate", "NREL");
	if (cfg.Read("m_curDir", &dir ))
		m_curDir->SetPath( dir );
	if (cfg.Read("m_oldDir", &dir ))
		m_oldDir->SetPath( dir );
		
	wxBoxSizer *buttons = new wxBoxSizer( wxHORIZONTAL );
	buttons->Add( new wxButton( this, wxID_CANCEL, "Close" ), 0, wxALL, 2 );
	buttons->Add( new wxButton( this, ID_DIFF, "Generate diff" ), 0, wxALL, 2 );
	buttons->Add( new wxButton( this, ID_PACKAGE, "Create package" ), 0, wxALL, 2 );
	buttons->Add( new wxButton( this, ID_PATCHHELPER, "Patch line..." ), 0, wxALL, 2 );
	buttons->Add( new wxButton( this, ID_DEFFILT, "Reset filters" ), 0, wxALL, 2 );
	buttons->Add( new wxButton( this, ID_FILTER, "Apply filters:" ), 0, wxALL, 2 );
	
	m_filter = new wxTextCtrl( this, wxID_ANY, DEFAULT_FILTER ); 

	wxString buf;
	if (cfg.Read("m_filter", &buf ))
		m_filter->ChangeValue( buf );

	buttons->Add( m_filter, 1, wxALL|wxEXPAND, 3 );

	wxBoxSizer *dirs = new wxBoxSizer( wxHORIZONTAL );
	dirs->Add( new wxStaticText(this, wxID_ANY, "New version:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	dirs->Add( m_curDir, 1, wxALL|wxEXPAND, 2 );
	dirs->Add( new wxStaticText(this, wxID_ANY, "Old version:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	dirs->Add( m_oldDir, 1, wxALL|wxEXPAND, 2 );
	
	wxSplitterWindow *split = new wxSplitterWindow( this );
	
	m_grid = new wxExtGridCtrl( split, ID_GRID );
	m_grid->CreateGrid(1,1);

	m_output = new wxTextCtrl( split, ID_OUTPUT, "Ready", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_DONTWRAP );
	m_output->SetFont( wxFont(12, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "Consolas" ));
	m_output->SetForegroundColour( "navy" );

	split->SplitHorizontally(m_grid, m_output, -300);

	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( buttons, 0, wxALL|wxEXPAND, 3 );	
	sizer->Add( dirs, 0, wxALL|wxEXPAND, 3 );
	sizer->Add( new wxStaticText( this, wxID_ANY, help_text ), 0, wxALL|wxEXPAND, 3 );
	sizer->Add( split, 1, wxALL|wxEXPAND, 0 );
	SetSizer( sizer );
}

PackageDialog::~PackageDialog()
{
	SaveStateToConfig();
	ClearMaps();
	ClearDiffs();
}

void PackageDialog::SaveStateToConfig()
{
	wxConfig cfg( "SamUpdate", "NREL");
	cfg.Write( "m_curDir", m_curDir->GetPath() );
	cfg.Write( "m_oldDir", m_oldDir->GetPath() );
	cfg.Write( "m_filter", m_filter->GetValue() );
}

void PackageDialog::ScanCurrentFiles( const wxString &basepath, const wxString &path, 
									 int *count, filemap *map )
{
	//printf("scanning files in %s...\n", (const char*)path.c_str());
	
	wxDir dir( path );

	m_output->AppendText("scanning folder: " + path + "\n");
	wxSafeYield( NULL, true );
	
	if (!dir.IsOpened())
		return;
	
	wxString f,item;
	bool hasmore = dir.GetFirst(&f);
	while (hasmore)
	{
		if ( f.Lower().Left(9) == "samupdate" )
		{
			hasmore = dir.GetNext(&f);
			continue;
		}

		item = path + "/" + f;

		if ( wxDirExists(item) )
		{
			ScanCurrentFiles( basepath, item, count, map );
			if (count) (*count)++;
		}
		else
		{
			wxFileName fn( item );
			FileInfo *ff = new FileInfo;
			ff->relpath = "." + item.Mid( basepath.Len() );
			ff->lastmod = fn.GetModificationTime();
			(*map)[ ff->relpath.Lower() ] = ff;

			if (count) (*count)++;
		}

		hasmore = dir.GetNext(&f);
	}
}

void PackageDialog::ClearDiffs()
{
	for( size_t i=0;i<m_diffs.size();i++ )
		delete m_diffs[i];

	m_diffs.clear();
}

void PackageDialog::ClearMaps()
{
	for ( filemap::iterator it = m_curFiles.begin();
		it != m_curFiles.end();
		++it )
		delete it->second;

	m_curFiles.clear();

	
	for ( filemap::iterator it = m_oldFiles.begin();
		it != m_oldFiles.end();
		++it )
		delete it->second;

	m_oldFiles.clear();
}

bool PackageDialog::Identical( const wxString &file1, const wxString &file2 )
{
	if ( file1 == file2 ) return true;

	FILE *fp1 = fopen( file1.c_str(), "rb" );
	if (!fp1) return false;
	FILE *fp2 = fopen( file2.c_str(), "rb" );
	if (!fp2) { fclose(fp1); return false; }


	bool same = true;
	int c1, c2;
	while ( 1 )
	{
		c1 = fgetc(fp1);
		c2 = fgetc(fp2);

		if ( c1 != c2 )
		{
			same = false;
			break;
		}

		if ( c1 == EOF && c2 == EOF )
			break;

		if ( c1 == EOF && c2 != EOF 
				|| c1 != EOF && c2 == EOF )
		{
			same = false;
			break;
		}
	}


	fclose(fp1);
	fclose(fp2);

	return same;
}

void PackageDialog::MakeDiff()
{
	SaveStateToConfig();

	m_output->Clear();
	int count1 = 0, count2 = 0;

	wxString curpath( m_curDir->GetPath() );
	wxString oldpath( m_oldDir->GetPath() );

	if ( curpath == oldpath )
	{
		wxMessageBox("new and old versions have the same path... stopping");
		return;
	}
	
	ClearMaps();
	m_output->AppendText("Scanning current (new) path: " + curpath + "\n"); 
	ScanCurrentFiles( curpath, curpath, &count1, &m_curFiles );
	m_output->AppendText("\nScanning old path: " + oldpath + "\n"); 
	ScanCurrentFiles( oldpath, oldpath, &count2, &m_oldFiles );
	
	m_output->AppendText( wxString::Format("Scanned: current ver = %d, old ver = %d\n", count1, count2 ) );
	wxSafeYield( NULL, true );

	ClearDiffs();

	int ii=0;
	for ( filemap::iterator cur = m_curFiles.begin();
		cur != m_curFiles.end();
		++cur )
	{
		++ii;
		filemap::iterator old = m_oldFiles.find( cur->first );
		if ( old != m_oldFiles.end() )
		{
			if ( !Identical( curpath + "/" + cur->second->relpath,
				m_oldDir->GetPath() + "/" + old->second->relpath ))
			{
				DiffInfo *di = new DiffInfo;
				di->cur = cur->second->relpath;
				di->cur_time = cur->second->lastmod;
				di->old = old->second->relpath;
				di->old_time = old->second->lastmod;
				di->bindiff = true;
				m_diffs.push_back( di );
			}
			else
			{
				m_output->AppendText(wxString::Format("[%d of %d] ignore (no changes): ",ii, count1) + cur->second->relpath  + "\n");
				wxSafeYield( NULL, true );
			}
		}
		else
		{
			DiffInfo *di = new DiffInfo;
			di->cur = cur->second->relpath;
			di->cur_time = cur->second->lastmod;
			di->bindiff = false;
			m_diffs.push_back( di );
		}
	}

	// sort the selections by file names
	int count = m_diffs.size();
	for (int i=0;i<count-1;i++)
	{
		int smallest = i;

		for (size_t j=i+1;j<count;j++)
			if ( m_diffs[j]->cur < m_diffs[smallest]->cur )
				smallest = j;

		// swap
		DiffInfo *di = m_diffs[i];
		m_diffs[i] = m_diffs[smallest];
		m_diffs[smallest] = di;		
	}

	UpdateGrid();
}

void PackageDialog::UpdateGrid()
{	
	if ( m_diffs.size() == 0 )
	{
		wxMessageBox( "no differences found. no update needed." );
		return;
	}

	m_grid->Freeze();
	m_grid->ResizeGrid( m_diffs.size(), 3 );
	m_grid->SetColLabelValue( 0, "file" );
	m_grid->SetColLabelValue( 1, "mod" );
	m_grid->SetColLabelValue( 2, "reason" );

	for( size_t i=0;i<m_diffs.size();i++ )
	{
		m_grid->SetCellValue( m_diffs[i]->cur, i, 0 );
		m_grid->SetCellValue( m_diffs[i]->cur_time.FormatDate() + " " + m_diffs[i]->cur_time.FormatTime(), i, 1 );
		m_grid->SetCellValue( m_diffs[i]->bindiff ? "UPDATED" : "NEW", i, 2 );		
		m_grid->SetCellBackgroundColour(  m_diffs[i]->bindiff ? "wheat" : "sea green", i, 2 );
	}
	m_grid->AutoSizeColumns();
	m_grid->Thaw();
}

void PackageDialog::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_DIFF:
		MakeDiff();
		break;
	case ID_PACKAGE:
		MakePackage();
		break;
	case ID_FILTER:
		FilterResults();
		break;
	case ID_DEFFILT:
		m_filter->SetValue( DEFAULT_FILTER );
		break;
	case ID_PATCHHELPER:
		{
			PatchFileHelper pfh( this, m_archive, m_platStr, m_verStr, m_patchLevel, m_md5 );
			if ( !m_notes.IsEmpty() )
				pfh.SetNotes( m_notes );

			pfh.ShowModal();
			m_notes = pfh.GetNotes();
		}
		break;
	}
}

void PackageDialog::FilterResults()
{
	wxArrayString filters = wxStringTokenize(m_filter->GetValue(), ";");

	size_t i = 0;
	while( i < m_diffs.size() )
	{
		bool remove = false;
		for ( size_t j=0;j<filters.Count();j++ )
			if ( wxMatchWild( filters[j], m_diffs[i]->cur, false ) )
				remove = true;

		if ( remove )
		{
			delete m_diffs[i];
			m_diffs.erase( m_diffs.begin() + i );
		}
		else
			i++;
	}

	UpdateGrid();
}

void PackageDialog::MakePackage()
{
	if (m_diffs.size() == 0)
	{
		wxMessageBox("no files to update");
		return;
	}

	wxString temp;
	wxGetTempFileName("zzsam", temp );

	m_output->AppendText("writing zip: " + temp + "\n");
	wxSafeYield( NULL, true );

	wxFFileOutputStream out( temp );
	wxZipOutputStream zip( out );
	if (!zip.IsOk())
	{
		wxMessageBox("Could not create temporary file for zip output");
		return;
	}

	wxString curpath( m_curDir->GetPath() );
	
#define NRWBUFBYTES 4096
	char rwbuf[NRWBUFBYTES];

	for (size_t i=0;i<m_diffs.size();i++)
	{
		zip.PutNextEntry( m_diffs[i]->cur );
		m_output->AppendText( wxString::Format("[%d of %d] zip: ", (int)i+1, (int)m_diffs.size()) + m_diffs[i]->cur + "\n");
		wxSafeYield( NULL, true );

		wxString fn = curpath+ "/" + m_diffs[i]->cur;
		wxFFileInputStream fin( fn ); // file input

		if (!fin.IsOk())
		{
			wxMessageBox("could not open finput stream:", fn);
			return;
		}

		while (!fin.Eof())
		{
			fin.Read(rwbuf, NRWBUFBYTES);
			zip.Write(rwbuf, fin.LastRead());
		}
	}

	zip.Close();
	out.Close();

	m_md5 = wxMD5::GetFileMD5( temp );
	m_output->AppendText("zip ok: md5=" + m_md5 + "\n");
	wxSafeYield( NULL, true );

	m_patchLevel = wxGetTextFromUser("Enter patch level number (1,2,3,...etc):" );

	m_archive = 
		m_platStr + "_" + m_verStr + "_patch" 
		+ m_patchLevel
		+ "_" + m_md5
		+ ".zip";

	
	wxFileDialog dlg( this, "Save patch zip file", 
		curpath, m_archive, 
		"ZIP archive|*.zip", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
	if (dlg.ShowModal() != wxID_OK)
		return;
	
	m_archive = dlg.GetPath();

	wxCopyFile( temp, m_archive );
	if ( wxMD5::GetFileMD5( m_archive ) == m_md5 )
	{
		m_output->AppendText( "patch archive written to: " + dlg.GetPath() + "\n");
		wxRemoveFile( temp );
		m_output->AppendText( wxString::Format("size: %.1lf kb\n", wxFileName::GetSize( dlg.GetPath()).ToDouble()*0.001 ) );
		wxSafeYield( NULL, true );
	}
	else
		m_output->AppendText( "error copying patch file, md5 mismatch\n");


	// help create the patchline to add to the patch file online
	
	PatchFileHelper pfh(this, m_archive, m_platStr, m_verStr, m_patchLevel, m_md5 );
	if ( !m_notes.IsEmpty() )
		pfh.SetNotes( m_notes );
	pfh.ShowModal();
	m_notes = pfh.GetNotes();

}


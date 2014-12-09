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
#include <wx/progdlg.h>
#include <wx/zipstrm.h>
#include <wx/txtstrm.h>
#include <wx/datstrm.h>
#include <wx/tokenzr.h>

#include <wex/utils.h>
#include <wex/md5.h>

#include <wex/extgrid.h>


enum { ID_GRID = wxID_HIGHEST + 941,
	ID_OUTPUT, 
	ID_DIFF,
	ID_PACKAGE,
	ID_FILTER
};

BEGIN_EVENT_TABLE( PackageDialog, wxDialog )
	EVT_BUTTON( ID_DIFF, PackageDialog::OnCommand )
	EVT_BUTTON( ID_PACKAGE, PackageDialog::OnCommand )
	EVT_BUTTON( ID_FILTER, PackageDialog::OnCommand )

END_EVENT_TABLE()

PackageDialog::PackageDialog( wxWindow *parent, const wxString &title,
	const wxString &ver, const wxString &basepath)
	: wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxSize(1100, 800), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)
{
	m_appPath = basepath;
	m_verStr = ver;

	m_curDir = new wxDirPickerCtrl( this, wxID_ANY, m_appPath );
	m_oldDir = new wxDirPickerCtrl( this, wxID_ANY, m_appPath );
	
	wxString dir;
	wxConfig cfg( "SamUpdate", "NREL");
	if (cfg.Read("m_curDir", &dir ))
		m_curDir->SetPath( dir );
	if (cfg.Read("m_oldDir", &dir ))
		m_oldDir->SetPath( dir );
	
	m_grid = new wxExtGridCtrl( this, ID_GRID );
	m_grid->CreateGrid(1,1);
	m_output = new wxTextCtrl( this, ID_OUTPUT, "Ready", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_DONTWRAP );
	m_output->SetFont( wxFont(12, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "Consolas" ));
	m_output->SetForegroundColour( "navy" );
	
	wxBoxSizer *buttons = new wxBoxSizer( wxHORIZONTAL );
	buttons->Add( new wxButton( this, wxID_CANCEL, "Close" ) );
	buttons->Add( new wxButton( this, ID_DIFF, "Generate diff" ) );
	buttons->Add( new wxButton( this, ID_PACKAGE, "Create package" ) );
	buttons->Add( new wxButton( this, ID_FILTER, "Apply filters:" ) );
	
	m_filter = new wxTextCtrl( this, wxID_ANY, 
		"*d.exe;*.ilk;*.tlog;*d.pdb;*.log;*.lastbuildstate;*msvc*.dll;*lib*.lib" );

	wxString buf;
	if (cfg.Read("m_filter", &buf ))
		m_filter->ChangeValue( buf );

	buttons->Add( m_filter, 1, wxALL|wxEXPAND, 3 );

	wxBoxSizer *dirs = new wxBoxSizer( wxHORIZONTAL );
	dirs->Add( new wxStaticText(this, wxID_ANY, "Current version:"), 0, wxALL|wxALIGN_CENTER_VERTICAL );
	dirs->Add( m_curDir, 1, wxALL|wxEXPAND, 0 );
	dirs->Add( new wxStaticText(this, wxID_ANY, "Old version:"), 0, wxALL|wxALIGN_CENTER_VERTICAL );
	dirs->Add( m_oldDir, 1, wxALL|wxEXPAND, 0 );

	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( buttons, 0, wxALL|wxEXPAND, 3 );	
	sizer->Add( dirs, 0, wxALL|wxEXPAND, 3 );
	sizer->Add( m_grid, 5, wxALL|wxEXPAND, 0 );
	sizer->Add( m_output, 2, wxALL|wxEXPAND, 0 );
	SetSizer( sizer );
}

PackageDialog::~PackageDialog()
{
	wxConfig cfg( "SamUpdate", "NREL");
	cfg.Write( "m_curDir", m_curDir->GetPath() );
	cfg.Write( "m_oldDir", m_oldDir->GetPath() );
	cfg.Write( "m_filter", m_filter->GetValue() );
	ClearMaps();
}

void PackageDialog::ScanCurrentFiles( const wxString &basepath, const wxString &path, 
									 int *count, filemap *map )
{
	wxDir dir( path );

	m_output->AppendText("scanning folder: " + path + "\n");
	
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

		if ( c1 == EOF || c2 == EOF )
			break;
	}


	fclose(fp1);
	fclose(fp2);

	return same;
}

void PackageDialog::MakeDiff()
{
	m_output->Clear();
	int count1 = 0, count2 = 0;
	
	ClearMaps();
	ScanCurrentFiles( m_curDir->GetPath(), m_curDir->GetPath(), &count1, &m_curFiles );
	ScanCurrentFiles( m_oldDir->GetPath(), m_oldDir->GetPath(), &count2, &m_oldFiles );
	
	m_output->AppendText( wxString::Format("Scanned: current ver = %d, old ver = %d\n", count1, count2 ) );

	m_diffs.clear();

	int ii=0;
	for ( filemap::iterator cur = m_curFiles.begin();
		cur != m_curFiles.end();
		++cur )
	{
		++ii;
		filemap::iterator old = m_oldFiles.find( cur->first );
		if ( old != m_oldFiles.end() )
		{
			if ( !Identical( m_curDir->GetPath() + "/" + cur->second->relpath,
				m_oldDir->GetPath() + "/" + old->second->relpath ))
			{
				DiffInfo di;
				di.cur = cur->second->relpath;
				di.cur_time = cur->second->lastmod;
				di.old = old->second->relpath;
				di.old_time = old->second->lastmod;
				di.bindiff = true;
				m_diffs.push_back( di );
			}
			else
			{
				m_output->AppendText(wxString::Format("[%d of %d] ignore (no changes): ",ii, count1) + cur->second->relpath  + "\n");
			}
		}
		else
		{
			DiffInfo di;
			di.cur = cur->second->relpath;
			di.cur_time = cur->second->lastmod;
			di.bindiff = false;
			m_diffs.push_back( di );
		}
	}

	// sort the selections by file names
	DiffInfo di;
	size_t count = m_diffs.size();
	for (size_t i=0;i<count-1;i++)
	{
		int smallest = i;

		for (size_t j=i+1;j<count;j++)
			if ( m_diffs[j].cur < m_diffs[smallest].cur )
				smallest = j;

		// swap
		di = m_diffs[i];
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
		m_grid->SetCellValue( m_diffs[i].cur, i, 0 );
		m_grid->SetCellValue( m_diffs[i].cur_time.FormatDate() + " " + m_diffs[i].cur_time.FormatTime(), i, 1 );
		m_grid->SetCellValue( m_diffs[i].bindiff ? "UPDATED" : "NEW", i, 2 );		
		m_grid->SetCellBackgroundColour(  m_diffs[i].bindiff ? "salmon" : "sea green", i, 2 );
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
			if ( wxMatchWild( filters[j], m_diffs[i].cur, false ) )
				remove = true;

		if ( remove )
			m_diffs.erase( m_diffs.begin() + i );
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

	wxFFileOutputStream out( temp );
	wxZipOutputStream zip( out );
	if (!zip.IsOk())
	{
		wxMessageBox("Could not create temporary file for zip output");
		return;
	}
	
#define NRWBUFBYTES 4096
	char rwbuf[NRWBUFBYTES];

	for (size_t i=0;i<m_diffs.size();i++)
	{
		zip.PutNextEntry( m_diffs[i].cur );
		m_output->AppendText( wxString::Format("[%d of %d] zip: ", (int)i+1, (int)m_diffs.size()) + m_diffs[i].cur + "\n");

		wxString fn = m_curDir->GetPath() + "/" + m_diffs[i].cur;
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

	wxString md5 = wxMD5::GetFileMD5( temp );
	m_output->AppendText("zip ok: md5=" + md5 + "\n");
	wxFileDialog dlg( this, "Save patch zip file", 
		m_curDir->GetPath(), m_verStr + "_patch_" + md5 + ".zip", 
		"ZIP archive|*.zip", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
	if (dlg.ShowModal() != wxID_OK)
		return;

	wxCopyFile( temp, dlg.GetPath() );
	if ( wxMD5::GetFileMD5( dlg.GetPath() ) == md5 )
	{
		m_output->AppendText( "patch archive written to: " + dlg.GetPath() + "\n");
		wxRemoveFile( temp );
		m_output->AppendText( wxString::Format("size: %.1lf kb\n", wxFileName::GetSize( dlg.GetPath()).ToDouble()*0.001 ) );
	}
	else
		m_output->AppendText( "error copying patch file\n");

	
}

#ifndef __pkgdlg_h
#define __pkgdlg_h

#include <vector>
#include <wx/wx.h>


#include <unordered_map>
using std::unordered_map;
#pragma warning(disable: 4290)  // ignore warning: 'C++ exception specification ignored except to indicate a function is not __declspec(nothrow)'


class wxDirPickerCtrl;
class wxExtGridCtrl;

#include <wx/html/htmlwin.h>

class MyHtmlWindow : public wxHtmlWindow
{
public:
	MyHtmlWindow( wxWindow *parent, int id );
	void OnLinkClicked( wxHtmlLinkEvent &evt );
	DECLARE_EVENT_TABLE();
};

struct FileInfo
{
	wxString relpath;
	wxDateTime lastmod;

};
typedef unordered_map< wxString, FileInfo*, wxStringHash, wxStringEqual> filemap;

class PackageDialog : public wxDialog
{
public:
	PackageDialog( wxWindow *parent, const wxString &title,
		const wxString &ver, const wxString &basepath,
		const wxString &plat);
	virtual ~PackageDialog();

	
private:
	void SaveStateToConfig();
	void OnCommand( wxCommandEvent & );

	wxExtGridCtrl *m_grid;
	wxTextCtrl *m_output;
	wxTextCtrl *m_filter;

	wxDirPickerCtrl *m_curDir;
	wxDirPickerCtrl *m_oldDir;


	void ClearMaps();
	void ClearDiffs();

	filemap m_curFiles;
	filemap m_oldFiles;

	wxString m_md5;
	wxString m_patchLevel;
	wxString m_archive;
	wxString m_notes;
	wxString m_verStr;
	wxString m_appPath;
	wxString m_platStr;
		
	void ScanCurrentFiles( const wxString &basepath, const wxString &path, int *count,
		filemap *map );
	void MakeDiff();
	void FilterResults();

	bool Identical( const wxString &file1, const wxString &file2 );

	struct DiffInfo
	{
		wxString old;
		wxDateTime old_time;
		wxString cur;
		wxDateTime cur_time;
		bool bindiff;
	};

	std::vector<DiffInfo*> m_diffs;

	void UpdateGrid();

	
	void MakePackage();


	DECLARE_EVENT_TABLE();
};

#endif

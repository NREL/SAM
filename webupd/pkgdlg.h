#ifndef __pkgdlg_h
#define __pkgdlg_h

#include <vector>
#include <wx/wx.h>

#ifdef _MSC_VER
#include <unordered_map>
using std::tr1::unordered_map;
#else
#include <tr1/unordered_map>
using std::tr1::unordered_map;
#endif



class wxDirPickerCtrl;
class wxExtGridCtrl;


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
		const wxString &ver, const wxString &basepath);
	virtual ~PackageDialog();

	
private:
	void OnCommand( wxCommandEvent & );

	wxExtGridCtrl *m_grid;
	wxTextCtrl *m_output;
	wxTextCtrl *m_filter;

	wxDirPickerCtrl *m_curDir;
	wxDirPickerCtrl *m_oldDir;


	void ClearMaps();

	filemap m_curFiles;
	filemap m_oldFiles;

	wxString m_verStr;
	wxString m_appPath;
		
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

	std::vector<DiffInfo> m_diffs;

	void UpdateGrid();

	
	void MakePackage();


	DECLARE_EVENT_TABLE();
};

#endif

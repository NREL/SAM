#ifndef __library_h
#define __library_h

#include <wx/string.h>
#include <wx/listctrl.h>
#include <wx/panel.h>

#include <wex/csv.h>

#include "variables.h"


/* Example library.  CSV Format

Name,Area,Pmp,Vmp,Imp,Voc,Isc,a,Il,Io,Rs,Rsh,gamma,NOCT
Units,m2,W,V,A,V,A,n/a,...
[0],pv.mod.cec.area,pv.mod.cec.pmp,pv.mod.cec.vmp,pv.mod.cec.imp,...
[1],pv.mod.cecuser.area,pv.mod.cecuser.pmp,...
Sunpower-215,2,5,1,3,4,4,4,2,4,5
Sunpower-330,4,4,1,5,3,2,5,3,2,1

*/

class Library
{
public:
	Library();

	static Library *Load( const wxString &file );
	static void UnloadAll();
	static Library *Find( const wxString &name );
	
	struct Field
	{
		wxString Name;
		wxString Units;
		wxArrayString Variables;
		int DataIndex;
	};

	bool Read( const wxString &file );
	bool Read( const wxCSVData &data, const wxString &name );
	wxString GetName() const;	
	wxArrayString &GetErrors() { return m_errors; }

	wxArrayString ListEntries();
	size_t NumEntries();
	std::vector<Field> &GetFields();
	int GetFieldIndex( const wxString &name );
	int FindEntry( const wxString &name );
	wxString GetEntryValue( int entry, int field );
	wxString GetEntryName( int entry );
	bool ApplyEntry( int entry, int varindex, VarTable &tab, wxArrayString &changed );

private:
	wxCSVData m_csv;

	bool ScanData();

	wxString m_name;
	std::vector<Field> m_fields;
	size_t m_startRow;
	wxArrayString m_errors;
};

bool ScanSolarResourceData();


class LibraryCtrl;
class wxTextCtrl;
class wxStaticText;
class wxChoice;

class LibraryListView : public wxListView
{
	LibraryCtrl *m_libctrl;
public:
	LibraryListView( LibraryCtrl *parent, int id, const wxPoint &pos = wxDefaultPosition,
		const wxSize &size = wxDefaultSize );
	
	virtual wxString OnGetItemText( long item, long col ) const;
//	virtual wxListItemAttr *OnGetItemAttr( long item ) const;
};

#define EVT_LIBRARYCTRL(id, func) EVT_LISTBOX(id,func)

class LibraryCtrl : public wxPanel
{
public:
	LibraryCtrl( wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition,
		const wxSize &size = wxDefaultSize );
	virtual ~LibraryCtrl();

	void SetLabel( const wxString &label );

	void SetLibrary( const wxString &name, const wxString &fields );
	void ReloadLibrary();

	bool SetEntrySelection( const wxString &entry );
	wxString GetEntrySelection();


	wxString GetCellValue( long item, long col );
	void UpdateList();

protected:
	void OnSelected( wxListEvent & );
	void OnColClick( wxListEvent & );
	void OnCommand( wxCommandEvent & );
	
private:
	wxString m_library;
	bool m_inclEntryName;

	std::vector<bool> m_sortDir;

	wxArrayString m_fields;
	std::vector<size_t> m_fieldMap;

	wxArrayString m_entries;

	struct viewable
	{
		viewable( const wxString &n, size_t i ) : name(n), index(i) { }
		wxString name;
		size_t index;
	};
	
	class viewable_compare
	{
		wxArrayString &col;
		bool dir;
	public:
		viewable_compare( wxArrayString &c,  bool d ) : col(c), dir(d) { }
		bool operator() ( const viewable &lhs, const viewable &rhs );
	};

	std::vector<viewable> m_view;

	wxStaticText *m_label;
	wxTextCtrl *m_filter;
	wxStaticText *m_notify;
	wxChoice *m_target;
	LibraryListView *m_list;

	bool m_sendEvents;
	
	
	DECLARE_EVENT_TABLE();
};



#endif

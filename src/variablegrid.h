#ifndef __variablegrid_h
#define __variablegrid_h

#include <wx/string.h>
#include <wx/listctrl.h>
#include <wx/panel.h>

#include "variables.h"
#include "case.h"


/* Example variablegrid.
*/

class VariableGrid
{
public:
	VariableGrid();

	static VariableGrid *Load(std::vector<Case*> &cases);
	static void UnloadAll();
	static VariableGrid *Find( const wxString &name );

	struct Field
	{
		wxString Name;
		wxString Units;
		wxArrayString Variables;
		int DataIndex;
	};

	bool Read( std::vector<Case*> &cases );
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

	bool ScanData();

	wxString m_name;
	std::vector<Field> m_fields;
	size_t m_startRow;
	wxArrayString m_errors;
};


class VariableGridCtrl;
class wxTextCtrl;
class wxStaticText;
class wxChoice;

class VariableGridListView : public wxListView
{
	VariableGridCtrl *m_variablegridctrl;
public:
	VariableGridListView( VariableGridCtrl *parent, int id, const wxPoint &pos = wxDefaultPosition,
		const wxSize &size = wxDefaultSize );

	virtual wxString OnGetItemText( long item, long col ) const;
};

#define EVT_VARIABLEGRIDCTRL(id, func) EVT_LISTBOX(id,func)

class VariableGridCtrl : public wxPanel
{
public:
	VariableGridCtrl( wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition,
		const wxSize &size = wxDefaultSize );
	virtual ~VariableGridCtrl();

	void SetLabel( const wxString &label );

	void SetVariableGrid( const wxString &name, const wxString &fields );
	void ReloadVariableGrid();

	bool SetEntrySelection( const wxString &entry );
	wxString GetEntrySelection();


	wxString GetCellValue( long item, long col );
	void UpdateList();

protected:
	void OnSelected( wxListEvent & );
	void OnColClick( wxListEvent & );
	void OnCommand( wxCommandEvent & );

private:
	wxString m_variablegrid;
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
	VariableGridListView *m_list;

	bool m_sendEvents;


	DECLARE_EVENT_TABLE();
};



#endif

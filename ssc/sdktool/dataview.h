#ifndef __dataview_h
#define __dataview_h

#include <vector>

#include <wx/panel.h>
#include <wx/checklst.h>
#include <wx/treebase.h>
#include <wx/grid.h>

#include "dllinvoke.h"

class wxExtGridCtrl;

class DataView : public wxPanel
{
public:

	class Table; // forward

	DataView( wxWindow *parent );
	virtual ~DataView() { m_vt = NULL; }

	void SetDataObject( var_table *vt ) { m_vt = vt; UpdateView(); }
	ssc_data_t GetDataObject() { return m_vt; }

	void UpdateView();	
	void UpdateGrid();
	virtual void Freeze();
	virtual void Thaw();


	std::vector<int> GetColumnWidths();
	void SetColumnWidths( const std::vector<int> &cwl );
	wxArrayString GetSelections();
	void SetSelections(const wxArrayString &sel);

	wxString GetSelection();

	void AddVariable();
	void EditVariable(wxString name=wxEmptyString);
	void DeleteVariable(wxString name=wxEmptyString);
	void ShowStats( wxString name=wxEmptyString );

private:
	void OnCommand(wxCommandEvent &evt);
	void OnVarListCheck(wxCommandEvent &evt);
	void OnVarListDClick(wxCommandEvent &evt);
	void OnPopup( wxCommandEvent &evt);

	void OnGridLabelRightClick(wxGridEvent &evt);
	void OnGridLabelDoubleClick(wxGridEvent &evt);

	bool m_frozen;
	wxExtGridCtrl *m_grid;
	Table *m_grid_table;
	wxCheckListBox *m_varlist;

	wxTreeItemId m_root_item;
	std::vector<wxTreeItemId> m_tree_items;
	wxArrayString m_names;
	wxArrayString m_selections;

	wxString m_popup_var_name;

	var_table *m_vt;

	DECLARE_EVENT_TABLE();
};


class wxExtGridCtrl;
class wxNumericCtrl;

class StatDialog: public wxDialog
{
public:
	StatDialog(wxWindow *parent, const wxString &title);

	void Compute( util::matrix_t<ssc_number_t> &val );

private:
	wxExtGridCtrl *grdMonthly;
	wxNumericCtrl *numSumOver1000;
	wxNumericCtrl *numSum;
	wxNumericCtrl *numMax;
	wxNumericCtrl *numMean;
	wxNumericCtrl *numMin;
};


#endif

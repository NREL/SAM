#ifndef __variablegrid_h
#define __variablegrid_h

#include <wx/string.h>
#include <wx/grid.h>
#include <wx/frame.h>
#include <wx/renderer.h>
#include <wx/event.h>
#include <wx/dc.h>
#include <wx/button.h>
#include <wx/stattext.h>

#include <wex/uiform.h>
#include <wex/numeric.h>
#include <wex/exttext.h>
#include <wex/extgrid.h>
#include <wex/radiochoice.h>
#include <wex/diurnal.h>

#include "variables.h"
#include "project.h"
#include "case.h"
// UI widgets
#include "widgets.h"
#include "adjfac.h"
#include "ptlayoutctrl.h"
#include "troughloop.h"
#include "materials.h"
#include "shadingfactors.h"
#include "library.h"

#include <vector>

class VariableGridData : public wxGridTableBase
{
public:
	VariableGridData(ProjectFile *pf, Case *c = NULL);
	virtual int GetNumberRows();
	virtual int GetNumberCols();
	virtual bool IsEmptyCell(int row, int col);
	virtual wxString GetValue(int row, int col);
	virtual void SetValue(int row, int col, const wxString& value);
	virtual wxString GetColLabelValue(int col);
	virtual void SetColLabelValue(int col, const wxString &label);
	virtual wxString GetTypeName(int row, int col);
	virtual bool DeleteCols(size_t pos = 0, size_t	numCols = 1);
//	virtual bool InsertCols(size_t pos = 0, size_t numCols = 1);
	virtual bool AppendCols(size_t numCols = 1);
	bool DeleteCase(Case *c);
	bool AddCase(Case *c);
	bool RenameCase(const wxString &old_name, const wxString &new_name);
	bool ShowRow(int row, int comparison_type);
	void Sort(int col, bool ascending);

	VarInfo* GetVarInfo(int row, int col);
	void SetVarInfo(int row, int col, VarInfo *vi);
	VarValue* GetVarValue(int row, int col);
	void SetVarValue(int row, int col, VarValue *vv);

private:
	void Init();
	int m_rows;
	int m_cols;
	bool m_sorted;
	ProjectFile *m_pf;
	wxArrayInt m_sorted_index;
	std::vector<Case*> m_cases;
	wxArrayString m_col_hdrs;
	wxArrayString m_var_names;
	wxArrayString m_var_labels;
	std::vector<VarTable*> m_var_table_vec;
	std::vector<VarInfoLookup*> m_var_info_lookup_vec;
};


class GridCellVarValueRenderer : public wxGridCellRenderer
{
public:
	GridCellVarValueRenderer();
	virtual ~GridCellVarValueRenderer(void);

	virtual void Draw(wxGrid &grid, wxGridCellAttr &attr, wxDC &dc, const wxRect &rect, int row, int col, bool isSelected);
	virtual wxSize GetBestSize(wxGrid &grid, wxGridCellAttr& attr, wxDC &dc, int row, int col);
	virtual wxGridCellRenderer *Clone() const;
	void SetTextColoursAndFont(const wxGrid& grid, const wxGridCellAttr& attr, wxDC& dc, bool isSelected);

private:
	wxString m_ellipsis;

};

class VariablePopupPanel : public wxPanel
{
public:
	VariablePopupPanel(wxWindow *parent, VarInfo *vi, VarValue *vv, wxString &var_name);
	~VariablePopupPanel();

	wxUIObject *GetUIObject() { return m_obj; }
private:
	wxUIObject *m_obj;
	VarInfo *m_vi;
	VarValue *m_vv;
	wxUIFormData *m_form_data;
	wxString m_var_name;
};

class VariablePopupDialog : public wxDialog
{
public:
	VariablePopupDialog(wxWindow *parent, VarInfo *vi, VarValue *vv, wxString &var_name);
	~VariablePopupDialog();

	wxUIObject *GetUIObject();
private:
	VariablePopupPanel *m_panel;
	VarInfo *m_vi;
	VarValue *m_vv;
	wxUIFormData *m_form_data;
	wxString m_var_name;
};


class GridCellVarValueEditor : public wxEvtHandler, public wxGridCellEditor
{
public:
	GridCellVarValueEditor();
	virtual ~GridCellVarValueEditor(void);

	virtual void PaintBackground(wxDC& dc,
		const wxRect& rectCell,
		const wxGridCellAttr& attr);

	virtual void Create(wxWindow *parent, wxWindowID id, wxEvtHandler* pEvtHandler);
	virtual bool IsAcceptedKey(wxKeyEvent& event);
	virtual void SetSize(const wxRect &rect_orig);
	virtual void BeginEdit(int row, int col, wxGrid *pGrid);
	virtual bool EndEdit(int row, int col, const wxGrid *grid, const wxString &oldval, wxString *newval);
	virtual void ApplyEdit(int row, int col, wxGrid *grid);
	virtual void Reset();
	virtual wxString GetValue() const;
	virtual wxGridCellEditor *Clone() const;

private:
	wxString m_cell_value;
	wxStaticText *m_text;
	VarInfo *m_var_info;
	VarValue *m_var_value;
	wxWindow *m_parent;
	VariablePopupDialog *m_vpe;

	DECLARE_NO_COPY_CLASS(GridCellVarValueEditor)
};




class VariableGrid : public wxGrid
{
public:
	VariableGrid(wxWindow *parent, wxWindowID id, const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize,
		long style = wxWANTS_CHARS, const wxString& name = wxPanelNameStr);
	virtual ~VariableGrid();

	void OnLeftClick(wxGridEvent &evt);

	DECLARE_EVENT_TABLE()
};

class VariableGridFrame : public wxFrame, ProjectFileEventListener, CaseEventListener
{
public:
	VariableGridFrame(wxWindow *parent, ProjectFile *pf, Case *c=NULL);
	~VariableGridFrame();
	void UpdateGrid();
private:
	wxGrid *m_grid;
	ProjectFile *m_pf;
	bool m_input_list;
	VariableGridData *m_griddata;
	std::vector<Case*> m_cases;
	int m_compare_show_type;

	void OnGridColSort(wxGridEvent& event);
	void SizeColumns();
	void OnCommand(wxCommandEvent &evt);

	// update data when values in case change
	virtual void OnCaseEvent(Case *c, CaseEvent &evt);
	virtual void OnProjectFileEvent(ProjectFile *p, ProjectFileEvent &evt);

	DECLARE_EVENT_TABLE();
};

#endif

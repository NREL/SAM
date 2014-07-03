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
#include "gridsupport.h"

#include <vector>

class VariableGridData : public GridChoiceData
{
public:
	VariableGridData(ProjectFile *pf, Case *c = NULL, VarTable *vt = NULL);
	virtual int GetNumberRows();
	virtual int GetNumberCols();
	virtual bool IsEmptyCell(int row, int col);
	virtual wxString GetValue(int row, int col);
	virtual void SetValue(int row, int col, const wxString& value);
	virtual wxString GetColLabelValue(int col);
	virtual void SetColLabelValue(int col, const wxString &label);
	virtual wxString GetTypeName(int row, int col);
	virtual bool DeleteCols(size_t pos = 0, size_t	numCols = 1);
	virtual bool AppendCols(size_t numCols = 1);
	bool DeleteCase(Case *c);
	bool AddCase(Case *c);
	bool RenameCase(const wxString &old_name, const wxString &new_name);
	bool ShowRow(int row, int comparison_type);
	void Sort(int col, bool ascending);

	// for choice controls
	wxString GetChoices(int row, int col);
	wxString GetVarName(int row, int col);

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
	VarTable *m_vt;
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
	VariableGridFrame(wxWindow *parent, ProjectFile *pf, Case *c=NULL, VarTable *vt=NULL, wxString frame_title=wxEmptyString);
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

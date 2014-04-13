#ifndef __variablegrid_h
#define __variablegrid_h

#include <wx/string.h>
#include <wx/grid.h>
#include <wx/frame.h>

#include "variables.h"
#include "project.h"
#include "case.h"

#include <vector>

class VariableGridData : public wxGridTableBase
{
public:
	VariableGridData(std::vector<Case *> &cases, wxArrayString &case_names);
	virtual int GetNumberRows();
	virtual int GetNumberCols();
//	virtual bool IsEmptyCell(int row, int col);
	virtual wxString GetValue(int row, int col);
	virtual void SetValue(int row, int col, const wxString& value);
	virtual wxString GetColLabelValue(int col);
	void Sort(int col, bool ascending);

private:
	int m_rows;
	int m_cols;
	bool m_sorted;
	wxArrayInt m_sorted_index;
	std::vector<Case*> m_cases;
	wxArrayString m_col_hdrs;
	wxArrayString m_var_names;
	wxArrayString m_var_labels;
	std::vector<VarTable> m_var_table_vec;
	std::vector<VarInfoLookup> m_var_info_lookup_vec;
};

class VariableGridFrame : public wxFrame
{
public:
	VariableGridFrame(wxWindow *parent, std::vector<Case *> &cases, wxArrayString &case_names);
private:
	wxGrid *m_grid;
	VariableGridData *m_griddata;

	void OnGridColSort(wxGridEvent& event);

	DECLARE_EVENT_TABLE();
};

#endif

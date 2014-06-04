#ifndef __parametric_h
#define __parametric_h

#include <wx/datstrm.h>
#include <wx/panel.h>
#include <wx/grid.h>

#include <wex/numeric.h>
#include <wex/extgrid.h>

#include "simulation.h"
#include "object.h"

class Case;

class ParametricData
{
public:
	ParametricData( Case *c );
	~ParametricData();

	void ClearRuns();
	void Copy(ParametricData &rhs);

	struct Var {
		wxString Name;
		std::vector<VarValue> Values;
	};
	std::vector<Var> Setup;
	std::vector<Simulation*> Runs;

	int FindSetup(wxString &name);
	bool RemoveSetup(wxString &name);

	void Write( wxOutputStream & );
	bool Read( wxInputStream & );

	Case *GetCase() { return m_case; }
private:
	Case *m_case;
};


class ParametricGridData : public wxGridTableBase
{
public:
	ParametricGridData(Case *cc);
	virtual int GetNumberRows();
	virtual int GetNumberCols();
	virtual bool IsEmptyCell(int row, int col);
	virtual wxString GetValue(int row, int col);
	virtual void SetValue(int row, int col, const wxString& value);
	virtual wxString GetColLabelValue(int col);
	virtual void SetColLabelValue(int col, const wxString &label);
	virtual wxString GetTypeName(int row, int col);
	virtual bool AppendRows(size_t nrows);
	virtual bool InsertRows(size_t pos=0, size_t nrows=1);
	virtual bool DeleteRows(size_t pos=0, size_t nrows=1);
	virtual bool AppendCols(size_t ncols=1);
	virtual bool InsertCols(size_t pos=0, size_t ncols=1);
	virtual bool DeleteCols(size_t pos=0, size_t ncols=1);

	// for choice controls
	wxString GetChoices(int row, int col);

	bool IsInput(int col);
	VarInfo* GetVarInfo(int row, int col);
	void SetVarInfo(int row, int col, VarInfo *vi);
	VarValue* GetVarValue(int row, int col);
	void SetVarValue(int row, int col, VarValue *vv);
	void Init();

	wxArrayString GetVarNames();

	void AddSetup(ParametricData::Var &var);
	void DeleteSetup(wxString &var_name);
	void UpdateNumberRows(int rows);
	bool RunSimulations(int row = -1);
	void ClearResults(int row = -1);
	void Configure(wxArrayString &var_names);
	void UpdateView();

private:
	int m_rows;
	int m_cols;
	wxArrayString m_col_hdrs;
	ParametricData &m_par;
	Case *m_case;
	wxArrayString m_var_names;
};



class ParametricGrid : public wxExtGridCtrl
{
public:
	ParametricGrid(wxWindow *parent, wxWindowID id, const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize,
		long style = wxWANTS_CHARS, const wxString& name = wxPanelNameStr);
	virtual ~ParametricGrid();

	void OnLeftClick(wxGridEvent &evt);

	DECLARE_EVENT_TABLE()
};


class ParametricViewer : public wxPanel
{
public:
	ParametricViewer( wxWindow *parent, Case *cc );

private:
	void OnCommand(wxCommandEvent &evt);
	void OnGridColLabelRightClick(wxGridEvent &evt);

	void Configure();
	void UpdateGrid();
	void UpdateNumRuns();
	void RunSimulations();
	void ClearResults();

	ParametricGrid *m_grid;
	ParametricGridData *m_grid_data;
	wxNumericCtrl *m_num_runs_ctrl;

	Case *m_case;
	wxArrayString m_var_names;

	DECLARE_EVENT_TABLE();
};


#endif


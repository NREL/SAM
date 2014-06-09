#ifndef __parametric_h
#define __parametric_h

#include <wx/datstrm.h>
#include <wx/panel.h>
#include <wx/grid.h>

#include <wex/numeric.h>
#include <wex/extgrid.h>

#include "simulation.h"
#include "object.h"
#include "gridsupport.h"


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


class ParametricGridData : public GridChoiceData
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
	bool IsInput(wxString &var_name);
	VarInfo* GetVarInfo(int row, int col);
	void SetVarInfo(int row, int col, VarInfo *vi);
	VarValue* GetVarValue(int row, int col);
	void SetVarValue(int row, int col, VarValue *vv);
	void Init();
	void UpdateSetup();

	wxArrayString GetInputNames();
	wxArrayString GetOutputNames();

	void AddSetup(ParametricData::Var &var);
	void DeleteSetup(wxString &var_name);
	void UpdateNumberRows(int rows);
	bool RunSimulations(int row = -1);
	void ClearResults(int row = -1);
	void UpdateInputs(wxArrayString &input_names);
	void UpdateOutputs(wxArrayString &output_names);
	void UpdateView();

	double GetDouble(int row, int col);
	std::vector<float> GetArray(int row, int col);
	float *GetArray(int row, int col, size_t *n);
	wxString GetUnits(int col);
	wxString GetVarName(int col);

private:
	int m_rows;
	int m_cols;
	wxArrayString m_col_hdrs;
	ParametricData &m_par;
	Case *m_case;
	wxArrayString m_input_names;
	wxArrayString m_output_names;
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
	void OnMenuItem(wxCommandEvent &evt);

	void SelectInputs();
	void SelectOutputs();
	void UpdateGrid();
	void UpdateNumRuns();
	void RunSimulations();
	void ClearResults();

	void AddPlot();
	void RemovePlot();
	bool Plot();

	ParametricGrid *m_grid;
	ParametricGridData *m_grid_data;
	wxNumericCtrl *m_num_runs_ctrl;

	Case *m_case;
	wxArrayString m_input_names;
	wxArrayString m_output_names;
	wxArrayString m_plot_var_names;
	wxBoxSizer *m_par_sizer;

	int m_selected_grid_col;
	int m_fixed_sizer_count;

	DECLARE_EVENT_TABLE();
};


#endif


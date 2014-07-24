#ifndef __parametric_h
#define __parametric_h

#include <wx/datstrm.h>
#include <wx/panel.h>
#include <wx/grid.h>

#include <wex/numeric.h>
//#include <wex/extgrid.h>

#include "simulation.h"
#include "object.h"
#include "gridsupport.h"
#include "graph.h"


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
	~ParametricGridData();
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

	// set cell background based on input and output status
	virtual wxGridCellAttr *GetAttr(int row, int col, wxGridCellAttr::wxAttrKind kind);

	// for choice controls
	wxString GetChoices(int row, int col);
	int GetMaxChoice(int row, int col);
	wxString GetChoice(int row, int col); // actual string choice
	wxString GetVarName(int row, int col);

	bool IsInput(int col);
	bool IsInput(wxString &var_name);
	VarInfo* GetVarInfo(int row, int col);
	void SetVarInfo(int row, int col, VarInfo *vi);
	VarValue* GetVarValue(int row, int col);
	int GetColumnForName(const wxString &name);
	void SetVarValue(int row, int col, VarValue *vv);
	void Init();
	void UpdateSetup();
	void UpdateView();

	wxArrayString GetInputNames();
	wxArrayString GetOutputNames();

	void AddSetup(ParametricData::Var &var);
	void DeleteSetup(wxString &var_name);
	void UpdateNumberRows(int rows);
	bool RunSimulations_single();
	bool RunSimulations_multi();
	void ClearResults();
	void UpdateInputs(wxArrayString &input_names);
	void UpdateOutputs(wxArrayString &output_names);

	double GetDouble(int row, int col);
	std::vector<float> GetArray(int row, int col);
	float *GetArray(int row, int col, size_t *n);
	wxString GetUnits(int col);

	void FillDown(int col, int rows=2);
	void FillEvenly(int col);

	std::vector<Simulation *> GetRuns();

private:
	int m_rows;
	int m_cols;
	wxArrayString m_col_hdrs;
	ParametricData &m_par;
	Case *m_case;
	wxArrayString m_input_names;
	wxArrayString m_output_names;
	wxArrayString m_var_names;

	wxGridCellAttr *m_attr_for_inputs;
	wxGridCellAttr *m_attr_for_valid_outputs;
	wxGridCellAttr *m_attr_for_invalid_outputs;
	wxColour m_color_for_inputs;
	wxColour m_color_for_valid_outputs;
	wxColour m_color_for_invalid_outputs;

	std::vector<bool> m_valid_run;
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

	void Setup();

	GraphCtrl *CreateNewGraph();
	void DeleteGraph(GraphCtrl *);
	void DeleteAll();

	void SetGraphs(std::vector<Graph> &gl);
	void GetGraphs(std::vector<Graph> &gl);

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

	void AddPlot(const wxString &output_name);
	void RemovePlot(const wxString &output_name);
	bool Plot(int col, Graph &g);
	void AddAllPlots();
	void RemoveAllPlots();

	void FillDown(int rows);

	void ShowAllData();

	void CopyToClipboard();
	void SaveToCSV();
	void SendToExcel();
	void GetTextData(wxString &dat, char sep);

	ParametricGrid *m_grid;
	ParametricGridData *m_grid_data;
	wxNumericCtrl *m_num_runs_ctrl;
	wxCheckBox *m_run_multithreaded;

	Case *m_case;
	wxArrayString m_input_names;
	wxArrayString m_output_names;
	wxArrayString m_plot_var_names;

	int m_selected_grid_col;
	int m_selected_grid_row;

	void UpdateGraph();
	GraphCtrl* CurrentGraph();
	void OnGraphSelect(wxCommandEvent &);
	void SetCurrent(GraphCtrl *gc);

	GraphCtrl *m_current_graph;
	wxSnapLayout *m_layout;
	std::vector<wxWindow*> m_graphs;

	DECLARE_EVENT_TABLE();
};


#endif


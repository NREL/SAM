/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef __parametric_h
#define __parametric_h

#include <wx/datstrm.h>
#include <wx/panel.h>
#include <wx/grid.h>

#include <wex/numeric.h>
#include <wex/radiochoice.h>

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

	std::vector<wxArrayString> QuickSetup;
	size_t QuickSetupMode;

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
	bool Generate_lk();
	bool RunSimulations_multi();
	void ClearResults(int row);
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

	void Generate_lk();

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



class Parametric_QS : public wxDialog
{
public:
	Parametric_QS(wxWindow *parent, Case *c);
	void UpdateFromParametricData();
	ParametricData &GetParametricData();

	void OnEnableChange(wxCommandEvent &evt);
	void OnRemoveVariable(wxCommandEvent &evt);
	void OnAddVariable(wxCommandEvent &evt);
	void OnVariableSelect(wxCommandEvent &evt);
	void OnVarDblClick(wxCommandEvent &evt);
	void OnEditValues(wxCommandEvent &evt);
	void OnValueDblClick(wxCommandEvent &evt);

	void RefreshVariableList();
	void RefreshValuesList();

	wxString GetBaseCaseValue(const wxString &varname);
	wxArrayString GetValuesList(const wxString &varname);
	wxArrayString GetValuesDisplayList(const wxString &varname);
	void SetValuesList(const wxString &varname, const wxArrayString &values);

	bool ShowEditValuesDialog(const wxString &title,
		wxArrayString &values, const wxString &varname);
	bool ShowNumericValuesDialog(const wxString &title, wxArrayString &values);
	bool ShowFixedDomainDialog(const wxString &title,
		const wxArrayString &names, const wxArrayString &labels, wxArrayString &list,
		bool expand_all);

	void UpdateCaseParametricData();

	void OnCommand(wxCommandEvent &evt);

	size_t UpdateNumberRuns();
	bool UpdateLinkedEnabled();

private:
	wxListBox *lstValues;
	wxListBox *lstVariables;
	wxRadioChoice *rchSetupOption;
	wxNumericCtrl *numberRuns;

	Case *m_case;
	wxArrayString m_input_names;
	std::vector<wxArrayString> m_input_values;

	DECLARE_EVENT_TABLE()
};


#endif


/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef __ResultsViewer_h
#define __ResultsViewer_h

#include <wx/datstrm.h>
#include <wex/metro.h>

#include <wex/dview/dvtimeseriesdataset.h>
#include <wex/dview/dvplotctrlsettings.h>
#include <wex/plot/plplotctrl.h>

#include "object.h"
#include "case.h"

#include <map>
#include <vector>

class wxSearchCtrl;
class wxDVSelectionListCtrl;
class CaseWindow;
class Case;
class wxExtGridCtrl;
class wxDVPlotCtrl;
class wxDVTimeSeriesCtrl;
class wxDVDMapCtrl;
class wxDVProfileCtrl;
class wxDVPnCdfCtrl;
class wxDVDCCtrl;
class wxDVScatterPlotCtrl;
class wxDVStatisticsTableCtrl;
class wxPLPlotCtrl;
class wxTextCtrl;
class wxListBox;
class wxSimplebook;
class MetricsTable;
class TabularBrowser;
class wxMetroListBox;
class wxDVTimeSeriesDataSet;
class ConfigInfo;
class GraphViewer; 
class UncertaintiesViewer;

class wxSnapLayout;

class Case;
class Simulation;
class VarTable;
class ResultsViewer;
class LossDiagramCtrl;



//#define EXP_GRAPHDATA         0x01
#define EXP_CASHFLOW          0x02
//#define EXP_BASE_ALL          0x04

#define EXP_COPY_CLIPBOARD    0x01
#define EXP_SAVE_CSV          0x02
#define EXP_SEND_EXCEL        0x04

void PopulateSelectionList(wxDVSelectionListCtrl *sel, wxArrayString *names, Simulation *results);
void UpdateSelectionList(wxDVSelectionListCtrl *sel, wxArrayString *names, Simulation *results, wxString srch, wxArrayString &selected);

static std::map<wxString, wxString> group_by_name;

class ResultsCallbackContext  : public CaseCallbackContext
{
	ResultsViewer *m_resview = nullptr;
public:
	ResultsCallbackContext( ResultsViewer *rv, const wxString &desc = wxEmptyString );	
	ResultsViewer *GetResultsViewer();
	Simulation *GetSimulation();
protected:
	virtual void SetupLibraries( lk::env_t *env );
};
	
struct MetricData {
	MetricData() :
		scale(1.0), mode('g'), thousep(false), deci(2)
	{}
	wxString var;
	wxString label;
	double scale;
	char mode;
	bool thousep;
	int deci;
	wxString pre, post;
};

struct MetricRow {
	wxString label;
	wxString tableName; 
	std::vector<MetricData> metrics;
};

struct MetricTable {
	wxArrayString headers;
	wxString tableName;
};

struct CashFlowLine {
	enum { SPACER, HEADER, VARIABLE, CELLHEADER, CELLVARIABLE, CELLCOLHEADER };
	CashFlowLine() : type(VARIABLE), digits(2), scale(1.0f), coloff(0) {  }

	int type;
	wxString name;
	int digits;
	float scale;
	size_t coloff;
};

struct AutoGraph {
	wxString title, xlabel, ylabel, yvals, legend_pos;
	int size, Type;
	bool show_legend, show_xvalues;
	double XMin, XMax;
};
	
class ResultsViewer : public wxMetroNotebook
{
public:
	ResultsViewer( wxWindow *parent, int id = wxID_ANY );
	virtual ~ResultsViewer();

	void Setup( Simulation *results );
	void Clear();

	void SavePerspective( StringHash &map );
	void LoadPerspective( StringHash &map );

	void SetGraphs(std::vector<Graph> &gl);
	void GetGraphs(std::vector<Graph> &gl);

	void SetUncertainties(std::vector<Uncertainties> &ul);
	void GetUncertainties(std::vector<Uncertainties> &ul);

	Simulation *GetSimulation() { return m_sim; }
	
	void AddMetric(MetricData& md) { m_metrics.push_back(md); }
	void AddMetricTable(MetricTable& mt) { m_metricTables.push_back(mt); }
	void AddMetricRow(MetricRow& mr) { m_metricRows.push_back(mr); }
	void AddCashFlowLine( CashFlowLine &cl ) { m_cashflow.push_back(cl); }
	void AddAutoGraph( AutoGraph &ag ) { m_autographs.push_back(ag); }

	wxDVPlotCtrlSettings GetDViewState();
	void SetDViewState( wxDVPlotCtrlSettings &settings );

	TabularBrowser *GetTabularBrowser() { return m_tables; }
	wxString GetCurrentContext() const;
	
private:	
	Simulation *m_sim = nullptr;

	std::vector<MetricData> m_metrics;
	std::vector<MetricTable> m_metricTables;
	std::vector<MetricRow> m_metricRows;
	std::vector<CashFlowLine> m_cashflow;
	std::vector<AutoGraph> m_autographs;

	wxSnapLayout *m_summaryLayout = nullptr;
	MetricsTable *m_metricsTable = nullptr;
	wxScrolledWindow *m_lossDiagramScroller = nullptr;
	LossDiagramCtrl *m_lossDiagram = nullptr;
	TabularBrowser *m_tables = nullptr;
	wxExtGridCtrl *m_cashFlowTable = nullptr;
	wxExtGridCtrl *m_depreciationTable = nullptr;
	wxSplitterWindow *m_cf_splitter = nullptr;
	wxPanel *m_cf_top_panel = nullptr;
	wxPanel *m_cf_bottom_panel = nullptr;

	GraphViewer *m_graphViewer = nullptr;
	UncertaintiesViewer *m_uncertaintiesViewer = nullptr;

	std::vector<wxDVTimeSeriesDataSet*> m_tsDataSets;
	wxDVTimeSeriesCtrl *m_timeSeries = nullptr;
	wxDVDMapCtrl *m_dMap = nullptr;
	wxDVProfileCtrl *m_profilePlots = nullptr;
	wxDVStatisticsTableCtrl *m_statTable = nullptr;
	wxDVPnCdfCtrl *m_pnCdf = nullptr;

	wxTextCtrl *m_messages = nullptr;

	void AddDataSet( wxDVTimeSeriesDataSet *ds, const wxString &group = wxEmptyString, bool update_ui = true );
	void RemoveAllDataSets();
	void Export(int data, int mechanism);
	void GetExportData(int data, matrix_t<wxString> &table);
	void ExportEqnExcel();

	void CreateAutoGraphs();
	

	void OnCommand( wxCommandEvent & );


	DECLARE_EVENT_TABLE();
};

class MetricsTable : public wxWindow
{
public:
	MetricsTable(wxWindow *parent);
	void SetData(const matrix_t<wxString> &data);
	void Clear();
	virtual wxSize DoGetBestSize() const;
private:	
	matrix_t<wxString> m_table;
	matrix_t<wxSize> m_cellsz;
	std::vector<int> m_colxsz;
	int m_rowHeight;

	void OnPaint(wxPaintEvent &evt);
	void OnResize(wxSizeEvent &evt);
	void OnRightClick(wxMouseEvent &evt);
	void OnContextMenu(wxCommandEvent &evt);
	DECLARE_EVENT_TABLE();
};
class TabularBrowser : public wxPanel
{
public:
	class ResultsTable;

	TabularBrowser( wxWindow *parent );

	void ProcessRemoved(wxString, bool update_grid=true);
	void ProcessRemovedAll(ArraySizeKey removed_size);
	void ProcessAdded(wxString);
	void SetLastSelection();
	void Setup( Simulation *sim );
	void UpdateAll();
	void UpdateDisplayed(wxString &srch);
	void UpdateSelectionList(int &vsx, int &vsy, bool select_in_list=false);
	void UpdateSelectionExpansion(int vsx, int vsy);
	void GetTextData(wxString &dat, char sep);
	
	wxArrayString GetSelectedVariables();
	void RemoveUnusedVariables();
	void SelectVariables(const wxArrayString &list);
	bool CheckSizeChanged(int index);
	ArraySizeKey GetVariableSize(int index);
	ArraySizeKey GetStoredVariableSize(int index, bool &contained);
	ArraySizeKey GetSimulationVariableSize(int index);
	ArraySizeKey GetVariableSizeByPage();
	wxExtGridCtrl* GetPage();

private:	
	Simulation *m_sim = nullptr;

	void OnCommand(wxCommandEvent &evt);
	void OnVarSel(wxCommandEvent &evt);
	void OnPageChanged(wxAuiNotebookEvent& event);
	void OnPageClosed(wxAuiNotebookEvent& event);
	void UpdateNotebook(ArraySizeKey grid_size, wxString name);
	void UpdateGridSpecific(wxExtGridCtrl*& grid, ResultsTable*& grid_table, wxArrayString selected_vars);
	void UpdateCase();
	
	typedef std::map<ArraySizeKey, wxArrayString, ArraySizeKeyCompare>::iterator ArrayIterator;
	typedef std::map<ArraySizeKey, wxExtGridCtrl*, ArraySizeKeyCompare>::iterator GridIterator;
	typedef std::map<ArraySizeKey, ResultsTable*, ArraySizeKeyCompare>::iterator ResultsIterator;

	typedef std::map<ArraySizeKey, wxArrayString, ArraySizeKeyCompare> VariableMap;

	wxAuiNotebook *m_notebook = nullptr;
	std::map<ArraySizeKey, wxExtGridCtrl*, ArraySizeKeyCompare> m_gridMap;
	std::map<ArraySizeKey, ResultsTable*, ArraySizeKeyCompare> m_gridTableMap;
	std::map<ArraySizeKey, wxString, ArraySizeKeyCompare> m_tabLabelsMap;
	std::map<wxString, ArraySizeKey> m_selectedVarsBySizeMap;
	std::map<ArraySizeKey, wxArrayString, ArraySizeKeyCompare> m_selectedVarsMap;

	size_t m_numberOfTabs;
	ArraySizeKey m_lastSize;

	wxExtGridCtrl *m_grid = nullptr;
	ResultsTable *m_gridTable = nullptr;
	wxDVSelectionListCtrl *m_varSel = nullptr;
	wxSearchCtrl *m_varSearch = nullptr;
	
	wxArrayString m_names;
	wxArrayString m_selectedVars;
	
	int m_key;

	DECLARE_EVENT_TABLE()
};


class TimeSeriesData : public wxDVTimeSeriesDataSet
{
	double *m_pdata = nullptr;
	size_t m_len;
	double m_tsHour;
	wxString m_label, m_units;
	double m_startOffsetHours;
public:
	TimeSeriesData( double *p, size_t len, double ts_hour, double ts_offset, const wxString &label, const wxString &units );
	virtual wxRealPoint At(size_t i) const;
	virtual size_t Length() const { return m_len; }
	virtual double GetTimeStep() const { return m_tsHour; }
	virtual double GetOffset() const { return 0.0; }
	virtual wxString GetSeriesTitle() const { return m_label; }
	virtual wxString GetUnits() const { return m_units; }
	virtual void SetDataValue(size_t , double ) { /* nothing to do */ }
};

#endif

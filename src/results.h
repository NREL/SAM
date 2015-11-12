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
	ResultsViewer *m_resview;
public:
	ResultsCallbackContext( ResultsViewer *rv, const wxString &desc = wxEmptyString );	
	ResultsViewer *GetResultsViewer();
	Simulation *GetSimulation();
protected:
	virtual void SetupLibraries( lk::env_t *env );
};
	
struct MetricData {
	MetricData() :
		scale( 1.0 ), mode( 'g' ), thousep( false ), deci( 2 )
	{}
	wxString var;
	wxString label;
	double scale;
	char mode;
	bool thousep;
	int deci;
	wxString pre, post;
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
	int size;
	bool show_legend, show_xvalues;
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

	void SetGraphs( std::vector<Graph> &gl );
	void GetGraphs( std::vector<Graph> &gl );

	Simulation *GetSimulation() { return m_sim; }
	
	void AddMetric( MetricData &md ) { m_metrics.push_back(md); }
	void AddCashFlowLine( CashFlowLine &cl ) { m_cashflow.push_back(cl); }
	void AddAutoGraph( AutoGraph &ag ) { m_autographs.push_back(ag); }

	wxDVPlotCtrlSettings GetDViewState();
	void SetDViewState( wxDVPlotCtrlSettings &settings );

	TabularBrowser *GetTabularBrowser() { return m_tables; }
	wxString GetCurrentContext() const;
	
private:	
	Simulation *m_sim;

	std::vector<MetricData> m_metrics;
	std::vector<CashFlowLine> m_cashflow;
	std::vector<AutoGraph> m_autographs;

	wxSnapLayout *m_summaryLayout;
	MetricsTable *m_metricsTable;	
	wxScrolledWindow *m_lossDiagramScroller;
	LossDiagramCtrl *m_lossDiagram;
	TabularBrowser *m_tables;
	wxExtGridCtrl *m_cashFlowTable;
	wxExtGridCtrl *m_depreciationTable;
	wxSplitterWindow *m_cf_splitter;
	wxPanel *m_cf_top_panel;
	wxPanel *m_cf_bottom_panel;

	GraphViewer *m_graphViewer;
		
	std::vector<wxDVTimeSeriesDataSet*> m_tsDataSets;
	wxDVTimeSeriesCtrl *m_timeSeries;
	//wxDVTimeSeriesCtrl *m_dailySeries;
	wxDVDMapCtrl *m_dMap;
	wxDVProfileCtrl *m_profilePlots;
	wxDVStatisticsTableCtrl *m_statTable;
	wxDVPnCdfCtrl *m_pnCdf;
	//wxDVDCCtrl *m_durationCurve;
	//wxDVScatterPlotCtrl *m_scatterPlot;

	wxTextCtrl *m_messages;

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

	void Setup( Simulation *sim );
	void UpdateAll();
	void UpdateDisplayed(wxString &srch);
	void GetTextData(wxString &dat, char sep);
	
	wxArrayString GetSelectedVariables();
	void SelectVariables(const wxArrayString &list);
	void UpdateVariableSize(int index, ArraySizeKey &var_size);

private:	
	Simulation *m_sim;

	void OnCommand(wxCommandEvent &evt);
	void OnVarSel(wxCommandEvent &evt);
	void OnPageChanged(wxBookCtrlEvent& event);
	void UpdateNotebook();
	void UpdateGridSpecific(wxExtGridCtrl*& grid, ResultsTable*& grid_table, wxArrayString selected_vars, bool show_grid);
	

	// static std::vector<wxString>  UI_HOUR_TIME_OF_DAY;

	wxNotebook *m_notebook;
	std::map<ArraySizeKey, wxExtGridCtrl*, ArraySizeKeyCompare> m_grid_map;
	std::map<ArraySizeKey, ResultsTable*, ArraySizeKeyCompare> m_gridTable_map;
	std::map<ArraySizeKey, wxArrayString, ArraySizeKeyCompare> m_selectedVars_map;
	std::map<wxString, ArraySizeKey> m_selectedVarsWithSize;
	std::map<ArraySizeKey, size_t, ArraySizeKeyCompare> m_pageBySize;

	size_t m_lastPageSelected;
	ArraySizeKey m_lastSize;

	wxExtGridCtrl *m_grid;
	ResultsTable *m_gridTable;
	wxDVSelectionListCtrl *m_varSel;
	wxSearchCtrl *m_varSearch;
	
	wxArrayString m_names;
	wxArrayString m_selectedVars;
	
	int m_key;

	DECLARE_EVENT_TABLE()
};


class TimeSeriesData : public wxDVTimeSeriesDataSet
{
	float *m_pdata;
	size_t m_len;
	double m_tsHour;
	wxString m_label, m_units;
public:
	TimeSeriesData( float *p, size_t len, double ts_hour, const wxString &label, const wxString &units );
	virtual wxRealPoint At(size_t i) const;
	virtual size_t Length() const { return m_len; }
	virtual double GetTimeStep() const { return m_tsHour; }
	virtual double GetOffset() const { return 0.0; }
	virtual wxString GetSeriesTitle() const { return m_label; }
	virtual wxString GetUnits() const { return m_units; }
	virtual void SetDataValue(size_t i, double newYValue) { /* nothing to do */ }
};

#endif

#ifndef __ResultsViewer_h
#define __ResultsViewer_h

#include <wex/metro.h>
#include <wex/exttree.h>

#include "object.h"

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
class wxPLPlotCtrl;
class wxTextCtrl;
class wxListBox;
class wxSimplebook;
class DataProvider;
class MetricsTable;
class TabularBrowser;
class wxMetroListBox;
class wxDVTimeSeriesDataSet;

class ConfigInfo;
class VarTable;

class ResultsViewer : public wxMetroNotebook
{
public:
	ResultsViewer( wxWindow *parent );
	virtual ~ResultsViewer();

	void Setup( ConfigInfo *cfg, DataProvider *results );
	void Clear();

	void SavePerspective( StringHash &map );
	void LoadPerspective( StringHash &map );

private:
	void OnCommand( wxCommandEvent & );

	ConfigInfo *m_cfg;
	DataProvider *m_results;

	MetricsTable *m_metrics;		
	TabularBrowser *m_tables;
	wxExtGridCtrl *m_cashFlow;
	
	std::vector<wxDVTimeSeriesDataSet*> m_tsDataSets;
	wxDVTimeSeriesCtrl *m_hourlySeries;
	wxDVTimeSeriesCtrl *m_dailySeries;
	wxDVTimeSeriesCtrl *m_monthlySeries;
	wxDVDMapCtrl *m_dMap;
	wxDVProfileCtrl *m_profilePlots;
	wxDVPnCdfCtrl *m_pnCdf;
	wxDVDCCtrl *m_durationCurve;
	wxDVScatterPlotCtrl *m_scatterPlot;

	void AddDataSet( wxDVTimeSeriesDataSet *ds, const wxString &group = wxEmptyString, bool update_ui = true );
	void RemoveAllDataSets();
	

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

	void Setup( ConfigInfo *cfg, DataProvider *results );
	void UpdateAll();
	void GetTextData(wxString &dat, char sep);
	

private:
	void OnCommand(wxCommandEvent &evt);
	void OnVarTree(wxTreeEvent &evt);

	void ListByCount( size_t n, wxArrayString &list );
	void SortByLabels(wxArrayString &names, wxArrayString &labels);
	void UpdateGrid();

	ConfigInfo *m_config;
	DataProvider *m_results;

	wxExtGridCtrl *m_grid;
	ResultsTable *m_gridTable;
	wxExtTreeCtrl *m_tree;
	
	wxTreeItemId m_root;
	std::vector<wxTreeItemId> m_items;
	wxArrayString m_names;
	wxArrayString m_selectedVars;
	
	DECLARE_EVENT_TABLE()
};

#endif

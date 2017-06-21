#ifndef __DVPlotCtrl_h
#define __DVPlotCtrl_h

/*
 * wxDVPlotCtrl.h
 *
 * This class is the control that contains all of the other DView-like controls in tabs.
 * This will have a tab for hourly, daily, monthly, profile, dmap, etc.
 */

#include "wex/metro.h"

#include "wex/dview/dvtimeseriesdataset.h"
#include "wex/dview/dvtimeseriesctrl.h"
#include "wex/dview/dvdmapctrl.h"
#include "wex/dview/dvprofilectrl.h"
#include "wex/dview/dvpncdfctrl.h"
#include "wex/dview/dvdcctrl.h"
#include "wex/dview/dvscatterplotctrl.h"
#include "wex/dview/dvplotctrlsettings.h"
#include "wex/dview/dvstatisticstablectrl.h"

class wxDVPlotCtrl : public wxMetroNotebook
{
public:
	wxDVPlotCtrl(wxWindow* parent, wxWindowID id = wxID_ANY, 
		const wxPoint& pos = wxDefaultPosition, 
		const wxSize& size = wxDefaultSize,
		long style = wxMT_LIGHTTHEME );
	virtual ~wxDVPlotCtrl();

	//When a data set is added, wxDVTimeSeriesCtrl takes ownership and will delete it upon destruction.
	void AddDataSet(wxDVTimeSeriesDataSet *d, bool update_ui = true);
	//RemoveDataSet releases ownership.
	void RemoveDataSet(wxDVTimeSeriesDataSet *d);
	//RemoveAll deletes data sets.
	void RemoveAllDataSets();

	wxDVStatisticsTableCtrl* GetStatisticsTable();

	//These methods get and set the view perspective to resume later with the same view.
	wxDVPlotCtrlSettings GetPerspective();
	void SetPerspective( wxDVPlotCtrlSettings& settings);

	enum { TAB_TS = 0, TAB_HTS, TAB_DTS, TAB_MTS, TAB_DMAP, TAB_PROFILE, TAB_PDF, TAB_DC, TAB_SCATTER };

	void SelectTabIndex( size_t index);
	void SelectDataIndex( size_t index, bool allTabs = false);
	void SelectDataIndexOnTab( size_t index, int tab);
	
	void SetTimeSeriesMode( int mode );
	void SetupTopYLeft( double min, double max );
	void SetupTopYRight( double min, double max );
	void SetTimeSeriesRange( double start, double end );
	void SetSelectedNames( const wxArrayString &names );

	void SelectDataOnBlankTabs();
	
	void DisplayTabs();
	double GetMinTimeStep();
	
private:
	std::vector<wxDVTimeSeriesDataSet*> m_dataSets;

	wxDVTimeSeriesCtrl *m_timeSeries;
	wxDVTimeSeriesCtrl *m_hourlyTimeSeries;
	wxDVTimeSeriesCtrl *m_dailyTimeSeries;
	wxDVTimeSeriesCtrl *m_monthlyTimeSeries;
	wxDVDMapCtrl *m_dMap;
	wxDVProfileCtrl *m_profilePlots;
	wxDVStatisticsTableCtrl *m_statisticsTable;
	wxDVPnCdfCtrl *m_pnCdf;
	wxDVDCCtrl *m_durationCurve;
	wxDVScatterPlotCtrl *m_scatterPlot;

DECLARE_EVENT_TABLE()
};

#endif


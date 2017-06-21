#ifndef __DVDCCtrl_h
#define __DVDCCtrl_h

#include <vector>
#include <wx/panel.h>
#include "wex/plot/plplotctrl.h"

class wxDVTimeSeriesDataSet;
class wxPLLinePlot;
class wxDVSelectionListCtrl;

class wxDVDCCtrl : public wxPanel
{
public:
	wxDVDCCtrl(wxWindow* parent, wxWindowID id = wxID_ANY, const wxPoint& pos = wxDefaultPosition, 
		const wxSize& size = wxDefaultSize, long style = wxTAB_TRAVERSAL, const wxString& name = "panel");
	virtual ~wxDVDCCtrl();

	//Data Set Functions - do not take ownership.
	void AddDataSet(wxDVTimeSeriesDataSet* d, bool update_ui);
	void RemoveDataSet(wxDVTimeSeriesDataSet* d);
	void RemoveAllDataSets();

	void ShowPlotAtIndex(int index);
	void HidePlotAtIndex(int index, bool update = true);
	void RefreshDisabledCheckBoxes();

	wxDVSelectionListCtrl* GetDataSelectionList();
	void SetSelectedNames(const wxString& names, bool restrictToSmallDataSets = false);
	void SelectDataSetAtIndex(int index);
	int GetNumberOfSelections();

	//Event Handlers
	void OnDataChannelSelection(wxCommandEvent& e);

private:
	wxPLPlotCtrl *m_plotSurface;
	wxDVSelectionListCtrl *m_dataSelector;

	struct PlotSet
	{
		PlotSet( wxDVTimeSeriesDataSet *ds );
		~PlotSet();

		wxDVTimeSeriesDataSet *dataset;
		wxPLLinePlot *plot;
		wxPLPlotCtrl::AxisPos axisPosition;
	};

	std::vector<PlotSet*> m_plots;
	std::vector<int> m_currentlyShownIndices;

	void CalculateDCPlotData( PlotSet *p );


	DECLARE_EVENT_TABLE()
};


#endif

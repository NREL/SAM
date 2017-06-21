#ifndef __DV_ProfileCtrl_h
#define __DV_ProfileCtrl_h

#include <vector>
#include <wx/panel.h>

#include "wex/plot/plplotctrl.h"

class wxDVTimeSeriesDataSet;
class wxPLLinePlot;
class wxDVSelectionListCtrl;
class wxGridSizer;
class wxCheckBox;

class wxDVProfileCtrl : public wxPanel
{
public:
	wxDVProfileCtrl(wxWindow* parent, wxWindowID id = wxID_ANY, const wxPoint& pos = wxDefaultPosition, 
		const wxSize& size = wxDefaultSize, long style = wxTAB_TRAVERSAL, const wxString& name = "panel");
	virtual ~wxDVProfileCtrl();

	//Does not take ownership.
	void AddDataSet(wxDVTimeSeriesDataSet *d, bool update_ui);
	bool RemoveDataSet(wxDVTimeSeriesDataSet *d); //true if found & removed.
	//RemoveAllDataSets does not delete original datasets since we never took ownership.
	void RemoveAllDataSets();


	bool IsMonthIndexSelected(int i);
	void SetMonthIndexSelected(int i, bool value = true);
	void ShowMonthPlotAtIndex(int index, bool show = true);
	void ShowPlotAtIndex(int i);
	void HidePlotAtIndex(int i, bool update = true);
	void HideAllPlots(bool update = true);
	void AutoScaleYAxes();
	void RefreshDisabledCheckBoxes();
	wxDVSelectionListCtrl* GetDataSelectionList();
	void SetSelectedNames(const wxString& names);
	void SelectDataSetAtIndex(int index);
	int GetNumberOfSelections();

	class VerticalLabelCtrl;
private:
	//Event Handlers
	void OnDataChannelSelection(wxCommandEvent& e);
	void OnMonthSelection(wxCommandEvent& e);
	void OnSelAllMonths(wxCommandEvent& e);
	
	struct PlotSet
	{
		PlotSet( wxDVTimeSeriesDataSet *ds );
		~PlotSet();

		void CalculateProfileData();

		wxDVTimeSeriesDataSet *dataset;
		wxPLLinePlot *plots[13];
		wxPLPlotCtrl::AxisPos axisPosition;
	};

	std::vector<PlotSet*> m_plots; //12 months of a day of data for each added data set.
	wxDVSelectionListCtrl *m_dataSelector;
	wxCheckBox *m_monthCheckBoxes[13];
	wxPLPlotCtrl *m_plotSurfaces[13];
	int m_numberOfPlotSurfacesShown;
	wxGridSizer *m_graphsSizer;

	VerticalLabelCtrl *m_leftAxisLabel;
	VerticalLabelCtrl *m_rightAxisLabel;
	
	void CalculateProfilePlotData( PlotSet *ps );

	DECLARE_EVENT_TABLE();
};

#endif

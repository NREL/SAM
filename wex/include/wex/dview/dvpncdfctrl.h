#ifndef __DVPnCdfCtrl_h 
#define __DVPnCdfCtrl_h

#include <wx/panel.h>
#include <vector>

#include "wex/plot/plhistplot.h"

#include "wex/dview/dvtimeseriesdataset.h"

class wxPlPlotCtrl;
class wxPLLinePlot;
class wxComboBox;
class wxChoice;
class wxTextCtrl;
class wxCheckBox;
class wxDVSelectionListCtrl;

class wxDVPnCdfCtrl : public wxPanel
{
public:
	wxDVPnCdfCtrl(wxWindow* parent, wxWindowID id = wxID_ANY, const wxPoint& pos = wxDefaultPosition, 
		const wxSize& size = wxDefaultSize, long style = wxTAB_TRAVERSAL, const wxString& name = "panel");
	virtual ~wxDVPnCdfCtrl();

	//Data Set Functions - do not take ownership.
	void AddDataSet(wxDVTimeSeriesDataSet* d, bool update_ui);
	void RemoveDataSet(wxDVTimeSeriesDataSet* d);
	void RemoveAllDataSets();

	wxString GetCurrentDataName();
	bool SetCurrentDataName(const wxString& name, bool restrictNoSmallDataSet = false);
	void SelectDataSetAtIndex(int index);
	int GetNumberOfSelections();
	void SetNumberOfBins(int n);
	int GetNumberOfBins();
	int GetBinSelectionIndex();
	void SetBinSelectionIndex(int i);
	wxPLHistogramPlot::NormalizeType GetNormalizeType();
	void SetNormalizeType(wxPLHistogramPlot::NormalizeType t);
	double GetYMax();
	void SetYMax(double max);

	void ReadCdfFrom( wxDVTimeSeriesDataSet& d, std::vector<wxRealPoint>* cdfArray );
	void ChangePlotDataTo( wxDVTimeSeriesDataSet* d, bool forceDataRefresh = false );
	void RebuildPlotSurface( double maxYPercent );

	// Event Handlers
	void OnDataChannelSelection(wxCommandEvent& e);

	void OnEnterYMax( wxCommandEvent & );
	void OnBinComboSelection( wxCommandEvent & );
	void OnBinTextEnter( wxCommandEvent & );
	void OnNormalizeChoice( wxCommandEvent & );
	void OnShowZerosClick( wxCommandEvent & );
	void OnPlotTypeSelection(wxCommandEvent &);

private:
	std::vector<wxDVTimeSeriesDataSet*> m_dataSets;
	int m_selectedDataSetIndex;
	std::vector< std::vector<wxRealPoint>* > m_cdfPlotData; //We track cdf plots since they take long to calculate.

	wxTextCtrl *m_minTextBox;
	wxTextCtrl *m_maxTextBox;
	
	wxDVSelectionListCtrl *m_selector;
	wxComboBox *m_binsCombo;
	wxChoice *m_normalizeChoice;
	wxCheckBox *m_hideZeros;
	wxChoice *m_PlotTypeDisplayed;

	wxPLPlotCtrl *m_plotSurface;
	wxPLHistogramPlot *m_pdfPlot;
	wxPLLinePlot *m_cdfPlot;

	void UpdateYAxisLabel();

	void InvalidatePlot();


DECLARE_EVENT_TABLE()
};


#endif

#ifndef __DVMapCtrl_h
#define __DVMapCtrl_h

/*
 * wxDVDmapCtrl.h
 *
 * This control contains a DMap plot surface and the controls
 * necessary to select a channel.  This control handles data sets and laying things out.
 * The plot surface takes care of the actual drawing.
 */

#include <vector>

#include <wx/panel.h>

class wxPLTimeAxis;
class wxPLLinearAxis;
class wxPLPlotCtrl;
class wxScrollBar;
class wxChoice;
class wxTextCtrl;
class wxCheckBox;
class wxDVTimeSeriesDataSet;
class wxPLColourMap;
class wxDVDMapPlot;
class wxDVSelectionListCtrl;

class wxDVDMapCtrl : public wxPanel
{
public:
	wxDVDMapCtrl(wxWindow* parent, wxWindowID id = wxID_ANY, 
		const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize);
	virtual ~wxDVDMapCtrl();

	//Does not take ownership.
	void AddDataSet(wxDVTimeSeriesDataSet* d, bool update_ui );
	void RemoveDataSet(wxDVTimeSeriesDataSet* d); //releases ownership, does not delete.
	void RemoveAllDataSets(); //clear all data sets from graphs and memory. (delete plottables.  Never took ownership.

	wxString GetCurrentDataName();
	bool SetCurrentDataName(const wxString& name);
	wxPLColourMap* GetCurrentColourMap();
	void SetColourMapName(const wxString& name);
	void SetReverseColours( bool b );
	bool IsReversedColours();
	void SelectDataSetAtIndex(int index);
	int GetNumberOfSelections();

	void ChangePlotDataTo(wxDVTimeSeriesDataSet* d);

	//These just set the value.
	//They do not check that it is within the limits or adjust it at all.
	double GetZMin();
	void SetZMin(double min);
	double GetZMax();
	void SetZMax(double max);
	double GetXMin();
	void SetXMin(double min);
	double GetXMax();
	void SetXMax(double max);
	double GetYMin();
	void SetYMin(double min);
	double GetYMax();
	void SetYMax(double max);
	void SetXViewRange(double min, double max);
	void SetYViewRange(double min, double max);
	void SetViewWindow(double xMin, double yMin, double xMax, double yMax);
	void PanXByPercent(double p);
	void PanYByPercent(double p);
	
	//These functions will move the bounds if they need to be moved.
	void KeepXBoundsWithinLimits(double* xMin, double* xMax);
	void KeepYBoundsWithinLimits(double* yMin, double* yMax);
	void KeepBoundsWithinLimits(double*xMin, double* yMin, double* xMax, double* yMax);
	void KeepNewBoundsWithinLimits(double* newMin, double* newMax);

	//Keep in limits and round to nice number.
	void MakeXBoundsNice(double* xMin, double* xMax);
	void MakeYBoundsNice(double* yMin, double* yMax);
	void MakeAllBoundsNice(double* xMin, double* yMin, double* xMax, double* yMax);

	void UpdateScrollbarPosition();
	void UpdateXScrollbarPosition();
	void UpdateYScrollbarPosition();

	void ZoomFactorAndUpdate(double factor, double shiftPercent = 0.0);


	/*Event Handlers*/
	void OnDataChannelSelection(wxCommandEvent& e);

	void OnColourMapSelection(wxCommandEvent& e);
	void OnColourMapMinChanged(wxCommandEvent& e);
	void OnColourMapMaxChanged(wxCommandEvent& e);
	void OnZoomIn(wxCommandEvent& e);
	void OnZoomOut(wxCommandEvent& e);
	void OnZoomFit(wxCommandEvent& e);
	void OnHighlight(wxCommandEvent& e);
	void OnMouseWheel(wxMouseEvent& e);
	void OnScroll(wxScrollEvent& e);
	void OnScrollLineUp(wxScrollEvent& e);
	void OnScrollLineDown(wxScrollEvent& e);
	void OnScrollPageUp(wxScrollEvent& e);
	void OnScrollPageDown(wxScrollEvent& e);
	void OnYScroll(wxScrollEvent& e);
	void OnYScrollLineUp(wxScrollEvent& e);
	void OnYScrollLineDown(wxScrollEvent& e);
	void OnYScrollPageUp(wxScrollEvent& e);
	void OnYScrollPageDown(wxScrollEvent& e);

	void OnResetColourMapMinMax(wxCommandEvent& e);
	void OnReverseColours( wxCommandEvent & );

	void Invalidate(); // recalculate and rerender plot
private:
	wxDVSelectionListCtrl *m_selector;
	wxChoice *m_colourMapSelector;
	wxCheckBox *m_reverseColours;

	wxTextCtrl *m_minTextBox;
	wxTextCtrl *m_maxTextBox;

	std::vector<wxDVTimeSeriesDataSet*> m_dataSets;
	wxDVTimeSeriesDataSet* m_currentlyShownDataSet;

	wxPLPlotCtrl *m_plotSurface;
	wxPLColourMap *m_colourMap;
	wxDVDMapPlot *m_dmap;
	wxPLTimeAxis *m_xAxis; // axes are owned by plot surface
	wxPLLinearAxis *m_yAxis;

	wxScrollBar *m_xGraphScroller;
	wxScrollBar *m_yGraphScroller;

	/*
	double mXWorldMin, mXWorldMax;
	double mYWorldMin, mYWorldMax;

	double mOrigXMin, mOrigXMax, mOrigYMin, mOrigYMax;
	*/

	DECLARE_EVENT_TABLE()
};

#endif

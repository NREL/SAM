
#include <wx/choice.h>
#include <wx/scrolbar.h>
#include <wx/textctrl.h>

#include "wex/plot/plplotctrl.h"
#include "wex/plot/plaxis.h"

#include "wex/dview/dvtimeseriesdataset.h"
#include "wex/dview/dvselectionlist.h"
#include "wex/plot/plcolourmap.h"
#include "wex/dview/dvplothelper.h"
#include "wex/dview/dvdmapctrl.h"

#include "wex/icons/zoom_in.cpng"
#include "wex/icons/zoom_out.cpng"
#include "wex/icons/zoom_fit.cpng"
#include <algorithm>  

class wxDVDMapPlot : public wxPLPlottable
{
private:
	wxDVTimeSeriesDataSet *m_data;
	wxPLColourMap *m_colourMap;
public:
	wxDVDMapPlot() : wxPLPlottable()
	{
		m_antiAliasing = false; // turn off AA for this plottable
		m_data = 0;
		m_colourMap = 0;
	}

	void SetData( wxDVTimeSeriesDataSet *d ) { m_data = d; }
	void SetColourMap( wxPLColourMap *c ) { m_colourMap = c; }
	
	virtual wxString GetXDataLabel( wxPLPlot * ) const 
	{
		return _("Hours since 00:00 Jan 1");
	}

	virtual wxString GetYDataLabel( wxPLPlot * ) const
	{
		wxString label = m_data->GetSeriesTitle();
		if ( !m_data->GetUnits().IsEmpty() )
			label += " (" + m_data->GetUnits() + ")";
		return label;
	}
	
	virtual wxRealPoint At( size_t i ) const
	{
		if ( !m_data ) return wxRealPoint(0,0);
		else return m_data->At(i);
	}

	virtual size_t Len() const
	{
		if ( !m_data ) return 0;
		else return m_data->Length();
	}

	virtual void Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map )
	{
		if ( !m_data || !m_colourMap ) return;
	
		wxRealPoint pos, size;
		map.GetDeviceExtents( &pos, &size );

		dc.Pen( *wxWHITE, 0, wxPLOutputDevice::NONE );			
		dc.Brush( *wxRED, wxPLOutputDevice::HATCH  );
		dc.Rect( pos.x, pos.y, size.x+1, size.y+1 );

		wxRealPoint wmin = map.GetWorldMinimum();
		wxRealPoint wmax = map.GetWorldMaximum();
		double xlen = wmax.x - wmin.x;
		double ylen = wmax.y - wmin.y;

		//Right now we iterate through points in data set.
		//Alternatively we could iterate through days and hours with nested loops.
		//That would give us the ability to average data points into boxes if we wanted.
		//But would lower the resolution (for high res data sets).

		double dRectWidth = size.x / (xlen / 24); //Rect width does not depend on data.
		double dRectHeight = size.y / ylen * m_data->GetTimeStep(); 

		for (size_t i=0; i<m_data->Length(); i++)
		{
			if (m_data->At(i).x < wmin.x)
				continue;
			if (m_data->At(i).x >= wmax.x)  //We include the = case because we draw the box to the right of the data point.
				break;

			int worldXDay = int(m_data->At(i).x) / 24; //x-res does not change with higher res data.

			double worldY = fmod(m_data->At(i).x, 24.0);
			worldY -= fmod(worldY, m_data->GetTimeStep()); // This makes sure the entire plot doesn't shift up for something like 1/2 hour data.
			if (worldY < wmin.y)
				continue;
			if (worldY >= wmax.y)
				continue;
			worldY -= wmin.y;

			double x = pos.x + (worldXDay - wmin.x/24) * dRectWidth;
			double y = pos.y + size.y - dRectHeight * (worldY/m_data->GetTimeStep() + 1); //+1 is because we have top corner, not bottom.
			
			dc.Brush( m_colourMap->ColourForValue(m_data->At(i).y) );
	
			// increase rect dimensions by about 0.5 point to
			// make sure they render overlapped without white space
			// showing in between
			dc.Rect(x, y, ceil(dRectWidth+0.5), ceil(dRectHeight+0.5)); //+1s cover empty spaces between rects.
		}
	}

	virtual void DrawInLegend( wxPLOutputDevice &dc, const wxPLRealRect &rct)
	{
		// nothing to do: won't be showing legends
	}
};

enum {
	ID_DATA_SELECTOR = wxID_HIGHEST + 1, 
	ID_COLOURMAP_SELECTOR_CHOICE, ID_GRAPH_SCROLLBAR, ID_GRAPH_Y_SCROLLBAR,
	ID_MIN_Z_INPUT, ID_MAX_Z_INPUT, ID_DMAP_SURFACE, ID_RESET_MIN_MAX, ID_REVERSE_COLOURS };

static const double MIN_ZOOM_LENGTH = 7 * 24;

BEGIN_EVENT_TABLE(wxDVDMapCtrl, wxPanel)
	
	EVT_DVSELECTIONLIST(ID_DATA_SELECTOR, wxDVDMapCtrl::OnDataChannelSelection)
	EVT_CHOICE(ID_COLOURMAP_SELECTOR_CHOICE, wxDVDMapCtrl::OnColourMapSelection)
	EVT_CHECKBOX( ID_REVERSE_COLOURS, wxDVDMapCtrl::OnReverseColours )

	EVT_TEXT_ENTER(ID_MIN_Z_INPUT, wxDVDMapCtrl::OnColourMapMinChanged)
	EVT_TEXT_ENTER(ID_MAX_Z_INPUT, wxDVDMapCtrl::OnColourMapMaxChanged)
	EVT_BUTTON(ID_RESET_MIN_MAX, wxDVDMapCtrl::OnResetColourMapMinMax)

	EVT_BUTTON( wxID_ZOOM_IN, wxDVDMapCtrl::OnZoomIn )
	EVT_BUTTON( wxID_ZOOM_OUT, wxDVDMapCtrl::OnZoomOut )
	EVT_BUTTON( wxID_ZOOM_FIT, wxDVDMapCtrl::OnZoomFit )

	EVT_PLOT_HIGHLIGHT(ID_DMAP_SURFACE, wxDVDMapCtrl::OnHighlight)

	EVT_MOUSEWHEEL( wxDVDMapCtrl::OnMouseWheel )

	EVT_COMMAND_SCROLL_THUMBTRACK(ID_GRAPH_SCROLLBAR, wxDVDMapCtrl::OnScroll)
	EVT_COMMAND_SCROLL_LINEUP(ID_GRAPH_SCROLLBAR, wxDVDMapCtrl::OnScrollLineUp)
	EVT_COMMAND_SCROLL_LINEDOWN(ID_GRAPH_SCROLLBAR, wxDVDMapCtrl::OnScrollLineDown)
	//EVT_COMMAND_SCROLL_CHANGED(ID_GRAPH_SCROLLBAR, wxDVDMapCtrl::OnScroll)
	EVT_COMMAND_SCROLL_PAGEUP(ID_GRAPH_SCROLLBAR, wxDVDMapCtrl::OnScrollPageUp)
	EVT_COMMAND_SCROLL_PAGEDOWN(ID_GRAPH_SCROLLBAR, wxDVDMapCtrl::OnScrollPageDown)

	EVT_COMMAND_SCROLL_THUMBTRACK(ID_GRAPH_Y_SCROLLBAR, wxDVDMapCtrl::OnYScroll)
	EVT_COMMAND_SCROLL_LINEUP(ID_GRAPH_Y_SCROLLBAR, wxDVDMapCtrl::OnYScrollLineUp)
	EVT_COMMAND_SCROLL_LINEDOWN(ID_GRAPH_Y_SCROLLBAR, wxDVDMapCtrl::OnYScrollLineDown)
	//EVT_COMMAND_SCROLL_CHANGED(ID_GRAPH_Y_SCROLLBAR, wxDVDMapCtrl::OnYScroll)
	EVT_COMMAND_SCROLL_PAGEUP(ID_GRAPH_Y_SCROLLBAR, wxDVDMapCtrl::OnYScrollPageUp)
	EVT_COMMAND_SCROLL_PAGEDOWN(ID_GRAPH_Y_SCROLLBAR, wxDVDMapCtrl::OnYScrollPageDown)

END_EVENT_TABLE()

wxDVDMapCtrl::wxDVDMapCtrl(wxWindow* parent, wxWindowID id, 
						   const wxPoint& pos, const wxSize& size)
	: wxPanel(parent, id, pos, size, wxTAB_TRAVERSAL)
{
	m_currentlyShownDataSet = 0;

	m_colourMap = new wxPLJetColourMap(0, 24);

	m_plotSurface = new wxPLPlotCtrl( this, ID_DMAP_SURFACE );
	m_plotSurface->SetBackgroundColour( *wxWHITE );
	m_plotSurface->SetHighlightMode( wxPLPlotCtrl::HIGHLIGHT_SPAN );
	m_plotSurface->ShowGrid( false, false );
	m_plotSurface->ShowTitle( false );
	m_plotSurface->ShowLegend( false );

	m_plotSurface->SetSideWidget( m_colourMap, wxPLPlotCtrl::Y_RIGHT );
	
	m_xAxis = new wxPLTimeAxis( 0, 8760 );
	m_yAxis = new wxPLLinearAxis( 0, 24, "Hour of day" );
	
	m_plotSurface->SetXAxis1( m_xAxis );
	m_plotSurface->SetYAxis1( m_yAxis );

	m_dmap = new wxDVDMapPlot;	
	m_dmap->SetColourMap( m_colourMap );
	m_plotSurface->AddPlot( m_dmap );

	m_minTextBox = new wxTextCtrl(this, ID_MIN_Z_INPUT, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER);
	m_maxTextBox = new wxTextCtrl(this, ID_MAX_Z_INPUT, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER);
	
	wxString choices[5] = { "Jet", "Parula", "Grayscale", "Coarse Rainbow", "Fine Rainbow" };
	m_colourMapSelector = new wxChoice(this, ID_COLOURMAP_SELECTOR_CHOICE, wxDefaultPosition, wxDefaultSize, 5, choices );
	m_colourMapSelector->SetSelection(0);

	m_reverseColours = new wxCheckBox( this, ID_REVERSE_COLOURS, "Reverse colors" );

	m_yGraphScroller = new wxScrollBar(this, ID_GRAPH_Y_SCROLLBAR, wxDefaultPosition, wxDefaultSize, wxSB_VERTICAL);
	m_xGraphScroller = new wxScrollBar(this, ID_GRAPH_SCROLLBAR, wxDefaultPosition, wxDefaultSize, wxSB_HORIZONTAL);
	
	wxBitmapButton *zoom_in =  new wxBitmapButton( this, wxID_ZOOM_IN, wxBITMAP_PNG_FROM_DATA( zoom_in ));
	zoom_in->SetToolTip("Zoom in");

	wxBitmapButton *zoom_out = new wxBitmapButton( this, wxID_ZOOM_OUT, wxBITMAP_PNG_FROM_DATA( zoom_out ));
	zoom_out->SetToolTip("Zoom out");

	wxBitmapButton *zoom_fit = new wxBitmapButton( this, wxID_ZOOM_FIT, wxBITMAP_PNG_FROM_DATA( zoom_fit ));
	zoom_fit->SetToolTip("Zoom fit");


	wxBoxSizer *scrollSizer = new wxBoxSizer(wxHORIZONTAL);
	scrollSizer->Add( m_xGraphScroller, 1, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	scrollSizer->Add( zoom_in, 0, wxALL|wxEXPAND, 1);
	scrollSizer->Add( zoom_out, 0, wxALL|wxEXPAND, 1);
	scrollSizer->Add( zoom_fit , 0, wxALL|wxEXPAND, 1);

	m_selector = new wxDVSelectionListCtrl(this, ID_DATA_SELECTOR, 1, wxDefaultPosition, wxDefaultSize, wxDVSEL_RADIO_FIRST_COL|wxDVSEL_NO_COLOURS); 
	
	wxBoxSizer *horizPlotSizer = new wxBoxSizer(wxHORIZONTAL);
	horizPlotSizer->Add( m_plotSurface, 1, wxEXPAND|wxALL, 0);
//	horizPlotSizer->Add(m_yGraphScroller, 0, wxALL | wxEXPAND | wxALIGN_CENTER_VERTICAL, 0);
	horizPlotSizer->Add(m_yGraphScroller, 0, wxALL | wxEXPAND , 0);

	wxBoxSizer *optionsSizer = new wxBoxSizer(wxHORIZONTAL);	
//	optionsSizer->Add(m_colourMapSelector, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALIGN_RIGHT, 3);
	optionsSizer->Add(m_colourMapSelector, 0, wxALL | wxEXPAND , 3);
	optionsSizer->Add( m_reverseColours, 0, wxLEFT|wxRIGHT|wxALIGN_CENTER_VERTICAL, 5 );
	optionsSizer->AddStretchSpacer();
	optionsSizer->Add(new wxStaticText(this, wxID_ANY, "Min:"), 0, wxALIGN_CENTER|wxALIGN_CENTER_VERTICAL, 4);
	optionsSizer->Add(m_minTextBox, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
	optionsSizer->AddSpacer( 6 );
	optionsSizer->Add(new wxStaticText(this, wxID_ANY, "Max:"), 0, wxALIGN_CENTER|wxALIGN_CENTER_VERTICAL, 4);
	optionsSizer->Add(m_maxTextBox, 0,  wxALL|wxALIGN_CENTER_VERTICAL, 3);
	optionsSizer->Add(new wxButton(this, ID_RESET_MIN_MAX, "Reset", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALIGN_CENTER | wxRIGHT, 5);
	
	wxBoxSizer *leftSizer = new wxBoxSizer( wxVERTICAL );
	leftSizer->Add( optionsSizer, 0, wxEXPAND, 2 );
	leftSizer->Add( horizPlotSizer, 1, wxEXPAND, 0 );
	leftSizer->Add( scrollSizer, 0, wxEXPAND, 0 );

	wxBoxSizer *mainSizer = new wxBoxSizer( wxHORIZONTAL );
	mainSizer->Add( leftSizer, 1, wxALL|wxEXPAND, 0 );
	mainSizer->Add( m_selector, 0, wxALL|wxEXPAND, 0 );
	SetSizer( mainSizer );
}

wxDVDMapCtrl::~wxDVDMapCtrl()
{
	m_plotSurface->ReleaseSideWidget( wxPLPlotCtrl::Y_RIGHT );
	delete m_colourMap;
}

/*Member Functions*/
void wxDVDMapCtrl::AddDataSet(wxDVTimeSeriesDataSet* d, bool update_ui )
{
	m_dataSets.push_back(d);
	m_selector->Append( d->GetTitleWithUnits(), d->GetGroupName() );

	if (update_ui)
		Layout(); //Resize dataSelector.
}

void wxDVDMapCtrl::RemoveDataSet(wxDVTimeSeriesDataSet* d)
{
	std::vector<wxDVTimeSeriesDataSet*>::iterator it = std::find(  m_dataSets.begin(), m_dataSets.end(), d );
	int removedIndex = it - m_dataSets.begin();
	if ( removedIndex < 0 ) return;

	m_dataSets.erase( it );
	m_selector->RemoveAt( removedIndex );

	if (m_currentlyShownDataSet == d)
		ChangePlotDataTo( NULL );
}

void wxDVDMapCtrl::RemoveAllDataSets()
{
	ChangePlotDataTo(NULL);

	m_dataSets.clear();
	m_selector->RemoveAll();
}

void wxDVDMapCtrl::SetXViewRange(double min, double max)
{
	SetXMin(min);
	SetXMax(max);
}

void wxDVDMapCtrl::SetYViewRange(double min, double max)
{
	SetYMin(min);
	SetYMax(max);
}

void wxDVDMapCtrl::SetViewWindow(double xMin, double yMin, double xMax, double yMax)
{
	SetXMin(xMin);
	SetYMin(yMin);
	SetXMax(xMax);
	SetYMax(yMax);
}

void wxDVDMapCtrl::PanXByPercent(double p)
{
	double wmin = m_xAxis->GetWorldMin() + m_xAxis->GetWorldLength() * p;
	double wmax = m_xAxis->GetWorldMax() + m_xAxis->GetWorldLength() * p;

	MakeXBoundsNice(&wmin, &wmax);
	m_xAxis->SetWorld(wmin, wmax);
}

void wxDVDMapCtrl::PanYByPercent(double p)
{
	double wmin = m_yAxis->GetWorldMin() - m_yAxis->GetWorldLength() * p;
	double wmax = m_yAxis->GetWorldMax() - m_yAxis->GetWorldLength() * p;

	MakeYBoundsNice(&wmin, &wmax);
	m_yAxis->SetWorld(wmin, wmax);
}

void wxDVDMapCtrl::MakeXBoundsNice(double* xMin, double* xMax)
{
	KeepXBoundsWithinLimits(xMin, xMax);
	wxDVPlotHelper::SetRangeEndpointsToDays(xMin, xMax);
}

void wxDVDMapCtrl::MakeYBoundsNice(double* yMin, double* yMax)
{
	wxPLAxis::ExtendBoundsToNiceNumber(yMax, yMin);
	KeepYBoundsWithinLimits(yMin, yMax);
}

void wxDVDMapCtrl::MakeAllBoundsNice(double* xMin, double* yMin, double* xMax, double* yMax)
{
	MakeXBoundsNice(xMin, xMax);
	MakeYBoundsNice(yMin, yMax);
}

void wxDVDMapCtrl::KeepNewBoundsWithinLimits(double* newMin, double* newMax)
{
	KeepXBoundsWithinLimits(newMin, newMax);
}

void wxDVDMapCtrl::KeepXBoundsWithinLimits(double* xMin, double* xMax)
{
	//X bounds should be an integer number of days.
	*xMin -= fmod(*xMin, double(24));
	*xMax -= fmod(*xMax, double(24));

	//Don't zoom in too far.
	if (*xMax - *xMin < MIN_ZOOM_LENGTH)
	{
		*xMax = *xMin + MIN_ZOOM_LENGTH;
	}

	//Don't zoom out too far.
	double timeMin = 0; 
	double timeMax = 1; 
	if ( m_currentlyShownDataSet != 0 )
	{
		timeMin = m_currentlyShownDataSet->GetMinHours();
		timeMax = m_currentlyShownDataSet->GetMaxHours();
	}

	if (*xMin < timeMin)
	{
		*xMax += timeMin - *xMin;
		*xMin = timeMin;

		if (*xMax > timeMax)
			*xMax = timeMax;
	}
	if (*xMax > timeMax)
	{
		*xMin -= *xMax - timeMax;
		*xMax = timeMax;

		if (*xMin < timeMin)
			*xMin = timeMin;
	}
}

void wxDVDMapCtrl::KeepYBoundsWithinLimits(double* yMin, double* yMax)
{
	if (*yMax == *yMin)
	{
		if (*yMax < 24)
			yMax += 1;
		else
			yMin -= 1;
	}

	if (*yMin < 0)
		*yMin = 0;
	if (*yMax > 24)
		*yMax = 24;
}

void wxDVDMapCtrl::KeepBoundsWithinLimits(double* xMin, double* yMin, double* xMax, double* yMax)
{
	KeepXBoundsWithinLimits(xMin, xMax);
	KeepYBoundsWithinLimits(yMin, yMax);
}

void wxDVDMapCtrl::UpdateScrollbarPosition()
{
	UpdateXScrollbarPosition();
	UpdateYScrollbarPosition();
}

void wxDVDMapCtrl::UpdateXScrollbarPosition()
{
	int thumbSize = m_xAxis->GetWorldLength();
	int pageSize = thumbSize;
	
	double tmin = 0, tmax = 1;
	if ( m_currentlyShownDataSet != 0 )
	{
		tmin = m_currentlyShownDataSet->GetMinHours();
		tmax = m_currentlyShownDataSet->GetMaxHours();
	}

	int range = tmax-tmin;
	int position = m_xAxis->GetWorldMin() - tmin;

	m_xGraphScroller->SetScrollbar(position, thumbSize, range, pageSize);
}

void wxDVDMapCtrl::UpdateYScrollbarPosition()
{
	int thumbSize = m_yAxis->GetWorldLength();
	int pageSize = thumbSize;
	int range = 24;

	int position = 24 - m_yAxis->GetWorldMax();

	m_yGraphScroller->SetScrollbar(position, thumbSize, range, pageSize);
}

bool wxDVDMapCtrl::SetCurrentDataName(const wxString& name)
{
	for (int i=0; i < m_dataSets.size(); i++)
	{
		if (m_selector->GetRowLabelWithGroup(i) == name)
		{
			ChangePlotDataTo(m_dataSets[i]);
			m_selector->SelectRowInCol( i );
			return true;
		}
	}

	return false;
}

wxString wxDVDMapCtrl::GetCurrentDataName()
{
	int index = -1;
	for( size_t i=0;i<m_dataSets.size();i++)
		if ( m_currentlyShownDataSet == m_dataSets[i] )
			index = i;

	return m_selector->GetRowLabelWithGroup( index ); // if index < 0, returns ""
}

void wxDVDMapCtrl::SelectDataSetAtIndex(int index)
{
	if (index < 0 || index >= m_dataSets.size()) return;

	ChangePlotDataTo(m_dataSets[index]);
	m_selector->SelectRowInCol( index );
}

int wxDVDMapCtrl::GetNumberOfSelections()
{
	return m_selector->GetNumberOfSelections();
}

void wxDVDMapCtrl::SetZMin(double min)
{
	m_colourMap->SetScaleMin( min );
	m_minTextBox->SetValue(wxString::Format("%g", min));
}

double wxDVDMapCtrl::GetZMin()
{
	return m_colourMap->GetScaleMin();
}

void wxDVDMapCtrl::SetZMax(double max)
{
	m_colourMap->SetScaleMax( max );
	m_maxTextBox->SetValue(wxString::Format("%g", max));
}

double wxDVDMapCtrl::GetZMax()
{
	return m_colourMap->GetScaleMax();
}

void wxDVDMapCtrl::SetXMin(double min)
{
	m_xAxis->SetWorldMin( min );
	//m_xAxis->ReLabelThisAxis(); //Have to call this because we set an exposed property directly.
	UpdateXScrollbarPosition();
}

double wxDVDMapCtrl::GetXMin()
{
	return m_xAxis->GetWorldMin();
}

void wxDVDMapCtrl::SetXMax(double max)
{
	m_xAxis->SetWorldMax( max );
	//m_xAxis->ReLabelThisAxis(); //Have to call this because we set an exposed property directly.
	UpdateXScrollbarPosition();
}

double wxDVDMapCtrl::GetXMax()
{
	return m_xAxis->GetWorldMax();
}

void wxDVDMapCtrl::SetYMin(double min)
{
	m_yAxis->SetWorldMin( min );
}

double wxDVDMapCtrl::GetYMin()
{
	return m_yAxis->GetWorldMin();
}

void wxDVDMapCtrl::SetYMax(double max)
{
	m_yAxis->SetWorldMax( max );
}

double wxDVDMapCtrl::GetYMax()
{
	return m_yAxis->GetWorldMax();
}

wxPLColourMap* wxDVDMapCtrl::GetCurrentColourMap()
{
	return m_colourMap;
}

void wxDVDMapCtrl::SetColourMapName(const wxString& name)
{
	double scaleMin = m_colourMap->GetScaleMin();
	double scaleMax = m_colourMap->GetScaleMax();

	// make sure the plot no longer sees this colourmap widget
	m_plotSurface->ReleaseSideWidget( wxPLPlotCtrl::Y_RIGHT );
	delete m_colourMap;
	m_colourMap = 0;

	int position = m_colourMapSelector->FindString(name);
	switch(position)
	{
	case 0: m_colourMap = new wxPLJetColourMap; break;
	case 1: m_colourMap = new wxPLParulaColourMap; break;
	case 2: m_colourMap = new wxPLGrayscaleColourMap; break;
	case 3: m_colourMap = new wxPLCoarseRainbowColourMap; break;
	default:
		m_colourMap = new wxPLFineRainbowColourMap; break;
	}

	m_colourMap->SetReversed( m_reverseColours->GetValue() );

	if (position != wxNOT_FOUND)
	{
		m_plotSurface->SetSideWidget( m_colourMap, wxPLPlotCtrl::Y_RIGHT );
		m_colourMap->SetScaleMinMax( scaleMin, scaleMax );
		m_colourMapSelector->SetSelection(position);
		m_dmap->SetColourMap( m_colourMap );
		Invalidate();
	}
}

void wxDVDMapCtrl::SetReverseColours( bool b )
{
	m_reverseColours->SetValue( b );
	m_colourMap->SetReversed( b );
	Invalidate();
}

bool wxDVDMapCtrl::IsReversedColours()
{
	return m_reverseColours->GetValue();
}

void wxDVDMapCtrl::Invalidate()
{
	m_plotSurface->Invalidate();
	m_plotSurface->Refresh();
}

void wxDVDMapCtrl::ChangePlotDataTo(wxDVTimeSeriesDataSet* d)
{
	m_currentlyShownDataSet = d;
	
	m_dmap->SetData(d);
	
	double min, max;
	if ( d )
	{
		d->GetDataMinAndMax( &min, &max );
		m_colourMap->SetScaleMinMax( min, max );
		m_colourMap->ExtendScaleToNiceNumbers();		
		m_dmap->SetColourMap( m_colourMap );
	}

	min = m_colourMap->GetScaleMin();
	max = m_colourMap->GetScaleMax();

	//Update our text boxes to reflect the auto-scaled min/max.
	m_minTextBox->ChangeValue( wxString::Format("%lg", min) );
	m_maxTextBox->ChangeValue( wxString::Format("%lg", max) );

	UpdateScrollbarPosition();	
	Invalidate();
}

/*Event Handlers*/
void wxDVDMapCtrl::OnDataChannelSelection(wxCommandEvent &)
{
	int row;
	bool isChecked;
	m_selector->GetLastEventInfo( &row, 0, &isChecked );
	if ( row >= 0 && row < (int)m_dataSets.size() )
		ChangePlotDataTo( m_dataSets[row] );
}

void wxDVDMapCtrl::OnColourMapSelection(wxCommandEvent &)
{
	SetColourMapName( m_colourMapSelector->GetStringSelection() );
}

void wxDVDMapCtrl::OnReverseColours( wxCommandEvent & )
{
	m_colourMap->SetReversed( m_reverseColours->GetValue() );
	m_plotSurface->Refresh();
}

void wxDVDMapCtrl::OnColourMapMinChanged(wxCommandEvent &)
{
	double val;
	if (m_minTextBox->GetValue().ToDouble(&val))
	{
		m_colourMap->SetScaleMin(val);
		Invalidate();
	}
}

void wxDVDMapCtrl::OnColourMapMaxChanged(wxCommandEvent &)
{
	double val;
	if (m_maxTextBox->GetValue().ToDouble(&val))
	{
		m_colourMap->SetScaleMax(val);
		Invalidate();
	}
}

void wxDVDMapCtrl::OnZoomIn(wxCommandEvent &)
{
	if (m_xAxis->GetWorldLength() <= 7*24) return;
	ZoomFactorAndUpdate(2.0);
}

void wxDVDMapCtrl::OnZoomOut(wxCommandEvent &)
{
	ZoomFactorAndUpdate(0.5);
}

void wxDVDMapCtrl::OnZoomFit(wxCommandEvent &)
{
	double xmin = 0;
	double xmax = 8760;
	if ( m_currentlyShownDataSet )
	{
		xmin = m_currentlyShownDataSet->GetMinHours();
		xmax = m_currentlyShownDataSet->GetMaxHours();
	}
	

	MakeXBoundsNice(&xmin, &xmax);
	m_xAxis->SetWorld(xmin, xmax);
	m_yAxis->SetWorld(0, 24);

	UpdateScrollbarPosition();
	Invalidate();
}

void wxDVDMapCtrl::OnHighlight(wxCommandEvent &)
{
	double left, right;
	m_plotSurface->GetHighlightBounds( &left, &right ); // x bounds in percentages

	double min = m_xAxis->GetWorldMin() + left/100.0*m_xAxis->GetWorldLength();
	double max = m_xAxis->GetWorldMin() + right/100.0*m_xAxis->GetWorldLength();

	
	MakeXBoundsNice(&min, &max);
	SetXViewRange(min, max);

	Invalidate();
}

void wxDVDMapCtrl::OnMouseWheel(wxMouseEvent& e)
{
	
	if (e.CmdDown()) //cmd + wheel : scroll vertical
	{
		PanYByPercent(-0.25 * e.GetWheelRotation() / e.GetWheelDelta());
	}
	else if (e.ShiftDown()) //shift + wheel : scroll horizontal
	{
		PanXByPercent(-0.25 * e.GetWheelRotation() / e.GetWheelDelta());
	}
	else //wheel: zoom
	{
		if (m_xAxis->GetWorldLength() <= 7*24 && e.GetWheelRotation() > 0) return;

		//There was code here to zoom in both directions.  I commented out the y direction.
		//Center zooming on the location of the mouse.
		wxCoord xPos, yPos;
		e.GetPosition(&xPos, &yPos);
		wxSize client = m_plotSurface->GetClientSize();
		
		double xmin = m_xAxis->GetWorldMin();
		double xmax = m_xAxis->GetWorldMax();
		wxDVPlotHelper::MouseWheelZoom(&xmin, &xmax, xPos, 0, client.x, e.GetWheelRotation() / e.GetWheelDelta());

		//m_plotSurface->GetYAxis()->GetPhysical(&min, &max);
		//wxDVPlotHelper::MouseWheelZoom(&mYWorldMin, &mYWorldMax, yPos, min.y, max.y, e.GetWheelRotation() / e.GetWheelDelta());

		//MakeAllBoundsNice(&mXWorldMin, &mYWorldMin, &mXWorldMax, &mYWorldMax);
		MakeXBoundsNice(&xmin, &xmax);
		m_xAxis->SetWorld(xmin, xmax);
		//m_plotSurface->GetYAxis()->SetWorld(mYWorldMin, mYWorldMax);
	}

	UpdateScrollbarPosition();
	Invalidate();
}

void wxDVDMapCtrl::OnScroll(wxScrollEvent& e)
{
	if ( !m_currentlyShownDataSet ) return;

	double xmin = e.GetPosition() + m_currentlyShownDataSet->GetMinHours();
	double xmax = xmin + m_xAxis->GetWorldLength();

	MakeXBoundsNice(&xmin, &xmax);
	m_xAxis->SetWorld(xmin, xmax);

	Invalidate();
}

void wxDVDMapCtrl::OnScrollLineUp(wxScrollEvent &)
{
	PanXByPercent(-0.25);

	UpdateXScrollbarPosition();
	Invalidate();
}

void wxDVDMapCtrl::OnScrollLineDown(wxScrollEvent&)
{
	PanXByPercent(0.25);

	UpdateXScrollbarPosition();
	Invalidate();
}

void wxDVDMapCtrl::OnScrollPageUp(wxScrollEvent& e)
{
	PanXByPercent(-1.0);

	UpdateXScrollbarPosition();
	Invalidate();
}

void wxDVDMapCtrl::OnScrollPageDown(wxScrollEvent& e)
{
	PanXByPercent(1.0);

	UpdateXScrollbarPosition();
	Invalidate();
}

void wxDVDMapCtrl::OnYScroll(wxScrollEvent& e)
{
	double ymax = 24 - e.GetPosition();
	double ymin = ymax - m_yAxis->GetWorldLength();

	MakeYBoundsNice(&ymin, &ymax);
	m_yAxis->SetWorld(ymin, ymax);

	Invalidate();
}

void wxDVDMapCtrl::OnYScrollLineUp(wxScrollEvent& e)
{
	PanYByPercent(-0.25);

	UpdateYScrollbarPosition();
	Invalidate();
}

void wxDVDMapCtrl::OnYScrollLineDown(wxScrollEvent& e)
{
	PanYByPercent(0.25);

	UpdateYScrollbarPosition();
	Invalidate();
}

void wxDVDMapCtrl::OnYScrollPageUp(wxScrollEvent& e)
{
	PanYByPercent(-1.0);

	UpdateYScrollbarPosition();
	Invalidate();
}

void wxDVDMapCtrl::OnYScrollPageDown(wxScrollEvent& e)
{
	PanYByPercent(1.0);

	UpdateYScrollbarPosition();
	Invalidate();
}

void wxDVDMapCtrl::OnResetColourMapMinMax(wxCommandEvent &)
{
	if ( !m_currentlyShownDataSet ) return;

	double dataMin, dataMax;
	m_currentlyShownDataSet->GetDataMinAndMax(&dataMin, &dataMax);

	m_colourMap->SetScaleMinMax(dataMin, dataMax);
	m_colourMap->ExtendScaleToNiceNumbers();

	//Update our text boxes to reflect the auto-scaled min/max.
	m_minTextBox->ChangeValue(wxString::Format("%lg", m_colourMap->GetScaleMin()));
	m_maxTextBox->ChangeValue(wxString::Format("%lg", m_colourMap->GetScaleMax()));

	Invalidate();
}

void wxDVDMapCtrl::ZoomFactorAndUpdate(double factor, double shiftPercent)
{
	double xmin = m_xAxis->GetWorldMin();
	double xmax = m_xAxis->GetWorldMax();
	wxDVPlotHelper::ZoomFactor(&xmin, &xmax, factor, shiftPercent);

	MakeXBoundsNice(&xmin, &xmax);
	m_xAxis->SetWorld(xmin, xmax);

	UpdateXScrollbarPosition();
	Invalidate();
}

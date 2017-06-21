#include <wx/checkbox.h>
#include <wx/sizer.h>
#include <wx/dcbuffer.h>
#include <wx/tokenzr.h>

#include "wex/plot/plplotctrl.h"
#include "wex/plot/pllineplot.h"

#include "wex/dview/dvtimeseriesdataset.h"
#include "wex/dview/dvselectionlist.h"
#include "wex/dview/dvprofilectrl.h"
#include "wex/dview/dvplothelper.h"
#include <algorithm>

static const wxString NO_UNITS("ThereAreNoUnitsForThisAxis.");

#if defined(_DEBUG) && defined(WIN32)
#define VEC_ASSERT(x) {if(!(x)) __debugbreak(); }
#else
#define VEC_ASSERT(X) assert(X)
#endif

template< typename T >
class dvMatrix
{
private:
	T *t_array;
	size_t n_rows, n_cols;
public:

	dvMatrix()
	{
		t_array = NULL;
		n_rows = n_cols = 0;
	}
		
	dvMatrix(size_t nr, size_t nc, const T &val)
	{
		t_array = NULL;
		resize_fill(nr,nc,val);
	}

	virtual ~dvMatrix()
	{
		if (t_array) delete [] t_array;
	}
	
	void resize_fill(size_t nr, size_t nc, const T &val)
	{
		if (t_array == 0)
		{
			delete [] t_array;
			t_array = NULL;
			n_rows = n_cols = 0;
		}

		size_t len = nr*nc;
		if ( len > 0 )
		{
			t_array = new T[nr*nc];
			for ( size_t i=0;i<len;i++ )
				t_array[i] = val;
			n_rows = nr;
			n_cols = nc;
		}
	}
		
	inline T &at(size_t r, size_t c)
	{
#ifdef _DEBUG
		VEC_ASSERT( r >= 0 && r < n_rows && c >= 0 && c < n_cols );
#endif
		return t_array[n_cols*r+c];
	}

	inline const T &at(size_t r, size_t c) const
	{
#ifdef _DEBUG
		VEC_ASSERT( r >= 0 && r < n_rows && c >= 0 && c < n_cols );
#endif
		return t_array[n_cols*r+c];
	}
						
	inline size_t nrows() const
	{
		return n_rows;
	}
		
	inline size_t ncols() const
	{
		return n_cols;
	}
};


class wxDVProfileCtrl::VerticalLabelCtrl: public wxWindow
{
private:
	wxString m_label;
	static const int border_space = 2;
	double TextRotationAngle;
public:
	VerticalLabelCtrl(wxWindow* parent, wxWindowID id)
		: wxWindow(parent, id)
	{
		SetBackgroundStyle(wxBG_STYLE_CUSTOM);
		SetBackgroundColour( *wxWHITE );
	}

	virtual wxSize DoGetBestSize() const
	{
		wxCoord width, height;
		GetTextExtent(m_label, &width, &height);
		return wxSize(height+2*border_space, width+2*border_space);
	}

	void SetLabelText(const wxString& text, double textRotationAngle)
	{
		m_label = text;
		TextRotationAngle = textRotationAngle;
		InvalidateBestSize();
	}

	void OnErase( wxEraseEvent & )
	{
		// nothing to do 
	}

	void OnPaint(wxPaintEvent& e)
	{
		wxAutoBufferedPaintDC pdc(this);	
		pdc.SetBackground( wxBrush(GetBackgroundColour()) );
		pdc.Clear();

		wxString label = m_label;
		wxSize client = GetClientSize();
		pdc.SetFont( GetFont() );
		wxCoord width, height;
		pdc.GetTextExtent(label, &width, &height);	
		pdc.SetTextForeground( GetForegroundColour() );

		int padding = border_space + (TextRotationAngle == 270 ? 12 : 0);
		pdc.DrawRotatedText(label, padding, client.GetHeight() / 2 + width / 2, TextRotationAngle);
	}
	
	void OnSize( wxSizeEvent & )
	{
		Refresh();
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(wxDVProfileCtrl::VerticalLabelCtrl, wxWindow)
	EVT_SIZE( wxDVProfileCtrl::VerticalLabelCtrl::OnSize )
	EVT_PAINT( wxDVProfileCtrl::VerticalLabelCtrl::OnPaint )
	EVT_ERASE_BACKGROUND( wxDVProfileCtrl::VerticalLabelCtrl::OnErase )
END_EVENT_TABLE()


enum { 
	ID_DATA_COMBOBOX = wxID_HIGHEST+1,
	ID_DATA_SELECTOR,
	ID_JAN_CHECK, ID_FEB_CHECK, ID_MAR_CHECK, ID_APR_CHECK, ID_MAY_CHECK, ID_JUN_CHECK,
	ID_JUL_CHECK, ID_AUG_CHECK, ID_SEP_CHECK, ID_OCT_CHECK, ID_NOV_CHECK, ID_DEC_CHECK, 
	ID_ANNUAL_CHECK, ID_SEL_ALL_CHECK
};

BEGIN_EVENT_TABLE(wxDVProfileCtrl, wxPanel)
	EVT_DVSELECTIONLIST( ID_DATA_SELECTOR, wxDVProfileCtrl::OnDataChannelSelection )
	EVT_COMMAND_RANGE( ID_JAN_CHECK, ID_ANNUAL_CHECK, wxEVT_COMMAND_CHECKBOX_CLICKED, wxDVProfileCtrl::OnMonthSelection )
	EVT_CHECKBOX( ID_SEL_ALL_CHECK, wxDVProfileCtrl::OnSelAllMonths )
END_EVENT_TABLE()

wxDVProfileCtrl::wxDVProfileCtrl(wxWindow* parent, wxWindowID id, const wxPoint& pos, 
		const wxSize& size, long style, const wxString& name)
		: wxPanel(parent, id, pos, size, style, name)
{

	wxScrolledWindow *monthSelector = new wxScrolledWindow (this, wxID_ANY, 
		wxDefaultPosition, wxDefaultSize, wxHSCROLL);
	wxBoxSizer *monthSizer = new wxBoxSizer( wxHORIZONTAL );
	monthSelector->SetSizer(monthSizer);
	monthSelector->SetScrollRate(10, 0);

	for (int i=0; i<12; i++) 
	{
		m_monthCheckBoxes[i] = new wxCheckBox(monthSelector, ID_JAN_CHECK + i, 
			wxDateTime::GetMonthName(wxDateTime::Month(i), wxDateTime::Name_Abbr));
		m_monthCheckBoxes[i]->SetValue(true);
		monthSizer->Add( m_monthCheckBoxes[i], 0, wxALL, 5 );
	}

	m_numberOfPlotSurfacesShown = 12;

	//Add an extra at the end for the annual profile.
	m_monthCheckBoxes[12] = new wxCheckBox( monthSelector, ID_ANNUAL_CHECK, wxT("Annual") );
	monthSizer->Add( m_monthCheckBoxes[12], 0, wxALL, 5 );
	monthSizer->Add( new wxCheckBox(monthSelector, ID_SEL_ALL_CHECK, 
		wxT("Select All")), 0, wxALL, 5 );
	
	for( int i=0; i<12; i++ )
	{
		m_plotSurfaces[i] = new wxPLPlotCtrl( this, wxID_ANY );
		m_plotSurfaces[i]->SetIncludeLegendOnExport( true );
		m_plotSurfaces[i]->SetBackgroundColour( *wxWHITE );
		m_plotSurfaces[i]->SetTitle( wxDateTime::GetMonthName(wxDateTime::Month(i)) );
		m_plotSurfaces[i]->ShowGrid( true, false );
		m_plotSurfaces[i]->ShowLegend( false );
	}

	//Add annual profile:
	m_plotSurfaces[12] = new wxPLPlotCtrl(this, wxID_ANY);
	m_plotSurfaces[12]->SetTitle(wxT("Annual Profile"));
	m_plotSurfaces[12]->ShowGrid( true, false );
	m_plotSurfaces[12]->ShowLegend( false );
	m_plotSurfaces[12]->Hide();
	
	m_graphsSizer = new wxGridSizer(0, 4, 0, 0);
	for( int i=0; i<12; i++ )
		m_graphsSizer->Add( m_plotSurfaces[i], 1, wxEXPAND, 0 );
		
	wxFont label_font( *wxNORMAL_FONT );
	label_font.SetWeight( wxFONTWEIGHT_BOLD );
	m_leftAxisLabel = new VerticalLabelCtrl(this, wxID_ANY);
	m_leftAxisLabel->SetFont( label_font );
	m_rightAxisLabel = new VerticalLabelCtrl(this, wxID_ANY);
	m_rightAxisLabel->SetFont( label_font );
		
	wxBoxSizer *graphsAndAxisLabel = new wxBoxSizer(wxHORIZONTAL);
//	graphsAndAxisLabel->Add(m_leftAxisLabel, 0, wxALIGN_CENTER_VERTICAL | wxALL | wxEXPAND, 0);
	graphsAndAxisLabel->Add(m_leftAxisLabel, 0,  wxALL | wxEXPAND, 0);
	graphsAndAxisLabel->Add(m_graphsSizer, 1, wxEXPAND, 0);
//	graphsAndAxisLabel->Add(m_rightAxisLabel, 0, wxALIGN_CENTER_VERTICAL | wxALL | wxEXPAND, 0);
	graphsAndAxisLabel->Add(m_rightAxisLabel, 0,  wxALL | wxEXPAND, 0);

	wxBoxSizer *left_sizer = new wxBoxSizer( wxVERTICAL );
	left_sizer->Add( monthSelector, 0, wxEXPAND, 0 );	
	left_sizer->Add(graphsAndAxisLabel, 1, wxEXPAND, 0);

	m_dataSelector = new wxDVSelectionListCtrl(this, ID_DATA_SELECTOR, 1); 

	wxBoxSizer *main_sizer = new wxBoxSizer( wxHORIZONTAL );
	main_sizer->Add( left_sizer, 1, wxALL|wxEXPAND, 0 );
	main_sizer->Add( m_dataSelector, 0, wxALL|wxEXPAND, 0 );
	SetSizer( main_sizer );
}

wxDVProfileCtrl::~wxDVProfileCtrl()
{
	// removeall the plots manually from the 13 plot surface widgets
	// this regains ownership of the plots that are shown so that they can
	// be deleted in the destructor  ~PlotSet
	for( int i=0; i<m_plots.size(); i++ )
		for( int j=0; j<13; j++ )
			m_plotSurfaces[j]->RemovePlot( m_plots[i]->plots[j] );

	// now delete all the plotsets
	for( int i=m_plots.size()-1; i>=0; i-- )
		delete m_plots[i];
}


void wxDVProfileCtrl::AddDataSet( wxDVTimeSeriesDataSet* d, bool update_ui )
{
	PlotSet *p = new PlotSet( d );
	m_plots.push_back( p );	
	m_dataSelector->Append( d->GetTitleWithUnits(), d->GetGroupName() );

	if (update_ui)
	{
		Layout();
		Refresh();
	}
}

bool wxDVProfileCtrl::RemoveDataSet( wxDVTimeSeriesDataSet *d )
{
	// We never took ownership of d, but we do 
	// need to delete some plottables that we made.

	int index = -1;
	for (int i=0; i<m_plots.size(); i++)
	{
		if (d == m_plots[i]->dataset)
		{
			index = i;
			break;
		}
	}
	
	if ( index < 0 ) return false;
	
	std::vector<int> currently_shown = m_dataSelector->GetSelectionsInCol();

	if ( std::find( currently_shown.begin(), currently_shown.end(), index ) != currently_shown.end() )
		HidePlotAtIndex( index );

	delete m_plots[index]; // this will delete the WPLinePlots in the destructor
	m_plots.erase( m_plots.begin() + index );
	m_dataSelector->RemoveAt( index );
	return true;
}

void wxDVProfileCtrl::RemoveAllDataSets()
{
	HideAllPlots( false );

	for( int i=m_plots.size()-1; i>=0; i-- )
	{
		m_dataSelector->RemoveAt(i);
		delete m_plots[i];
	}

	m_plots.clear();
}

wxDVProfileCtrl::PlotSet::PlotSet( wxDVTimeSeriesDataSet *ds )
{
	dataset = ds;
	axisPosition = wxPLPlotCtrl::Y_LEFT;

	for (int i=0;i<13;i++)
		plots[i] = 0;
}

wxDVProfileCtrl::PlotSet::~PlotSet()
{
	for (int i=0;i<13;i++)
		if ( plots[i] != 0 )
			delete plots[i];
}

void wxDVProfileCtrl::PlotSet::CalculateProfileData( )
{
	if (!dataset || dataset->Length() < 2)
		return;

	bool already_calculated = true;
	for (int i=0;i<13;i++)
		already_calculated = already_calculated && (plots[i] != 0);
	if ( already_calculated ) return;

	// Holds all of the data points we read at each interval so we can average them.
	// We have to store each months data instead of reusing our array because the data could be multi-year.
	dvMatrix<double> dailyData( 12, 24.0/dataset->GetTimeStep(), 0.0 );

	// For averaging
	dvMatrix<int> dataPointCount( 12, 24.0/dataset->GetTimeStep(), 0.0 );

	wxDateTime timeKeeper(01, wxDateTime::Jan, 1970, 00, 00, 00);
	double hours = dataset->At(0).x;
	double offsetFraction = fmod(dataset->At(0).x, dataset->GetTimeStep()); //Non int offsets get chopped off without this.
	timeKeeper.Add(wxTimeSpan::Hours(hours));

	int k = (timeKeeper.GetHour() + (double)timeKeeper.GetMinute() / 60.0) / dataset->GetTimeStep(); //Account for initial offset.
	//Read data points into the correct month array.
	//This code requires a uniform time step. (Otherwise we have to re-define how we compute the average).
	//This will handle multiple years and data that starts mid-year correctly.
	//Multi-year data will all get averaged into the same plots (if there are 2 Jan months in the data, we average over 62 days).
	for (size_t i=0; i < dataset->Length(); i++)
	{
		wxDateTime::Month currentMonth = timeKeeper.GetMonth();
		dailyData.at(currentMonth,k) = dailyData.at(currentMonth,k) + dataset->At(i).y;
		dataPointCount.at(currentMonth,k) += 1;
		k = (k + 1) % int(24 / dataset->GetTimeStep());
		timeKeeper.Add(wxTimeSpan::Minutes(dataset->GetTimeStep() * 60));
		currentMonth = timeKeeper.GetMonth();
		hours += dataset->GetTimeStep();
	}
	
	std::vector< std::vector<wxRealPoint> > plotData(13);
	//Compute average, and then set plottable data.
	for (size_t i=0; i < dailyData.nrows(); i++)
	{
		plotData[i].reserve(dailyData.ncols());
		for (size_t j=0; j<dailyData.ncols(); j++)
		{
			dailyData.at(i,j) = dataPointCount.at(i,j) > 0 ? dailyData.at(i,j) / dataPointCount.at(i,j) : 0; //Do avarage. Don't /0.
			plotData[i].push_back(wxRealPoint(offsetFraction + j*dataset->GetTimeStep(), dailyData.at(i,j)));
		}
	}

	// Average again into annual profile:
	// Right now we are averaging across 12 months.
	// This is more efficient than averaging across days since we already have the monthly averages
	// But, this is slightly less accurate.  (days in Feb have bigger weight than in March because feb has 28 days).
	for (int i = 0; i < plotData[0].size(); i++)
	{
		double average = 0;
		for (int j=0; j < 12; j++)
		{
			average += plotData[j][i].y;
		}
		average /= 12;

		plotData[12].push_back(wxRealPoint(plotData[0][i].x,
			average));
	}

	for(int i=0; i<13; i++) //12 months and annual
	{
		wxString month;
		if ( i < 12 ) month = wxDateTime::GetMonthName( (wxDateTime::Month)i );
		else month = _("Annual");

		if ( plots[i] == 0 ) plots[i] = new wxPLLinePlot;
		plots[i]->SetThickness( 2 );
		plots[i]->SetData( plotData[i] );


		wxString text( dataset->GetGroupName().Len() > 0 
			? dataset->GetGroupName() + ": " + dataset->GetSeriesTitle() 
			: dataset->GetSeriesTitle() );

		if ( !dataset->GetUnits().IsEmpty() )
			text += " (" + dataset->GetUnits() + ")";

		plots[i]->SetLabel( text );
		plots[i]->SetYDataLabel( text );
		plots[i]->SetXDataLabel( _("Time of day") 
			+  " (" + month + " " + _("average") + wxString(")") );
	}
}

/*Event Handlers*/
void wxDVProfileCtrl::OnDataChannelSelection(wxCommandEvent& e)
{
	int row;
	bool isChecked;
	m_dataSelector->GetLastEventInfo( &row, 0, &isChecked );

	if ( isChecked ) ShowPlotAtIndex(row);
	else HidePlotAtIndex(row);
}

void wxDVProfileCtrl::OnMonthSelection(wxCommandEvent& e)
{
	int index = e.GetId() - ID_JAN_CHECK;
	if ( index >= 0 && index < 13 )
		ShowMonthPlotAtIndex( index, m_monthCheckBoxes[index]->IsChecked());
}

void wxDVProfileCtrl::OnSelAllMonths(wxCommandEvent& e)
{
	m_graphsSizer->Clear();
	if (dynamic_cast<wxCheckBox*>(e.GetEventObject())->IsChecked())
	{
		for( int i=0; i<13; i++ )
			m_monthCheckBoxes[i]->SetValue(true);

		for (int i=0; i<13; i++)
		{
			m_plotSurfaces[i]->Show(true);
			m_graphsSizer->Add(m_plotSurfaces[i], 1, wxEXPAND, 0);
		}

		m_numberOfPlotSurfacesShown = 13;

		m_graphsSizer->SetCols(4);
		m_graphsSizer->Layout();
	}
	else
	{
		for( int i=0; i<13; i++ )
		{
			m_monthCheckBoxes[i]->SetValue(false);
			m_plotSurfaces[i]->Show(false);
		}

		m_numberOfPlotSurfacesShown = 0;

		m_graphsSizer->SetCols(1);
		m_graphsSizer->Layout();
	}

	Refresh();
}


void wxDVProfileCtrl::SetMonthIndexSelected(int i, bool value)
{
	if ( i < 0 || i >= 13 ) return;

	if ( m_monthCheckBoxes[i]->IsChecked() != value )
	{
		m_monthCheckBoxes[i]->SetValue( value );
		ShowMonthPlotAtIndex( i, value );
	}
}

bool wxDVProfileCtrl::IsMonthIndexSelected(int i)
{
	if ( i < 0 || i >= 13 ) return false;
	return m_monthCheckBoxes[i]->IsChecked();
}

void wxDVProfileCtrl::ShowMonthPlotAtIndex( int index, bool show )
{
	if ( index < 0 || index >= 13 ) return;

	m_plotSurfaces[index]->Show( show );
	if ( show )
	{
		m_graphsSizer->Clear(); // have to remove all and add in correct order.
		for( int i=0; i<13; i++ )
			if ( m_plotSurfaces[i]->IsShown() )
				m_graphsSizer->Add( m_plotSurfaces[i], 1, wxEXPAND, 0 );

		m_numberOfPlotSurfacesShown++;
	}
	else
	{
		m_graphsSizer->Detach( m_plotSurfaces[index] );
		m_numberOfPlotSurfacesShown--;
	}

	if ( m_numberOfPlotSurfacesShown > 9 )
		m_graphsSizer->SetCols( 4 );
	else if ( m_numberOfPlotSurfacesShown > 4 )
		m_graphsSizer->SetCols( 3 );
	else if ( m_numberOfPlotSurfacesShown > 2 )
		m_graphsSizer->SetCols( 2 );
	else
		m_graphsSizer->SetCols( 1 );

	m_graphsSizer->Layout();
	Refresh();
}

void wxDVProfileCtrl::ShowPlotAtIndex( int i )
{
	if ( i < 0 || i >= m_plots.size() ) return;

	wxString YLabelText;
	size_t NumY1AxisSelections = 0;
	size_t NumY2AxisSelections = 0;
	m_plots[i]->CalculateProfileData(); 
	
	wxPLPlotCtrl::AxisPos yap = wxPLPlotCtrl::Y_LEFT;
	double yaxisMax=0, yaxisMin=0;

	for( int j=0; j < 13; j++ )
	{
		m_plots[i]->plots[j]->ExtendMinMax(NULL, NULL, &yaxisMin, &yaxisMax);
		m_plots[i]->plots[j]->SetColour( m_dataSelector->GetColourForIndex(i) );
				
		wxString units = m_plots[i]->dataset->GetUnits();
		
		wxString y1Units = NO_UNITS, y2Units = NO_UNITS;

		if ( m_plotSurfaces[j]->GetYAxis1() )
			y1Units = m_plotSurfaces[j]->GetYAxis1()->GetUnits();

		if ( m_plotSurfaces[j]->GetYAxis2() )
			y2Units = m_plotSurfaces[j]->GetYAxis2()->GetUnits();

		if ( m_plotSurfaces[j]->GetYAxis1() && y1Units == units )
			yap = wxPLPlotCtrl::Y_LEFT;
		else if ( m_plotSurfaces[j]->GetYAxis2() && y2Units == units )
			yap = wxPLPlotCtrl::Y_RIGHT;
		else if ( m_plotSurfaces[j]->GetYAxis1() == 0 )
			yap = wxPLPlotCtrl::Y_LEFT;
		else
			yap = wxPLPlotCtrl::Y_RIGHT;

		m_plotSurfaces[j]->AddPlot(  m_plots[i]->plots[j], wxPLPlotCtrl::X_BOTTOM, yap );
		m_plotSurfaces[j]->GetAxis(yap)->SetUnits(units);

		YLabelText = units;
		for (size_t i = 0; i < m_dataSelector->Length(); i++)
		{
			if (m_dataSelector->IsSelected(i, 0) && m_plots[i]->dataset->GetUnits() == units)
			{
				if (yap == wxPLPlotCtrl::Y_LEFT)
				{
					NumY1AxisSelections++;
				}
				else
				{
					NumY2AxisSelections++;
				}
			}
		}
		if ((NumY1AxisSelections == 1 && yap == wxPLPlotCtrl::Y_LEFT) || (NumY2AxisSelections == 1 && yap == wxPLPlotCtrl::Y_RIGHT))
		{
			YLabelText = m_plots[i]->dataset->GetLabel();
		}
		m_plotSurfaces[j]->GetAxis(yap)->SetLabel(YLabelText);

		m_plotSurfaces[j]->GetXAxis1()->SetWorld(0, 24);
		m_plotSurfaces[j]->GetXAxis1()->ShowLabel( false );
		m_plotSurfaces[j]->GetYAxis1()->ShowLabel( false );
		if ( m_plotSurfaces[j]->GetYAxis2() )
			m_plotSurfaces[j]->GetYAxis2()->ShowLabel( false );
	}

	m_plots[i]->axisPosition = yap;

	for(int j=0; j<13; j++)
		m_plotSurfaces[j]->GetAxis(yap)->SetWorld(yaxisMin, yaxisMax);

	m_leftAxisLabel->SetLabelText( m_plotSurfaces[0]->GetYAxis1()->GetLabel(), 90);
	if (m_plotSurfaces[0]->GetYAxis2() 
		&& m_plotSurfaces[0]->GetYAxis2()->GetLabel() != m_plotSurfaces[0]->GetYAxis1()->GetLabel())
		m_rightAxisLabel->SetLabelText(m_plotSurfaces[0]->GetYAxis2()->GetLabel(), 270);

	AutoScaleYAxes();
	RefreshDisabledCheckBoxes();

	Layout();
	for ( int j=0;j<13;j++ )
		m_plotSurfaces[j]->Refresh();
}

void wxDVProfileCtrl::HidePlotAtIndex(int i, bool update)
{
	if (i < 0 || i >= m_plots.size())
		return;
	
	wxString YLabelText;
	size_t NumY1AxisSelections = 0;
	size_t NumY2AxisSelections = 0;
	int FirstY1AxisSelectionIndex = -1;
	int FirstY2AxisSelectionIndex = -1;
	wxPLPlotCtrl::AxisPos yap = wxPLPlotCtrl::Y_LEFT;
	wxString y1Units = NO_UNITS, y2Units = NO_UNITS;
	int SelIndex = -1;
	wxString units = m_plots[i]->dataset->GetUnits();

	if (m_plotSurfaces[0]->GetYAxis1())
		y1Units = m_plotSurfaces[0]->GetYAxis1()->GetUnits();

	if (m_plotSurfaces[0]->GetYAxis2())
		y2Units = m_plotSurfaces[0]->GetYAxis2()->GetUnits();

	std::vector<int> currently_shown = m_dataSelector->GetSelectionsInCol();

	for (size_t j = 0; j < currently_shown.size(); j++)
	{
		if (m_plots[currently_shown[j]]->dataset->GetUnits() == y1Units)
		{
			NumY1AxisSelections++;
			if (FirstY1AxisSelectionIndex == -1) { FirstY1AxisSelectionIndex = currently_shown[j]; }
		}
		if (m_plots[currently_shown[j]]->dataset->GetUnits() == y2Units)
		{
			NumY2AxisSelections++;
			if (FirstY2AxisSelectionIndex == -1) { FirstY2AxisSelectionIndex = currently_shown[j]; }
		}
	}

	if (currently_shown.size() == 0)
	{
		for (int k=0; k<13; k++)
		{
			m_plotSurfaces[k]->SetYAxis1( 0 );
			m_plotSurfaces[k]->SetYAxis2( 0 );
		}

		m_leftAxisLabel->SetLabelText( wxEmptyString, 90 );
		m_rightAxisLabel->SetLabelText( wxEmptyString, 90 );
	}
	else
	{
		if (NumY1AxisSelections > 0)
		{
			YLabelText = y1Units;
			if (NumY1AxisSelections == 1 && FirstY1AxisSelectionIndex > -1) { YLabelText = m_plots[FirstY1AxisSelectionIndex]->dataset->GetLabel(); }
			for (int k = 0; k<13; k++)
			{
				m_plotSurfaces[k]->GetXAxis1()->SetWorld(0, 24);
				if (m_plotSurfaces[k]->GetAxis(wxPLPlotCtrl::Y_LEFT))
				{
					m_plotSurfaces[k]->GetAxis(wxPLPlotCtrl::Y_LEFT)->SetUnits(y1Units);
					m_plotSurfaces[k]->GetAxis(wxPLPlotCtrl::Y_LEFT)->SetLabel(YLabelText);
				}
			}

			if (NumY2AxisSelections > 0)
			{
				YLabelText = y2Units;
				if (NumY2AxisSelections == 1 && FirstY2AxisSelectionIndex > -1) { YLabelText = m_plots[FirstY2AxisSelectionIndex]->dataset->GetLabel(); }
				for (int k = 0; k<13; k++)
				{
					if (m_plotSurfaces[k]->GetAxis(wxPLPlotCtrl::Y_RIGHT))
					{
						m_plotSurfaces[k]->GetAxis(wxPLPlotCtrl::Y_RIGHT)->SetUnits(y2Units);
						m_plotSurfaces[k]->GetAxis(wxPLPlotCtrl::Y_RIGHT)->SetLabel(YLabelText);
					}
				}
			}
			else
			{
				for (int k = 0; k<13; k++)
				{
					m_plotSurfaces[k]->SetYAxis2(0);
				}
			}
		}
		else if (NumY2AxisSelections > 0)	//We deselected the last variable with Y1 units, so move Y2 to Y1 
		{
			for (int j = 0; j < currently_shown.size(); j++)
			{
				int index = currently_shown[j];
				m_plots[index]->CalculateProfileData();
				m_plots[index]->axisPosition = wxPLPlotCtrl::Y_LEFT;
				for (int k = 0; k<13; k++)
				{
					m_plotSurfaces[k]->RemovePlot(m_plots[index]->plots[k]);
					m_plotSurfaces[k]->AddPlot(m_plots[index]->plots[k], wxPLPlotCtrl::X_BOTTOM, wxPLPlotCtrl::Y_LEFT);
				}
			}

			YLabelText = y2Units;
			if (NumY2AxisSelections == 1 && FirstY2AxisSelectionIndex > -1) { YLabelText = m_plots[FirstY2AxisSelectionIndex]->dataset->GetLabel(); }
			for (int k = 0; k<13; k++)
			{
				m_plotSurfaces[k]->GetXAxis1()->SetWorld(0, 24);
				if (m_plotSurfaces[k]->GetAxis(wxPLPlotCtrl::Y_LEFT))
				{
					m_plotSurfaces[k]->GetAxis(wxPLPlotCtrl::Y_LEFT)->SetUnits(y2Units);
					m_plotSurfaces[k]->GetAxis(wxPLPlotCtrl::Y_LEFT)->SetLabel(YLabelText);
				}

				m_plotSurfaces[k]->SetYAxis2(0);
			}
		}

		if (m_plotSurfaces[0]->GetYAxis1()) 
		{ 
			m_leftAxisLabel->SetLabelText(m_plotSurfaces[0]->GetYAxis1()->GetLabel(), 90); 
		}
		else
		{
			m_leftAxisLabel->SetLabelText(wxEmptyString, 90);
		}

		if (m_plotSurfaces[0]->GetYAxis2() && m_plotSurfaces[0]->GetYAxis2()->GetLabel() != m_plotSurfaces[0]->GetYAxis1()->GetLabel())
		{
			m_rightAxisLabel->SetLabelText(m_plotSurfaces[0]->GetYAxis2()->GetLabel(), 270);
		}
		else
		{
			m_rightAxisLabel->SetLabelText( wxEmptyString, 90);
		}
	}

	AutoScaleYAxes();
	RefreshDisabledCheckBoxes();

	for (int j=0; j<13; j++)
		m_plotSurfaces[j]->RemovePlot( m_plots[i]->plots[j] );

	if (update)
	{
		Refresh();
		Layout();
	}
}

void wxDVProfileCtrl::HideAllPlots(bool update)
{
	for( int i=0; i<m_plots.size(); i++ )
		for( int j=0; j<13; j++ )
			m_plotSurfaces[j]->RemovePlot( m_plots[i]->plots[j] );
	
	for (int k=0; k<13; k++)
	{
		m_plotSurfaces[k]->SetYAxis1( 0 );
		m_plotSurfaces[k]->SetYAxis2( 0 );
	}

	m_leftAxisLabel->SetLabelText( wxEmptyString, 90 );
	m_rightAxisLabel->SetLabelText( wxEmptyString, 90 );

	RefreshDisabledCheckBoxes();

	if (update)
	{
		Refresh();
		Layout();
	}
}

void wxDVProfileCtrl::AutoScaleYAxes()
{
	std::vector<int> currently_shown = m_dataSelector->GetSelectionsInCol();
	double leftYAxisMax = 0, leftYAxisMin = 1000000000, rightYAxisMin = 1000000000, rightYAxisMax = 0;

	for(int i=0; i<13; i++)
	{
		for (int j=0; j<currently_shown.size(); j++)
		{
			switch(m_plots[currently_shown[j]]->axisPosition)
			{
			case wxPLPlotCtrl::Y_LEFT:
				m_plots[currently_shown[j]]->plots[i]->ExtendMinMax(NULL, NULL, &leftYAxisMin, &leftYAxisMax, true);
				break;
			case wxPLPlotCtrl::Y_RIGHT:
				m_plots[currently_shown[j]]->plots[i]->ExtendMinMax(NULL, NULL, &rightYAxisMin, &rightYAxisMax, true);
				break;
			//We don't care about x-axis.  It WILL be Y.
			case wxPLPlotCtrl::X_BOTTOM:
			case wxPLPlotCtrl::X_TOP:
				break;
			}
		}
	}

	for (int i=0; i<13; i++)
	{
		if (m_plotSurfaces[i]->GetYAxis1())
			m_plotSurfaces[i]->GetYAxis1()->SetWorld(leftYAxisMin, leftYAxisMax);

		if (m_plotSurfaces[i]->GetYAxis2())
			m_plotSurfaces[i]->GetYAxis2()->SetWorld(rightYAxisMin, rightYAxisMax);
	}
}

void wxDVProfileCtrl::RefreshDisabledCheckBoxes()
{
	std::vector<int> currently_shown = m_dataSelector->GetSelectionsInCol();
	if ( currently_shown.size() == 0 )
	{
		for (int i=0; i<m_plots.size(); i++)
			m_dataSelector->Enable(i, 0, true);
		return;
	}

	int first_plot_index = currently_shown[0];
	if ( first_plot_index < 0 || first_plot_index >= m_plots.size() )
		return;

	wxDVTimeSeriesDataSet *my_dataset = m_plots[first_plot_index]->dataset;

	wxString units1 = my_dataset->GetUnits();
	wxString units2;
	bool units2Set = false;

	for( int i=1; i<currently_shown.size(); i++ )
	{
		if ( m_plots[currently_shown[i]]->dataset->GetUnits() != units1 )
		{
			units2 = m_plots[currently_shown[i]]->dataset->GetUnits();
			units2Set = true;
			break;
		}
	}

	if (!units2Set)
	{
		for (int i=0; i<m_plots.size(); i++)
			m_dataSelector->Enable(i, 0, true);
	}
	else
	{
		for (int i=0; i<m_plots.size(); i++)
			m_dataSelector->Enable( i, 0, units1 == m_plots[i]->dataset->GetUnits()
				|| units2 == m_plots[i]->dataset->GetUnits() );
	}
}

wxDVSelectionListCtrl* wxDVProfileCtrl::GetDataSelectionList()
{
	return m_dataSelector;
}

void wxDVProfileCtrl::SetSelectedNames(const wxString& names)
{
	HideAllPlots(false);

	wxStringTokenizer tkz(names, ";");

	while(tkz.HasMoreTokens())
	{
		wxString token = tkz.GetNextToken();

		int row = m_dataSelector->SelectRowWithNameInCol(token);
		if (row != -1)
			ShowPlotAtIndex(row);
	}
}

void wxDVProfileCtrl::SelectDataSetAtIndex(int index)
{
	if ( index >= 0 && index < (int) m_plots.size() )
	{
		m_dataSelector->SelectRowInCol(index);
		ShowPlotAtIndex(index);
	}
}

int wxDVProfileCtrl::GetNumberOfSelections()
{
	return m_dataSelector->GetNumberOfSelections();
}

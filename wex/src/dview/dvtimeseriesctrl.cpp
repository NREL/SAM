
/*
 * wxDVTimeSeriesCtrl.cpp
 *
 * This class Is a wxPanel that contains a time-series graph with its axes and a scrollbar, as well as
 * a list of channels (different data sets) that can be viewed.
 *
 * Note: Plot units are hours on the x-axis.  To handle these, we use wxTimeDate objects to get the date
 * for axis labels.  If no year is specified, we use 1970 because the year doesn't matter.
 */

#include <algorithm>
#include <wx/scrolbar.h>
#include <wx/gbsizer.h>
#include <wx/tokenzr.h>
#include <wx/statline.h>
#include <wx/gdicmn.h>

#include "wex/plot/pllineplot.h"

#include "wex/icons/zoom_in.cpng"
#include "wex/icons/zoom_out.cpng"
#include "wex/icons/zoom_fit.cpng"
#include "wex/icons/preferences.cpng"

#include "wex/dview/dvselectionlist.h"
#include "wex/dview/dvtimeseriesdataset.h"
#include "wex/dview/dvtimeseriesctrl.h"

#include <wex/radiochoice.h>

#if defined(__WXOSX__)||defined(__WXGTK__)
#include <cmath>
#undef wxIsNaN
#define wxIsNaN(a) std::isnan(a)
#endif

static const wxString NO_UNITS("ThereAreNoUnitsForThisAxis.");
enum { ID_TopCheckbox = wxID_HIGHEST + 1, ID_BottomCheckbox, ID_StatCheckbox };

class wxDVTimeSeriesPlot : public wxPLPlottable
{
	private:
		wxDVTimeSeriesDataSet *m_data;
		wxColour m_colour;
		wxDVTimeSeriesStyle m_style;
		wxDVTimeSeriesType m_seriesType;
		bool m_ownsDataset;
		wxDVTimeSeriesPlot *m_stackedOnTopOf;
		bool m_stacked;

	public:
		wxDVTimeSeriesPlot( wxDVTimeSeriesDataSet *ds, wxDVTimeSeriesType seriesType, bool OwnsDataset = false )
			: m_data(ds), m_stackedOnTopOf( 0 )
		{
			assert( ds != 0 );

			m_stacked = false;
			m_colour = *wxRED;
			m_seriesType = seriesType;
			m_style = (seriesType == wxDV_RAW || seriesType == wxDV_HOURLY) ? wxDV_NORMAL : wxDV_STEPPED;
			m_ownsDataset = OwnsDataset;
		}

		~wxDVTimeSeriesPlot()
		{
			if(m_ownsDataset)
			{
				delete m_data;
			}
		}

		void SetStackingMode( bool b ) { m_stacked = b; }
		bool GetStackingMode() { return m_stacked; }
		
		void StackOnTopOf( wxDVTimeSeriesPlot *p )
		{
			if ( !p ) { m_stackedOnTopOf = 0; return; }

			wxDVTimeSeriesDataSet *ds = p->GetDataSet();
			if ( ds != 0
				&& ds->GetOffset() == m_data->GetOffset()
				&& ds->Length() == m_data->Length()
				&& ds->GetTimeStep() == m_data->GetTimeStep() )
			{
				m_stackedOnTopOf = p;
			}
		}

		bool IsStackedOnTopOf( wxDVTimeSeriesPlot * p )
		{
			return m_stackedOnTopOf == p;
		}

		void SetStyle( wxDVTimeSeriesStyle ss ) { m_style = ss; }
		void SetColour( const wxColour &col ) { m_colour = col; }

		virtual wxString GetXDataLabel( wxPLPlot * ) const 
		{
			return _("Hours since 00:00 Jan 1");
		}

		virtual wxString GetYDataLabel( wxPLPlot * ) const
		{
			if ( !m_data ) return wxEmptyString;

			wxString label( 
				m_data->GetGroupName().Len() > 0 
					? m_data->GetGroupName() + ": " + m_data->GetSeriesTitle() 
					: m_data->GetSeriesTitle() );

			if ( !m_data->GetUnits().IsEmpty() )
				label += " (" + m_data->GetUnits() + ")";

			return label;
		}
		
		virtual wxRealPoint At( size_t i ) const
		{
			return i < m_data->Length() 
				? m_data->At(i)
				: wxRealPoint( std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN() );
		}
		
		virtual wxRealPoint StackedAt( size_t i, double *ybase = 0 ) const
		{
			if ( m_stackedOnTopOf == this )
				wxMessageBox("Error - how did a plot get stacked on itself??" );

			if ( i >= m_data->Length() ) 
				return wxRealPoint( std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN() );
			
			if ( m_stackedOnTopOf != 0 && m_stackedOnTopOf != this )
			{
				wxRealPoint base( m_stackedOnTopOf->StackedAt(i, ybase ) );
				if ( ybase ) *ybase = base.y;
				wxRealPoint cur( m_data->At(i)  );
				return wxRealPoint( cur.x, base.y + cur.y );
			}
			else
			{
				if ( ybase ) *ybase = 0;
				return m_data->At(i) ;
			}

		}

		virtual size_t Len() const
		{
			return m_data->Length();
		}

		virtual void Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map )
		{
			if ( !m_data || m_data->Length() < 2 ) return;

			size_t len;
			std::vector< wxRealPoint > points;
			wxRealPoint rpt;
			wxRealPoint rpt2;
			double tempY;
			wxRealPoint wmin = map.GetWorldMinimum();
			wxRealPoint wmax = map.GetWorldMaximum();
		
			dc.Pen( m_colour, 2 );

			// check that Y axis is a left axis, so as to
			// only allow stacking on the left axis.
			// otherwise, this could result in problems
			// if a plot is checked for both left and right Y axes,
			// since this wxDVTimeSeriesPlot instance is the same for both
			if ( m_stacked && map.IsPrimaryXAxis() )
			{
				len = m_data->Length();

				size_t reserve_len = len;
				if ( m_style == wxDV_STEPPED )
					reserve_len *= 2;

				// reserve double the baseline need since will
				// use this array to create the wraparound polygon
				// for the stacked area
				points.reserve( reserve_len*2 ); 
				
				double timeStep = m_data->GetTimeStep();
				double lowX;
				double highX;

				if ( m_stackedOnTopOf != 0 )
				{
					std::vector<wxRealPoint> base;
					base.reserve( reserve_len );
					for( size_t i=0;i<len;i++ )
					{
						double yb = 0;
						wxRealPoint p( StackedAt(i, &yb) );					
						if ( p.x < wmin.x || p.x > wmax.x ) continue;

						if ( m_style == wxDV_STEPPED )
						{
							lowX = GetPeriodLowerBoundary( p.x, timeStep );
							highX = GetPeriodUpperBoundary( p.x, timeStep );
							points.push_back( map.ToDevice( lowX, p.y ) );
							points.push_back( map.ToDevice( highX, p.y ) );
							base.push_back( wxRealPoint(lowX, yb) );
							base.push_back( wxRealPoint( highX, yb ) );
						}
						else
						{
							base.push_back( wxRealPoint( p.x, yb ) );
							points.push_back( map.ToDevice( p ) );
						}
					}

					for( size_t i=0;i<base.size();i++ )
						points.push_back( map.ToDevice( base[ base.size() - i - 1 ] ) );
				}
				else
				{	
					for( size_t i=0;i<len;i++ )
					{
						wxRealPoint p( At(i) );					
						if ( p.x < wmin.x || p.x > wmax.x ) continue;
						if ( m_style == wxDV_STEPPED )
						{
							lowX = GetPeriodLowerBoundary( p.x, timeStep );
							highX = GetPeriodUpperBoundary( p.x, timeStep );
							points.push_back( map.ToDevice( lowX, p.y ) );
							points.push_back( map.ToDevice( highX, p.y ) );
						}
						else
							points.push_back( map.ToDevice( p ) );
					}

					points.push_back( wxRealPoint( points[ points.size()-1 ].x,  map.ToDevice( wxRealPoint(0,0) ).y ) );
					points.push_back( wxRealPoint( points[ 0 ].x, map.ToDevice( wxRealPoint(0,0) ).y ) );
				}

				wxRealPoint pos, size;
				map.GetDeviceExtents( &pos, &size );
				if ( points.size() > (int)(3.0*size.x) ) {
					dc.Text( "too many data points: please zoom in", pos );
					return; // quit if 3x more x coord points than pixels
				}
				
				dc.Pen( *wxBLACK, 0, wxPLOutputDevice::NONE );
				dc.Brush( m_colour );
				dc.Polygon( points.size(), &points[0], wxPLOutputDevice::WINDING_RULE );
			}
			else
			{
				// not stacked - just lines

				if( m_style == wxDV_NORMAL )
				{
					len = m_data->Length();
					if(m_data->At(0).x < wmin.x) { len++; }
					if(m_data->At(m_data->Length() - 1).x > wmax.x) { len++; }

					points.reserve( len );

					//If this is a line plot then add a point at the left edge of the graph if there isn't one there in the data
					if(m_style == wxDV_NORMAL && m_data->At(0).x < wmin.x)
					{
						for ( size_t i = 1; i < m_data->Length(); i++ )
						{
							rpt = m_data->At(i);
							rpt2 = m_data->At(i - 1);
							if ( rpt.x > wmin.x )
							{
								tempY = rpt2.y + ((rpt.y - rpt2.y) * (wmin.x - rpt2.x) / (rpt.x - rpt2.x));
								points.push_back( map.ToDevice( wxRealPoint(wmin.x, tempY) ) );
								break;
							}
						}
					}
					
					// cull data points that get mapped to the same X coordinate on the device
					// this is a much needed rendering optimization for large datasets
					size_t i = 0;
					size_t len = m_data->Length();
					while( i < len )
					{
						rpt = m_data->At(i);
						if ( rpt.x < wmin.x || rpt.x > wmax.x ) {
							i++;
							continue;
						}
						
						wxRealPoint cur( map.ToDevice( rpt ) );
						
						size_t jmin=i, jmax=i;
						double min=rpt.y, max=rpt.y;
						// scan ahead in the points array to find all values with the same X
						// coordinate, and the associated min/max Y values
						size_t j=i+1;
						size_t npscan = 0;
						wxRealPoint rpt2;
						while( j < len )
						{
							rpt2 = m_data->At(j);
							wxRealPoint cur2( map.ToDevice( rpt2 ) );

							if ( wxRound(cur.x) != wxRound(cur2.x) )
								break;

							if ( rpt2.y > max ) { max = rpt2.y; jmax = j;}
							if ( rpt2.y < min ) { min = rpt2.y; jmin = j;}

							npscan++;
							j++;
						}

						if ( npscan > 0 ) // duplicate points found for same x coordinate
						{
							// replace with just two points						
							points.push_back( map.ToDevice( wxRealPoint( rpt.x, jmax < jmin ? max : min ) ) );
							points.push_back( map.ToDevice( wxRealPoint( rpt.x, jmax < jmin ? min : max ) ) );
						}
						else
						{
							// no multiple points with same x coordinate, just keep this point where it is.
							points.push_back( cur );
						}

						i=j;
					}

					//If this is a line plot then add a point at the right edge of the graph if there isn't one there in the data
					if(m_style == wxDV_NORMAL && m_data->At(m_data->Length() - 1).x > wmax.x)
					{
						for ( int i = m_data->Length() - 2; i >= 0; i-- )
						{
							rpt = m_data->At(i);
							rpt2 = m_data->At(i + 1);
							if ( rpt.x < wmax.x )
							{
								tempY = rpt.y + ((rpt2.y - rpt.y) * (wmax.x - rpt.x) / (rpt2.x - rpt.x));
								points.push_back( map.ToDevice( wxRealPoint(wmax.x, tempY) ) );
								break;
							}
						}
					}
				}
				else
				{
					//For stepped graphs create an array twice as big as the original and replace each single x value with two x values, one with the prior y value and one with the current y value
					len = m_data->Length() * 2;
					points.reserve( len );
					double timeStep = m_data->GetTimeStep();
					double lowX;
					double highX;
					double priorY;
					double nextY;

					for (size_t i = 0; i < m_data->Length(); i++)
					{
						rpt = m_data->At(i);
						lowX = GetPeriodLowerBoundary(rpt.x, timeStep);
						highX = GetPeriodUpperBoundary(rpt.x, timeStep);
					
						if(lowX >= wmin.x && highX <= wmax.x)	//Draw points for the lower and upper X boundaries of the point's horizontal range for each range that fits in the boundaries of the plot
						{
							//If the prior point's lower X boundary is off the left edge of the plot then draw points for the left edge of the plot and the visible point's lower X boundary at the prior point's Y
							if(i > 0 && GetPeriodLowerBoundary(m_data->At(i - 1).x, timeStep) < wmin.x)
							{
								priorY = m_data->At(i - 1).y;
								points.push_back( map.ToDevice( wxRealPoint(wmin.x, priorY) ) );
								points.push_back( map.ToDevice( wxRealPoint(lowX, priorY) ) );
							}

							points.push_back( map.ToDevice( wxRealPoint(lowX, rpt.y) ) );
							points.push_back( map.ToDevice( wxRealPoint(highX, rpt.y) ) );

							//If the next point's upper X boundary is off the right edge of the plot then draw points for the visible point's upper X boundary and the right edge of the plot at the next point's Y
							if(i < m_data->Length() - 1 && GetPeriodUpperBoundary(m_data->At(i + 1).x, timeStep) > wmax.x)
							{
								nextY = m_data->At(i + 1).y;
								points.push_back( map.ToDevice( wxRealPoint(highX, nextY) ) );
								points.push_back( map.ToDevice( wxRealPoint(wmax.x, nextY) ) );
								break;	//Any future points are outside the bounds of the graph
							}
						}
						else if(lowX < wmin.x && highX > wmin.x && highX <= wmax.x)	//Draw points for the plot left edge and point's upper X boundary at the point's Y if the lower boundary (only) is off the plot's left edge
						{
							points.push_back( map.ToDevice( wxRealPoint(wmin.x, rpt.y) ) );
							points.push_back( map.ToDevice( wxRealPoint(highX, rpt.y) ) );

							//If the next point's upper X boundary is off the right edge of the plot then draw points for the visible point's upper X boundary and the right edge of the plot at the next point's Y
							if(i < m_data->Length() - 1 && GetPeriodUpperBoundary(m_data->At(i + 1).x, timeStep) > wmax.x)
							{
								nextY = m_data->At(i + 1).y;
								points.push_back( map.ToDevice( wxRealPoint(highX, nextY) ) );
								points.push_back( map.ToDevice( wxRealPoint(wmax.x, nextY) ) );
								break;	//Any future points are outside the bounds of the graph
							}
						}
						else if(highX > wmax.x && lowX < wmax.x && lowX >= wmin.x)	//Draw points for the point's upper X boundary and the plot right edge at the point's Y if the upper boundary (only) is off the plot's right edge
						{
							//If the prior point's lower X boundary is off the left edge of the plot then draw points for the left edge of the plot and the visible point's lower X boundary at the prior point's Y
							if(i > 0 && GetPeriodLowerBoundary(m_data->At(i - 1).x, timeStep) < wmin.x)
							{
								priorY = m_data->At(i - 1).y;
								points.push_back( map.ToDevice( wxRealPoint(wmin.x, priorY) ) );
								points.push_back( map.ToDevice( wxRealPoint(lowX, priorY) ) );
							}

							points.push_back( map.ToDevice( wxRealPoint(wmin.x, rpt.y) ) );
							points.push_back( map.ToDevice( wxRealPoint(highX, rpt.y) ) );
						}
						else if(lowX < wmin.x && highX > wmax.x)	//Draw points for the plot's left and right edges and point's Y if the point's lower X boundary is off the plot's left edge and the upper X boundary is off the right edge
						{
							points.push_back( map.ToDevice( wxRealPoint(wmin.x, rpt.y) ) );
							points.push_back( map.ToDevice( wxRealPoint(highX, rpt.y) ) );
						}
					}
				}

				if ( points.size() < 2 ) return;

				wxRealPoint devpos, devsize;
				map.GetDeviceExtents( &devpos, &devsize );

				if ( points.size() > 4 * devsize.x ) {
					dc.Text( "too many data points: please zoom in", devpos );
					return; // quit if 4x more x coord points than integer device units
				}
				
				dc.Lines( points.size(), &points[0] );

			}
		}

		virtual wxString GetLabel() const
		{
			return GetYDataLabel( NULL );
			//if ( !m_data ) return wxEmptyString;
			//else return m_data->GetTitleWithUnits();
		}

		virtual void DrawInLegend( wxPLOutputDevice &dc, const wxPLRealRect &rct )
		{
			dc.Pen( m_colour, 3, wxPLOutputDevice::SOLID, wxPLOutputDevice::MITER, wxPLOutputDevice::BUTT );
			dc.Line( rct.x, rct.y+rct.height/2, rct.x+rct.width, rct.y+rct.height/2 );
		}

		double GetPeriodLowerBoundary(double hourNumber, double timeStep)
		{
			if(m_seriesType == wxDV_DAILY)
			{
				hourNumber = hourNumber - fmod(hourNumber, 24);
			}
			else if (m_seriesType == wxDV_MONTHLY)
			{
				double year = hourNumber - fmod(hourNumber, 8760);
				hourNumber = fmod(hourNumber, 8760);

				if(hourNumber >= 0.0 && hourNumber < 744.0) { hourNumber = 0.0; }
				else if(hourNumber >= 744.0 && hourNumber < 1416.0) { hourNumber = 744.0; }
				else if(hourNumber >= 1416.0 && hourNumber < 2160.0) { hourNumber = 1416.0; }
				else if(hourNumber >= 2160.0 && hourNumber < 2880.0) { hourNumber = 2160.0; }
				else if(hourNumber >= 2880.0 && hourNumber < 3624.0) { hourNumber = 2880.0; }
				else if(hourNumber >= 3624.0 && hourNumber < 4344.0) { hourNumber = 3624.0; }
				else if(hourNumber >= 4344.0 && hourNumber < 5088.0) { hourNumber = 4344.0; }
				else if(hourNumber >= 5088.0 && hourNumber < 5832.0) { hourNumber = 5088.0; }
				else if(hourNumber >= 5832.0 && hourNumber < 6552.0) { hourNumber = 5832.0; }
				else if(hourNumber >= 6552.0 && hourNumber < 7296.0) { hourNumber = 6552.0; }
				else if(hourNumber >= 7296.0 && hourNumber < 8016.0) { hourNumber = 7296.0; }
				else if(hourNumber >= 8016.0 && hourNumber < 8760.0) { hourNumber = 8016.0; }

				hourNumber = hourNumber + year;
			}
			else
			{
				hourNumber = hourNumber - timeStep/2.0;
			}

			return hourNumber;
		}

		double GetPeriodUpperBoundary(double hourNumber, double timeStep)
		{
			if(m_seriesType == wxDV_DAILY)
			{
				hourNumber = hourNumber - fmod(hourNumber, 24) + 24;
			}
			else if (m_seriesType == wxDV_MONTHLY)
			{
				double year = hourNumber - fmod(hourNumber, 8760);
				hourNumber = fmod(hourNumber, 8760);

				if(hourNumber >= 0.0 && hourNumber < 744.0) { hourNumber = 744.0; }
				else if(hourNumber >= 744.0 && hourNumber < 1416.0) { hourNumber = 1416.0; }
				else if(hourNumber >= 1416.0 && hourNumber < 2160.0) { hourNumber = 2160.0; }
				else if(hourNumber >= 2160.0 && hourNumber < 2880.0) { hourNumber = 2880.0; }
				else if(hourNumber >= 2880.0 && hourNumber < 3624.0) { hourNumber = 3624.0; }
				else if(hourNumber >= 3624.0 && hourNumber < 4344.0) { hourNumber = 4344.0; }
				else if(hourNumber >= 4344.0 && hourNumber < 5088.0) { hourNumber = 5088.0; }
				else if(hourNumber >= 5088.0 && hourNumber < 5832.0) { hourNumber = 5832.0; }
				else if(hourNumber >= 5832.0 && hourNumber < 6552.0) { hourNumber = 6552.0; }
				else if(hourNumber >= 6552.0 && hourNumber < 7296.0) { hourNumber = 7296.0; }
				else if(hourNumber >= 7296.0 && hourNumber < 8016.0) { hourNumber = 8016.0; }
				else if(hourNumber >= 8016.0 && hourNumber < 8760.0) { hourNumber = 8760.0; }

				hourNumber = hourNumber + year;
			}
			else
			{
				hourNumber = hourNumber + timeStep/2.0;
			}

			return hourNumber;
		}

		virtual void UpdateSummaryData(bool divide)
		{
			double factor = 24.0;

			for(size_t i = 0; i < m_data->Length(); i++)
			{
				if (m_seriesType == wxDV_MONTHLY)
				{
					factor = fmod(m_data->At(i).x, 8760);

					if(factor >= 0.0 && factor < 744.0) { factor = 744.0; }
					else if(factor >= 744.0 && factor < 1416.0) { factor = 672.0; }
					else if(factor >= 1416.0 && factor < 2160.0) { factor = 744.0; }
					else if(factor >= 2160.0 && factor < 2880.0) { factor = 720.0; }
					else if(factor >= 2880.0 && factor < 3624.0) { factor = 744.0; }
					else if(factor >= 3624.0 && factor < 4344.0) { factor = 720.0; }
					else if(factor >= 4344.0 && factor < 5088.0) { factor = 744.0; }
					else if(factor >= 5088.0 && factor < 5832.0) { factor = 744.0; }
					else if(factor >= 5832.0 && factor < 6552.0) { factor = 720.0; }
					else if(factor >= 6552.0 && factor < 7296.0) { factor = 744.0; }
					else if(factor >= 7296.0 && factor < 8016.0) { factor = 720.0; }
					else if(factor >= 8016.0 && factor < 8760.0) { factor = 744.0; }
				}
				else if (m_seriesType == wxDV_DAILY)
				{
					factor = 24.0;
				}
				else
				{
					factor = m_data->GetTimeStep();
				}


				if ( wxDVArrayDataSet *arrdata = dynamic_cast<wxDVArrayDataSet*>( m_data ) )
				{
					if( divide ) arrdata->SetY( i, m_data->At(i).y / factor );
					else arrdata->SetY( i, m_data->At(i).y * factor );
				}
			}
		}

		wxDVTimeSeriesDataSet *GetDataSet() const { return m_data; }
};

BEGIN_EVENT_TABLE(wxDVTimeSeriesSettingsDialog, wxDialog)
	EVT_CHECKBOX(ID_TopCheckbox, wxDVTimeSeriesSettingsDialog::OnClickTopHandler)
	EVT_CHECKBOX(ID_BottomCheckbox, wxDVTimeSeriesSettingsDialog::OnClickBottomHandler)
	EVT_CHECKBOX(ID_StatCheckbox, wxDVTimeSeriesSettingsDialog::OnClickStatHandler)
END_EVENT_TABLE()

wxDVTimeSeriesSettingsDialog::wxDVTimeSeriesSettingsDialog( wxWindow *parent, const wxString &title, 
	bool isTopRightYVisible, bool isBottomGraphVisible, bool isBottomRightGraphVisible )
	: wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, wxRESIZE_BORDER|wxDEFAULT_DIALOG_STYLE)
{
	
	mSteppedLines = new wxCheckBox( this, wxID_ANY, "Stepped lines" );
	mStackedArea = new wxCheckBox( this, wxID_ANY, "Stacked area on left Y axis");
	mStatTypeCheck = new wxCheckBox(this, ID_StatCheckbox, "Show sum over time step" );
				
		
	wxFlexGridSizer *yBoundSizer = new wxFlexGridSizer(4, 2, 2);
	yBoundSizer->AddSpacer(1);
	yBoundSizer->Add( new wxStaticText( this, wxID_ANY, "Autoscale" ), 0, wxLEFT|wxRIGHT|wxALIGN_CENTER|wxALIGN_CENTER_VERTICAL, 2 );
	yBoundSizer->Add( new wxStaticText( this, wxID_ANY, "Y Min" ), 0, wxLEFT|wxRIGHT|wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL, 2 );
	yBoundSizer->Add( new wxStaticText( this, wxID_ANY, "Y Max" ), 0, wxLEFT|wxRIGHT|wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL, 2 );

	yBoundSizer->Add( new wxStaticText( this, wxID_ANY, "Top left axis:" ), 0, wxLEFT|wxRIGHT|wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL, 3 );
	yBoundSizer->Add( mTopAutoscaleCheck = new wxCheckBox(this, ID_TopCheckbox, wxEmptyString), 0, wxLEFT|wxRIGHT|wxALIGN_CENTER|wxALIGN_CENTER_VERTICAL,  2);	
	yBoundSizer->Add( mTopYMinCtrl = new wxNumericCtrl(this), 0, wxLEFT|wxRIGHT, 2);	
	yBoundSizer->Add( mTopYMaxCtrl = new wxNumericCtrl(this), 0, wxLEFT|wxRIGHT, 2);

	if ( isTopRightYVisible )
	{
		yBoundSizer->Add( new wxStaticText( this, wxID_ANY, "Top right axis:" ), 0, wxLEFT|wxRIGHT|wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL, 3 );
		yBoundSizer->Add( mTop2AutoscaleCheck = new wxCheckBox(this, ID_TopCheckbox, wxEmptyString), 0, wxLEFT|wxRIGHT|wxALIGN_CENTER|wxALIGN_CENTER_VERTICAL,  2);	
		yBoundSizer->Add( mTopY2MinCtrl = new wxNumericCtrl(this), 0, wxLEFT|wxRIGHT, 2);	
		yBoundSizer->Add( mTopY2MaxCtrl = new wxNumericCtrl(this), 0, wxLEFT|wxRIGHT, 2);
	}
	else
	{
		mTopY2MinCtrl = mTopY2MaxCtrl = 0;
		mTop2AutoscaleCheck = 0;
	}
	
	if ( isBottomGraphVisible )
	{
		yBoundSizer->Add( new wxStaticText( this, wxID_ANY, "Bottom left axis:" ), 0, wxLEFT|wxRIGHT|wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL, 3 );
		yBoundSizer->Add( mBottomAutoscaleCheck = new wxCheckBox(this, ID_BottomCheckbox, wxEmptyString), 0, wxLEFT|wxRIGHT|wxALIGN_CENTER|wxALIGN_CENTER_VERTICAL,  2);	
		yBoundSizer->Add( mBottomYMinCtrl = new wxNumericCtrl(this), 0, wxLEFT|wxRIGHT, 2);	
		yBoundSizer->Add( mBottomYMaxCtrl = new wxNumericCtrl(this), 0, wxLEFT|wxRIGHT, 2);
	}
	else
	{
		mBottomYMinCtrl = mBottomYMaxCtrl = 0;
		mBottomAutoscaleCheck = 0;
	}
	
	
	if ( isBottomRightGraphVisible )
	{
		yBoundSizer->Add( new wxStaticText( this, wxID_ANY, "Bottom right axis:" ), 0, wxLEFT|wxRIGHT|wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL, 3 );
		yBoundSizer->Add( mBottom2AutoscaleCheck = new wxCheckBox(this, ID_BottomCheckbox, wxEmptyString), 0, wxLEFT|wxRIGHT|wxALIGN_CENTER|wxALIGN_CENTER_VERTICAL,  2);	
		yBoundSizer->Add( mBottomY2MinCtrl = new wxNumericCtrl(this), 0, wxLEFT|wxRIGHT, 2);	
		yBoundSizer->Add( mBottomY2MaxCtrl = new wxNumericCtrl(this), 0, wxLEFT|wxRIGHT, 2);
	}
	else
	{
		mBottomY2MinCtrl = mBottomY2MaxCtrl = 0;
		mBottom2AutoscaleCheck = 0;
	}
			
	wxBoxSizer *boxmain = new wxBoxSizer(wxVERTICAL);
	boxmain->Add( mSteppedLines, 0, wxALL|wxEXPAND, 10 );
	boxmain->Add( mStackedArea, 0, wxALL|wxEXPAND, 10 );
	boxmain->Add( mStatTypeCheck, 0, wxALL|wxEXPAND, 10 );
	boxmain->Add( new wxStaticLine( this ), 0, wxALL|wxEXPAND, 0 );
	boxmain->Add( yBoundSizer, 1, wxALL|wxEXPAND, 10 );
	boxmain->Add( new wxStaticLine( this ), 0, wxALL|wxEXPAND, 0 );
	boxmain->Add( CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 20 );
		
	SetSizer(boxmain);
	Fit();
}

void wxDVTimeSeriesSettingsDialog::SetTopYBounds( double y1min, double y1max )
{
	mTopYMinCtrl->SetValue( y1min );
	mTopYMaxCtrl->SetValue( y1max );
}


void wxDVTimeSeriesSettingsDialog::SetTopY2Bounds( double y1min, double y1max )
{
	if ( mTopY2MinCtrl ) mTopY2MinCtrl->SetValue( y1min );
	if ( mTopY2MaxCtrl ) mTopY2MaxCtrl->SetValue( y1max );
}

void wxDVTimeSeriesSettingsDialog::SetBottomYBounds( double y2min, double y2max )
{
	if ( mBottomYMinCtrl ) mBottomYMinCtrl->SetValue( y2min );
	if ( mBottomYMaxCtrl ) mBottomYMaxCtrl->SetValue( y2max );
}

void wxDVTimeSeriesSettingsDialog::SetBottomY2Bounds( double y2min, double y2max )
{
	if ( mBottomY2MinCtrl ) mBottomY2MinCtrl->SetValue( y2min );
	if ( mBottomY2MaxCtrl ) mBottomY2MaxCtrl->SetValue( y2max );
}

void wxDVTimeSeriesSettingsDialog::GetTopYBounds( double *y1min, double *y1max )
{
	if ( mTopYMinCtrl ) *y1min = mTopYMinCtrl->Value();
	if ( mTopYMaxCtrl ) *y1max = mTopYMaxCtrl->Value();
}

void wxDVTimeSeriesSettingsDialog::GetTopY2Bounds( double *y1min, double *y1max )
{
	if ( mTopY2MinCtrl ) *y1min = mTopY2MinCtrl->Value();
	if ( mTopY2MaxCtrl ) *y1max = mTopY2MaxCtrl->Value();
}


void wxDVTimeSeriesSettingsDialog::GetBottomYBounds( double *y2min, double *y2max )
{
	if ( mBottomYMinCtrl ) *y2min = mBottomYMinCtrl->Value();
	if ( mBottomYMaxCtrl ) *y2max = mBottomYMaxCtrl->Value();
}

void wxDVTimeSeriesSettingsDialog::GetBottomY2Bounds( double *y2min, double *y2max )
{
	if ( mBottomY2MinCtrl ) *y2min = mBottomY2MinCtrl->Value();
	if ( mBottomY2MaxCtrl ) *y2max = mBottomY2MaxCtrl->Value();
}


void wxDVTimeSeriesSettingsDialog::SetStacked( bool b ) { mStackedArea->SetValue( b ); }
bool wxDVTimeSeriesSettingsDialog::GetStacked() { return mStackedArea->GetValue(); }

void wxDVTimeSeriesSettingsDialog::SetStyle( wxDVTimeSeriesStyle id ) { mSteppedLines->SetValue( id == wxDV_STEPPED ); }
wxDVTimeSeriesStyle wxDVTimeSeriesSettingsDialog::GetStyle() { return mSteppedLines->GetValue() ? wxDV_STEPPED : wxDV_NORMAL; }

void wxDVTimeSeriesSettingsDialog::SetStatType( wxDVStatType statType ) { mStatTypeCheck->SetValue( statType == wxDV_SUM ? true : false ); }
wxDVStatType wxDVTimeSeriesSettingsDialog::GetStatType() { return mStatTypeCheck->GetValue() ? wxDV_SUM : wxDV_AVERAGE; }

void wxDVTimeSeriesSettingsDialog::SetAutoscale( bool b ) 
{ 
	mTopAutoscaleCheck->SetValue( b ); 
	mTopYMaxCtrl->Enable(!b);
	mTopYMinCtrl->Enable(!b);
}

void wxDVTimeSeriesSettingsDialog::SetAutoscale2( bool b ) 
{ 
	if ( mTop2AutoscaleCheck )
	{
		mTop2AutoscaleCheck->SetValue( b ); 
		mTopY2MaxCtrl->Enable(!b);
		mTopY2MinCtrl->Enable(!b);
	}
}

bool wxDVTimeSeriesSettingsDialog::GetAutoscale() { return mTopAutoscaleCheck->GetValue(); }
bool wxDVTimeSeriesSettingsDialog::GetAutoscale2() { return mTop2AutoscaleCheck ? mTop2AutoscaleCheck->GetValue() : false; }

void wxDVTimeSeriesSettingsDialog::SetBottomAutoscale( bool b ) 
{ 
	if ( mBottomAutoscaleCheck ) {
		mBottomAutoscaleCheck->SetValue( b ); 
		mBottomYMaxCtrl->Enable(!b);
		mBottomYMinCtrl->Enable(!b);
	}
}
bool wxDVTimeSeriesSettingsDialog::GetBottomAutoscale() { return mBottomAutoscaleCheck ? mBottomAutoscaleCheck->GetValue() : false; }

void wxDVTimeSeriesSettingsDialog::SetBottomAutoscale2( bool b ) 
{ 
	if ( mBottom2AutoscaleCheck ) {
		mBottom2AutoscaleCheck->SetValue( b ); 
		mBottomY2MaxCtrl->Enable(!b);
		mBottomY2MinCtrl->Enable(!b);
	}
}
bool wxDVTimeSeriesSettingsDialog::GetBottomAutoscale2() { return mBottom2AutoscaleCheck ? mBottom2AutoscaleCheck->GetValue() : false; }


void wxDVTimeSeriesSettingsDialog::OnClickTopHandler(wxCommandEvent& event)
{
	SetAutoscale( mTopAutoscaleCheck->IsChecked() );
	if ( mTop2AutoscaleCheck ) SetAutoscale2( mTop2AutoscaleCheck->IsChecked() );
}

void wxDVTimeSeriesSettingsDialog::OnClickBottomHandler(wxCommandEvent& event)
{
	if ( mBottomAutoscaleCheck ) SetBottomAutoscale( mBottomAutoscaleCheck->IsChecked() );
	if ( mBottom2AutoscaleCheck ) SetBottomAutoscale2( mBottom2AutoscaleCheck->IsChecked() );
}

void wxDVTimeSeriesSettingsDialog::OnClickStatHandler(wxCommandEvent& event)
{
	SetStatType( mStatTypeCheck->IsChecked() ? wxDV_SUM : wxDV_AVERAGE );
}


enum{
		ID_DATA_CHANNEL_SELECTOR = wxID_HIGHEST+1, 
		ID_GRAPH_SCROLLBAR,
		ID_PLOT_SURFACE };

BEGIN_EVENT_TABLE(wxDVTimeSeriesCtrl, wxPanel)
	EVT_BUTTON(wxID_ZOOM_IN, wxDVTimeSeriesCtrl::OnZoomIn)
	EVT_BUTTON(wxID_ZOOM_OUT, wxDVTimeSeriesCtrl::OnZoomOut)
	EVT_BUTTON(wxID_ZOOM_FIT, wxDVTimeSeriesCtrl::OnZoomFit)
	EVT_BUTTON(wxID_PREFERENCES, wxDVTimeSeriesCtrl::OnSettings)

	EVT_MOUSEWHEEL(wxDVTimeSeriesCtrl::OnMouseWheel)

	EVT_PLOT_HIGHLIGHT(ID_PLOT_SURFACE, wxDVTimeSeriesCtrl::OnHighlight)
	
	EVT_DVSELECTIONLIST(ID_DATA_CHANNEL_SELECTOR, wxDVTimeSeriesCtrl::OnDataChannelSelection)

	EVT_COMMAND_SCROLL_THUMBTRACK(ID_GRAPH_SCROLLBAR, wxDVTimeSeriesCtrl::OnGraphScroll)
	EVT_COMMAND_SCROLL_LINEUP(ID_GRAPH_SCROLLBAR, wxDVTimeSeriesCtrl::OnGraphScrollLineUp)
	EVT_COMMAND_SCROLL_LINEDOWN(ID_GRAPH_SCROLLBAR, wxDVTimeSeriesCtrl::OnGraphScrollLineDown)
	//EVT_COMMAND_SCROLL_CHANGED(ID_GRAPH_SCROLLBAR, wxDVTimeSeriesCtrl::OnGraphScroll)
	EVT_COMMAND_SCROLL_PAGEDOWN(ID_GRAPH_SCROLLBAR, wxDVTimeSeriesCtrl::OnGraphScrollPageDown)
	EVT_COMMAND_SCROLL_PAGEUP(ID_GRAPH_SCROLLBAR, wxDVTimeSeriesCtrl::OnGraphScrollPageUp)

END_EVENT_TABLE()


/*Constructors and Destructors*/
wxDVTimeSeriesCtrl::wxDVTimeSeriesCtrl(wxWindow *parent, wxWindowID id, wxDVTimeSeriesType seriesType, wxDVStatType statType)
	: wxPanel(parent, id)
{	
	SetBackgroundColour( *wxWHITE );
	m_stackingOnYLeft = false;
	m_topAutoScale = false;
	m_top2AutoScale = false;
	m_bottomAutoScale = false;
	m_bottom2AutoScale = false;
	m_style = ((seriesType == wxDV_RAW || seriesType == wxDV_HOURLY) ? wxDV_NORMAL : wxDV_STEPPED); // line, stepped, points
	m_seriesType = seriesType;
	m_statType = statType;

	m_plotSurface = new wxPLPlotCtrl(this, ID_PLOT_SURFACE); 
	m_plotSurface->SetBackgroundColour( *wxWHITE );
	m_plotSurface->SetHighlightMode( wxPLPlotCtrl::HIGHLIGHT_SPAN );
	m_plotSurface->ShowTitle( false );
	m_plotSurface->ShowLegend( false );
	//m_plotSurface->SetLegendLocation( wxPLPlotCtrl::RIGHT );
	m_plotSurface->SetIncludeLegendOnExport( true );
	m_plotSurface->ShowGrid( true, true );
	m_xAxis = new wxPLTimeAxis( 0, 8760 );
	m_plotSurface->SetXAxis1( m_xAxis );

	wxBoxSizer *scrollerAndZoomSizer = new wxBoxSizer(wxHORIZONTAL);
	m_graphScrollBar = new wxScrollBar(this, ID_GRAPH_SCROLLBAR, wxDefaultPosition, wxDefaultSize, wxHORIZONTAL);
	scrollerAndZoomSizer->Add(m_graphScrollBar, 1, wxALL|wxALIGN_CENTER_VERTICAL, 3);

	wxBitmapButton *zoom_in =  new wxBitmapButton( this, wxID_ZOOM_IN, wxBITMAP_PNG_FROM_DATA( zoom_in ));
	zoom_in->SetToolTip("Zoom in");
	scrollerAndZoomSizer->Add( zoom_in, 0, wxALL|wxEXPAND, 1);

	wxBitmapButton *zoom_out = new wxBitmapButton( this, wxID_ZOOM_OUT, wxBITMAP_PNG_FROM_DATA( zoom_out ));
	zoom_out->SetToolTip("Zoom out");
	scrollerAndZoomSizer->Add( zoom_out, 0, wxALL|wxEXPAND, 1);

	wxBitmapButton *zoom_fit = new wxBitmapButton( this, wxID_ZOOM_FIT, wxBITMAP_PNG_FROM_DATA( zoom_fit ));
	zoom_fit->SetToolTip("Zoom fit");
	scrollerAndZoomSizer->Add( zoom_fit , 0, wxALL|wxEXPAND, 1);

	wxBitmapButton *pref_btn = new wxBitmapButton( this, wxID_PREFERENCES, wxBITMAP_PNG_FROM_DATA( preferences ));
	pref_btn->SetToolTip("Edit view settings and graph scaling...");
	scrollerAndZoomSizer->Add( pref_btn, 0, wxALL|wxEXPAND, 1);
	
	//Contains boxes to turn lines on or off.
	m_dataSelector = new wxDVSelectionListCtrl(this, ID_DATA_CHANNEL_SELECTOR, 2);
		
	wxBoxSizer *graph_sizer = new wxBoxSizer(wxVERTICAL);	
	graph_sizer->Add( m_plotSurface, 1, wxEXPAND|wxALL, 4);
	graph_sizer->Add( scrollerAndZoomSizer, 0, wxEXPAND|wxALL, 0);

	wxBoxSizer *top_sizer = new wxBoxSizer(wxHORIZONTAL);
	top_sizer->Add( graph_sizer, 1, wxALL|wxEXPAND, 0 );
	top_sizer->Add( m_dataSelector, 0, wxEXPAND, 0);
	SetSizer(top_sizer);

	for (int i=0; i<GRAPH_AXIS_POSITION_COUNT; i++)
		m_selectedChannelIndices.push_back(new std::vector<int>());

	UpdateScrollbarPosition();
}

wxDVTimeSeriesCtrl::~wxDVTimeSeriesCtrl(void)
{
	RemoveAllDataSets();

	for (int i=0; i<GRAPH_AXIS_POSITION_COUNT; i++)
		delete m_selectedChannelIndices[i];
}

void wxDVTimeSeriesCtrl::Invalidate()
{
	m_plotSurface->Invalidate();
	m_plotSurface->Refresh();
}

wxDVTimeSeriesType wxDVTimeSeriesCtrl::GetTimeSeriesType()
{
	return m_seriesType;
}


/*** EVENT HANDLERS ***/

void wxDVTimeSeriesCtrl::OnZoomIn(wxCommandEvent& e)
{
	if ( m_plots.size() == 0)
		return;

	//Make sure we are not already zoomed in too far.
	if ( CanZoomIn() )
		ZoomFactorAndUpdate(2.0);
}

void wxDVTimeSeriesCtrl::OnZoomOut(wxCommandEvent& e)
{
	if ( m_plots.size() == 0)
		return;

	//Make sure we don't zoom out past the data range.
	if ( CanZoomOut() )
		ZoomFactorAndUpdate(0.5);
}
void wxDVTimeSeriesCtrl::OnZoomFit(wxCommandEvent& e)
{
	ZoomToFit();		
}

void wxDVTimeSeriesCtrl::SetStackingOnYLeft( bool b )
{
	m_stackingOnYLeft = b;
	UpdateStacking();
	Invalidate();
}


void wxDVTimeSeriesCtrl::OnSettings( wxCommandEvent &e )
{
	double y1min = 0, y1max = 0, y2min = 0, y2max = 0;
	bool isBottomGraphVisible = false;

	for(int i = 0; i < m_dataSelector->Length(); i++)
	{
		if(m_dataSelector->IsSelected(i, 1))
		{
			isBottomGraphVisible = true;
			break;
		}
	}

	if ( m_plotSurface->GetYAxis1( wxPLPlotCtrl::PLOT_TOP ) != 0 )
		m_plotSurface->GetYAxis1( wxPLPlotCtrl::PLOT_TOP )->GetWorld( &y1min, &y1max );

	double yrmin = 0, yrmax = 0;
	if ( m_plotSurface->GetYAxis2( wxPLPlotCtrl::PLOT_TOP ) != 0 )
		m_plotSurface->GetYAxis2( wxPLPlotCtrl::PLOT_TOP )->GetWorld( &yrmin, &yrmax );

	if ( m_plotSurface->GetYAxis1( wxPLPlotCtrl::PLOT_BOTTOM ) != 0 )
		m_plotSurface->GetYAxis1( wxPLPlotCtrl::PLOT_BOTTOM )->GetWorld( &y2min, &y2max );

	double y2rmin = 0, y2rmax = 0;
	if ( m_plotSurface->GetYAxis2( wxPLPlotCtrl::PLOT_BOTTOM ) != 0 )
		m_plotSurface->GetYAxis2( wxPLPlotCtrl::PLOT_BOTTOM )->GetWorld( &y2rmin, &y2rmax );


	wxDVTimeSeriesSettingsDialog dlg(  this, "View Settings",  
		m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_TOP) != 0,
		isBottomGraphVisible, 
		m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_BOTTOM) != 0);
	dlg.CentreOnParent();
	dlg.SetStatType( m_statType );
	dlg.SetStyle( m_style );
	dlg.SetStacked( m_stackingOnYLeft );
	dlg.SetAutoscale( m_topAutoScale );
	dlg.SetAutoscale2( m_top2AutoScale );
	dlg.SetBottomAutoscale( m_bottomAutoScale );
	dlg.SetBottomAutoscale2( m_bottom2AutoScale );
	dlg.SetTopYBounds( y1min, y1max );
	dlg.SetTopY2Bounds( yrmin, yrmax );
	dlg.SetBottomYBounds( y2min, y2max );
	dlg.SetBottomY2Bounds( y2rmin, y2rmax );
	if (wxID_OK == dlg.ShowModal())
	{
		m_stackingOnYLeft = dlg.GetStacked();
		m_style = dlg.GetStyle();
		for (size_t i=0; i<m_plots.size(); i++)
			m_plots[i]->SetStyle( m_style );

		if(m_statType != dlg.GetStatType())
		{
			m_statType = dlg.GetStatType();
			wxString nonmodifiables;

			for(size_t i = 0; i < m_plots.size(); i++)
			{
				m_plots[i]->SetStyle((wxDVTimeSeriesStyle)m_style);
				m_plots[i]->UpdateSummaryData(m_statType == wxDV_AVERAGE ? true : false);

				if ( 0 == dynamic_cast<wxDVArrayDataSet*>( m_plots[i]->GetDataSet() ) )
					nonmodifiables += m_plots[i]->GetDataSet()->GetSeriesTitle() + "\n";
			}

			if ( nonmodifiables.size() > 0 )
				wxMessageBox("The following plots could not be configured for the modified statistic type:\n\n" + nonmodifiables );
		}
	
		UpdateStacking();

		m_topAutoScale = dlg.GetAutoscale();
		m_top2AutoScale = dlg.GetAutoscale2();
		m_bottomAutoScale = dlg.GetBottomAutoscale();
		m_bottom2AutoScale = dlg.GetBottomAutoscale2();

		if (m_topAutoScale)
		{ 
			SetupTopYLeft();
			AutoscaleYAxis(m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_TOP),
				*m_selectedChannelIndices[TOP_LEFT_AXIS],
				true);
		}
		else {
			double min, max;
			dlg.GetTopYBounds(&min,&max);
			SetupTopYLeft( min, max );
		}
		
		if (m_top2AutoScale) 
		{ 
			SetupTopYRight(0.0, 0.0);
			AutoscaleYAxis(m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_TOP),
				m_selectedChannelIndices[TOP_RIGHT_AXIS]->size() > 0 ? *m_selectedChannelIndices[TOP_RIGHT_AXIS] : *m_selectedChannelIndices[TOP_LEFT_AXIS],
				true);
		}
		else {
			double min, max;
			dlg.GetTopY2Bounds(&min,&max);
			SetupTopYRight( min, max );
		}

		if ( m_bottomAutoScale )
		{
			if (m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_BOTTOM))
				AutoscaleYAxis(m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_BOTTOM), 
					*m_selectedChannelIndices[BOTTOM_LEFT_AXIS], 
					true);
		}
		else
		{
			dlg.GetBottomYBounds( &y2min, &y2max );

			if (y2max > y2min)
			{		
				if (m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_BOTTOM))
					m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_BOTTOM)->SetWorld( y2min, y2max );
			}
		}

		if ( m_bottom2AutoScale )
		{
			if (m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_BOTTOM))
				AutoscaleYAxis(m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_BOTTOM), 
					m_selectedChannelIndices[BOTTOM_RIGHT_AXIS]->size() > 0 ? *m_selectedChannelIndices[BOTTOM_RIGHT_AXIS] : *m_selectedChannelIndices[BOTTOM_LEFT_AXIS], 
					true);	
		}
		else
		{
			dlg.GetBottomY2Bounds( &y2min, &y2max );
			if (y2max > y2min) {
				if (m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_BOTTOM))
					m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_BOTTOM)->SetWorld( y2min, y2max );
			}
		}

		Invalidate();
	}
}

void wxDVTimeSeriesCtrl::OnMouseWheel(wxMouseEvent& e)
{
	if (!m_xAxis || m_plots.size() == 0)
		return;

	bool update_view = false;
	if (e.ShiftDown()) //shift + wheel: scroll
	{
		//Negative rotation will go left.
		PanByPercent(-0.25 * e.GetWheelRotation() / e.GetWheelDelta()); 
		update_view = true;
	}
	else if ( e.ControlDown() ) //wheel only: zoom
	{
		if (e.GetWheelRotation() > 0 && !CanZoomIn()) return;
		//Center zooming on the location of the mouse.
		wxCoord xPos;
		e.GetPosition(&xPos, NULL);

		double min = m_xAxis->GetWorldMin();
		double max = m_xAxis->GetWorldMax();
		wxRect rr = m_plotSurface->GetClientRect(); 
		wxDVPlotHelper::MouseWheelZoom( &min, &max, 
			xPos, 0, rr.width, e.GetWheelRotation() / e.GetWheelDelta()); // TODO: replace 0, rr.width with physical xaxis coordinates

		MakeXBoundsNice(&min, &max);
		m_xAxis->SetWorld( min, max );
		update_view = true;
	}
	else
	{
		m_dataSelector->GetEventHandler()->ProcessEvent( e );
	}


	if (update_view)
	{
		AutoscaleYAxis();
		UpdateScrollbarPosition();
		Invalidate();
	}
}

void wxDVTimeSeriesCtrl::OnHighlight(wxCommandEvent& e)
{
	double left, right;
	m_plotSurface->GetHighlightBounds( &left, &right );

	double leftWorld = 0, rightWorld = 1;
	double wmin, wmax;
	m_xAxis->GetWorld(&wmin, &wmax);

	leftWorld = wmin + left/100.0 * (wmax-wmin);
	rightWorld = wmin + right/100.0 * (wmax-wmin);
	SetViewRange(leftWorld, rightWorld);
}

void wxDVTimeSeriesCtrl::OnGraphScroll(wxScrollEvent &e)
{
	double dataRange = m_xAxis->GetWorldLength();
	double min = e.GetPosition() + GetMinPossibleTimeForVisibleChannels();
	double max = min + dataRange;
	//double tempMax = GetMaxPossibleTimeForVisibleChannels();
	//if (max > tempMax) { max = tempMax; }
	wxDVPlotHelper::SetRangeEndpointsToDays( &min, &max );
	m_xAxis->SetWorld( min, max );
	if (m_topAutoScale || m_top2AutoScale || m_bottomAutoScale || m_bottom2AutoScale) { AutoscaleYAxis(true); }
	Invalidate();
}

//Scrolling the graph a line up or a line down occurs when the user clicks the left or right button on the scrollbar.
void wxDVTimeSeriesCtrl::OnGraphScrollLineUp(wxScrollEvent& e)
{
	PanByPercent(-0.25);
}
void wxDVTimeSeriesCtrl::OnGraphScrollLineDown(wxScrollEvent& e)
{
	PanByPercent(0.25);
}
void wxDVTimeSeriesCtrl::OnGraphScrollPageUp(wxScrollEvent& e)
{
	PanByPercent(-1.0);
}
void wxDVTimeSeriesCtrl::OnGraphScrollPageDown(wxScrollEvent& e)
{
	PanByPercent(1.0);
}
void wxDVTimeSeriesCtrl::OnDataChannelSelection(wxCommandEvent& e)
{
	int row, col;
	bool isChecked;

	m_dataSelector->GetLastEventInfo(&row, &col, &isChecked);

	if (isChecked)
		AddGraphAfterChannelSelection(wxPLPlotCtrl::PlotPos(col), row);
	else
		RemoveGraphAfterChannelSelection(wxPLPlotCtrl::PlotPos(col), row);
}

void wxDVTimeSeriesCtrl::AddDataSet(wxDVTimeSeriesDataSet *d, bool refresh_ui)
{
	wxDVTimeSeriesPlot *p = 0;

	//For daily and monthly time series create a dataset with the average x and y values for the day/month
	//For stepped graphs we have to start the array with the average y value for the first period at x = 0, have each avg y value at the beginning of the period (leftmost x for the period),
	//and end it with the average y value for the final period duplicated at x = max
	double sum = 0.0;
	double avg = 0.0;
	double counter = 0.0;
	double timestep = d->GetTimeStep();
	double MinHrs = d->GetMinHours();
	double MaxHrs = d->GetMaxHours();
	wxDVArrayDataSet *d2 = 0;
	bool IsDataSetEmpty = true;

	if (m_seriesType == wxDV_RAW)
	{
		IsDataSetEmpty = false;
	}
	else if (m_seriesType == wxDV_HOURLY && timestep < 1.0)
	{
		//Create hourly data set (avg value of data by day) from m_data if timestep < 1
		IsDataSetEmpty = false;
		double nextHour = 0.0;
		double currentHour = 0.0;

		d2 = new wxDVArrayDataSet(d->GetSeriesTitle(), d->GetUnits(), 1.0 / timestep);
		d2->SetGroupName( d->GetGroupName() );

		while (nextHour <= MinHrs)
		{
			nextHour += 1.0;
		}

		currentHour = nextHour - 1.0;

		for (size_t i = 0; i < d->Length(); i++)
		{
			if (d->At(i).x >= nextHour)
			{
				if (i != 0 && counter != 0)
				{
					avg = sum / counter;
					d2->Append(wxRealPoint((double)currentHour + (double)(nextHour - currentHour) / 2.0, (m_statType == wxDV_AVERAGE ? avg : sum)));
					currentHour = nextHour;
					nextHour += 1;
				}

				counter = 0.0;
				sum = 0.0;
			}

			counter += 1.0;
			sum += d->At(i).y;
		}

		avg = sum / counter;

		//Prevent appending the final point if it represents 12/31 24:00, which the system interprets as 1/1 0:00 and creates a point for January of the next year
		if (MaxHrs > 0.0 && fmod(MaxHrs, 8760.0) != 0)
		{
			d2->Append(wxRealPoint((double)currentHour + (double)(nextHour - currentHour) / 2.0, (m_statType == wxDV_AVERAGE ? avg : sum)));
		}
	}
	else if (m_seriesType == wxDV_DAILY && timestep < 24.0)
	{
		//Create daily data set (avg value of data by day) from m_data if timestep < 24
		IsDataSetEmpty = false;
		double nextDay = 0.0;
		double currentDay = 0.0;

		d2 = new wxDVArrayDataSet(d->GetSeriesTitle(), d->GetUnits(), 24.0 / timestep);
		d2->SetGroupName( d->GetGroupName() );

		while (nextDay <= MinHrs)
		{
			nextDay += 24.0;
		}

		currentDay = nextDay - 24.0;

		for (size_t i = 0; i < d->Length(); i++)
		{
			if(d->At(i).x >= nextDay)
			{
				if(i != 0 && counter != 0)
				{ 
					avg = sum / counter;
					d2->Append(wxRealPoint((double) currentDay + (double)(nextDay - currentDay) / 2.0, (m_statType == wxDV_AVERAGE ? avg : sum))); 
					currentDay = nextDay;
					nextDay += 24.0;
				}

				counter = 0.0;
				sum = 0.0;
			}

			counter += 1.0;
			sum += d->At(i).y;
		}

		avg = sum / counter;

		//Prevent appending the final point if it represents 12/31 24:00, which the system interprets as 1/1 0:00 and creates a point for January of the next year
		if(MaxHrs > 0.0 && fmod(MaxHrs, 8760.0) != 0)
		{
			d2->Append(wxRealPoint((double) currentDay + (double)(nextDay - currentDay) / 2.0, (m_statType == wxDV_AVERAGE ? avg : sum))); 
		}
	}
	else if (m_seriesType == wxDV_MONTHLY && timestep < 672.0)	//672 hours = 28 days = shortest possible month
	{
		//Create monthly data set (avg value of data by month) from m_data
		IsDataSetEmpty = false;
		double nextMonth = 0.0;
		double currentMonth = 0.0;
		double year = 0.0;

		d2 = new wxDVArrayDataSet(d->GetSeriesTitle(), d->GetUnits(), 744.0 / timestep);
		d2->SetGroupName( d->GetGroupName() );

		while(MinHrs > 8760.0)
		{
			year += 8760.0;
			MinHrs -= 8760.0;
		}

		if(MinHrs >= 0.0 && MinHrs < 744.0) { currentMonth = year; nextMonth = year + 744.0; }
		else if(MinHrs >= 744.0 && MinHrs < 1416.0) { currentMonth = year + 744.0; nextMonth = year + 1416.0; }
		else if(MinHrs >= 1416.0 && MinHrs < 2160.0) { currentMonth = year + 1416.0; nextMonth = year + 2160.0; }
		else if(MinHrs >= 2160.0 && MinHrs < 2880.0) { currentMonth = year + 2160.0; nextMonth = year + 2880.0; }
		else if(MinHrs >= 2880.0 && MinHrs < 3624.0) { currentMonth = year + 2880.0; nextMonth = year + 3624.0; }
		else if(MinHrs >= 3624.0 && MinHrs < 4344.0) { currentMonth = year + 3624.0; nextMonth = year + 4344.0; }
		else if(MinHrs >= 4344.0 && MinHrs < 5088.0) { currentMonth = year + 4344.0; nextMonth = year + 5088.0; }
		else if(MinHrs >= 5088.0 && MinHrs < 5832.0) { currentMonth = year + 5088.0; nextMonth = year + 5832.0; }
		else if(MinHrs >= 5832.0 && MinHrs < 6552.0) { currentMonth = year + 5832.0; nextMonth = year + 6552.0; }
		else if(MinHrs >= 6552.0 && MinHrs < 7296.0) { currentMonth = year + 6552.0; nextMonth = year + 7296.0; }
		else if(MinHrs >= 7296.0 && MinHrs < 8016.0) { currentMonth = year + 7296.0; nextMonth = year + 8016.0; }
		else if(MinHrs >= 8016.0 && MinHrs < 8760.0) { currentMonth = year + 8016.0; nextMonth = year + 8760.0; }

		for (size_t i = 0; i < d->Length(); i++)
		{
			if(d->At(i).x >= nextMonth)
			{
				if(i != 0 && counter != 0)
				{ 
					avg = sum / counter;

					d2->Append(wxRealPoint((double) currentMonth + (double)(nextMonth - currentMonth) / 2.0, (m_statType == wxDV_AVERAGE ? avg : sum))); 

					currentMonth = nextMonth;
					if(nextMonth == 744.0 + year) { nextMonth = 1416.0 + year; }
					else if(nextMonth == 1416.0 + year) { nextMonth = 2160.0 + year; }
					else if(nextMonth == 2160.0 + year) { nextMonth = 2880.0 + year; }
					else if(nextMonth == 2880.0 + year) { nextMonth = 3624.0 + year; }
					else if(nextMonth == 3624.0 + year) { nextMonth = 4344.0 + year; }
					else if(nextMonth == 4344.0 + year) { nextMonth = 5088.0 + year; }
					else if(nextMonth == 5088.0 + year) { nextMonth = 5832.0 + year; }
					else if(nextMonth == 5832.0 + year) { nextMonth = 6552.0 + year; }
					else if(nextMonth == 6552.0 + year) { nextMonth = 7296.0 + year; }
					else if(nextMonth == 7296.0 + year) { nextMonth = 8016.0 + year; }
					else if(nextMonth == 8016.0 + year) { nextMonth = 8760.0 + year; }
					else if(nextMonth == 8760.0 + year) 
					{ 
						year += 8760.0;
						nextMonth = 744.0 + year; 
					}
				}

				counter = 0.0;
				sum = 0.0;
			}

			counter += 1.0;
			sum += d->At(i).y;
		}

		avg = sum / counter;

		//Prevent appending the final point if it represents 12/31 24:00, which the system interprets as 1/1 0:00 and creates a point for January of the next year
		if(MaxHrs > 0.0 && fmod(MaxHrs, 8760.0) != 0)
		{
			d2->Append(wxRealPoint((double) currentMonth + (double)(nextMonth - currentMonth) / 2.0, (m_statType == wxDV_AVERAGE ? avg : sum))); 
		}
	}
	
	if (!IsDataSetEmpty)
	{
		if(m_seriesType == wxDV_RAW)
		{
			p = new wxDVTimeSeriesPlot(d, m_seriesType);
		}
		else
		{
			p = new wxDVTimeSeriesPlot(d2, m_seriesType, true);
		}

		p->SetStyle( m_style );
		m_plots.push_back(p); //Add to data sets list.
		m_dataSelector->Append( d->GetTitleWithUnits(), d->GetGroupName() );

		if ( refresh_ui )
		{
			Layout();
			RefreshDisabledCheckBoxes();
		}
	}
}

bool wxDVTimeSeriesCtrl::RemoveDataSet(wxDVTimeSeriesDataSet *d)
{
	wxDVTimeSeriesPlot *plotToRemove = NULL;
	int removedIndex = 0;
	//Find the plottable:
	for (size_t i=0; i<m_plots.size(); i++)
	{
		if (m_plots[i]->GetDataSet() == d)
		{
			removedIndex = i;
			plotToRemove = m_plots[i];
			break;
		}
	}

	if (!plotToRemove)
		return false;

	m_dataSelector->RemoveAt(removedIndex);

	for (int i=0; i<wxPLPlotCtrl::NPLOTPOS; i++)
		m_plotSurface->RemovePlot(plotToRemove);

	m_plots.erase( m_plots.begin() + removedIndex); //This is more efficient than remove when we already know the index.
	
	// make sure an erased plot is nolonger referenced in stacking
	UpdateStacking();

	Invalidate();

	//We base our logic for showing/hiding plots, etc on indices, so when a single data set is removed
	//we have to re-index everything.
	for (size_t i=0; i<m_selectedChannelIndices.size(); i++)
	{
		for(size_t k=0; k<m_selectedChannelIndices[i]->size(); k++)
		{
			if ((*m_selectedChannelIndices[i])[k] > removedIndex)
				(*m_selectedChannelIndices[i])[k]--;
		}
	}

	m_dataSelector->Layout(); //We removed some check boxes.
	RefreshDisabledCheckBoxes();
	return true;
}

void wxDVTimeSeriesCtrl::RemoveAllDataSets( )
{
	ClearAllChannelSelections( wxPLPlotCtrl::PLOT_BOTTOM );
	ClearAllChannelSelections( wxPLPlotCtrl::PLOT_TOP );

	/*
	//Remove plottables from plot surfaces
	for (size_t i=0; i<m_selectedChannelIndices.size(); i++)
	{
		for (size_t k=0; k<m_selectedChannelIndices[i]->size(); k++)
			m_plotSurface->RemovePlot(m_plots[(*m_selectedChannelIndices[i])[k]] );

		m_selectedChannelIndices[i]->clear();
	}
	
	Invalidate();
	*/
	
	m_dataSelector->RemoveAll();

	//Remove all data sets. Deleting a data set also deletes its plottable.
	for(size_t i=0; i<m_plots.size(); i++)
		delete m_plots[i];

	m_plots.clear();
}


/*** VIEW METHODS ***/

double wxDVTimeSeriesCtrl::GetViewMin()
{
	return m_xAxis->GetWorldMin();
}

double wxDVTimeSeriesCtrl::GetViewMax()
{
	return m_xAxis->GetWorldMax();
}

void wxDVTimeSeriesCtrl::SetViewMin(double min)
{
	m_xAxis->SetWorldMin( min );
	AutoscaleYAxis();
	UpdateScrollbarPosition();
	Invalidate();
}

void wxDVTimeSeriesCtrl::SetViewMax(double max)
{
	m_xAxis->SetWorldMax( max );
	AutoscaleYAxis();
	UpdateScrollbarPosition();
	Invalidate();
}

//This setter also sets endpoints to days.
void wxDVTimeSeriesCtrl::SetViewRange(double min, double max)
{
	wxDVPlotHelper::SetRangeEndpointsToDays(&min, &max);
	m_xAxis->SetWorld(min, max);
	if (m_topAutoScale || m_top2AutoScale || m_bottomAutoScale || m_bottom2AutoScale) { AutoscaleYAxis(true); }
	UpdateScrollbarPosition();
	Invalidate();
}

void wxDVTimeSeriesCtrl::SetStyle( wxDVTimeSeriesStyle sty )
{
	m_style = sty;	
	for (size_t i=0; i<m_plots.size(); i++)
		m_plots[i]->SetStyle( m_style );
	UpdateStacking();
	Invalidate();
}

void wxDVTimeSeriesCtrl::GetVisibleDataMinAndMax(double* min, double* max, const std::vector<int>& selectedChannelIndices)
{
	*min = 1000000000;
	*max = 0; 

	bool has_stacking = false;
	
	for( size_t i=0;i<selectedChannelIndices.size();i++ )
		if ( m_plots[selectedChannelIndices[i]]->GetStackingMode() )
			has_stacking = true;

	if ( has_stacking )
	{		
		double worldMin = m_xAxis->GetWorldMin();
		double worldMax = m_xAxis->GetWorldMax();

		for(size_t i=0; i<selectedChannelIndices.size(); i++)
		{
			wxDVTimeSeriesPlot *plot = m_plots[selectedChannelIndices[i]];
			for( size_t j=0;j<plot->Len();j++ )
			{
				wxRealPoint p( plot->StackedAt(j) );
				if ( p.x < worldMin || p.x > worldMax )
					continue;
				if ( p.y > *max )
					*max = p.y;
				if ( p.y < *min )
					*min = p.y;
			}
		}

		if ( *min > 0 ) *min = 0;
		if ( *max < 0 ) *max = 0;
	}
	else
	{
		double tempMin = 0;
		double tempMax = 0;
		if (m_xAxis)
		{
			double worldMin = m_xAxis->GetWorldMin();
			double worldMax = m_xAxis->GetWorldMax();

			for(size_t i=0; i<selectedChannelIndices.size(); i++)
			{
				m_plots[selectedChannelIndices[i]]->GetDataSet()->GetMinAndMaxInRange(&tempMin, &tempMax, worldMin, worldMax);
				if (tempMin < *min)
					*min = tempMin;
				if (tempMax > *max)
					*max = tempMax;		
			}
		}
		else
		{
			for(size_t i=0; i<selectedChannelIndices.size(); i++)
			{
				m_plots[selectedChannelIndices[i]]->GetDataSet()->GetDataMinAndMax(&tempMin, &tempMax);
				if (tempMin < *min)
					*min = tempMin;
				if (tempMax > *max)
					*max = tempMax;		
			}
		}
	}
}

void wxDVTimeSeriesCtrl::GetAllDataMinAndMax(double* min, double* max, const std::vector<int>& selectedChannelIndices)
{
	*min = 1000000000;
	*max = 0;

	bool has_stacking = false;

	for (size_t i = 0; i<selectedChannelIndices.size(); i++)
		if (m_plots[selectedChannelIndices[i]]->GetStackingMode())
			has_stacking = true;

	if (has_stacking)
	{
		for (size_t i = 0; i<selectedChannelIndices.size(); i++)
		{
			wxDVTimeSeriesPlot *plot = m_plots[selectedChannelIndices[i]];
			for (size_t j = 0; j<plot->Len(); j++)
			{
				wxRealPoint p(plot->StackedAt(j));
				if (p.y > *max)
					*max = p.y;
				if (p.y < *min)
					*min = p.y;
			}
		}

		if (*min > 0) *min = 0;
		if (*max < 0) *max = 0;
	}
	else
	{
		double tempMin = 0;
		double tempMax = 0;
		for (size_t i = 0; i<selectedChannelIndices.size(); i++)
		{
			m_plots[selectedChannelIndices[i]]->GetDataSet()->GetDataMinAndMax(&tempMin, &tempMax);
			if (tempMin < *min)
				*min = tempMin;
			if (tempMax > *max)
				*max = tempMax;
		}
	}
}

double wxDVTimeSeriesCtrl::GetMinPossibleTimeForVisibleChannels()
{
	double min = 1000000000;
	for(size_t i=0; i<m_plots.size(); i++)
	{
		if (m_dataSelector->IsRowSelected(i))
		{
			if (m_plots[i]->GetDataSet()->GetMinHours() < min)
				min = m_plots[i]->GetDataSet()->GetMinHours();
		}
	}

	return min;
}

double wxDVTimeSeriesCtrl::GetMaxPossibleTimeForVisibleChannels()
{
	double max = 0;
	for(size_t i=0; i<m_plots.size(); i++)
	{
		if (m_dataSelector->IsRowSelected(i))
		{
			if (m_plots[i]->GetDataSet()->GetMaxHours() > max)
				max = m_plots[i]->GetDataSet()->GetMaxHours();
		}
	}

	return max;
}

void wxDVTimeSeriesCtrl::MakeXBoundsNice(double* xMin, double* xMax)
{
	KeepNewBoundsWithinLimits(xMin, xMax);
	KeepNewBoundsWithinLowerLimit(xMin, xMax);
	wxDVPlotHelper::SetRangeEndpointsToDays(xMin, xMax);
}

void wxDVTimeSeriesCtrl::KeepNewBoundsWithinLimits(double* newMin, double* newMax)
{
	//Make sure we don't zoom out past the data range.
	//This can only happen zooming out.
	int visDataMin = m_plots[0]->GetDataSet()->GetMinHours();
	int visDataMax = m_plots[0]->GetDataSet()->GetMaxHours();
	for (size_t i=1; i<m_plots.size(); i++)
	{
		if (m_dataSelector->IsRowSelected(i))
		{
			if (visDataMin > m_plots[i]->GetDataSet()->GetMinHours())
				visDataMin = m_plots[i]->GetDataSet()->GetMinHours();
			if (visDataMax < m_plots[i]->GetDataSet()->GetMaxHours())
				visDataMax = m_plots[i]->GetDataSet()->GetMaxHours();
		}
	}

	if (*newMin < visDataMin)
	{
		*newMax += visDataMin - *newMin;
		*newMin = visDataMin;
	}

	if (*newMax > visDataMax)
	{
		*newMin -= *newMax - visDataMax;
		if (*newMin < visDataMin)
			*newMin = visDataMin;
		*newMax = visDataMax;
	}
		
}

void wxDVTimeSeriesCtrl::KeepNewBoundsWithinLowerLimit(double* newMin, double* newMax)
{
	//Don't zoom in too far.  
	double lowerLim = 12;

	if (*newMax - *newMin > lowerLim)
		return;

	int visDataMin = m_plots[0]->GetDataSet()->GetMinHours();
	int visDataMax = m_plots[0]->GetDataSet()->GetMaxHours();
	for (size_t i=1; i<m_plots.size(); i++)
	{
		if (m_dataSelector->IsRowSelected(i))
		{
			if (visDataMin > m_plots[i]->GetDataSet()->GetMinHours())
				visDataMin = m_plots[i]->GetDataSet()->GetMinHours();
			if (visDataMax < m_plots[i]->GetDataSet()->GetMaxHours())
				visDataMax = m_plots[i]->GetDataSet()->GetMaxHours();
		}
	}

	//If we make it here, we need to extend our bounds.
	if (visDataMax - *newMax > lowerLim - (*newMax - *newMin))
	{
		//Extend max and we are done.
		*newMax = *newMin + lowerLim;
		return;
	}
	else
	{
		//Extend as far as we can, and we aren't done.
		*newMax = visDataMax;
	}
	if (*newMin - visDataMin > lowerLim - (*newMax - *newMin))
	{
		*newMin = *newMax - lowerLim;
		return;
	}
	else
	{
		//Worst case.  Not extended to lower lim, but extended to data range.
		*newMin = visDataMin;
	}
}


/*** GRAPH RELATED METHODS ***/

bool wxDVTimeSeriesCtrl::CanZoomIn()
{
	if (!m_xAxis) return false;
	//Don't zoom in past 12 hours.
	double min, max;
	m_xAxis->GetWorld(&min,&max);
	return (max-min) > 12;
}

bool wxDVTimeSeriesCtrl::CanZoomOut()
{
	double min, max;
	m_xAxis->GetWorld(&min,&max);
	//Don't zoom out past data range.
	for (size_t i=0; i<m_selectedChannelIndices.size(); i++)
	{
		for (size_t k=0; k<m_selectedChannelIndices[i]->size(); k++)
		{
			if ( max-min < m_plots[(*m_selectedChannelIndices[i])[k]]->GetDataSet()->GetTotalHours())
				return true;
		}
	}

	return false;
}

void wxDVTimeSeriesCtrl::ZoomFactorAndUpdate(double factor, double shiftPercent)
{
	double min = m_xAxis->GetWorldMin();
	double max = m_xAxis->GetWorldMax();
	wxDVPlotHelper::ZoomFactor(&min, &max, factor, shiftPercent);
	MakeXBoundsNice(&min, &max);
	m_xAxis->SetWorld(min, max);
	if (m_topAutoScale || m_top2AutoScale || m_bottomAutoScale || m_bottom2AutoScale) { AutoscaleYAxis(true); }
	UpdateScrollbarPosition();
	Invalidate();
}

void wxDVTimeSeriesCtrl::ZoomToFit()
{
	double min = GetMinPossibleTimeForVisibleChannels();
	double max = GetMaxPossibleTimeForVisibleChannels();
	if ( max <= min || (min==0&&max==0) )
	{
		min = 0;
		max = 8760;
	}

	SetViewRange(min, max);
}

void wxDVTimeSeriesCtrl::PanByPercent(double p)
{
	//If p is 0.5, for example, the center becomes the new left bound.

	if (!m_xAxis) return;

	double min, max;
	m_xAxis->GetWorld(&min,&max);

	double newMin = min + (max-min) * p;
	double newMax = max + (max-min) * p;

	double highestTimeValue = GetMaxPossibleTimeForVisibleChannels();
	if (newMax > highestTimeValue)
	{
		if (max > highestTimeValue)
		{
			newMin -= newMax - max;
		}
		else
		{
			newMin -= newMax - highestTimeValue;
		}
		newMax = highestTimeValue;
	}

	double timeMin = GetMinPossibleTimeForVisibleChannels();
	if (newMin < timeMin)
	{
		if (min < timeMin)
		{
			newMax += min - newMin;
		}
		else
		{
			newMax += timeMin - newMin;
		}
		newMin = timeMin;
	}

	SetViewRange(newMin, newMax);
	if (m_topAutoScale || m_top2AutoScale || m_bottomAutoScale || m_bottom2AutoScale) { AutoscaleYAxis(true); }
	Invalidate();
}

void wxDVTimeSeriesCtrl::UpdateScrollbarPosition()
{
	if(m_plots.size() == 0)
	{
		//Don't Call m_plotSurface1->GetXAxis1()->WorldLength() because we don't have a graph yet.
		m_graphScrollBar->SetScrollbar(0, 1, 1, 1, true);
		return;
	}

	else
	{
		//All units are hours.
		int thumbSize = m_xAxis->GetWorldLength();
		int pageSize = thumbSize;
		int range = 0;

		double timeMin = GetMinPossibleTimeForVisibleChannels();
		range = GetMaxPossibleTimeForVisibleChannels() - timeMin;
		int position = m_xAxis->GetWorldMin() - timeMin;
		
		m_graphScrollBar->SetScrollbar(position, thumbSize, range, pageSize, true);
	}
}

void wxDVTimeSeriesCtrl::SetupTopYLeft( double min, double max )
{
	wxPLAxis *axis = m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_TOP);
	if ( !axis ) return;
	m_topAutoScale = ( !wxIsNaN(min) && !wxIsNaN(max) &&  min >= max );
	if ( m_topAutoScale )
		AutoscaleYAxis( axis, *m_selectedChannelIndices[TOP_LEFT_AXIS], true);
	else if ( max > min )
	{
		axis->SetWorld( min, max );
		m_plotSurface->Invalidate();
	}

}

void wxDVTimeSeriesCtrl::SetupTopYRight( double min, double max )
{
	m_top2AutoScale = false;

	wxPLAxis *axis = m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_TOP);
	if ( !axis ) return;	
	
	m_top2AutoScale = ( !wxIsNaN(min) && !wxIsNaN(max) &&  min >= max );
	if ( m_top2AutoScale )
		AutoscaleYAxis( axis, *m_selectedChannelIndices[TOP_RIGHT_AXIS], true);
	else if ( max > min )
	{
		axis->SetWorld( min, max );
		m_plotSurface->Invalidate();
	}
}

void wxDVTimeSeriesCtrl::AutoscaleYAxis(bool forceUpdate, bool ScaleOverAllData)
{
	//It is probably best to avoid this function and use the more specific version where possible.
	if (m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_TOP))
		AutoscaleYAxis(m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_TOP), *m_selectedChannelIndices[TOP_LEFT_AXIS], forceUpdate, ScaleOverAllData);

	if (wxPLAxis *axis = m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_TOP))
		AutoscaleYAxis(axis, m_selectedChannelIndices[TOP_RIGHT_AXIS]->size() > 0 ? *m_selectedChannelIndices[TOP_RIGHT_AXIS] : *m_selectedChannelIndices[TOP_LEFT_AXIS], 
				forceUpdate, ScaleOverAllData);
	

	if (m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_BOTTOM))
		AutoscaleYAxis(m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_BOTTOM), *m_selectedChannelIndices[BOTTOM_LEFT_AXIS], forceUpdate, ScaleOverAllData);
	
	if (m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_BOTTOM))
		AutoscaleYAxis(m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_BOTTOM), 
			m_selectedChannelIndices[BOTTOM_RIGHT_AXIS]->size() > 0 ? *m_selectedChannelIndices[BOTTOM_RIGHT_AXIS] : *m_selectedChannelIndices[BOTTOM_LEFT_AXIS], 
			forceUpdate, ScaleOverAllData);
}

void wxDVTimeSeriesCtrl::AutoscaleYAxis( wxPLAxis *axisToScale, const std::vector<int>& selectedChannelIndices, bool forceUpdate, bool ScaleOverAllData)
{
	//If autoscaling is off don't scale y1 axis
	// But do scale y2 axis (since we don't allow manual scaling there for UI simplicity).
	if (!m_topAutoScale && axisToScale == m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_TOP)) { return; }
	if (!m_top2AutoScale && axisToScale == m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_TOP)) { return; }
	if (!m_bottomAutoScale && axisToScale == m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_BOTTOM)) { return; }
	if (!m_bottom2AutoScale && axisToScale == m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_BOTTOM)) { return; }

	//Force Update is used to rescale even if data is still in acceptable range
	bool needsRescale = false;
	double dataMax; 
	double dataMin; 
	double timestep = 1.0;

	if (ScaleOverAllData)
	{
		GetAllDataMinAndMax(&dataMin, &dataMax, selectedChannelIndices);
	}
	else
	{
		GetVisibleDataMinAndMax(&dataMin, &dataMax, selectedChannelIndices);
	}

	//If the maximum of the visible data is outside the acceptable range
	if(forceUpdate || (dataMax > 0 && (dataMax >= axisToScale->GetWorldMax() || dataMax < axisToScale->GetWorldMax()/2.0)))
		needsRescale = true;

	if(forceUpdate || (dataMin < 0 && (dataMin <= axisToScale->GetWorldMin() || dataMin > axisToScale->GetWorldMin()/2.0)))
		needsRescale = true;
	
	if (needsRescale)
	{
		wxPLAxis::ExtendBoundsToNiceNumber(&dataMax, &dataMin);
		axisToScale->SetWorld(dataMin, dataMax);
	}
}

void wxDVTimeSeriesCtrl::AddGraphAfterChannelSelection(wxPLPlotCtrl::PlotPos pPos, int index)
{
	if (index < 0 || index >= (int)m_plots.size()) return;

	size_t idx = (size_t)index;
	wxString YLabelText;

	//Set our line colour correctly.  Assigned by data selection window.
	m_plots[idx]->SetColour(m_dataSelector->GetColourForIndex(index) );
	
	wxPLPlotCtrl::AxisPos yap = wxPLPlotCtrl::Y_LEFT;
	wxString units = m_plots[idx]->GetDataSet()->GetUnits();
		
	wxString y1Units = NO_UNITS, y2Units = NO_UNITS;

	if (m_plotSurface->GetYAxis1(pPos))
	{
		y1Units = m_plotSurface->GetYAxis1(pPos)->GetUnits();
	}

	if (m_plotSurface->GetYAxis2(pPos))
	{
		y2Units = m_plotSurface->GetYAxis2(pPos)->GetUnits();
	}

	if (m_plotSurface->GetYAxis1(pPos) && y1Units == units)
	{
		yap = wxPLPlotCtrl::Y_LEFT;
	}
	else if (m_plotSurface->GetYAxis2(pPos) && y2Units == units)
	{
		yap = wxPLPlotCtrl::Y_RIGHT;
	}
	else if (m_plotSurface->GetYAxis1(pPos) == 0)
	{
		yap = wxPLPlotCtrl::Y_LEFT;
	}
	else
	{
		yap = wxPLPlotCtrl::Y_RIGHT;
	}
	
	m_plotSurface->AddPlot( m_plots[index], wxPLPlotCtrl::X_BOTTOM, yap, pPos, false );
	m_plotSurface->GetAxis(yap, pPos)->SetUnits(units);
		
	UpdateStacking();

	//Calculate index from 0-3.  0,1 are top graph L,R axis.  2,3 are L,R axis on bottom graph.
	int graphIndex = TOP_LEFT_AXIS;
	if (pPos == wxPLPlotCtrl::PLOT_BOTTOM)
	{
		graphIndex += 2;
	}
	if (yap == wxPLPlotCtrl::Y_RIGHT)
	{
		graphIndex += 1;
	}

	m_selectedChannelIndices[graphIndex]->push_back(idx);

	YLabelText = units;
	if ((pPos == wxPLPlotCtrl::PLOT_TOP && m_selectedChannelIndices[0]->size() == 1 && yap == wxPLPlotCtrl::Y_LEFT)
		|| (pPos == wxPLPlotCtrl::PLOT_TOP && m_selectedChannelIndices[1]->size() == 1 && yap == wxPLPlotCtrl::Y_RIGHT)
		|| (pPos == wxPLPlotCtrl::PLOT_BOTTOM && m_selectedChannelIndices[2]->size() == 1 && yap == wxPLPlotCtrl::Y_LEFT)
		|| (pPos == wxPLPlotCtrl::PLOT_BOTTOM && m_selectedChannelIndices[3]->size() == 1 && yap == wxPLPlotCtrl::Y_RIGHT)
		)
	{ 
		YLabelText = m_plots[idx]->GetDataSet()->GetLabel(); 
	}
	m_plotSurface->GetAxis(yap, pPos)->SetLabel(YLabelText);

	AutoscaleYAxisByPlot(yap == wxPLPlotCtrl::Y_LEFT, pPos == wxPLPlotCtrl::PLOT_TOP, graphIndex);

	UpdateScrollbarPosition();
	RefreshDisabledCheckBoxes();
	Invalidate();
}

void wxDVTimeSeriesCtrl::RemoveGraphAfterChannelSelection(wxPLPlotCtrl::PlotPos pPos, int index)
{
	//Find our GraphAxisPosition, and remove from selected indices list.
	//Have to do it this way because of ambiguous use of int in an array storing ints.
	int graphIndex = 0;
	wxString YLabelText;
	wxString y1Units = NO_UNITS, y2Units = NO_UNITS;
	wxPLPlotCtrl::AxisPos yap = wxPLPlotCtrl::Y_LEFT;

	if (pPos == wxPLPlotCtrl::PLOT_BOTTOM)
		graphIndex += 2;
	for (int i = graphIndex; i < graphIndex+2; i++)
	{
		std::vector<int>::iterator it = std::find( m_selectedChannelIndices[i]->begin(), m_selectedChannelIndices[i]->end(), index );
		if ( it != m_selectedChannelIndices[i]->end())
		{
			m_selectedChannelIndices[i]->erase( it );
			graphIndex = i;
			break;
		}
	}

	bool keepAxis = false;
	if (m_selectedChannelIndices[graphIndex]->size() > 0)
		keepAxis = true;
	
	wxPLPlotCtrl::AxisPos ryap;
	wxPLPlotCtrl::PlotPos rppos;
	m_plotSurface->GetPlotPosition( m_plots[index], 0, &ryap, &rppos );

	m_plotSurface->RemovePlot(m_plots[index], pPos);

	// reset the stacking info for this plot upon removal
	m_plots[index]->SetStackingMode( false );
	m_plots[index]->StackOnTopOf( 0 );

	// if there was another plot stacked on top of this one, reset that plot's stacking
	// pointer to the previous plot in the list that is on the same axis
	UpdateStacking();

	//See if axis is still in use or not, and to some cleanup.
	wxPLLinearAxis *axisThatWasUsed = 0;
	if (graphIndex % 2 == 0)
		axisThatWasUsed = dynamic_cast<wxPLLinearAxis*>( m_plotSurface->GetYAxis1(pPos) );
	else
		axisThatWasUsed = dynamic_cast<wxPLLinearAxis*>( m_plotSurface->GetYAxis2(pPos) );


	if (!keepAxis)
	{
		int otherAxisIndex = TOP_LEFT_AXIS;
		switch (graphIndex)
		{
		case TOP_LEFT_AXIS:
			otherAxisIndex = TOP_RIGHT_AXIS;
			break;
		case TOP_RIGHT_AXIS:
			otherAxisIndex = TOP_LEFT_AXIS;
			break;
		case BOTTOM_LEFT_AXIS:
			otherAxisIndex = BOTTOM_RIGHT_AXIS;
			break;
		case BOTTOM_RIGHT_AXIS:
			otherAxisIndex = BOTTOM_LEFT_AXIS;
			break;
		}

		if (m_selectedChannelIndices[otherAxisIndex]->size() == 0)
		{
			m_plotSurface->SetYAxis1(NULL, pPos);
			m_plotSurface->SetYAxis2(NULL, pPos);
		}
		else
		{
			//If we only have one Y axis, we must use the left y axis.
			//Code in wpplotsurface2D uses this assumption.
			if (axisThatWasUsed == m_plotSurface->GetYAxis1(pPos))
			{
				std::vector<int> *temp = m_selectedChannelIndices[graphIndex];
				m_selectedChannelIndices[graphIndex] = m_selectedChannelIndices[otherAxisIndex];
				m_selectedChannelIndices[otherAxisIndex] = temp;

				//Set the y axis to the left side (instead of the right), making sure that we preserve any zoom factor
				double wmin, wmax;
				m_xAxis->GetWorld(&wmin, &wmax);
				for (size_t i=0; i<m_selectedChannelIndices[graphIndex]->size(); i++)
				{
					m_plotSurface->RemovePlot(m_plots[(*m_selectedChannelIndices[graphIndex])[i]], pPos);
					m_plotSurface->AddPlot( m_plots[(*m_selectedChannelIndices[graphIndex])[i]], 
						wxPLPlotCtrl::X_BOTTOM, wxPLPlotCtrl::Y_LEFT, pPos);
				}
				SetViewRange(wmin, wmax);

				UpdateStacking();

				m_plotSurface->GetYAxis1(pPos)->SetUnits( m_plots[(*m_selectedChannelIndices[graphIndex])[0]]->GetDataSet()->GetUnits() );
				SetYAxisLabelText();
				AutoscaleYAxisByPlot(graphIndex % 2 == 0, pPos == wxPLPlotCtrl::PLOT_TOP, graphIndex);
			}
			m_plotSurface->SetYAxis2(NULL, pPos);
		}
	}
	else
	{
		AutoscaleYAxisByPlot(graphIndex % 2 == 0, pPos == wxPLPlotCtrl::PLOT_TOP, graphIndex);
		SetYAxisLabelText();
	}

	RefreshDisabledCheckBoxes();
	Invalidate();
}

void wxDVTimeSeriesCtrl::StackUp( wxPLPlotCtrl::AxisPos yap, wxPLPlotCtrl::PlotPos ppos )
{
	wxPLPlotCtrl::AxisPos tyap;
	wxPLPlotCtrl::PlotPos tppos;
	std::vector<wxDVTimeSeriesPlot*> stack;

	for( size_t i=0;i<m_plotSurface->GetPlotCount();i++ )
	{
		if ( wxDVTimeSeriesPlot *cur = dynamic_cast<wxDVTimeSeriesPlot*>( m_plotSurface->GetPlot(i) ) )
		{
			if ( std::find( stack.begin(), stack.end(), cur ) == stack.end() 
				&& m_plotSurface->GetPlotPosition( cur, 0, &tyap, &tppos )
				&& tyap == yap && tppos == ppos )
			{
				cur->SetStackingMode( true );
				cur->StackOnTopOf( stack.size() > 0 ? stack.back() : NULL );
				stack.push_back( cur );
			}
		}
	}
}

void wxDVTimeSeriesCtrl::ClearStacking()
{
	for( size_t i=0;i<m_plots.size();i++ )
	{
		m_plots[i]->SetStackingMode( false );
		m_plots[i]->StackOnTopOf( NULL );
	}
}

void wxDVTimeSeriesCtrl::UpdateStacking()
{
	ClearStacking();
	if ( m_stackingOnYLeft )
	{
		// update all stacking pointers for plots.
		StackUp( wxPLPlotCtrl::Y_LEFT, wxPLPlotCtrl::PLOT_TOP );
		StackUp( wxPLPlotCtrl::Y_LEFT, wxPLPlotCtrl::PLOT_BOTTOM );
	}
}

void wxDVTimeSeriesCtrl::SetYAxisLabelText()
{
	size_t idx;
	wxString YLabelText;
	wxPLPlotCtrl::AxisPos yap;
	wxPLPlotCtrl::PlotPos pPos;

	for (size_t i = 0; i < 4; i++)
	{
		yap = (i == 0 || i == 2) ? wxPLPlotCtrl::Y_LEFT : wxPLPlotCtrl::Y_RIGHT;
		pPos = (i == 0 || i == 1) ? wxPLPlotCtrl::PLOT_TOP : wxPLPlotCtrl::PLOT_BOTTOM;

		if (m_selectedChannelIndices[i]->size() > 0)
		{
			idx = (size_t)m_selectedChannelIndices[i]->at(0);
			YLabelText = m_plots[idx]->GetDataSet()->GetUnits();
			if (m_selectedChannelIndices[i]->size() == 1) { YLabelText = m_plots[idx]->GetDataSet()->GetLabel(); }
			if (m_plotSurface->GetAxis(yap, pPos)) { m_plotSurface->GetAxis(yap, pPos)->SetLabel(YLabelText); }
		}
	}
}

void wxDVTimeSeriesCtrl::ClearAllChannelSelections(wxPLPlotCtrl::PlotPos pPos)
{
	m_dataSelector->ClearColumn(int(pPos));

	int graphIndex = 0;
	if (pPos == wxPLPlotCtrl::PLOT_BOTTOM)
		graphIndex += 2;
	for (int i=graphIndex; i<graphIndex+2; i++)
	{
		for (size_t j=0; j<m_selectedChannelIndices[i]->size(); j++)
		{
			m_plotSurface->RemovePlot(m_plots[m_selectedChannelIndices[i]->at(j)] ); 
		}
		m_selectedChannelIndices[i]->clear();
	}

	ClearStacking();

	m_plotSurface->SetYAxis1(NULL, pPos);
	m_plotSurface->SetYAxis2(NULL, pPos);

	RefreshDisabledCheckBoxes(pPos);
	Invalidate();
}

void wxDVTimeSeriesCtrl::RefreshDisabledCheckBoxes()
{
	RefreshDisabledCheckBoxes(wxPLPlotCtrl::PLOT_TOP);
	RefreshDisabledCheckBoxes(wxPLPlotCtrl::PLOT_BOTTOM);
}

void wxDVTimeSeriesCtrl::RefreshDisabledCheckBoxes(wxPLPlotCtrl::PlotPos pPos)
{
	wxString axis1Label = NO_UNITS;
	wxString axis2Label = NO_UNITS;
	
	if(m_plotSurface->GetYAxis1(pPos))
		axis1Label = m_plotSurface->GetYAxis1(pPos)->GetUnits();
	if (m_plotSurface->GetYAxis2(pPos))
		axis2Label = m_plotSurface->GetYAxis2(pPos)->GetUnits();

	if (axis1Label != NO_UNITS
		&& axis2Label != NO_UNITS
		&& axis1Label != axis2Label)
	{
		for (int i=0; i<m_dataSelector->Length(); i++)
		{
			m_dataSelector->Enable(i, pPos, axis1Label == m_plots[i]->GetDataSet()->GetUnits() 
				|| axis2Label == m_plots[i]->GetDataSet()->GetUnits());
		}
	}
	else
	{
		for (int i=0; i<m_dataSelector->Length(); i++)
		{
			m_dataSelector->Enable(i, pPos, true);
		}
	}
}

wxDVSelectionListCtrl* wxDVTimeSeriesCtrl::GetDataSelectionList()
{
	return m_dataSelector;
}

void wxDVTimeSeriesCtrl::SetTopSelectedNames(const wxString& names)
{
	SetSelectedNamesForColIndex(names, 0);
}

void wxDVTimeSeriesCtrl::SetBottomSelectedNames(const wxString& names)
{
	SetSelectedNamesForColIndex(names, 1);
}

void wxDVTimeSeriesCtrl::SetSelectedNamesForColIndex(const wxString& names, int col)
{
	ClearAllChannelSelections(wxPLPlotCtrl::PlotPos(col));

	wxStringTokenizer tkz(names, ";");

	while(tkz.HasMoreTokens())
	{
		wxString token = tkz.GetNextToken();

		int row = m_dataSelector->SelectRowWithNameInCol(token, col);
		if (row != -1)
			AddGraphAfterChannelSelection(wxPLPlotCtrl::PlotPos(col), row);
	}
}

void wxDVTimeSeriesCtrl::SelectDataSetAtIndex(int index)
{
	m_dataSelector->SelectRowInCol(index, 0);
	AddGraphAfterChannelSelection(wxPLPlotCtrl::PLOT_TOP, index);
}

int wxDVTimeSeriesCtrl::GetNumberOfSelections()
{
	return m_dataSelector->GetNumberOfSelections();
}

wxDVStatType wxDVTimeSeriesCtrl::GetStatType()
{
	return m_statType;
}

void wxDVTimeSeriesCtrl::SetStatType(wxDVStatType statType)
{
	m_statType = statType;
}

void wxDVTimeSeriesCtrl::AutoscaleYAxisByPlot(bool IsLeftAxis, bool IsTopPlot, int SelectedChannelIndex)
{
	//In each of 4 sections below for m_xxxAutoScale = false we must set the y axis min & max to the dataset min & max before the AutoscalYAxis call and set them back to their original values after it.
	//Pass true for ForceUpdate in case we removed a tall graph from a shorter one.
	if (IsLeftAxis && IsTopPlot)
	{
		if (m_topAutoScale == false)
		{
			m_topAutoScale = true;
			AutoscaleYAxis(m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_TOP), *m_selectedChannelIndices[SelectedChannelIndex], true, true);
			m_topAutoScale = false;
		}
		else
		{
			AutoscaleYAxis(m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_TOP), *m_selectedChannelIndices[SelectedChannelIndex], true, false);
		}
	}
	else if (IsLeftAxis && !IsTopPlot)
	{
		if (m_bottomAutoScale == false)
		{
			m_bottomAutoScale = true;
			AutoscaleYAxis(m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_BOTTOM), *m_selectedChannelIndices[SelectedChannelIndex], true, true);
			m_bottomAutoScale = false;
		}
		else
		{
			AutoscaleYAxis(m_plotSurface->GetYAxis1(wxPLPlotCtrl::PLOT_BOTTOM), *m_selectedChannelIndices[SelectedChannelIndex], true, false);
		}
	}
	else if (!IsLeftAxis && IsTopPlot)
	{
		if (m_top2AutoScale == false)
		{
			m_top2AutoScale = true;
			AutoscaleYAxis(m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_TOP), *m_selectedChannelIndices[SelectedChannelIndex], true, true);
			m_top2AutoScale = false;
		}
		else
		{
			AutoscaleYAxis(m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_TOP), *m_selectedChannelIndices[SelectedChannelIndex], true, false);
		}
	}
	else if (!IsLeftAxis && !IsTopPlot)
	{
		if (m_bottom2AutoScale == false)
		{
			m_bottom2AutoScale = true;
			AutoscaleYAxis(m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_BOTTOM), *m_selectedChannelIndices[SelectedChannelIndex], true, true);
			m_bottom2AutoScale = false;
		}
		else
		{
			AutoscaleYAxis(m_plotSurface->GetYAxis2(wxPLPlotCtrl::PLOT_BOTTOM), *m_selectedChannelIndices[SelectedChannelIndex], true, false);
		}
	}
}

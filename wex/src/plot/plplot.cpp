#include <numeric>
#include <limits>

#include <wx/tokenzr.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/msgdlg.h>
#include <wx/filename.h>
#include <wx/dir.h>
#include <wx/stdpaths.h>

#include "wex/pdf/pdfdoc.h"
#include "wex/pdf/pdffont.h"
#include "wex/pdf/pdffontmanager.h"
#include "wex/plot/plhistplot.h"
#include "wex/plot/plplot.h"
#include "wex/plot/ploutdev.h"
#include "wex/plot/plannotation.h"

#if defined(__WXOSX__)||defined(__WXGTK__)
#include <cmath>
#undef wxIsNaN
#define wxIsNaN(a) std::isnan(a)
#endif

static const double text_space = 3.0;
static const wxRealPoint legend_item_box(13.0, 13.0);

class wxPLAxisDeviceMapping : public wxPLDeviceMapping
{
private:
	wxPLAxis *m_xAxis;
	double m_xPhysMin, m_xPhysMax;
	wxPLAxis *m_yAxis;
	double m_yPhysMin, m_yPhysMax;
	double m_physicalConstraint;
	wxRealPoint m_ptCenter;
	bool m_primaryX, m_primaryY;

public:
	wxPLAxisDeviceMapping( wxPLAxis *x, double xmin, double xmax, bool primaryx,
		wxPLAxis *y, double ymin, double ymax, bool primaryy )
		: m_xAxis(x), m_xPhysMin(xmin), m_xPhysMax(xmax), m_primaryX(primaryx), 
		m_yAxis(y), m_yPhysMin(ymin), m_yPhysMax(ymax), m_primaryY(primaryy)
	{
		wxRealPoint pos, size;
		GetDeviceExtents( &pos, &size );
		m_ptCenter = wxRealPoint( 0.5*(pos.x+size.x), 0.5*(pos.y+size.y) );
		m_physicalConstraint = (size.x < size.y) ? size.x : size.y;
	}
	
	virtual wxRealPoint ToDevice( double x, double y ) const
	{
		if (wxPLPolarAngularAxis *pa = dynamic_cast<wxPLPolarAngularAxis *>(m_xAxis)) {
			// this is a polar plot, so translate the "point" as if it's a angle/radius combination

			// adjust for where zero degrees should be (straight up?) and units of angular measure
			double angle_in_rad = pa->AngleInRadians(x);

			// get radius in physical units
			double radius0 = m_yAxis->WorldToPhysical(y, 0, m_physicalConstraint / 2.0); // max radius has to be 1/2 of physical constraint

			// locate point relative to center of polar plot
			wxRealPoint pt(radius0*cos(angle_in_rad), radius0*sin(angle_in_rad));

			// return point on physical device
			return (m_ptCenter + pt);
		}
		else // Cartesian plot
			return wxRealPoint( m_xAxis->WorldToPhysical( x, m_xPhysMin, m_xPhysMax ),
				m_yAxis->WorldToPhysical( y, m_yPhysMin, m_yPhysMax ) );
	}
	
	virtual void GetDeviceExtents( wxRealPoint *pos, wxRealPoint *size ) const
	{
			if ( pos ) {
				pos->x = m_xPhysMin;
				pos->y = m_yPhysMin < m_yPhysMax ? m_yPhysMin : m_yPhysMax;
			}

			if ( size ) {
				size->x = m_xPhysMax - m_xPhysMin;
				size->y = fabs( m_yPhysMax - m_yPhysMin);
			}
	}

	virtual wxRealPoint GetWorldMinimum() const
	{
		return wxRealPoint(
			m_xAxis->PhysicalToWorld( m_xPhysMin, m_xPhysMin, m_xPhysMax ),
			m_yAxis->PhysicalToWorld( m_yPhysMin, m_yPhysMin, m_yPhysMax ) );
	}

	virtual wxRealPoint GetWorldMaximum() const
	{
		return wxRealPoint(
			m_xAxis->PhysicalToWorld( m_xPhysMax, m_xPhysMin, m_xPhysMax ),
			m_yAxis->PhysicalToWorld( m_yPhysMax, m_yPhysMin, m_yPhysMax ) );
	}

	virtual wxPLAxis *GetXAxis() const {
		return m_xAxis;
	}

	virtual wxPLAxis *GetYAxis() const {
		return m_yAxis;
	}

	virtual bool IsPrimaryXAxis() const { return m_primaryX; }
	virtual bool IsPrimaryYAxis() const { return m_primaryY; }
};


wxString wxPLPlottable::GetXDataLabel( wxPLPlot *plot ) const
{
	if ( !m_xLabel.IsEmpty() ) return m_xLabel;
	
	wxPLPlot::AxisPos xap, yap;
	wxPLPlot::PlotPos ppos;
	if ( plot && plot->GetPlotPosition( this, &xap, &yap, &ppos ) )
		if ( wxPLAxis *ax = plot->GetAxis( xap, ppos ) )
			return ax->GetLabel();
	
	return wxEmptyString;
}

wxString wxPLPlottable::GetYDataLabel( wxPLPlot *plot ) const
{
	if ( !m_yLabel.IsEmpty() ) return m_yLabel;
	if ( !m_label.IsEmpty() ) return m_label;
	
	wxPLPlot::AxisPos xap, yap;
	wxPLPlot::PlotPos ppos;
	if ( plot && plot->GetPlotPosition( this, &xap, &yap, &ppos ) )
		if ( wxPLAxis *ax = plot->GetAxis( yap, ppos ) )
			return ax->GetLabel();
	
	return wxEmptyString;
}

wxPLAxis *wxPLPlottable::SuggestXAxis() const 
{
	double xmin = 0, xmax = 0, ymin = 0, ymax = 0;
	GetMinMax( &xmin, &xmax, &ymin, &ymax );
	return new wxPLLinearAxis( xmin, xmax );
}

wxPLAxis *wxPLPlottable::SuggestYAxis() const 
{
	double xmin = 0, xmax = 0, ymin = 0, ymax = 0;
	GetMinMax( &xmin, &xmax, &ymin, &ymax );
	return new wxPLLinearAxis( ymin, ymax );
}

bool wxPLPlottable::GetMinMax(double *pxmin, double *pxmax, double *pymin, double *pymax) const
{
	if (Len() == 0) return false;

	double myXMin = std::numeric_limits<double>::quiet_NaN();
	double myXMax = std::numeric_limits<double>::quiet_NaN();
	double myYMin = std::numeric_limits<double>::quiet_NaN();
	double myYMax = std::numeric_limits<double>::quiet_NaN();

	for (size_t i=0; i<Len(); i++)
	{
		wxRealPoint pt(At(i));

		bool nanval = wxIsNaN( pt.x ) || wxIsNaN( pt.y );
		if ( !nanval )
		{
			if ( wxIsNaN( myXMin ) ) 
				myXMin = pt.x;
			if ( wxIsNaN( myXMax ) ) 
				myXMax = pt.x;
			if ( wxIsNaN( myYMin ) ) 
				myYMin = pt.y;
			if ( wxIsNaN( myYMax ) ) 
				myYMax = pt.y;

			if ( pt.x < myXMin )
				myXMin = pt.x;
			if ( pt.x > myXMax )
				myXMax = pt.x;
			if ( pt.y < myYMin )
				myYMin = pt.y;
			if ( pt.y > myYMax )
				myYMax = pt.y;
		}
	}

	if (pxmin) *pxmin = myXMin;
	if (pxmax) *pxmax = myXMax;
	if (pymin) *pymin = myYMin;
	if (pymax) *pymax = myYMax;

	return !wxIsNaN( myXMin )
		&& !wxIsNaN( myXMax )
		&& !wxIsNaN( myYMin )
		&& !wxIsNaN( myYMax );
}

bool wxPLPlottable::ExtendMinMax(double *pxmin, double *pxmax, double *pymin, double *pymax, bool extendToNice) const
{
	double xmin, xmax, ymin, ymax;
	if (!GetMinMax(&xmin, &xmax, &ymin, &ymax)) return false;
	double yminNice = ymin, ymaxNice = ymax;
	if (extendToNice) wxPLAxis::ExtendBoundsToNiceNumber(&ymaxNice, &yminNice);
	if (pxmin && xmin < *pxmin) *pxmin = xmin;
	if (pxmax && xmax > *pxmax) *pxmax = xmax;
	if (pymin && yminNice < *pymin) *pymin = yminNice;
	if (pymax && ymaxNice > *pymax) *pymax = ymaxNice;
	return true;
}

std::vector<wxString> wxPLPlottable::GetExportableDatasetHeaders( wxUniChar sep, wxPLPlot *plot ) const
{
	std::vector<wxString> tt;
	wxString xLabel = GetXDataLabel( plot );
	wxString yLabel = GetYDataLabel( plot );
			
	//Remove sep chars that we don't want
	while (xLabel.Find(sep) != wxNOT_FOUND)
	{
		xLabel = xLabel.BeforeFirst(sep) + xLabel.AfterFirst(sep);
	}

	while (yLabel.Find(sep) != wxNOT_FOUND)
	{
		yLabel = yLabel.BeforeFirst(sep) + yLabel.AfterFirst(sep);
	}

	tt.push_back(xLabel);
	tt.push_back(yLabel);

	return tt;
}

std::vector<wxRealPoint> wxPLPlottable::GetExportableDataset(double Xmin, double Xmax, bool visible_only) const
{
	std::vector<wxRealPoint> data;
	wxRealPoint pt;

	for (size_t i = 0; i < Len(); i++)
	{
		pt = At(i);
		if((pt.x >= Xmin && pt.x <= Xmax) || !visible_only) { data.push_back(At(i)); }
	}

	return data;
}

wxPLSideWidgetBase::wxPLSideWidgetBase()
{
	m_bestSize.x = m_bestSize.y = -1;
}

wxPLSideWidgetBase::~wxPLSideWidgetBase()
{
	// nothing to do
}

void wxPLSideWidgetBase::InvalidateBestSize()
{
	m_bestSize.x = m_bestSize.y = -1;
}

wxRealPoint wxPLSideWidgetBase::GetBestSize( wxPLOutputDevice &dc )
{
	if ( m_bestSize.x < 0 || m_bestSize.y < 0 )
		m_bestSize = CalculateBestSize( dc );

	if ( m_bestSize.x <= 0 ) m_bestSize.x = 5;
	if ( m_bestSize.y <= 0 ) m_bestSize.y = 5;
	return m_bestSize;
}

class wxPLPlot::axis_layout
{
private:

	struct tick_layout
	{
		tick_layout( wxPLOutputDevice &dc, const wxPLAxis::TickData &td )
			: text( dc, td.label, wxPLTextLayout::CENTER ), world( td.world ), 
			  tick_size( td.size ), 
			  text_angle(0.0) {  }
				
		wxPLTextLayout text;
		double world;
		wxPLAxis::TickData::TickSize tick_size;
		double text_angle;
	};

	std::vector< tick_layout > m_tickList;
	int m_axisPos;
	wxSize m_bounds;

	void renderAngular( wxPLOutputDevice &dc, double radius, wxPLPolarAngularAxis *axis, double cntr_x, double cntr_y)
	{
		wxRealPoint cntr(cntr_x, cntr_y);

		// draw tick marks and tick labels
		for (size_t i = 0; i < m_tickList.size(); i++)
		{
			tick_layout &ti = m_tickList[i];
			double tick_length = ti.tick_size == wxPLAxis::TickData::LARGE ? 5 : 2;
			double angle = axis->AngleInRadians(ti.world);
			wxRealPoint pt0( cntr + wxRealPoint(radius*cos(angle), radius*sin(angle)) );
			wxRealPoint pt1( cntr + wxRealPoint((radius - tick_length)*cos(angle), (radius - tick_length)*sin(angle)) );
			dc.Line(pt0, pt1);

			if (ti.text.Width() > 0)
			{
				// text is place via upper left corner of text box, which only works for the bottom right
				// quadrant of the polar plot.  so, text must be placed based on quadrant
				
				if (pt0.x < pt1.x && pt0.y >= pt1.y) // bottom left quadrant
					ti.text.Render(dc, pt0.x - ti.text.Width() - text_space, pt0.y, 0, false);
				else if (pt0.x < pt1.x && pt0.y < pt1.y) // top left quadrant
					ti.text.Render(dc, pt0.x - ti.text.Width() - text_space, pt0.y-ti.text.Height(), 0, false);
				else if (pt0.x >= pt1.x && pt0.y < pt1.y) // top right quadrant
					ti.text.Render(dc, pt0.x + text_space, pt0.y-ti.text.Height(), 0, false);
				else
					ti.text.Render(dc, pt0.x + text_space, pt0.y, 0, false);
			}

		}
	}


public:	
	static const int TextAxisOffset = 3;

	std::vector<double> ticks( wxPLAxis::TickData::TickSize size )
	{
		std::vector<double> values;
		for (size_t i=0;i<m_tickList.size();i++)
			if ( m_tickList[i].tick_size == size )
				values.push_back( m_tickList[i].world );
		return values;
	}

	axis_layout( int ap, wxPLOutputDevice &dc, wxPLAxis *axis, double phys_min, double phys_max )
	{
		m_axisPos = ap;

		// compute the 'best' geometry 
		std::vector< wxPLAxis::TickData > tl;
		axis->GetAxisTicks( phys_min, phys_max, tl );
		if ( tl.size() == 0 ) return;

		// calculate text size of each tick
		m_tickList.reserve( tl.size() );
		for ( size_t i=0;i<tl.size();i++ )
			m_tickList.push_back( tick_layout( dc, tl[i] ) );
		
		if ( ap == X_BOTTOM || ap == X_TOP )
		{
			// calculate axis bounds extents assuming
			// no text rotation
			double xmin = phys_min;
			double xmax = phys_max;
			double ymax = 0;

			for ( size_t i=0;i<m_tickList.size();i++ )
			{
				tick_layout &ti = m_tickList[i];
				double phys = axis->WorldToPhysical( ti.world, phys_min, phys_max );
				if ( phys-ti.text.Width()/2 < xmin )
					xmin = phys-ti.text.Width()/2;
				if ( phys+ti.text.Width()/2 > xmax )
					xmax = phys+ti.text.Width()/2;
				if ( ti.text.Height() > ymax )
					ymax = ti.text.Height();
			}

			m_bounds.x = xmax-xmin;
			m_bounds.y = ymax;

			// if this is a polar axis, we're done
			if (wxPLPolarAngularAxis *pa = dynamic_cast<wxPLPolarAngularAxis *>(axis))
				return;

			// required pixels of separation between adjacent tick texts
			const double tick_label_space = 4;

			// mediocre attempt for laying out axis tick text to avoid overlap
			// note: may need to adjust bounds accordingly
			
			std::vector< tick_layout* > labeledTicks;
			for ( size_t i=0; i < m_tickList.size(); i++ )
				if ( m_tickList[i].text.Width() > 0 )
					labeledTicks.push_back( &m_tickList[i] );
			if ( labeledTicks.size() > 2 )
			{
				// find the largest tick mark label
				// and try to position it by rotation so
				// that it doesn't touch adjacent text
				double textAngle = 0.0; // default: no rotation

				size_t index = 0;
				double width = 0;
				for ( size_t j = 0; j < labeledTicks.size(); j++ )
				{
					if ( labeledTicks[j]->text.Width() > width )
					{
						width = labeledTicks[j]->text.Width();
						index = j;
					}
				}

				if ( index == 0 ) index++;
				else if ( index == labeledTicks.size()-1 ) index--;

				tick_layout &left = *labeledTicks[index-1];
				tick_layout &center = *labeledTicks[index];
				tick_layout &right = *labeledTicks[index+1];

				double phys_left = axis->WorldToPhysical( left.world, phys_min, phys_max );
				double phys = axis->WorldToPhysical( center.world, phys_min, phys_max );
				double phys_right = axis->WorldToPhysical( right.world, phys_min, phys_max );
				
				bool rotate = false;
				if ( !axis->IsReversed() )
				{
					rotate = (phys - center.text.Width()/2 - tick_label_space < phys_left + left.text.Width()/2)
						|| (phys + center.text.Width()/2 + tick_label_space > phys_right - right.text.Width()/2);
				}
				else
				{
					rotate = (phys + center.text.Width()/2 + tick_label_space > phys_left - left.text.Width()/2)
						|| (phys - center.text.Width()/2 - tick_label_space < phys_right + right.text.Width()/2);
				}

				if ( rotate )
				{
					// rotate all the ticks, not just the large ones

					textAngle = 45.0;
					for ( size_t j=0;j<m_tickList.size();j++ )
					{
						m_tickList[j].text_angle = textAngle;
						// calculate text offset positions so that 
						// text bounding rect corners line up with point on axis

						// realign text layout due to rotation (better look for multi-line tick labels)
						m_tickList[j].text.Align( ap == X_TOP ? wxPLTextLayout::LEFT : wxPLTextLayout::RIGHT );
					}

					// recalculate bound height to rotated text
					m_bounds.y = labeledTicks[index]->text.Height()
						+ fabs( labeledTicks[index]->text.Width() * sin( M_PI/180*textAngle ) );
				}
			}
		}
		else
		{ // ap = Y_LEFT || Y_RIGHT
			double ymin = phys_max; // ymin is upper coordinate
			double ymax = phys_min; // ymax is lower coordinate
			double xmax = 0; // maximum text width
			for ( size_t i=0;i<m_tickList.size();i++ )
			{
				tick_layout &ti = m_tickList[i];
				double phys = axis->WorldToPhysical( ti.world, phys_min, phys_max );
				if ( phys-ti.text.Height()/2 < ymin )
					ymin = phys-ti.text.Height()/2;
				if ( phys+ti.text.Height()/2 > ymax )
					ymax = phys+ti.text.Height()/2;				
				if ( ti.text.Width() > xmax )
					xmax = ti.text.Width();
			}

			m_bounds.x = xmax;
			m_bounds.y = ymax-ymin;		
		}
	}

	wxSize bounds() { return m_bounds; }

	void render( wxPLOutputDevice &dc, double ordinate, wxPLAxis *axis, double phys_min, double phys_max, double ordinate_opposite = -1 )
	{
		// if this is a polar angular axis, render it and leave
		// radius passed in a 'ordinate', center.x as phys_min, center.y as phys_max
		if (wxPLPolarAngularAxis *pa = dynamic_cast<wxPLPolarAngularAxis *>(axis)) {
			renderAngular(dc, ordinate, pa, phys_min, phys_max);
			return;
		}

		double tlarge, tsmall;
		axis->GetTickSizes( &tsmall, &tlarge );
		
		// draw tick marks and tick labels
		for ( size_t i=0;i<m_tickList.size(); i++ )
		{
			tick_layout &ti = m_tickList[i];
			double tick_length = ti.tick_size == wxPLAxis::TickData::LARGE ? tlarge : tsmall;
			double physical = axis->WorldToPhysical( ti.world, phys_min, phys_max );

			if ( ti.tick_size != wxPLAxis::TickData::NONE  && tick_length != 0.0 )
			{
				wxRealPoint tickStart, tickEnd;

				if ( m_axisPos == X_BOTTOM || m_axisPos == X_TOP )
				{
					tickStart.x = physical;
					tickStart.y = ordinate;
					tickEnd.x = physical;
					tickEnd.y = m_axisPos == X_BOTTOM ? ordinate-tick_length : ordinate+tick_length;
				}
				else
				{
					tickStart.x = ordinate;
					tickStart.y = physical;
					tickEnd.x = m_axisPos == Y_LEFT ? ordinate + tick_length : ordinate - tick_length;
					tickEnd.y = physical;
				}

				dc.Line( tickStart, tickEnd );

				// draw ticks on opposite ordinate line if needed
				if ( ordinate_opposite > 0 )
				{
					if ( m_axisPos == X_BOTTOM || m_axisPos == X_TOP )
					{
						tickStart.x = physical;
						tickStart.y = ordinate_opposite;
						tickEnd.x = physical;
						tickEnd.y = m_axisPos == X_BOTTOM ? ordinate_opposite+tick_length : ordinate_opposite-tick_length;
					}
					else
					{
						tickStart.x = ordinate_opposite;
						tickStart.y = physical;
						tickEnd.x = m_axisPos == Y_LEFT ? ordinate_opposite-tick_length : ordinate_opposite+tick_length;
						tickEnd.y = physical;
					}

					dc.Line( tickStart, tickEnd );
				}
			}

			if ( ti.text.Width() > 0 )
			{
				double text_x = 0, text_y = 0;
				if ( m_axisPos ==  X_BOTTOM )
				{
					if ( ti.text_angle == 0.0 )
					{
						text_x = physical - ti.text.Width()/2;
						text_y = ordinate + TextAxisOffset;
					}
					else
					{
						double angleRad = -M_PI/180*ti.text_angle;
						text_x = physical - ti.text.Width()*cos(angleRad);
						text_y = ordinate + TextAxisOffset - ti.text.Width()*sin(angleRad);
					}
				}
				else if ( m_axisPos == X_TOP )
				{
					if ( ti.text_angle == 0.0 )
					{
						text_x = physical - ti.text.Width()/2;
						text_y = ordinate - TextAxisOffset - ti.text.Height();
					}
					else
					{
						double angleRad = -M_PI/180*ti.text_angle;
						text_x = physical + ti.text.Height()*sin(angleRad);
						text_y = ordinate - TextAxisOffset - ti.text.Height()*cos(angleRad);
					}
				}
				else if ( m_axisPos == Y_LEFT )
				{
					text_x = ordinate - ti.text.Width() - TextAxisOffset;
					text_y = physical - ti.text.Height()/2;
				}
				else
				{ // Y_RIGHT
					text_x = ordinate + TextAxisOffset;
					text_y = physical - ti.text.Height()/2;
				}
			
				ti.text.Render( dc, 
					text_x, text_y, ti.text_angle, false );			
			}
		}

	}
};

wxPLPlot::axis_data::axis_data()
	: axis(0), layout(0), label(0)
{
}

wxPLPlot::axis_data::~axis_data()
{
	if (axis) delete axis;
	if (layout) delete layout;
	if (label) delete label;
}

void wxPLPlot::axis_data::set( wxPLAxis *a )
{
	if (axis == a) return;
	if (axis) delete axis;
	axis = a;
	invalidate();
}

void wxPLPlot::axis_data::invalidate()
{
	if (layout) delete layout;
	layout = 0;
	if (label) delete label;
	label = 0;
}


wxPLPlot::wxPLPlot()
{
	for ( size_t i=0;i<4;i++ )
		m_sideWidgets[i] = 0;

	m_textSizePoints = 11.0;
	m_borderWidth = 0.5;
	m_spaceLeftTop = wxRealPoint(0,0);
	m_spaceRightBottom = wxRealPoint(0,0);
	m_showLegend = true;
	m_showLegendBorder = true;
	m_showCoarseGrid = false;
	m_showFineGrid = false;
	m_showTitle = true;
	m_titleLayout = 0;
	m_gridColour.Set( 225, 225, 225 );
	m_plotAreaColour = *wxWHITE;
	m_tickTextColour = *wxBLACK;
	m_axisColour = *wxBLACK;
	m_legendRect.x = 10;
	m_legendRect.y = 10;
	m_reverseLegend = false;
	m_legendInvalidated = true;
	m_legendPosPercent.x = 85.0;
	m_legendPosPercent.y = 4.0;
	m_legendPos = RIGHT;
	m_anchorPoint = wxPoint(0, 0);
	m_currentPoint = wxPoint(0, 0);
	m_moveLegendMode = false;
	m_moveLegendErase = false;
}

wxPLPlot::~wxPLPlot()
{
	if ( m_titleLayout != 0 )
		delete m_titleLayout;

	for ( std::vector<plot_data>::iterator it = m_plots.begin();
		it != m_plots.end();
		++it )
		delete it->plot;

	for ( size_t i=0;i<m_legendItems.size(); i++ )
		delete m_legendItems[i];

	for ( size_t i=0;i<4;i++ )
		if ( m_sideWidgets[i] != 0 )
			delete m_sideWidgets[i];

	DeleteAllAnnotations();
}

void wxPLPlot::AddAnnotation( wxPLAnnotation *an, wxPLAnnotation::PositionMode pm,
	AxisPos xap, AxisPos yap, PlotPos ppos, wxPLAnnotation::ZOrder zo )
{
	annot_data d;
	d.ann = an;
	d.posm = pm;
	d.xap = xap;
	d.yap = yap;
	d.ppos = ppos;
	d.zorder = zo;
	m_annotations.push_back( d );
}

void wxPLPlot::DeleteAllAnnotations()
{
	for( std::vector<annot_data>::iterator it = m_annotations.begin();
		it != m_annotations.end();
		++it )
		delete it->ann;

	m_annotations.clear();
}

void wxPLPlot::AddPlot( wxPLPlottable *p, AxisPos xap, AxisPos yap, PlotPos ppos, bool update_axes )
{
	plot_data dd;
	dd.plot = p;
	dd.ppos = ppos;
	dd.xap = xap;
	dd.yap = yap;

	m_plots.push_back( dd );

	if ( GetAxis( xap ) == 0 )
		SetAxis( p->SuggestXAxis(), xap );

	if ( GetAxis( yap, ppos ) == 0 )
		SetAxis( p->SuggestYAxis(), yap, ppos );

	if ( p->IsShownInLegend() )		
		m_legendInvalidated = true;

	if ( update_axes )
		UpdateAxes( false );
}

wxPLPlottable *wxPLPlot::RemovePlot( wxPLPlottable *p, PlotPos plotPosition )
{
	for ( std::vector<plot_data>::iterator it = m_plots.begin();
		it != m_plots.end();
		++it )
	{
		if ( it->plot == p && (it->ppos == plotPosition || plotPosition == NPLOTPOS) )
		{
			m_plots.erase( it );
			if ( p->IsShownInLegend() )
				m_legendInvalidated = true;

			return p;
		}
	}

	return 0;
}

bool wxPLPlot::ContainsPlot(wxPLPlottable *p, PlotPos plotPosition)
{
	for (std::vector<plot_data>::iterator it = m_plots.begin();
		it != m_plots.end();
		++it)
	{
		if (it->plot == p && (it->ppos == plotPosition || plotPosition == NPLOTPOS))
		{
			return true;
		}
	}

	return false;
}

void wxPLPlot::DeleteAllPlots()
{
	for ( std::vector<plot_data>::iterator it = m_plots.begin();
		it != m_plots.end();
		++it )
		delete it->plot;

	m_plots.clear();	
	m_legendInvalidated = true;

	UpdateAxes( true );
}

size_t wxPLPlot::GetPlotCount()
{
	return m_plots.size();
}

wxPLPlottable *wxPLPlot::GetPlot( size_t i )
{
	if ( i >= m_plots.size() ) return 0;
	
	return m_plots[i].plot;
}

wxPLPlottable *wxPLPlot::GetPlotByLabel( const wxString &series )
{
	for( size_t i=0;i<m_plots.size();i++ )
		if ( m_plots[i].plot->GetLabel() == series )
			return m_plots[i].plot;

	return 0;
}

bool wxPLPlot::GetPlotPosition( const wxPLPlottable *p, 
	AxisPos *xap, AxisPos *yap, PlotPos *ppos )
{
	for ( std::vector<plot_data>::iterator it = m_plots.begin();
		it != m_plots.end();
		++it )
	{
		if ( it->plot == p )
		{
			if ( xap != 0 ) *xap = it->xap;
			if ( yap != 0 ) *yap = it->yap;
			if ( ppos != 0 ) *ppos = it->ppos;
			return true;
		}
	}

	return false;
}

wxPLAxis *wxPLPlot::GetAxis( AxisPos axispos, PlotPos ppos )
{
	switch( axispos )
	{
	case X_BOTTOM: return m_x1.axis;
	case X_TOP: return m_x2.axis;
	case Y_LEFT: return m_y1[ppos].axis;
	case Y_RIGHT: return m_y2[ppos].axis;
	default: return 0;
	}
}

wxPLAxis &wxPLPlot::Axis( AxisPos axispos, PlotPos ppos )
{
static wxPLLinearAxis s_nullAxis(0, 1, "<null-axis>");
	switch( axispos )
	{
	case X_BOTTOM: if ( m_x1.axis ) return *m_x1.axis;
	case X_TOP: if ( m_x2.axis ) return *m_x2.axis;
	case Y_LEFT: if ( m_y1[ppos].axis ) return *m_y1[ppos].axis;
	case Y_RIGHT: if ( m_y2[ppos].axis ) return *m_y2[ppos].axis;
	default: return s_nullAxis;
	}
}

void wxPLPlot::SetAxis( wxPLAxis *a, AxisPos axispos, PlotPos ppos )
{
	switch( axispos )
	{
	case X_BOTTOM: SetXAxis1( a ); break;
	case X_TOP: SetXAxis2( a ); break;
	case Y_LEFT: SetYAxis1( a, ppos ); break;
	case Y_RIGHT: SetYAxis2( a, ppos ); break;
	}
}

void wxPLPlot::SetTitle( const wxString &title )
{
	if ( title != m_title && m_titleLayout != 0 )
	{
		delete m_titleLayout;
		m_titleLayout = 0;
	}
		
	m_title = title;
}

void wxPLPlot::SetLegendLocation( LegendPos pos, double xpercent, double ypercent )
{
	if ( xpercent > -998.0 )
	{
		if ( xpercent < -10 ) xpercent = -10;
		if ( xpercent > 90 ) xpercent = 90;
		m_legendPosPercent.x = xpercent;
	}

	if ( ypercent > -998.0 )
	{
		if ( ypercent < -10 ) ypercent = -10;
		if ( ypercent > 90 ) ypercent = 90;
		m_legendPosPercent.y = ypercent;
	}

	m_legendPos = pos;
}

bool wxPLPlot::SetLegendLocation( const wxString &spos )
{
	wxString llpos = spos.Lower();
	
	if ( llpos == "northwest" ) { SetLegendLocation( NORTHWEST ); return true; }
	if ( llpos == "northeast" ) { SetLegendLocation( NORTHEAST ); return true; }
	if ( llpos == "southwest" ) { SetLegendLocation( SOUTHWEST ); return true; }
	if ( llpos == "southeast" ) { SetLegendLocation( SOUTHEAST ); return true; }
	
	if ( llpos == "north" ) { SetLegendLocation( NORTH ); return true; }
	if ( llpos == "south" ) { SetLegendLocation( SOUTH ); return true; }
	if ( llpos == "east" ) { SetLegendLocation( EAST ); return true; }
	if ( llpos == "west" ) { SetLegendLocation( WEST ); return true; }

	if ( llpos == "bottom" ) { SetLegendLocation( BOTTOM ); return true; }
	if ( llpos == "right" ) { SetLegendLocation( RIGHT ); return true; }

	return false;
}

void wxPLPlot::SetSideWidget( wxPLSideWidgetBase *sw, AxisPos pos )
{
	if ( m_sideWidgets[pos] != 0 
		&& m_sideWidgets[pos] != sw )
		delete m_sideWidgets[pos];

	m_sideWidgets[pos] = sw;
}

wxPLSideWidgetBase *wxPLPlot::GetSideWidget( AxisPos pos )
{
	return m_sideWidgets[pos];
}

wxPLSideWidgetBase *wxPLPlot::ReleaseSideWidget( AxisPos pos )
{
	wxPLSideWidgetBase *w = m_sideWidgets[pos];
	m_sideWidgets[pos] = 0;
	return w;
}

void wxPLPlot::WriteDataAsText( wxUniChar sep, wxOutputStream &os, bool visible_only, bool include_x )
{
	if ( m_plots.size() == 0 ) { return; }

	wxTextOutputStream tt(os);
	wxString sepstr(sep);
	wxString xDataLabel = "";
	wxPLAxis *xaxis;
	wxPLPlottable *plot;
	double worldMin;
	double worldMax;
	wxPLHistogramPlot* histPlot;
	std::vector<bool> includeXForPlot(m_plots.size(), false);
	std::vector<wxString> Headers;
	std::vector< std::vector<wxRealPoint> > data;
	size_t maxLength = 0;
	bool keepGoing; //Used to stop early if all columns are no longer visible.

	//Add column headers
	for ( size_t i = 0; i < m_plots.size(); i++ )
	{
		plot = m_plots[i].plot;

		worldMin = 0.0;
		worldMax = plot->At(plot->Len() - 1).x;

		//We only include the x column on a plot if we are including X and if its x header is different than the previous column's.
		if(i == 0)
		{
			includeXForPlot[i] = true;
		}
		else if(histPlot = dynamic_cast<wxPLHistogramPlot*>( plot )) 
		{ 
			includeXForPlot[i] = true;

			//For CDF plots there is no X data label. The closest useful label is the Y label of the companion PDF histogram plot, so we need to store for use by the CDF plot.
			if ( xDataLabel.IsEmpty() ) { xDataLabel = m_plots[i].plot->GetYDataLabel(this); }
		}
		else if(histPlot = dynamic_cast<wxPLHistogramPlot*>( m_plots[i - 1].plot ))
		{
			includeXForPlot[i] = true;
		}
		else
		{
			includeXForPlot[i] = (m_plots[i].plot->GetXDataLabel( this ) != m_plots[i-1].plot->GetXDataLabel( this ));
		}

		if(histPlot = dynamic_cast<wxPLHistogramPlot*>( m_plots[i].plot ))
		{
			//Do nothing
		}
		else
		{
			xaxis = GetAxis( m_plots[i].xap, m_plots[i].ppos );
			if ( xaxis )
			{
				worldMin = xaxis->GetWorldMin();
				worldMax = xaxis->GetWorldMax();
			}
		}

		Headers = plot->GetExportableDatasetHeaders(sep, this);
		data.push_back(plot->GetExportableDataset(worldMin, worldMax, visible_only));

		if (include_x && includeXForPlot[i])
		{
			if (i > 0) { tt << sepstr;}	//Extra column since we have a new set of x values.
			tt << (Headers[0].IsEmpty() ? xDataLabel : Headers[0]);
			tt << sepstr;
		}

		tt << Headers[1];

		if (Headers.size() > 2)
		{
			for (size_t j = 2; j < Headers.size(); j++)
			{
				tt << sepstr;
				tt << Headers[j];
			}
		}
		if ( i < m_plots.size() - 1 ) { tt << sepstr; }
		if ( data[i].size() > maxLength ) { maxLength = data[i].size(); }
	}

	tt << "\n";

	//Add data
	for (size_t RowNum = 0; RowNum < maxLength; RowNum++)
	{
		keepGoing = false;

		for (size_t PlotNum = 0; PlotNum < m_plots.size(); PlotNum++)
		{
			if ( RowNum < data[PlotNum].size() )
			{
				keepGoing = true;

				if ( include_x && includeXForPlot[PlotNum] )
				{
					if (PlotNum > 0) tt << sepstr; //extra sep before to add blank column before new x values, as in header.
					tt << wxString::Format("%lg", data[PlotNum][RowNum].x );
					tt << sepstr; 
				}

				tt << wxString::Format("%lg", data[PlotNum][RowNum].y);
			}
			else
			{
				if (PlotNum > 0) tt << sepstr; //extra sep before to add blank column before new x values, as in header.
				tt << sepstr; 
			}

			if ( PlotNum < m_plots.size() - 1 ) { tt << sepstr; }
		}

		if (!keepGoing) break;

		tt << "\n";
	}
}

void wxPLPlot::ShowAxes( bool b )
{
	if ( m_x1.axis ) m_x1.axis->Show( b );
	if ( m_x2.axis ) m_x2.axis->Show( b );
	
	for ( size_t pp = 0; pp < NPLOTPOS; pp++ )
	{
		if ( m_y1[pp].axis ) m_y1[pp].axis->Show( b );
		if ( m_y2[pp].axis ) m_y2[pp].axis->Show( b );
	}

	Invalidate();
}


class wxPLDefaultAnnotationMapper : public wxPLAnnotationMapping
{
	wxPLRealRect m_dev;
	wxPLAnnotation::PositionMode m_posMode;
	wxPLAxis *m_xax, *m_yax;
	double m_xmin, m_xmax, m_ymin, m_ymax;
public:
	wxPLDefaultAnnotationMapper( wxPLAnnotation::PositionMode posmode, const wxPLRealRect &extent, 
		wxPLAxis *xx, double xmin, double xmax, wxPLAxis *yy, double ymin, double ymax )
		: m_posMode(posmode), m_dev(extent), 
		m_xax(xx), m_xmin(xmin), m_xmax(xmax),
		m_yax(yy), m_ymin(ymin), m_ymax(ymax)
	{
	}

	virtual ~wxPLDefaultAnnotationMapper() { /* nothing to do */ }

	wxRealPoint ToDevice( const wxRealPoint &pos ) const
	{
		switch( m_posMode )
		{
		case wxPLAnnotation::AXIS:
			if ( m_xax && m_yax )
				return wxRealPoint( m_xax->WorldToPhysical( pos.x, m_xmin, m_xmax ),
					m_yax->WorldToPhysical( pos.y, m_ymin, m_ymax ) );

		case wxPLAnnotation::POINTS:
			return wxRealPoint( m_dev.x + pos.x, m_dev.y + pos.y );

		case wxPLAnnotation::FRACTIONAL:
		default:
			return wxRealPoint( m_dev.x + pos.x*m_dev.width,
				m_dev.y + pos.y*m_dev.height );
		}
	}



};

void wxPLPlot::Render( wxPLOutputDevice &dc, wxPLRealRect geom )
{
#define NORMAL_FONT(dc)  dc.TextPoints( 0 )
#define TITLE_FONT(dc)   dc.TextPoints( +1 )
#define LEGEND_FONT(dc)  dc.TextPoints( -1 )
#define AXIS_FONT(dc)    dc.TextPoints( 0 )

	NORMAL_FONT(dc);
	dc.TextColour( *wxBLACK );

	// ensure plots have the axes they need to be rendered
	for ( size_t i = 0; i< m_plots.size(); i++ )
	{
		if ( GetAxis( m_plots[i].xap ) == 0 )
			SetAxis( m_plots[i].plot->SuggestXAxis(), m_plots[i].xap );

		if ( GetAxis( m_plots[i].yap, m_plots[i].ppos ) == 0 )
			SetAxis( m_plots[i].plot->SuggestYAxis(), m_plots[i].yap, m_plots[i].ppos );
	}

	// draw any side widgets first and remove the space from the total plot area
	if ( m_sideWidgets[Y_LEFT] != 0 )
	{
		wxRealPoint sz = m_sideWidgets[Y_LEFT]->GetBestSize( dc );
		m_sideWidgets[Y_LEFT]->Render( dc, 
			wxPLRealRect( geom.x, geom.y, 
				sz.x, geom.height ) );
		geom.width -= sz.x;
		geom.x += sz.x;
	}

	if ( m_sideWidgets[Y_RIGHT] != 0 )
	{
		wxRealPoint sz = m_sideWidgets[Y_RIGHT]->GetBestSize( dc );		
		m_sideWidgets[Y_RIGHT]->Render( dc, 
			wxPLRealRect( geom.x+geom.width-sz.x, geom.y, 
				sz.x, geom.height ) );

		geom.width -= sz.x;
	}


	wxPLRealRect box( geom.x+text_space, 
		geom.y+text_space, 
		geom.width-text_space-text_space, 
		geom.height-text_space-text_space );

	bool legend_bottom = false;
	bool legend_right = false;
	if ( m_showLegend )
	{
		LEGEND_FONT(dc);
		CalculateLegendLayout( dc );

		if ( m_legendPos == BOTTOM )
		{
			if ( m_legendRect.height > 0 ) legend_bottom = true;
			box.height -= m_legendRect.height;
		}

		if ( m_legendPos == RIGHT )
		{
			if (m_legendRect.width > 0) legend_right = true;
			box.width -= m_legendRect.width;
		}
	}
	
	TITLE_FONT(dc);

	// position and render title using cached layout if possible
	if ( m_showTitle && !m_title.IsEmpty() )
	{
		if ( m_titleLayout == 0 )
			m_titleLayout = new wxPLTextLayout( dc, m_title, wxPLTextLayout::CENTER );

		m_titleLayout->Render( dc, box.x+box.width/2-m_titleLayout->Width()/2, box.y, 0, false );
		box.y += m_titleLayout->Height() + text_space;
		box.height -= m_titleLayout->Height() + text_space;
	}
	else
	{

		// if no title, leave a few extra pixels at the top for Y axes labels at the edges
		double topmargin = 0;
		dc.Measure( "0", NULL, &topmargin );
		topmargin = 0.5*topmargin + text_space;
		box.y += topmargin;
		box.height -= topmargin;
	}

	NORMAL_FONT(dc);

	wxPLRealRect plotbox = box; // save current box for where to draw the axis labels

	// determine sizes of axis labels

	if ( m_x2.axis && m_x2.axis->IsShown() && m_x2.axis->IsLabelVisible() && !m_x2.axis->GetLabel().IsEmpty() )
	{
		if ( m_x2.label == 0 )
			m_x2.label = new wxPLTextLayout( dc, m_x2.axis->GetLabel(), wxPLTextLayout::CENTER );

		box.y += m_x2.label->Height() + text_space;
		box.height -= m_x2.label->Height() + text_space;
	}

	if ( m_x1.axis && m_x1.axis->IsShown() && m_x1.axis->IsLabelVisible() && !m_x1.axis->GetLabel().IsEmpty() )
	{
		if ( m_x1.label == 0 )
			m_x1.label = new wxPLTextLayout( dc, m_x1.axis->GetLabel(), wxPLTextLayout::CENTER );

		box.height -= m_x1.label->Height() + 2*text_space;
	}

	double yleft_max_label_width = 0, yright_max_label_width = 0;
	for ( size_t pp = 0; pp < NPLOTPOS; pp++ )
	{
		if (m_y1[pp].axis && m_y1[pp].axis->IsShown() && m_y1[pp].axis->IsLabelVisible() && !m_y1[pp].axis->GetLabel().IsEmpty() )
		{
			if ( m_y1[pp].label == 0 )
				m_y1[pp].label = new wxPLTextLayout( dc, m_y1[pp].axis->GetLabel(), wxPLTextLayout::CENTER );

			if ( m_y1[pp].label->Height() > yleft_max_label_width )
				yleft_max_label_width = m_y1[pp].label->Height();
		}

		if (m_y2[pp].axis && m_y2[pp].axis->IsShown() && m_y2[pp].axis->IsLabelVisible() && !m_y2[pp].axis->GetLabel().IsEmpty() )
		{
			if ( m_y2[pp].label == 0 )
				m_y2[pp].label = new wxPLTextLayout( dc, m_y2[pp].axis->GetLabel(), wxPLTextLayout::CENTER );

			if ( m_y2[pp].label->Height() > yright_max_label_width )
				yright_max_label_width = m_y2[pp].label->Height();
		}
	}

	box.x += yleft_max_label_width + 2*text_space;
	box.width -= yleft_max_label_width + 2*text_space;
	box.width -= yright_max_label_width + 2*text_space;
	
	if ( box.width < 50 || box.height < 50 )
		return; // nothing useful to do at this scale

	// now that we have the labels approximately sized for the various axes
	// and the size of the bounding box is what is left for the graphing area,
	// estimate the space required by the axis tick labels and reduce the 
	// effective box accordingly which defines how much space is left for the
	// actual plot

	// note that the axis_layouts are cached and are only
	// invalidated on a resize even.t, or when an axis is changed

	AXIS_FONT(dc);	
	if ( m_x2.axis != 0 && m_x2.axis->IsShown() )
	{
		if ( m_x2.layout == 0 )
			m_x2.layout = new axis_layout( X_TOP, dc, m_x2.axis, box.x, box.x+box.width );

		if ( m_x2.layout->bounds().x > box.width )
		{   // this adjusts for really wide tick text at the ends of the axis
			double diff = m_x2.layout->bounds().x - box.width;
			double adj = 2*diff/3; // actual adjustment needed is diff/2, but leave a little extra space

			if (box.x + box.width + diff > plotbox.x + plotbox.width )
				box.width -= adj;

			if ( box.x - diff < plotbox.x )
			{
				box.x += adj;
				box.width -= adj;
			}
		}

		box.y += m_x2.layout->bounds().y;
		box.height -= m_x2.layout->bounds().y;
	}

	// create an indicator as to whether or not this is a cartesion plot
	bool is_cartesian = true;
	if (wxPLPolarAngularAxis *pa = dynamic_cast<wxPLPolarAngularAxis *>(m_x1.axis))
		is_cartesian = false;


	if ( m_x1.axis != 0 && m_x1.axis->IsShown() )
	{
		if (is_cartesian) {
			if (m_x1.layout == 0)
				m_x1.layout = new axis_layout(X_BOTTOM, dc, m_x1.axis, box.x, box.x + box.width);
			
			box.height -= m_x1.layout->bounds().y;
		}
		else {
			if (m_x1.layout == 0) {
				if (box.width < box.height)
					m_x1.layout = new axis_layout(X_BOTTOM, dc, m_x1.axis, box.x, box.x + box.width);
				else
					m_x1.layout = new axis_layout(X_BOTTOM, dc, m_x1.axis, box.y, box.y + box.height);
			}

			box.y += m_x1.layout->bounds().y;
			box.height -= m_x1.layout->bounds().y*2;
		}
	}
	
	double yleft_max_axis_width = 0, yright_max_axis_width = 0;
	for ( size_t pp=0;pp<NPLOTPOS;pp++)
	{
		if ( m_y1[pp].axis != 0 && m_y1[pp].axis->IsShown() )
		{
			if (m_y1[pp].layout == 0) {
				if (is_cartesian)
					m_y1[pp].layout = new axis_layout( Y_LEFT, dc, m_y1[pp].axis, box.y+box.height, box.y );
				else {
					if (box.width < box.height)
						m_y1[pp].layout = new axis_layout(Y_LEFT, dc, m_y1[pp].axis, box.x, box.x + box.width);
					else
						m_y1[pp].layout = new axis_layout(Y_LEFT, dc, m_y1[pp].axis, box.y + box.height, box.y);
				}
			}

			if ( m_y1[pp].layout->bounds().x > yleft_max_axis_width )
				yleft_max_axis_width = m_y1[pp].layout->bounds().x;
		}

		if ( m_y2[pp].axis != 0 && m_y2[pp].axis->IsShown() )
		{
			if ( m_y2[pp].layout == 0 )
				m_y2[pp].layout = new axis_layout( Y_RIGHT, dc, m_y2[pp].axis, box.y+box.height, box.y );

			if ( m_y2[pp].layout->bounds().x > yright_max_axis_width )
				yright_max_axis_width = m_y2[pp].layout->bounds().x;
				
		}
	}

	box.x += yleft_max_axis_width;
	box.width -= yleft_max_axis_width;
	box.width -= yright_max_axis_width;
	
	// add extra space if desired for annotations, etc
	box.x += m_spaceLeftTop.x;
	box.y += m_spaceLeftTop.y;
	box.width -= ( m_spaceLeftTop.x + m_spaceRightBottom.x );
	box.height -= ( m_spaceLeftTop.y + m_spaceRightBottom.y );

	if ( box.width < 30 || box.height < 30 )
		return; // nothing useful to do at this scale

	size_t nyaxes = 0; // number of Y axes (plots) that need to be shown
	for ( size_t pp=0;pp<NPLOTPOS;pp++ )
		if ( m_y1[pp].axis || m_y2[pp].axis )
			if ( pp+1 > nyaxes )
				nyaxes = pp+1;

	if ( nyaxes == 0 ) nyaxes = 1;
	const double plot_space = 16;
	double single_plot_height = box.height/nyaxes - (nyaxes-1)*(plot_space/2);
	if ( single_plot_height < 50 ) return;

	// compute box dimensions for each plot/subplot
	// and fill plot area with background color
	dc.Brush(  m_plotAreaColour );
	dc.NoPen();

	double cur_plot_y_start = box.y;
	m_plotRects.clear();
	for ( size_t pp=0;pp<nyaxes; pp++ )
	{
		wxPLRealRect rect( box.x, cur_plot_y_start, box.width, single_plot_height );
		if (is_cartesian) 
			dc.Rect(rect);
		else {
			double radius = (box.width < box.height) ? box.width / 2.0 : box.height / 2.0;
			wxRealPoint cntr(box.x + box.width / 2.0, box.y + box.height / 2.0);
			dc.Circle(cntr, radius);
		}
		m_plotRects.push_back(rect);
		cur_plot_y_start += single_plot_height + plot_space;
	}

	// render grid lines
	if ( m_showCoarseGrid )
	{
		dc.Pen( m_gridColour, 0.5, 
			wxPLOutputDevice::SOLID, wxPLOutputDevice::MITER, wxPLOutputDevice::BUTT );

		if (is_cartesian) DrawGrid( dc, wxPLAxis::TickData::LARGE );
		else DrawPolarGrid(dc, wxPLAxis::TickData::LARGE);
	}

	if ( m_showFineGrid )
	{		
		dc.Pen( m_gridColour, 0.5, 
			wxPLOutputDevice::DOT, wxPLOutputDevice::MITER, wxPLOutputDevice::BUTT );

		if (is_cartesian) DrawGrid( dc, wxPLAxis::TickData::SMALL );
		else DrawPolarGrid(dc, wxPLAxis::TickData::SMALL);
	}
	
	// calculate extents of all plots
	wxPLRealRect plotarea( m_plotRects[0].x, 
				m_plotRects[0].y, 
				m_plotRects[0].width, 
				m_plotRects[0].height*nyaxes + (nyaxes-1)*plot_space );

	// draw annotations that are zorder 'back' (i.e. under the plots)	
	DrawAnnotations( dc, plotarea, wxPLAnnotation::BACK );

	// render plots
	for ( size_t i = 0; i< m_plots.size(); i++ )
	{
		wxPLAxis *xaxis = GetAxis( m_plots[i].xap );
		wxPLAxis *yaxis = GetAxis( m_plots[i].yap, m_plots[i].ppos );
		if ( xaxis == 0 || yaxis == 0 ) continue; // this should never be encountered
		wxPLRealRect &bb = m_plotRects[ m_plots[i].ppos ];
		wxPLAxisDeviceMapping map( 
			xaxis, 
				bb.x, 
				bb.x+bb.width, 
				xaxis == GetXAxis1(),
			yaxis, 
				bb.y+bb.height, 
				bb.y, 
				yaxis == GetYAxis1() );

		dc.SetAntiAliasing( m_plots[i].plot->GetAntiAliasing() );

		dc.Clip( bb.x, bb.y, bb.width, bb.height );
		m_plots[i].plot->Draw( dc, map );
		dc.Unclip();
	}

	dc.SetAntiAliasing( false );

	// draw some axes
	AXIS_FONT(dc);
	dc.TextColour( m_axisColour );
	dc.Pen( m_axisColour, 0.5 );	
	if ( m_x2.axis && m_x2.axis->IsShown() )
		m_x2.layout->render( dc, m_plotRects[0].y, m_x2.axis, 
			box.x, box.x+box.width, 
			-1 /*m_x1.axis == 0 ? m_plotRects[nyaxes-1].y+m_plotRects[nyaxes-1].height : -1 */ );
	
	// set up some polar plot values
	wxPLRealRect rect1 = m_plotRects[0];
	double pp_radius = (rect1.width < rect1.height) ? rect1.width / 2.0 : rect1.height / 2.0;
	wxRealPoint pp_center(rect1.x + rect1.width / 2.0, rect1.y + rect1.height / 2.0);

	// render y axes
	for ( size_t pp=0;pp<nyaxes; pp++ )
	{
		if (m_y1[pp].axis != 0 && m_y1[pp].axis->IsShown() ) {
			if (is_cartesian)
				m_y1[pp].layout->render( dc, box.x, m_y1[pp].axis, 
					m_plotRects[pp].y + m_plotRects[pp].height,  m_plotRects[pp].y,
					-1 /*m_y2[pp].axis == 0 ? box.x+box.width : -1*/ );
			else
				m_y1[pp].layout->render( dc, pp_center.x, m_y1[pp].axis, 
					pp_center.y,  pp_center.y-pp_radius,
					-1 );
		}
		
		if ( m_y2[pp].axis != 0 && m_y2[pp].axis->IsShown() )
			m_y2[pp].layout->render( dc, box.x+box.width, m_y2[pp].axis, 
				m_plotRects[pp].y + m_plotRects[pp].height,  m_plotRects[pp].y,
				-1 /* m_y1[pp].axis == 0 ? box.x : -1 */ );
	}

	// render x1 axis (or angular axis on polar plots)
	if (m_x1.axis && m_x1.axis->IsShown() ) {
		if (is_cartesian)
			m_x1.layout->render(dc, m_plotRects[nyaxes - 1].y + m_plotRects[nyaxes - 1].height, m_x1.axis,
				box.x, box.x + box.width,
				-1 /*m_x2.axis == 0 ? m_plotRects[0].y : -1*/);
		else
			m_x1.layout->render(dc, pp_radius, m_x1.axis, pp_center.x, pp_center.y, -1);
	}

	if ( m_borderWidth > 0 )
	{
		dc.Pen( m_axisColour, m_borderWidth );
		// draw boundaries around plots
		if (is_cartesian) {
			for (size_t pp = 0; pp < nyaxes; pp++)
			{
				dc.Line(m_plotRects[pp].x, m_plotRects[pp].y, m_plotRects[pp].x + m_plotRects[pp].width, m_plotRects[pp].y);
				dc.Line(m_plotRects[pp].x, m_plotRects[pp].y, m_plotRects[pp].x, m_plotRects[pp].y + m_plotRects[pp].height);
				dc.Line(m_plotRects[pp].x, m_plotRects[pp].y + m_plotRects[pp].height, m_plotRects[pp].x + m_plotRects[pp].width, m_plotRects[pp].y + m_plotRects[pp].height);
				dc.Line(m_plotRects[pp].x + m_plotRects[pp].width, m_plotRects[pp].y, m_plotRects[pp].x + m_plotRects[pp].width, m_plotRects[pp].y + m_plotRects[pp].height);
			}
		}
		else {
			dc.Line(pp_center.x-pp_radius,pp_center.y,pp_center.x+pp_radius,pp_center.y);
			dc.Line(pp_center.x, pp_center.y + pp_radius, pp_center.x, pp_center.y - pp_radius);

			dc.NoBrush();
			dc.Circle(pp_center, pp_radius);
		}
	}


	// draw axis labels
	AXIS_FONT(dc);
	if (m_x1.axis && m_x1.axis->IsLabelVisible() && !m_x1.axis->GetLabel().IsEmpty()) {
		if (is_cartesian)
			m_x1.label->Render(dc, box.x + box.width / 2 - m_x1.label->Width() / 2, plotbox.y + plotbox.height - m_x1.label->Height() - text_space, 0, false);
		else {
			double dist = sqrt(pow(pp_radius, 2) / 2.0);
			m_x1.label->Render(dc, pp_center.x + dist, pp_center.y - dist - m_x1.layout->bounds().y - m_x1.label->Height() - text_space, 0, false);
		}
	}
	
	if ( m_x2.axis && m_x2.axis->IsLabelVisible() && !m_x2.axis->GetLabel().IsEmpty() )
		m_x2.label->Render( dc, box.x+box.width/2-m_x2.label->Width()/2, plotbox.y, 0, false );

	for ( size_t pp = 0; pp<nyaxes; pp++ )
	{
		if (m_y1[pp].axis && m_y1[pp].axis->IsLabelVisible() && !m_y1[pp].axis->GetLabel().IsEmpty()) {
			if (is_cartesian) {
				m_y1[pp].label->Render(dc, plotbox.x + yleft_max_label_width - m_y1[pp].label->Height(),
					m_plotRects[pp].y + m_plotRects[pp].height / 2 + m_y1[pp].label->Width() / 2, 90, false);
			}
			else {
				m_y1[pp].label->Render(dc, pp_center.x, pp_center.y - pp_radius + m_y1[pp].label->Width()+text_space, 90, false);
			}
		}

		if (m_y2[pp].axis && m_y2[pp].axis->IsLabelVisible() && !m_y2[pp].axis->GetLabel().IsEmpty() )
			m_y2[pp].label->Render( dc, plotbox.x+plotbox.width - yright_max_label_width + m_y2[pp].label->Height(),
				m_plotRects[pp].y + m_plotRects[pp].height/2 - m_y2[pp].label->Width()/2, -90, false );
	}

	LEGEND_FONT(dc);
	
	DrawLegend( dc, (m_legendPos==FLOATING||legend_bottom||legend_right) ? geom : plotarea  );

	// draw annotations on the top
	DrawAnnotations( dc, plotarea, wxPLAnnotation::FRONT );
}


void wxPLPlot::DrawAnnotations( wxPLOutputDevice &dc, const wxPLRealRect &plotarea, wxPLAnnotation::ZOrder zo )
{
	if ( m_annotations.size() > 0 )
	{
		NORMAL_FONT(dc);
		for( size_t i=0;i<m_annotations.size();i++ )
		{
			if ( m_annotations[i].zorder != zo ) continue;

			wxPLAxis *xax = 0; double xmin=0, xmax = 0;
			wxPLAxis *yax = 0; double ymin=0, ymax = 0;
			if ( m_annotations[i].posm == wxPLAnnotation::AXIS )
			{
				xax = GetAxis( m_annotations[i].xap );
				yax = GetAxis( m_annotations[i].yap );
				
				if ( m_annotations[i].ppos < m_plotRects.size() )
				{
					wxPLRealRect &bb = m_plotRects[ m_annotations[i].ppos ];
					xmin = bb.x;
					xmax = bb.x+bb.width;
					ymin = bb.y+bb.height;
					ymax = bb.y;
				}
			}
			wxPLDefaultAnnotationMapper mmap( m_annotations[i].posm, plotarea, 
				xax, xmin, xmax,
				yax, ymin, ymax );
			
			m_annotations[i].ann->Draw( dc, mmap );
		}
	}
}

void wxPLPlot::DrawGrid( wxPLOutputDevice &dc, wxPLAxis::TickData::TickSize size )
{
	if ( m_plotRects.size() < 1 ) return;

	dc.SetAntiAliasing( false );

	axis_data *xgrid_axis = 0;
	if ( m_x1.axis != 0 ) xgrid_axis = &m_x1;
	else if (m_x2.axis != 0 ) xgrid_axis = &m_x2;

	if ( xgrid_axis != 0 )
	{
		std::vector<double> ticks = xgrid_axis->layout->ticks( size );
		for ( size_t i=0;i<ticks.size();i++ )
		{
			double xpos = xgrid_axis->axis->WorldToPhysical( ticks[i], m_plotRects[0].x, m_plotRects[0].x+m_plotRects[0].width );

			for ( size_t pp = 0; pp < m_plotRects.size(); pp++ )
			{
				dc.Line( xpos, m_plotRects[pp].y, 
					xpos, m_plotRects[pp].y + m_plotRects[pp].height );
			}				
		}
	}

	for (size_t pp = 0; pp < m_plotRects.size(); pp++ )
	{
		axis_data *ygrid_axis = 0;
		if (m_y1[pp].axis != 0 ) ygrid_axis = &m_y1[pp];
		else if ( m_y2[pp].axis != 0 ) ygrid_axis = &m_y2[pp];

		if ( ygrid_axis != 0 && ygrid_axis->layout != 0 )
		{
			std::vector<double> ticks = ygrid_axis->layout->ticks( size );
			for ( size_t j=0;j<ticks.size();j++)
			{
				double ypos = ygrid_axis->axis->WorldToPhysical( ticks[j], m_plotRects[pp].y+m_plotRects[pp].height, m_plotRects[pp].y );

				dc.Line( m_plotRects[0].x, ypos, 
					m_plotRects[0].x + m_plotRects[0].width, ypos );
			}
		}
	}	
}

void wxPLPlot::DrawPolarGrid( wxPLOutputDevice &dc, wxPLAxis::TickData::TickSize size)
{
	if (m_plotRects.size() < 1) return;

	dc.NoBrush();
	wxRealPoint cntr(m_plotRects[0].x + m_plotRects[0].width / 2.0, m_plotRects[0].y + m_plotRects[0].height / 2.0);
	double max_radius = (m_plotRects[0].width < m_plotRects[0].height) ? m_plotRects[0].width / 2.0 : m_plotRects[0].height / 2.0;

	// circles from center
	axis_data *radial_grid = 0;
	if (m_y1[0].axis != 0) radial_grid = &m_y1[0];

	if (radial_grid != 0)
	{
		std::vector<double> ticks = radial_grid->layout->ticks(size);
		for (size_t j = 0; j<ticks.size(); j++)
		{
			double radius = radial_grid->axis->WorldToPhysical(ticks[j], 0, max_radius);
			dc.Circle(cntr, radius);
		}
	}

	// rays from center out to edge of plot
	axis_data *angular_grid = 0;
	if (m_x1.axis != 0) angular_grid = &m_x1;

	if (angular_grid != 0) {
		if (wxPLPolarAngularAxis *pa = dynamic_cast<wxPLPolarAngularAxis *>(m_x1.axis)) {
			std::vector<double> ticks = angular_grid->layout->ticks(size);
			for (size_t i = 0; i<ticks.size(); i++)
			{
				double angle = pa->AngleInRadians(ticks[i]);
				wxRealPoint pt(max_radius*cos(angle), max_radius*sin(angle));
				dc.Line(cntr, cntr + pt);
			}
		}
	}

}



wxPLPlot::legend_item::legend_item( wxPLOutputDevice &dc, wxPLPlottable *p )
{
	plot = p;
	text = new wxPLTextLayout( dc, p->GetLabel() );
	width = legend_item_box.x + text_space + text->Width();
	height = text->Height();
	if ( height < legend_item_box.y ) height = legend_item_box.y;
}

wxPLPlot::legend_item::~legend_item()
{
	if ( text ) delete text;
}

void wxPLPlot::CalculateLegendLayout( wxPLOutputDevice &dc )
{
	if ( !m_showLegend ) return;

	if ( m_legendInvalidated )
	{
		LEGEND_FONT(dc);

		// rebuild legend items to show in plot
		for ( size_t i=0;i<m_legendItems.size(); i++ )
			delete m_legendItems[i];
		m_legendItems.clear();

		for ( size_t i=0; i<m_plots.size(); i++ )
		{
			if ( !m_plots[i].plot->IsShownInLegend() 
				|| m_plots[i].plot->GetLabel().IsEmpty() ) continue;

			m_legendItems.push_back( new legend_item( dc, m_plots[i].plot ) );
		}
	
		m_legendRect.width = 0;
		m_legendRect.height = 0;

		if ( m_legendItems.size() == 0 )
			return;
		
		// realculate legend bounds based on text layouts and item box size
		if ( m_legendPos == BOTTOM )
		{
			m_legendRect.width = text_space;
			for ( size_t i=0; i<m_legendItems.size(); i++ )
			{
				if ( m_legendItems[i]->height > m_legendRect.height )
					m_legendRect.height = m_legendItems[i]->height;

				m_legendRect.width += m_legendItems[i]->width + text_space + text_space;
			}
			
			// top & bottom padding
			m_legendRect.height += 2*text_space;
		}
		else
		{
			m_legendRect.height = text_space;
			for ( size_t i=0; i<m_legendItems.size(); i++ )
			{
				if ( m_legendItems[i]->width > m_legendRect.width )
					m_legendRect.width = m_legendItems[i]->width;
					
				m_legendRect.height += m_legendItems[i]->height + text_space;
			}

			// left & right padding
			m_legendRect.width += 2*text_space;
		}
		
		m_legendInvalidated = false;
	}
}

void wxPLPlot::DrawLegend( wxPLOutputDevice &dc, const wxPLRealRect& geom )
{
	if ( !m_showLegend )
		return;

	int layout = wxVERTICAL;
	double max_item_height = 0; // for vertical center alignment on horizontal layout
	
	// offset by LegendXYPercents
	if ( m_legendPos == FLOATING )
	{
		if (m_legendPosPercent.x < -10) m_legendPosPercent.x = -10;
		if (m_legendPosPercent.x > 90) m_legendPosPercent.x = 90;
		if (m_legendPosPercent.y < -10) m_legendPosPercent.y = -10;
		if (m_legendPosPercent.y > 90) m_legendPosPercent.y = 90;

		m_legendRect.x = (int)(geom.x + m_legendPosPercent.x / 100.0 * geom.width);
		m_legendRect.y = (int)(geom.y + m_legendPosPercent.y / 100.0 * geom.height);
	}
	else
	{
		switch ( m_legendPos )
		{
		case NORTHWEST:
			m_legendRect.x = geom.x + text_space*2;
			m_legendRect.y = geom.y + text_space*2;
			break;
		case SOUTHWEST:
			m_legendRect.x = geom.x + text_space*2;
			m_legendRect.y = geom.y + geom.height - m_legendRect.height - text_space*2;
			break;
		case NORTHEAST:
			m_legendRect.x = geom.x + geom.width - m_legendRect.width - text_space*2;
			m_legendRect.y = geom.y + text_space*2;
			break;
		case SOUTHEAST:
			m_legendRect.x = geom.x + geom.width - m_legendRect.width - text_space*2;
			m_legendRect.y = geom.y + geom.height - m_legendRect.height - text_space*2;
			break;
		case NORTH:
			m_legendRect.x = geom.x + geom.width/2 - m_legendRect.width/2;
			m_legendRect.y = geom.y + text_space*2;
			break;
		case SOUTH:
			m_legendRect.x = geom.x + geom.width/2 - m_legendRect.width/2;
			m_legendRect.y = geom.y + geom.height - m_legendRect.height - text_space*2;
			break;
		case EAST:
			m_legendRect.x = geom.x + geom.width - m_legendRect.width - text_space*2;
			m_legendRect.y = geom.y + geom.height/2 - m_legendRect.height/2;
			break;
		case WEST:
			m_legendRect.x = geom.x + text_space*2;
			m_legendRect.y = geom.y + geom.height/2 - m_legendRect.height/2;
			break;
		case BOTTOM:
			m_legendRect.x = geom.x + text_space;
			m_legendRect.y = geom.y + geom.height - m_legendRect.height;
			layout = wxHORIZONTAL;			
			for( size_t i=0;i<m_legendItems.size();i++ )
				if ( m_legendItems[i]->height > max_item_height )
					max_item_height = m_legendItems[i]->height;			
			break;
		case RIGHT:
			m_legendRect.y = geom.y + text_space;
			m_legendRect.x = geom.x + geom.width - m_legendRect.width;
			break;
		}
	}
	
	dc.SetAntiAliasing( false );
	
	if ( m_legendPos != BOTTOM && m_legendPos != RIGHT )
	{
		dc.Brush( *wxWHITE );
		
		if ( m_showLegendBorder ) dc.Pen( *wxLIGHT_GREY, 0.5 );
		else dc.NoPen();

		dc.Rect( m_legendRect );
	}
	
	double x = m_legendRect.x + text_space;
	double y = m_legendRect.y + text_space;

	for ( size_t i = 0; i < m_legendItems.size(); i++ )
	{
		legend_item &li = *m_legendItems[ m_reverseLegend ? m_legendItems.size() - i - 1 : i ];
				
		double yoff_text = li.height/2 - li.text->Height()/2;
		if ( layout == wxHORIZONTAL )
			yoff_text = (max_item_height - li.text->Height())/2;

		li.text->Render( dc, 
			x + legend_item_box.x + text_space, 
			y + yoff_text );

		double yoff_box = li.height/2 - legend_item_box.y/2;
		if ( layout == wxHORIZONTAL )
			yoff_box = max_item_height/2 - legend_item_box.y/2;

		dc.SetAntiAliasing( li.plot->GetAntiAliasing() );

		li.plot->DrawInLegend( dc, wxPLRealRect( x, y + yoff_box, 
			legend_item_box.x, legend_item_box.y ) );

		if ( layout == wxHORIZONTAL ) x += li.width + text_space + text_space;
		else y += li.height + text_space;
	}
	
	dc.SetAntiAliasing( false );

	//dc.SetPen( *wxBLACK_PEN );
	//dc.SetBrush( *wxTRANSPARENT_BRUSH );
	//dc.DrawRectangle( m_legendRect );
}

void wxPLPlot::Invalidate()
{
	if ( m_titleLayout != 0 )
	{
		delete m_titleLayout;
		m_titleLayout = 0;
	}

	m_legendInvalidated = true;
	m_x1.invalidate();
	m_x2.invalidate();
	for ( size_t i=0;i<NPLOTPOS;i++ )
	{
		m_y1[i].invalidate();
		m_y2[i].invalidate();
	}
}

void wxPLPlot::DeleteAxes()
{
	SetXAxis1( 0 );
	SetXAxis2( 0 );
	for ( size_t pp = 0; pp<NPLOTPOS; pp++ )
	{
		SetYAxis1( 0, (wxPLPlot::PlotPos)pp );
		SetYAxis2( 0, (wxPLPlot::PlotPos)pp );
	}

	Invalidate();
}

void wxPLPlot::RescaleAxes()
{
	//This does not set axes to null, or change anything other than their bounds.
	bool xAxis1Set = false, xAxis2Set = false;
	std::vector<bool> yAxis1Set(NPLOTPOS, false), yAxis2Set(NPLOTPOS, false);

	for (size_t i = 0; i < m_plots.size(); ++i)
    {
        wxPLPlottable *p = m_plots[i].plot;
        wxPLPlot::AxisPos xap = m_plots[i].xap;
		wxPLPlot::AxisPos yap = m_plots[i].yap;
		wxPLPlot::PlotPos ppos = m_plots[i].ppos;

        if (xap == X_BOTTOM)
        {
            if ( !xAxis1Set && GetXAxis1() )
            {
				double xMin, xMax;
				p->GetMinMax(&xMin, &xMax, NULL, NULL);
				GetXAxis1()->SetWorld(xMin, xMax);
            }
            else
            {
				wxPLAxis *pnew = p->SuggestXAxis();
				if (pnew)
				{
					if (GetXAxis1()) GetXAxis1()->ExtendBound( pnew );

					delete pnew;
				}
            }
			xAxis1Set = true;
        }

        if (xap == X_TOP)
        {
            if (!xAxis2Set && GetXAxis2())
            {
				double xMin, xMax;
				p->GetMinMax(&xMin, &xMax, NULL, NULL);
				GetXAxis2()->SetWorld(xMin, xMax);              
            }
            else
            {
				wxPLAxis *pnew = p->SuggestXAxis();
				if (pnew)
				{
					if (GetXAxis2()) GetXAxis2()->ExtendBound( pnew );
					delete pnew;
				}
            }
			xAxis2Set = true;
        }

        if (yap == Y_LEFT)
        {
			if (!yAxis1Set[ppos] && GetYAxis1(ppos))
            {
				double yMin, yMax;
				p->GetMinMax(NULL, NULL, &yMin, &yMax);
				GetYAxis1(ppos)->SetWorld(yMin, yMax);
            }
            else
            {
				wxPLAxis *pnew = p->SuggestYAxis();
				if (pnew)
				{
					if (GetYAxis1( ppos ) )
						GetYAxis1(ppos)->ExtendBound(pnew);
					delete pnew;
				}
            }
			yAxis1Set[ppos] = true;
        }

        if (yap == Y_RIGHT)
        {
			if (!yAxis2Set[ppos] && GetYAxis2( ppos ) )
            {
				double yMin, yMax;
				p->GetMinMax(NULL, NULL, &yMin, &yMax);
				GetYAxis2(ppos)->SetWorld(yMin, yMax);
            }
            else
            {
				wxPLAxis *pnew = p->SuggestYAxis();
				if (pnew)
				{
					if (GetYAxis2(ppos)) GetYAxis2(ppos)->ExtendBound(pnew);
					delete pnew;
				}
            }
			yAxis2Set[ppos] = true;
        }
    }
}

void wxPLPlot::UpdateAxes( bool recalc_all )
{
	int position = 0;

    // if we're not recalculating axes using all iplots then set
    // position to last one in list.
    if ( !recalc_all )
    {
        position =  m_plots.size()-1;
        if (position < 0) 
			position = 0;
    }

    if ( recalc_all )
    {
		SetXAxis1( NULL );
		SetXAxis2( NULL );
		
		for (int i=0; i<NPLOTPOS; i++)
		{
			SetYAxis1( NULL, (PlotPos)i );
			SetYAxis2( NULL, (PlotPos)i );
		}
    }

    for (int i = position; i < m_plots.size(); i++ )
    {
        wxPLPlottable *p = m_plots[i].plot;
        AxisPos xap = m_plots[i].xap;
		AxisPos yap = m_plots[i].yap;
		PlotPos ppos = m_plots[i].ppos;

        if( xap == X_BOTTOM )
        {
            if ( GetXAxis1() == NULL )
            {
                SetXAxis1( p->SuggestXAxis() );
            }
            else
            {
				if ( wxPLAxis *pnew = p->SuggestXAxis() )
				{
					GetXAxis1()->ExtendBound( pnew );
					delete pnew;
				}
            }
        }

        if( xap == X_TOP )
        {
            if( GetXAxis2() == NULL )
            {
                SetXAxis2( p->SuggestXAxis() );                
            }
            else
            {
				if ( wxPLAxis *pnew = p->SuggestXAxis() )
				{
					GetXAxis2()->ExtendBound( pnew );
					delete pnew;
				}
            }
        }

        if( yap == Y_LEFT )
        {
			if ( GetYAxis1( ppos ) == NULL )
            {
                SetYAxis1( p->SuggestYAxis(), ppos );
            }
            else
            {
				if ( wxPLAxis *pnew = p->SuggestYAxis() )
				{
					GetYAxis1( ppos )->ExtendBound( pnew );
					delete pnew;
				}
            }
        }

        if ( yap == Y_RIGHT )
        {
            if ( GetYAxis2( ppos ) == NULL )
            {
                SetYAxis2(p->SuggestYAxis(), ppos);
            }
            else
            {
				if ( wxPLAxis *pnew = p->SuggestYAxis() )
				{
					GetYAxis2( ppos )->ExtendBound(pnew);
					delete pnew;
				}
            }
			GetYAxis2(ppos)->ShowTickText( true );
        }
    }
}

static wxPathList s_pdfFontDirs;

bool wxPLPlot::AddPdfFontDir( const wxString &path )
{
	wxFileName fn(path);
	fn.Normalize();
	wxString folder( fn.GetFullPath() );
	if (s_pdfFontDirs.Index( folder ) == wxNOT_FOUND )
	{
		if ( wxDirExists( folder ) )
		{
			s_pdfFontDirs.Add( folder );
			return true;
		}
	}

	return false;
}

wxString wxPLPlot::LocatePdfFontDataFile( const wxString &face )
{
	wxString fd( s_pdfFontDirs.FindAbsoluteValidPath( face + ".otf" ) );
	if ( fd.IsEmpty() )
		fd = s_pdfFontDirs.FindAbsoluteValidPath( face + ".ttf" );

	return fd;
}

wxArrayString wxPLPlot::ListAvailablePdfFonts()
{
	wxArrayString faces;
	for( size_t i=0;i<s_pdfFontDirs.size();i++ )
	{
		wxArrayString files;
		wxDir::GetAllFiles( s_pdfFontDirs[i], &files, wxEmptyString, wxDIR_FILES );
		for( size_t k=0;k<files.size();k++ )
		{
			wxFileName file( files[k] );
			wxString ext( file.GetExt().Lower() );
			if ( ext == "ttf" || ext == "otf" )
				faces.Add( file.GetName() );
		}
	}
	return faces;
}

static wxString s_pdfDefaultFontFace("Helvetica");

static void EnsureStandardPdfFontPaths()
{
	// make sure we have the standard locations for pdf font data
	wxPLPlot::AddPdfFontDir( wxPathOnly(wxStandardPaths::Get().GetExecutablePath() ) + "/pdffonts" );
}

static bool IsBuiltinPdfFont( const wxString &face )
{
	return face == "Helvetica"
		|| face == "Courier"
		|| face == "Times"
		|| face == "Arial"
		|| face == "ZapfDingbats"
		|| face == "Symbol";
}

bool wxPLPlot::SetPdfDefaultFont( const wxString &face )
{
	EnsureStandardPdfFontPaths();
	
	if ( face.IsEmpty() ) return true; // no changes to the face

	if (  !IsBuiltinPdfFont(face) )
	{
		wxString fontfile = LocatePdfFontDataFile( face );
		if ( fontfile.IsEmpty() )
			return false;
		
		wxPdfFontManager *fmng = wxPdfFontManager::GetFontManager();
		
		wxPdfFont font( fmng->GetFont( s_pdfDefaultFontFace ) );
		if ( !font.IsValid() )
			font = fmng->RegisterFont( fontfile, face );

		if ( !font.IsValid() )
			return false;
	}

	s_pdfDefaultFontFace = face;

	return true;
}

bool wxPLPlot::RenderPdf( const wxString &file, double width, double height, double fontpoints )
{
	if ( fontpoints < 0 )
		fontpoints = m_textSizePoints;

	EnsureStandardPdfFontPaths();

	wxPdfDocument doc( wxPORTRAIT, "pt", wxPAPER_A5 );
	doc.AddPage( wxPORTRAIT, width, height );

	if ( !IsBuiltinPdfFont( s_pdfDefaultFontFace ) )
	{
		wxString datafile( LocatePdfFontDataFile( s_pdfDefaultFontFace ) );
		if ( !doc.AddFont( s_pdfDefaultFontFace, wxEmptyString, datafile ) 
			|| !doc.SetFont( s_pdfDefaultFontFace, wxPDF_FONTSTYLE_REGULAR, fontpoints ) )
			doc.SetFont( "Helvetica", wxPDF_FONTSTYLE_REGULAR, fontpoints );
	}
	else
		doc.SetFont( s_pdfDefaultFontFace, wxPDF_FONTSTYLE_REGULAR, fontpoints );
	
	doc.SetFontSize( fontpoints );

	Invalidate();
	
	wxPLPdfOutputDevice dc( doc, fontpoints );
	Render( dc, wxPLRealRect( 0, 0, width, height ) );
		
	Invalidate();
		
	const wxMemoryOutputStream &data = doc.CloseAndGetBuffer();
	wxFileOutputStream fp( file );
	if (!fp.IsOk()) return false;

	wxMemoryInputStream tmpis( data );
	fp.Write( tmpis );
	return fp.Close();
}

/*
wxPostScriptDC dc(wxT("output.ps"), true, wxGetApp().GetTopWindow());

if (dc.Ok())
{
    // Tell it where to find the AFM files
    dc.GetPrintData().SetFontMetricPath(wxGetApp().GetFontPath());

    // Set the resolution in points per inch (the default is 720)
    dc.SetResolution(1440);

    // Draw on the device context
    ...
}
*/


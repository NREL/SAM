
#include <algorithm>
#include <math.h>
#include <wx/dc.h>
#include "wex/plot/plbarplot.h"


wxPLBarPlotBase::wxPLBarPlotBase()
{
	Init();
}

wxPLBarPlotBase::wxPLBarPlotBase( const std::vector<wxRealPoint> &data, double baseline,
	const wxString &label, 
	const wxColour &col )
	: wxPLPlottable( label )
{
	Init();
	m_baseline = baseline;
	m_colour = col;
	m_data = data;
}

wxPLBarPlotBase::~wxPLBarPlotBase()
{
	// nothing to do
}

void wxPLBarPlotBase::Init()
{
	m_baseline = 0;
	m_antiAliasing = false;
	m_colour = *wxLIGHT_GREY;
	m_thickness = wxPL_BAR_AUTOSIZE;
}
	
wxRealPoint wxPLBarPlotBase::At( size_t i ) const
{
	return m_data[i];
}

size_t wxPLBarPlotBase::Len() const
{
	return m_data.size();
}

void wxPLBarPlotBase::DrawInLegend( wxPLOutputDevice &dc, const wxPLRealRect &rct)
{
	dc.NoPen();
	dc.Brush( m_colour );
	dc.Rect( rct );
}

////////// wxPLBarPlot ///////////


wxPLBarPlot::wxPLBarPlot()
	: wxPLBarPlotBase()
{
	m_stackedOn = 0;
}

wxPLBarPlot::wxPLBarPlot( const std::vector<wxRealPoint> &data, double baseline_y,
	const wxString &label, 
	const wxColour &col )
	: wxPLBarPlotBase( data, baseline_y, label, col )
{
	m_stackedOn = 0;
}

wxPLBarPlot::~wxPLBarPlot()
{
	// nothing to do
}

wxPLAxis *wxPLBarPlot::SuggestYAxis() const
{
	if ( Len() < 1 ) return new wxPLLinearAxis( 0, 1 );

	double ymin = CalcYPos( At(0).x );
	double ymax = ymin;

	for (size_t i=1; i<Len(); i++)
	{
		double y = CalcYPos( At(i).x );
		if ( y < ymin ) ymin = y;
		if ( y > ymax ) ymax = y;
	}

	if ( ymin > m_baseline ) ymin = m_baseline;
	if ( ymax < m_baseline ) ymax = m_baseline;

	return new wxPLLinearAxis( ymin, ymax );
}

double wxPLBarPlot::CalcYPos(double x) const
{
	double y_start = m_baseline;

	if (m_stackedOn && m_stackedOn != this)
		y_start += m_stackedOn->CalcYPos(x);

	for (size_t i=0; i<Len(); i++)
	{
		if (At(i).x == x)
		{
			y_start += At(i).y;
			break;
		}
	}

	return y_start;
}

double wxPLBarPlot::CalcXPos(double x, const wxPLDeviceMapping &map, double dispwidth)
{
	if (m_stackedOn && m_stackedOn != this)
		return m_stackedOn->CalcXPos(x, map, dispwidth);

	double x_start = map.ToDevice( x, 0 ).x;

	size_t g, idx;
	std::vector<wxPLBarPlot*>::iterator iit = std::find( m_group.begin(), m_group.end(), this );
	if ( m_group.size() > 0 
		&&  iit != m_group.end() )
	{
		idx = iit-m_group.begin();

		std::vector<double> barwidths;
		for (g=0;g<m_group.size();g++)
			barwidths.push_back( m_group[g]->CalcDispBarWidth(map) );

		double group_width = 0;
		for (g=0;g<m_group.size();g++)
			group_width += barwidths[g];

		double bar_start = x_start - group_width/2;
		for (g=0;g<idx;g++)
			bar_start += barwidths[g];

		x_start = bar_start + dispwidth/2;
	}

	return x_start;
}

double wxPLBarPlot::CalcDispBarWidth( const wxPLDeviceMapping &map )
{
	if ( m_thickness <= 0 )
	{
		wxRealPoint pos, size;
		map.GetDeviceExtents( &pos, &size );
		double xmin = map.GetWorldMinimum().x;
		double xmax = map.GetWorldMaximum().x;

		double cxmin=1e99, cxmax=-1e99;
		int bars_in_view = 0;
		for ( size_t i=0;i<Len();i++ )
		{
			double x = At(i).x;
			if ( x >= xmin && x <= xmax )
			{
				bars_in_view++;

				wxRealPoint C( map.ToDevice( At(i) ) );
				if ( C.x < cxmin ) cxmin = C.x;
				if ( C.x > cxmax ) cxmax = C.x;
			}
		}

		if ( bars_in_view == 0 || cxmin == cxmax )
			return 10; // default point thickness

		if ( m_group.size() > 0 ) bars_in_view *= m_group.size();
		
		int w = (int)( 0.75*( (cxmax-cxmin) / ((double)( bars_in_view )) ) );
		if ( w < 2 ) w = 2;
		return (double)w;
	}
	else return m_thickness;
}

void wxPLBarPlot::Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map )
{
	if ( Len() == 0 ) return;

	double dispbar_w = CalcDispBarWidth( map );
	
	dc.SetAntiAliasing( false );
	dc.NoPen();
	dc.Brush( m_colour );
	
	for (size_t i=0; i<Len(); i++)
	{
		wxRealPoint pt = At(i);
		double pbottom=0, ptop=0;
		double y_start=m_baseline;
		double x_start = CalcXPos( pt.x, map, dispbar_w );

		if (m_stackedOn != NULL && m_stackedOn != this)
		{
			y_start = m_stackedOn->CalcYPos( pt.x );	
			x_start = m_stackedOn->CalcXPos( pt.x, map, dispbar_w);
		}

		wxPLRealRect prct;
		prct.x = x_start - dispbar_w/2;
		prct.width = dispbar_w;

		pbottom = map.ToDevice( 0, y_start ).y;
		ptop = map.ToDevice( 0, pt.y + y_start ).y;

		prct.y = pbottom < ptop ? pbottom : ptop;
		prct.height = fabs( pbottom-ptop );

		dc.Rect(prct.x, prct.y, prct.width, prct.height);
	}
}


/////////// wxPLHBarPlot ////////////

wxPLHBarPlot::wxPLHBarPlot() : wxPLBarPlotBase() { /* nothing to do */ }

wxPLHBarPlot::wxPLHBarPlot( const std::vector<wxRealPoint> &data, double baseline_x,
	const wxString &label,
	const wxColour &col)
	: wxPLBarPlotBase( data, baseline_x, label, col )
{
	m_stackedOn = 0;
}

wxPLHBarPlot:: ~wxPLHBarPlot()
{
	m_stackedOn = 0;
}

void wxPLHBarPlot::Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map )
{
	if( Len() == 0 ) return;

	double bar_width = CalcDispBarWidth( map );
	dc.NoPen();
	dc.Brush( m_colour );
	for (size_t i=0; i<Len(); i++)
	{
		wxRealPoint pt(  At(i) );
		double pleft=0, pright=0;
		double x_start=m_baseline;
			
		if ( m_stackedOn != NULL && m_stackedOn != this )
			x_start = m_stackedOn->CalcXPos( pt.y );	

		wxPLRealRect prct;

		pleft = map.ToDevice( x_start, 0 ).x;
		pright = map.ToDevice( pt.x, 0 ).x;

		if (pleft < pright)
		{
			prct.x = pleft;
			prct.width = pright-pleft;
		}
		else
		{
			prct.x = pright;
			prct.width = pleft-pright;
		}
						
		prct.y = map.ToDevice( 0, pt.y ).y - bar_width/2;
		prct.height = bar_width;
		prct.width = fabs(pleft - pright);
		if ( prct.width > 0 && prct.height > 0 )
			dc.Rect(prct.x, prct.y, prct.width, prct.height);
	}
	
	/*
	wxRealPoint start,end;
	end.x = start.x = map.ToDevice( m_baseline, 0 ).x;
	
	wxRealPoint pos, size;
	map.GetDeviceExtents( &pos, &size );

	// don't automatically draw a vertical bar to show baseline
	// now can use annotation feature to do this.
	start.y = pos.y+1;
	end.y = start.y + size.y-1;	
	dc.Pen( *wxBLACK, 1 );
	dc.Line(start.x, start.y, end.x, end.y);
	*/
}

	
double wxPLHBarPlot::CalcXPos(double y) const
{
	double x_start = m_baseline;

	if (m_stackedOn && m_stackedOn != this)
		x_start += m_stackedOn->CalcXPos(y);

	for (size_t i=0; i<Len(); i++)
	{
		if (At(i).y == y)
		{
			x_start += At(i).x;
			break;
		}
	}

	return x_start;

}

double wxPLHBarPlot::CalcDispBarWidth( const wxPLDeviceMapping &map )
{
	if ( m_thickness <= 0.0 )
	{
		wxRealPoint pos, size;
		map.GetDeviceExtents( &pos, &size );
		double ymin = map.GetWorldMinimum().y;
		double ymax = map.GetWorldMaximum().y;

		int bars_in_view = 0;
		for ( size_t i=0;i<Len();i++ )
		{
			double y = At(i).y;
			if ( y >= ymin && y <= ymax )
				bars_in_view++;
		}


		return ( ((double)size.y) / ((double)( bars_in_view + 4 )) );
	}
	else return m_thickness;
}

wxPLAxis *wxPLHBarPlot::SuggestXAxis() const
{
	if ( Len() < 1 ) return new wxPLLinearAxis( 0, 1 );

	double xmin = CalcXPos( At(0).y );
	double xmax = xmin;

	for (size_t i=1; i<Len(); i++)
	{
		double x = CalcXPos( At(i).y );
		if ( x < xmin ) xmin = x;
		if ( x > xmax ) xmax = x;
	}

	if ( xmin > m_baseline ) xmin = m_baseline;
	if ( xmax < m_baseline ) xmax = m_baseline;

	return new wxPLLinearAxis( xmin, xmax );
}



#include <wx/dc.h>
#include "wex/plot/pllineplot.h"

#if defined(__WXOSX__)||defined(__WXGTK__)
#include <cmath>
#undef wxIsNaN
#define wxIsNaN(a) std::isnan(a)
#endif

static wxPLOutputDevice::Style PLDevLineStyle( wxPLLinePlot::Style sty )
{
	wxPLOutputDevice::Style wxsty = wxPLOutputDevice::SOLID;
	if ( sty == wxPLLinePlot::DASHED ) wxsty = wxPLOutputDevice::DASH;
	if ( sty == wxPLLinePlot::DOTTED ) wxsty = wxPLOutputDevice::DOT;
	return wxsty;
}

#define LINE_PEN dc.Pen( m_colour, m_thickness, PLDevLineStyle(m_style), wxPLOutputDevice::BEVEL, wxPLOutputDevice::BUTT )
#define MARKER_PEN dc.Pen( m_colour, m_thickness-1, wxPLOutputDevice::SOLID, wxPLOutputDevice::MITER, wxPLOutputDevice::BUTT )

wxPLLinePlot::wxPLLinePlot()
{
	Init();
}

wxPLLinePlot::wxPLLinePlot( const std::vector< wxRealPoint > &data, 
						   const wxString &label, const wxColour &col,
						   Style sty,
						   double thick,
						   Marker mkr )
	: wxPLPlottable( label )
{
	Init();
	m_colour = col;
	m_data = data;
	m_style = sty;
	m_thickness = thick;
	m_marker = mkr;
}

wxPLLinePlot::~wxPLLinePlot()
{
	// nothing to do 
}

void wxPLLinePlot::Init()
{
	m_colour = "forest green";
	m_thickness = 2;
	m_style = SOLID;
	m_marker = NO_MARKER;
	m_ignoreZeros = false;
}

wxRealPoint wxPLLinePlot::At( size_t i ) const
{
	return m_data[i];
}

size_t wxPLLinePlot::Len() const
{
	return m_data.size();
}

bool wxPLLinePlot::GetIgnoreZeros()
{
	return m_ignoreZeros;
}

void wxPLLinePlot::SetIgnoreZeros(bool value)
{
	m_ignoreZeros = value;
}

void wxPLLinePlot::DrawMarkers( wxPLOutputDevice &dc, std::vector<wxRealPoint> &points, double size )
{
	if ( m_marker == NO_MARKER ) return;

	std::vector<wxRealPoint> mkr(6, wxRealPoint() );
		
	double radius = 5;
	if ( size <= 1 ) radius = 3;
	else if ( size <= 3 ) radius = 4;

	for( size_t i=0;i<points.size();i++ )
	{
		wxRealPoint &p = points[i];
		
		if ( m_marker == HOURGLASS )
		{
			mkr[0] = wxRealPoint( p.x-radius, p.y-radius );
			mkr[1] = wxRealPoint( p.x+radius, p.y-radius );
			mkr[2] = wxRealPoint( p.x-radius, p.y+radius );
			mkr[3] = wxRealPoint( p.x+radius, p.y+radius );
			mkr[4] = mkr[0];
			mkr[5] = mkr[1];
			
			dc.Polygon( 4, &mkr[0] );
			dc.Lines( 6, &mkr[0] );
		}
		else if ( m_marker == SQUARE )
		{
			mkr[0] = wxRealPoint( p.x-radius, p.y-radius );
			mkr[1] = wxRealPoint( p.x+radius, p.y-radius );
			mkr[2] = wxRealPoint( p.x+radius, p.y+radius );
			mkr[3] = wxRealPoint( p.x-radius, p.y+radius );
			mkr[4] = mkr[0];
			mkr[5] = mkr[1];

			dc.Polygon( 4, &mkr[0] );
			dc.Lines( 6, &mkr[0] );
		}
		else if ( m_marker == DIAMOND )
		{
			mkr[0] = wxRealPoint( p.x, p.y-radius );
			mkr[1] = wxRealPoint( p.x+radius, p.y );
			mkr[2] = wxRealPoint( p.x, p.y+radius );
			mkr[3] = wxRealPoint( p.x-radius, p.y );
			mkr[4] = mkr[0];
			mkr[5] = mkr[1];

			dc.Polygon( 4, &mkr[0] );
			dc.Lines( 6, &mkr[0] );
		}
		else
		{
			dc.Circle( p.x, p.y, radius );
		}
	}
}

void wxPLLinePlot::Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map )
{
	size_t len = Len();
	if ( len < 2 ) return;

	wxRealPoint wmin( map.GetWorldMinimum() );
	wxRealPoint wmax( map.GetWorldMaximum() );
	
	dc.Brush( m_colour );

	std::vector< wxRealPoint > points;
	points.reserve( len );

	for ( size_t i = 0; i<len; i++ )
	{
		wxRealPoint pt( At(i) );

		bool has_next = ( i+1 < len );
		wxRealPoint next;
		if ( has_next )
		{
			next = At(i+1);
			if ( wxIsNaN( next.x ) )
				has_next = false;
		}

		bool has_prev = ( i > 0 );
		wxRealPoint prev;
		if ( has_prev )
		{
			prev = At(i-1);
			if ( wxIsNaN( prev.x ) )
				has_prev = false;
		}

		bool nanval = wxIsNaN( pt.x ) || wxIsNaN( pt.y );
		if ( !nanval )
		{
			if (	( has_next && pt.x < wmin.x && next.x >= wmin.x )  // straddles left boundary
				||	( has_prev && prev.x < wmax.x && pt.x >= wmax.x ) // straddles right boundary
				||	( pt.x >= wmin.x && pt.x <= wmax.x ) ) // within world min/max
			{
				points.push_back(map.ToDevice( At(i) ));
			}
		}

		if ( nanval && points.size() > 1 )
		{
			// draw currently accumulated points and clear
			// accumulator - this will draw the contiguous
			// segments of data that don't have any NaN values
			if ( m_style != NO_LINE )
			{
				LINE_PEN;
				dc.Lines( points.size(), &points[0] );
			}
			
			MARKER_PEN;
			DrawMarkers( dc, points, m_thickness );
			points.clear();
		}
	}
	
	if ( points.size() > 1 )
	{
		if ( m_style != NO_LINE )
		{
			LINE_PEN;
			dc.Lines( points.size(), &points[0] );
		}

		MARKER_PEN;
		DrawMarkers( dc, points, m_thickness );
	}
}

void wxPLLinePlot::DrawInLegend( wxPLOutputDevice &dc, const wxPLRealRect &rct)
{
	double thick = m_thickness;
	if ( thick > 3 ) thick = 3; // limit line thickness for legend display
	
	if ( m_style != NO_LINE )
	{
		LINE_PEN;
		dc.Line( rct.x, rct.y+rct.height/2, rct.x+rct.width, rct.y+rct.height/2 );
	}

	if ( m_marker != NO_MARKER )
	{
		std::vector<wxRealPoint> mkr(1, wxPoint( rct.x + rct.width/2, rct.y + rct.height/2 ) );
		MARKER_PEN;
		dc.Brush( m_colour );
		DrawMarkers( dc, mkr, thick );
	}
}

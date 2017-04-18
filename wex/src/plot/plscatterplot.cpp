#include <algorithm>

#include <wx/dc.h>

#include "wex/plot/plcolourmap.h"
#include "wex/plot/plscatterplot.h"

wxPLScatterPlot::wxPLScatterPlot()
{
	m_cmap = 0;
	m_colour = *wxBLUE;
	m_radius = 3;
	m_scale = false;
	m_antiAliasing = false;
	m_drawLineOfPerfectAgreement = false;
}

wxPLScatterPlot::wxPLScatterPlot( const std::vector<wxRealPoint> &data,
	const wxString &label,
	const wxColour &col,
	double size,
	bool scale )
	: wxPLPlottable( label )
{
	m_cmap = 0;
	m_data = data;
	m_colour = col;
	m_radius = size;
	m_scale = scale;
	m_antiAliasing = false;
	m_drawLineOfPerfectAgreement = false;
}


wxPLScatterPlot::~wxPLScatterPlot()
{
	// nothing to do currently
}

void wxPLScatterPlot::SetColourMap( wxPLColourMap *cmap )
{
	m_cmap = cmap;
}

void wxPLScatterPlot::SetColours( const std::vector<double> &zv )
{
	m_colours = zv;
}

void wxPLScatterPlot::ClearColours()
{
	m_colours.clear();
}

void wxPLScatterPlot::SetSizes( const std::vector<double> &sv )
{
	m_sizes = sv;
}

void wxPLScatterPlot::ClearSizes()
{
	m_sizes.clear();
}
	
wxRealPoint wxPLScatterPlot::At( size_t i ) const
{
	return m_data[i];
}

size_t wxPLScatterPlot::Len() const
{
	return m_data.size();
}

void wxPLScatterPlot::Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map )
{
	dc.Pen( m_colour, 1 );
	dc.Brush( m_colour );

	wxRealPoint min = map.GetWorldMinimum();
	wxRealPoint max = map.GetWorldMaximum();
	
	size_t len = Len();
	wxPLColourMap *zcmap = 0;
	if ( m_cmap && m_colours.size() == len )
		zcmap = m_cmap;

	bool has_sizes = ( m_sizes.size() == len );

	for ( size_t i=0; i<len; i++ )
	{
		const wxRealPoint p = At(i);
		if ( p.x >= min.x && p.x <= max.x
			&& p.y >= min.y && p.y <= max.y )
		{
			double rad = m_radius;

			if ( has_sizes )
			{
				rad = m_sizes[i];
				if ( rad < 1 ) rad = 1;
			}
			
			if ( zcmap )
			{
				wxColour C( zcmap->ColourForValue( m_colours[i] ) );
				dc.Pen( C, 1 );
				dc.Brush( C );
			}

			dc.Circle( map.ToDevice(p), rad );
		}
	}

	if ( m_drawLineOfPerfectAgreement 
		&& !m_isLineOfPerfectAgreementDrawn )
	{
		m_isLineOfPerfectAgreementDrawn = true;		
		dc.Pen(*wxBLACK, 1);

		wxRealPoint pstart, pend;
		if (min.x <= min.y) pstart = wxRealPoint(min.y, min.y);
		else pstart = wxRealPoint(min.x, min.x);

		if (max.x <= max.y) pend = wxRealPoint(max.x, max.x);
		else pend = wxRealPoint(max.y, max.y);

		dc.Line(map.ToDevice(pstart), map.ToDevice(pend));
	}
}

void wxPLScatterPlot::DrawInLegend( wxPLOutputDevice &dc, const wxPLRealRect &rct)
{
	dc.Pen( m_colour, 1 );
	dc.Brush( m_colour );
	wxCoord rad = std::min( rct.width, rct.height );
	rad = rad/2 - 2;
	if ( rad < 2 ) rad = 2;
	dc.Circle( rct.x+rct.width/2, rct.y+rct.height/2, rad );
}
void wxPLScatterPlot::SetLineOfPerfectAgreementFlag(bool flagValue)
{
	m_drawLineOfPerfectAgreement = flagValue;
	m_isLineOfPerfectAgreementDrawn = false;
}

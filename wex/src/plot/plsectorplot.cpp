#include "wex/plot/plsectorplot.h"
#include <algorithm>

static std::vector<wxColour> gs_sectorColors;

wxPLSectorPlot::wxPLSectorPlot()
	: wxPLPlottable( wxEmptyString )
{
	// initialize shared (static) default color list for sectors
	if ( gs_sectorColors.size() == 0 )
	{
		gs_sectorColors.push_back( wxColour(111,164,196) );
		gs_sectorColors.push_back( wxColour("GREY") );
		gs_sectorColors.push_back( wxColour(181,211,227) );
		gs_sectorColors.push_back( *wxLIGHT_GREY );
		gs_sectorColors.push_back( wxColour("PALE GREEN") );
		gs_sectorColors.push_back( wxColour("GOLDENROD") );
		gs_sectorColors.push_back( wxColour("MEDIUM VIOLET RED") );
		gs_sectorColors.push_back( wxColour("MEDIUM SEA GREEN") );
		gs_sectorColors.push_back( wxColour("DARK SLATE GREY") );
		gs_sectorColors.push_back( wxColour("WHEAT") );
		gs_sectorColors.push_back( wxColour("FIREBRICK") );
		gs_sectorColors.push_back( wxColour("dark orchid") );
		gs_sectorColors.push_back( wxColour("dim grey") );
		gs_sectorColors.push_back( wxColour("brown") );
	}

	m_fmtMode = wxNUMERIC_REAL;
	m_fmtDeci = wxNUMERIC_GENERIC;
	m_fmtThouSep = false;
	m_labelValues = true;
	m_holeFraction = 0.6;
	m_calloutSize = 20;
	m_textSpace = 3;
	m_border = 15;
}


void wxPLSectorPlot::AddSector( double value, const wxString &label )
{
	m_sectors.push_back( sector(value,label) );
}

void wxPLSectorPlot::AddInnerSector( double value, const wxString &label )
{
	m_inner.push_back( sector(value,label) );
}

wxRealPoint wxPLSectorPlot::At( size_t i ) const
{
	if ( i < m_sectors.size() ) return wxRealPoint( i, m_sectors[i].value );
	else return wxRealPoint( std::numeric_limits<double>::quiet_NaN(),
		std::numeric_limits<double>::quiet_NaN() );
}

size_t wxPLSectorPlot::Len() const
{
	return m_sectors.size();
}

void wxPLSectorPlot::CalculateAngles( std::vector<sector> &list )
{
	double sum = 0;
	for( size_t i=0;i<list.size();i++ )
		sum += list[i].value;

	if ( sum == 0.0 ) return;

	for( size_t i=0;i<list.size();i++ )
	{
		list[i].angle =  list[i].value / sum * 360;
		list[i].start = (i==0) ? 0.0 : list[i-1].start + list[i-1].angle;
	}
}
#define sind(x) sin(M_PI/180.0*(x))
#define cosd(x) cos(M_PI/180.0*(x))

static void extend( wxRealPoint &min, wxRealPoint &max, const wxRealPoint &p )
{
	if ( p.x < min.x ) min.x = p.x;
	if ( p.y < min.y ) min.y = p.y;
	if ( p.x > max.x ) max.x = p.x;
	if ( p.y > max.y ) max.y = p.y;
}

void wxPLSectorPlot::Layout( double radius, wxRealPoint *TL, wxRealPoint *BR )
{
	// coming into this, text layouts, sizes and angles are precalculated
	for( size_t i=0;i<m_sectors.size();i++ )
	{
		sector &S = m_sectors[i];

		// calculate the midpoint angle in the sector
		double midangle = S.start + S.angle*0.5;

		double offset_radius = radius + m_textSpace;
		S.callout_start.x = offset_radius*cosd( 90-midangle );
		S.callout_start.y = -offset_radius*sind( 90-midangle );

		double callout_radius = radius + m_textSpace + m_calloutSize;
		S.callout_end.x = callout_radius*cosd( 90-midangle );
		S.callout_end.y = -callout_radius*sind( 90-midangle );
		
		// position the text.  sizes are precalculated 
		if ( midangle >= 0 && midangle <= 180 )
		{
			// text on the right
			S.textpos.x = S.callout_end.x + m_textSpace;
			S.textpos.y = S.callout_end.y - S.textsize.y*0.5;
		}
		else
		{
			// text on the left
			S.textpos.x = S.callout_end.x - S.textsize.x - m_textSpace;
			S.textpos.y = S.callout_end.y - S.textsize.y*0.5;
		}
	}

	wxRealPoint min(-radius,-radius), max(radius,radius);
	// calculate extents
	for( size_t i=0;i<m_sectors.size();i++ )
	{
		sector &S = m_sectors[i];
		extend( min, max, S.callout_end );
		extend( min, max, wxRealPoint( S.textpos.x,                S.textpos.y ) );
		extend( min, max, wxRealPoint( S.textpos.x + S.textsize.x, S.textpos.y ) );
		extend( min, max, wxRealPoint( S.textpos.x + S.textsize.x, S.textpos.y + S.textsize.y ) );
		extend( min, max, wxRealPoint( S.textpos.x,                S.textpos.y + S.textsize.y ) );
	}

	*TL = wxRealPoint(min.x-m_border, min.y-m_border);
	*BR = wxRealPoint(max.x+m_border, max.y+m_border);
}

void wxPLSectorPlot::Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map )
{
	wxRealPoint pos, size;
	map.GetDeviceExtents( &pos, &size );

	// calculate all the start angles and sector sizes
	CalculateAngles( m_sectors );

	// calculate all the text layouts
	dc.TextPoints( 0 ); // ensure default normal font
	dc.TextColour( *wxBLACK );

	for( size_t i=0;i<m_sectors.size();i++ )
	{
		sector &S = m_sectors[i];
		double midangle = S.start + 0.5*S.angle;

		// don't cache text layouts.  the sector plot
		// doesn't know its render state and whether the font has changed
		if ( S.layout != 0 ) delete S.layout;		
		S.layout = new wxPLTextLayout( dc, S.label, (midangle >= 0 && midangle <= 180 ) ? wxPLTextLayout::LEFT : wxPLTextLayout::RIGHT );
		
		S.textsize.x = S.layout->Width();
		S.textsize.y = S.layout->Height();
	}

	double radius = std::min( size.x, size.y ) * 0.5; // start with radius that is definitely a bit too big
	wxRealPoint tl, br; // top left and bottom right coordinates of layout, assuming center at 0,0

	// iteratively shrink until it all fits or a minimum radius is reached
	// this is pretty fast since all the text layout sizes and angles are 
	// precalculated and don't change.
	wxRealPoint extent;
	do {
		radius -= 5;
		Layout( radius, &tl, &br );
		extent.x = br.x - tl.x;
		extent.y = br.y - tl.y;
	} while ( (extent.x >= size.x || extent.y >= size.y) && radius > 50 );
	
	
	wxRealPoint offset( pos.x - tl.x, pos.y - tl.y );

	std::vector<wxColour> &cl = m_colourList.size() > 0 ? m_colourList : gs_sectorColors;
	
	// draw sectors
	dc.NoPen();
	for( size_t i=0;i<m_sectors.size();i++ )
	{
		sector &S = m_sectors[i];
		dc.Brush( cl[i%cl.size()] );
		dc.Sector( offset.x, offset.y, radius, S.start, S.start+S.angle );
	}
	
	// draw callout lines and text
	dc.Pen( *wxLIGHT_GREY, 0.5 );
	for( size_t i=0;i<m_sectors.size();i++ )
	{
		sector &S = m_sectors[i];
		dc.Line( offset + S.callout_start, offset + S.callout_end );
		S.layout->Render( dc, offset.x+S.textpos.x, offset.y+S.textpos.y, 0, false );
	}
	
	if ( m_labelValues )
	{
		// calculate and draw label inside sector with the value
		dc.TextPoints( -1 );
		dc.TextColour( wxColour(240,240,240) );
		for( size_t i=0;i<m_sectors.size();i++ )
		{
			double f = 0.5*(1.0 + m_holeFraction);
			if ( f < 0.75 ) f = 0.75;

			sector &S = m_sectors[i];
			double midangle = S.start + S.angle*0.5;
			wxRealPoint valpt( offset.x + f*radius*cosd(90-midangle),
				offset.y - f*radius*sind(90-midangle) );
			wxString label( wxNumericFormat( S.value, m_fmtMode, m_fmtDeci, m_fmtThouSep, m_fmtPre, m_fmtPost ) );
			double tw, th;
			dc.Measure(label, &tw, &th );
			dc.Text( label, valpt.x - tw/2, valpt.y - th/2 );
		}
	}

	// draw inner sectors
	if ( m_holeFraction > 0 || m_inner.size() > 0 )
	{
		dc.Brush( *wxWHITE );
		dc.NoPen();
		dc.Circle( offset.x, offset.y, m_holeFraction*radius );
	}

	if ( m_holeFraction > 0 && m_inner.size() > 0 )
	{
		dc.Pen( *wxLIGHT_GREY, 0.5 );
		dc.TextPoints( -1 );
		dc.TextColour( *wxBLACK );

		CalculateAngles( m_inner );
		for( size_t i=0;i<m_inner.size();i++ )
		{
			sector &S = m_inner[i];
			if ( S.layout != 0 ) delete S.layout;			
			S.layout = new wxPLTextLayout( dc, S.label, wxPLTextLayout::CENTER );

			double f = m_holeFraction * 0.666;

			double midangle = S.start + S.angle*0.5;
			wxRealPoint valpt( offset.x + f*radius*cosd(90-midangle),
				offset.y - f*radius*sind(90-midangle) );

			S.layout->Render( dc, valpt.x - S.layout->Width()/2,
				valpt.y - S.layout->Height()/2, 0, false );
			
			wxRealPoint end;
			end.x = offset.x + m_holeFraction*radius*cosd(90-(S.start+S.angle));
			end.y = offset.y - m_holeFraction*radius*sind(90-(S.start+S.angle));
			dc.Line( offset, end );
		}
		
	}

}

void wxPLSectorPlot::Invalidate()
{
	Invalidate( m_sectors );
	Invalidate( m_inner );
}

void wxPLSectorPlot::Invalidate( std::vector<sector> &list )
{
	for( size_t i=0;i<list.size();i++ )
	{
		sector &S = list[i];
		if( S.layout ) delete S.layout;
		S.layout = 0;
	}
}

void wxPLSectorPlot::DrawInLegend( wxPLOutputDevice &dc, const wxPLRealRect &rct)
{
	// nothing to do - don't want it to show up in legend
}

wxPLAxis *wxPLSectorPlot::SuggestXAxis()
{
	return new wxPLLinearAxis( 0, 1 );
}

wxPLAxis *wxPLSectorPlot::SuggestYAxis()
{
	return new wxPLLinearAxis( 0, 1 );
}



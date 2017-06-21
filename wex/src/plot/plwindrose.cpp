#include <algorithm>

#include <wx/dc.h>
#include "wex/plot/plwindrose.h"

wxPLWindRose::wxPLWindRose()
{
	m_colour = *wxBLUE;
	m_ignoreAngles = true;
}

wxPLWindRose::wxPLWindRose( const std::vector<wxRealPoint> &data,
	const wxString &label,
	const wxColour &col )
	: wxPLPlottable( label )
{
	m_data = data;
	m_colour = col;
	m_ignoreAngles = true;
}


wxPLWindRose::~wxPLWindRose()
{
	// nothing to do currently
}

	
wxRealPoint wxPLWindRose::At( size_t i ) const
{
	return m_data[i];
}

size_t wxPLWindRose::Len() const
{
	return m_data.size();
}

void wxPLWindRose::DrawInLegend( wxPLOutputDevice &dc, const wxPLRealRect &rct)
{
	dc.Pen( m_colour, 1 );
	wxCoord rad = std::min( rct.width, rct.height );
	rad = rad/2 - 2;
	if ( rad < 2 ) rad = 2;
	dc.Circle( rct.x+rct.width/2, rct.y+rct.height/2, rad );
}

wxPLAxis *wxPLWindRose::SuggestXAxis() const
{
	return new wxPLPolarAngularAxis("Wind Direction", wxPLPolarAngularAxis::DEGREES, wxPLPolarAngularAxis::UP, wxPLPolarAngularAxis::DIRECTIONS);
}

wxPLAxis *wxPLWindRose::SuggestYAxis() const
{
	double xmin = 0, xmax = 0, ymin = 0, ymax = 0;
	GetMinMax(&xmin, &xmax, &ymin, &ymax);
	return new wxPLLinearAxis(0, ymax);
}

void wxPLWindRose::Draw(wxPLOutputDevice &dc, const wxPLDeviceMapping &map)
{
	if (Len() == 0) return;
	wxPLPolarAngularAxis *pa = dynamic_cast<wxPLPolarAngularAxis *>(map.GetXAxis());
	if (!pa) return;

	dc.Brush( m_colour );

	// basic assumption - wind rose will ignore the X values (angle) and use only the radius
	double fullCircle = 0;
	switch (pa->GetAxisUnits())
	{
		case wxPLPolarAngularAxis::RADIANS:
			fullCircle = 8.0 * atan(1);
			break;

		case wxPLPolarAngularAxis::GRADIANS:
			fullCircle = 400;
			break;

		default:
			fullCircle = 360.0;
			break;
	}

	double width = fullCircle / Len();
	if (width > fullCircle / 36.0) width = fullCircle / 36.0;

	double centerOfSlice = width / 2.0;
	wxRealPoint pts[3];
	pts[0] = map.ToDevice(0, 0);

	for (size_t i = 0; i<Len(); i++)
	{
		wxRealPoint pt = At(i);
		double half = width*0.95 / 2.0;
		double center = (m_ignoreAngles) ? centerOfSlice : pt.x;
		pts[1] = map.ToDevice(center - half, pt.y);
		pts[2] = map.ToDevice(center + half, pt.y);
		dc.Polygon(3, pts);

		centerOfSlice += fullCircle / Len();;
	}
}

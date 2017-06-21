
#include "wex/plot/plannotation.h"

wxPLAnnotation::wxPLAnnotation()
{
}

wxPLAnnotation::~wxPLAnnotation()
{
}

wxPLAnnotationMapping::wxPLAnnotationMapping()
{
}

wxPLAnnotationMapping::~wxPLAnnotationMapping()
{
}


wxPLTextAnnotation::wxPLTextAnnotation( const wxString &text,
	const wxRealPoint &pos, 
	double size,
	double angle,
	const wxColour &c, 
	wxPLTextLayout::TextAlignment align,
	const wxRealPoint &delta )

	: wxPLAnnotation( ),
		m_text(text), m_pos(pos), m_size(size), 
		m_angle(angle), m_colour(c), m_align(align), m_layout(0),
		m_delta(delta)
{
}

wxPLTextAnnotation::~wxPLTextAnnotation()
{
	if ( m_layout ) delete m_layout;
}
	
void wxPLTextAnnotation::Draw( wxPLOutputDevice &dc, const wxPLAnnotationMapping &map )
{
	if ( m_text.IsEmpty() ) return;

	dc.TextPoints( m_size );

	if ( m_layout ) delete m_layout;
	m_layout = new wxPLTextLayout( dc, m_text, m_align );
	
	wxRealPoint pos( map.ToDevice( m_pos ) );	
	if ( m_align == wxPLTextLayout::CENTER )
		pos.x -= m_layout->Width()/2;
	else if ( m_align == wxPLTextLayout::RIGHT )
		pos.x -= m_layout->Width();
	
	pos.y -= m_layout->Height()/2;
	
	dc.TextColour( m_colour );
	m_layout->Render( dc, pos.x+m_delta.x, pos.y+m_delta.y, m_angle, false );
}


wxPLLineAnnotation::wxPLLineAnnotation( const std::vector<wxRealPoint> &pts,
		double size,
		const wxColour &c,
		wxPLOutputDevice::Style style,
		ArrowType arrow ) 
	: wxPLAnnotation( ), m_points( pts ), m_size(size), m_colour(c), m_style(style), m_arrow(arrow)
{
}

wxPLLineAnnotation::~wxPLLineAnnotation()
{
	// nothing to do
}

static wxRealPoint rotate2d(
	const wxRealPoint &P, 
	double angle )
{
	double rad = angle*M_PI/180.0;
	return wxRealPoint(
		cos(rad)*P.x - sin(rad)*P.y,
		sin(rad)*P.x + cos(rad)*P.y );
}

static wxRealPoint scalept( const wxRealPoint &p, double scale )
{
	return wxRealPoint( p.x*scale, p.y*scale );
}

void wxPLLineAnnotation::Draw( wxPLOutputDevice &dc, const wxPLAnnotationMapping &map )
{
	if ( m_points.size() < 2 ) return;

	dc.SetAntiAliasing( true );
	
	std::vector<wxRealPoint> mapped( m_points.size(), wxRealPoint() );
	for( size_t i=0;i<m_points.size();i++ )
		mapped[i] = map.ToDevice( m_points[i] );

	dc.Pen( m_colour, m_size, m_style );
	dc.Lines( mapped.size(), &mapped[0] );

	if ( m_arrow != NO_ARROW )
	{
		size_t len = mapped.size();
		wxRealPoint pt = mapped[len-1];
		wxRealPoint pt2 = mapped[len-2];

		wxRealPoint d( pt2.x - pt.x, pt2.y - pt.y );
		double sc = sqrt( d.x*d.x + d.y*d.y );
		double arrow_size = 3+m_size; // points
		d.x *= arrow_size/sc;
		d.y *= arrow_size/sc;

		const double angle = 30;

		wxRealPoint p1 = pt + rotate2d( d, -angle );
		wxRealPoint p2 = pt + rotate2d( d, angle );
		wxRealPoint avg( 0.5*(p1.x+p2.x), 0.5*(p1.y+p2.y) );
		wxRealPoint pts[5] = { avg, p1, pt, p2, avg };

		dc.Pen( m_colour, m_size, wxPLOutputDevice::SOLID );
		dc.Brush( m_colour );

		if ( m_arrow == FILLED_ARROW )
			dc.Polygon( 5, pts, wxPLOutputDevice::WINDING_RULE );
		else
			dc.Lines( 3, pts+1 );
	}
}


wxPLBraceAnnotation::wxPLBraceAnnotation( 
		const wxRealPoint &p1,
		const wxRealPoint &p2,
		double scale, 
		double size ,
		const wxColour &c,
		wxPLOutputDevice::Style style )
		: wxPLAnnotation(),
		m_p1(p1), m_p2(p2), m_scale(scale), m_size(size), m_colour(c), m_style(style)
{
}

wxPLBraceAnnotation::~wxPLBraceAnnotation()
{
}

void wxPLBraceAnnotation::Draw( wxPLOutputDevice &dc, const wxPLAnnotationMapping &map )
{
	wxRealPoint a( map.ToDevice(m_p1) ),
		b( map.ToDevice(m_p2) );

	wxRealPoint c( 0.5*(b.x+a.x), 0.5*(b.y+a.y) );
	wxRealPoint d( a-b );
	d = rotate2d( d, 90 );
	
	double L = sqrt(d.x*d.x + d.y*d.y);
	if ( L == 0.0 ) return;

	double sf = 12.0/L * m_scale;
	wxRealPoint p( c+scalept(d,sf) );
	
	wxRealPoint x( 0.5*(c.x+p.x), 0.5*(c.y+p.y) );

	std::vector<wxRealPoint> ln;
	ln.push_back( a );
	ln.push_back( a + scalept(rotate2d(d,7),sf*0.35) );
	ln.push_back( x );
	ln.push_back( p );
	ln.push_back( x );
	ln.push_back( b + scalept(rotate2d(d,-7),sf*0.35) );
	ln.push_back( b );
	
	dc.SetAntiAliasing( true );
	dc.Pen( m_colour, m_size, m_style );
	dc.Lines( ln.size(), &ln[0] );
}


wxPLShapeAnnotation::wxPLShapeAnnotation( 
		ShapeType type,
		const wxPLRealRect &rect,
		const wxColour &c,
		bool filled,
		double size ) : wxPLAnnotation(),
	m_type(type), m_rect(rect), m_colour(c), m_filled(filled), m_size(size)
{
}

wxPLShapeAnnotation::~wxPLShapeAnnotation()
{
}

void wxPLShapeAnnotation::Draw( wxPLOutputDevice &dc, const wxPLAnnotationMapping &map )
{
	dc.Pen( m_colour, m_size );
	if ( m_filled ) dc.Brush( m_colour ); else dc.NoBrush();

	if ( m_type == RECTANGLE )
	{
		wxRealPoint tl = map.ToDevice( wxRealPoint( m_rect.x, m_rect.y ) );
		wxRealPoint br = map.ToDevice( wxRealPoint( m_rect.x+m_rect.width, m_rect.y+m_rect.height ) );
		dc.Rect( tl.x, tl.y, br.x-tl.x, tl.y-br.y );
	}
	else
	{
		wxRealPoint c = map.ToDevice( wxRealPoint(m_rect.x, m_rect.y) );
		wxRealPoint rp = map.ToDevice( wxRealPoint( m_rect.x+m_rect.width, m_rect.y ) );
		dc.Circle( c, rp.x-c.x );
	}
}

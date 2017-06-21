#include <vector>
#include <math.h>
#include <wex/pdf/pdfdoc.h>
#include <wex/pdf/pdffont.h>
#include <wex/plot/ploutdev.h>
#include <wex/plot/pltext.h>

wxPLPdfOutputDevice::wxPLPdfOutputDevice( wxPdfDocument &doc, double fontpnts ) 
	: wxPLOutputDevice(), m_pdf(doc)	
{
	m_fontRelSize = 0;
	m_fontPoint0 = fontpnts;
	m_pen = m_brush = true;
	m_pdf.SetTextColour( *wxBLACK );
}

void wxPLPdfOutputDevice::SetAntiAliasing( bool )
{
	// nothing to do here for PDFs...
}

void wxPLPdfOutputDevice::Clip( double x, double y, double width, double height ) {
	m_pdf.ClippingRect( x, y, width, height );
}

void wxPLPdfOutputDevice::Unclip() {
	m_pdf.UnsetClipping();
}

void wxPLPdfOutputDevice::Pen( const wxColour &c, double size, 
	Style line, Style join, Style cap )
{
	if ( line == NONE ) {
		m_pen = false;
		return;
	}

	m_pen = true;

	wxPdfArrayDouble dash;
	wxPdfLineStyle style;
		
	style.SetColour( c );
		
	style.SetWidth( size );

	double dsize = size;
	if ( dsize < 1.5 ) dsize = 1.5;
		
	switch( line )
	{
	case DOT:
		dash.Add( dsize );
		dash.Add( dsize );
		break;
	case DASH:
		dash.Add( 2.0*dsize );
		dash.Add( dsize );
		break;
	case DOTDASH:
		dash.Add( dsize );
		dash.Add( dsize );
		dash.Add( 2.0*dsize );
		dash.Add( dsize );
		break;
	}
	style.SetDash( dash );

	switch( join )
	{
	case ROUND: style.SetLineJoin( wxPDF_LINEJOIN_ROUND ); break;
	case BEVEL: style.SetLineJoin( wxPDF_LINEJOIN_BEVEL ); break;
	default: style.SetLineJoin( wxPDF_LINEJOIN_MITER ); break;
	}
		
	switch( cap )
	{
	case ROUND: style.SetLineCap( wxPDF_LINECAP_ROUND ); break;
	default: style.SetLineCap( wxPDF_LINECAP_BUTT ); break;
	}

	m_pdf.SetLineStyle( style );
}

void wxPLPdfOutputDevice::Brush( const wxColour &c, Style sty ) {
	if ( sty == NONE )
	{
		m_brush = false;
		return;
	}

	// currently, hatch and other patterns not supported
	m_pdf.SetFillColour( c );
	m_brush = true;
}

void wxPLPdfOutputDevice::Line( double x1, double y1, double x2, double y2 )	{
	m_pdf.Line( x1, y1, x2, y2 );
}

void wxPLPdfOutputDevice::Lines( size_t n, const wxRealPoint *pts ) {
	for (size_t i = 0; i < n; ++i)
	{			
		if (i == 0) m_pdf.MoveTo( pts[i].x, pts[i].y );
		else m_pdf.LineTo( pts[i].x, pts[i].y );
	}
	m_pdf.EndPath(wxPDF_STYLE_DRAW);
}

int wxPLPdfOutputDevice::GetDrawingStyle()
{
	int style = wxPDF_STYLE_NOOP;
	if ( m_brush && m_pen ) style = wxPDF_STYLE_FILLDRAW;
	else if (m_pen) style = wxPDF_STYLE_DRAW;
	else if (m_brush) style = wxPDF_STYLE_FILL;
	return style;
}

void wxPLPdfOutputDevice::Polygon( size_t n, const wxRealPoint *pts, FillRule rule ) {		
	if ( n == 0 ) return;
	int saveFillingRule = m_pdf.GetFillingRule();
	m_pdf.SetFillingRule( rule==ODD_EVEN_RULE ? wxODDEVEN_RULE : wxWINDING_RULE );
	wxPdfArrayDouble xp(n, 0.0), yp(n, 0.0);
	for( size_t i=0;i<n;i++ )
	{
		xp[i] = pts[i].x;
		yp[i] = pts[i].y;
	}
	m_pdf.Polygon(xp, yp, GetDrawingStyle() );
	m_pdf.SetFillingRule(saveFillingRule);
}
void wxPLPdfOutputDevice::Rect( double x, double y, double width, double height ) {
	m_pdf.Rect( x, y, width, height, GetDrawingStyle() );
}

void wxPLPdfOutputDevice::Circle( double x, double y, double radius ) {
	m_pdf.Circle( x, y, radius, 0.0, 360.0, GetDrawingStyle() );
}

void wxPLPdfOutputDevice::Sector( double x, double y, double radius, double angle1, double angle2 ) {
	m_pdf.Sector( x, y, radius, angle1, angle2, GetDrawingStyle(), true, 90.0 );
}

void wxPLPdfOutputDevice::MoveTo( double x, double y )
{
	m_shape.MoveTo( x, y );
}

void wxPLPdfOutputDevice::LineTo( double x, double y )
{
	m_shape.LineTo( x, y );
}

void wxPLPdfOutputDevice::CloseSubPath()
{
	m_shape.ClosePath();
}

void wxPLPdfOutputDevice::Path( FillRule rule )
{
	int saveFillingRule = m_pdf.GetFillingRule();
	m_pdf.SetFillingRule( rule==ODD_EVEN_RULE ? wxODDEVEN_RULE : wxWINDING_RULE );
	m_pdf.Shape( m_shape, GetDrawingStyle() );
	m_shape = wxPdfShape(); // clear the path for next one
	m_pdf.SetFillingRule( saveFillingRule );
}

	
void wxPLPdfOutputDevice::TextPoints( double relpt ) {
	m_pdf.SetFontSize( m_fontPoint0 + relpt );
	m_fontRelSize = relpt;
}

double wxPLPdfOutputDevice::TextPoints( ) const {
	return m_fontRelSize;
}

void wxPLPdfOutputDevice::TextColour( const wxColour &c ) {	
	m_pdf.SetTextColour( c );	
}

void wxPLPdfOutputDevice::Text( const wxString &text, double x, double y,  double angle ) {		
	double points = m_pdf.GetFontSize();
	double asc = (double)abs(m_pdf.GetFontDescription().GetAscent());
	double des = (double)abs(m_pdf.GetFontDescription().GetDescent());
	double em = asc+des;
	double ascfrac = asc/em;
	double ybase = points*ascfrac;

	if ( fabs(angle) < 0.5)
	{
		m_pdf.Text( x, y + ybase, text );
	}
	else
	{
		double xx = x + ybase*sin( angle*M_PI/180 );
		double yy = y + ybase*cos( angle*M_PI/180 );
		m_pdf.RotatedText( xx, yy, text, angle );
	}
}

void wxPLPdfOutputDevice::Measure( const wxString &text, double *width, double *height ) {
	if (width) *width = m_pdf.GetStringWidth(text);
	if (height) *height = m_pdf.GetFontSize();
}

#define CAST(x) ((int)wxRound(m_scale*(x)))

static void TranslateBrush( wxBrush *b, const wxColour &c, wxPLOutputDevice::Style sty )
{
	b->SetColour( c );
	switch( sty )
	{
	case wxPLOutputDevice::NONE: *b = *wxTRANSPARENT_BRUSH; break;
	case wxPLOutputDevice::HATCH: b->SetStyle(wxCROSSDIAG_HATCH); break;
	default: b->SetStyle( wxSOLID ); break;
	}
}

static void TranslatePen( wxPen *p, const wxColour &c, double size, 
	wxPLOutputDevice::Style line, wxPLOutputDevice::Style join, wxPLOutputDevice::Style cap )
{	
	p->SetColour( c );
	p->SetWidth( size < 1.0 ? 1 : ((int)size) );

	switch( join )
	{
	case wxPLOutputDevice::MITER: p->SetJoin( wxJOIN_MITER ); break;
	case wxPLOutputDevice::BEVEL: p->SetJoin( wxJOIN_BEVEL ); break;
	default: p->SetJoin( wxJOIN_ROUND ); break;
	}
	switch( cap )
	{
	case wxPLOutputDevice::ROUND: p->SetCap( wxCAP_ROUND ); break;
	case wxPLOutputDevice::MITER: p->SetCap( wxCAP_PROJECTING ); break;
	default: p->SetCap( wxCAP_BUTT );
	}
	switch( line )
	{
	case wxPLOutputDevice::NONE: p->SetStyle( wxPENSTYLE_TRANSPARENT ); break;
	case wxPLOutputDevice::DOT: p->SetStyle( wxPENSTYLE_DOT ); break;
	case wxPLOutputDevice::DASH: p->SetStyle( wxPENSTYLE_SHORT_DASH ); break;
	case wxPLOutputDevice::DOTDASH: p->SetStyle( wxPENSTYLE_DOT_DASH ); break;
	default: p->SetStyle( wxPENSTYLE_SOLID ); break;
	}
}


/*
wxPLDCOutputDevice::wxPLDCOutputDevice( wxDC *dc, wxDC *aadc, double scale ) 
	: wxPLOutputDevice(), m_dc(dc), m_aadc(aadc), m_curdc(dc),
		m_pen( *wxBLACK_PEN ), m_brush( *wxBLACK_BRUSH ), 
		m_font0( m_dc->GetFont() ), m_scale( scale )
{
	m_fontSize = 0;
	m_fontBold = ( m_font0.GetWeight() == wxFONTWEIGHT_BOLD );
}


void wxPLDCOutputDevice::SetAntiAliasing( bool on )
{
	if ( 0 != m_aadc )
	{
		if ( on )
		{
			m_aadc->SetPen( m_curdc->GetPen() );
			m_aadc->SetBrush( m_curdc->GetBrush() );
			m_aadc->SetFont( m_curdc->GetFont() );
			m_curdc = m_aadc;
		}
		else
		{
			m_dc->SetPen( m_curdc->GetPen() );
			m_dc->SetBrush( m_curdc->GetBrush() );
			m_dc->SetFont( m_curdc->GetFont() );
			m_curdc = m_dc;
		}
	}
}

wxDC *wxPLDCOutputDevice::GetDC() { return m_curdc; }
	
void wxPLDCOutputDevice::Clip( double x, double y, double width, double height ) { 
	m_curdc->SetClippingRegion( CAST(x), CAST(y), CAST(width), CAST(height) );
}

void wxPLDCOutputDevice::Unclip() {
	m_curdc->DestroyClippingRegion();
}

void wxPLDCOutputDevice::Brush( const wxColour &c, Style sty ) { 

	TranslateBrush( &m_brush, c, sty );
	m_curdc->SetBrush( m_brush );
}

void wxPLDCOutputDevice::Pen( const wxColour &c, double size, 
	Style line, Style join, Style cap ) {

	TranslatePen( &m_pen, c, m_scale*size, line, join, cap );
	m_curdc->SetPen( m_pen );
}

void wxPLDCOutputDevice::Line( double x1, double y1, double x2, double y2 ) {
	m_curdc->DrawLine( CAST(x1), CAST(y1), CAST(x2), CAST(y2) );
}

void wxPLDCOutputDevice::Lines( size_t n, const wxRealPoint *pts ) {
	if ( n == 0 ) return;
	std::vector<wxPoint> ipt( n, wxPoint(0,0) );
	for( size_t i=0;i<n;i++ )
		ipt[i] = wxPoint( CAST(pts[i].x), CAST(pts[i].y) );

	m_curdc->DrawLines( n, &ipt[0] );
}

void wxPLDCOutputDevice::Polygon( size_t n, const wxRealPoint *pts, Style sty ) {
	if ( n == 0 ) return;
	std::vector<wxPoint> ipt( n, wxPoint(0,0) );
	for( size_t i=0;i<n;i++ )
		ipt[i] = wxPoint( CAST(pts[i].x), CAST(pts[i].y) );

	m_curdc->DrawPolygon( n, &ipt[0], 0, 0, sty==ODDEVEN ?  wxODDEVEN_RULE : wxWINDING_RULE );
}
void wxPLDCOutputDevice::Rect( double x, double y, double width, double height ) {
	m_curdc->DrawRectangle( CAST(x), CAST(y), CAST(width), CAST(height) );
}
void wxPLDCOutputDevice::Circle( double x, double y, double radius ) {
	int irad = CAST(radius);
	if ( irad < 1 ) m_curdc->DrawPoint( CAST(x), CAST(y) );
	else m_curdc->DrawCircle( CAST(x), CAST(y), irad );
}
	
void wxPLDCOutputDevice::Font( double relpt, bool bold ) {
	wxFont font( m_font0 );
	if ( relpt != 0 ) font.SetPointSize( font.GetPointSize() + CAST(relpt) );
	font.SetWeight( bold ? wxFONTWEIGHT_BOLD : wxFONTWEIGHT_NORMAL );
	m_curdc->SetFont( font );
	m_fontSize = relpt;
	m_fontBold = bold;
}
	
void wxPLDCOutputDevice::Font( double *rel, bool *bld ) const {
	if ( rel ) *rel = m_fontSize;
	if ( bld ) *bld = m_fontBold;
}

void wxPLDCOutputDevice::Text( const wxString &text, double x, double y, double angle ) {
	if ( angle != 0 ) m_curdc->DrawRotatedText( text, CAST(x), CAST(y), angle );
	else m_curdc->DrawText( text, CAST(x), CAST(y) );
}
void wxPLDCOutputDevice::Measure( const wxString &text, double *width, double *height ) {
	wxSize sz( m_curdc->GetTextExtent( text ) );
	if ( width )  *width = (double)sz.x/m_scale;
	if ( height ) *height = (double)sz.y/m_scale;
}
*/



#define SCALE(x) (m_scale*(x))



wxPLGraphicsOutputDevice::wxPLGraphicsOutputDevice( wxGraphicsContext *gc, double scale, double fontpoints)
	: m_gc(gc), m_scale( scale )
{
	m_fontPoints0 = fontpoints;
	m_fontRelSize = 0;
	m_textColour = *wxBLACK;

	m_pen = m_brush = true;
	Pen( *wxBLACK, 1, SOLID );
	Brush( *wxBLACK, SOLID );
	m_path = gc->CreatePath();
}
	
void wxPLGraphicsOutputDevice::SetAntiAliasing( bool b )
{	
	m_gc->SetAntialiasMode( b ? wxANTIALIAS_DEFAULT : wxANTIALIAS_NONE );
}

bool wxPLGraphicsOutputDevice::GetAntiAliasing() const
{
	return m_gc->GetAntialiasMode() != wxANTIALIAS_NONE;
}

void wxPLGraphicsOutputDevice::Clip( double x, double y, double width, double height )
{
	m_gc->Clip( SCALE(x), SCALE(y), SCALE(width), SCALE(height) );
}

void wxPLGraphicsOutputDevice::Unclip()
{
	m_gc->ResetClip();
}

void wxPLGraphicsOutputDevice::Brush( const wxColour &c, Style sty )
{
	wxBrush brush;
	TranslateBrush( &brush, c, sty );
	m_gc->SetBrush( brush );
	m_brush = (sty!=NONE);
}

void wxPLGraphicsOutputDevice::Pen( const wxColour &c, double size, 
	Style line, Style join, Style cap )
{
	wxPen pen;
	TranslatePen( &pen, c, m_scale*size, line, join, cap );

	// smallest line in pixels supported by Direct2D graphics backend
	// ref: http://trac.wxwidgets.org/changeset/77694/svn-wx
	if ( line!=NONE && pen.GetWidth() < 1 ) 
		pen.SetWidth( 1 );

	m_gc->SetPen( pen );
	m_pen = (line!=NONE);
}

void wxPLGraphicsOutputDevice::Line( double x1, double y1, double x2, double y2 )
{
    m_gc->StrokeLine( SCALE(x1),SCALE(y1),SCALE(x2),SCALE(y2) );
}

void wxPLGraphicsOutputDevice::Lines( size_t n, const wxRealPoint *pts )
{
    wxPoint2DDouble* pointsD = new wxPoint2DDouble[n];
    for( int i = 0; i < n; ++i)
    {
        pointsD[i].m_x = SCALE(pts[i].x);
        pointsD[i].m_y = SCALE(pts[i].y);
    }

    m_gc->StrokeLines( n , pointsD );
    delete[] pointsD;
}

void wxPLGraphicsOutputDevice::Polygon( size_t n, const wxRealPoint *pts, FillRule sty )
{
    bool closeIt = false;
    if (pts[n-1] != pts[0])
        closeIt = true;
	
    wxPoint2DDouble* pointsD = new wxPoint2DDouble[n+(closeIt?1:0)];
    for( int i = 0; i < n; ++i)
    {
        pointsD[i].m_x = SCALE(pts[i].x);
        pointsD[i].m_y = SCALE(pts[i].y);
    }
    if ( closeIt )
        pointsD[n] = pointsD[0];
	
	m_gc->DrawLines( n+(closeIt?1:0), pointsD, sty==ODD_EVEN_RULE ?  wxODDEVEN_RULE : wxWINDING_RULE );
	delete[] pointsD;
}

void wxPLGraphicsOutputDevice::Rect( double x, double y, double width, double height )
{
	m_gc->DrawRectangle( SCALE(x), SCALE(y), SCALE(width), SCALE(height) );
}

void wxPLGraphicsOutputDevice::Circle( double x, double y, double radius )
{
	wxGraphicsPath path = m_gc->CreatePath();
	path.AddCircle( SCALE(x), SCALE(y), SCALE(radius) );
	m_gc->DrawPath( path );
}

void wxPLGraphicsOutputDevice::Sector( double x, double y, double radius, double angle1, double angle2 )
{
	double sa = (angle1-90.0)*M_PI/180.0;
	double ea = (angle2-90.0)*M_PI/180.0;

	wxGraphicsPath path = m_gc->CreatePath();
	path.AddArc( SCALE(x), SCALE(y), SCALE(radius), sa, ea, true );
	path.AddLineToPoint( SCALE(x), SCALE(y) );
	m_gc->DrawPath( path );
}


void wxPLGraphicsOutputDevice::MoveTo( double x, double y )
{
	m_path.MoveToPoint( SCALE(x), SCALE(y) );
}

void wxPLGraphicsOutputDevice::LineTo( double x, double y )
{
	m_path.AddLineToPoint( SCALE(x), SCALE(y) );
}

void wxPLGraphicsOutputDevice::CloseSubPath()
{
	m_path.CloseSubpath();
}

void wxPLGraphicsOutputDevice::Path( FillRule rule )
{
	if ( m_brush && m_pen ) m_gc->DrawPath( m_path, rule == WINDING_RULE ? wxWINDING_RULE : wxODDEVEN_RULE );
	else if ( m_pen ) m_gc->StrokePath( m_path );
	else if ( m_brush ) m_gc->FillPath( m_path, rule == WINDING_RULE ? wxWINDING_RULE : wxODDEVEN_RULE );

	m_path = m_gc->CreatePath();
}

void wxPLGraphicsOutputDevice::TextPoints( double relpt )
{
	m_fontRelSize = relpt;
}

double wxPLGraphicsOutputDevice::TextPoints() const
{
	return m_fontRelSize;
}

void wxPLGraphicsOutputDevice::TextColour( const wxColour &c )
{
	m_textColour = c;
}

//#define FREETYPE_TEXT 1
#define FT_FONT_FACE_DEFAULT 0

#include <wex/utils.h>

void wxPLGraphicsOutputDevice::Text( const wxString &text, double x, double y, double angle )
{
#ifdef FREETYPE_TEXT
	wxPoint pos( (int)SCALE(x), (int)SCALE(y) );
	unsigned int dpi = wxGetDrawingDPI();
	wxFreeTypeDraw( *m_gc, pos, FT_FONT_FACE_DEFAULT, 
		m_fontPoints0+m_fontRelSize, dpi, text, m_textColour, angle );
#else
	if ( angle == 0.0 ) m_gc->DrawText( text, SCALE(x), SCALE(y) );
	else m_gc->DrawText( text, SCALE(x), SCALE(y), angle * 3.1415926/180.0 );
#endif
}


void wxPLGraphicsOutputDevice::Measure( const wxString &text, double *width, double *height )
{
#ifdef FREETYPE_TEXT
	unsigned int dpi = wxGetDrawingDPI();
	wxSize sz = wxFreeTypeMeasure( FT_FONT_FACE_DEFAULT, 
		m_fontPoints0+m_fontRelSize, dpi, text );

	if ( width ) *width = (wxCoord)(sz.x+0.5)/m_scale;
	if ( height ) *height = (wxCoord)(sz.y+0.5)/m_scale;
#else
    wxDouble h , d , e , w;
    m_gc->GetTextExtent( text, &w, &h, &d, &e );
    if ( height )
        *height = (wxCoord)(h+0.5)/m_scale;
    
	//if ( descent )
    //    *descent = (wxCoord)(d+0.5);
    //if ( externalLeading )
    //    *externalLeading = (wxCoord)(e+0.5);

    if ( width )
        *width = (wxCoord)(w+0.5)/m_scale;
#endif
}

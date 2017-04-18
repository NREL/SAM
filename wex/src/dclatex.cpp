/////////////////////////////////////////////////////////////////////////////
// Name:        wx/wxLatexDC.h
// Purpose:     wxLatexDC
// Author:      Christoph Schmidt-Hieber, based on dcsvg by Chris Elliott and
//              dcpsg by Julian Smart, Robert Roebling, Markus Holzhem
// Modified by:
// Created:
// Copyright:   (c) Christoph Schmidt-Hieber
// RCS-ID:      $$
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
    #include "wx/dcmemory.h"
    #include "wx/dcscreen.h"
    #include "wx/icon.h"
    #include "wx/image.h"
#endif

#include <wex/dclatex.h>
#include "wx/wfstream.h"

// conversion
static inline double DegToRad(double deg) { return (deg * M_PI) / 180.0; }
static inline double RadToDeg(double rad) { return (rad * 180.0) / M_PI; }

// ----------------------------------------------------------
//   wxLatexDC
// ----------------------------------------------------------

IMPLEMENT_ABSTRACT_CLASS(wxLatexDCImpl, wxDC)

wxLatexDCImpl::wxLatexDCImpl( wxLatexDC *owner, const wxString &filename, 
                     int width, int height, double dpi ) :
        wxDCImpl( owner )
    {
        Init( filename, width, height, dpi ); 
    }

void wxLatexDCImpl::Init (const wxString &filename, int Width, int Height, double dpi)
{
    m_width = Width ;
    m_height = Height ;
    m_signY = -1;
    m_dpi = dpi;

    m_OK = TRUE;

    m_mm_to_pix_x = dpi/25.4;
    m_mm_to_pix_y = dpi/25.4;

    m_backgroundBrush = *wxTRANSPARENT_BRUSH;
    m_textForegroundColour = *wxBLACK;
    m_textBackgroundColour = *wxWHITE;
    m_colour = wxColourDisplay();
    m_colourcounter = 0;

    m_pen   = *wxBLACK_PEN;
    m_font  = *wxNORMAL_FONT;
    m_brush = *wxWHITE_BRUSH;

    m_graphics_changed = TRUE ;

    ////////////////////code here

    m_outfile = new wxFileOutputStream(filename) ;
    m_OK = m_outfile->Ok ();
    if (m_OK)
    {
        m_filename = filename ;
        m_sub_images = 0 ;
    }
}

wxLatexDCImpl::~wxLatexDCImpl()
{
    // insert final dimensions at the beginning of the stream:
    // THE FOLLOWING HAS BEEN CONTRIBUTED BY Andy Fyfe <andy@hyperparallel.com>

    // Compute the bounding box.  Note that it is in the default user
    // coordinate system, thus we have to convert the values.
    double minX = m_minX;
    double minY = m_minY * m_signY;
    double maxX = m_maxX;
    double maxY = m_maxY * m_signY;

    // LOG2DEV may have changed the minimum to maximum vice versa
    if ( minX > maxX ) { double tmp = minX; minX = maxX; maxX = tmp; }
    if ( minY > maxY ) { double tmp = minY; minY = maxY; maxY = tmp; }

    // Adjust width:
    double pxToCm = 2.54 / m_dpi;
    double widthX = (maxX - minX) * pxToCm; // width in cm

    wxString buffer;
    buffer.Printf(
            "%%%% Creator: wxWidgets Latex renderer\n"
            "%%%% CreationDate: %s\n\n"
            "%s"
            "\\begin{pspicture}(%.2fcm,%.2fcm)\n"
            "\\psset{unit=%.8fcm}\n"
            "\\pstVerb{ 1 setlinejoin }\n\0",
            (const char*)wxNow().c_str(),
			(const char*)m_pendingHeader.c_str(),
			floor(minX) * pxToCm, floor(minY) * pxToCm, pxToCm );
    m_outstring.Prepend(buffer);

    write( wxT("\\end{pspicture}\n") );
    if ( m_outfile ) {
        m_outfile->Write(m_outstring, strlen((const char *)m_outstring));
        m_OK = m_outfile->Ok();
        m_outfile -> Close();
        delete m_outfile ;
    }
}

void wxLatexDCImpl::DoGetSizeMM( int *width, int *height ) const
{
    if (width)
        *width = wxRound( (double)m_width / m_mm_to_pix_x );
        
    if (height)
        *height = wxRound( (double)m_height / m_mm_to_pix_y );
}
   
wxSize wxLatexDCImpl::GetPPI() const
{
    return wxSize( wxRound(m_dpi), wxRound(m_dpi) );
}

void wxLatexDCImpl::DoDrawLine (wxCoord x1, wxCoord y1, wxCoord x2, wxCoord y2)
{
    wxCHECK_RET( m_OK, wxT("invalid latex dc") );

    wxString filled = "[";
    if (m_pen.GetStyle() != wxTRANSPARENT)
    {
        filled = filled + CreatePenString(m_pen);
    } else {
        return;
    }

    write( wxT("\\psline") );

    wxString buffer;
    buffer.Printf( "%s](%f,%f)(%f,%f)",
            (const char*)filled.c_str(), XLOG2DEV(x1), YLOG2DEV(y1),
            XLOG2DEV(x2), YLOG2DEV(y2) );
    write( buffer );

    CalcBoundingBox( x1, y1 );
    CalcBoundingBox( x2, y2 );
    write( wxT("\n") );
}

void wxLatexDCImpl::DoDrawLines(int n, const wxPoint points[], wxCoord xoffset , wxCoord yoffset )
{
    wxCHECK_RET( m_OK, wxT("invalid latex dc") );

    if (n <= 0) return;

    wxString filled = "[";
    if (m_pen.GetStyle() != wxTRANSPARENT)
    {
        filled = filled + CreatePenString(m_pen);
    } else {
        return;
    }
    write( wxT("\\psline") );

    wxString buffer;
    buffer.Printf( "%s]", (const char*)filled.c_str() );
    // buffer.Replace( ",", "." );
    write( buffer );

    int i;
    for ( i =0; i<n ; i++ )
        CalcBoundingBox( points[i].x+xoffset, points[i].y+yoffset );

    for (i = 0; i < n; i++)
    {
        buffer.Printf( wxT("(%f,%f)"),
                  XLOG2DEV(points[i].x+xoffset),
                  YLOG2DEV(points[i].y+yoffset) );
        write( buffer );
    }
    write( wxT("\n") );
}

// TODO:
// void wxLatexDCImpl::DoDrawSpline( const wxPointList* points ) { }

void wxLatexDCImpl::DoDrawPoint (wxCoord x1, wxCoord y1)
{
    wxCHECK_RET( m_OK, wxT("invalid latex dc") );

    wxString filled = "[";
    if (m_pen.GetStyle() != wxTRANSPARENT)
    {
        filled = filled + CreatePenString(m_pen);
    } else {
        return;
    }

    wxString buffer;
    buffer.Printf( wxT("\\psdot%s](%f,%f)\n"), (const char*)filled.c_str(), XLOG2DEV(x1), YLOG2DEV(y1) );
    write( buffer );

    CalcBoundingBox( x1, y1 );
}


void wxLatexDCImpl::DoDrawCheckMark(wxCoord x1, wxCoord y1, wxCoord width, wxCoord height)
{
    wxDCImpl::DoDrawCheckMark (x1,y1,width,height) ;
}


void wxLatexDCImpl::DoDrawText(const wxString& text, wxCoord x1, wxCoord y1)
{
    wxCHECK_RET( m_OK, wxT("invalid latex dc") );

    const wxWX2MBbuf textbuf = text.mb_str();
    if ( !textbuf )
        return;

    wxString filled = "";
    if (m_textForegroundColour.Ok())
    {
        wxString buffer;
        buffer.Printf( wxT("\\color{%s} "),
                       (const char*)CreateColourString( m_textForegroundColour ).c_str() );
        filled = filled + buffer;
    }

    wxString buffer;
    buffer.Printf( wxT("\\rput[lt](%f,%f){%s"), XLOG2DEV(x1), YLOG2DEV(y1), (const char*)filled.c_str());
    write( buffer );

    size_t len = strlen(textbuf);
    size_t i;
    for (i = 0; i < len; i++)
    {
        int c = (unsigned char) textbuf[i];
        if (c == ')' || c == '(' || c == '\\')
        {
            /* Cope with special characters */
            write( "\\" );
            write( (char) c );
        }
        else if ( c >= 128 )
        {
            /* Cope with character codes > 127 */
            buffer.Printf( "\\%o", c );
            write( buffer );
        }
        else
        {
            write( (char) c );
        }
    }

    write( wxT("}\n") );

    if (m_font.GetUnderlined())
    {
        // TODO
    }

    CalcBoundingBox( x1, y1 );
    CalcBoundingBox( x1 + text.length(), y1 );
}


void wxLatexDCImpl::DoDrawRotatedText(const wxString& sText, wxCoord x, wxCoord y, double angle)
{
    if ( wxIsNullDouble(angle) )
    {
        DoDrawText(sText, x, y);
        return;
    }

    wxCHECK_RET( m_OK, wxT("invalid latex dc") );

    wxString filled = "";
    if (m_textForegroundColour.Ok())
    {
        wxString buffer;
        buffer.Printf( wxT("\\color{%s} "),
                       (const char*)CreateColourString( m_textForegroundColour ).c_str() );
        filled = filled + buffer;
    }

    wxString buffer;
    buffer.Printf( wxT("\\rput[lt]{%f}(%f,%f){%s"), angle, XLOG2DEV(x), YLOG2DEV(y), (const char*)filled.c_str());
    write( buffer );

    const wxWX2MBbuf textbuf = sText.mb_str();
    size_t len = strlen(textbuf);
    size_t i;
    for (i = 0; i < len; i++)
    {
        int c = (unsigned char) textbuf[i];
        if (c == ')' || c == '(' || c == '\\')
        {
            /* Cope with special characters */
            write( "\\" );
            write( (char) c );
        }
        else if ( c >= 128 )
        {
            /* Cope with character codes > 127 */
            buffer.Printf( "\\%o", c);
            write( buffer );
        }
        else
        {
            write( (char) c );
        }
    }

    write( wxT("}\n") );

    if (m_font.GetUnderlined())
    {
        // TODO
    }

    CalcBoundingBox( x, y );
}

void wxLatexDCImpl::DrawLabelLatex(const wxString& text, const wxRect& rect,
        int alignment, int indexAccel)
{
    wxCHECK_RET( m_OK, wxT("invalid latex dc") );

    const wxWX2MBbuf textbuf = text.mb_str();
    if ( !textbuf )
        return;

    wxString filled = "";
    if (m_textForegroundColour.Ok())
    {
        wxString buffer;
        buffer.Printf( wxT("\\color{%s} "),
                       (const char*)CreateColourString( m_textForegroundColour ).c_str() );
        filled = filled + buffer;
    }

    // reference point for x:
    char xc_refpoint;
    double xf_refpoint;
    if ( alignment & wxALIGN_CENTRE_HORIZONTAL ) {
        xc_refpoint =  ' ';
        xf_refpoint = (double)rect.x + ((double)rect.width / 2.0);
    } else if (alignment & wxALIGN_RIGHT ) {
        xc_refpoint = 'r'; 
        xf_refpoint = (double)rect.x + (double)rect.width;
    } else { // wxALIGN_LEFT: 
        xc_refpoint = 'l';
        xf_refpoint = (double)rect.x;
    }
    char yc_refpoint;
    double yf_refpoint;
    if ( alignment & wxALIGN_CENTRE_VERTICAL ) {   
        yc_refpoint =  ' ';
        yf_refpoint = (double)rect.y + ((double)rect.height / 2.0);
    } else if ( alignment & wxALIGN_BOTTOM ){
        yc_refpoint = 'b'; 
        yf_refpoint = (double)rect.y + (double)rect.height;
    } else { // wxALIGN_TOP
        yc_refpoint = 't';
        yf_refpoint = (double)rect.y;
    }
    // Find the center of the original text:
    write( wxT("\\rput[") );
    write( xc_refpoint );
    write( yc_refpoint );
    wxString buffer;
    buffer.Printf( "](%f,%f){%s", XLOG2DEV(xf_refpoint), YLOG2DEV(yf_refpoint), 
		(const char*)filled.c_str() );
    write( buffer );

    size_t len = strlen(textbuf);
    size_t i;
    for (i = 0; i < len; i++)
    {
        int c = (unsigned char) textbuf[i];
        if (c == ')' || c == '(' || c == '\\')
        {
            /* Cope with special characters */
            write( "\\" );
            write( (char) c );
        }
        else if ( c >= 128 )
        {
            /* Cope with character codes > 127 */
            buffer.Printf( "\\%o", c );
            write( buffer );
        }
        else
        {
            write( (char) c );
        }
    }

    write( wxT("}\n") );

    if (m_font.GetUnderlined())
    {
        // TODO
    }

    CalcBoundingBox( rect.x, rect.y );
}

void wxLatexDCImpl::DoDrawRectangle(wxCoord x, wxCoord y, wxCoord width, wxCoord height)
{
    DoDrawRoundedRectangle(x,y,width,height,0);
}


void wxLatexDCImpl::DoDrawRoundedRectangle(wxCoord x, wxCoord y, wxCoord width, wxCoord height, double radius )
{
    wxCHECK_RET( m_OK, wxT("invalid latex dc") );

    width--;
    height--;
    wxString filled = "[";
    if (m_brush.GetStyle () != wxTRANSPARENT) {
        filled = filled + CreateBrushString(m_brush);
    }
    
    if (m_pen.GetStyle() != wxTRANSPARENT)
    {
        filled = filled + CreatePenString(m_pen);
    } else {
        return;
    }

    // radius is expressed in units of the shortest edge:
    double shortestEdge = (double)((width < height) ? width : height);
    // a radius of 1 refers to half of the shortes edge:
    double psradius = radius / ( 0.5 * shortestEdge );
    // avoid rounding errors:
    psradius *= 1e6;
    wxString buffer;
    buffer.Printf( wxT("\\psframe%s,framearc=%f](%f,%f)(%f,%f)\n"),
                   (const char*)filled.c_str(), XLOG2DEV(psradius)/1e6, XLOG2DEV(x), YLOG2DEV(y),
                   XLOG2DEV(width), YLOG2DEV(height) );
    write( buffer );
    CalcBoundingBox( x, y );
    CalcBoundingBox( x + width, y + height );
}

void wxLatexDCImpl::DoDrawPolygon(int n, const wxPoint points[], wxCoord xoffset, wxCoord yoffset,wxPolygonFillMode fillStyle)
{
    wxCHECK_RET( m_OK, wxT("invalid latex dc") );

    wxString filled = "[";
    if (m_brush.GetStyle () != wxTRANSPARENT) {
        filled = filled + CreateBrushString(m_brush);
    }
    
    if (m_pen.GetStyle() != wxTRANSPARENT)
    {
        filled = filled + CreatePenString(m_pen);
    } else {
        return;
    }
    write( wxT("\\pspolygon") );

    wxString buffer;
    buffer.Printf( wxT("%s]"), (const char*)filled.c_str() );
    // buffer.Replace( ",", "." );
    write( buffer );
    
    int i;
    for ( i =0; i<n ; i++ )
        CalcBoundingBox( points[i].x+xoffset, points[i].y+yoffset );

    for (i = 0; i < n; i++)
    {
        buffer.Printf( wxT("(%f,%f)"),
                  XLOG2DEV(points[i].x+xoffset),
                  YLOG2DEV(points[i].y+yoffset) );
        write( buffer );
    }
    write( wxT("\n") );
}


void wxLatexDCImpl::DoDrawEllipse (wxCoord x, wxCoord y, wxCoord w, wxCoord h)
{
    // x and y specify the upper left corner; need to correct for that:
    double xc = (double) x + (double)w / 2.0;
    double yc = (double) y + (double)h / 2.0;

    wxCHECK_RET( m_OK, wxT("invalid latex dc") );

    wxString filled = "[";
    if (m_brush.GetStyle () != wxTRANSPARENT) {
        filled = filled + CreateBrushString(m_brush);
    }
    
    if (m_pen.GetStyle() != wxTRANSPARENT)
    {
        filled = filled + CreatePenString(m_pen);
    } else {
        return;
    }
    write( wxT("\\psellipse") );
    
    wxString buffer;
    buffer.Printf( wxT("%s](%f,%f)(%f,%f)\n"),
                   (const char*)filled.c_str(), XLOG2DEV(xc), YLOG2DEV(yc),
                   XLOG2DEVREL(w/2.0),YLOG2DEVREL(h/2.0) );
    write( buffer );

    CalcBoundingBox( x ,y );
    CalcBoundingBox( x+w, y+h );}


void wxLatexDCImpl::DoDrawArc(wxCoord x1, wxCoord y1, wxCoord x2, wxCoord y2, wxCoord xc, wxCoord yc)
{
    wxCHECK_RET( m_OK, wxT("invalid latex dc") );

    double dx = (double)(x1 - xc);
    double dy = (double)(y1 - yc);
    double radius = sqrt( dx*dx+dy*dy );
    double alpha1, alpha2;

    if (x1 == x2 && y1 == y2)
    {
        alpha1 = 0.0;
        alpha2 = 360.0;
    }
    else if ( wxIsNullDouble(radius) )
    {
        alpha1 =
        alpha2 = 0.0;
    }
    else
    {
        alpha1 = (x1 - xc == 0) ?
            (y1 - yc < 0) ? 90.0 : -90.0 :
                RadToDeg(-atan2(double(y1-yc), double(x1-xc)));
        alpha2 = (x2 - xc == 0) ?
            (y2 - yc < 0) ? 90.0 : -90.0 :
                RadToDeg(-atan2(double(y2-yc), double(x2-xc)));
    }
    while (alpha1 <= 0)   alpha1 += 360;
    while (alpha2 <= 0)   alpha2 += 360; // adjust angles to be between
    while (alpha1 > 360)  alpha1 -= 360; // 0 and 360 degree
    while (alpha2 > 360)  alpha2 -= 360;

    int i_radius = wxRound( radius );

    wxString filled = "[";
    if (m_brush.GetStyle () != wxTRANSPARENT) {
        filled = filled + CreateBrushString(m_brush);
    }
    
    if (m_pen.GetStyle() != wxTRANSPARENT)
    {
        filled = filled + CreatePenString(m_pen);
    } else {
        return;
    }
    write( wxT("\\pswedge") );
    
    wxString buffer;
    buffer.Printf(  wxT("%s](%f,%f){%f}{%f}{%f}\n"),
                   (const char*)filled.c_str(), XLOG2DEV(xc), YLOG2DEV(yc),
                   XLOG2DEVREL(radius),alpha1,alpha2 );
    write( buffer );

    CalcBoundingBox( xc-i_radius, yc-i_radius );
    CalcBoundingBox( xc+i_radius, yc+i_radius );
}


void wxLatexDCImpl::DoDrawEllipticArc(wxCoord x,wxCoord y,wxCoord w,wxCoord h,double sa,double ea)
{
    wxCHECK_RET( m_OK, wxT("invalid latex dc") );

    // x and y specify the upper left corner; need to correct for that:
    double xc = (double) x + (double)w / 2.0;
    double yc = (double) y + (double)h / 2.0;

    if ( sa >= 360 || sa <= -360 )
        sa -= int(sa/360)*360;
    if ( ea >= 360 || ea <=- 360 )
        ea -= int(ea/360)*360;
    if ( sa < 0 )
        sa += 360;
    if ( ea < 0 )
        ea += 360;

    if ( wxIsSameDouble(sa, ea) )
    {
        DoDrawEllipse(x,y,w,h);
        return;
    }

    wxString filled = "[";
    if (m_brush.GetStyle () != wxTRANSPARENT) {
        filled = filled + CreateBrushString(m_brush);
    }
    
    if (m_pen.GetStyle() != wxTRANSPARENT)
    {
        filled = filled + CreatePenString(m_pen);
    } else {
        return;
    }
    write( wxT("\\psellipticwedge") );
    
    wxString buffer;
    buffer.Printf(  wxT("%s](%f,%f)(%f,%f){%f}{%f}\n"),
                   (const char*)filled.c_str(), XLOG2DEV(xc), YLOG2DEV(yc),
                   XLOG2DEVREL(w/2),YLOG2DEVREL(h/2),sa,ea );
    write( buffer );

    CalcBoundingBox( x ,y );
    CalcBoundingBox( x+w, y+h );
}

/// Set Functions /////////////////////////////////////////////////////////////////
void wxLatexDCImpl::SetBackground( const wxBrush &brush )
{

    m_backgroundBrush = brush;
    return;
}


void wxLatexDCImpl::SetBackgroundMode( int mode )
{
    m_backgroundMode = mode;
    return;
}


void wxLatexDCImpl::SetBrush(const wxBrush& brush)

{
    wxCHECK_RET( m_OK, wxT("invalid latex dc") );

    if (!brush.Ok()) return;

    m_brush = brush;
}


void wxLatexDCImpl::SetPen(const wxPen& pen)
{
    wxCHECK_RET( m_OK, wxT("invalid latex dc") );
    m_pen = pen;

    if (!pen.Ok()) return;

    m_pen = pen;
}

// export a bitmap as a raster image in png
bool wxLatexDCImpl::DoBlit(wxCoord xdest, wxCoord ydest, wxCoord width, wxCoord height,
                         wxDC* source, wxCoord xsrc, wxCoord ysrc,
                         wxRasterOperationMode logicalFunc /*= wxCOPY*/, bool useMask /*= FALSE*/,
                         wxCoord /*xsrcMask = -1*/, wxCoord /*ysrcMask = -1*/)
{
    wxFAIL_MSG( wxT("wxLatexDCImpl::DoBlit not implemented.") );
    return FALSE;
}

void wxLatexDCImpl::DoDrawIcon(const class wxIcon & myIcon, wxCoord x, wxCoord y)
{
    DoDrawBitmap( myIcon, x, y, true );
}

void wxLatexDCImpl::DoDrawBitmap(const class wxBitmap & bmp, wxCoord x, wxCoord y , bool  WXUNUSED(bTransparent) /*=0*/ )
{
    wxFAIL_MSG( wxT("wxLatexDCImpl::DoDrawBitmap not implemented.") );
}

void wxLatexDCImpl::write(const wxString &s)
{
    m_outstring.Append(s);
}

wxString wxLatexDCImpl::CreateColourString( const wxColour &colour )
{
    wxString buffer;
    buffer.Printf( wxT("\\definecolor{c%d}{rgb}{%f,%f,%f}\n"), 
                      m_colourcounter,
                      (double)colour.Red()/255.0,
                      (double)colour.Green()/255.0,
                      (double)colour.Blue()/255.0 );
    write( buffer );
    
    wxString colourName;
    colourName.Printf( wxT("c%d"), m_colourcounter++ );
    return colourName;
}

wxString wxLatexDCImpl::CreatePenString( const wxPen &pen )
{
    wxString style;
    switch (m_pen.GetStyle())
    {
        case wxDOT:           style = wxT("dotted"); break;
        case wxSHORT_DASH:    
        case wxLONG_DASH:     // TODO: 
        case wxDOT_DASH:      // implement all these dash variations
        case wxUSER_DASH:     style = wxT("dashed"); break;
        case wxSOLID:
        case wxTRANSPARENT:
        default:              style = wxT("solid");
    }
    wxString buffer;
    double width = (double)( (pen.GetWidth() <= 0) ? 0.1 : pen.GetWidth() );
    buffer.Printf( wxT("linecolor=%s,linewidth=%f,linestyle=%s"),
                   (const char*)CreateColourString(pen.GetColour()).c_str(), width, (const char*)style.c_str() );
    return buffer;
}

wxString wxLatexDCImpl::CreateBrushString( const wxBrush &brush )
{
    wxString buffer;
    // TODO: implement fill styles
    buffer.Printf( wxT("fillstyle=solid,fillcolor=%s,"),
                   (const char*)CreateColourString(brush.GetColour()).c_str() );
    return buffer;
}

#ifdef __BORLANDC__
#pragma warn .rch
#pragma warn .ccc
#endif


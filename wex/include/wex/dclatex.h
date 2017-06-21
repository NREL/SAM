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

#ifndef _WX_DCLATEX_H_
#define _WX_DCLATEX_H_

#include "wx/string.h"
#include "wx/dc.h"

#ifdef __BORLANDC__
#pragma warn -8008
#pragma warn -8066
#endif

class WXDLLIMPEXP_FWD_BASE wxFileOutputStream;

class WXDLLIMPEXP_FWD_BASE wxLatexDC;

class WXDLLIMPEXP_CORE wxLatexDCImpl : public wxDCImpl
{
public:
    wxLatexDCImpl( wxLatexDC *owner, const wxString &filename, 
                     int width=320, int height=240, double dpi=72.0 );

    virtual ~wxLatexDCImpl();
    
    bool IsOk() const { return m_OK; }

    virtual bool CanDrawBitmap() const { return false; }
    virtual bool CanGetTextExtent() const { return false; }

    virtual int GetDepth() const
    {
        wxFAIL_MSG(wxT("wxLatexDC::GetDepth Call not implemented"));
        return -1;
    }

    virtual void Clear()
    {
        wxFAIL_MSG(wxT("wxLatexDC::Clear() Call not implemented \nNot sensible for an output file?"));
    }

    virtual void DestroyClippingRegion()
    {
        wxFAIL_MSG(wxT("wxLatexDC::DestroyClippingRegion not yet implemented"));
    }

    virtual wxCoord GetCharHeight() const
    {
        wxFAIL_MSG(wxT("wxLatexDC::GetCharHeight not implemented"));
        return wxCoord(0);
    }

    virtual wxCoord GetCharWidth() const
    {
        wxFAIL_MSG(wxT("wxLatexDC::GetCharWidth not implemented"));
        return wxCoord(0);
    }

    virtual void SetClippingRegion(wxCoord WXUNUSED(x), wxCoord WXUNUSED(y),
                                   wxCoord WXUNUSED(w), wxCoord WXUNUSED(h))
    {
        wxFAIL_MSG(wxT("wxLatexDC::SetClippingRegion not implemented"));
    }

    virtual void SetPalette(const wxPalette&  WXUNUSED(palette))
    {
        wxFAIL_MSG(wxT("wxLatexDC::SetPalette not implemented"));
    }

    virtual void GetClippingBox(wxCoord *WXUNUSED(x), wxCoord *WXUNUSED(y),
                                wxCoord *WXUNUSED(w), wxCoord *WXUNUSED(h))
    {
        wxFAIL_MSG(wxT("wxLatexDC::GetClippingBox not implemented"));
    }

    virtual void SetLogicalFunction( wxRasterOperationMode WXUNUSED(function))
    {
        wxFAIL_MSG(wxT("wxLatexDC::SetLogicalFunction Call not implemented"));
    }

    virtual wxRasterOperationMode GetLogicalFunction() const
    {
        wxFAIL_MSG(wxT("wxLatexDC::GetLogicalFunction() not implemented"));
        return wxCOPY;
    }

    virtual void SetBackground( const wxBrush &brush );
    virtual void SetBackgroundMode( int mode );
    virtual void SetBrush(const wxBrush& brush);
    virtual void SetFont(const wxFont& font)
    {
        wxFAIL_MSG(wxT("wxLatexDC::SetFont() not implemented"));
    }
    virtual void SetPen(const wxPen& pen);

    void DrawLabelLatex(const wxString& text, const wxRect& rect,
                        int alignment = wxALIGN_LEFT | wxALIGN_TOP,
                        int indexAccel = -1);
    
private:
   virtual bool DoGetPixel(wxCoord, wxCoord, wxColour *) const
   {
       wxFAIL_MSG(wxT("wxLatexDC::DoGetPixel Call not implemented"));
       return true;
   }

   virtual bool DoBlit(wxCoord, wxCoord, wxCoord, wxCoord, wxDC *,
                       wxCoord, wxCoord, wxRasterOperationMode = wxCOPY,
                       bool = 0, wxCoord = -1, wxCoord = -1);

   virtual void DoCrossHair(wxCoord, wxCoord)
   {
       wxFAIL_MSG(wxT("wxLatexDC::CrossHair Call not implemented"));
   }

   virtual void DoDrawArc(wxCoord, wxCoord, wxCoord, wxCoord, wxCoord, wxCoord);

   virtual void DoDrawBitmap(const wxBitmap &, wxCoord, wxCoord, bool = 0);

   virtual void DoDrawCheckMark(wxCoord x, wxCoord y, wxCoord w, wxCoord h);

   virtual void DoDrawEllipse(wxCoord x, wxCoord y, wxCoord w, wxCoord h);

   virtual void DoDrawEllipticArc(wxCoord x, wxCoord y, wxCoord w, wxCoord h,
                                  double sa, double ea);

   virtual void DoDrawIcon(const wxIcon &, wxCoord, wxCoord);

   virtual void DoDrawLine (wxCoord x1, wxCoord y1, wxCoord x2, wxCoord y2);

   virtual void DoDrawLines(int n, const wxPoint points[],
                            wxCoord xoffset = 0, wxCoord yoffset = 0);

   // TODO: 
   // virtual void DoDrawSpline(const wxPointList* points);
   
   virtual void DoDrawPoint(wxCoord, wxCoord);

   virtual void DoDrawPolygon(int n, const wxPoint points[], wxCoord xoffset, wxCoord yoffset,wxPolygonFillMode fillStyle);

   virtual void DoDrawRectangle(wxCoord x, wxCoord y, wxCoord w, wxCoord h);

   virtual void DoDrawRotatedText(const wxString& text, wxCoord x, wxCoord y,
                                  double angle);

   virtual void DoDrawRoundedRectangle(wxCoord x, wxCoord y,
                                       wxCoord w, wxCoord h,
                                       double radius = 20) ;

   virtual void DoDrawText(const wxString& text, wxCoord x, wxCoord y);

   virtual bool DoFloodFill(wxCoord WXUNUSED(x), wxCoord WXUNUSED(y),
                            const wxColour& WXUNUSED(col),
                            wxFloodFillStyle WXUNUSED(style) = wxFLOOD_SURFACE)
   {
       wxFAIL_MSG(wxT("wxLatexDC::DoFloodFill Call not implemented"));
       return false;
   }

   virtual void DoGetSize(int * x, int *y) const
   {
       if ( x )
           *x = m_width;
       if ( y )
           *y = m_height;
   }

   virtual void DoGetTextExtent(const wxString& string, wxCoord *w, wxCoord *h,
                                wxCoord *descent = NULL,
                                wxCoord *externalLeading = NULL,
                                const wxFont *font = NULL) const
   {
       wxFAIL_MSG(wxT("wxLatexDC::DoGetTextExtent not yet implemented"));
   }

   
   virtual void DoSetDeviceClippingRegion(const wxRegion& region)
   {
      wxFAIL_MSG(wxT("wxLatexDC::DoSetDeviceClippingRegion not yet implemented")); 
   }

   virtual void DoSetClippingRegionAsRegion(const wxRegion& WXUNUSED(region))
   {
       wxFAIL_MSG(wxT("wxLatexDC::DoSetClippingRegionAsRegion not yet implemented"));
   }

   virtual void DoSetClippingRegion( int WXUNUSED(x),  int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height) )
   {
       wxFAIL_MSG(wxT("wxLatexDC::DoSetClippingRegion not yet implemented"));
   }

   virtual void DoGetSizeMM( int *width, int *height ) const;
   
   virtual wxSize GetPPI() const;

   void Init (const wxString &filename, int width, int height, double dpi);

   void write( const wxString &s );

private:
   wxFileOutputStream *m_outfile;
   wxString            m_filename;
   wxString            m_outstring;
   int                 m_sub_images; // number of png format images we have
   bool                m_OK;
   bool                m_graphics_changed;
   int                 m_width, m_height;
   double              m_dpi;
   int                 m_colourcounter;
   wxString            m_pendingHeader;
   
   double XLOG2DEV(wxCoord x) const { return (double)LogicalToDeviceX(x); }
   double XLOG2DEVREL(wxCoord x) const { return (double)LogicalToDeviceXRel(x); }
   double YLOG2DEV(wxCoord y) const { return (double)LogicalToDeviceY(y); }
   double YLOG2DEVREL(wxCoord y) const { return (double)LogicalToDeviceYRel(y); }
   
   wxString CreateColourString(const wxColour& colour);
   wxString CreatePenString(const wxPen& pen);
   wxString CreateBrushString(const wxBrush& brush);

private:
   DECLARE_ABSTRACT_CLASS(wxLatexDCImpl)
};


class WXDLLIMPEXP_CORE wxLatexDC : public wxDC
{
public:
    wxLatexDC(const wxString& filename, 
                int width = 320,
                int height = 240,
                double dpi = 72.0)
        : wxDC(new wxLatexDCImpl(this, filename, width, height, dpi))
    { 
    }
};

#endif // _WX_DCLATEX_H_


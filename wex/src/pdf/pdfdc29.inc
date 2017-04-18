///////////////////////////////////////////////////////////////////////////////
// Name:        pdfdc29.inc
// Purpose:     Implementation of wxPdfDC for wxWidgets 2.9.x
// Author:      Ulrich Telle
// Modified by:
// Created:     2010-11-28
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfdc29.inc Implementation of the wxPdfDC for wxWidgets 2.9.x

#include <wx/region.h>
#include <wx/font.h>
#include <wx/paper.h>
#include <wx/tokenzr.h>

#include "wex/pdf/pdfdc.h"
#include "wex/pdf/pdffontmanager.h"

#include <math.h>

//-------------------------------------------------------------------------------
// wxPostScriptDC
//-------------------------------------------------------------------------------


IMPLEMENT_DYNAMIC_CLASS(wxPdfDC, wxDC)

wxPdfDC::wxPdfDC()
  : wxDC(new wxPdfDCImpl(this))
{
}

wxPdfDC::wxPdfDC(const wxPrintData& printData)
  : wxDC(new wxPdfDCImpl(this, printData))
{
}

wxPdfDC::wxPdfDC(wxPdfDocument* pdfDocument, double templateWidth, double templateHeight)
  : wxDC(new wxPdfDCImpl(this, pdfDocument, templateWidth, templateHeight))
{
}

wxPdfDocument*
wxPdfDC::GetPdfDocument()
{
  return ((wxPdfDCImpl*) m_pimpl)->GetPdfDocument();
}

void
wxPdfDC::SetResolution(int ppi)
{
  ((wxPdfDCImpl*) m_pimpl)->SetResolution(ppi);
}

int
wxPdfDC::GetResolution() const
{
  return ((wxPdfDCImpl*) m_pimpl)->GetResolution();
}

void
wxPdfDC::SetImageType(wxBitmapType bitmapType, int quality)
{
  ((wxPdfDCImpl*) m_pimpl)->SetImageType(bitmapType, quality);
}

void
wxPdfDC::SetMapModeStyle(wxPdfMapModeStyle style)
{
  ((wxPdfDCImpl*) m_pimpl)->SetMapModeStyle(style);
}

wxPdfMapModeStyle
wxPdfDC::GetMapModeStyle() const
{
  return ((wxPdfDCImpl*) m_pimpl)->GetMapModeStyle();
}

static double
angleByCoords(wxCoord xa, wxCoord ya, wxCoord xc, wxCoord yc)
{
  static double pi = 4. * atan(1.0);
  double diffX = xa - xc, diffY = -(ya - yc), ret = 0;
  if (diffX == 0) // singularity
  {
    ret = diffY > 0 ? 90 : -90;
  }
  else if (diffX >= 0) // quadrants 1, 4
  {
    ret = (atan(diffY / diffX) * 180.0 / pi);
  }
  else // quadrants 2, 3
  {
    ret = 180 + (atan(diffY / diffX) * 180.0 / pi);
  }
  return ret;
}

/*
 * PDF device context
 *
 */

IMPLEMENT_ABSTRACT_CLASS(wxPdfDCImpl, wxDCImpl)

wxPdfDCImpl::wxPdfDCImpl(wxPdfDC* owner)
  : wxDCImpl(owner)
{
  Init();
  m_ok = true;
}

wxPdfDCImpl::wxPdfDCImpl(wxPdfDC* owner, const wxPrintData& data)
  : wxDCImpl(owner)
{
  Init();
  SetPrintData(data);
  m_ok = true;
}

wxPdfDCImpl::wxPdfDCImpl(wxPdfDC* owner, const wxString& file, int w, int h)
  : wxDCImpl(owner)
{
  Init();
  m_printData.SetFilename(file);
  m_ok = true;
}

wxPdfDCImpl::wxPdfDCImpl(wxPdfDC* owner, wxPdfDocument* pdfDocument, double templateWidth, double templateHeight)
  : wxDCImpl(owner)
{
  Init();
  m_templateMode = true;
  m_templateWidth = templateWidth;
  m_templateHeight = templateHeight;
  m_pdfDocument = pdfDocument;
}

wxPdfDCImpl::~wxPdfDCImpl()
{
  if (m_pdfDocument != NULL)
  {
    if (!m_templateMode)
    {
      delete m_pdfDocument;
    }
  }
}

void
wxPdfDCImpl::Init()
{
  m_templateMode = false;
  m_ppi = 72;
  m_pdfDocument = NULL;
  m_imageCount = 0;

  wxScreenDC screendc;
  m_ppiPdfFont = screendc.GetPPI().GetHeight();
  m_mappingModeStyle = wxPDF_MAPMODESTYLE_STANDARD;

  m_jpegFormat = false;
  m_jpegQuality = 75;

  SetBackgroundMode(wxSOLID);

  m_printData.SetOrientation(wxPORTRAIT);
  m_printData.SetPaperId(wxPAPER_A4);
  m_printData.SetFilename(wxT("default.pdf"));
}

wxPdfDocument*
wxPdfDCImpl::GetPdfDocument()
{
  return m_pdfDocument;
}

void
wxPdfDCImpl::SetPrintData(const wxPrintData& data)
{
  m_printData = data;
  wxPaperSize id = m_printData.GetPaperId();
  wxPrintPaperType* paper = wxThePrintPaperDatabase->FindPaperType(id);
  if (!paper)
  {
    m_printData.SetPaperId(wxPAPER_A4);
  }
}

void
wxPdfDCImpl::SetResolution(int ppi)
{
  m_ppi = ppi;
}

int
wxPdfDCImpl::GetResolution() const
{
  return (int) m_ppi;
}

void
wxPdfDCImpl::SetImageType(wxBitmapType bitmapType, int quality)
{
  m_jpegFormat = (bitmapType == wxBITMAP_TYPE_JPEG);
  m_jpegQuality = (quality >= 0) ? ((quality <= 100) ? quality : 75) : 75;
}

void
wxPdfDCImpl::Clear()
{
  // Not yet implemented
}

bool
wxPdfDCImpl::StartDoc(const wxString& message)
{
  wxCHECK_MSG(m_ok, false, wxT("Invalid PDF DC"));
  wxUnusedVar(message);
  if (!m_templateMode && m_pdfDocument == NULL)
  {
    m_pdfDocument = new wxPdfDocument(m_printData.GetOrientation(), wxString(wxT("pt")), m_printData.GetPaperId());
    m_pdfDocument->Open();
    //m_pdfDocument->SetCompression(false);
    m_pdfDocument->SetAuthor(wxT("wxPdfDC"));
    m_pdfDocument->SetTitle(wxT("wxPdfDC"));

    SetBrush(*wxBLACK_BRUSH);
    SetPen(*wxBLACK_PEN);
    SetBackground(*wxWHITE_BRUSH);
    SetTextForeground(*wxBLACK);
    SetDeviceOrigin(0, 0);
  }
  return true;
}

void
wxPdfDCImpl::EndDoc()
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  if (!m_templateMode)
  {
    m_pdfDocument->SaveAsFile(m_printData.GetFilename());
    delete m_pdfDocument;
    m_pdfDocument = NULL;
  }
}

void
wxPdfDCImpl::StartPage()
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  if (!m_templateMode)
  {
    // Begin a new page
    // Library needs it this way (always landscape) to size the page correctly
    m_pdfDocument->AddPage(m_printData.GetOrientation());
    wxPdfLineStyle style = m_pdfDocument->GetLineStyle();
    style.SetWidth(1.0);
    style.SetColour(wxPdfColour(0, 0, 0));
    style.SetLineCap(wxPDF_LINECAP_ROUND);
    style.SetLineJoin(wxPDF_LINEJOIN_MITER);
    m_pdfDocument->SetLineStyle(style);
  }
}

void
wxPdfDCImpl::EndPage()
{
  if (m_ok)
  {
    if (m_clipping)
    {
      DestroyClippingRegion();
    }
  }
}

void
wxPdfDCImpl::SetFont(const wxFont& font)
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  m_font = font;
  if (!font.IsOk())
  {
    return;
  }
  int styles = wxPDF_FONTSTYLE_REGULAR;
  if (font.GetWeight() == wxFONTWEIGHT_BOLD)
  {
    styles |= wxPDF_FONTSTYLE_BOLD;
  }
  if (font.GetStyle() == wxFONTSTYLE_ITALIC)
  {
    styles |= wxPDF_FONTSTYLE_ITALIC;
  }
  if (font.GetUnderlined())
  {
    styles |= wxPDF_FONTSTYLE_UNDERLINE;
  }

  wxPdfFont regFont = wxPdfFontManager::GetFontManager()->GetFont(font.GetFaceName(), styles);
  bool ok = regFont.IsValid();
  if (!ok)
  {
    regFont = wxPdfFontManager::GetFontManager()->RegisterFont(font, font.GetFaceName());
    ok = regFont.IsValid();
  }

  if (ok)
  {
    ok = m_pdfDocument->SetFont(regFont, styles, ScaleFontSizeToPdf(font.GetPointSize()));
  }
}

void
wxPdfDCImpl::SetPen(const wxPen& pen)
{
  if (pen.Ok())
  {
    m_pen = pen;
  }
}

void
wxPdfDCImpl::SetBrush(const wxBrush& brush)
{
  if (brush.Ok())
  {
    m_brush = brush;
  }
}

void
wxPdfDCImpl::SetBackground(const wxBrush& brush)
{
  if (brush.Ok())
  {
    m_backgroundBrush = brush;
  }
}

void
wxPdfDCImpl::SetBackgroundMode(int mode)
{
  // TODO: check implementation
  m_backgroundMode = (mode == wxTRANSPARENT) ? wxTRANSPARENT : wxSOLID;
}

void
wxPdfDCImpl::SetPalette(const wxPalette& palette)
{
  // Not yet implemented
  wxUnusedVar(palette);
}

void
wxPdfDCImpl::SetTextForeground(const wxColour& colour)
{
  if (colour.IsOk())
  {
    m_textForegroundColour = colour;
  }
}

void
wxPdfDCImpl::DestroyClippingRegion()
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  if (m_clipping)
  {
    m_pdfDocument->UnsetClipping();
    {
      wxPen x(GetPen()); SetPen(x);
    }
    {
      wxBrush x(GetBrush()); SetBrush(x);
    }
    {
      wxFont x(GetFont()); m_pdfDocument->SetFont(x);
    }
  }
  ResetClipping();
}

wxCoord
wxPdfDCImpl::GetCharHeight() const
{
  // default for 12 point font
  int height = 18;
  int width;
  if (m_font.Ok())
  {
    DoGetTextExtent("x", &width, &height);
  }
  return height;
}

wxCoord
wxPdfDCImpl::GetCharWidth() const
{
  int height;
  int width = 8;
  if (m_font.Ok())
  {
    DoGetTextExtent("x", &width, &height);
  }
  return width;
}

bool
wxPdfDCImpl::CanDrawBitmap() const
{
  return true;
}

bool
wxPdfDCImpl::CanGetTextExtent() const
{
  return true;
}

int
wxPdfDCImpl::GetDepth() const
{
  // TODO: check value
  return 32;
}

wxSize
wxPdfDCImpl::GetPPI() const
{
  int ppi = (int) m_ppi;
  return wxSize(ppi,ppi);
}

void
wxPdfDCImpl::ComputeScaleAndOrigin()
{
  m_scaleX = m_logicalScaleX * m_userScaleX;
  m_scaleY = m_logicalScaleY * m_userScaleY;
}

void
wxPdfDCImpl::SetMapMode(wxMappingMode mode)
{
  m_mappingMode = mode;
  switch (mode)
  {
    case wxMM_TWIPS:
      SetLogicalScale(m_ppi / 1440.0, m_ppi / 1440.0);
      break;
    case wxMM_POINTS:
      SetLogicalScale(m_ppi / 72.0, m_ppi / 72.0);
      break;
    case wxMM_METRIC:
      SetLogicalScale(m_ppi / 25.4, m_ppi / 25.4);
      break;
    case wxMM_LOMETRIC:
      SetLogicalScale(m_ppi / 254.0, m_ppi / 254.0);
      break;
    default:
    case wxMM_TEXT:
      SetLogicalScale(1.0, 1.0);
      break;
  }
}

void
wxPdfDCImpl::SetUserScale(double x, double y)
{
  m_userScaleX = x;
  m_userScaleY = y;
  ComputeScaleAndOrigin();
}

void
wxPdfDCImpl::SetLogicalScale(double x, double y)
{
  m_logicalScaleX = x;
  m_logicalScaleY = y;
  ComputeScaleAndOrigin();
}

void
wxPdfDCImpl::SetLogicalOrigin(wxCoord x, wxCoord y)
{
  m_logicalOriginX = x * m_signX;
  m_logicalOriginY = y * m_signY;
  ComputeScaleAndOrigin();
}

void
wxPdfDCImpl::SetDeviceOrigin(wxCoord x, wxCoord y)
{
  m_deviceOriginX = x;
  m_deviceOriginY = y;
  ComputeScaleAndOrigin();
}

void
wxPdfDCImpl::SetAxisOrientation(bool xLeftRight, bool yBottomUp)
{
  m_signX = (xLeftRight ?  1 : -1);
  m_signY = (yBottomUp  ? -1 :  1);
  ComputeScaleAndOrigin();
}

void
wxPdfDCImpl::SetLogicalFunction(wxRasterOperationMode function)
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  // TODO: check implementation
  m_logicalFunction = function;
  switch(function)
  {
    case wxAND:
      m_pdfDocument->SetAlpha(0.5, 0.5);
      break;
    case wxCOPY:
    default:
      m_pdfDocument->SetAlpha(1.0, 1.0);
      break;
  }
}

// the true implementations

bool
wxPdfDCImpl::DoFloodFill(wxCoord x, wxCoord y, const wxColour& col, wxFloodFillStyle style)
{
  wxUnusedVar(x);
  wxUnusedVar(y);
  wxUnusedVar(col);
  wxUnusedVar(style);
  wxFAIL_MSG(wxString(wxT("wxPdfDCImpl::FloodFill: "))+_("Not implemented."));
  return false;
}

void
wxPdfDCImpl::DoGradientFillLinear(const wxRect& rect,
                              const wxColour& initialColour,
                              const wxColour& destColour,
                              wxDirection nDirection)
{
  // TODO: native implementation
  wxDCImpl::DoGradientFillLinear(rect, initialColour, destColour, nDirection);
}

void
wxPdfDCImpl::DoGradientFillConcentric(const wxRect& rect,
                                  const wxColour& initialColour,
                                  const wxColour& destColour,
                                  const wxPoint& circleCenter)
{
  // TODO: native implementation
  wxDCImpl::DoGradientFillConcentric(rect, initialColour, destColour, circleCenter);
}

bool
wxPdfDCImpl::DoGetPixel(wxCoord x, wxCoord y, wxColour* col) const
{
  wxUnusedVar(x);
  wxUnusedVar(y);
  wxUnusedVar(col);
  wxFAIL_MSG(wxString(wxT("wxPdfDCImpl::DoGetPixel: "))+_("Not implemented."));
  return false;
}

void
wxPdfDCImpl::DoDrawPoint(wxCoord x, wxCoord y)
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  SetupPen();
  double xx = ScaleLogicalToPdfX(x);
  double yy = ScaleLogicalToPdfY(y);
  m_pdfDocument->SetFillColour(m_pdfDocument->GetDrawColour());
  m_pdfDocument->Rect(xx-0.5, yy-0.5, 1.0, 1.0);
  CalcBoundingBox(x, y);
}

#if wxUSE_SPLINES
void
wxPdfDCImpl::DoDrawSpline(const wxPointList* points)
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  SetPen( m_pen );
  wxASSERT_MSG( points, wxT("NULL pointer to spline points?") );
  const size_t n_points = points->GetCount();
  wxASSERT_MSG( n_points > 2 , wxT("incomplete list of spline points?") );
#if 0
  wxPoint* p;
  wxPdfArrayDouble xp, yp;
  wxList::compatibility_iterator node = points->GetFirst();
  while (node)
  {
    p = (wxPoint *)node->GetData();
    xp.Add(ScaleLogicalToPdfX(p->x));
    yp.Add(ScaleLogicalToPdfY(p->y));
    node = node->GetNext();
  }
  m_pdfDocument->BezierSpline(xp, yp, wxPDF_STYLE_DRAW);
#endif

  // Code taken from wxDC MSW implementation
  // quadratic b-spline to cubic bezier spline conversion
  //
  // quadratic spline with control points P0,P1,P2
  // P(s) = P0*(1-s)^2 + P1*2*(1-s)*s + P2*s^2
  //
  // bezier spline with control points B0,B1,B2,B3
  // B(s) = B0*(1-s)^3 + B1*3*(1-s)^2*s + B2*3*(1-s)*s^2 + B3*s^3
  //
  // control points of bezier spline calculated from b-spline
  // B0 = P0
  // B1 = (2*P1 + P0)/3
  // B2 = (2*P1 + P2)/3
  // B3 = P2

  double x1, y1, x2, y2, cx1, cy1, cx4, cy4;
  double bx1, by1, bx2, by2, bx3, by3;

  wxPointList::compatibility_iterator node = points->GetFirst();
  wxPoint* p = (wxPoint*) node->GetData();

  x1 = ScaleLogicalToPdfX(p->x);
  y1 = ScaleLogicalToPdfY(p->y);
  m_pdfDocument->MoveTo(x1, y1);

  node = node->GetNext();
  p = (wxPoint*) node->GetData();

  bx1 = x2 = ScaleLogicalToPdfX(p->x);
  by1 = y2 = ScaleLogicalToPdfY(p->y);
  cx1 = ( x1 + x2 ) / 2;
  cy1 = ( y1 + y2 ) / 2;
  bx3 = bx2 = cx1;
  by3 = by2 = cy1;
  m_pdfDocument->CurveTo(bx1, by1, bx2, by2, bx3, by3);

#if !wxUSE_STL
  while ((node = node->GetNext()) != NULL)
#else
  while ((node = node->GetNext()))
#endif // !wxUSE_STL
  {
    p = (wxPoint*) node->GetData();
    x1 = x2;
    y1 = y2;
    x2 = ScaleLogicalToPdfX(p->x);
    y2 = ScaleLogicalToPdfY(p->y);
    cx4 = (x1 + x2) / 2;
    cy4 = (y1 + y2) / 2;
    // B0 is B3 of previous segment
    // B1:
    bx1 = (x1*2+cx1)/3;
    by1 = (y1*2+cy1)/3;
    // B2:
    bx2 = (x1*2+cx4)/3;
    by2 = (y1*2+cy4)/3;
    // B3:
    bx3 = cx4;
    by3 = cy4;
    cx1 = cx4;
    cy1 = cy4;
    m_pdfDocument->CurveTo(bx1, by1, bx2, by2, bx3, by3);
  }

  bx1 = bx3;
  by1 = by3;
  bx3 = bx2 = x2;
  by3 = by2 = y2;
  m_pdfDocument->CurveTo(bx1, by1, bx2, by2, bx3, by3);
  m_pdfDocument->EndPath(wxPDF_STYLE_DRAW);
}
#endif

void
wxPdfDCImpl::DoDrawLine(wxCoord x1, wxCoord y1, wxCoord x2, wxCoord y2)
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  if  (m_pen.GetStyle() != wxPENSTYLE_TRANSPARENT)
  {
    SetupBrush();
    SetupPen();
    m_pdfDocument->Line(ScaleLogicalToPdfX(x1), ScaleLogicalToPdfY(y1),
                        ScaleLogicalToPdfX(x2), ScaleLogicalToPdfY(y2));
    CalcBoundingBox(x1, y1);
    CalcBoundingBox(x2, y2);
  }
}

void
wxPdfDCImpl::DoDrawArc(wxCoord x1, wxCoord y1,
                       wxCoord x2, wxCoord y2,
                       wxCoord xc, wxCoord yc)
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  SetupBrush();
  SetupPen();
  bool doFill = GetBrush().IsNonTransparent();
  bool doDraw = GetPen().IsNonTransparent();
  if (doDraw || doFill)
  {
    const double start = angleByCoords(x1, y1, xc, yc);
    const double end   = angleByCoords(x2, y2, xc, yc);
    const double xx1 = ScaleLogicalToPdfX(x1);
    const double yy1 = ScaleLogicalToPdfY(y1);
    const double xx2 = ScaleLogicalToPdfX(x2);
    const double yy2 = ScaleLogicalToPdfY(y2);
    const double xxc = ScaleLogicalToPdfX(xc);
    const double yyc = ScaleLogicalToPdfY(yc);
    const double rx = xx1 - xxc;
    const double ry = yy1 - yyc;
    const double r = sqrt(rx * rx + ry * ry);
    int style = wxPDF_STYLE_FILLDRAW;
    if (!(doDraw && doFill))
    {
      style = (doFill) ? wxPDF_STYLE_FILL : wxPDF_STYLE_DRAW;
    }
    m_pdfDocument->Ellipse(xxc, yyc, r, 0, 0, start, end, style, 8, doFill);
    wxCoord radius = (wxCoord) sqrt( (double)((x1-xc)*(x1-xc)+(y1-yc)*(y1-yc)) );
    CalcBoundingBox(xc-radius, yc-radius);
    CalcBoundingBox(xc+radius, yc+radius);
  }
}

void
wxPdfDCImpl::DoDrawCheckMark(wxCoord x, wxCoord y,
                             wxCoord width, wxCoord height)
{
  // TODO: native implementation?
  wxDCImpl::DoDrawCheckMark(x, y, width, height);
}

void
wxPdfDCImpl::DoDrawEllipticArc(wxCoord x, wxCoord y, wxCoord width, wxCoord height,
                               double sa, double ea)
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  SetupBrush();
  SetupPen();
  bool doFill = GetBrush().IsNonTransparent();
  bool doDraw = GetPen().IsNonTransparent();
  if (doDraw || doFill)
  {
    m_pdfDocument->SetLineWidth(ScaleLogicalToPdfXRel(1)); // pen width != 1 sometimes fools readers when closing paths
    if (doFill)
    {
      m_pdfDocument->Ellipse(ScaleLogicalToPdfX(x + (width + 1) / 2),
                             ScaleLogicalToPdfY(y + (height + 1) / 2),
                             ScaleLogicalToPdfXRel((width + 1) / 2),
                             ScaleLogicalToPdfYRel((height + 1) / 2),
                             0, sa, ea, wxPDF_STYLE_FILL, 8, true);
    }
    if (doDraw)
    {
      m_pdfDocument->Ellipse(ScaleLogicalToPdfX(x + (width + 1) / 2),
                             ScaleLogicalToPdfY(y + (height + 1) / 2),
                             ScaleLogicalToPdfXRel((width + 1) / 2),
                             ScaleLogicalToPdfYRel((height + 1) / 2),
                             0, sa, ea, wxPDF_STYLE_DRAW, 8, false);
    }
    CalcBoundingBox(x, y);
    CalcBoundingBox(x+width, y+height);
  }
}

void
wxPdfDCImpl::DoDrawRectangle(wxCoord x, wxCoord y, wxCoord width, wxCoord height)
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  SetupBrush();
  SetupPen();
  m_pdfDocument->Rect(ScaleLogicalToPdfX(x), ScaleLogicalToPdfY(y),
                      ScaleLogicalToPdfXRel(width), ScaleLogicalToPdfYRel(height),
                      GetDrawingStyle());
  CalcBoundingBox(x, y);
  CalcBoundingBox(x+width, y+height);
}

void
wxPdfDCImpl::DoDrawRoundedRectangle(wxCoord x, wxCoord y,
                                    wxCoord width, wxCoord height,
                                    double radius)
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  if (radius < 0.0)
  {
    // Now, a negative radius is interpreted to mean
    // 'the proportion of the smallest X or Y dimension'
    double smallest = width < height ? width : height;
    radius =  (-radius * smallest);
  }
  SetupBrush();
  SetupPen();
  m_pdfDocument->RoundedRect(ScaleLogicalToPdfX(x), ScaleLogicalToPdfY(y),
                             ScaleLogicalToPdfXRel(width), ScaleLogicalToPdfYRel(height),
                             ScaleLogicalToPdfXRel(wxRound(radius)), wxPDF_CORNER_ALL, GetDrawingStyle());
  CalcBoundingBox(x, y);
  CalcBoundingBox(x+width, y+height);
}

void
wxPdfDCImpl::DoDrawEllipse(wxCoord x, wxCoord y, wxCoord width, wxCoord height)
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  SetupBrush();
  SetupPen();
  m_pdfDocument->Ellipse(ScaleLogicalToPdfX(x + (width + 1) / 2),
                         ScaleLogicalToPdfY(y + (height + 1)/ 2),
                         ScaleLogicalToPdfXRel((width + 1) / 2),
                         ScaleLogicalToPdfYRel((height + 1) / 2),
                         0, 0, 360, GetDrawingStyle());
  CalcBoundingBox(x-width, y-height);
  CalcBoundingBox(x+width, y+height);
}

void
wxPdfDCImpl::DoCrossHair(wxCoord x, wxCoord y)
{
  wxUnusedVar(x);
  wxUnusedVar(y);
  wxFAIL_MSG(wxString(wxT("wxPdfDCImpl::DoCrossHair: "))+_("Not implemented."));
}

void
wxPdfDCImpl::DoDrawIcon(const wxIcon& icon, wxCoord x, wxCoord y)
{
  DoDrawBitmap(icon, x, y, true);
}

void
wxPdfDCImpl::DoDrawBitmap(const wxBitmap& bitmap, wxCoord x, wxCoord y, bool useMask)
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  wxCHECK_RET( IsOk(), wxT("wxPdfDCImpl::DoDrawBitmap - invalid DC") );
  wxCHECK_RET( bitmap.Ok(), wxT("wxPdfDCImpl::DoDrawBitmap - invalid bitmap") );

  if (!bitmap.Ok()) return;

  int idMask = 0;
  wxImage image = bitmap.ConvertToImage();
  if (!image.Ok()) return;

  if (!useMask)
  {
    image.SetMask(false);
  }
  wxCoord w = image.GetWidth();
  wxCoord h = image.GetHeight();

  const double ww = ScaleLogicalToPdfXRel(w);
  const double hh = ScaleLogicalToPdfYRel(h);

  const double xx = ScaleLogicalToPdfX(x);
  const double yy = ScaleLogicalToPdfY(y);

  wxString imgName = wxString::Format(wxT("pdfdcimg%d"), ++m_imageCount);

  if (bitmap.GetDepth() == 1)
  {
    wxPen savePen = m_pen;
    wxBrush saveBrush = m_brush;
    SetPen(*wxTRANSPARENT_PEN);
    SetBrush(m_textBackgroundColour);
    DoDrawRectangle(x, y, w, h);
    SetBrush(m_textForegroundColour);
    m_pdfDocument->Image(imgName, image, xx, yy, ww, hh, wxPdfLink(-1), idMask, m_jpegFormat, m_jpegQuality);
    SetBrush(saveBrush);
    SetPen(savePen);
  }
  else
  {
    m_pdfDocument->Image(imgName, image, xx, yy, ww, hh, wxPdfLink(-1), idMask, m_jpegFormat, m_jpegQuality);
  }
}

void
wxPdfDCImpl::DoDrawText(const wxString& text, wxCoord x, wxCoord y)
{
  if (text.Find(wxT('\n')) == wxNOT_FOUND)
  {
    // text contains a single line: just draw it
    DoDrawRotatedText(text, x, y, 0.0);
  }
  else
  {
    // this is a multiline text: split it and print the lines one by one
    wxCoord charH = GetCharHeight();
    wxCoord curY = y;
    wxStringTokenizer tok( text, "\n" );
    while( tok.HasMoreTokens() ) {
      wxString s = tok.GetNextToken();
      DoDrawRotatedText(s, x, curY, 0.0);
      curY += charH;
    }
  }
}

void
wxPdfDCImpl::DoDrawRotatedText(const wxString& text, wxCoord x, wxCoord y, double angle)
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));

  wxFont* fontToUse = &m_font;
  if (!fontToUse->IsOk())
  {
    return;
  }
  wxFont old = m_font;
  wxCoord originalY = y;

  wxPdfFontDescription desc = m_pdfDocument->GetFontDescription();
  int height, descent;
  CalculateFontMetrics(&desc, fontToUse->GetPointSize(), &height, NULL, &descent, NULL);
  if (m_mappingModeStyle != wxPDF_MAPMODESTYLE_PDF)
  {
    y += (height - abs(descent));
  }

  m_pdfDocument->SetTextColour(m_textForegroundColour.Red(), m_textForegroundColour.Green(), m_textForegroundColour.Blue());
  m_pdfDocument->SetFontSize(ScaleFontSizeToPdf(fontToUse->GetPointSize()));
  m_pdfDocument->RotatedText(ScaleLogicalToPdfX(x), ScaleLogicalToPdfY(y), ScaleLogicalToPdfX(x), ScaleLogicalToPdfY(originalY), text, angle);
  SetFont(old);
}

bool
wxPdfDCImpl::DoBlit(wxCoord xdest, wxCoord ydest, wxCoord width, wxCoord height,
                    wxDC* source, wxCoord xsrc, wxCoord ysrc,
                    wxRasterOperationMode rop /*= wxCOPY*/, bool useMask /*= false*/,
                    wxCoord xsrcMask /*= -1*/, wxCoord ysrcMask /*= -1*/)
{
//  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  wxCHECK_MSG( IsOk(), false, wxT("wxPdfDCImpl::DoBlit - invalid DC") );
  wxCHECK_MSG( source->IsOk(), false, wxT("wxPdfDCImpl::DoBlit - invalid source DC") );

  wxUnusedVar(useMask);
  wxUnusedVar(xsrcMask);
  wxUnusedVar(ysrcMask);

  // blit into a bitmap
  wxBitmap bitmap((int)width, (int)height);
  wxMemoryDC memDC;
  memDC.SelectObject(bitmap);
  memDC.Blit(0, 0, width, height, source, xsrc, ysrc, rop);
  memDC.SelectObject(wxNullBitmap);

  // draw bitmap. scaling and positioning is done there
  DoDrawBitmap( bitmap, xdest, ydest );

  return true;
}

void
wxPdfDCImpl::DoGetSize(int* width, int* height) const
{
  int w;
  int h;
  if (m_templateMode)
  {
    w = wxRound(m_templateWidth * m_pdfDocument->GetScaleFactor());
    h = wxRound(m_templateHeight * m_pdfDocument->GetScaleFactor());
  }
  else
  {
    wxPaperSize id = m_printData.GetPaperId();
    wxPrintPaperType *paper = wxThePrintPaperDatabase->FindPaperType(id);
    if (!paper) paper = wxThePrintPaperDatabase->FindPaperType(wxPAPER_A4);

    w = 595;
    h = 842;
    if (paper)
    {
      w = paper->GetSizeDeviceUnits().x;
      h = paper->GetSizeDeviceUnits().y;
    }

    if (m_printData.GetOrientation() == wxLANDSCAPE)
    {
      int tmp = w;
      w = h;
      h = tmp;
    }
  }

  if (width)
  {
    *width = wxRound( w * m_ppi / 72.0 );
  }

  if (height)
  {
    *height = wxRound( h * m_ppi / 72.0 );
  }
}

void
wxPdfDCImpl::DoGetSizeMM(int* width, int* height) const
{
  int w;
  int h;
  if (m_templateMode)
  {
    w = wxRound(m_templateWidth * m_pdfDocument->GetScaleFactor() * 25.4/72.0);
    h = wxRound(m_templateHeight * m_pdfDocument->GetScaleFactor() * 25.4/72.0);
  }
  else
  {
    wxPaperSize id = m_printData.GetPaperId();
    wxPrintPaperType *paper = wxThePrintPaperDatabase->FindPaperType(id);
    if (!paper) paper = wxThePrintPaperDatabase->FindPaperType(wxPAPER_A4);

    w = 210;
    h = 297;
    if (paper)
    {
      w = paper->GetWidth() / 10;
      h = paper->GetHeight() / 10;
    }

    if (m_printData.GetOrientation() == wxLANDSCAPE)
    {
      int tmp = w;
      w = h;
      h = tmp;
    }
  }
  if (width)
  {
    *width = w;
  }
  if (height)
  {
    *height = h;
  }
}

#if wxCHECK_VERSION(2,9,5)
void
wxPdfDCImpl::DoDrawLines(int n, const wxPoint points[], wxCoord xoffset, wxCoord yoffset)
#else
void
wxPdfDCImpl::DoDrawLines(int n, wxPoint points[], wxCoord xoffset, wxCoord yoffset)
#endif // wxCHECK_VERSION
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  SetupPen();
  int i;
  for (i = 0; i < n; ++i)
  {
    const wxPoint& point = points[i];
    double xx = ScaleLogicalToPdfX(xoffset + point.x);
    double yy = ScaleLogicalToPdfY(yoffset + point.y);
    CalcBoundingBox(point.x+xoffset, point.y+yoffset);
    if (i == 0)
    {
      m_pdfDocument->MoveTo(xx, yy);
    }
    else
    {
      m_pdfDocument->LineTo(xx, yy);
    }
  }
  m_pdfDocument->EndPath(wxPDF_STYLE_DRAW);
}

#if wxCHECK_VERSION(2,9,5)
void
wxPdfDCImpl::DoDrawPolygon(int n, const wxPoint points[],
                           wxCoord xoffset, wxCoord yoffset,
                           wxPolygonFillMode fillStyle /* = wxODDEVEN_RULE*/)
#else
void
wxPdfDCImpl::DoDrawPolygon(int n, wxPoint points[],
                           wxCoord xoffset, wxCoord yoffset,
                           wxPolygonFillMode fillStyle /* = wxODDEVEN_RULE*/)
#endif // wxCHECK_VERSION
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  SetupBrush();
  SetupPen();
  wxPdfArrayDouble xp;
  wxPdfArrayDouble yp;
  int i;
  for (i = 0; i < n; ++i)
  {
    const wxPoint& point = points[i];
    xp.Add(ScaleLogicalToPdfX(xoffset + point.x));
    yp.Add(ScaleLogicalToPdfY(yoffset + point.y));
    CalcBoundingBox(point.x + xoffset, point.y + yoffset);
  }
  int saveFillingRule = m_pdfDocument->GetFillingRule();
  m_pdfDocument->SetFillingRule(fillStyle);
  int style = GetDrawingStyle();
  m_pdfDocument->Polygon(xp, yp, style);
  m_pdfDocument->SetFillingRule(saveFillingRule);
}

#if wxCHECK_VERSION(2,9,5)
void
wxPdfDCImpl::DoDrawPolyPolygon(int n, const int count[], const wxPoint points[],
                               wxCoord xoffset, wxCoord yoffset,
                               wxPolygonFillMode fillStyle)
#else // wx < 2.9.5
void
wxPdfDCImpl::DoDrawPolyPolygon(int n, int count[], wxPoint points[],
                               wxCoord xoffset, wxCoord yoffset,
                               int fillStyle)
#endif // wxCHECK_VERSION
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  if (n > 0)
  {
    SetupBrush();
    SetupPen();
    int style = GetDrawingStyle();
    int saveFillingRule = m_pdfDocument->GetFillingRule();
    m_pdfDocument->SetFillingRule(fillStyle);

    int ofs = 0;
    int i, j;
    for (j = 0; j < n; ofs += count[j++])
    {
      wxPdfArrayDouble xp;
      wxPdfArrayDouble yp;
      for (i = 0; i < count[j]; ++i)
      {
        const wxPoint& point = points[ofs+i];
        xp.Add(ScaleLogicalToPdfX(xoffset + point.x));
        yp.Add(ScaleLogicalToPdfY(yoffset + point.y));
        CalcBoundingBox(point.x + xoffset, point.y + yoffset);
      }
      m_pdfDocument->Polygon(xp, yp, style);
    }
    m_pdfDocument->SetFillingRule(saveFillingRule);
  }
}

void
wxPdfDCImpl::DoSetClippingRegionAsRegion(const wxRegion& region)
{
  wxCoord x, y, w, h;
  region.GetBox(x, y, w, h);
  DoSetClippingRegion(x, y, w, h);
}

void
wxPdfDCImpl::DoSetClippingRegion(wxCoord x, wxCoord y, wxCoord width, wxCoord height)
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  if (m_clipping)
  {
    DestroyClippingRegion();
  }

  m_clipX1 = (int) x;
  m_clipY1 = (int) y;
  m_clipX2 = (int) (x + width);
  m_clipY2 = (int) (y + height);

  // Use the current path as a clipping region
  m_pdfDocument->ClippingRect(ScaleLogicalToPdfX(x),
                              ScaleLogicalToPdfY(y),
                              ScaleLogicalToPdfXRel(width),
                              ScaleLogicalToPdfYRel(height));
  m_clipping = true;
}

void
wxPdfDCImpl::DoSetDeviceClippingRegion(const wxRegion& region)
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  wxCoord x, y, w, h;
  region.GetBox(x, y, w, h);
  DoSetClippingRegion(DeviceToLogicalX(x), DeviceToLogicalY(y), DeviceToLogicalXRel(w), DeviceToLogicalYRel(h));
}

void
wxPdfDCImpl::DoGetTextExtent(const wxString& text,
                         wxCoord* x, wxCoord* y,
                         wxCoord* descent,
                         wxCoord* externalLeading,
                         const wxFont* theFont) const
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));

  const wxFont* fontToUse = theFont;
  if (!fontToUse)
  {
    fontToUse = const_cast<wxFont*>(&m_font);
  }

  if (fontToUse)
  {
    wxFont old = m_font;

    const_cast<wxPdfDCImpl*>(this)->SetFont(*fontToUse);
    wxPdfFontDescription desc = const_cast<wxPdfDCImpl*>(this)->m_pdfDocument->GetFontDescription();
    int myAscent, myDescent, myHeight, myExtLeading;
    CalculateFontMetrics(&desc, fontToUse->GetPointSize(), &myHeight, &myAscent, &myDescent, &myExtLeading);

    if (descent)
    {
      *descent = abs(myDescent);
    }
    if( externalLeading )
    {
      *externalLeading = myExtLeading;
    }
    if (x)
    {
      *x = ScalePdfToFontMetric((double)const_cast<wxPdfDCImpl*>(this)->m_pdfDocument->GetStringWidth(text));
    }
    if (y)
    {
      *y = myHeight;
    }
    const_cast<wxPdfDCImpl*>(this)->SetFont(old);
  }
  else
  {
    if (x)
    {
      *x = 0;
    }
    if (y)
    {
      *y = 0;
    }
    if (descent)
    {
      *descent = 0;
    }
    if (externalLeading)
    {
      *externalLeading = 0;
    }
  }
}

bool
wxPdfDCImpl::DoGetPartialTextExtents(const wxString& text, wxArrayInt& widths) const
{
  wxCHECK_MSG( m_pdfDocument, false, wxT("wxPdfDCImpl::DoGetPartialTextExtents - invalid DC") );

  /// very slow - but correct ??

  const size_t len = text.length();
  if (len == 0)
  {
    return true;
  }

  widths.Empty();
  widths.Add(0, len);
  int w, h;

  wxString buffer;
  buffer.Alloc(len);

  for ( size_t i = 0; i < len; ++i )
  {
    buffer.Append(text.Mid( i, 1));
    DoGetTextExtent(buffer, &w, &h);
    widths[i] = w;
  }

  buffer.Clear();
  return true;
}

void
wxPdfDCImpl::SetupPen()
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  // pen
  const wxPen& curPen = GetPen();
  if (curPen != wxNullPen)
  {
    wxPdfLineStyle style = m_pdfDocument->GetLineStyle();
    wxPdfArrayDouble dash;
    style.SetColour(wxColour(curPen.GetColour().Red(),
                             curPen.GetColour().Green(),
                             curPen.GetColour().Blue()));
    if (curPen.GetWidth())
    {
      style.SetWidth(ScaleLogicalToPdfXRel(curPen.GetWidth()));
    }
    switch (curPen.GetJoin())
    {
      case wxJOIN_BEVEL:
        style.SetLineJoin(wxPDF_LINEJOIN_BEVEL);
        break;
      case wxJOIN_ROUND:
        style.SetLineJoin(wxPDF_LINEJOIN_ROUND);
        break;
      case wxJOIN_MITER:
      default:
        style.SetLineJoin(wxPDF_LINEJOIN_MITER);
        break;
    }
    switch (curPen.GetCap())
    {
      case wxCAP_BUTT:
        style.SetLineCap(wxPDF_LINECAP_BUTT);
        break;
      case wxCAP_PROJECTING:
        style.SetLineCap(wxPDF_LINECAP_SQUARE);
        break;
      case wxCAP_ROUND:
      default:
        style.SetLineCap(wxPDF_LINECAP_ROUND);
        break;
    }
    switch (curPen.GetStyle())
    {
      case wxDOT:
        dash.Add(1);
        dash.Add(1);
        style.SetDash(dash);
        break;
      case wxLONG_DASH:
        dash.Add(4);
        dash.Add(4);
        style.SetDash(dash);
        break;
      case wxSHORT_DASH:
        dash.Add(2);
        dash.Add(2);
        style.SetDash(dash);
        break;
      case wxDOT_DASH:
        {
          dash.Add(1);
          dash.Add(1);
          dash.Add(4);
          dash.Add(1);
          style.SetDash(dash);
        }
        break;
      case wxSOLID:
      default:
        style.SetDash(dash);
        break;
    }
    m_pdfDocument->SetLineStyle(style);
  }
  else
  {
    m_pdfDocument->SetDrawColour(0, 0, 0);
    m_pdfDocument->SetLineWidth(ScaleLogicalToPdfXRel(1));
  }
}

void
wxPdfDCImpl::SetupBrush()
{
  wxCHECK_RET(m_pdfDocument, wxT("Invalid PDF DC"));
  // brush
  const wxBrush& curBrush = GetBrush();
  if (curBrush != wxNullBrush)
  {
    m_pdfDocument->SetFillColour(curBrush.GetColour().Red(), curBrush.GetColour().Green(), curBrush.GetColour().Blue());
  }
  else
  {
    m_pdfDocument->SetFillColour(0, 0, 0);
  }
}

// Get the current drawing style based on the current brush and pen
int
wxPdfDCImpl::GetDrawingStyle()
{
  int style = wxPDF_STYLE_NOOP;
  bool doFill = GetBrush().IsNonTransparent();
  bool doDraw = GetPen().IsNonTransparent();
  if (doFill && doDraw)
  {
    style = wxPDF_STYLE_FILLDRAW;
  }
  else if (doDraw)
  {
    style = wxPDF_STYLE_DRAW;
  }
  else if (doFill)
  {
    style = wxPDF_STYLE_FILL;
  }
  return style;
}

double
wxPdfDCImpl::ScaleLogicalToPdfX(wxCoord x) const
{
  double docScale = 72.0 / (m_ppi * m_pdfDocument->GetScaleFactor());
  return docScale * (((double)((x - m_logicalOriginX) * m_signX) * m_scaleX) +
         m_deviceOriginX + m_deviceLocalOriginX);
}

double
wxPdfDCImpl::ScaleLogicalToPdfXRel(wxCoord x) const
{
  double docScale = 72.0 / (m_ppi * m_pdfDocument->GetScaleFactor());
  return (double)(x) * m_scaleX * docScale;
}

double
wxPdfDCImpl::ScaleLogicalToPdfY(wxCoord y) const
{
  double docScale = 72.0 / (m_ppi * m_pdfDocument->GetScaleFactor());
  return docScale * (((double)((y - m_logicalOriginY) * m_signY) * m_scaleY) +
         m_deviceOriginY + m_deviceLocalOriginY);
}

double
wxPdfDCImpl::ScaleLogicalToPdfYRel(wxCoord y) const
{
  double docScale = 72.0 / (m_ppi * m_pdfDocument->GetScaleFactor());
  return (double)(y) * m_scaleY * docScale;
}

double
wxPdfDCImpl::ScaleFontSizeToPdf(int pointSize) const
{
  double fontScale;
  double rval;
  switch (m_mappingModeStyle)
  {
    case wxPDF_MAPMODESTYLE_MSW:
      // as implemented in wxMSW
      fontScale = (m_ppiPdfFont / m_ppi);
      rval = (double) pointSize * fontScale * m_scaleY;
      break;
    case wxPDF_MAPMODESTYLE_GTK:
      // as implemented in wxGTK / wxMAC / wxOSX
      fontScale = (m_ppiPdfFont / m_ppi);
      rval = (double) pointSize * fontScale * m_userScaleY;
      break;
    case wxPDF_MAPMODESTYLE_MAC:
      // as implemented in wxGTK / wxMAC / wxOSX
      fontScale = (m_ppiPdfFont / m_ppi);
      rval = (double) pointSize * fontScale * m_userScaleY;
      break;
    case wxPDF_MAPMODESTYLE_PDF:
      // an implementation where a font size of 12 gets a 12 point font if the
      // mapping mode is wxMM_POINTS and suitable scaled for other modes
      fontScale = (m_mappingMode == wxMM_TEXT) ? (m_ppiPdfFont / m_ppi) : (72.0 / m_ppi);
      rval = (double) pointSize * fontScale * m_scaleY;
      break;
    default:
      // standard and fall through
#if defined( __WXMSW__)
      fontScale = (m_ppiPdfFont / m_ppi);
      rval = (double) pointSize * fontScale * m_scaleY;
#else
      fontScale = (m_ppiPdfFont / m_ppi);
      rval = (double) pointSize * fontScale * m_userScaleY;
#endif
      break;
  }
  return rval;
}

int
wxPdfDCImpl::ScalePdfToFontMetric( double metric ) const
{
  double fontScale = (72.0 / m_ppi) / (double)m_pdfDocument->GetScaleFactor();
  return wxRound(((metric * m_signY) / m_scaleY ) / fontScale);
}

void
wxPdfDCImpl::CalculateFontMetrics(wxPdfFontDescription* desc, int pointSize,
                                  int* height, int* ascent, int* descent, int* extLeading) const
{
  double em_height, em_ascent, em_descent, em_externalLeading;
  int hheaAscender, hheaDescender, hheaLineGap;

  double size;
  if ((m_mappingModeStyle == wxPDF_MAPMODESTYLE_PDF) && (m_mappingMode != wxMM_TEXT))
  {
    size = (double) pointSize;
  }
  else
  {
    size = (double) pointSize * (m_ppiPdfFont / 72.0);
  }

  int os2sTypoAscender, os2sTypoDescender, os2sTypoLineGap, os2usWinAscent, os2usWinDescent;

  desc->GetOpenTypeMetrics(&hheaAscender, &hheaDescender, &hheaLineGap,
                           &os2sTypoAscender, &os2sTypoDescender, &os2sTypoLineGap,
                           &os2usWinAscent, &os2usWinDescent);

  if (hheaAscender)
  {
    em_ascent  = os2usWinAscent;
    em_descent = os2usWinDescent;
    em_externalLeading = (hheaLineGap - ((os2usWinAscent + os2usWinDescent) - (hheaAscender - hheaDescender)));
    if (em_externalLeading < 0)
    {
      em_externalLeading = 0;
    }
    em_height = em_ascent + em_descent;
  }
  else
  {
    // Magic numbers below give reasonable defaults for core fonts
    // This may need adjustment for CJK fonts ??
    em_ascent  = 1325;
    em_descent = 1.085 * desc->GetDescent();
    em_height  = em_ascent + em_descent;
    em_externalLeading = 33;   
  }
    
  if (ascent)
  {
    *ascent = wxRound(em_ascent * size / 1000.0);
  }
  if (descent)
  {
    *descent = wxRound(em_descent * size / 1000.0);
  }
  if (height)
  {
    *height = wxRound(em_height * size / 1000.0);
  }
  if (extLeading)
  {
    *extLeading = wxRound(em_externalLeading * size / 1000.0);
  }
}

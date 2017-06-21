///////////////////////////////////////////////////////////////////////////////
// Name:        pdfgraphics.cpp
// Purpose:     Implementation of wxPdfDocument graphics primitives
// Author:      Ulrich Telle
// Modified by:
// Created:     2006-01-27
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfgraphics.cpp Implementation of the wxPdfDocument graphics primitives

// For compilers that support precompilation, includes <wx/wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

#include <wx/tokenzr.h>

#include "wex/pdf/pdfcoonspatchmesh.h"
#include "wex/pdf/pdfdocument.h"
#include "wex/pdf/pdfgradient.h"
#include "wex/pdf/pdfgraphics.h"
#include "wex/pdf/pdfshape.h"
#include "wex/pdf/pdfutility.h"

wxPdfExtGState::wxPdfExtGState(double lineAlpha, double fillAlpha, wxPdfBlendMode blendMode)
{
  m_lineAlpha = lineAlpha;
  m_fillAlpha = fillAlpha;
  m_blendMode = blendMode;
}

wxPdfExtGState::~wxPdfExtGState()
{
}

int
wxPdfDocument::SetAlpha(double lineAlpha, double fillAlpha, wxPdfBlendMode blendMode)
{
  int n = 0;

  // Force alpha into range 0 .. 1
  if (lineAlpha < 0) lineAlpha = 0;
  else if (lineAlpha > 1) lineAlpha = 1;
  if (fillAlpha < 0) fillAlpha = 0;
  else if (fillAlpha > 1) fillAlpha = 1;

  // Create state id for lookup map
  int id = ((int) blendMode) * 100000000 + (int) (lineAlpha * 1000) * 10000 + (int) (fillAlpha * 1000);

  // Lookup state
  wxPdfExtGSLookupMap::iterator extGState = (*m_extGSLookup).find(id);
  if (extGState == (*m_extGSLookup).end())
  {
    n = (int) (*m_extGStates).size() + 1;
    (*m_extGStates)[n] = new wxPdfExtGState(lineAlpha, fillAlpha, blendMode);
    (*m_extGSLookup)[id] = n;
  }
  else
  {
    n = extGState->second;
  }

  if (n != m_currentExtGState)
  {
    SetAlphaState(n);
  }

  return n;
}

void
wxPdfDocument::SetAlphaState(int alphaState)
{
  if (alphaState > 0 && (size_t) alphaState <= (*m_extGStates).size())
  {
    OutAscii(wxString::Format(wxT("/GS%d gs"), alphaState));
  }
}

// ----------------------------------------------------------------------------
// wxPdfLineStyle: class representing line style for drawing graphics
// ----------------------------------------------------------------------------

wxPdfLineStyle::wxPdfLineStyle(double width,
                               wxPdfLineCap cap, wxPdfLineJoin join,
                               const wxPdfArrayDouble& dash, double phase,
                               const wxPdfColour& colour)
{
  m_isSet = (width > 0) || (cap >= 0) || (join >= 0) || (dash.GetCount() > 0);
  m_width = width;
  m_cap   = cap;
  m_join  = join;
  m_dash  = dash;
  m_phase = phase;
  m_colour = colour;
}

wxPdfLineStyle::~wxPdfLineStyle()
{
}


wxPdfLineStyle::wxPdfLineStyle(const wxPdfLineStyle& lineStyle)
{
  m_isSet = lineStyle.m_isSet;
  m_width = lineStyle.m_width;
  m_cap   = lineStyle.m_cap;
  m_join  = lineStyle.m_join;
  m_dash  = lineStyle.m_dash;
  m_phase = lineStyle.m_phase;
  m_colour = lineStyle.m_colour;
}

wxPdfLineStyle&
wxPdfLineStyle::operator= (const wxPdfLineStyle& lineStyle)
{
  m_isSet = lineStyle.m_isSet;
  m_width = lineStyle.m_width;
  m_cap   = lineStyle.m_cap;
  m_join  = lineStyle.m_join;
  m_dash  = lineStyle.m_dash;
  m_phase = lineStyle.m_phase;
  m_colour = lineStyle.m_colour;
  return *this;
}

// ---

wxPdfShape::wxPdfShape()
{
  m_subpath = -1;
  m_index = 0;
}

wxPdfShape::~wxPdfShape()
{
}

void
wxPdfShape::MoveTo(double x, double y)
{
  m_subpath = (int) m_x.GetCount();
  m_types.Add(wxPDF_SEG_MOVETO);
  m_x.Add(x);
  m_y.Add(y);
}

void
wxPdfShape::LineTo(double x, double y)
{
  if (m_subpath >= 0)
  {
    m_types.Add(wxPDF_SEG_LINETO);
    m_x.Add(x);
    m_y.Add(y);
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfShape::LineTo: ")) +
               wxString(_("Invalid subpath.")));
  }
}

void
wxPdfShape::CurveTo(double x1, double y1, double x2, double y2, double x3, double y3)
{
  if (m_subpath >= 0)
  {
    m_types.Add(wxPDF_SEG_CURVETO);
    m_x.Add(x1);
    m_y.Add(y1);
    m_x.Add(x2);
    m_y.Add(y2);
    m_x.Add(x3);
    m_y.Add(y3);
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfShape::CurveTo: ")) +
               wxString(_("Invalid subpath.")));
  }
}

void
wxPdfShape::ClosePath()
{
  if (m_subpath >= 0 && m_types.GetCount() > 0 && m_types.Last() != wxPDF_SEG_CLOSE)
  {
    m_types.Add(wxPDF_SEG_CLOSE);
    m_x.Add(m_x[m_subpath]);
    m_y.Add(m_y[m_subpath]);
    m_subpath = -1;
  }
}

wxPdfSegmentType
wxPdfShape::GetSegment(int iterType, int iterPoints, double coords[]) const
{
  wxPdfSegmentType segType = wxPDF_SEG_UNDEFINED;
  if (iterType >= 0 && (size_t) iterType < m_types.GetCount())
  {
    int pointCount = (m_types[iterType] == wxPDF_SEG_CURVETO) ? 2 : 0;
    if (iterPoints >= 0 && (size_t) (iterPoints + pointCount) < m_x.GetCount())
    {
      segType = (wxPdfSegmentType) m_types[iterType];
      switch (segType)
      {
        case wxPDF_SEG_CLOSE:
          coords[0] = m_x[iterPoints];
          coords[1] = m_y[iterPoints];
          break;

        case wxPDF_SEG_MOVETO:
        case wxPDF_SEG_LINETO:
          coords[0] = m_x[iterPoints];
          coords[1] = m_y[iterPoints];
          break;

        case wxPDF_SEG_CURVETO:
          coords[0] = m_x[iterPoints];
          coords[1] = m_y[iterPoints];
          iterPoints++;
          coords[2] = m_x[iterPoints];
          coords[3] = m_y[iterPoints];
          iterPoints++;
          coords[4] = m_x[iterPoints];
          coords[5] = m_y[iterPoints];
          break;
        default:
          break;
      }
    }
  }
  return segType;
}

wxPdfFlatPath::wxPdfFlatPath(const wxPdfShape* shape, double flatness, int limit)
{
  m_shape = shape;
  m_iterType = 0;
  m_iterPoints = 0;
  m_done = false;
  m_flatnessSq = flatness * flatness;
  m_recursionLimit = limit;

  m_stackMaxSize = 6 * m_recursionLimit + /* 6 + 2 */ 8;
  m_stack = new double[m_stackMaxSize];
  m_recLevel = new int[m_recursionLimit + 1];

  FetchSegment();
}

wxPdfFlatPath::~wxPdfFlatPath()
{
  delete [] m_stack;
  delete [] m_recLevel;
}

void
wxPdfFlatPath::InitIter()
{
  m_done       = false;
  m_iterType   = 0;
  m_iterPoints = 0;
  m_stackSize  = 0;
  FetchSegment();
}

  /**
   * Fetches the next segment from the source iterator.
   */
void
wxPdfFlatPath::FetchSegment()
{
  int sp;

  if ((size_t) m_iterType >= m_shape->GetSegmentCount())
  {
    m_done = true;
    return;
  }

  m_srcSegType = m_shape->GetSegment(m_iterType, m_iterPoints, m_scratch);
    
  switch (m_srcSegType)
  {
    case wxPDF_SEG_CLOSE:
      return;

    case wxPDF_SEG_MOVETO:
    case wxPDF_SEG_LINETO:
      m_srcPosX = m_scratch[0];
      m_srcPosY = m_scratch[1];
      return;

    case wxPDF_SEG_CURVETO:
      if (m_recursionLimit == 0)
      {
        m_srcPosX = m_scratch[4];
        m_srcPosY = m_scratch[5];
        m_stackSize = 0;
        return;
      }
      sp = 6 * m_recursionLimit;
      m_stackSize = 1;
      m_recLevel[0] = 0;
      m_stack[sp] = m_srcPosX;                  // P1.x
      m_stack[sp + 1] = m_srcPosY;              // P1.y
      m_stack[sp + 2] = m_scratch[0];           // C1.x
      m_stack[sp + 3] = m_scratch[1];           // C1.y
      m_stack[sp + 4] = m_scratch[2];           // C2.x
      m_stack[sp + 5] = m_scratch[3];           // C2.y
      m_srcPosX = m_stack[sp + 6] = m_scratch[4]; // P2.x
      m_srcPosY = m_stack[sp + 7] = m_scratch[5]; // P2.y
      SubdivideCubic();
      return;
  }
}

void
wxPdfFlatPath::Next()
{
  if (m_stackSize > 0)
  {
    --m_stackSize;
    if (m_stackSize > 0)
    {
      switch (m_srcSegType)
      {
        case wxPDF_SEG_CURVETO:
          SubdivideCubic();
          return;

        default:
          break;
      }
    }
  }

  if ((size_t) m_iterType < m_shape->GetSegmentCount())
  {
    switch (m_srcSegType)
    {
      case wxPDF_SEG_CLOSE:
      case wxPDF_SEG_MOVETO:
      case wxPDF_SEG_LINETO:
        m_iterPoints++;
        break;

      case wxPDF_SEG_CURVETO:
        m_iterPoints += 3;
        break;
    }
    m_iterType++;
  }

  FetchSegment();
}

int
wxPdfFlatPath::CurrentSegment(double coords[])
{
  switch (m_srcSegType)
  {
    case wxPDF_SEG_CLOSE:
      return m_srcSegType;

    case wxPDF_SEG_MOVETO:
    case wxPDF_SEG_LINETO:
      coords[0] = m_srcPosX;
      coords[1] = m_srcPosY;
      return m_srcSegType;

    case wxPDF_SEG_CURVETO:
      if (m_stackSize == 0)
      {
        coords[0] = m_srcPosX;
        coords[1] = m_srcPosY;
      }
      else
      {
        int sp = m_stackMaxSize - 6 * m_stackSize;
        coords[0] = m_stack[sp + 4];
        coords[1] = m_stack[sp + 5];
      }
      return wxPDF_SEG_LINETO;
  }

  return wxPDF_SEG_UNDEFINED;
}

static void
SubdivideCubicCurve(double src[], int srcOff,
                    double left[], int leftOff,
                    double right[], int rightOff)
{
  // To understand this code, please have a look at the image
  // "CubicCurve2D-3.png" in the sub-directory "doc-files".
  double srcC1x;
  double srcC1y;
  double srcC2x;
  double srcC2y;
  double leftP1x;
  double leftP1y;
  double leftC1x;
  double leftC1y;
  double leftC2x;
  double leftC2y;
  double rightC1x;
  double rightC1y;
  double rightC2x;
  double rightC2y;
  double rightP2x;
  double rightP2y;
  double midx; // Mid = left.P2 = right.P1
  double midy; // Mid = left.P2 = right.P1

  leftP1x = src[srcOff];
  leftP1y = src[srcOff + 1];
  srcC1x = src[srcOff + 2];
  srcC1y = src[srcOff + 3];
  srcC2x = src[srcOff + 4];
  srcC2y = src[srcOff + 5];
  rightP2x = src[srcOff + 6];
  rightP2y = src[srcOff + 7];

  leftC1x = (leftP1x + srcC1x) / 2;
  leftC1y = (leftP1y + srcC1y) / 2;
  rightC2x = (rightP2x + srcC2x) / 2;
  rightC2y = (rightP2y + srcC2y) / 2;
  midx = (srcC1x + srcC2x) / 2;
  midy = (srcC1y + srcC2y) / 2;
  leftC2x = (leftC1x + midx) / 2;
  leftC2y = (leftC1y + midy) / 2;
  rightC1x = (midx + rightC2x) / 2;
  rightC1y = (midy + rightC2y) / 2;
  midx = (leftC2x + rightC1x) / 2;
  midy = (leftC2y + rightC1y) / 2;

  if (left != NULL)
  {
    left[leftOff] = leftP1x;
    left[leftOff + 1] = leftP1y;
    left[leftOff + 2] = leftC1x;
    left[leftOff + 3] = leftC1y;
    left[leftOff + 4] = leftC2x;
    left[leftOff + 5] = leftC2y;
    left[leftOff + 6] = midx;
    left[leftOff + 7] = midy;
  }

  if (right != NULL)
  {
    right[rightOff] = midx;
    right[rightOff + 1] = midy;
    right[rightOff + 2] = rightC1x;
    right[rightOff + 3] = rightC1y;
    right[rightOff + 4] = rightC2x;
    right[rightOff + 5] = rightC2y;
    right[rightOff + 6] = rightP2x;
    right[rightOff + 7] = rightP2y;
  }
}

static double
PointSegmentDistanceSq(double x1, double y1, double x2, double y2, double px, double py)
{
  double pd2 = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2);

  double x, y;
  if (pd2 == 0)
  {
    // Points are coincident.
    x = x1;
    y = y2;
  }
  else
  {
    double u = ((px - x1) * (x2 - x1) + (py - y1) * (y2 - y1)) / pd2;

    if (u < 0)
    {
      // "Off the end"
      x = x1;
      y = y1;
    }
    else if (u > 1.0)
    {
      x = x2;
      y = y2;
    }
    else
    {
      x = x1 + u * (x2 - x1);
      y = y1 + u * (y2 - y1);
    }
  }

  return (x - px) * (x - px) + (y - py) * (y - py);
}

static double
GetFlatnessSq(double x1, double y1, double cx1, double cy1,
              double cx2, double cy2, double x2, double y2)
{

  double d1 = PointSegmentDistanceSq(x1, y1, x2, y2, cx1, cy1);
  double d2 = PointSegmentDistanceSq(x1, y1, x2, y2, cx2, cy2);
  return (d1 > d2) ? d1 : d2;
//  return Math.max(Line2D.ptSegDistSq(x1, y1, x2, y2, cx1, cy1),
//                  Line2D.ptSegDistSq(x1, y1, x2, y2, cx2, cy2));
}

static double
GetFlatnessSq(double coords[], int offset)
{
  return GetFlatnessSq(coords[offset+0], coords[offset+1], coords[offset+2],
                       coords[offset+3], coords[offset+4], coords[offset+5],
                       coords[offset+6], coords[offset+7]);
}

  /**
   * Repeatedly subdivides the cubic curve segment that is on top
   * of the stack. The iteration terminates when the recursion limit
   * has been reached, or when the resulting segment is flat enough.
   */
void
wxPdfFlatPath::SubdivideCubic()
{
  int sp;
  int level;

  sp = m_stackMaxSize - 6 * m_stackSize - 2;
  level = m_recLevel[m_stackSize - 1];
  while ((level < m_recursionLimit)
         && (GetFlatnessSq(m_stack, sp) >= m_flatnessSq))
  {
    m_recLevel[m_stackSize] = m_recLevel[m_stackSize - 1] = ++level;
      
    SubdivideCubicCurve(m_stack, sp, m_stack, sp - 6, m_stack, sp);
    ++m_stackSize;
    sp -= 6;
  }
}

double
wxPdfFlatPath::MeasurePathLength()
{
  double points[6];
  double moveX = 0, moveY = 0;
  double lastX = 0, lastY = 0;
  double thisX = 0, thisY = 0;
  int type = 0;
  double total = 0;

  // Save iterator state
  bool saveDone      = m_done;
  int saveIterType   = m_iterType;
  int saveIterPoints = m_iterPoints;
  int saveStackSize  = m_stackSize;

  InitIter();
  while (!IsDone())
  {
    type = CurrentSegment(points);
    switch( type )
    {
      case wxPDF_SEG_MOVETO:
        moveX = lastX = points[0];
        moveY = lastY = points[1];
        break;

      case wxPDF_SEG_CLOSE:
        points[0] = moveX;
        points[1] = moveY;
        // Fall into....

      case wxPDF_SEG_LINETO:
        thisX = points[0];
        thisY = points[1];
        double dx = thisX-lastX;
        double dy = thisY-lastY;
        total += sqrt(dx*dx + dy*dy);
        lastX = thisX;
        lastY = thisY;
        break;
    }
    Next();
  }

  // Restore iterator state
  m_done       = saveDone;
  m_iterType   = saveIterType;
  m_iterPoints = saveIterPoints;
  m_stackSize  = saveStackSize;
  FetchSegment();

  return total;
}

void
wxPdfDocument::ShapedText(const wxPdfShape& shape, const wxString& text, wxPdfShapedTextMode mode)
{
  wxString voText = ApplyVisualOrdering(text);
  bool stretchToFit = (mode == wxPDF_SHAPEDTEXTMODE_STRETCHTOFIT);
  bool repeat = (mode == wxPDF_SHAPEDTEXTMODE_REPEAT);
  double flatness = 0.25 / GetScaleFactor();
  wxPdfFlatPath it(&shape, flatness);
  double points[6];
  double moveX = 0, moveY = 0;
  double lastX = 0, lastY = 0;
  double thisX = 0, thisY = 0;
  int type = 0;
  double next = 0;
  unsigned int currentChar = 0;
  unsigned int length = (unsigned int) voText.Length();
  double height = GetFontSize() / GetScaleFactor();

  if (length == 0)
  {
    return;
  }

  double factor = stretchToFit ? it.MeasurePathLength() / DoGetStringWidth(voText) : 1.0;
  double nextAdvance = 0;

  while (currentChar < length && !it.IsDone())
  {
    type = it.CurrentSegment(points);
    switch (type)
    {
      case wxPDF_SEG_MOVETO:
      {
        moveX = lastX = points[0];
        moveY = lastY = points[1];
        SetXY(moveX, moveY);
        nextAdvance = DoGetStringWidth(voText.Mid(currentChar,1)) * 0.5;
        next = nextAdvance;
        break;
      }

      case wxPDF_SEG_CLOSE:
      {
        points[0] = moveX;
        points[1] = moveY;
        // Fall into...
      }

      case wxPDF_SEG_LINETO:
      {
        thisX = points[0];
        thisY = points[1];
        double dx = thisX-lastX;
        double dy = thisY-lastY;
        double distance = sqrt(dx*dx + dy*dy);
        if (distance >= next)
        {
          double r = 1.0 / distance;
          double angle = atan2(-dy, dx) * 45. / atan(1.);
          while (currentChar < length && distance >= next)
          {
            wxString glyph = voText.Mid(currentChar, 1);
            double x = lastX + next*dx*r;
            double y = lastY + next*dy*r;
            double advance = nextAdvance;
            nextAdvance = currentChar < length-1 ? DoGetStringWidth(voText.Mid(currentChar+1,1)) * 0.5 : 
                                                   (repeat) ? DoGetStringWidth(voText.Mid(0,1)) * 0.5 : 0;
            SetXY(x, y);
            StartTransform();
            Rotate(angle);
            SetXY(x-advance,y-height);
            Write(height, glyph);
            StopTransform();
            next += (advance+nextAdvance) * factor;
            currentChar++;
            if ( repeat )
            {
              currentChar %= length;
            }
          }
        }
        next -= distance;
        lastX = thisX;
        lastY = thisY;
        break;
      }
    }
    it.Next();
  }
}

// ---

void
wxPdfDocument::SetFillingRule(int rule)
{
  if (rule == wxWINDING_RULE || rule == wxODDEVEN_RULE)
  {
    m_fillRule = rule;
  }
}

int
wxPdfDocument::GetFillingRule()
{
  return m_fillRule;
}

void
wxPdfDocument::Line(double x1, double y1, double x2, double y2)
{
  // Draw a line
  OutAscii(wxPdfUtility::Double2String(x1*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(y1*m_k,2) + wxString(wxT(" m ")) +
           wxPdfUtility::Double2String(x2*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(y2*m_k,2) + wxString(wxT(" l S")));
}

void
wxPdfDocument::Rect(double x, double y, double w, double h, int style)
{
  wxString op;
  // Draw a rectangle
  if ((style & wxPDF_STYLE_FILLDRAW) == wxPDF_STYLE_FILL)
  {
    op = wxT("f");
  }
  else if ((style & wxPDF_STYLE_FILLDRAW) == wxPDF_STYLE_FILLDRAW)
  {
    op = wxT("B");
  }
  else
  {
    op = wxT("S");
  }
  OutAscii(wxPdfUtility::Double2String(x*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(y*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(w*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(h*m_k,2) + wxString(wxT(" re ")) + op);
}

void
wxPdfDocument::RoundedRect(double x, double y, double w, double h,
                           double r, int roundCorner, int style)
{
  if ((roundCorner & wxPDF_CORNER_ALL) == wxPDF_CORNER_NONE)
  {
    // Not rounded
    Rect(x, y, w, h, style);
  }
  else
  { 
    // Rounded
    wxString op;
    // Draw a rectangle
    if ((style & wxPDF_STYLE_FILLDRAW) == wxPDF_STYLE_FILL)
    {
      op = wxT("f");
    }
    else
    {
      if ((style & wxPDF_STYLE_FILLDRAW) == wxPDF_STYLE_FILLDRAW)
      {
        op = wxT("B");
      }
      else
      {
        op = wxT("S");
      }
    }

    double myArc = 4. / 3. * (sqrt(2.) - 1.);

    OutPoint(x + r, y);
    double xc = x + w - r;
    double yc = y + r;
    OutLine(xc, y);

    if (roundCorner & wxPDF_CORNER_TOP_LEFT)
    {
      OutCurve(xc + (r * myArc), yc - r, xc + r, yc - (r * myArc), xc + r, yc);
    }
    else
    {
      OutLine(x + w, y);
    }

    xc = x + w - r ;
    yc = y + h - r;
    OutLine(x + w, yc);

    if (roundCorner & wxPDF_CORNER_TOP_RIGHT)
    {
      OutCurve(xc + r, yc + (r * myArc), xc + (r * myArc), yc + r, xc, yc + r);
    }
    else
    {
      OutLine(x + w, y + h);
    }

    xc = x + r;
    yc = y + h - r;
    OutLine(xc, y + h);

    if (roundCorner & wxPDF_CORNER_BOTTOM_LEFT)
    {
      OutCurve(xc - (r * myArc), yc + r, xc - r, yc + (r * myArc), xc - r, yc);
    }
    else
    {
      OutLine(x, y + h);
    }

    xc = x + r;
    yc = y + r;
    OutLine(x, yc);
    
    if (roundCorner & wxPDF_CORNER_BOTTOM_RIGHT)
    {
      OutCurve(xc - r, yc - (r * myArc), xc - (r * myArc), yc - r, xc, yc - r);
    }
    else
    {
      OutLine(x, y);
      OutLine(x + r, y);
    }
    OutAscii(op);
  }
}

void
wxPdfDocument::Curve(double x0, double y0, double x1, double y1, 
                     double x2, double y2, double x3, double y3,
                     int style)
{
  wxString op;
  // Draw a rectangle
  if ((style & wxPDF_STYLE_FILLDRAW) == wxPDF_STYLE_FILL)
  {
    op = (m_fillRule == wxODDEVEN_RULE) ? wxT("f*") : wxT("f");
  }
  else
  {
    if ((style & wxPDF_STYLE_FILLDRAW) == wxPDF_STYLE_FILLDRAW)
    {
      op = (m_fillRule == wxODDEVEN_RULE) ? wxT("B*") : wxT("B");
    }
    else
    {
      op = wxT("S");
    }
  }

  OutPoint(x0, y0);
  OutCurve(x1, y1, x2, y2, x3, y3);
  OutAscii(op);
}

void
wxPdfDocument::Ellipse(double x0, double y0, double rx, double ry, 
                       double angle, double astart, double afinish,
                       int style, int nSeg, bool doSector)
{
  if (rx <= 0) return;

  wxString op;
  // Draw an ellipse
  if ((style & wxPDF_STYLE_DRAWCLOSE) == wxPDF_STYLE_DRAWCLOSE)
  {
    // Close the path as well
    if ((style & wxPDF_STYLE_FILL) == wxPDF_STYLE_FILL)
    {
      op = wxT("b"); // small 'b' means closing the path as well
    }
    else
    {
      op = wxT("s"); // small 's' means closing the path as well
    }
  }
  else
  {
    if ((style & wxPDF_STYLE_MASK) == wxPDF_STYLE_FILL)
    {
      op = wxT("f");
    }
    else if ((style & wxPDF_STYLE_MASK) == wxPDF_STYLE_FILLDRAW)
    {
      op = (doSector) ? wxT("b") : wxT("B");
    }
    else
    {
      op = (doSector) ? wxT("s") : wxT("S");
    }
  }

  if (ry <= 0)
  {
    ry = rx;
  }
  rx *= m_k;
  ry *= m_k;
  if (nSeg < 2)
  {
    nSeg = 2;
  }

  static double pi = 4. * atan(1.0);
  astart = pi * astart / 180.;
  afinish = pi * afinish / 180.;
  if (m_yAxisOriginTop)
  {
    astart *= -1.0;
    afinish *= -1.0;
  }
  double totalAngle = afinish - astart;

  double dt = totalAngle / nSeg;
  double dtm = dt / 3;

  x0 *= m_k;
  y0 *= m_k;
  if (angle != 0)
  {
    double a = -(pi * angle / 180.);
    if (m_yAxisOriginTop)
    {
      a *= -1.0;
    }
    OutAscii(wxString(wxT("q ")) + 
             wxPdfUtility::Double2String(cos(a),2) + wxString(wxT(" ")) +
             wxPdfUtility::Double2String(-1 * sin(a),2) + wxString(wxT(" ")) +
             wxPdfUtility::Double2String(sin(a),2) + wxString(wxT(" ")) +
             wxPdfUtility::Double2String(cos(a),2) + wxString(wxT(" ")) +
             wxPdfUtility::Double2String(x0,2) + wxString(wxT(" ")) +
             wxPdfUtility::Double2String(y0,2) + wxString(wxT(" cm")));
    x0 = 0;
    y0 = 0;
  }

  double t1, a0, b0, c0, d0, a1, b1, c1, d1;
  t1 = astart;
  a0 = x0 + (rx * cos(t1));
  b0 = y0 + (ry * sin(t1));
  c0 = -rx * sin(t1);
  d0 = ry * cos(t1);
  OutPoint(a0 / m_k, b0 / m_k);
  int i;
  for (i = 1; i <= nSeg; i++)
  {
    // Draw this bit of the total curve
    t1 = (i * dt) + astart;
    a1 = x0 + (rx * cos(t1));
    b1 = y0 + (ry * sin(t1));
    c1 = -rx * sin(t1);
    d1 = ry * cos(t1);
    OutCurve((a0 + (c0 * dtm)) / m_k,
             (b0 + (d0 * dtm)) / m_k,
             (a1 - (c1 * dtm)) / m_k,
             (b1 - (d1 * dtm)) / m_k,
             a1 / m_k,
             b1 / m_k);
    a0 = a1;
    b0 = b1;
    c0 = c1;
    d0 = d1;
  }
  if (doSector)
  {
    OutLine(x0 / m_k, y0 / m_k);
//    a0 = x0 + (rx * cos(t1));
//    b0 = y0 + (ry * sin(t1));
//    OutLine(a0, b0);
  }
  OutAscii(op);
  if (angle !=0)
  {
    Out("Q");
  }
}

void
wxPdfDocument::Circle(double x0, double y0, double r, double astart, double afinish,
                      int style, int nSeg)
{
  Ellipse(x0, y0, r, 0, 0, astart, afinish, style, nSeg);
}

void
wxPdfDocument::Sector(double xc, double yc, double r, double astart, double afinish,
                      int style, bool clockwise, double origin)
{
  static double pi = 4. * atan(1.);
  static double pi2 = 0.5 * pi;
  double d;
  if (clockwise)
  {
    d = afinish;
    afinish = origin - astart;
    astart = origin - d;
  }
  else
  {
    afinish += origin;
    astart += origin;
  }
  astart = fmod(astart, 360.) + 360;
  afinish = fmod(afinish, 360.) + 360;
  if (astart > afinish)
  {
    afinish += 360;
  }
  afinish = afinish / 180. * pi;
  astart = astart / 180. * pi;
  d = afinish - astart;
  if (d == 0)
  {
    d = 2 * pi;
  }
  
  wxString op;
  if ((style & wxPDF_STYLE_FILLDRAW) == wxPDF_STYLE_FILL)
  {
    op = wxT("f");
  }
  else
  {
    if ((style & wxPDF_STYLE_FILLDRAW) == wxPDF_STYLE_FILLDRAW)
    {
      op = wxT("b");
    }
    else
    {
      op = wxT("s");
    }
  }

  double myArc;
  if (sin(d/2) != 0.0)
  {
    myArc = 4./3. * (1.-cos(d/2))/sin(d/2) * r;
  }
  else
  {
    myArc = 0.0;
  }
  // first put the center
  OutPoint(xc,yc);
  // put the first point
  OutLine(xc+r*cos(astart),yc-r*sin(astart));
  // draw the arc
  if (d < pi2)
  {
    OutCurve(xc+r*cos(astart)+myArc*cos(pi2+astart),
             yc-r*sin(astart)-myArc*sin(pi2+astart),
             xc+r*cos(afinish)+myArc*cos(afinish-pi2),
             yc-r*sin(afinish)-myArc*sin(afinish-pi2),
             xc+r*cos(afinish),
             yc-r*sin(afinish));
  }
  else
  {
    afinish = astart + d/4;
    myArc = 4./3. * (1.-cos(d/8))/sin(d/8) * r;
    OutCurve(xc+r*cos(astart)+myArc*cos(pi2+astart),
             yc-r*sin(astart)-myArc*sin(pi2+astart),
             xc+r*cos(afinish)+myArc*cos(afinish-pi2),
             yc-r*sin(afinish)-myArc*sin(afinish-pi2),
             xc+r*cos(afinish),
             yc-r*sin(afinish));
    astart = afinish;
    afinish = astart + d/4;
    OutCurve(xc+r*cos(astart)+myArc*cos(pi2+astart),
             yc-r*sin(astart)-myArc*sin(pi2+astart),
             xc+r*cos(afinish)+myArc*cos(afinish-pi2),
             yc-r*sin(afinish)-myArc*sin(afinish-pi2),
             xc+r*cos(afinish),
             yc-r*sin(afinish));
    astart = afinish;
    afinish = astart + d/4;
    OutCurve(xc+r*cos(astart)+myArc*cos(pi2+astart),
             yc-r*sin(astart)-myArc*sin(pi2+astart),
             xc+r*cos(afinish)+myArc*cos(afinish-pi2),
             yc-r*sin(afinish)-myArc*sin(afinish-pi2),
             xc+r*cos(afinish),
             yc-r*sin(afinish));
    astart = afinish;
    afinish = astart + d/4;
    OutCurve(xc+r*cos(astart)+myArc*cos(pi2+astart),
             yc-r*sin(astart)-myArc*sin(pi2+astart),
             xc+r*cos(afinish)+myArc*cos(afinish-pi2),
             yc-r*sin(afinish)-myArc*sin(afinish-pi2),
             xc+r*cos(afinish),
             yc-r*sin(afinish));
  }
  // terminate drawing
  OutAscii(op);
}

void
wxPdfDocument::Polygon(const wxPdfArrayDouble& x, const wxPdfArrayDouble& y, int style)
{
  unsigned int np = (x.GetCount() < y.GetCount()) ? (unsigned int) x.GetCount() : (unsigned int) y.GetCount();

  wxString op;
  if ((style & wxPDF_STYLE_FILLDRAW) == wxPDF_STYLE_FILL)
  {
    op = (m_fillRule == wxODDEVEN_RULE) ? wxT("f*") : wxT("f");
  }
  else
  {
    if ((style & wxPDF_STYLE_FILLDRAW) == wxPDF_STYLE_FILLDRAW)
    {
      op = (m_fillRule == wxODDEVEN_RULE) ? wxT("B*") : wxT("B");
    }
    else
    {
      op = wxT("S");
    }
  }

  OutPoint(x[0], y[0]);
  unsigned int i;
  for (i = 1; i < np; i++)
  {
    OutLine(x[i], y[i]);
  }
  OutLine(x[0], y[0]);
  OutAscii(op);
}

void
wxPdfDocument::RegularPolygon(double x0, double y0, double r, int ns, double angle, bool circle, int style, 
                              int circleStyle, const wxPdfLineStyle& circleLineStyle, const wxPdfColour& circleFillColour)
{
  if (ns < 3)
  {
    ns = 3;
  }
  if (circle)
  {
    wxPdfLineStyle saveStyle = GetLineStyle();
    SetLineStyle(circleLineStyle);
    wxPdfColour saveColour = GetFillColour();
    SetFillColour(circleFillColour);
    Circle(x0, y0, r, 0, 360, circleStyle);
    SetLineStyle(saveStyle);
    SetFillColour(saveColour);
  }
  static double pi = 4. * atan(1.);
  double a;
  wxPdfArrayDouble x, y;
  int i;
  for (i = 0; i < ns; i++)
  {
    a = (angle + (i * 360 / ns)) / 180. * pi;
    x.Add(x0 + (r * sin(a)));
    y.Add(y0 + (r * cos(a)));
  }
  Polygon(x, y, style);
}


void
wxPdfDocument::StarPolygon(double x0, double y0, double r, int nv, int ng, double angle, bool circle, int style, 
                           int circleStyle, const wxPdfLineStyle& circleLineStyle, const wxPdfColour& circleFillColour)
{
  if (nv < 2)
  {
    nv = 2;
  }
  if (circle)
  {
    wxPdfLineStyle saveStyle = GetLineStyle();
    SetLineStyle(circleLineStyle);
    wxPdfColour saveColour = GetFillColour();
    SetFillColour(circleFillColour);
    Circle(x0, y0, r, 0, 360, circleStyle);
    SetLineStyle(saveStyle);
    SetFillColour(saveColour);
  }
  wxArrayInt visited;
  visited.SetCount(nv);
  int i;
  for (i = 0; i < nv; i++)
  {
    visited[i] = 0;
  }
  static double pi = 4. * atan(1.);
  double a;
  wxPdfArrayDouble x, y;
  i = 0;
  do
  {
    visited[i] = 1;
    a = (angle + (i * 360 / nv)) / 180. * pi;
    x.Add(x0 + (r * sin(a)));
    y.Add(y0 + (r * cos(a)));
    i = (i + ng) % nv;
  }
  while (visited[i] == 0);
  Polygon(x, y, style);
}

static void
SolveTridiagonalSpecial(const wxPdfArrayDouble& r, wxPdfArrayDouble& x)
{
  size_t i;
  size_t n = r.GetCount();
  x.SetCount(n);
  wxPdfArrayDouble gamma;
  gamma.SetCount(n);

  // Decomposition and forward substitution.
  double beta = 2.0;
  x[0] = r[0] / beta;
  for (i = 1; i < n; ++i)
  {
    gamma[i] = 1 / beta;
    beta = (i < n-1 ? 4.0 : 3.5) - gamma[i];
    x[i] = (r[i] - x[i-1]) / beta;
  }

  // Backsubstitution.
  for (i = 1; i < n; ++i)
  {
    x[n-i-1] -= gamma[n-i] * x[n-i];
  }
}

static bool
GetBezierControlPoints(const wxPdfArrayDouble& x, const wxPdfArrayDouble& y,
                       wxPdfArrayDouble& x1, wxPdfArrayDouble& y1,
                       wxPdfArrayDouble& x2, wxPdfArrayDouble& y2)
{
  size_t i;
  size_t n = x.GetCount() - 1;
  if (n <= 1)
  {
    wxLogDebug(wxString(wxT("GetBezierControlPoints: "))+_("n must be greater than 2."));
    return false;
  }
#if 0
  // Special case: Bezier curve should be a straight line.
  if (n == 1)
  {
    x1[0] = (2 * x[0] + x[1]) / 3;
    y1[0] = (2 * y[0] + y[1]) / 3;
    x2[0] = 2 * x1[0] - x[0];
    y2[0] = 2 * y1[0] - y[0];
    return true;
  }
#endif

  // First control point
  wxPdfArrayDouble r;
  r.SetCount(n);

  // Set right hand side X values
  for (i = 1; i < n-1; ++i)
  {
    r[i] = 4 * x[i] + 2 * x[i+1];
  }
  r[0] = x[0] + 2 * x[1];
  r[n-1] = (8 * x[n-1] + x[n]) / 2.0;

  x1.SetCount(n);
  SolveTridiagonalSpecial(r, x1);

  // Set right hand side Y values
  for (i = 1; i < n-1; ++i)
  {
    r[i] = 4 * y[i] + 2 * y[i+1];
  }
  r[0] = y[0] + 2 * y[1];
  r[n - 1] = (8 * y[n-1] + y[n]) / 2.0;

  y1.SetCount(n);
  SolveTridiagonalSpecial(r, y1);

  // Second control point
  x2.SetCount(n);
  y2.SetCount(n);
  for (i = 0; i < n; ++i)
  {
    if (i < n - 1)
    {
      x2[i] = 2 * x[i+1] - x1[i+1];
      y2[i] = 2 * y[i+1] - y1[i+1];
    }
    else
    {
      x2[i] = (x[n] + x1[n-1]) / 2;
      y2[i] = (y[n] + y1[n-1]) / 2;
    }
  }
  return true;
}

void
wxPdfDocument::BezierSpline(const wxPdfArrayDouble& x, const wxPdfArrayDouble& y, int style)
{
  size_t n = x.GetCount();
  if (n == y.GetCount())
  {
    if (n > 2)
    {
      wxPdfArrayDouble x1, y1, x2, y2;
      if (GetBezierControlPoints(x, y, x1, y1, x2, y2))
      {
        wxString op;
        if ((style & wxPDF_STYLE_FILLDRAW) == wxPDF_STYLE_FILL)
        {
          op = (m_fillRule == wxODDEVEN_RULE) ? wxT("f*") : wxT("f");
        }
        else
        {
          if ((style & wxPDF_STYLE_FILLDRAW) == wxPDF_STYLE_FILLDRAW)
          {
            op = (m_fillRule == wxODDEVEN_RULE) ? wxT("B*") : wxT("B");
          }
          else
          {
            op = wxT("S");
          }
        }
        MoveTo(x[0], y[0]);
        size_t j;
        for (j = 0; j < n-1; ++j)
        {
          CurveTo(x1[j], y1[j], x2[j], y2[j], x[j+1], y[j+1]);
        }
        OutAscii(op);
      }
    }
    else
    {
      Line(x[0], y[0], x[1], y[1]);
    }
  }
}

static bool
SolveTridiagonalGeneral(const wxPdfArrayDouble& a, const wxPdfArrayDouble& b, 
                        const wxPdfArrayDouble& c, const wxPdfArrayDouble& r,
                        wxPdfArrayDouble& u)
{
  size_t n = r.GetCount();
  // a, b, c and rhs vectors must have the same size.
  if (n != a.GetCount() || n != b.GetCount() || n != c.GetCount())
  {
    wxLogDebug(wxString(wxT("SolveTridiagonal: "))+_("Mismatch of vector sizes."));
    return false;
  }
  if (b[0] == 0.0)
  {
    wxLogDebug(wxString(wxT("SolveTridiagonal: "))+_("Singular matrix."));
    return false;
  }

  wxPdfArrayDouble gamma;
  gamma.SetCount(n);
  u.SetCount(n);

  // Decomposition and forward substitution.
  double beta = b[0];
  u[0] = r[0] / beta;
  size_t j;
  for (j = 1; j < n; ++j)
  {
    gamma[j] = c[j-1] / beta;
    beta = b[j] - a[j] * gamma[j];
    if (beta == 0.0)
    {
      wxLogDebug(wxString(wxT("SolveTridiagonal: "))+_("Singular matrix."));
      return false;
    }
    u[j] = (r[j] - a[j] * u[j - 1]) / beta;
  }

  // Backward substitution.
  for (j = 1; j < n; ++j)
  {
    u[n-j-1] -= gamma[n-j] * u[n-j];
  }
  return true;
}

static bool
SolveCyclic(const wxPdfArrayDouble& a, const wxPdfArrayDouble& b, 
            const wxPdfArrayDouble& c, double alpha, double beta, 
            const wxPdfArrayDouble& r, wxPdfArrayDouble& x)
{
  size_t i;
  size_t n = r.GetCount();
  // a, b, c and rhs vectors must have the same size.
  if (n != a.GetCount() || n != b.GetCount() || n != c.GetCount())
  {
    wxLogDebug(wxString(wxT("SolveCyclic: "))+_("Mismatch of vector sizes."));
    return false;
  }
  if (n <= 2)
  {
    wxLogDebug(wxString(wxT("SolveCyclic: "))+_("n must be greater than 2."));
    return false;
  }

  // Set up the diagonal of the modified tridiagonal system.
  wxPdfArrayDouble bb;
  bb.SetCount(n);
  double gamma = -b[0];
  bb[0] = b[0] - gamma;
  bb[n-1] = b[n-1] - alpha * beta / gamma;
  for (i = 1; i < n-1; ++i)
  {
    bb[i] = b[i];
  }
  // Solve A · x = rhs.
  x.SetCount(n);
  if (!SolveTridiagonalGeneral(a, bb, c, r, x))
  {
    return false;
  }

  // Set up the vector u.
  wxPdfArrayDouble u;
  u.SetCount(n, 0.0);
  u[0] = gamma;
  u[n-1] = alpha;

  // Solve A · z = u.
  wxPdfArrayDouble z;
  z.SetCount(n);
  if (!SolveTridiagonalGeneral(a, bb, c, u, z))
  {
    return false;
  }

  // Calculate solution vector x.
  double fact = (x[0] + beta * x[n-1] / gamma) / (1.0 + z[0] + beta * z[n-1] / gamma);
  for (i = 0; i < n; ++i)
  {
    x[i] -= fact * z[i];
  }
  return true;
} 

static bool
GetCyclicControlPoints(const wxPdfArrayDouble& x, const wxPdfArrayDouble& y,
                       wxPdfArrayDouble& x1, wxPdfArrayDouble& y1,
                       wxPdfArrayDouble& x2, wxPdfArrayDouble& y2)
{
  size_t i, j;
  size_t n = x.GetCount();
  bool ok = (n == y.GetCount());
  if (n <= 2 || !ok)
  {
    wxLogDebug(wxString(wxT("GetCyclicControlPoints: "))+_("n must be greater than 2."));
    return false;
  }

  // Calculate first Bezier control points

  // The matrix.
  wxPdfArrayDouble a, b, c;
  a.SetCount(n, 1.0);
  b.SetCount(n, 4.0);
  c.SetCount(n, 1.0);

  // Right hand side vector for points X coordinates.
  wxPdfArrayDouble r;
  r.SetCount(n);
  for (i = 0; i < n; ++i)
  {
    j = (i == n-1) ? 0 : i+1;
    r[i] = 4 * x[i] + 2 * x[j];
  }

  // Solve the system for X.
  x1.SetCount(n);
  if (!SolveCyclic(a, b, c, 1.0, 1.0, r, x1))
  {
    return false;
  }

  // Right hand side vector for points Y coordinates.
  for (i = 0; i < n; ++i)
  {
    j = (i == n - 1) ? 0 : i + 1;
    r[i] = 4 * y[i] + 2 * y[j];
  }
  // Solve the system for Y.
  y1.SetCount(n);
  if (!SolveCyclic(a, b, c, 1, 1, r, y1))
  {
    return false;
  }

  // Second control point.
  x2.SetCount(n);
  y2.SetCount(n);
  for (i = 0; i < n; ++i)
  {
    x2[i] = 2 * x[i] - x1[i];
    y2[i] = 2 * y[i] - y1[i];
  }
  return true;
}

void
wxPdfDocument::ClosedBezierSpline(const wxPdfArrayDouble& x, const wxPdfArrayDouble& y, int style)
{
  size_t n = x.GetCount();
  if (n == y.GetCount())
  {
    if (n > 2)
    {
      wxPdfArrayDouble x1, y1, x2, y2;
      if (GetCyclicControlPoints(x, y, x1, y1, x2, y2))
      {
        wxString op;
        if ((style & wxPDF_STYLE_FILLDRAW) == wxPDF_STYLE_FILL)
        {
          op = (m_fillRule == wxODDEVEN_RULE) ? wxT("f*") : wxT("f");
        }
        else
        {
          if ((style & wxPDF_STYLE_FILLDRAW) == wxPDF_STYLE_FILLDRAW)
          {
            op = (m_fillRule == wxODDEVEN_RULE) ? wxT("B*") : wxT("B");
          }
          else
          {
            op = wxT("S");
          }
        }
        MoveTo(x[0], y[0]);
        size_t j;
        for (j = 1; j < n; ++j)
        {
          CurveTo(x1[j-1], y1[j-1], x2[j], y2[j], x[j], y[j]);
        }
        CurveTo(x1[n-1], y1[n-1], x2[0], y2[0], x[0], y[0]);
        OutAscii(op);
      }
    }
    else
    {
      Line(x[0], y[0], x[1], y[1]);
    }
  }
}

void
wxPdfDocument::Shape(const wxPdfShape& shape, int style)
{
  wxString op;
  if ((style & wxPDF_STYLE_MASK) == wxPDF_STYLE_FILL)
  {
    op = (m_fillRule == wxODDEVEN_RULE) ? wxT("f*") : wxT("f");
  }
  else
  {
    if ((style & wxPDF_STYLE_MASK) == wxPDF_STYLE_FILLDRAW)
    {
      op = (m_fillRule == wxODDEVEN_RULE) ? wxT("B*") : wxT("B");
    }
    else if ((style & wxPDF_STYLE_MASK) == (wxPDF_STYLE_DRAWCLOSE | wxPDF_STYLE_FILL))
    {
      // small 'b' means closing the path as well
      op = (m_fillRule == wxODDEVEN_RULE) ? wxT("b*") : wxT("b");
    }
    else if ((style & wxPDF_STYLE_MASK) == wxPDF_STYLE_DRAWCLOSE)
    {
      op = wxT("s"); // small 's' means closing the path as well
    }
    else
    {
      op = wxT("S");
    }
  }

  Out("q");

  double scratch[6];
  unsigned int iterType;
  unsigned int iterPoints = 0;
  unsigned int segCount = shape.GetSegmentCount();
  for (iterType = 0; iterType < segCount; iterType++)
  {
    int segType = shape.GetSegment(iterType, iterPoints, scratch);
    switch (segType)
    {
      case wxPDF_SEG_CLOSE:
        Out("h");
        iterPoints++;
        break;
      case wxPDF_SEG_MOVETO:
        OutPoint(scratch[0], scratch[1]);
        iterPoints++;
        break;
      case wxPDF_SEG_LINETO:
        OutLine(scratch[0], scratch[1]);
        iterPoints++;
        break;
      case wxPDF_SEG_CURVETO:
        OutCurve(scratch[0], scratch[1], scratch[2], scratch[3],scratch[4], scratch[5]);
        iterPoints += 3;
        break;
    }
  }
  OutAscii(op);
  Out("Q");

//  ClosePath(style);
}

void
wxPdfDocument::ClippingText(double x, double y, const wxString& txt, bool outline)
{
  wxString op = outline ? wxT("5") : wxT("7");
  if (m_yAxisOriginTop)
  {
    OutAscii(wxString(wxT("q BT 1 0 0 -1 ")) +
             wxPdfUtility::Double2String(x*m_k,2) + wxString(wxT(" ")) +
             wxPdfUtility::Double2String(y*m_k,2) + wxString(wxT(" Tm ")) +
             op + wxString(wxT(" Tr (")),false);
  }
  else
  {
    OutAscii(wxString(wxT("q BT ")) +
             wxPdfUtility::Double2String(x*m_k,2) + wxString(wxT(" ")) +
             wxPdfUtility::Double2String(y*m_k,2) + wxString(wxT(" Td ")) +
             op + wxString(wxT(" Tr (")),false);
  }
  TextEscape(txt,false);
  Out(") Tj ET");
  SaveGraphicState();
}

void
wxPdfDocument::ClippingRect(double x, double y, double w, double h, bool outline)
{
  wxString op = outline ? wxT("S") : wxT("n");
  OutAscii(wxString(wxT("q ")) +
           wxPdfUtility::Double2String(x*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(y*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(w*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(h*m_k,2) + wxString(wxT(" re W ")) + op);
  SaveGraphicState();
}

void
wxPdfDocument::ClippingEllipse(double x, double y, double rx, double ry, bool outline)
{
  wxString op = outline ? wxT("S") : wxT("n");
  if (ry <= 0)
  {
    ry = rx;
  }
  double lx = 4./3. * (sqrt(2.)-1.) * rx;
  double ly = 4./3. * (sqrt(2.)-1.) * ry;

  OutAscii(wxString(wxT("q ")) +
           wxPdfUtility::Double2String((x+rx)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(y*m_k,2) + wxString(wxT(" m ")) +
           wxPdfUtility::Double2String((x+rx)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((y-ly)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((x+lx)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((y-ry)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(x*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((y-ry)*m_k,2) + wxString(wxT(" c")));

  OutAscii(wxPdfUtility::Double2String((x-lx)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((y-ry)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((x-rx)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((y-ly)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((x-rx)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(y*m_k,2) + wxString(wxT(" c")));

  OutAscii(wxPdfUtility::Double2String((x-rx)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((y+ly)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((x-lx)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((y+ry)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(x*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((y+ry)*m_k,2) + wxString(wxT(" c")));

  OutAscii(wxPdfUtility::Double2String((x+lx)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((y+ry)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((x+rx)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((y+ly)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((x+rx)*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(y*m_k,2) + wxString(wxT(" c W ")) + op);
  SaveGraphicState();
}

void
wxPdfDocument::ClippingPolygon(const wxPdfArrayDouble& x, const wxPdfArrayDouble& y, bool outline)
{
  unsigned int np = (x.GetCount() < y.GetCount()) ? (unsigned int) x.GetCount() : (unsigned int) y.GetCount();

  wxString op = outline ? wxT("S") : wxT("n");

  Out("q");
  OutPoint(x[0], y[0]);
  unsigned int i;
  for (i = 1; i < np; i++)
  {
    OutLine(x[i], y[i]);
  }
  OutLine(x[0], y[0]);
  OutAscii(wxString(wxT("h W ")) + op);
  SaveGraphicState();
}

void
wxPdfDocument::ClippingPath()
{
  Out("q");
  SaveGraphicState();
}

void
wxPdfDocument::MoveTo(double x, double y)
{
  OutPoint(x, y);
}

void
wxPdfDocument::LineTo(double x, double y)
{
  OutLine(x, y);
}

void
wxPdfDocument::CurveTo(double x1, double y1, double x2, double y2, double x3, double y3)
{
  OutCurve(x1, y1, x2, y2, x3, y3);
}

void
wxPdfDocument::EndPath(int style)
{
  wxString op;
  switch (style)
  {
    case wxPDF_STYLE_FILL:     op = (m_fillRule == wxODDEVEN_RULE) ? wxT("f*") : wxT("f"); break;
    case wxPDF_STYLE_FILLDRAW: op = (m_fillRule == wxODDEVEN_RULE) ? wxT("B*") : wxT("B"); break;
    case wxPDF_STYLE_DRAW:
    default:                   op = wxT("S"); break;
  }
  OutAscii(op);
}

void
wxPdfDocument::ClosePath(int style)
{
  wxString op;
  switch (style)
  {
    case wxPDF_STYLE_DRAW:     op = wxT("S"); break;
    case wxPDF_STYLE_FILL:     op = (m_fillRule == wxODDEVEN_RULE) ? wxT("f*") : wxT("f"); break;
    case wxPDF_STYLE_FILLDRAW: op = (m_fillRule == wxODDEVEN_RULE) ? wxT("B*") : wxT("B"); break;
    default:                   op = wxT("n"); break;
  }
  OutAscii(wxString(wxT("h W ")) + op);
}

void
wxPdfDocument::ClippingPath(const wxPdfShape& shape, int style)
{
  ClippingPath();
  double scratch[6];
  unsigned int iterType;
  unsigned int iterPoints = 0;
  unsigned int segCount = shape.GetSegmentCount();
  for (iterType = 0; iterType < segCount; iterType++)
  {
    int segType = shape.GetSegment(iterType, iterPoints, scratch);
    switch (segType)
    {
      case wxPDF_SEG_CLOSE:
        iterPoints++;
        break;
      case wxPDF_SEG_MOVETO:
        MoveTo(scratch[0], scratch[1]);
        iterPoints++;
        break;
      case wxPDF_SEG_LINETO:
        LineTo(scratch[0], scratch[1]);
        iterPoints++;
        break;
      case wxPDF_SEG_CURVETO:
        CurveTo(scratch[0], scratch[1], scratch[2], scratch[3],scratch[4], scratch[5]);
        iterPoints += 3;
        break;
    }
  }
  ClosePath(style);
}

void
wxPdfDocument::UnsetClipping()
{
  Out("Q");
  RestoreGraphicState();
}

void
wxPdfDocument::ClippedCell(double w, double h, const wxString& txt,
                           int border, int ln, int align, int fill, const wxPdfLink& link)
{
  bool doPageBreak = (m_yAxisOriginTop) ? (m_y+h > m_pageBreakTrigger) : (m_y-h < m_pageBreakTrigger);
  if ((border != wxPDF_BORDER_NONE) || (fill != 0) || doPageBreak)
  {
    Cell(w, h, wxT(""), border, 0, wxPDF_ALIGN_LEFT, fill);
    m_x -= w;
  }
  ClippingRect(m_x, m_y, w, h);
  Cell(w, h, txt, wxPDF_BORDER_NONE, ln, align, 0, link);
  UnsetClipping();
}

void
wxPdfDocument::SetLineStyle(const wxPdfLineStyle& linestyle)
{
  m_lineStyle = linestyle;
  if (linestyle.GetWidth() >= 0)
  {
    double width_prev = m_lineWidth;
    SetLineWidth(linestyle.GetWidth());
    m_lineWidth = width_prev;
  }
  switch (linestyle.GetLineCap())
  {
    case wxPDF_LINECAP_BUTT:
    case wxPDF_LINECAP_ROUND:
    case wxPDF_LINECAP_SQUARE:
      OutAscii(wxString::Format(wxT("%d  J"), linestyle.GetLineCap()));
      break;
    default:
      break;
  }
  switch (linestyle.GetLineJoin())
  {
    case wxPDF_LINEJOIN_MITER:
    case wxPDF_LINEJOIN_ROUND:
    case wxPDF_LINEJOIN_BEVEL:
      OutAscii(wxString::Format(wxT("%d  j"), linestyle.GetLineJoin()));
      break;
    default:
      break;
  }

  const wxPdfArrayDouble& dash = linestyle.GetDash();
  if (&dash != NULL)
  {
    wxString dashString = wxT("");
    size_t j;
    for (j = 0; j < dash.GetCount(); j++)
    {
      if (j > 0)
      {
        dashString += wxString(wxT(" "));
      }
      dashString += wxPdfUtility::Double2String(dash[j]*m_k,2);
    }
    double phase = linestyle.GetPhase();
    if (phase < 0 || dashString.Length() == 0)
    {
      phase = 0;
    }
    OutAscii(wxString(wxT("[")) + dashString + wxString(wxT("] ")) +
             wxPdfUtility::Double2String(phase*m_k,2) + wxString(wxT(" d")));
  }
  SetDrawColour(linestyle.GetColour());
}

const wxPdfLineStyle&
wxPdfDocument::GetLineStyle()
{
  return m_lineStyle;
}

void
wxPdfDocument::StartTransform()
{
  //save the current graphic state
  m_inTransform++;
  Out("q");
  SaveGraphicState();
}

bool
wxPdfDocument::ScaleX(double sx, double x, double y)
{
  return Scale(sx, 100, x, y);
}

bool
wxPdfDocument::ScaleY(double sy, double x, double y)
{
  return Scale(100, sy, x, y);
}

bool
wxPdfDocument::ScaleXY(double s, double x, double y)
{
  return Scale(s, s, x, y);
}

bool
wxPdfDocument::Scale(double sx, double sy, double x, double y)
{
  if (x < 0)
  {
    x = m_x;
  }
  if (y < 0)
  {
    y = m_y;
  }
  if (sx == 0 || sy == 0)
  {
    wxLogError(wxString(wxT("wxPdfDocument::Scale: ")) +
               wxString(_("Please use values unequal to zero for Scaling.")));
    return false;
  }
  y *= m_k;
  x *= m_k;
  //calculate elements of transformation matrix
  sx /= 100;
  sy /= 100;
  double tm[6];
  tm[0] = sx;
  tm[1] = 0;
  tm[2] = 0;
  tm[3] = sy;
  tm[4] = x * (1 - sx);
  tm[5] = y * (1 - sy);
  //scale the coordinate system
  if (m_inTransform == 0)
  {
    StartTransform();
  }
  Transform(tm);
  return true;
}

void
wxPdfDocument::MirrorH(double x)
{
  Scale(-100, 100, x);
}

void
wxPdfDocument::MirrorV(double y)
{
  Scale(100, -100, -1, y);
}

void
wxPdfDocument::TranslateX(double tx)
{
  Translate(tx, 0);
}

void
wxPdfDocument::TranslateY(double ty)
{
  Translate(0, ty);
}

void
wxPdfDocument::Translate(double tx, double ty)
{
  if (m_inTransform == 0)
  {
    StartTransform();
  }
  // calculate elements of transformation matrix
  double tm[6];
  tm[0] = 1;
  tm[1] = 0;
  tm[2] = 0;
  tm[3] = 1;
  tm[4] = tx;
  tm[5] = (m_yAxisOriginTop) ? ty : -ty;
  // translate the coordinate system
  Transform(tm);
}

void
wxPdfDocument::Rotate(double angle, double x, double y)
{
  if (m_inTransform == 0)
  {
    StartTransform();
  }
  if (x < 0)
  {
    x = m_x;
  }
  if (y < 0)
  {
    y = m_y;
  }
  y *= m_k;
  x *= m_k;
  // calculate elements of transformation matrix
  double tm[6];
  if (m_yAxisOriginTop)
  {
    angle *= -1.0;
  }
  angle *= (atan(1.) / 45.);
  tm[0] = cos(angle);
  tm[1] = sin(angle);
  tm[2] = -tm[1];
  tm[3] = tm[0];
  tm[4] = x + tm[1] * y - tm[0] * x;
  tm[5] = y - tm[0] * y - tm[1] * x;
  //rotate the coordinate system around ($x,$y)
  Transform(tm);
}

void
wxPdfDocument::Transform( double a, double b, double c, double d, double tx, double ty )
{
  if (m_inTransform == 0)
  {
    StartTransform();
  }
  // copy the elements of transformation matrix
  double tm[6];
  tm[0] = a;
  tm[1] = b;
  tm[2] = c;
  tm[3] = d;
  tm[4] = tx;
  tm[5] = ty;

  Transform(tm);
}

bool
wxPdfDocument::SkewX(double xAngle, double x, double y)
{
  return Skew(xAngle, 0, x, y);
}

bool
wxPdfDocument::SkewY(double yAngle, double x, double y)
{
  return Skew(0, yAngle, x, y);
}

bool
wxPdfDocument::Skew(double xAngle, double yAngle, double x, double y)
{
  if (x < 0)
  {
    x = m_x;
  }
  if (y < 0)
  {
    y = m_y;
  }
  if (xAngle <= -90 || xAngle >= 90 || yAngle <= -90 || yAngle >= 90)
  {
    wxLogError(wxString(wxT("wxPdfDocument::Skew: ")) +
               wxString(_("Please use values between -90 and 90 degree for skewing.")));
    return false;
  }
  x *= m_k;
  y *= m_k;
  //calculate elements of transformation matrix
  double tm[6];
  if (m_yAxisOriginTop)
  {
    xAngle *= -1.0;
    yAngle *= -1.0;
  }
  xAngle *= (atan(1.) / 45.);
  yAngle *= (atan(1.) / 45.);
  tm[0] = 1;
  tm[1] = tan(yAngle);
  tm[2] = tan(xAngle);
  tm[3] = 1;
  tm[4] = -tm[2] * y;
  tm[5] = -tm[1] * x;
  //skew the coordinate system
  if (m_inTransform == 0)
  {
    StartTransform();
  }
  Transform(tm);
  return true;
}

void
wxPdfDocument::StopTransform()
{
  //restore previous graphic state
  if (m_inTransform > 0)
  {
    m_inTransform--;
    Out("Q");
    RestoreGraphicState();
  }
}

static bool
ColourSpaceOk(const wxPdfColour& col1, const wxPdfColour& col2)
{
  return (col1.GetColourType() != wxPDF_COLOURTYPE_SPOT &&
          col1.GetColourType() == col2.GetColourType());
}

int
wxPdfDocument::LinearGradient(const wxPdfColour& col1, const wxPdfColour& col2,
                              wxPdfLinearGradientType gradientType)
{
  static double h[] = { 0, 0, 1, 0 };
  static double v[] = { 0, 0, 0, 1 };
  wxPdfGradient* gradient;

  int n = 0;
  if (ColourSpaceOk(col1, col2))
  {
    switch (gradientType)
    {
      case wxPDF_LINEAR_GRADIENT_REFLECTION_TOP:
        gradient = new wxPdfMidAxialGradient(col1, col2, v[0], v[1], v[2], v[3], 0.67, 0.7);
        break;
      case wxPDF_LINEAR_GRADIENT_REFLECTION_BOTTOM:
        gradient = new wxPdfMidAxialGradient(col1, col2, v[0], v[1], v[2], v[3], 0.33, 0.7);
        break;
      case wxPDF_LINEAR_GRADIENT_REFLECTION_LEFT:
        gradient = new wxPdfMidAxialGradient(col1, col2, h[0], h[1], h[2], h[3], 0.33, 0.7);
        break;
      case wxPDF_LINEAR_GRADIENT_REFLECTION_RIGHT:
        gradient = new wxPdfMidAxialGradient(col1, col2, h[0], h[1], h[2], h[3], 0.67, 0.7);
        break;
      case wxPDF_LINEAR_GRADIENT_MIDVERTICAL:
        gradient = new wxPdfMidAxialGradient(col1, col2, v[0], v[1], v[2], v[3], 0.5, 1);
        break;
      case wxPDF_LINEAR_GRADIENT_MIDHORIZONTAL:
        gradient = new wxPdfMidAxialGradient(col1, col2, h[0], h[1], h[2], h[3], 0.5, 1);
        break;
      case wxPDF_LINEAR_GRADIENT_VERTICAL:
        gradient = new wxPdfAxialGradient(col1, col2, v[0], v[1], v[2], v[3], 1);
        break;
      case wxPDF_LINEAR_GRADIENT_HORIZONTAL:
      default:
        gradient = new wxPdfAxialGradient(col1, col2, h[0], h[1], h[2], h[3], 1);
        break;
    }
    n = (int) (*m_gradients).size()+1;
    (*m_gradients)[n] = gradient;
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfDocument::LinearGradient: ")) +
               wxString(_("Colour spaces do not match.")));
  }
  return n;
}

int
wxPdfDocument::AxialGradient(const wxPdfColour& col1, const wxPdfColour& col2,
                             double x1, double y1, double x2, double y2,
                             double intexp)
{
  int n = 0;
  if (ColourSpaceOk(col1, col2))
  {
    n = (int) (*m_gradients).size()+1;
    (*m_gradients)[n] = new wxPdfAxialGradient(col1, col2, x1, y1, x2, y2, intexp);
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfDocument::AxialGradient: ")) +
               wxString(_("Colour spaces do not match.")));
  }
  return n;
}

int
wxPdfDocument::MidAxialGradient(const wxPdfColour& col1, const wxPdfColour& col2,
                               double x1, double y1, double x2, double y2,
                               double midpoint, double intexp)
{
  int n = 0;
  if (ColourSpaceOk(col1, col2))
  {
    n = (int) (*m_gradients).size()+1;
    (*m_gradients)[n] = new wxPdfMidAxialGradient(col1, col2, x1, y1, x2, y2, midpoint, intexp);
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfDocument::MidAxialGradient: ")) +
               wxString(_("Colour spaces do not match.")));
  }
  return n;
}

int
wxPdfDocument::RadialGradient(const wxPdfColour& col1, const wxPdfColour& col2,
                              double x1, double y1, double r1,
                              double x2, double y2, double r2, double intexp)
{
  int n = 0;
  if (ColourSpaceOk(col1, col2))
  {
    n = (int) (*m_gradients).size()+1;
    (*m_gradients)[n] = new wxPdfRadialGradient(col1, col2, x1, y1, r1, x2, y2, r2, intexp);
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfDocument::RadialGradient: ")) +
               wxString(_("Colour spaces do not match.")));
  }
  return n;
}

int
wxPdfDocument::CoonsPatchGradient(const wxPdfCoonsPatchMesh& mesh, double minCoord, double maxCoord)
{
  int n = 0;
  if (mesh.Ok())
  {
    n = (int) (*m_gradients).size()+1;
    (*m_gradients)[n] = new wxPdfCoonsPatchGradient(mesh, minCoord, maxCoord);
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfDocument::CoonsPatchGradient: ")) +
               wxString(_("Mesh is invalid.")));
  }
  return n;
}

/* draw a marker at a raw point-based coordinate */
void
wxPdfDocument::Marker(double x, double y, wxPdfMarker markerType, double size)
{
  double saveLineWidth = m_lineWidth;
  double halfsize = size * 0.5;
  static double b = 4. / 3.;

  Out("q");
  switch (markerType) 
  {
    case wxPDF_MARKER_CIRCLE:
      SetLineWidth(size * 0.15);
      OutPoint(x - halfsize, y);
      OutCurve(x - halfsize, y + b * halfsize, x + halfsize, y + b * halfsize, x + halfsize, y);
      OutCurve(x + halfsize, y - b * halfsize, x - halfsize, y - b * halfsize, x - halfsize, y);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_TRIANGLE_UP:
      SetLineWidth(size * 0.15);
      OutPoint(x, y - size * 0.6667);
      OutLineRelative(-size / 1.7321, size);
      OutLineRelative(1.1546 * size, 0.0);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_TRIANGLE_DOWN:
      SetLineWidth(size * 0.15);
      OutPoint(x, y + size * 0.6667);
      OutLineRelative(-size / 1.7321, -size);
      OutLineRelative(1.1546 * size, 0.0);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_TRIANGLE_LEFT:
      SetLineWidth(size * 0.15);
      OutPoint(x - size * 0.6667, y);
      OutLineRelative(size, -size / 1.7321);
      OutLineRelative(0.0, 1.1546 * size);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_TRIANGLE_RIGHT:
      SetLineWidth(size * 0.15);
      OutPoint(x + size * 0.6667, y);
      OutLineRelative(-size, -size / 1.7321);
      OutLineRelative(0.0, 1.1546 * size);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_DIAMOND:
      SetLineWidth(size * 0.15);
      size *= 0.9;
      OutPoint( x, y+size/1.38);
      OutLineRelative( 0.546 * size, -size / 1.38);
      OutLineRelative(-0.546 * size, -size / 1.38);
      OutLineRelative(-0.546 * size,  size / 1.38);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_SQUARE:
      SetLineWidth(size * 0.15);
      Rect(x - halfsize, y - halfsize, size, size, wxPDF_STYLE_FILLDRAW);
      Out("B");
      break;
    case wxPDF_MARKER_STAR:
      size *= 1.2;
      halfsize = 0.5 * size;
      SetLineWidth(size * 0.09);
      OutPoint(x, y + size * 0.5);
      OutLine(x + 0.112255 * size, y + 0.15451 * size);
      OutLine(x + 0.47552  * size, y + 0.15451 * size);
      OutLine(x + 0.181635 * size, y - 0.05902 * size);
      OutLine(x + 0.29389  * size, y - 0.40451 * size);
      OutLine(x, y - 0.19098 * size);
      OutLine(x - 0.29389  * size, y - 0.40451 * size);
      OutLine(x - 0.181635 * size, y - 0.05902 * size);
      OutLine(x - 0.47552  * size, y + 0.15451 * size);
      OutLine(x - 0.112255 * size, y + 0.15451 * size);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_STAR4:
      size *= 1.2;
      halfsize = 0.5 * size;
      SetLineWidth(size * 0.09);
      OutPoint(x, y + size * 0.5);
      OutLine(x + 0.125 * size, y + 0.125 * size);
      OutLine(x + size * 0.5, y);
      OutLine(x + 0.125 * size, y - 0.125 * size);
      OutLine(x, y - size * 0.5);
      OutLine(x - 0.125 * size, y - 0.125 * size);
      OutLine(x - size * 0.5, y);
      OutLine(x - 0.125 * size, y + 0.125 * size);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_PLUS:
      size *= 1.2;
      halfsize = 0.5 * size;
      SetLineWidth(size * 0.1);
      OutPoint(x + 0.125 * size, y + size * 0.5);
      OutLine(x + 0.125 * size, y + 0.125 * size);
      OutLine(x + size * 0.5, y + 0.125 * size);
      OutLine(x + size * 0.5, y - 0.125 * size);
      OutLine(x + 0.125 * size, y - 0.125 * size);
      OutLine(x + 0.125 * size, y - size * 0.5);
      OutLine(x - 0.125 * size, y - size * 0.5);
      OutLine(x - 0.125 * size, y - 0.125 * size);
      OutLine(x - size * 0.5, y - 0.125 * size);
      OutLine(x - size * 0.5, y + 0.125 * size);
      OutLine(x - 0.125 * size, y + 0.125 * size);
      OutLine(x - 0.125 * size, y + size * 0.5);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_CROSS:
      size *= 1.2;
      halfsize = 0.5 * size;
      SetLineWidth(size * 0.1);
      OutPoint(x, y + 0.176777 * size);
      OutLine(x + 0.265165 * size, y + 0.441941 * size);
      OutLine(x + 0.441941 * size, y + 0.265165 * size);
      OutLine(x + 0.176777 * size, y);
      OutLine(x + 0.441941 * size, y - 0.265165 * size);
      OutLine(x + 0.265165 * size, y - 0.441941 * size);
      OutLine(x, y - 0.176777 * size);
      OutLine(x - 0.265165 * size, y - 0.441941 * size);
      OutLine(x - 0.441941 * size, y - 0.265165 * size);
      OutLine(x - 0.176777 * size, y);
      OutLine(x - 0.441941 * size, y + 0.265165 * size);
      OutLine(x - 0.265165 * size, y + 0.441941 * size);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_PENTAGON_UP:
      SetLineWidth(size * 0.15);
      OutPoint(x + 0.5257 * size, y - size * 0.1708);
      OutLineRelative(-0.5257 * size, -0.382  * size);
      OutLineRelative(-0.5257 * size, 0.382  * size);
      OutLineRelative(0.2008 * size, 0.6181 * size);
      OutLineRelative(0.6499 * size,  0.0);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_PENTAGON_DOWN:
      SetLineWidth(size * 0.15);
      OutPoint(x - 0.5257 * size, y + size * 0.1708);
      OutLineRelative( 0.5257 * size,  0.382  * size);
      OutLineRelative( 0.5257 * size, -0.382  * size);
      OutLineRelative(-0.2008 * size, -0.6181 * size);
      OutLineRelative(-0.6499 * size,  0.0);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_PENTAGON_LEFT:
      SetLineWidth(size * 0.15);
      OutPoint(x - size * 0.1708, y + 0.5257 * size);
      OutLineRelative(-0.382  * size, -0.5257 * size);
      OutLineRelative( 0.382  * size, -0.5257 * size);
      OutLineRelative( 0.6181 * size,  0.2008 * size);
      OutLineRelative( 0.0,            0.6499 * size);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_PENTAGON_RIGHT:
      SetLineWidth(size * 0.15);
      OutPoint(x + size * 0.1708, y - 0.5257 * size);
      OutLineRelative( 0.382  * size,  0.5257 * size);
      OutLineRelative(-0.382  * size,  0.5257 * size);
      OutLineRelative(-0.6181 * size, -0.2008 * size);
      OutLineRelative( 0.0,           -0.6499 * size);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_BOWTIE_HORIZONTAL:
      SetLineWidth(size * 0.13);
      OutPoint(x - 0.5 * size, y - 0.5 * size);
      OutLine(x + 0.5 * size, y + 0.5 * size);
      OutLine(x + 0.5 * size, y - 0.5 * size);
      OutLine(x - 0.5 * size, y + 0.5 * size);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_BOWTIE_VERTICAL:
      SetLineWidth(size * 0.13);
      OutPoint(x - 0.5 * size, y - 0.5 * size);
      OutLine(x + 0.5 * size, y + 0.5 * size);
      OutLine(x - 0.5 * size, y + 0.5 * size);
      OutLine(x + 0.5 * size, y - 0.5 * size);
      Out("h");
      Out("B");
      break;
    case wxPDF_MARKER_ASTERISK:
      size *= 1.05;
      SetLineWidth(size * 0.15);
      OutPoint( x, y + size * 0.5);
      OutLineRelative(0.0, -size);
      OutPoint( x + 0.433 * size, y + 0.25 * size);
      OutLine(x - 0.433 * size, y - 0.25 * size);
      OutPoint(x + 0.433 * size, y - 0.25 * size);
      OutLine(x - 0.433 * size, y + 0.25 * size);
      Out("S");
      break;
    case wxPDF_MARKER_SUN:
      SetLineWidth(size * 0.15);
      halfsize = size * 0.25;
      OutPoint(x - halfsize, y);
      OutCurve(x - halfsize, y + b * halfsize, x + halfsize, y + b * halfsize, x + halfsize, y);
      OutCurve(x + halfsize, y - b * halfsize, x - halfsize, y - b * halfsize, x - halfsize, y);
      Out("h");
      OutPoint(x + size * 0.5, y);
      OutLine(x + size * 0.25, y);
      OutPoint(x - size * 0.5, y);
      OutLine(x - size * 0.25, y);
      OutPoint(x, y - size * 0.5);
      OutLine(x, y - size * 0.25);
      OutPoint(x, y + size * 0.5);
      OutLine(x, y + size * 0.25);
      Out("B");
      break;

    default:
      break;
  }
  Out("Q");
  m_x = x;
  m_y = y;
  SetLineWidth(saveLineWidth);
}

void
wxPdfDocument::Arrow(double x1, double y1, double x2, double y2, double linewidth, double height, double width)
{
  double saveLineWidth = m_lineWidth;
  double dx = x2 - x1;
  double dy = y2 - y1;
  double dz = sqrt (dx*dx+dy*dy);
  double sina = dy / dz;
  double cosa = dx / dz;
  double x3 = x2 - cosa * height + sina * width;
  double y3 = y2 - sina * height - cosa * width;
  double x4 = x2 - cosa * height - sina * width;
  double y4 = y2 - sina * height + cosa * width;

  SetLineWidth(0.2);

  //Draw a arrow head
  OutAscii(wxPdfUtility::Double2String( x2*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String( y2*m_k,2) + wxString(wxT(" m ")) +
           wxPdfUtility::Double2String( x3*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String( y3*m_k,2) + wxString(wxT(" l ")) +
           wxPdfUtility::Double2String( x4*m_k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String( y4*m_k,2) + wxString(wxT(" l b")));

  SetLineWidth(linewidth);
  Line(x1+cosa*linewidth, y1+sina*linewidth, x2-cosa*height, y2-sina*height);
  SetLineWidth(saveLineWidth);
}

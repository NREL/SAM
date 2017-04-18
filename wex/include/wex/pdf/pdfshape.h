///////////////////////////////////////////////////////////////////////////////
// Name:        pdfshape.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-05-14
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfshape.h Interface of the wxPdfShape class

#ifndef _PDF_SHAPE_H_
#define _PDF_SHAPE_H_

// wxWidgets headers
#include <wx/dynarray.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdfarraydouble.h"

/// Shape segment types
enum wxPdfSegmentType
{
  wxPDF_SEG_UNDEFINED,
  wxPDF_SEG_MOVETO,
  wxPDF_SEG_LINETO,
  wxPDF_SEG_CURVETO,
  wxPDF_SEG_CLOSE
};

/// Class representing a shape consisting of line and curve segments
class WXDLLIMPEXP_PDFDOC wxPdfShape
{
public:
  /// Constructor
  wxPdfShape();

  /// Destructor
  virtual ~wxPdfShape();

  /// Begin a new subpath of the shape
  /**
  * Move to the starting point of a new (sub)path.
  * The new current point is (x, y).
  * \param x abscissa value
  * \param y ordinate value
  * \remark This must be the first operation in constructing the shape.
  */
  void MoveTo(double x, double y);

  /// Add line segment to the shape
  /**
  * Append a straight line segment from the current point to the point (x, y).
  * The new current point is (x, y).
  * \param x abscissa value
  * \param y ordinate value
  */
  void LineTo(double x, double y);

  /// Add a cubic Bezier curve to the shape
  /**
  * Append a cubic Bezier curve to the current path. The curve extends
  * from the current point to the point (x3, y3), using (x1, y1) and (x2, y2)
  * as the Bezier control points. The new current point is (x3, y3).
  * \param x1: Abscissa of control point 1
  * \param y1: Ordinate of control point 1
  * \param x2: Abscissa of control point 2
  * \param y2: Ordinate of control point 2
  * \param x3: Abscissa of end point
  * \param y3: Ordinate of end point
  */
  void CurveTo(double x1, double y1, double x2, double y2, double x3, double y3);

  /// Close (sub)path of the shape
  void ClosePath();

  /// Get the number of segments of the shape
  unsigned int GetSegmentCount() const { return (unsigned int) m_types.GetCount(); }

  /// Get a specific segment of the shape (for internal use only)
  /**
  * \param[in] iterType index of segment in segment type array
  * \param[in] iterPoints index of segment in segment coordinate array
  * \param[out] coords array of segment coordinates (size: >= 8)
  * \returns the type of the segment
  */
  wxPdfSegmentType GetSegment(int iterType, int iterPoints, double coords[]) const;

private:
  wxArrayInt       m_types;   ///< array of segment types
  wxPdfArrayDouble m_x;       ///< array of abscissa values
  wxPdfArrayDouble m_y;       ///< array of ordinate values
  int              m_subpath; ///< subpath index
  int              m_segment; ///< segment index
  int              m_index;   ///< points index
};

#endif

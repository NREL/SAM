///////////////////////////////////////////////////////////////////////////////
// Name:        pdfgraphics.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2006-08-24
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfgraphics.h Interface of PDF graphics classes

#ifndef _PDF_GRAPHICS_H_
#define _PDF_GRAPHICS_H_

// wxWidgets headers

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

// TODO: include Header for wxPdfBlendMode

class WXDLLIMPEXP_FWD_PDFDOC wxPdfShape;

/// Class representing ExtGState. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfExtGState
{
public:
  /// Constructor
  wxPdfExtGState(double lineAlpha, double fillAlpha, wxPdfBlendMode blendMode);

  /// Destructor
  virtual ~wxPdfExtGState();

  /// Set ExtGState object index
  void SetObjIndex(int n) { m_n = n; }

  /// Get ExtGState object index
  int  GetObjIndex() { return m_n; }

  /// Get the alpha value for drawing operations
  double GetLineAlpha() const { return m_lineAlpha; }

  /// Get the alpha value for filling operations
  double GetFillAlpha() const { return m_fillAlpha; }

  /// Get the blend mode
  wxPdfBlendMode GetBlendMode() const { return m_blendMode; };

private:
  int            m_n;         ///< ExtGState index
  double         m_lineAlpha; ///< alpha value for drawing operations
  double         m_fillAlpha; ///< alpha value for filling operations
  wxPdfBlendMode m_blendMode;
};

/// Class representing a flattened path
class WXDLLIMPEXP_PDFDOC wxPdfFlatPath
{
public:
  /// Constructor
  wxPdfFlatPath(const wxPdfShape* shape, double flatness = 1, int limit = 10);

  /// Destructor
  virtual ~wxPdfFlatPath();

  /// Initialize path iterator
  void InitIter();

  /// Fetch current path segment
  void FetchSegment();

  /// Advance path iterator
  void Next();

  /// Get current path segment
  /**
  * \param[out] coords coordinates of current segment
  * \return current segment type
  */
  int CurrentSegment(double coords[]);

  /// Subdivide cubic bezier curve path
  void SubdivideCubic();

  /// Check whether path iterator is done
  bool IsDone() { return m_done; }

  /// Measure flattened path length
  double MeasurePathLength();

private:
  const wxPdfShape* m_shape;    ///< associated shape
  double      m_flatnessSq;     ///< square of flatness
  int         m_recursionLimit; ///< resursion limit
  int         m_stackMaxSize;   ///< maximal stack size
  int         m_stackSize;      ///< current stack size
  double*     m_stack;          ///< recursion stack
  int*        m_recLevel;       ///< level stack
  double      m_scratch[6];     ///< coordinate array for current segment
  int         m_iterType;       ///< iterator for segment type
  int         m_iterPoints;     ///< iterator for segment points
  int         m_srcSegType;     ///< current segment type
  double      m_srcPosX;        ///< current segment abscissa
  double      m_srcPosY;        ///< current segment ordinate
  bool        m_done;           ///< flag whether iterator is done
};

#endif

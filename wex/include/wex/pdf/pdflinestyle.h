///////////////////////////////////////////////////////////////////////////////
// Name:        pdflinestyle.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-05-25
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdflinestyle.h Interface of the line style class

#ifndef _PDF_LINE_STYLE_H_
#define _PDF_LINE_STYLE_H_

// wxWidgets headers

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdfarraydouble.h"
#include "wex/pdf/pdfcolour.h"

/// Line Cap options
enum wxPdfLineCap
{
  wxPDF_LINECAP_NONE   = -1,
  wxPDF_LINECAP_BUTT   = 0,
  wxPDF_LINECAP_ROUND  = 1,
  wxPDF_LINECAP_SQUARE = 2
};

/// Line join options
enum wxPdfLineJoin
{
  wxPDF_LINEJOIN_NONE  = -1,
  wxPDF_LINEJOIN_MITER = 0,
  wxPDF_LINEJOIN_ROUND = 1,
  wxPDF_LINEJOIN_BEVEL = 2
};

/// Class representing line styles.
class WXDLLIMPEXP_PDFDOC wxPdfLineStyle
{
public:
  /// Constructor
  /**
  * Creates a line style for use in graphics primitives.
  * \param[in] width Width of the line in user units.
  * \param[in] cap   Type of cap to put on the line (butt, round, square).
  *                  The difference between 'square' and 'butt' is that 'square'
  *                  projects a flat end past the end of the line.
  * \param[in] join  form of line joining: miter, round or bevel
  * \param[in] dash  pattern for dashed lines.Is an empty array (without dash) or
  *   array with series of length values, which are the lengths of the on and off dashes.
  *           For example: (2) represents 2 on, 2 off, 2 on , 2 off ...
  *                        (2,1) is 2 on, 1 off, 2 on, 1 off.. etc
  * \param[in] phase Modifier of the dash pattern which is used to shift the point at which the pattern starts
  * \param[in] colour line colour.
  * \see SetLineStyle(), Curve(), Line(), Circle(), Ellipse(), Rect(), RoundedRect(), Polygon(), RegularPolygon(), StarPolygon()
  */
  wxPdfLineStyle(double width = -1,
                 wxPdfLineCap cap = wxPDF_LINECAP_NONE, wxPdfLineJoin join = wxPDF_LINEJOIN_NONE,
                 const wxPdfArrayDouble& dash = wxPdfArrayDouble(), double phase = -1,
                 const wxPdfColour& colour = wxPdfColour());

  /// Copy constructor
  wxPdfLineStyle(const wxPdfLineStyle& lineStyle);

  /// Assignment operator
  wxPdfLineStyle& operator= (const wxPdfLineStyle& lineStyle);

  /// Destructor
  virtual ~wxPdfLineStyle();

  /// Check whether the style is initialized.
  bool IsSet() const { return m_isSet; }

  /// Set the line width
  void SetWidth(double width) { m_width = width; }

  /// Get the line width
  double GetWidth() const { return m_width; }

  /// Set the line ending style
  void SetLineCap(const wxPdfLineCap cap) { m_cap = cap; }

  /// Get the line ending style
  wxPdfLineCap GetLineCap() const { return m_cap; }

  /// Set the line join style
  void SetLineJoin(const wxPdfLineJoin join) { m_join = join; }

  /// Get the line join style
  wxPdfLineJoin GetLineJoin() const { return m_join; }

  /// Set the dash pattern
  void SetDash(const wxPdfArrayDouble& dash) { m_dash = dash; }

  /// Get the dash pattern
  const wxPdfArrayDouble& GetDash() const { return m_dash; }

  /// Set the dash pattern phase
  void SetPhase(double phase) { m_phase = phase; }

  /// Get the dash pattern phase
  double GetPhase() const { return m_phase; }

  /// Set the line colour
  void SetColour(const wxPdfColour& colour) { m_colour = colour; };

  /// Get the line colour
  const wxPdfColour& GetColour() const { return m_colour; };

private:
  bool             m_isSet;   ///< Flag whether the style is initialized
  double           m_width;   ///< Line width
  wxPdfLineCap     m_cap;     ///< Line ending style
  wxPdfLineJoin    m_join;    ///< Line joining style
  wxPdfArrayDouble m_dash;    ///< Dash pattern
  double           m_phase;   ///< Dash pattern phase
  wxPdfColour      m_colour;   ///< Line colour
};

#endif

///////////////////////////////////////////////////////////////////////////////
// Name:        pdfannotation.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-06-24
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfannotation.h Interface of the wxPdfAnnotation class

#ifndef _PDF_ANNOTATION_H_
#define _PDF_ANNOTATION_H_

// wxWidgets headers
#include <wx/string.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

/// Class representing text annotations.
class WXDLLIMPEXP_PDFDOC wxPdfAnnotation
{
public:
  /// Constructor for text annotation
  /**
  * Use this constructor to create a text annotation.
  * \param x X offset of the annotation
  * \param y Y offset of the annotation
  * \param text annotation text
  */
  wxPdfAnnotation(double x, double y, const wxString& text);

  /// Copy constructor
  wxPdfAnnotation(const wxPdfAnnotation& annotation);

  /// Destructor
  virtual ~wxPdfAnnotation() {}

  /// Get the X offset of the annotation
  double GetX() const { return m_x; }

  /// Get the Y offset of the annotation
  double GetY() const { return m_y; }

  /// Get the text of the annotation
  wxString GetText() const { return m_text; }

private:
  double   m_x;     ///< X offset of the annotation
  double   m_y;     ///< Y offset of the annotation
  wxString m_text;  ///< Annotation text
};

#endif

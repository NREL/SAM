///////////////////////////////////////////////////////////////////////////////
// Name:        pdfcjkfontdata.h
// Purpose:     Definition of CJK font data structures
// Author:      Ulrich Telle
// Modified by:
// Created:     2010-03-29
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfcjkfontdata.h Definition of CJK font data structures

#ifndef _PDF_CJK_FONT_DATA_H_
#define _PDF_CJK_FONT_DATA_H_

/// Structure describing core fonts (For internal use only)
typedef struct _wxPdfCjkFontDesc
{
  const wxChar*      family;             ///< font family
  const wxChar*      name;               ///< font name
  const wxChar*      encoding;           ///< font encoding
  const wxChar*      ordering;           ///< registry ordering
  const wxChar*      supplement;         ///< registry supplement
  const wxChar*      cmap;               ///< font cmap
  short*             cwArray;            ///< array of character widths
  const wxChar*      bbox;               ///< bounding box
  int                ascent;             ///< ascender
  int                descent;            ///< descender
  int                capHeight;          ///< height of capital characters
  int                flags;              ///< font flags
  int                italicAngle;        ///< italic angle
  int                stemV;              ///< stemV value
  int                missingWidth;       ///< width used for missing characters
  int                xHeight;            ///< height of the character X
  int                underlinePosition;  ///< position of the underline decoration
  int                underlineThickness; ///< thickness of the underline decoration
} wxPdfCjkFontDesc;

#endif

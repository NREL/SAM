///////////////////////////////////////////////////////////////////////////////
// Name:        pdfcorefontdata.h
// Purpose:     Definition of core font data structures
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-02-25
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfcorefontdata.h Definition of core font data structures

#ifndef _PDF_CORE_FONT_DATA_H_
#define _PDF_CORE_FONT_DATA_H_

/// Class representing a kerning pair (For internal use only)
class wxPdfKernPairDesc
{
public:
  wxUint32 unicode1; ///< first unicode character in kerning pair
  wxUint32 unicode2; ///< second unicode character in kerning pair
  int      kerning;  ///< kerning measure
};

/// Structure describing core fonts (For internal use only)
typedef struct _wxPdfCoreFontDesc
{
  const wxChar*      family;             ///< font family
  const wxChar*      alias;              ///< font family alias
  const wxChar*      name;               ///< font name
  short*             cwArray;            ///< array of character widths
  const wxPdfKernPairDesc* kpArray;      ///< array of kerning pairs
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
} wxPdfCoreFontDesc;

#endif

///////////////////////////////////////////////////////////////////////////////
// Name:        pdfproperties.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2006-07-13
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfproperties.h Interface of the several wxPdfDocument property classes

#ifndef _PDF_PROPERTIES_H_
#define _PDF_PROPERTIES_H_

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

/// Border options
#define wxPDF_BORDER_NONE    0x0000
#define wxPDF_BORDER_LEFT    0x0001
#define wxPDF_BORDER_RIGHT   0x0002
#define wxPDF_BORDER_TOP     0x0004
#define wxPDF_BORDER_BOTTOM  0x0008
#define wxPDF_BORDER_FRAME   0x000F

/// Corner options
#define wxPDF_CORNER_NONE          0x0000
#define wxPDF_CORNER_TOP_LEFT      0x0001
#define wxPDF_CORNER_TOP_RIGHT     0x0002
#define wxPDF_CORNER_BOTTOM_LEFT   0x0004
#define wxPDF_CORNER_BOTTOM_RIGHT  0x0008
#define wxPDF_CORNER_ALL           0x000F

/// Style options
#define wxPDF_STYLE_NOOP      0x0000
#define wxPDF_STYLE_DRAW      0x0001
#define wxPDF_STYLE_FILL      0x0002
#define wxPDF_STYLE_FILLDRAW  0x0003
#define wxPDF_STYLE_DRAWCLOSE 0x0004
#define wxPDF_STYLE_MASK      0x0007

/// Text render mode
enum wxPdfTextRenderMode
{
  wxPDF_TEXT_RENDER_FILL       = 0,
  wxPDF_TEXT_RENDER_STROKE     = 1,
  wxPDF_TEXT_RENDER_FILLSTROKE = 2,
  wxPDF_TEXT_RENDER_INVISIBLE  = 3
};

/// Font style flags
enum
{
  wxPDF_FONTSTYLE_REGULAR    = 0,
  wxPDF_FONTSTYLE_ITALIC     = 1 << 0,
  wxPDF_FONTSTYLE_BOLD       = 1 << 1,
  wxPDF_FONTSTYLE_BOLDITALIC = wxPDF_FONTSTYLE_ITALIC |
                               wxPDF_FONTSTYLE_BOLD,
  wxPDF_FONTSTYLE_UNDERLINE  = 1 << 2,
  wxPDF_FONTSTYLE_OVERLINE   = 1 << 3,
  wxPDF_FONTSTYLE_STRIKEOUT  = 1 << 4,

  wxPDF_FONTSTYLE_DECORATION_MASK = wxPDF_FONTSTYLE_UNDERLINE |
                                    wxPDF_FONTSTYLE_OVERLINE  |
                                    wxPDF_FONTSTYLE_STRIKEOUT,  ///< Mask of decoration styles
  wxPDF_FONTSTYLE_MASK = wxPDF_FONTSTYLE_REGULAR   |
                         wxPDF_FONTSTYLE_ITALIC    |
                         wxPDF_FONTSTYLE_BOLD      |
                         wxPDF_FONTSTYLE_UNDERLINE |
                         wxPDF_FONTSTYLE_OVERLINE  |
                         wxPDF_FONTSTYLE_STRIKEOUT
};

/// Permission options
#define wxPDF_PERMISSION_NONE     0x0000  ///< Allow nothing
#define wxPDF_PERMISSION_PRINT    0x0004  ///< Allow printing
#define wxPDF_PERMISSION_MODIFY   0x0008  ///< Allow modifying
#define wxPDF_PERMISSION_COPY     0x0010  ///< Allow text copying
#define wxPDF_PERMISSION_ANNOT    0x0020  ///< Allow annotations
#define wxPDF_PERMISSION_FILLFORM 0x0100  ///< Allow filling forms
#define wxPDF_PERMISSION_EXTRACT  0x0200  ///< Allow extract text and/or garphics
#define wxPDF_PERMISSION_ASSEMBLE 0x0400  ///< Allow assemble document
#define wxPDF_PERMISSION_HLPRINT  0x0800  ///< Allow high resolution print
#define wxPDF_PERMISSION_ALL      0x0F3C  ///< Allow anything

/// Encryption methods
enum wxPdfEncryptionMethod
{
  wxPDF_ENCRYPTION_RC4V1,
  wxPDF_ENCRYPTION_RC4V2,
  wxPDF_ENCRYPTION_AESV2
};

/// Page box types
enum wxPdfPageBox
{
  wxPDF_PAGEBOX_MEDIABOX,
  wxPDF_PAGEBOX_CROPBOX,
  wxPDF_PAGEBOX_BLEEDBOX,
  wxPDF_PAGEBOX_TRIMBOX,
  wxPDF_PAGEBOX_ARTBOX
};

/// Form field border styles
enum wxPdfBorderStyle
{
  wxPDF_BORDER_SOLID,
  wxPDF_BORDER_DASHED,
  wxPDF_BORDER_BEVELED,
  wxPDF_BORDER_INSET,
  wxPDF_BORDER_UNDERLINE
};

/// Alignment options
enum wxPdfAlignment
{
  wxPDF_ALIGN_LEFT,
  wxPDF_ALIGN_CENTER,
  wxPDF_ALIGN_RIGHT,
  wxPDF_ALIGN_JUSTIFY,
  wxPDF_ALIGN_TOP    = wxPDF_ALIGN_LEFT,
  wxPDF_ALIGN_MIDDLE = wxPDF_ALIGN_CENTER,
  wxPDF_ALIGN_BOTTOM = wxPDF_ALIGN_RIGHT
};

/// Zoom options
enum wxPdfZoom
{
  wxPDF_ZOOM_FULLPAGE,
  wxPDF_ZOOM_FULLWIDTH,
  wxPDF_ZOOM_REAL,
  wxPDF_ZOOM_DEFAULT,
  wxPDF_ZOOM_FACTOR
};

/// Layout options
enum wxPdfLayout
{
  wxPDF_LAYOUT_CONTINUOUS,
  wxPDF_LAYOUT_SINGLE,
  wxPDF_LAYOUT_TWO,
  wxPDF_LAYOUT_DEFAULT
};

/// Viewer preferences
#define wxPDF_VIEWER_HIDETOOLBAR     0x0001
#define wxPDF_VIEWER_HIDEMENUBAR     0x0002
#define wxPDF_VIEWER_HIDEWINDOWUI    0x0004
#define wxPDF_VIEWER_FITWINDOW       0x0008
#define wxPDF_VIEWER_CENTERWINDOW    0x0010
#define wxPDF_VIEWER_DISPLAYDOCTITLE 0x0020

/// Marker symbols
enum wxPdfMarker
{
  wxPDF_MARKER_CIRCLE,
  wxPDF_MARKER_SQUARE,
  wxPDF_MARKER_TRIANGLE_UP,
  wxPDF_MARKER_TRIANGLE_DOWN,
  wxPDF_MARKER_TRIANGLE_LEFT,
  wxPDF_MARKER_TRIANGLE_RIGHT,
  wxPDF_MARKER_DIAMOND,
  wxPDF_MARKER_PENTAGON_UP,
  wxPDF_MARKER_PENTAGON_DOWN,
  wxPDF_MARKER_PENTAGON_LEFT,
  wxPDF_MARKER_PENTAGON_RIGHT,
  wxPDF_MARKER_STAR,
  wxPDF_MARKER_STAR4,
  wxPDF_MARKER_PLUS,
  wxPDF_MARKER_CROSS,
  wxPDF_MARKER_SUN,
  wxPDF_MARKER_BOWTIE_HORIZONTAL,
  wxPDF_MARKER_BOWTIE_VERTICAL,
  wxPDF_MARKER_ASTERISK,
  wxPDF_MARKER_LAST  // Marks the last available marker symbol; do not use!
};

/// Linear gradient types
enum wxPdfLinearGradientType
{
  wxPDF_LINEAR_GRADIENT_HORIZONTAL,
  wxPDF_LINEAR_GRADIENT_VERTICAL,
  wxPDF_LINEAR_GRADIENT_MIDHORIZONTAL,
  wxPDF_LINEAR_GRADIENT_MIDVERTICAL,
  wxPDF_LINEAR_GRADIENT_REFLECTION_LEFT,
  wxPDF_LINEAR_GRADIENT_REFLECTION_RIGHT,
  wxPDF_LINEAR_GRADIENT_REFLECTION_TOP,
  wxPDF_LINEAR_GRADIENT_REFLECTION_BOTTOM
};

enum wxPdfBlendMode
{
  wxPDF_BLENDMODE_NORMAL,
  wxPDF_BLENDMODE_MULTIPLY, 
  wxPDF_BLENDMODE_SCREEN, 
  wxPDF_BLENDMODE_OVERLAY, 
  wxPDF_BLENDMODE_DARKEN, 
  wxPDF_BLENDMODE_LIGHTEN, 
  wxPDF_BLENDMODE_COLORDODGE, 
  wxPDF_BLENDMODE_COLORBURN,
  wxPDF_BLENDMODE_HARDLIGHT,
  wxPDF_BLENDMODE_SOFTLIGHT,
  wxPDF_BLENDMODE_DIFFERENCE, 
  wxPDF_BLENDMODE_EXCLUSION, 
  wxPDF_BLENDMODE_HUE, 
  wxPDF_BLENDMODE_SATURATION, 
  wxPDF_BLENDMODE_COLOR, 
  wxPDF_BLENDMODE_LUMINOSITY
};

enum wxPdfShapedTextMode
{
  wxPDF_SHAPEDTEXTMODE_ONETIME,
  wxPDF_SHAPEDTEXTMODE_STRETCHTOFIT,
  wxPDF_SHAPEDTEXTMODE_REPEAT
};

/// PDF/X conformance
enum wxPdfXConformanceType
{
  wxPDF_PDFXNONE,
  wxPDF_PDFX1A2001,
  wxPDF_PDFX32002,
  wxPDF_PDFA1A,
  wxPDF_PDFA1B
};

/// Run direction of text
enum wxPdfRunDirection
{
  wxPDF_RUN_DIRECTION_DEFAULT, // default run direction
  wxPDF_RUN_DIRECTION_NO_BIDI, // do not use bidirectional reordering
  wxPDF_RUN_DIRECTION_LTR,     // bidirectional reordering with left-to-right preferential run direction
  wxPDF_RUN_DIRECTION_RTL      // bidirectional reordering with right-to-left preferential run direction
};  

#endif

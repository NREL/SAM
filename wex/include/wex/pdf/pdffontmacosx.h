///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontmacosx.h
// Purpose:     Definition of Mac OS X specific font data handling
// Author:      Ulrich Telle
// Modified by:
// Created:     2010-09-30
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontmacosx.h Definition of Mac OS X specific font data handling

#ifndef _PDF_FONT_MACOSX_H_
#define _PDF_FONT_MACOSX_H_

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

#if defined(__WXMAC__)

#if wxCHECK_VERSION(2,9,0)
  // wxWidgets 2.9.x or higher
  #include <wx/osx/private.h>
  #define wxPDFMACOSX_HAS_CORE_TEXT wxOSX_USE_CORE_TEXT
  #define wxPDFMACOSX_HAS_ATSU_TEXT wxOSX_USE_ATSU_TEXT
#else // wxWidgets 2.8.x
  #include <wx/mac/private.h>
  #ifndef __LP64__
    #define wxPDFMACOSX_HAS_CORE_TEXT 0
    #define wxPDFMACOSX_HAS_ATSU_TEXT 1
  #else
    #define wxPDFMACOSX_HAS_CORE_TEXT 1
    #define wxPDFMACOSX_HAS_ATSU_TEXT 0
  #endif
#endif

#endif

#endif

///////////////////////////////////////////////////////////////////////////////
// Name:        pdfdc.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2010-11-28
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfdc.h Interface of the wxPdfDC class

#ifndef _PDF_DC_H_
#define _PDF_DC_H_

#include <wx/cmndata.h>
#include <wx/dc.h>

#include <stack>

#include "wex/pdf/pdfdocument.h"
#include "wex/pdf/pdffont.h"

/// Enumeration of map mode styles
enum wxPdfMapModeStyle
{
  wxPDF_MAPMODESTYLE_STANDARD = 1,
  wxPDF_MAPMODESTYLE_MSW,
  wxPDF_MAPMODESTYLE_GTK,
  wxPDF_MAPMODESTYLE_MAC,
  wxPDF_MAPMODESTYLE_PDF
};

#if wxCHECK_VERSION(2,9,0)
// Interface of wxPdfDC for wxWidgets 2.9.x (and above)
#include "wex/pdf/pdfdc29.h"
#else
// Interface of wxPdfDC for wxWidgets 2.8.x
#include "wex/pdf/pdfdc28.h"
#endif

#endif

///////////////////////////////////////////////////////////////////////////////
// Name:        pdfarraydouble.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-05-25
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfarraydouble.h Definition of array of type double

#ifndef _PDF_ARRAY_DOUBLE_H_
#define _PDF_ARRAY_DOUBLE_H_

// wxWidgets headers
#include <wx/dynarray.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

#if wxCHECK_VERSION(2, 7, 0)
// Just map wxPdfArrayDouble to wxArrayDouble, for wxWidgets version 2.7.0 or later
#define wxPdfArrayDouble wxArrayDouble
#else
// Unfortunately double arrays are not available in wxWidgets until version 2.7.0
/// Class representing double arrays
WX_DEFINE_USER_EXPORTED_ARRAY_DOUBLE(double, wxPdfArrayDouble, class WXDLLIMPEXP_PDFDOC);
#endif

#endif

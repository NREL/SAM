///////////////////////////////////////////////////////////////////////////////
// Name:        pdfarraytypes.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-08
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfarraytypes.h Definition of array types

#ifndef _PDF_ARRAY_TYPES_H_
#define _PDF_ARRAY_TYPES_H_

// wxWidgets headers
#include <wx/dynarray.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

/// Unsorted array types
WX_DEFINE_USER_EXPORTED_ARRAY_SHORT(wxUint16, wxPdfArrayUint16, class WXDLLIMPEXP_PDFDOC);
WX_DEFINE_USER_EXPORTED_ARRAY_LONG(wxUint32, wxPdfArrayUint32, class WXDLLIMPEXP_PDFDOC);

/// Sorted array types
WX_DEFINE_SORTED_USER_EXPORTED_ARRAY_INT(int, wxPdfSortedArrayInt, WXDLLIMPEXP_PDFDOC);

#endif

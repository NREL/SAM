///////////////////////////////////////////////////////////////////////////////
// Name:        wxmemdbg.h
// Purpose:     Debugging of wxWidgets memory allocations
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-01-28
//                            
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file wxmemdbg.h Defines for debugging memory allocations

#ifndef _WX_MEMDBG_H_
#define _WX_MEMDBG_H_

#if defined(__WXMSW__) && defined(__VISUALC__)

#ifdef _DEBUG
#include <crtdbg.h>
#define DEBUG_NEW new(_NORMAL_BLOCK ,__FILE__, __LINE__)
#else
#define DEBUG_NEW new
#endif

#ifdef _DEBUG
#define new DEBUG_NEW
#endif 

#endif

#endif // _WX_MEMDBG_H_

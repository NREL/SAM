///////////////////////////////////////////////////////////////////////////////
// Name:        pdfpattern.cpp
// Purpose:     Implementation of pattern handling
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-06-18
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfpattern.cpp Implementation of the pattern handling

// For compilers that support precompilation, includes <wx/wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

//#include "wex/pdf/pdfcolour.h"
#include "wex/pdf/pdfpattern.h"
//#include "wex/pdf/pdfutility.h"

wxPdfPattern::wxPdfPattern(int index, double width, double height)
  : m_objIndex(0), m_index(index), m_width(width), m_height(height)
{
}

wxPdfPattern::wxPdfPattern(const wxPdfPattern& pattern)
{
  m_objIndex = pattern.m_objIndex;
  m_index    = pattern.m_index; 
  m_width    = pattern.m_width;
  m_height   = pattern.m_height;
  m_image    = pattern.m_image;
}

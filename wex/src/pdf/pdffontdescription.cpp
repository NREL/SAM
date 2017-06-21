///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontdescription.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-10
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontdescription.cpp Implementation of wxPdfFontDescription class

// For compilers that support precompilation, includes <wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

// includes

#include "wex/pdf/pdffontdescription.h"

#include "wxmemdbg.h"

wxPdfFontDescription::wxPdfFontDescription()
  : m_ascent(0), m_descent(0), m_capHeight(0), m_flags(0),
    m_fontBBox(wxEmptyString), m_italicAngle(0), m_stemV(0),
    m_missingWidth(0), m_xHeight(0),
    m_underlinePosition(-100), m_underlineThickness(50),
    m_hheaAscender(0), m_hheaDescender(0), m_hheaLineGap(0),
    m_os2sTypoAscender(0), m_os2sTypoDescender(0), m_os2sTypoLineGap(0),
    m_os2usWinAscent(0), m_os2usWinDescent(0)
{
}

wxPdfFontDescription::wxPdfFontDescription (int ascent, int descent, int capHeight, int flags,
                                            const wxString& fontBBox, int italicAngle, int stemV,
                                            int missingWidth, int xHeight,
                                            int underlinePosition, int underlineThickness,
                                            int hheaAscender, int hheaDescender, int hheaLineGap,
                                            int os2sTypoAscender, int os2sTypoDescender, int os2sTypoLineGap,
                                            int os2usWinAscent, int os2usWinDescent)
  : m_ascent(ascent), m_descent(descent), m_capHeight(capHeight), m_flags(flags),
    m_fontBBox(fontBBox), m_italicAngle(italicAngle), m_stemV(stemV),
    m_missingWidth(missingWidth), m_xHeight(xHeight),
    m_underlinePosition(underlinePosition), m_underlineThickness(underlineThickness),
    m_hheaAscender(hheaAscender), m_hheaDescender(hheaDescender), m_hheaLineGap(hheaLineGap),
    m_os2sTypoAscender(os2sTypoAscender), m_os2sTypoDescender(os2sTypoDescender), m_os2sTypoLineGap(os2sTypoLineGap),
    m_os2usWinAscent(os2usWinAscent), m_os2usWinDescent(os2usWinDescent)
{
}

wxPdfFontDescription::wxPdfFontDescription(const wxPdfFontDescription& desc)
{
  m_ascent             = desc.m_ascent;
  m_descent            = desc.m_descent;
  m_capHeight          = desc.m_capHeight;
  m_flags              = desc.m_flags;
  m_fontBBox           = desc.m_fontBBox;
  m_italicAngle        = desc.m_italicAngle;
  m_stemV              = desc.m_stemV;
  m_missingWidth       = desc.m_missingWidth;
  m_xHeight            = desc.m_xHeight;
  m_underlinePosition  = desc.m_underlinePosition;
  m_underlineThickness = desc.m_underlineThickness;
  m_hheaAscender       = desc.m_hheaAscender;
  m_hheaDescender      = desc.m_hheaDescender;
  m_hheaLineGap        = desc.m_hheaLineGap;
  m_os2sTypoAscender   = desc.m_os2sTypoAscender;
  m_os2sTypoDescender  = desc.m_os2sTypoDescender;
  m_os2sTypoLineGap    = desc.m_os2sTypoLineGap;
  m_os2usWinAscent     = desc.m_os2usWinAscent;
  m_os2usWinDescent    = desc.m_os2usWinDescent;
}

wxPdfFontDescription::~wxPdfFontDescription()
{
}

void
wxPdfFontDescription::SetOpenTypeMetrics(int hheaAscender, int hheaDescender, int hheaLineGap,
                                         int os2sTypoAscender, int os2sTypoDescender, int os2sTypoLineGap,
                                         int os2usWinAscent, int os2usWinDescent)
{
  if (hheaAscender)
  {
    m_hheaAscender = hheaAscender;
  }

  if (hheaDescender)
  {
    m_hheaDescender = hheaDescender;
  }

  if (hheaLineGap)
  {
    m_hheaLineGap = hheaLineGap;
  }

  if (os2sTypoAscender)
  {
    m_os2sTypoAscender = os2sTypoAscender;
  }

  if (os2sTypoDescender)
  {
    m_os2sTypoDescender = os2sTypoDescender;
  }

  if (os2sTypoLineGap)
  {
    m_os2sTypoLineGap = os2sTypoLineGap;
  }

  if (os2usWinAscent)
  {
    m_os2usWinAscent = os2usWinAscent;
  }

  if (os2usWinDescent)
  {
    m_os2usWinDescent = os2usWinDescent;
  }
}

void
wxPdfFontDescription::GetOpenTypeMetrics(int* hheaAscender, int* hheaDescender, int* hheaLineGap,
                                         int* os2sTypoAscender, int* os2sTypoDescender, int* os2sTypoLineGap,
                                         int* os2usWinAscent, int* os2usWinDescent)
{
  if (hheaAscender)
  {
    *hheaAscender = m_hheaAscender;
  }

  if (hheaDescender)
  {
    *hheaDescender = m_hheaDescender;
  }

  if (hheaLineGap)
  {
    *hheaLineGap = m_hheaLineGap;
  }

  if (os2sTypoAscender)
  {
    *os2sTypoAscender = m_os2sTypoAscender;
  }

  if (os2sTypoDescender)
  {
    *os2sTypoDescender = m_os2sTypoDescender;
  }

  if (os2sTypoLineGap)
  {
    *os2sTypoLineGap = m_os2sTypoLineGap;
  }

  if (os2usWinAscent)
  {
    *os2usWinAscent = m_os2usWinAscent;
  }

  if (os2usWinDescent)
  {
    *os2usWinDescent = m_os2usWinDescent;
  }
}

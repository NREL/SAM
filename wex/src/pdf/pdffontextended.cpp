///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontextended.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-10
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontextended.cpp Implementation of the wxPdfFontExtended class

// For compilers that support precompilation, includes <wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

// includes

#include "wex/pdf/pdfarraytypes.h"
#include "wex/pdf/pdfencoding.h"
#include "wex/pdf/pdffont.h"
#include "wex/pdf/pdffontextended.h"
#include "wex/pdf/pdffontdata.h"
#include "wex/pdf/pdffontdatatype1.h"

#include "wxmemdbg.h"

wxPdfFontExtended::wxPdfFontExtended()
  : m_embed(false), m_subset(false), 
    m_fontData(NULL), m_encoding(NULL)
{
}

wxPdfFontExtended::wxPdfFontExtended(const wxPdfFont& font)
  : m_embed(font.m_embed), m_subset(font.m_subset), 
    m_fontData(font.m_fontData)
{
  if (m_fontData != NULL)
  {
    m_fontData->IncrementRefCount();
  }
  m_encoding = font.m_encoding;
}

wxPdfFontExtended::wxPdfFontExtended(const wxPdfFontExtended& font)
  : m_embed(font.m_embed), m_subset(font.m_subset), 
    m_fontData(font.m_fontData)
{ 
  if (m_fontData != NULL)
  {
    m_fontData->IncrementRefCount();
  }
  m_encoding = font.m_encoding;
}

wxPdfFontExtended::~wxPdfFontExtended()
{
  if (m_fontData != NULL && m_fontData->DecrementRefCount() == 0)
  {
    delete m_fontData;
  }
}

wxPdfFontExtended& 
wxPdfFontExtended::operator=(const wxPdfFontExtended& font)
{ // DO NOT CHANGE THE ORDER OF THESE STATEMENTS!
  // (This order properly handles self-assignment)
  // (This order also properly handles recursion, e.g., if a Fred contains FredPtrs)
  wxPdfFontData* const prevFontData = m_fontData;
  m_embed = font.m_embed;
  m_subset = font.m_subset;
//  m_fontStyle = font.m_fontStyle;
  m_fontData = font.m_fontData;
  if (m_fontData != NULL)
  {
    m_fontData->IncrementRefCount();
  }
  if (prevFontData != NULL && prevFontData->DecrementRefCount() == 0)
  {
    delete prevFontData;
  }
  m_encoding = font.m_encoding;
  return *this;
}

bool
wxPdfFontExtended::IsValid() const
{
  return (m_fontData != NULL);
}

wxString
wxPdfFontExtended::GetType() const
{
  return (m_fontData != NULL) ? m_fontData->GetType() : wxString(wxT(""));
}

wxString
wxPdfFontExtended::GetFamily() const
{
  return (m_fontData != NULL) ? m_fontData->GetFamily() : wxString(wxT(""));
}

wxString
wxPdfFontExtended::GetName() const
{
  return (m_fontData != NULL) ? m_fontData->GetName() : wxString(wxT(""));
}

int
wxPdfFontExtended::GetStyle() const
{
  return (m_fontData != NULL) ? m_fontData->GetStyle() : wxPDF_FONTSTYLE_REGULAR;
}

int
wxPdfFontExtended::GetUnderlinePosition() const
{
  return (m_fontData != NULL) ? m_fontData->GetUnderlinePosition() : 0;
}

int
wxPdfFontExtended::GetUnderlineThickness() const
{
  return (m_fontData != NULL) ? m_fontData->GetUnderlineThickness() : 0;
}

int
wxPdfFontExtended::GetBBoxTopPosition() const
{
  return (m_fontData != NULL) ? m_fontData->GetBBoxTopPosition() : 0;
}

wxString
wxPdfFontExtended::GetEncoding() const
{
  wxString encoding = wxEmptyString;
  if (m_encoding != NULL)
  {
    encoding = m_encoding->GetEncodingName();
  }
  else if (m_fontData != NULL)
  {
    encoding = m_fontData->GetEncoding();
  }
  return encoding;
}

wxString
wxPdfFontExtended::GetBaseEncoding() const
{
  wxString baseEncoding = wxEmptyString;
  if (m_encoding != NULL)
  {
    baseEncoding = m_encoding->GetBaseEncodingName();
  }
  else
  {
    if (HasDiffs())
    {
      baseEncoding = wxString(wxT("WinAnsiEncoding"));
    }
  }
  return baseEncoding;
}

bool
wxPdfFontExtended::HasDiffs() const
{
  bool hasDiffs = false;
  if (m_fontData != NULL)
  {
    if (m_fontData->GetType().IsSameAs(wxT("Type1")) && m_encoding != NULL)
    {
      // TODO: Check whether encoding is different from base encoding
      hasDiffs = true;
    }
    else
    {
      hasDiffs = m_fontData->HasDiffs();
    }
  }
  return hasDiffs;
}

wxString
wxPdfFontExtended::GetDiffs() const
{
  wxString diffs = wxEmptyString;
  if (m_fontData != NULL)
  {
    if (m_fontData->GetType().IsSameAs(wxT("Type1")) && m_encoding != NULL)
    {
      diffs = m_encoding->GetDifferences();
    }
    else
    {
      diffs = m_fontData->GetDiffs();
    }
  }
  return diffs;
}

size_t
wxPdfFontExtended::GetSize1() const
{
  return (m_fontData != NULL) ? m_fontData->GetSize1() : 0;
}

bool
wxPdfFontExtended::HasSize2() const
{
  return (m_fontData != NULL) ? m_fontData->HasSize2() : 0;
}

size_t
wxPdfFontExtended::GetSize2() const
{
  return (m_fontData != NULL) ? m_fontData->GetSize2() : 0;
}

wxString
wxPdfFontExtended::GetCMap() const
{
  return (m_fontData != NULL) ? m_fontData->GetCMap() : wxString(wxT(""));
}

wxString
wxPdfFontExtended::GetOrdering() const
{
  return (m_fontData != NULL) ? m_fontData->GetOrdering() : wxString(wxT(""));
}

wxString
wxPdfFontExtended::GetSupplement() const
{
  return (m_fontData != NULL) ? m_fontData->GetSupplement() : wxString(wxT(""));
}

wxString
wxPdfFontExtended::GetWidthsAsString(bool subset, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs) const
{
  wxString widths = wxEmptyString;
  if (m_fontData != NULL)
  {
#if wxUSE_UNICODE
    if (m_fontData->GetType().IsSameAs(wxT("Type1")) && m_encoding != NULL)
    {
      widths = ((wxPdfFontDataType1*) m_fontData)->GetWidthsAsString(m_encoding->GetGlyphNames(), subset, usedGlyphs, subsetGlyphs);
    }
    else
#endif
    {
      widths = m_fontData->GetWidthsAsString(subset, usedGlyphs, subsetGlyphs);
    }
  }
  return widths;
}
  
double
wxPdfFontExtended::GetStringWidth(const wxString& s, bool withKerning)
{
  double width = 0;
  if (m_fontData != NULL)
  {
    width = m_fontData->GetStringWidth(s, m_encoding, withKerning);
  }
  return width;
}

wxArrayInt
wxPdfFontExtended::GetKerningWidthArray(const wxString& s) const
{
  return m_fontData->GetKerningWidthArray(s);
}

bool
wxPdfFontExtended::CanShow(const wxString& s) const
{
  bool canShow = false;
  if (m_fontData != NULL)
  {
    canShow = m_fontData->CanShow(s, m_encoding);
  }
  return canShow;
}

wxString
wxPdfFontExtended::ConvertCID2GID(const wxString& s, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs)
{
  wxString sConv = wxEmptyString;
  if (m_fontData != NULL)
  {
    sConv = m_fontData->ConvertCID2GID(s, m_encoding, usedGlyphs, subsetGlyphs);
  }
  return sConv;
}

wxString
wxPdfFontExtended::ConvertGlyph(wxUint32 glyph, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs)
{
  wxString sConv = wxEmptyString;
  if (m_fontData != NULL)
  {
    sConv = m_fontData->ConvertGlyph(glyph, m_encoding, usedGlyphs, subsetGlyphs);
  }
  return sConv;
}

bool
wxPdfFontExtended::IsEmbedded() const
{
  return m_embed;
}

bool
wxPdfFontExtended::SupportsSubset() const
{
  return (m_fontData != NULL) ? m_fontData->SubsetSupported() : false;
}

size_t
wxPdfFontExtended::WriteFontData(wxOutputStream* fontData, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs)
{
  return (m_fontData != NULL) ? m_fontData->WriteFontData(fontData, usedGlyphs, subsetGlyphs) : 0;
}

size_t
wxPdfFontExtended::WriteUnicodeMap(wxOutputStream* mapData, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs)
{
  size_t maplen = 0;
  if (m_fontData != NULL)
  {
    maplen = m_fontData->WriteUnicodeMap(mapData, m_encoding, usedGlyphs, subsetGlyphs);
  }
  return maplen;
}

static wxPdfFontDescription dummyDescription;

const wxPdfFontDescription&
wxPdfFontExtended::GetDescription() const
{
  return (m_fontData != NULL) ? m_fontData->GetDescription() : dummyDescription;
}

wxPdfFont 
wxPdfFontExtended::GetUserFont() const
{
  wxPdfFont userFont;
  userFont.m_embed = m_embed;
  userFont.m_subset = m_subset;
  userFont.m_fontData = m_fontData;
  if (m_fontData != NULL)
  {
    m_fontData->IncrementRefCount();
  }
  userFont.m_encoding = m_encoding;
  return userFont;
}

bool
wxPdfFontExtended::HasEncodingMap() const
{ 
#if wxUSE_UNICODE
  return (m_encoding != NULL);
#else
  return false;
#endif
}

#if wxUSE_UNICODE
wxMBConv*
wxPdfFontExtended::GetEncodingConv() const
{
  wxMBConv* conv = NULL;
  if (m_fontData != NULL)
  {
    if (m_fontData->GetType().IsSameAs(wxT("Type1")) && m_encoding != NULL)
    {
      conv = &wxConvISO8859_1;
    }
    else
    {
      conv = m_fontData->GetEncodingConv();
    }
  }
  return conv;
}
#endif

bool
wxPdfFontExtended::HasVoltData() const
{
  return m_fontData->HasVoltData();
}

wxString
wxPdfFontExtended::ApplyVoltData(const wxString& txt) const
{
  return (m_fontData->HasVoltData()) ? m_fontData->ApplyVoltData(txt) : txt;
}

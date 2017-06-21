///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontdetails.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-07
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontdetails.cpp Implementation of the wxPdfFontDetails class

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
#include "wex/pdf/pdffont.h"
#include "wex/pdf/pdffontdetails.h"

#include "wxmemdbg.h"

static int
CompareInts(int n1, int n2)
{
  return n1 - n2;
}

wxPdfFontDetails::wxPdfFontDetails(int index, const wxPdfFont& font)
  : m_index(index), m_n(0), m_fn(0), m_ndiff(0), m_font(font)
{
  if (m_font.SubsetRequested())
  {
    m_usedGlyphs = new wxPdfSortedArrayInt(CompareInts);
    m_usedGlyphs->Add(0);
    if (m_font.GetType().IsSameAs(wxT("TrueTypeUnicode")) ||
        m_font.GetType().IsSameAs(wxT("OpenTypeUnicode")))
    {
      m_subsetGlyphs = new wxPdfChar2GlyphMap();
      (*m_subsetGlyphs)[0] = 0;
    }
    else
    {
      m_subsetGlyphs = NULL;
    }
  }
  else
  {
    m_usedGlyphs = NULL;
    m_subsetGlyphs = NULL;
  }
}
  
wxPdfFontDetails::~wxPdfFontDetails()
{
  if (m_usedGlyphs != NULL)
  {
    delete m_usedGlyphs;
  }
  if (m_subsetGlyphs != NULL)
  {
    delete m_subsetGlyphs;
  }
}

wxPdfFontDetails::wxPdfFontDetails(const wxPdfFontDetails&)
{
}

wxPdfFontDetails&
wxPdfFontDetails::operator=(const wxPdfFontDetails&)
{
  return *this;
}

wxString
wxPdfFontDetails::GetType() const
{
  return m_font.GetType();
}

wxString
wxPdfFontDetails::GetName() const
{
  wxString name = m_font.GetName();
  if (m_font.SubsetRequested())
  {
    name.Prepend(CreateSubsetPrefix());
  }
  return name;
}

wxString
wxPdfFontDetails::GetOriginalName() const
{
  return m_font.GetName();
}

wxString
wxPdfFontDetails::GetFontFamily() const
{
  return m_font.GetFamily();
}

bool
wxPdfFontDetails::HasDiffs() const
{
  return m_font.HasDiffs();
}

wxString
wxPdfFontDetails::GetDiffs() const
{
  return m_font.GetDiffs();
}

wxString
wxPdfFontDetails::GetBaseEncoding() const
{
  return m_font.GetBaseEncoding();
}

wxPdfFontExtended
wxPdfFontDetails::GetFont() const
{
  return m_font;
}

wxPdfFont
wxPdfFontDetails::GetUserFont() const
{
  return m_font.GetUserFont();
}

double
wxPdfFontDetails::GetStringWidth(const wxString& s, bool withKerning)
{
  return m_font.GetStringWidth(s, withKerning);
}

wxArrayInt
wxPdfFontDetails::GetKerningWidthArray(const wxString& s) const
{
  return m_font.GetKerningWidthArray(s);
}

const wxPdfFontDescription&
wxPdfFontDetails::GetDescription() const
{
  return m_font.GetDescription();
}

wxString
wxPdfFontDetails::CreateSubsetPrefix() const
{
  wxString prefix = wxT("WXP");
  int k;
  int code = m_index;
  for (k = 0; k < 3; k++)
  {
#if wxCHECK_VERSION(2,9,0)
    prefix += wxUniChar(wxT('A' + (code % 26)));
#else
    prefix += wxChar(wxT('A' + (code % 26)));
#endif
    code /= 26;
  }
  prefix += wxT("+");
  return prefix;
}

wxString
wxPdfFontDetails::ConvertCID2GID(const wxString& s)
{
  return m_font.ConvertCID2GID(s, m_usedGlyphs, m_subsetGlyphs);
}

wxString
wxPdfFontDetails::ConvertGlyph(wxUint32 glyph)
{
  return m_font.ConvertGlyph(glyph, m_usedGlyphs, m_subsetGlyphs);
}

wxString
wxPdfFontDetails::GetWidthsAsString() const
{
  return m_font.GetWidthsAsString(m_font.IsEmbedded(), m_usedGlyphs, m_subsetGlyphs);
}

size_t
wxPdfFontDetails::WriteFontData(wxOutputStream* fontData)
{
  return m_font.WriteFontData(fontData, m_usedGlyphs, m_subsetGlyphs);
}

size_t
wxPdfFontDetails::WriteUnicodeMap(wxOutputStream* mapData)
{
  return m_font.WriteUnicodeMap(mapData, m_usedGlyphs, m_subsetGlyphs);
}

#if wxUSE_UNICODE

wxMBConv*
wxPdfFontDetails::GetEncodingConv() const
{
  return m_font.GetEncodingConv();
}

#endif

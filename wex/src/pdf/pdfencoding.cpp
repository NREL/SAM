///////////////////////////////////////////////////////////////////////////////
// Name:        pdfencoding.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-05-30
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfencoding.cpp Implementation of encoding class

// For compilers that support precompilation, includes <wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

// includes
#include <wx/string.h>

#include "wex/pdf/pdfencoding.h"

#include "wxmemdbg.h"

#include "pdfglyphnames.inc"
#include "pdfcmapdata.inc"

bool
wxPdfEncoding::GlyphName2Unicode(const wxString& glyphName, wxUint32& unicode)
{
  // TODO:
  // 1. Drop all characters after period if any
  // 2. Split into components using underscore as delimiter

  // 3. map each component
  //  a. for ZapfDingBats use appropriate list
  //  b. search in AGL
  //  c. uniXXXXYYYYZZZZ map to XXXX, YYYY, ZZZZ resp.
  //  d. uXXXXXX map to XXXXXX
  unicode = 0;
  bool found = false;
  int first = 0;
  int last = (int) gs_glyphName2UnicodeTableSize - 1;
  int mid;
  while (!found && first < last)
  {
    mid = (first + last) / 2;
    int relation = glyphName.Cmp(gs_glyphName2UnicodeTable[mid].glyphname);
    if (relation == 0)
    {
      found = true;
      unicode = gs_glyphName2UnicodeTable[mid].unicode;
    }
    else
    {
      if (relation < 0)
      {
        last = mid - 1;
      }
      else
      {
        first = mid + 1;
      }
    }
  }
  if (!found)
  {
    wxString glyphRest;
    unsigned long convUni = 0;
    if (glyphName.StartsWith(wxT("uni"), &glyphRest))
    {
      if (glyphRest.length() >= 4)
      {
        found = glyphRest.Mid(0,4).ToULong(&convUni, 16);
        if (found) unicode = convUni;
      }
    }
    else if (glyphName.StartsWith(wxT("u"), &glyphRest))
    {
      if (glyphRest.length() >= 6)
      {
        found = glyphRest.Mid(0,6).ToULong(&convUni, 16);
        if (found) unicode = convUni;
      }
    }
  }
  return found;
}

bool
wxPdfEncoding::Unicode2GlyphName(wxUint32 unicode, wxString& glyphName)
{
  glyphName = wxEmptyString;
  bool found = false;
  int first = 0;
  int last = (int) gs_unicode2GlyphNameTableSize - 1;
  int mid;
  while (!found && first <= last)
  {
    mid = (first + last) / 2;
    wxUint32 code = gs_unicode2GlyphNameTable[mid].unicode;
    if (unicode == code)
    {
      found = true;
      glyphName = gs_unicode2GlyphNameTable[mid].glyphname;
    }
    else
    {
      if (unicode < code)
      {
        last = mid - 1;
      }
      else
      {
        first = mid + 1;
      }
    }
  }
  return found;
}

wxArrayString
wxPdfEncoding::GetKnownEncodings()
{
  wxArrayString knownEncodings;
  size_t j = 0;
  while (gs_encodingData[j].m_encodingName != NULL)
  {
    knownEncodings.Add(gs_encodingData[j].m_encodingName);
    ++j;
  }
  return knownEncodings;
}

wxPdfEncoding::wxPdfEncoding()
{
  m_encoding = wxEmptyString;
  m_baseEncoding = wxEmptyString;
  m_specific = false;
  m_firstChar = 0;
  m_lastChar = 0;
  m_cmap.Alloc(256);
  m_cmapBase.Alloc(256);
  m_glyphNames.Alloc(256);
  m_cmap.Insert(0, 0, 256);
  m_cmapBase.Insert(0, 0, 256);
  m_glyphNames.Insert(wxString(wxT(".notdef")), 0, 256);
  m_encodingMap = NULL;
}

wxPdfEncoding::~wxPdfEncoding()
{
  if (m_encodingMap != NULL)
  {
    delete m_encodingMap;
  }
}

wxPdfEncoding::wxPdfEncoding(const wxPdfEncoding &encoding)
{
  m_encoding = encoding.m_encoding;
  m_baseEncoding = encoding.m_baseEncoding;
  m_specific = encoding.m_specific;
  m_firstChar = encoding.m_firstChar;
  m_lastChar = encoding.m_lastChar;
  m_cmap = encoding.m_cmap;
  m_cmapBase = encoding.m_cmapBase;
  m_glyphNames = encoding.m_glyphNames;
  m_encodingMap = NULL;
}

wxPdfEncoding&
wxPdfEncoding::operator=(const wxPdfEncoding& encoding)
{
  m_encoding = encoding.m_encoding;
  m_baseEncoding = encoding.m_baseEncoding;
  m_specific = encoding.m_specific;
  m_firstChar = encoding.m_firstChar;
  m_lastChar = encoding.m_lastChar;
  m_cmap = encoding.m_cmap;
  m_cmapBase = encoding.m_cmapBase;
  m_glyphNames = encoding.m_glyphNames;
  m_encodingMap = NULL;
  return *this;
}

bool
wxPdfEncoding::IsOk() const
{
  return !m_encoding.IsEmpty();
}

bool
wxPdfEncoding::SetEncoding(const wxString& encoding)
{
  wxString encodingName = encoding.Lower();
  bool isWinAnsi = encodingName.IsSameAs(wxT("winansi"));
  bool found = false;
  size_t j = 0;
  while (!found && gs_encodingData[j].m_encodingName != NULL)
  {
    if (encodingName.IsSameAs(gs_encodingData[j].m_encodingName))
    {
      found = true;
    }
    else
    {
      ++j;
    }
  }
  if (found)
  {
    bool hasFullSize = gs_encodingData[j].m_fullsize;
    m_encoding = encodingName;
    m_baseEncoding = gs_encodingData[j].m_baseEncoding;
    m_specific = false;
    m_firstChar = 32;
    m_lastChar = 255;
    wxUint32 unicodeChar;
    wxString glyphName;
    int k;
    for (k = 0; k < 128; ++k)
    {
      if (hasFullSize)
      {
        unicodeChar = (wxUint32) gs_encodingData[j].m_encodingMap[k];
        m_cmap[k] = unicodeChar;
        m_cmapBase[k] = unicodeChar;
      }
      else
      {
        unicodeChar = (wxUint32) k;
        m_cmap[k] = unicodeChar;
        m_cmapBase[k] = unicodeChar;
      }
      if (k >= m_firstChar && k != 127 && Unicode2GlyphName(unicodeChar, glyphName))
      {
        m_glyphNames[k] = glyphName;
      }
      else
      {
        if (k > 40 && isWinAnsi)
        {
          m_glyphNames[k] = wxString(wxT("bullet"));
        }
        else
        {
          m_glyphNames[k] = wxString(wxT(".notdef"));
        }
      }
    }
    for (k = 128; k < 256; ++k)
    {
      int delta = (hasFullSize) ? 0 : 128;
      unicodeChar = (wxUint32) gs_encodingData[j].m_encodingMap[k-delta];
      m_cmap[k] = unicodeChar;
      m_cmapBase[k] = (wxUint32) gs_encodingData[j].m_encodingBase[k-delta];
      if (Unicode2GlyphName(unicodeChar, glyphName))
      {
        m_glyphNames[k] = glyphName;
      }
      else
      {
        if (isWinAnsi)
        {
          m_glyphNames[k] = wxString(wxT("bullet"));
        }
        else
        {
          m_glyphNames[k] = wxString(wxT(".notdef"));
        }
      }
    }
  }
  return found;
}

wxString
wxPdfEncoding::GetEncodingName() const
{
  return m_encoding;
}

wxString
wxPdfEncoding::GetBaseEncodingName() const
{
  return m_baseEncoding;
}

wxString
wxPdfEncoding::GetDifferences() const
{
  wxString diffs = wxEmptyString;
  int last = 0;
  int i;
  for (i = m_firstChar; i <= m_lastChar; i++)
  {
    if (m_cmap[i] != 0x0000 && m_cmap[i] != m_cmapBase[i])
    {
      if (i != last+1)
      {
        diffs += wxString::Format(wxT("%d "), i);
      }
      last = i;
      diffs = diffs + wxString(wxT("/")) + m_glyphNames[i] + wxString(wxT(" "));
    }
  }
  return diffs;
}

wxPdfArrayUint32
wxPdfEncoding::GetCMap() const
{
  return m_cmap;
}

void
wxPdfEncoding::InitializeEncodingMap()
{
  if (m_encodingMap == NULL)
  {
    CreateEncodingConvMap();
  }
}

const wxPdfChar2GlyphMap*
wxPdfEncoding::GetEncodingMap() const
{
  return m_encodingMap;
}

wxArrayString
wxPdfEncoding::GetGlyphNames() const
{
  return m_glyphNames;
}

void
wxPdfEncoding::CreateEncodingConvMap()
{
  if (m_encodingMap == NULL)
  {
    m_encodingMap = new wxPdfChar2GlyphMap();
    size_t n = m_cmap.GetCount();
    size_t j;
    for (j = 0; j < n; ++j)
    {
      (*m_encodingMap)[m_cmap[j]] = j;
    }
  }
}

// --- Encoding Checker

wxPdfEncodingChecker::wxPdfEncodingChecker()
{
  m_encoding = wxEmptyString;
}

wxPdfEncodingChecker::~wxPdfEncodingChecker()
{
}

wxString
wxPdfEncodingChecker::GetEncodingName() const
{
  return m_encoding;
}

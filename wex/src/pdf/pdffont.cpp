///////////////////////////////////////////////////////////////////////////////
// Name:        pdffont.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-10
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffont.cpp Implementation of wxPdfFont class

// For compilers that support precompilation, includes <wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

// includes

#include "wex/pdf/pdfencoding.h"
#include "wex/pdf/pdffont.h"
#include "wex/pdf/pdffontextended.h"
#include "wex/pdf/pdffontdata.h"
#include "wex/pdf/pdffontdatatype1.h"
#include "wex/pdf/pdffontmanager.h"

#include "wxmemdbg.h"

wxPdfFont::wxPdfFont() 
  : m_embed(false), m_subset(false), m_fontStyle(wxPDF_FONTSTYLE_REGULAR), 
    m_fontData(NULL), m_encoding(NULL)
{
}

wxPdfFont::~wxPdfFont()
{
  if (m_fontData != NULL && m_fontData->DecrementRefCount() == 0)
  {
    delete m_fontData;
  }
}

wxPdfFont::wxPdfFont(wxPdfFontData* fontData, int fontStyle) 
  : m_embed(false), m_subset(false), m_fontStyle(fontStyle), 
    m_fontData(fontData), m_encoding(NULL)
{
  if (m_fontData != NULL)
  {
    m_fontData->IncrementRefCount();
    m_embed = m_fontData->EmbedSupported();
    m_subset = m_fontData->SubsetSupported();
    m_fontStyle |= m_fontData->GetStyle();
  }
  m_fontStyle &= wxPDF_FONTSTYLE_MASK;
}

wxPdfFont::wxPdfFont(const wxPdfFont& font)
  : m_embed(font.m_embed), m_subset(font.m_subset), m_fontStyle(font.m_fontStyle), 
    m_fontData(font.m_fontData), m_encoding(NULL)
{ 
  if (m_fontData != NULL)
  {
    m_fontData->IncrementRefCount();
  }
  m_encoding = font.m_encoding;
}

wxPdfFont& 
wxPdfFont::operator=(const wxPdfFont& font)
{ // DO NOT CHANGE THE ORDER OF THESE STATEMENTS!
  // (This order properly handles self-assignment)
  // (This order also properly handles recursion, e.g., if a Fred contains FredPtrs)
  wxPdfFontData* const prevFontData = m_fontData;
  m_embed = font.m_embed;
  m_subset = font.m_subset;
  m_fontStyle = font.m_fontStyle;
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
wxPdfFont::IsValid() const
{
  return (m_fontData != NULL);
}

wxString
wxPdfFont::GetType() const
{
  return (m_fontData != NULL) ? m_fontData->GetType() : wxString(wxT(""));
}

wxString
wxPdfFont::GetFamily() const
{
  return (m_fontData != NULL) ? m_fontData->GetFamily() : wxString(wxT(""));
}

wxString
wxPdfFont::GetName() const
{
  return (m_fontData != NULL) ? m_fontData->GetName() : wxString(wxT(""));
}

int
wxPdfFont::GetStyle() const
{
  return m_fontStyle;
}

wxString
wxPdfFont::GetEncoding() const
{
  wxString encoding = wxEmptyString;
  if (m_fontData != NULL)
  {
    if (m_fontData->GetType().IsSameAs(wxT("Type1")) && m_encoding != NULL)
    {
      encoding = m_encoding->GetEncodingName();
    }
    else
    {
      encoding = m_fontData->GetEncoding();
    }
  }
  return encoding;
}

bool
wxPdfFont::CanShow(const wxString& s)
{
  bool canShow = false;
  if (m_fontData != NULL && wxPdfFontManager::GetFontManager()->InitializeFontData(*this))
  {
    wxPdfFontExtended extendedFont(*this);
    canShow = extendedFont.CanShow(s);
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfFont::CanShow: ")) +
               wxString(_("Error on initializing the font.")));
  }
  return canShow;
}

static int wxCMPFUNC_CONV
CompareUint32(wxUint32* n1, wxUint32* n2)
{
  return (*n1 > *n2) ? 1 : (*n1 < *n2) ? -1 : 0;
}

bool
wxPdfFont::GetSupportedUnicodeCharacters(wxPdfArrayUint32& unicodeCharacters) const
{
  bool ok = false;
#if wxUSE_UNICODE
  if (m_fontData != NULL && wxPdfFontManager::GetFontManager()->InitializeFontData(*this))
  {
    size_t charCount = unicodeCharacters.GetCount();
    size_t n = 0;
    const wxPdfChar2GlyphMap* ctgMap = m_fontData->GetChar2GlyphMap();
    if (ctgMap == NULL && m_encoding != NULL)
    {
      ctgMap = m_encoding->GetEncodingMap();
    }
    if (ctgMap != NULL)
    {
      size_t glyphCount = ctgMap->size();
      if (glyphCount < charCount)
      {
        unicodeCharacters.RemoveAt(glyphCount, charCount-glyphCount);
      }
      else
      {
        unicodeCharacters.SetCount(glyphCount);
      }
      wxPdfChar2GlyphMap::const_iterator ccIter;
      for (ccIter = ctgMap->begin(); ccIter != ctgMap->end(); ccIter++)
      {
        unicodeCharacters[n++] = ccIter->first;
      }
      unicodeCharacters.Sort(CompareUint32);
      ok = true;
    }
    else
    {
      wxPdfEncodingChecker* encodingChecker = m_fontData->GetEncodingChecker();
      if (encodingChecker != NULL)
      {
        wxUint32 cc;
        for (cc = 0; cc < 0xffff; ++cc)
        {
          if (encodingChecker->IsIncluded(cc))
          {
            if (n < charCount)
            {
              unicodeCharacters[n++] = cc;
            }
            else
            {
              unicodeCharacters.Add(cc);
            }
          }
        }
        ok = true;
      }
    }
  }
#else
  wxUnusedVar(unicodeCharacters);
#endif
  return ok;
}

wxString
wxPdfFont::ConvertToValid(const wxString& s, wxChar replace) const
{
  wxString t;
  if (m_fontData != NULL && wxPdfFontManager::GetFontManager()->InitializeFontData(*this))
  {
    t = m_fontData->ConvertToValid(s, replace);
  }
  else
  {
    t = s;
  }
  return t;
}

bool
wxPdfFont::GetGlyphNames(wxArrayString& glyphNames) const
{
  bool ok = false;
  if (m_fontData != NULL && wxPdfFontManager::GetFontManager()->InitializeFontData(*this))
  {
    ok = m_fontData->GetGlyphNames(glyphNames);
  }
  return ok;
}

double
wxPdfFont::GetStringWidth(const wxString& s) const
{
  double width = 0.0;
  if (m_fontData != NULL && wxPdfFontManager::GetFontManager()->InitializeFontData(*this))
  {
    width = m_fontData->GetStringWidth(s);
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfFont::GetStringWidth: ")) +
               wxString(_("Error on initializing the font.")));
  }
  return width;
}

bool
wxPdfFont::EmbedRequired() const
{
  return (m_fontData != NULL) ? m_fontData->EmbedRequired() : false;
}

bool
wxPdfFont::EmbedSupported() const
{
  return (m_fontData != NULL) ? m_fontData->EmbedSupported() : false;
}

bool
wxPdfFont::SubsetSupported() const
{
  return (m_fontData != NULL) ? m_fontData->SubsetSupported() : false;
}

const wxPdfFontDescription
wxPdfFont::GetDescription() const
{
  wxPdfFontDescription fontDesc;
  if (m_fontData != NULL && wxPdfFontManager::GetFontManager()->InitializeFontData(*this))
  {
    fontDesc = m_fontData->GetDescription();
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfFont::GetDescription: ")) +
               wxString(_("Error on initializing the font.")));
  }
  return fontDesc;
}

void
wxPdfFont::SetEmbed(bool embed)
{
  if (embed)
  {
    m_embed = EmbedSupported() || EmbedRequired();
  }
  else
  {
    m_embed = EmbedRequired();
  }
}

void
wxPdfFont::SetSubset(bool subset)
{
  m_subset = subset && SubsetSupported();
}

bool
wxPdfFont::SetEncoding(const wxString& encodingName)
{
  bool ok = false;
#if wxUSE_UNICODE
  const wxPdfEncoding* encoding = wxPdfFontManager::GetFontManager()->GetEncoding(encodingName);
  if (m_fontData != NULL)
  {
    if (m_fontData->GetType().IsSameAs(wxT("Type1")) && 
        encoding != NULL && encoding->IsOk())
    {
      ok = wxPdfFontManager::GetFontManager()->InitializeFontData(*this);
      if (ok)
      {
        if (!((wxPdfFontDataType1*) m_fontData)->GetEncodingType().IsEmpty())
        {
          m_encoding = encoding;
          ok = true;
        }
        else
        {
          wxLogDebug(wxString(wxT("wxPdfFont::SetEncoding: ")) +
                     wxString(_("Setting a user defined encoding is only supported for dynamically loaded Type1 fonts.")));
        }
      }
      else
      {
        wxLogDebug(wxString(wxT("wxPdfFont::SetEncoding: ")) +
                   wxString(_("Loading of font data failed.")));
      }
    }
  }
#else
  wxUnusedVar(encodingName);
  wxLogDebug(wxString(wxT("wxPdfFont::SetEncoding: ")) +
             wxString(_("Setting a user defined encoding is not supported in ANSI build.")));
#endif
  return ok;
}

bool
wxPdfFont::GetEncoding(wxPdfEncoding& encoding)
{
  bool ok = false;
  if (m_encoding != NULL)
  {
    encoding = *m_encoding;
    ok = true;
  }
  else if (m_fontData != NULL)
  {
    const wxPdfEncoding* baseEncoding = m_fontData->GetBaseEncoding();
    if (baseEncoding != NULL)
    {
      encoding = *baseEncoding;
      ok = true;
    }
  }
  return ok;
}

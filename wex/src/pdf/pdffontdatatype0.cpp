///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontdatatype0.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-10
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontdatatype0.cpp Implementation of wxPdfFontDataType0 class

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
#include "wex/pdf/pdffontdatatype0.h"

#include "wxmemdbg.h"

#if wxUSE_UNICODE

// ----------------------------------------------------------------------------
// wxPdfFontType0: class 
// ----------------------------------------------------------------------------

wxPdfFontDataType0::wxPdfFontDataType0()
  : wxPdfFontData()
{
  m_type = wxT("Type0");
  m_conv = NULL;
  m_hwRange = false;
}

wxPdfFontDataType0::wxPdfFontDataType0(const wxString& family, const wxString& name,
                                       const wxString& encoding, const wxString& ordering, 
                                       const wxString& supplement, const wxString& cmap,
                                       short* cwArray, const wxPdfFontDescription& desc)
  : wxPdfFontData()
{
  m_type   = wxT("Type0");
  m_conv = NULL;
  m_family = family;
  m_name   = name;
  m_desc  = desc;

  m_style = FindStyleFromName(name);

  m_enc = encoding;
  m_ordering = ordering;
  m_supplement = supplement;
  m_cmap = cmap;

  if (cwArray != NULL)
  {
    m_cw = new wxPdfGlyphWidthMap();
    int j;
    for (j = 32; j < 127; ++j)
    {
      (*m_cw)[j] = cwArray[j-32];
    }
  }

  CreateDefaultEncodingConv();
  if (m_ordering == wxT("Japan1"))
  {
    m_hwRange = true;
    m_hwFirst = 0xff61;
    m_hwLast  = 0xff9f;
  }
  else
  {
    m_hwRange = false;
  }
  m_initialized = true;
}

wxPdfFontDataType0::~wxPdfFontDataType0()
{
  if (m_conv != NULL)
  {
    delete m_conv;
  }
}

void
wxPdfFontDataType0::CreateDefaultEncodingConv()
{
  if (m_conv == NULL)
  {
    if (m_enc.Length() > 0)
    {
      m_conv = new wxCSConv(m_enc);
    }
    else
    {
      m_conv = new wxCSConv(wxFONTENCODING_ISO8859_1);
    }
  }
}

void
wxPdfFontDataType0::SetHalfWidthRanges(bool hwRange, wxChar hwFirst, wxChar hwLast)
{
  m_hwRange = hwRange;
  m_hwFirst = hwFirst;
  m_hwLast  = hwLast;
}

bool
wxPdfFontDataType0::LoadFontMetrics(wxXmlNode* root)
{
  bool bName     = false,
       bDesc     = false,
       bRegistry = false,
       bCmap     = false,
       bWidth    = false;
  wxString value;
  wxXmlNode *child = root->GetChildren();
  while (child)
  {
    // parse the children
    if (child->GetName() == wxT("font-name"))
    {
      m_name = GetNodeContent(child);
      bName = m_name.Length() > 0;
    }
    else if (child->GetName() == wxT("encoding"))
    {
      m_enc = GetNodeContent(child);
    }
    else if (child->GetName() == wxT("description"))
    {
      bDesc = GetFontDescription(child, m_desc);
    }
    else if (child->GetName() == wxT("cmap"))
    {
      m_cmap = wxEmptyString;
      value = GetNodeContent(child);
      if (value.Length() > 0)
      {
        bCmap = true;
        m_cmap = value;
      }
    }
    else if (child->GetName() == wxT("registry"))
    {
      m_ordering = wxEmptyString;
      m_supplement = wxEmptyString;
#if wxCHECK_VERSION(2,9,0)
      value = child->GetAttribute(wxT("ordering"), wxT(""));
#else
      value = child->GetPropVal(wxT("ordering"), wxT(""));
#endif
      if (value.Length() > 0)
      {
        m_ordering = value;
#if wxCHECK_VERSION(2,9,0)
        value = child->GetAttribute(wxT("supplement"), wxT(""));
#else
        value = child->GetPropVal(wxT("supplement"), wxT(""));
#endif
        if (value.Length() > 0)
        {
          bRegistry = true;
          m_supplement = value;
        }
        else
        {
          bRegistry = false;
        }
      }
    }
    else if (child->GetName() == wxT("widths"))
    {
      bWidth = true;
      m_cw = new wxPdfGlyphWidthMap();
      const wxXmlNode *charNode = child->GetChildren();
      while (charNode)
      {
        wxString strId, strWidth;
        long charId, charWidth;
        if (charNode->GetName() == wxT("char"))
        {
#if wxCHECK_VERSION(2,9,0)
          strId = charNode->GetAttribute(wxT("id"), wxT(""));
          strWidth = charNode->GetAttribute(wxT("width"), wxT(""));
#else
          strId = charNode->GetPropVal(wxT("id"), wxT(""));
          strWidth = charNode->GetPropVal(wxT("width"), wxT(""));
#endif
          if (strId.Length() > 0 && strId.ToLong(&charId) &&
              strWidth.Length() > 0 && strWidth.ToLong(&charWidth))
          {
            (*m_cw)[charId] = charWidth;
          }
        }
        charNode = charNode->GetNext();
      }
    }
    child = child->GetNext();
  }
  CreateDefaultEncodingConv();
  if (m_ordering == wxT("Japan1"))
  {
    m_hwRange = true;
    m_hwFirst = 0xff61;
    m_hwLast  = 0xff9f;
  }
  m_initialized = (bName && bDesc && bRegistry && bCmap && bWidth);
  return m_initialized;
}

wxString
wxPdfFontDataType0::GetWidthsAsString(bool subset, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs) const
{
  wxUnusedVar(subset);
  wxUnusedVar(usedGlyphs);
  wxUnusedVar(subsetGlyphs);
  wxString s = wxString(wxT("[1 ["));
  int i;
  for (i = 32; i <= 126; i++)
  {
    s += wxString::Format(wxT("%u "), (*m_cw)[i]);
  }
  s += wxString(wxT("]"));
  if (HasHalfWidthRange())
  {
    s += wxString(wxT(" 231 325 500 631 [500] 326 389 500"));
  }
  s += wxString(wxT("]"));
  return s;
}

double
wxPdfFontDataType0::GetStringWidth(const wxString& s, const wxPdfEncoding* encoding, bool withKerning) const
{
  wxUnusedVar(encoding);
  wxString t = ConvertToValid(s);
  // Get width of a string in the current font
  double w = 0;
  wxPdfGlyphWidthMap::iterator charIter;
  wxString::const_iterator ch;
  for (ch = t.begin(); ch != t.end(); ++ch)
  {
    wxChar c = *ch;
    if (c >= 0 && c < 128)
    {
      wxPdfGlyphWidthMap::iterator charIter = (*m_cw).find(c);
      if (charIter != (*m_cw).end())
      {
        w += charIter->second;
      }
      else
      {
        w += m_desc.GetMissingWidth();
      }
    }
    else
    {
      if (HasHalfWidthRange() &&
          c >= HalfWidthRangeFirst() &&
          c <= HalfWidthRangeLast())
      {
        w += 500;
      }
      else
      {
        w += 1000;
      }
    }
  }
  if (withKerning)
  {
    int kerningWidth = GetKerningWidth(s);
    if (kerningWidth != 0)
    {
      w += (double) kerningWidth;
    }
  }
  return w / 1000;
}

bool
wxPdfFontDataType0::CanShow(const wxString& s, const wxPdfEncoding* encoding) const
{
  wxUnusedVar(encoding);
  bool canShow = true;
#if wxUSE_UNICODE
  if (m_encodingChecker != NULL)
  {
    wxString::const_iterator ch = s.begin();
    for (ch = s.begin(); canShow && ch != s.end(); ++ch)
    {
      canShow = (m_encodingChecker->IsIncluded((wxUint32) *ch));
    }
  }
#endif
  return canShow;
}

wxString
wxPdfFontDataType0::ConvertCID2GID(const wxString& s, 
                                   const wxPdfEncoding* encoding, 
                                   wxPdfSortedArrayInt* usedGlyphs, 
                                   wxPdfChar2GlyphMap* subsetGlyphs) const
{
  // No conversion from cid to gid
  wxUnusedVar(encoding);
  wxUnusedVar(usedGlyphs);
  wxUnusedVar(subsetGlyphs);
  return ConvertToValid(s);
}

#endif // wxUSE_UNICODE

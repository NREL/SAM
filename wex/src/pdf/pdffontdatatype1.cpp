///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontdatatype1.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-07
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontdatatype1.cpp Implementation of wxPdfFontDataType1 class

// For compilers that support precompilation, includes <wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

// includes
#include <wx/filename.h>
#include <wx/filesys.h>
#include <wx/zstream.h>

#include "wex/pdf/pdfencoding.h"
#include "wex/pdf/pdffontdatatype1.h"
#include "wex/pdf/pdffontparsertype1.h"

#include "wxmemdbg.h"

wxPdfFontDataType1::wxPdfFontDataType1(wxMemoryInputStream* pfbStream)
  : wxPdfFontData()
{
  m_type  = wxT("Type1");
  m_pfbStream = pfbStream;
  m_conv = NULL;

  m_embedRequired = false;
  m_embedSupported = true;
  m_subsetSupported = false;

#if wxUSE_UNICODE
  m_glyphWidthMap = NULL;
#endif
}

wxPdfFontDataType1::~wxPdfFontDataType1()
{
  if (m_pfbStream != NULL)
  {
    delete m_pfbStream;
  }
  if (m_conv != NULL)
  {
    delete m_conv;
  }
#if wxUSE_UNICODE
  if (m_glyphWidthMap != NULL)
  {
    delete m_glyphWidthMap;
  }
#endif
}

#if wxUSE_UNICODE
void
wxPdfFontDataType1::CreateDefaultEncodingConv()
{
  // Unicode build needs charset conversion
  if (m_conv == NULL)
  {
    m_conv = new wxCSConv(wxFONTENCODING_ISO8859_1);
  }
}
#endif

bool
wxPdfFontDataType1::LoadFontMetrics(wxXmlNode* root)
{
  bool bName  = false,
       bDesc  = false,
       bFile  = true,
       bSize  = true,
       bWidth = false;
  wxString value;
  long number;
  wxXmlNode *child = root->GetChildren();
  while (child)
  {
    // parse the children
    if (child->GetName() == wxT("font-name"))
    {
      m_name = GetNodeContent(child);
      m_style = FindStyleFromName(m_name);
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
    else if (child->GetName() == wxT("diff"))
    {
      m_diffs = GetNodeContent(child);
    }
    else if (child->GetName() == wxT("file"))
    {
#if wxCHECK_VERSION(2,9,0)
      value = child->GetAttribute(wxT("name"), wxT(""));
#else
      value = child->GetPropVal(wxT("name"), wxT(""));
#endif
      if (value.Length() > 0)
      {
        m_file = value;
#if wxCHECK_VERSION(2,9,0)
        value = child->GetAttribute(wxT("size1"), wxT(""));
#else
        value = child->GetPropVal(wxT("size1"), wxT(""));
#endif
        if (value.Length() > 0 && value.ToLong(&number))
        {
          bFile = true;
          m_size1 = number;
#if wxCHECK_VERSION(2,9,0)
          value = child->GetAttribute(wxT("size2"), wxT(""));
#else
          value = child->GetPropVal(wxT("size2"), wxT(""));
#endif
          if (value.Length() > 0 && value.ToLong(&number))
          {
            m_size2 = number;
          }
        }
        else
        {
          bFile = false;
          m_file = wxT("");
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

#if wxUSE_UNICODE
  CreateDefaultEncodingConv();
#endif

  m_initialized = (bName && bDesc && bFile && bSize && bWidth);
  if (m_initialized)
  {
    wxFileName fileName(m_file);
    m_initialized = fileName.MakeAbsolute(m_path) && fileName.FileExists() && fileName.IsFileReadable();
  }
  return m_initialized;
}

bool
wxPdfFontDataType1::Initialize()
{
  bool ok = true;
  if (!IsInitialized())
  {
#if wxUSE_UNICODE
    wxPdfFontParserType1 fontParser;
    ok = fontParser.LoadFontData(this);
    m_initialized = ok;
#else
    ok = false;
#endif
  }
  return ok;
}

double
wxPdfFontDataType1::GetStringWidth(const wxString& s, const wxPdfEncoding* encoding, bool withKerning) const
{
  // Get width of a string in the current font
  double w = 0;
  const wxPdfChar2GlyphMap* convMap = FindEncodingMap(encoding);
  if (convMap != NULL)
  {
    wxArrayString glyphNames;
    if (encoding != NULL)
    {
      glyphNames = encoding->GetGlyphNames();
    }
    else
    {
      glyphNames = m_encoding->GetGlyphNames();
    }
    wxUint16 glyph;
    wxPdfGlyphWidthMap::iterator glyphIter;
    wxPdfFontType1GlyphWidthMap::iterator type1GlyphIter;
    wxPdfChar2GlyphMap::const_iterator charIter;
    wxString::const_iterator ch;
    for (ch = s.begin(); ch != s.end(); ++ch)
    {
      charIter = (*convMap).find(*ch);
      if (charIter != (*convMap).end())
      {
        glyph = charIter->second;
      }
      else
      {
        glyph = 32;
      }
#if wxUSE_UNICODE
      if (m_glyphWidthMap != NULL)
      {
        type1GlyphIter = m_glyphWidthMap->find(glyphNames[glyph]);
        if (type1GlyphIter != m_glyphWidthMap->end())
        {
          w += type1GlyphIter->second;
        }
        else
        {
          w += m_desc.GetMissingWidth();
        }
      }
      else
#endif
      {
        glyphIter = (*m_cw).find(glyph);
        if (glyphIter != (*m_cw).end())
        {
          w += glyphIter->second;
        }
        else
        {
          w += m_desc.GetMissingWidth();
        }
      }
    }
  }
  else
  {
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
wxPdfFontDataType1::CanShow(const wxString& s, const wxPdfEncoding* encoding) const
{
  bool canShow = true;
  const wxPdfChar2GlyphMap* convMap = FindEncodingMap(encoding);
  if (convMap != NULL)
  {
    wxString::const_iterator ch;
    for (ch = s.begin(); canShow && ch != s.end(); ++ch)
    {
      canShow = (convMap->find(*ch) != convMap->end());
    }
  }
  return canShow;
}

wxString
wxPdfFontDataType1::ConvertCID2GID(const wxString& s, 
                                   const wxPdfEncoding* encoding, 
                                   wxPdfSortedArrayInt* usedGlyphs, 
                                   wxPdfChar2GlyphMap* subsetGlyphs) const
{
  wxUnusedVar(usedGlyphs);
  wxUnusedVar(subsetGlyphs);

  wxString t;
  const wxPdfChar2GlyphMap* convMap = FindEncodingMap(encoding);
  if (convMap != NULL)
  {
    wxPdfChar2GlyphMap::const_iterator charIter;
    wxString::const_iterator ch;
    for (ch = s.begin(); ch != s.end(); ++ch)
    {
      charIter = (*convMap).find(*ch);
      if (charIter != (*convMap).end())
      {
#if wxCHECK_VERSION(2,9,0)
        t.Append(wxUniChar(charIter->second));
#else
        t.Append(wxChar(charIter->second));
#endif
      }
      else
      {
        t += wxT(" ");
      }
    }
  }
  else
  {
    t = s;
  }
  return t;
}

wxString
wxPdfFontDataType1::GetWidthsAsString(bool subset, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs) const
{
  wxUnusedVar(subset);
  wxUnusedVar(usedGlyphs);
  wxUnusedVar(subsetGlyphs);
  wxString s = wxString(wxT("["));
  int i;
  for (i = 32; i <= 255; i++)
  {
    s += wxString::Format(wxT("%u "), (*m_cw)[i]);
  }
  s += wxString(wxT("]"));
  return s;
}

#if wxUSE_UNICODE
wxString
wxPdfFontDataType1::GetWidthsAsString(const wxArrayString& glyphNames, bool subset, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs) const
{
  wxUnusedVar(subset);
  wxUnusedVar(usedGlyphs);
  wxUnusedVar(subsetGlyphs);
  wxString glyph;
  wxPdfFontType1GlyphWidthMap::const_iterator glyphIter;
  wxString s = wxString(wxT("["));
  int missingWidth = m_desc.GetMissingWidth();
  int width;
  int i;
  for (i = 32; i <= 255; i++)
  {
    glyph = glyphNames[i];
    glyphIter = m_glyphWidthMap->find(glyph);
    if (glyphIter != m_glyphWidthMap->end())
    {
      width = glyphIter->second;
    }
    else
    {
      width = missingWidth;
    }
    s += wxString::Format(wxT("%d "), width);
  }
  s += wxString(wxT("]"));
  return s;
}

bool
wxPdfFontDataType1::GetGlyphNames(wxArrayString& glyphNames) const
{
  bool ok = m_glyphWidthMap != NULL && !m_glyphWidthMap->empty();
  if (ok)
  {
    glyphNames.Empty();
    wxPdfFontType1GlyphWidthMap::const_iterator glyphIter;
    for (glyphIter = m_glyphWidthMap->begin(); glyphIter != m_glyphWidthMap->end(); ++glyphIter)
    {
      glyphNames.Add(glyphIter->first);
    }
    glyphNames.Sort();
  }
  return ok;
}
#endif

/// Fast string search (KMP method): initialization
static int*
makeFail(const char* target, int tlen)
{
  int t = 0;
  int s, m;
  m = tlen;
  int* f = new int[m+1];
  f[1] = 0;
  for (s = 1; s < m; s++)
  {
    while ((t > 0) && (target[s] != target[t]))
    {
      t = f[t];
    }
    if (target[t] == target[s])
    {
      t++;
      f[s+1] = t;
    }
    else
    {
      f[s+1] = 0;
    }
  }
  return f;
}

/// Fast string search (KMP method)
static int
findString(const char* src, int slen, const char* target, int tlen, int* f)
{
  int s = 0;
  int i;
  int m = tlen;
  for (i = 0; i < slen; i++)
  {
    while ( (s > 0) && (src[i] != target[s]))
    {
      s = f[s];
    }
    if (src[i] == target[s]) s++;
    if (s == m) return (i-m+1);
  }
  return -1;
}

bool
wxPdfFontDataType1::CompressFontData(wxOutputStream* fontData, wxInputStream* pfbFile)
{
  bool ok = true;
  size_t len = pfbFile->GetLength();
  // Find first two sections and discard third one
  unsigned char* buffer = new unsigned char[len];
  unsigned char* buf1 = buffer;
  unsigned char* buf2;
  pfbFile->Read(buffer, len);
  unsigned char first = buffer[0];
  if (first == 128)
  {
    // Strip first binary header
    buf1 += 6;
    len -= 6;
  }
  int* f = makeFail("eexec",5);
  int size1 = (int) findString((char*) buf1, len, "eexec", 5, f);
  delete [] f;

  int size2 = -1;
  if (size1 >= 0)
  {
    size1 += 6;
    unsigned char second = buf1[size1];
    buf2 = buf1 + size1;
    len -= size1;
    if (first == 128 && second == 128)
    {
      // Strip second binary header
      buf2 += 6;
      len -= 6;
    }
    f = makeFail("00000000",8);
    size2 = (int) findString((char*) buf2, len, "00000000", 8, f);
    delete [] f;
    if (size2 >= 0)
    {
      wxZlibOutputStream zFontData(*fontData);
      zFontData.Write(buf1, size1);
      zFontData.Write(buf2, size2);
      zFontData.Close();
      m_size1 = size1;
      m_size2 = size2;
    }
  }
  if (size1 < 0 || size2 < 0)
  {
    wxLogError(wxString(wxT("wxPdfFontDataType1::CompressFontData: ")) + 
               wxString(_("Font file does not seem to be valid Type1, font embedding not possible.")));
    ok = false;
  }
  delete [] buffer;
  return ok;
}

size_t
wxPdfFontDataType1::WriteFontData(wxOutputStream* fontData, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs)
{
  wxUnusedVar(usedGlyphs);
  wxUnusedVar(subsetGlyphs);

  bool compressed = false;
  wxFileName fileName;
  if (m_fontFileName.IsEmpty())
  {
    // Font data preprocessed by MakeFont
    compressed = m_file.Lower().Right(2) == wxT(".z");
    fileName = m_file;
    fileName.MakeAbsolute(m_path);
  }
  else
  {
    fileName = m_fontFileName;
  }

  // Open font file
  wxFileSystem fs;
  wxFSFile* fontFile = NULL;
  wxInputStream* fontStream = NULL;
  if (m_pfbStream == NULL)
  {
    fontFile = fs.OpenFile(wxFileSystem::FileNameToURL(fileName));
    if (fontFile)
    {
      fontStream = fontFile->GetStream();
    }
    else
    {
      // usually this should not happen since file accessability was already checked
      wxLogError(wxString(wxT("wxPdfFontDataType1::WriteFontData: ")) +
                 wxString::Format(_("Font file '%s' not found."), fileName.GetFullPath().c_str()));
    }
  }
  else
  {
    fontStream = m_pfbStream;
  }

  if (fontStream != NULL)
  {
    if (!compressed)
    {
      CompressFontData(fontData, fontStream);
    }
    else
    {
      // Font data already compressed
      fontData->Write(*fontStream);
    }
  }

  if (fontFile != NULL)
  {
    delete fontFile;
  }

  return m_size1;
}

size_t
wxPdfFontDataType1::WriteUnicodeMap(wxOutputStream* mapData, 
                                    const wxPdfEncoding* encoding, 
                                    wxPdfSortedArrayInt* usedGlyphs, 
                                    wxPdfChar2GlyphMap* subsetGlyphs)
{
  wxUnusedVar(usedGlyphs);
  wxUnusedVar(subsetGlyphs);

  wxPdfGlyphList glyphList(wxPdfFontData::CompareGlyphListEntries);
  wxPdfChar2GlyphMap::const_iterator charIter;
  const wxPdfChar2GlyphMap* convMap = encoding->GetEncodingMap();
  if (convMap == NULL)
  {
    convMap = m_encoding->GetEncodingMap();
  }
  if (convMap != NULL)
  {
    for (charIter = convMap->begin(); charIter != convMap->end(); ++charIter)
    {
      wxPdfGlyphListEntry* glEntry = new wxPdfGlyphListEntry();
      glEntry->m_gid = charIter->second;
      glEntry->m_uid = charIter->first;
      glyphList.Add(glEntry);
    }
    wxMemoryOutputStream toUnicode;
    WriteToUnicode(glyphList, toUnicode, true);
    wxMemoryInputStream inUnicode(toUnicode);
    wxZlibOutputStream zUnicodeMap(*mapData);
    zUnicodeMap.Write(inUnicode);
    zUnicodeMap.Close();

    WX_CLEAR_ARRAY(glyphList);
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfFontDataType1::WriteUnicodeMap: ")) +
               wxString::Format(_("Encoding not found.")));
  }

  return 0;
}

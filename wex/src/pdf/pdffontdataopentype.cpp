///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontdataopentype.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-10
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontdataopentype.cpp Implementation of wxPdfFontDataOpenTypeUnicode class

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

#include "wex/pdf/pdfarraytypes.h"
#include "wex/pdf/pdffontdataopentype.h"
#include "wex/pdf/pdffontparsertruetype.h"

#include "wex/pdf/pdffontsubsetcff.h"

#include "wxmemdbg.h"

#if wxUSE_UNICODE

wxPdfFontDataOpenTypeUnicode::wxPdfFontDataOpenTypeUnicode()
  : wxPdfFontData()
{
  m_type = wxT("OpenTypeUnicode");
  m_gw   = NULL;
  m_conv = NULL;
  m_cff = true;

  m_embedRequired = true;
  m_embedSupported = true;
  m_subsetSupported = true;
}

wxPdfFontDataOpenTypeUnicode::~wxPdfFontDataOpenTypeUnicode()
{
  // delete m_cw;
  if (m_conv != NULL)
  {
    delete m_conv;
  }
  if (m_gw != NULL)
  {
    delete m_gw;
  }
}

void
wxPdfFontDataOpenTypeUnicode::CreateDefaultEncodingConv()
{
  if (m_conv == NULL)
  {
    m_conv = new wxMBConvUTF16BE();
  }
}

bool
wxPdfFontDataOpenTypeUnicode::LoadFontMetrics(wxXmlNode* root)
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
      value = child->GetAttribute(wxT("ctg"), wxT(""));
#else
      value = child->GetPropVal(wxT("ctg"), wxT(""));
#endif
      if (value.Length() > 0)
      {
        bFile = true;
        m_ctg = value;
#if wxCHECK_VERSION(2,9,0)
        value = child->GetAttribute(wxT("name"), wxT(""));
#else
        value = child->GetPropVal(wxT("name"), wxT(""));
#endif
        if (value.Length() > 0)
        {
          m_file = value;
#if wxCHECK_VERSION(2,9,0)
          value = child->GetAttribute(wxT("originalsize"), wxT(""));
#else
          value = child->GetPropVal(wxT("originalsize"), wxT(""));
#endif
          if (value.Length() > 0 && value.ToLong(&number))
          {
            bFile = true;
            m_size1 = number;
          }
          else
          {
            bFile = false;
            m_file = wxT("");
          }
        }
      }
      else
      {
        bFile = false;
        m_file = wxT("");
        m_ctg = wxT("");
      }
    }
    else if (child->GetName() == wxT("widths"))
    {
#if wxCHECK_VERSION(2,9,0)
      wxString subsetting = child->GetAttribute(wxT("subsetting"), wxT("disabled"));
#else
      wxString subsetting = child->GetPropVal(wxT("subsetting"), wxT("disabled"));
#endif
      m_subsetSupported = (subsetting == wxT("enabled"));
      bWidth = true;
      m_cw = new wxPdfGlyphWidthMap();
      if (m_subsetSupported)
      {
        m_gn = new wxPdfChar2GlyphMap();
      }
      const wxXmlNode *charNode = child->GetChildren();
      while (charNode)
      {
        wxString strId, strGn, strWidth;
        long charId, glyph, charWidth;
        if (charNode->GetName() == wxT("char"))
        {
#if wxCHECK_VERSION(2,9,0)
          strId = charNode->GetAttribute(wxT("id"), wxT(""));
          if (m_subsetSupported)
          {
            strGn = charNode->GetAttribute(wxT("gn"), wxT(""));
          }
          strWidth = charNode->GetAttribute(wxT("width"), wxT(""));
#else
          strId = charNode->GetPropVal(wxT("id"), wxT(""));
          if (m_subsetSupported)
          {
            strGn = charNode->GetPropVal(wxT("gn"), wxT(""));
          }
          strWidth = charNode->GetPropVal(wxT("width"), wxT(""));
#endif
          if (strId.Length() > 0 && strId.ToLong(&charId) &&
              strWidth.Length() > 0 && strWidth.ToLong(&charWidth))
          {
            (*m_cw)[charId] = charWidth;
            if (m_subsetSupported)
            {
              if (strGn.Length() > 0 && strGn.ToLong(&glyph))
              {
                (*m_gn)[charId] = glyph;
              }
              else
              {
                (*m_gn)[charId] = 0;
              }
            }
          }
        }
        charNode = charNode->GetNext();
      }
    }
    child = child->GetNext();
  }
  CreateDefaultEncodingConv();

  m_initialized = (bName && bDesc && bFile && bSize && bWidth);
  if (m_initialized)
  {
    wxFileName fileName(m_file);
    m_initialized = fileName.MakeAbsolute(m_path) && fileName.FileExists() && fileName.IsFileReadable();
    if (m_initialized)
    {
      fileName.Assign(m_ctg);
      m_initialized = fileName.MakeAbsolute(m_path) && fileName.FileExists() && fileName.IsFileReadable();
    }
  }
  if (m_initialized && m_gn == NULL)
  {
    // We now always need a cid to gid mapping whether subsetting is enabled or not
    // So we read the CTG file produced by MakeFont and create the map
    bool compressed = m_ctg.Lower().Right(2) == wxT(".z");
    wxFileName fileName(m_ctg);
    fileName.MakeAbsolute(m_path);
    wxFileSystem fs;
    wxFSFile* ctgFile = fs.OpenFile(wxFileSystem::FileNameToURL(fileName));
    wxInputStream* ctgStream = NULL;
    if (ctgFile)
    {
      ctgStream = ctgFile->GetStream();
    }
    else
    {
      m_initialized = false;
      // usually this should not happen since file accessability was already checked
      wxLogError(wxString(wxT("wxPdfFontDataOpenTypeUnicode::LoadFontMetrics: ")) +
                 wxString::Format(_("CTG file '%s' not found."), fileName.GetFullPath().c_str()));
    }
    if (ctgStream)
    {
      size_t ctgLen;
      unsigned char* cc2gn = NULL;
      if (compressed)
      {
        wxZlibInputStream zin(*ctgStream);
        wxMemoryOutputStream zout;
        zout.Write(zin);
        zout.Close();
        wxMemoryInputStream cid2gidStream(zout);
        ctgLen = cid2gidStream.GetSize();
        cc2gn = new unsigned char[ctgLen];
        cid2gidStream.Read(cc2gn, ctgLen);
      }
      else
      {
        ctgLen = ctgStream->GetSize();
        cc2gn = new unsigned char[ctgLen];
        ctgStream->Read(cc2gn, ctgLen);
      }
      delete ctgFile;
    
      // Create the cid to gid mapping
      m_gn = new wxPdfChar2GlyphMap();
      size_t charId;
      for (charId = 0; charId < 0xFFFF; ++charId)
      {
        int glyph = (cc2gn[2*charId] << 8) + cc2gn[2*charId+1];
        if (glyph != 0)
        {
          (*m_gn)[charId] = glyph;
        }
      }
      delete [] cc2gn;
    }
  }
  return m_initialized;
}

bool
wxPdfFontDataOpenTypeUnicode::Initialize()
{
  bool ok = true;
  if (!IsInitialized())
  {
    wxPdfFontParserTrueType fontParser;
    ok = fontParser.LoadFontData(this);
    m_initialized = ok;
  }
  return ok;
}

void
wxPdfFontDataOpenTypeUnicode::SetGlyphWidths(const wxPdfArrayUint16& glyphWidths)
{
  if (m_gw == NULL)
  {
    m_gw = new wxPdfArrayUint16();
  }
  *m_gw = glyphWidths;
}

double
wxPdfFontDataOpenTypeUnicode::GetStringWidth(const wxString& s, const wxPdfEncoding* encoding, bool withKerning) const
{
  wxUnusedVar(encoding);
  // Get width of a string in the current font
  double w = 0;

  wxPdfGlyphWidthMap::iterator charIter;
  wxString::const_iterator ch;
  for (ch = s.begin(); ch != s.end(); ++ch)
  {
    wxChar c = *ch;
    charIter = m_cw->find(c);
    if (charIter != m_cw->end())
    {
      w += charIter->second;
    }
    else
    {
      w += m_desc.GetMissingWidth();
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
wxPdfFontDataOpenTypeUnicode::CanShow(const wxString& s, const wxPdfEncoding* encoding) const
{
  wxUnusedVar(encoding);
  bool canShow = true;
  wxString::const_iterator ch;
  for (ch = s.begin(); canShow && ch != s.end(); ++ch)
  {
    canShow = (m_gn->find(*ch) != m_gn->end());
  }
  return canShow;
}

wxString
wxPdfFontDataOpenTypeUnicode::ConvertCID2GID(const wxString& s, 
                                             const wxPdfEncoding* encoding,
                                             wxPdfSortedArrayInt* usedGlyphs, 
                                             wxPdfChar2GlyphMap* subsetGlyphs) const
{
  wxUnusedVar(encoding);
  bool doSubsetting = usedGlyphs != NULL && subsetGlyphs != NULL;
  wxString t;
  wxPdfChar2GlyphMap::const_iterator charIter;
  wxUint32 glyph, subsetGlyph;
  wxString::const_iterator ch;
  for (ch = s.begin(); ch != s.end(); ++ch)
  {
    charIter = m_gn->find(*ch);
    if (charIter != m_gn->end())
    {
      glyph = charIter->second;
      if (doSubsetting)
      {
        if (usedGlyphs->Index(glyph) != wxNOT_FOUND)
        {
          glyph = (*subsetGlyphs)[glyph];
        }
        else
        {
          subsetGlyph = (wxUint32) usedGlyphs->GetCount();
          (*subsetGlyphs)[glyph] = subsetGlyph;
          usedGlyphs->Add(glyph);
          glyph = subsetGlyph;
        }
      }
#if wxCHECK_VERSION(2,9,0)
      t.Append(wxUniChar(glyph));
#else
      t.Append(wxChar(glyph));
#endif
    }
    else
    {
#if wxCHECK_VERSION(2,9,0)
      t.Append(wxUniChar(0));
#else
      t.Append(wxChar(0));
#endif
    }
  }
  return t;
}

wxString
wxPdfFontDataOpenTypeUnicode::ConvertGlyph(wxUint32 glyph, 
                                           const wxPdfEncoding* encoding, 
                                           wxPdfSortedArrayInt* usedGlyphs, 
                                           wxPdfChar2GlyphMap* subsetGlyphs) const
{
  wxUnusedVar(encoding);
  wxString t = wxEmptyString;
  if (m_gw != NULL && glyph < m_gw->size())
  {
    bool doSubsetting = usedGlyphs != NULL && subsetGlyphs != NULL;
    wxUint32 subsetGlyph;
    if (doSubsetting)
    {
      if (usedGlyphs->Index(glyph) != wxNOT_FOUND)
      {
        glyph = (*subsetGlyphs)[glyph];
      }
      else
      {
        subsetGlyph = (wxUint32) usedGlyphs->GetCount();
        (*subsetGlyphs)[glyph] = subsetGlyph;
        usedGlyphs->Add(glyph);
        glyph = subsetGlyph;
      }
    }
#if wxCHECK_VERSION(2,9,0)
    t.Append(wxUniChar(glyph));
#else
    t.Append(wxChar(glyph));
#endif
  }
  else
  {
#if wxCHECK_VERSION(2,9,0)
    t.Append(wxUniChar(0));
#else
    t.Append(wxChar(0));
#endif
  }
  return t;
}

wxString
wxPdfFontDataOpenTypeUnicode::GetWidthsAsString(bool subset, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs) const
{
  wxString s = wxString(wxT("["));
  wxUint32 glyph;
  wxPdfChar2GlyphMap::const_iterator glyphIter;
  wxPdfGlyphWidthMap::iterator charIter;
  for (charIter = m_cw->begin(); charIter != m_cw->end(); charIter++)
  {
    glyphIter = m_gn->find(charIter->first);
    if (glyphIter != m_gn->end())
    {
      glyph = glyphIter->second;
    }
    else
    {
      glyph = 0;
    }
    if (glyph != 0 && (!subset || usedGlyphs == NULL || 
                       (subset && SubsetSupported() && (usedGlyphs->Index(glyph) != wxNOT_FOUND))))
    {
      if (subset)
      {
        glyph = (*subsetGlyphs)[glyph];
      }
      // define a specific width for each individual CID
      s += wxString::Format(wxT("%u [%u] "), glyph, charIter->second);
    }
  }
  s += wxString(wxT("]"));
  return s;
}

size_t
wxPdfFontDataOpenTypeUnicode::WriteFontData(wxOutputStream* fontData, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs)
{
#if defined(__WXMAC__)
#if wxPDFMACOSX_HAS_CORE_TEXT
  wxCFRef<CFDataRef> tableRef;
#endif
#endif
  size_t fontSize1 = 0;
  wxFSFile* fontFile = NULL;
  wxInputStream* fontStream = NULL;
  bool compressed = false;
  wxFileName fileName;
  if (m_fontFileName.IsEmpty())
  {
#if defined(__WXMSW__)
    if (m_file.IsEmpty() && m_font.IsOk())
    {
      fontStream = wxPdfFontParserTrueType::LoadTrueTypeFontStream(m_font);
    }
    else
#elif defined(__WXMAC__)
#if wxPDFMACOSX_HAS_CORE_TEXT
    if (m_file.IsEmpty() && m_font.IsOk())
    {
#if wxCHECK_VERSION(2,9,0)
      // wxWidgets 2.9.x or higher
      CTFontRef fontRef = m_font.OSXGetCTFont();
#else // wxWidgets 2.8.x
      CTFontRef fontRef = (const void*) m_font.MacGetCTFont();
#endif
      tableRef.reset(CTFontCopyTable(fontRef, kCTFontTableCFF, 0));
      const UInt8* tableData = CFDataGetBytePtr(tableRef);
      CFIndex      tableLen  = CFDataGetLength(tableRef);
      fontStream = new wxMemoryInputStream((const char*) tableData, (size_t) tableLen);
    }
    else
#endif
#endif
    {
      // Font data preprocessed by MakeFont
      compressed = m_file.Lower().Right(2) == wxT(".z");
      fileName = m_file;
      fileName.MakeAbsolute(m_path);
    }
  }
  else
  {
    fileName = m_fontFileName;
  }

  if (fileName.IsOk())
  {
    // Open font file
    wxFileSystem fs;
    fontFile = fs.OpenFile(wxFileSystem::FileNameToURL(fileName));
    if (fontFile)
    {
      fontStream = fontFile->GetStream();
    }
    else
    {
      // usually this should not happen since file accessability was already checked
      wxLogError(wxString(wxT("wxPdfFontDataOpenTypeUnicode::WriteFontData: ")) +
                 wxString::Format(_("Font file '%s' not found."), fileName.GetFullPath().c_str()));
    }
  }
  
  if (fontStream != NULL)
  {
    if (usedGlyphs != NULL)
    {
      if (compressed)
      {
        // Uncompress font file
        wxZlibInputStream zCompressed(*fontStream);
        wxMemoryOutputStream zUncompressed;
        zUncompressed.Write(zCompressed);
        zUncompressed.Close();
        fontStream = new wxMemoryInputStream(zUncompressed);
      }
      else
      {
        // Extract CFF stream from font file
        char* buffer = new char[m_cffLength];
        fontStream->SeekI(m_cffOffset);
        fontStream->Read(buffer, m_cffLength);
        wxMemoryOutputStream cffStream;
        cffStream.Write(buffer, m_cffLength);
        cffStream.Close();
        delete [] buffer;
        fontStream = new wxMemoryInputStream(cffStream);
      }

      // Assemble subset
      wxPdfFontSubsetCff subset(fileName.GetFullPath());
      wxMemoryOutputStream* subsetStream = subset.CreateSubset(fontStream, subsetGlyphs, false);
      if (fontStream != NULL)
      {
        delete fontStream;
      }

      // Write font subset data
      wxZlibOutputStream zFontData(*fontData);
      wxMemoryInputStream tmp(*subsetStream);
      fontSize1 = tmp.GetSize();
      zFontData.Write(tmp);
      zFontData.Close();
      delete subsetStream;
    }
    else
    {
      if (!compressed)
      {
        // Extract CFF stream from font file
        char* buffer = new char[m_cffLength];
        fontStream->SeekI(m_cffOffset);
        fontStream->Read(buffer, m_cffLength);
        wxZlibOutputStream zFontData(*fontData);
        zFontData.Write(buffer, m_cffLength);
        zFontData.Close();
        delete [] buffer;
      }
      else
      {
        fontSize1 = GetSize1();
        fontData->Write(*fontStream);
      }
    }
  }

  if (fontFile != NULL)
  {
    delete fontFile;
  }

  return fontSize1;
}

size_t
wxPdfFontDataOpenTypeUnicode::WriteUnicodeMap(wxOutputStream* mapData, 
                                              const wxPdfEncoding* encoding, 
                                              wxPdfSortedArrayInt* usedGlyphs, 
                                              wxPdfChar2GlyphMap* subsetGlyphs)
{
  wxUnusedVar(encoding);
  wxPdfGlyphList glyphList(wxPdfFontData::CompareGlyphListEntries);
  wxPdfChar2GlyphMap::const_iterator charIter = m_gn->begin();
  for (charIter = m_gn->begin(); charIter != m_gn->end(); ++charIter)
  {
    if (usedGlyphs != NULL)
    {
      int usedGlyphIndex = usedGlyphs->Index(charIter->second);
      if (usedGlyphIndex != wxNOT_FOUND)
      {
        wxPdfGlyphListEntry* glEntry = new wxPdfGlyphListEntry();
        if (subsetGlyphs != NULL)
        {
          glEntry->m_gid = (*subsetGlyphs)[charIter->second];
          charIter->second;
        }
        else
        {
          glEntry->m_gid = charIter->second;
        }
        glEntry->m_uid = charIter->first;
        glyphList.Add(glEntry);
      }
    }
    else
    {
      wxPdfGlyphListEntry* glEntry = new wxPdfGlyphListEntry();
      glEntry->m_gid = charIter->second;
      glEntry->m_uid = charIter->first;
      glyphList.Add(glEntry);
    }
  }
  wxMemoryOutputStream toUnicode;
  WriteToUnicode(glyphList, toUnicode);
  wxMemoryInputStream inUnicode(toUnicode);
  wxZlibOutputStream zUnicodeMap(*mapData);
  zUnicodeMap.Write(inUnicode);
  zUnicodeMap.Close();

  WX_CLEAR_ARRAY(glyphList);

  return 0;
}

#endif // wxUSE_UNICODE

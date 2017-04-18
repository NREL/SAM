///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontdata.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-07
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontdata.cpp Implementation of wxPdfFontData class

// For compilers that support precompilation, includes <wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

// includes
#include <wx/tokenzr.h>

#include "wex/pdf/pdfencoding.h"
#include "wex/pdf/pdffontdata.h"
#include "wex/pdf/pdffont.h"

#include "wxmemdbg.h"

wxString
wxPdfFontData::GetNodeContent(const wxXmlNode *node)
{
  const wxXmlNode *n = node;
  if (n == NULL) return wxEmptyString;
   n = n->GetChildren();

  while (n)
  {
    if (n->GetType() == wxXML_TEXT_NODE ||
        n->GetType() == wxXML_CDATA_SECTION_NODE)
      return n->GetContent();
    n = n->GetNext();
  }

  return wxEmptyString;
}

bool
wxPdfFontData::GetFontDescription(const wxXmlNode *node, wxPdfFontDescription& fontDescription)
{
  bool bAscent             = false,
       bDescent            = false,
       bCapheight          = false,
       bFlags              = false,
       bFontbbox           = false,
       bItalicangle        = false,
       bStemv              = false,
       bMissingwidth       = false,
       bXHeight            = false,
       bUnderlinePosition  = false,
       bUnderlineThickness = false;
  wxString value;
  long number;
  wxXmlNode* child = node->GetChildren();
  while (child)
  {
    // parse the children
    if (child->GetName() == wxT("ascent"))
    {
      value = GetNodeContent(child);
      if (value.ToLong(&number))
      {
        bAscent = true;
        fontDescription.SetAscent(number);
      }
    }
    else if (child->GetName() == wxT("descent"))
    {
      value = GetNodeContent(child);
      if (value.ToLong(&number))
      {
        bDescent = true;
        fontDescription.SetDescent(number);
      }
    }
    else if (child->GetName() == wxT("cap-height"))
    {
      value = GetNodeContent(child);
      if (value.ToLong(&number))
      {
        bCapheight = true;
        fontDescription.SetCapHeight(number);
      }
    }
    else if (child->GetName() == wxT("flags"))
    {
      value = GetNodeContent(child);
      if (value.ToLong(&number))
      {
        bFlags = true;
        fontDescription.SetFlags(number);
      }
    }
    else if (child->GetName() == wxT("font-bbox"))
    {
      value = GetNodeContent(child);
      if (value.Length() > 0 && value[0] == wxT('[') && value.Last() == wxT(']'))
      {
        bFontbbox = true;
        fontDescription.SetFontBBox(value);
      }
    }
    else if (child->GetName() == wxT("italic-angle"))
    {
      value = GetNodeContent(child);
      if (value.ToLong(&number))
      {
        bItalicangle = true;
        fontDescription.SetItalicAngle(number);
      }
    }
    else if (child->GetName() == wxT("stem-v"))
    {
      value = GetNodeContent(child);
      if (value.ToLong(&number))
      {
        bStemv = true;
        fontDescription.SetStemV(number);
      }
    }
    else if (child->GetName() == wxT("missing-width"))
    {
      value = GetNodeContent(child);
      if (value.ToLong(&number))
      {
        bMissingwidth = true;
        fontDescription.SetMissingWidth(number);
      }
    }
    else if (child->GetName() == wxT("x-height"))
    {
      value = GetNodeContent(child);
      if (value.ToLong(&number))
      {
        bXHeight = true;
        fontDescription.SetXHeight(number);
      }
    }
    else if (child->GetName() == wxT("underline-position"))
    {
      value = GetNodeContent(child);
      if (value.ToLong(&number))
      {
        bUnderlinePosition = true;
        fontDescription.SetUnderlinePosition(number);
      }
    }
    else if (child->GetName() == wxT("underline-thickness"))
    {
      value = GetNodeContent(child);
      if (value.ToLong(&number))
      {
        bUnderlineThickness = true;
        fontDescription.SetUnderlineThickness(number);
      }
    }
    child = child->GetNext();
  }
  return (bAscent && bDescent && bCapheight && bFlags && bFontbbox &&
          bItalicangle && bStemv && bMissingwidth && bXHeight &&
          bUnderlinePosition && bUnderlineThickness);
}

wxPdfFontData::wxPdfFontData()
{
  m_refCount = 0;

  m_type   = wxT("unknown");
  m_family = wxEmptyString;
  m_alias  = wxEmptyString;
  m_name   = wxEmptyString;

  m_fontFileName = wxEmptyString;
  m_fontIndex = 0;

  m_cff = false;
  m_cffOffset = 0;
  m_cffLength = 0;

  m_cw = NULL;
  m_gn = NULL;
  m_kp = NULL;

  m_enc   = wxEmptyString;
  m_diffs = wxEmptyString;
  m_file  = wxEmptyString;
  m_ctg   = wxEmptyString;
  m_size1 = 0;
  m_size2 = 0;

  m_initialized = false;
  m_embedRequired = false;
  m_embedSupported = false;
  m_subsetSupported = false;

  m_encoding = NULL;
  m_encodingChecker = NULL;
}

wxPdfFontData::~wxPdfFontData()
{
  if (m_kp != NULL)
  {
    wxPdfKernPairMap::iterator kp;
    for (kp = m_kp->begin(); kp != m_kp->end(); kp++)
    {
      if (kp->second != NULL)
      {
        delete kp->second;
      }
    }
    delete m_kp;
  }
  if (m_gn != NULL)
  {
    delete m_gn;
  }
  delete m_cw;
}

void
wxPdfFontData::SetEncoding(wxPdfEncoding* encoding)
{
  m_encoding = encoding;
}

const wxPdfChar2GlyphMap*
wxPdfFontData::FindEncodingMap(const wxPdfEncoding* encoding) const
{
  const wxPdfChar2GlyphMap* convMap = NULL;
  if (encoding != NULL)
  {
    convMap = encoding->GetEncodingMap();
  }
  if (convMap == NULL && m_encoding != NULL)
  {
    convMap = m_encoding->GetEncodingMap();
  }
  return convMap;
}

void
wxPdfFontData::SetEncodingChecker(wxPdfEncodingChecker* encodingChecker)
{
  m_encodingChecker = encodingChecker;
}

wxPdfEncodingChecker*
wxPdfFontData::GetEncodingChecker() const
{
  return m_encodingChecker;
}

void
wxPdfFontData::SetType(const wxString& type)
{ 
  m_type = type;
}

wxString
wxPdfFontData::GetType() const
{
  return m_type;
}

void
wxPdfFontData::SetFamily(const wxString& family)
{
  m_family = family;
}

wxString
wxPdfFontData::GetFamily() const
{
  wxString family = m_family;
  if (family.IsEmpty())
  {
    family = (!m_alias.IsEmpty()) ? m_alias : m_name;
  }
  return family;
}

void
wxPdfFontData::SetAlias(const wxString& alias)
{
  m_alias = alias;
}

wxString
wxPdfFontData::GetAlias() const
{
  return m_alias;
}

void
wxPdfFontData::SetName(const wxString& name)
{
  m_name = name;
}

wxString
wxPdfFontData::GetName() const
{
  return m_name;
}

void
wxPdfFontData::SetFullNames(const wxArrayString& fullNames)
{
  m_fullNames = fullNames;
}

wxArrayString
wxPdfFontData::GetFullNames() const
{
  return m_fullNames;
}

void
wxPdfFontData::SetStyle(const wxString& style)
{
  wxString lcStyle = style.Lower();
  bool italic = (lcStyle.Find(wxT("italic"))  != wxNOT_FOUND) || 
                (lcStyle.Find(wxT("oblique")) != wxNOT_FOUND) ||
                lcStyle.IsSameAs(wxT("i")) || lcStyle.IsSameAs(wxT("bi")) || lcStyle.IsSameAs(wxT("ib"));
  bool bold = (lcStyle.Find(wxT("bold"))  != wxNOT_FOUND) || 
              (lcStyle.Find(wxT("black")) != wxNOT_FOUND) ||
              lcStyle.IsSameAs(wxT("b")) || lcStyle.IsSameAs(wxT("bi")) || lcStyle.IsSameAs(wxT("ib"));
  m_style = wxPDF_FONTSTYLE_REGULAR;
  if (bold)
  {
    m_style |= wxPDF_FONTSTYLE_BOLD;
  }
  if (italic)
  {
    m_style |= wxPDF_FONTSTYLE_ITALIC;
  }
}

void
wxPdfFontData::SetStyle(int style)
{
  m_style = (style & ~wxPDF_FONTSTYLE_DECORATION_MASK) & wxPDF_FONTSTYLE_MASK;
}

void
wxPdfFontData::SetStyleFromName()
{
  SetStyle(m_name);
}

int
wxPdfFontData::GetStyle() const
{ 
  return m_style;
}

void
wxPdfFontData::SetUnderlinePosition(int up)
{
  m_desc.SetUnderlinePosition(up);
}

int
wxPdfFontData::GetUnderlinePosition() const
{ 
  return m_desc.GetUnderlinePosition();
}

void
wxPdfFontData::SetUnderlineThickness(int ut)
{ 
  m_desc.SetUnderlineThickness(ut);
}

int
wxPdfFontData::GetUnderlineThickness() const
{ 
  return m_desc.GetUnderlineThickness();
}

int
wxPdfFontData::GetBBoxTopPosition() const
{
  long top = 1000;
  wxString bBox = m_desc.GetFontBBox();
  wxStringTokenizer tkz(bBox, wxT(" []"), wxTOKEN_STRTOK);
  if (tkz.CountTokens() >= 4)
  {
    tkz.GetNextToken();
    tkz.GetNextToken();
    tkz.GetNextToken();
    wxString topToken = tkz.GetNextToken();
    topToken.ToLong(&top);
  }
  return top;
}

void
wxPdfFontData::SetEncoding(const wxString& enc)
{
  m_enc = enc;
}

wxString
wxPdfFontData::GetEncoding() const
{
  return m_enc;
}

const wxPdfEncoding*
wxPdfFontData::GetBaseEncoding() const
{
  return m_encoding;
}

bool
wxPdfFontData::HasDiffs() const
{
  return m_diffs.Length() > 0;
}

void
wxPdfFontData::SetDiffs(const wxString& diffs)
{
  m_diffs = diffs;
}

wxString
wxPdfFontData::GetDiffs() const
{
  return m_diffs;
}

void
wxPdfFontData::SetFilePath(const wxString& path)
{
  m_path = path;
}

wxString
wxPdfFontData::GetFilePath() const
{
  return m_path;
}

bool
wxPdfFontData::HasFile() const
{
  return m_file.Length() > 0;
}

void
wxPdfFontData::SetFontFile(const wxString& file)
{
  m_file = file;
}

wxString
wxPdfFontData::GetFontFile() const
{
  return m_file;
}

void
wxPdfFontData::SetCtgFile(const wxString& ctg)
{
  m_ctg = ctg;
}

wxString
wxPdfFontData::GetCtgFile() const
{
  return m_ctg;
}

void
wxPdfFontData::SetSize1(size_t size1)
{
  m_size1 = size1;
}

size_t
wxPdfFontData::GetSize1() const
{
  return m_size1;
}

bool
wxPdfFontData::HasSize2() const
{
  return m_size2 > 0;
}

void
wxPdfFontData::SetSize2(size_t size2)
{
  m_size2 = size2;
}

size_t
wxPdfFontData::GetSize2() const
{
  return m_size2;
}

wxString
wxPdfFontData::GetCMap() const
{
  return m_cmap;
}

void
wxPdfFontData::SetCMap(const wxString& cmap)
{
  m_cmap = cmap;
}

wxString
wxPdfFontData::GetOrdering() const
{
  return m_ordering;
}

void
wxPdfFontData::SetOrdering(const wxString& ordering)
{
  m_ordering = ordering;
}

wxString
wxPdfFontData::GetSupplement() const
{
  return m_supplement;
}

void
wxPdfFontData::SetSupplement(const wxString& supplement)
{
  m_supplement = supplement;
}

void
wxPdfFontData::SetGlyphWidthMap(wxPdfGlyphWidthMap* cw)
{
  m_cw = cw;
}

const wxPdfGlyphWidthMap*
wxPdfFontData::GetGlyphWidthMap() const
{
  return m_cw;
}

void
wxPdfFontData::SetChar2GlyphMap(wxPdfChar2GlyphMap* gn)
{
  m_gn = gn;
}

const wxPdfChar2GlyphMap*
wxPdfFontData::GetChar2GlyphMap() const
{
  return m_gn;
}

void
wxPdfFontData::SetKernPairMap(wxPdfKernPairMap* kp)
{
  m_kp = kp;
}

const wxPdfKernPairMap*
wxPdfFontData::GetKernPairMap() const
{
  return m_kp;
}

int
wxPdfFontData::GetKerningWidth(const wxString& s) const
{
  bool translateChar2Glyph = m_type.IsSameAs(wxT("TrueTypeUnicode")) || 
                             m_type.IsSameAs(wxT("OpenTypeUnicode"));
  int width = 0;
  if (m_kp != NULL && s.length())
  {
    wxPdfKernPairMap::const_iterator kpIter;
    wxPdfKernWidthMap::const_iterator kwIter;
    wxUint32 ch1, ch2;
    wxString::const_iterator ch = s.begin();
    ch1 = (wxUint32) (*ch);
    if (translateChar2Glyph && m_gn != NULL)
    {
      wxPdfChar2GlyphMap::const_iterator glyphIter;
      glyphIter = m_gn->find(ch1);
      if (glyphIter != m_gn->end())
      {
        ch1 = glyphIter->second;
      }
    }
    for (++ch; ch != s.end(); ++ch)
    {
      ch2 = (wxUint32) (*ch);
      if (translateChar2Glyph && m_gn != NULL)
      {
        wxPdfChar2GlyphMap::const_iterator glyphIter;
        glyphIter = m_gn->find(ch2);
        if (glyphIter != m_gn->end())
        {
          ch2 = glyphIter->second;
        }
      }
      kpIter = (*m_kp).find(ch1);
      if (kpIter != (*m_kp).end())
      {
        kwIter = kpIter->second->find(ch2);
        if (kwIter != kpIter->second->end())
        {
          width += kwIter->second;
        }
      }
      ch1 = ch2;
    }
  }
  return width;
}

wxArrayInt
wxPdfFontData::GetKerningWidthArray(const wxString& s) const
{
  bool translateChar2Glyph = m_type.IsSameAs(wxT("TrueTypeUnicode")) || 
                             m_type.IsSameAs(wxT("OpenTypeUnicode"));
  wxArrayInt widths;
  int pos = 0;
  if (m_kp != NULL && s.length())
  {
    wxPdfKernPairMap::const_iterator kpIter;
    wxPdfKernWidthMap::const_iterator kwIter;
    wxUint32 ch1, ch2;
    wxString::const_iterator ch = s.begin();
    ch1 = (wxUint32) (*ch);
    if (translateChar2Glyph && m_gn != NULL)
    {
      wxPdfChar2GlyphMap::const_iterator glyphIter;
      glyphIter = m_gn->find(ch1);
      if (glyphIter != m_gn->end())
      {
        ch1 = glyphIter->second;
      }
    }
    for (++ch; ch != s.end(); ++ch)
    {
      ch2 = (wxUint32) (*ch);
      if (translateChar2Glyph && m_gn != NULL)
      {
        wxPdfChar2GlyphMap::const_iterator glyphIter;
        glyphIter = m_gn->find(ch2);
        if (glyphIter != m_gn->end())
        {
          ch2 = glyphIter->second;
        }
      }
      kpIter = (*m_kp).find(ch1);
      if (kpIter != (*m_kp).end())
      {
        kwIter = kpIter->second->find(ch2);
        if (kwIter != kpIter->second->end())
        {
          widths.Add(pos);
          widths.Add(-kwIter->second);
        }
      }
      ch1 = ch2;
      ++pos;
    }
  }
  return widths;
}

bool
wxPdfFontData::Initialize()
{
  return IsInitialized();
}

void
wxPdfFontData::SetInitialized(bool initialized)
{
  m_initialized = initialized;
}

wxString
wxPdfFontData::GetWidthsAsString(bool subset, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs) const
{
  wxUnusedVar(subset);
  wxUnusedVar(usedGlyphs);
  wxUnusedVar(subsetGlyphs);
  return wxEmptyString;
}
  
double
wxPdfFontData::GetStringWidth(const wxString& s, const wxPdfEncoding* encoding, bool withKerning) const
{
  wxUnusedVar(s);
  wxUnusedVar(encoding);
  wxUnusedVar(withKerning);
  return 0;
}

bool
wxPdfFontData::GetGlyphNames(wxArrayString& glyphNames) const
{
  wxUnusedVar(glyphNames);
  return false;
}

size_t
wxPdfFontData::WriteFontData(wxOutputStream* fontData, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs)
{
  wxUnusedVar(fontData);
  wxUnusedVar(usedGlyphs);
  wxUnusedVar(subsetGlyphs);
  return 0;
}

size_t
wxPdfFontData::WriteUnicodeMap(wxOutputStream* mapData, 
                               const wxPdfEncoding* encoding, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs)
{
  wxUnusedVar(mapData);
  wxUnusedVar(encoding);
  wxUnusedVar(usedGlyphs);
  wxUnusedVar(subsetGlyphs);
  return 0;
}

void
wxPdfFontData::SetDescription(const wxPdfFontDescription& desc)
{
  m_desc = desc;
}

const wxPdfFontDescription&
wxPdfFontData::GetDescription() const
{
  return m_desc;
}

bool
wxPdfFontData::LoadFontMetrics(wxXmlNode* root)
{
  wxUnusedVar(root);
  return false;
};

bool
wxPdfFontData::CanShow(const wxString& s, const wxPdfEncoding* encoding) const
{
  wxUnusedVar(encoding);
  bool canShow = true;
#if wxUSE_UNICODE
  wxMBConv* conv = GetEncodingConv();
  size_t len = conv->FromWChar(NULL, 0, s.wc_str(), s.length());
  canShow = (len != wxCONV_FAILED);
#endif
  return canShow;
}

wxString
wxPdfFontData::ConvertToValid(const wxString& s, wxChar replace) const
{
  wxString t;
  if (m_encodingChecker != NULL)
  {
    if (m_encodingChecker->IsIncluded((wxUint32) replace))
    {
      replace = wxT('?');
    }
    wxString::const_iterator ch = s.begin();
    for (ch = s.begin(); ch != s.end(); ++ch)
    {
      if (m_encodingChecker->IsIncluded((wxUint32) *ch))
      {
        t.Append(*ch);
      }
      else
      {
        t.Append(replace);
      }
    }
  }
  else
  {
    t = s;
  }
  return  t;
}

wxString
wxPdfFontData::ConvertCID2GID(const wxString& s,
                              const wxPdfEncoding* encoding,
                              wxPdfSortedArrayInt* usedGlyphs, 
                              wxPdfChar2GlyphMap* subsetGlyphs) const
{
  // No conversion from cid to gid
  wxUnusedVar(encoding);
  wxUnusedVar(usedGlyphs);
  wxUnusedVar(subsetGlyphs);
  return s;
}

wxString
wxPdfFontData::ConvertGlyph(wxUint32 glyph, 
                            const wxPdfEncoding* encoding, 
                            wxPdfSortedArrayInt* usedGlyphs, 
                            wxPdfChar2GlyphMap* subsetGlyphs) const
{
  wxUnusedVar(glyph);
  wxUnusedVar(encoding);
  wxUnusedVar(usedGlyphs);
  wxUnusedVar(subsetGlyphs);
  return wxEmptyString;
}

#if wxUSE_UNICODE
wxMBConv*
wxPdfFontData::GetEncodingConv() const
{
  return &wxConvISO8859_1;
}

void
wxPdfFontData::CreateDefaultEncodingConv()
{
}
#endif

void
wxPdfFontData::SetGlyphWidths(const wxPdfArrayUint16& glyphWidths)
{
  wxUnusedVar(glyphWidths);
}

int
wxPdfFontData::FindStyleFromName(const wxString& name)
{
  int style = wxPDF_FONTSTYLE_REGULAR;
  wxString lcName = name.Lower();
  if (lcName.Find(wxT("bold")) != wxNOT_FOUND)
  {
    style |= wxPDF_FONTSTYLE_BOLD;
  }
  if (lcName.Find(wxT("italic")) != wxNOT_FOUND || lcName.Find(wxT("oblique")) != wxNOT_FOUND)
  {
    style |= wxPDF_FONTSTYLE_ITALIC;
  }
  return style;
}

int
wxPdfFontData::CompareGlyphListEntries(wxPdfGlyphListEntry* item1, wxPdfGlyphListEntry* item2)
{
  return item1->m_gid - item2->m_gid;
}

void
wxPdfFontData::WriteStreamBuffer(wxOutputStream& stream, const char* buffer)
{
  size_t buflen = strlen(buffer);
  stream.Write(buffer, buflen);
}

void
wxPdfFontData::WriteToUnicode(wxPdfGlyphList& glyphs, wxMemoryOutputStream& toUnicode, bool simple)
{
  wxString gidFormat = (simple) ? wxString(wxT("<%02x>")) : wxString(wxT("<%04x>"));
  WriteStreamBuffer(toUnicode, "/CIDInit /ProcSet findresource begin\n");
  WriteStreamBuffer(toUnicode, "12 dict begin\n");
  WriteStreamBuffer(toUnicode, "begincmap\n");
  WriteStreamBuffer(toUnicode, "/CIDSystemInfo\n");
  WriteStreamBuffer(toUnicode, "<< /Registry (Adobe)\n");
  WriteStreamBuffer(toUnicode, "/Ordering (UCS)\n");
  WriteStreamBuffer(toUnicode, "/Supplement 0\n");
  WriteStreamBuffer(toUnicode, ">> def\n");
  WriteStreamBuffer(toUnicode, "/CMapName /Adobe-Identity-UCS def\n");
  WriteStreamBuffer(toUnicode, "/CMapType 2 def\n");
  WriteStreamBuffer(toUnicode, "1 begincodespacerange\n");
  if (simple)
  {
    WriteStreamBuffer(toUnicode, "<00><FF>\n");
  }
  else
  {
    WriteStreamBuffer(toUnicode, "<0000><FFFF>\n");
  }
  WriteStreamBuffer(toUnicode, "endcodespacerange\n");
  unsigned int size = 0;
  unsigned int k;
  unsigned int numGlyphs = (unsigned int) glyphs.GetCount();
  for (k = 0; k < numGlyphs; ++k)
  {
    if (size == 0)
    {
      if (k != 0)
      {
        WriteStreamBuffer(toUnicode, "endbfrange\n");
      }
      size = (numGlyphs-k > 100) ? 100 : numGlyphs - k;
      wxString sizeStr = wxString::Format(wxT("%u"), size);
      WriteStreamBuffer(toUnicode, sizeStr.ToAscii());
      WriteStreamBuffer(toUnicode, " beginbfrange\n");
    }
    size--;
    wxPdfGlyphListEntry* entry = glyphs[k];
    wxString fromTo = wxString::Format(gidFormat, entry->m_gid);
    wxString uniChr = wxString::Format(wxT("<%04x>"), entry->m_uid);
    WriteStreamBuffer(toUnicode, fromTo.ToAscii());
    WriteStreamBuffer(toUnicode, fromTo.ToAscii());
    WriteStreamBuffer(toUnicode, uniChr.ToAscii());
    WriteStreamBuffer(toUnicode, "\n");
  }
  WriteStreamBuffer(toUnicode, "endbfrange\n");
  WriteStreamBuffer(toUnicode, "endcmap\n");
  WriteStreamBuffer(toUnicode, "CMapName currentdict /CMap defineresource pop\n");
  WriteStreamBuffer(toUnicode, "end end\n");
}

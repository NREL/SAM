///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontsubsettruetype.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2006-11-20
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontsubsettruetype.cpp Implementation of subset support for TrueType fonts

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
#include "wex/pdf/pdffontsubsettruetype.h"

#include "wxmemdbg.h"

wxPdfFontSubsetTrueType::wxPdfFontSubsetTrueType(const wxString& fileName, 
                                                 int fontIndex, bool isMacCoreText)
  : wxPdfFontParserTrueType()

{
  m_fileName = fileName;
  m_fontIndex = fontIndex;
  m_includeCmap = false;
  m_isMacCoreText = isMacCoreText;
}

wxPdfFontSubsetTrueType::~wxPdfFontSubsetTrueType()
{
  delete [] m_newGlyfTable;
  delete [] m_newLocaTableStream;
  delete [] m_newLocaTable;
  delete [] m_locaTable;
}

#if defined(__WXMAC__)
#if wxPDFMACOSX_HAS_CORE_TEXT

void
wxPdfFontSubsetTrueType::SetCTFontRef(const wxFont& font)
{
#if wxCHECK_VERSION(2,9,0)
  m_fontRef = font.OSXGetCTFont();
#else // wxWidgets 2.8.x
  m_fontRef = (const void*) font.MacGetCTFont();
#endif
}

#endif
#endif

wxMemoryOutputStream*
wxPdfFontSubsetTrueType::CreateSubset(wxInputStream* inFont, wxPdfSortedArrayInt* usedGlyphs, bool includeCmap)
{
  m_inFont = inFont;
  m_usedGlyphs = usedGlyphs;
  m_includeCmap = includeCmap;
  m_outFont = NULL;

  m_inFont->SeekI(0);
  m_directoryOffset = 0;
  wxString mainTag = ReadString(4);
  if (mainTag == wxT("ttcf"))
  {
    SkipBytes(4);
    int dirCount = ReadInt();
    if (m_fontIndex < dirCount)
    {
      m_fontIndex = 0;
    }
    SkipBytes(m_fontIndex * 4);
    m_directoryOffset = ReadInt();
  }

  if (ReadTableDirectory())
  {
    if (ReadLocaTable())
    {
      if (CheckGlyphs())
      {
        CreateNewTables();
        WriteSubsetFont();
      }
    }
  }
  return m_outFont;
}

static const int HEAD_LOCA_FORMAT_OFFSET = 51;

bool
wxPdfFontSubsetTrueType::ReadLocaTable()
{
  bool ok = false;

  wxPdfTableDirectoryEntry* tableLocation;
  wxPdfTableDirectory::iterator entry = m_tableDirectory->find(wxT("head"));
  if (entry != m_tableDirectory->end())
  {
    tableLocation = entry->second;
    LockTable(wxT("head"));
    m_inFont->SeekI(tableLocation->m_offset + HEAD_LOCA_FORMAT_OFFSET);
    m_locaTableIsShort = (ReadUShort() == 0);
    ReleaseTable();

    entry = m_tableDirectory->find(wxT("loca"));
    if (entry != m_tableDirectory->end())
    {
      tableLocation = entry->second;
      LockTable(wxT("loca"));
      m_inFont->SeekI(tableLocation->m_offset);
      m_locaTableSize = (m_locaTableIsShort) ? tableLocation->m_length / 2 : tableLocation->m_length / 4;
      m_locaTable = new int[m_locaTableSize];
      size_t k;
      for (k = 0; k < m_locaTableSize; k++)
      {
        m_locaTable[k] = (m_locaTableIsShort) ? ReadUShort() * 2 : ReadInt();
      }
      ok = true;
      ReleaseTable();
    }
    else
    {
      wxLogError(wxString(wxT("wxPdfFontSubsetTrueType::ReadLocaTable: ")) +
                 wxString::Format(_("Table 'loca' does not exist in '%s'."), m_fileName.c_str()));
    }
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfFontSubsetTrueType::ReadLocaTable: ")) +
               wxString::Format(_("Table 'head' does not exist in '%s'."), m_fileName.c_str()));
  }
  return ok;
}
    
bool
wxPdfFontSubsetTrueType::CheckGlyphs()
{
  bool ok = false;
  wxPdfTableDirectoryEntry* tableLocation;
  wxPdfTableDirectory::iterator entry = m_tableDirectory->find(wxT("glyf"));
  if (entry != m_tableDirectory->end())
  {
    tableLocation = entry->second;
    LockTable(wxT("glyf"));
    int glyph0 = 0;
    if (m_usedGlyphs->Index(glyph0) == wxNOT_FOUND)
    {
      m_usedGlyphs->Add(glyph0);
    }
    m_glyfTableOffset = tableLocation->m_offset;
    size_t k;
    for (k = 0; k < m_usedGlyphs->GetCount(); k++)
    {
      FindGlyphComponents(m_usedGlyphs->Item(k));
    }
    ok = true;
    ReleaseTable();
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfFontSubsetTrueType::CheckGlyphs: ")) +
               wxString::Format(_("Table 'glyf' does not exist in '%s'."), m_fileName.c_str()));
  }
  return ok;
}

static const int FLAG_HAS_WORD_ARGS       =   1;
static const int FLAG_HAS_SCALE           =   8;
static const int FLAG_HAS_MORE_COMPONENTS =  32;
static const int FLAG_HAS_SCALE_XY        =  64;
static const int FLAG_HAS_TWO_BY_TWO      = 128;

void
wxPdfFontSubsetTrueType::FindGlyphComponents(int glyph)
{

  int glyphOffset = m_locaTable[glyph];
  if (glyphOffset == m_locaTable[glyph + 1])
  {
    // glyph has no contour
    return;
  }
  m_inFont->SeekI(m_glyfTableOffset + glyphOffset);
  int numberContours = ReadShort();
  if (numberContours >= 0)
  {
    // glyph has contours (not glyph components)
    return;
  }
  SkipBytes(8);
  for (;;)
  {
    int flags = ReadUShort();
    int glyphComponent = (int) ReadUShort();
    if (m_usedGlyphs->Index(glyphComponent) == wxNOT_FOUND)
    {
      m_usedGlyphs->Add(glyphComponent);
    }
    if ((flags & FLAG_HAS_MORE_COMPONENTS) == 0)
    {
      // glyph does not have more components
      return;
    }
    int skip = ((flags & FLAG_HAS_WORD_ARGS) != 0) ? 4 : 2;
    if ((flags & FLAG_HAS_SCALE) != 0)
    {
      skip += 2;
    }
    else if ((flags & FLAG_HAS_SCALE_XY) != 0)
    {
      skip += 4;
    }
    if ((flags & FLAG_HAS_TWO_BY_TWO) != 0)
    {
      skip += 8;
    }
    SkipBytes(skip);
  }
}

void
wxPdfFontSubsetTrueType::CreateNewTables()
{
  size_t usedGlyphCount = m_usedGlyphs->GetCount();
  size_t k;
  m_newLocaTable = new int[m_locaTableSize];

  // Calculate new 'glyf' table size
  m_newGlyfTableSize = 0;
  for (k = 0; k < usedGlyphCount; k++)
  {
    int glyph = (*m_usedGlyphs)[k];
    m_newGlyfTableSize += m_locaTable[glyph + 1] - m_locaTable[glyph];
  }
  m_newGlyfTableRealSize = m_newGlyfTableSize;
  m_newGlyfTableSize =  (m_newGlyfTableSize + 3) & (~3);
  m_newGlyfTable = new char[m_newGlyfTableSize];

  // Initialize new 'glyf' table
  for (k = 0; k < m_newGlyfTableSize; k++)
  {
    m_newGlyfTable[k] = 0;
  }

  // Copy used glyphs to new 'glyf' table
  LockTable(wxT("glyf"));
  int newGlyphOffset = 0;
  size_t glyphIndex = 0;
  for (k = 0; k < m_locaTableSize; k++)
  {
    m_newLocaTable[k] = newGlyphOffset;
    if (glyphIndex < usedGlyphCount && (size_t)(*m_usedGlyphs)[glyphIndex] == k)
    {
      glyphIndex++;
      m_newLocaTable[k] = newGlyphOffset;
      int glyphOffset = m_locaTable[k];
      int glyphLength = m_locaTable[k + 1] - glyphOffset;
      if (glyphLength > 0)
      {
        m_inFont->SeekI(m_glyfTableOffset + glyphOffset);
        m_inFont->Read(&m_newGlyfTable[newGlyphOffset], glyphLength);
        newGlyphOffset += glyphLength;
      }
    }
  }
  ReleaseTable();

  // Convert new 'loca' table to byte stream
  m_locaTableRealSize = (m_locaTableIsShort) ? m_locaTableSize * 2 : m_locaTableSize * 4;
  m_newLocaTableStreamSize = (m_locaTableRealSize + 3) & (~3);
  m_newLocaTableStream = new char[m_newLocaTableStreamSize];
  for (k = 0; k < m_newLocaTableStreamSize; k++)
  {
    m_newLocaTableStream[k] = 0;
  }
  int offset = 0;
  for (k = 0; k < m_locaTableSize; k++)
  {
    if (m_locaTableIsShort)
    {
      WriteShortToBuffer(m_newLocaTable[k] / 2, &m_newLocaTableStream[offset]);
      offset += 2;
    }
    else
    {
      WriteIntToBuffer(m_newLocaTable[k], &m_newLocaTableStream[offset]);
      offset += 4;
    }
  }
}

static const wxChar* tableNamesDefault[] = {
  wxT("cvt "), wxT("fpgm"), wxT("glyf"), wxT("head"),
  wxT("hhea"), wxT("hmtx"), wxT("loca"), wxT("maxp"), wxT("prep"),
  NULL
};
static const wxChar* tableNamesCmap[] = {
  wxT("cmap"), wxT("cvt "), wxT("fpgm"), wxT("glyf"), wxT("head"),
  wxT("hhea"), wxT("hmtx"), wxT("loca"), wxT("maxp"), wxT("prep"),
  NULL
};
#if 0
static const wxChar* tableNamesExtra[] = {
  wxT("OS/2"), wxT("cmap"), wxT("cvt "), wxT("fpgm"), wxT("glyf"), wxT("head"),
  wxT("hhea"), wxT("hmtx"), wxT("loca"), wxT("maxp"), wxT("name"), wxT("prep"),
  NULL
};
#endif

//                                   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
static const int entrySelectors[] = {0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4};

void
wxPdfFontSubsetTrueType::WriteSubsetFont()
{
  wxPdfTableDirectoryEntry* tableLocation;
  wxPdfTableDirectory::iterator entry;
  int k;
  const wxChar** tableNames = (m_includeCmap) ? tableNamesCmap : tableNamesDefault;
  int tableCount = 0;
  while (tableNames[tableCount] != NULL)
  {
    tableCount++;
  }

  int tablesUsed = 2;
  int tableLength = 0;
  for (k = 0; k < tableCount; k++)
  {
    wxString name = tableNames[k];
    if (name != wxT("glyf") && name != wxT("loca"))
    {
      entry = m_tableDirectory->find(name);
      if (entry != m_tableDirectory->end())
      {
        tableLocation = entry->second;
        ++tablesUsed;
      }
    }
  }

  // Write header and table directory
  int tableOffset = 16 * tablesUsed + 12;
  m_outFont = new wxMemoryOutputStream();
  WriteInt(0x00010000);
  WriteShort(tablesUsed);

  int selector = entrySelectors[tablesUsed];
  WriteShort((1 << selector) * 16);
  WriteShort(selector);
  WriteShort((tablesUsed - (1 << selector)) * 16);
  for (k = 0; k < tableCount; ++k)
  {
    wxString name = tableNames[k];
    entry = m_tableDirectory->find(name);
    if (entry != m_tableDirectory->end())
    {
      tableLocation = entry->second;
      WriteString(name);
      if (name == wxT("glyf"))
      {
        WriteInt(CalculateChecksum(m_newGlyfTable, m_newGlyfTableSize));
        tableLength = (int) m_newGlyfTableRealSize;
      }
      else if (name == wxT("loca"))
      {
        WriteInt(CalculateChecksum(m_newLocaTableStream, m_newLocaTableStreamSize));
        tableLength = (int) m_locaTableRealSize;
      }
      else
      {
        WriteInt(tableLocation->m_checksum);
        tableLength = tableLocation->m_length;
      }
      WriteInt(tableOffset);
      WriteInt(tableLength);
      tableOffset += (tableLength + 3) & (~3);
    }
  }

  // Write new 'loca' and 'glyf' tables and copy all others
  for (k = 0; k < tableCount; k++)
  {
    wxString name = tableNames[k];
    entry = m_tableDirectory->find(name);
    if (entry != m_tableDirectory->end())
    {
      tableLocation = entry->second;
      if (name == wxT("glyf"))
      {
        m_outFont->Write(m_newGlyfTable, m_newGlyfTableSize);
      }
      else if (name == wxT("loca"))
      {
        m_outFont->Write(m_newLocaTableStream, m_newLocaTableStreamSize);
      }
      else
      {
        char buffer[1024];
        LockTable(name);
        m_inFont->SeekI(tableLocation->m_offset);
        tableLength = tableLocation->m_length;
        int bufferLength;
        while (tableLength > 0)
        {
          bufferLength = (tableLength > 1024) ? 1024 : tableLength;
          m_inFont->Read(buffer, bufferLength);
          m_outFont->Write(buffer, bufferLength);
          tableLength -= bufferLength;
        }
        int paddingLength = ((tableLocation->m_length + 3) & (~3)) - tableLocation->m_length;
        if (paddingLength > 0)
        {
          int pad;
          for (pad = 0; pad < paddingLength; pad++)
          {
            buffer[pad] = 0;
          }
          m_outFont->Write(buffer, paddingLength);
        }
        ReleaseTable();
      }
    }
  }
}
    
void
wxPdfFontSubsetTrueType::WriteShort(int n)
{
  char buffer[2];
  WriteShortToBuffer(n, buffer);
  m_outFont->Write(buffer, 2);
}

void
wxPdfFontSubsetTrueType::WriteShortToBuffer(int n, char buffer[2])
{
  buffer[0] = (char)((n >> 8) & 0xff);
  buffer[1] = (char)((n     ) & 0xff);
}

void
wxPdfFontSubsetTrueType::WriteInt(int n)
{
  char buffer[4];
  WriteIntToBuffer(n, buffer);
  m_outFont->Write(buffer, 4);
}

void
wxPdfFontSubsetTrueType::WriteIntToBuffer(int n, char buffer[4])
{
  buffer[0] = (char)((n >> 24) & 0xff);
  buffer[1] = (char)((n >> 16) & 0xff);
  buffer[2] = (char)((n >>  8) & 0xff);
  buffer[3] = (char)((n      ) & 0xff);
}

void
wxPdfFontSubsetTrueType::WriteString(const wxString& s)
{
  size_t len = s.Length();;
  char* buffer = new char[len];
  size_t j;
  for (j = 0; j < len; j++)
  {
    buffer[j] = s[j];
  }
  m_outFont->Write(buffer, len);
  delete [] buffer;
}

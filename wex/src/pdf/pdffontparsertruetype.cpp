///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontparsertruetype.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-03-04
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontparsertruetype.cpp Implementation of TrueType/OpenType font parsing support classes

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

#include "wex/pdf/pdffontdataopentype.h"
#include "wex/pdf/pdffontdatatruetype.h"
#include "wex/pdf/pdffontparsertruetype.h"
#include "wex/pdf/pdfencoding.h"

#include "wxmemdbg.h"

// --- Windows specific loading of TrueType font data

#ifdef __WXMSW__

#include <stdio.h>
#include <windows.h>

#include "wx/msw/private.h"

static USHORT
ReadUShort(BYTE* p)
{
  return ((USHORT)p[0] <<  8) + ((USHORT)p[1]);
}

static DWORD
ReadDWord(BYTE* p)
{
  return ((LONG)p[0] << 24) + ((LONG)p[1] << 16) + ((LONG)p[2] << 8) + ((LONG)p[3]);
}

static DWORD ReadTag(BYTE* p)
{
  return ((LONG)p[3] << 24) + ((LONG)p[2] << 16) + ((LONG)p[1] << 8) + ((LONG)p[0]);
}

static void
WriteDWord(BYTE* p, DWORD dw)
{
  p[0] = (BYTE)((dw >> 24 ) & 0xFF);
  p[1] = (BYTE)((dw >> 16 ) & 0xFF);
  p[2] = (BYTE)((dw >>  8 ) & 0xFF);
  p[3] = (BYTE)((dw       ) & 0xFF);
}

static DWORD
RoundUpToDWord(DWORD val)
{
  return (val + 3) & ~3;
}

#define TTC_FILE 0x66637474

const DWORD sizeOfFixedHeader     = 12;
const DWORD offsetOfTableCount    = 4;

const DWORD sizeOfTableEntry      = 16;
const DWORD offsetOfTableTag      = 0;
const DWORD offsetOfTableChecksum = 4;
const DWORD offsetOfTableOffset   = 8;
const DWORD offsetOfTableLength   = 12;

static bool
ExtractFontData(HDC hdc, DWORD& fontDataSize, BYTE*& fontData)
{
  bool ok = false;
  fontData = NULL;
  fontDataSize = 0;

  // Check if font is in TrueType collection
  if (GetFontData(hdc, TTC_FILE, 0, NULL, 0) != GDI_ERROR)
  {
    // Extract font data from TTC (TrueType Collection)
    // 1. Read number of tables in the font (ushort value at offset 2)
    USHORT nTables;
    BYTE uShortBuf[2];
    if (GetFontData(hdc, 0, 4, uShortBuf, 2) == GDI_ERROR)
    {
      wxLogError(wxString(wxT("ExtractFontData: ")) +
                 wxString::Format(_("Error %d on reading number of tables from TTC."), GetLastError()));
      return false;
    }
    nTables = ReadUShort(uShortBuf);
    
    // 2. Calculate memory needed for the whole font header and read it into buffer
    DWORD headerSize = sizeOfFixedHeader + nTables * sizeOfTableEntry;
    BYTE* fontHeader = new BYTE[headerSize];
    if (!fontHeader)
    {
      wxLogError(wxString(wxT("ExtractFontData: ")) +
                 wxString(_("Out of memory while extracting from TTC.")));
      return false;
    }
    if (GetFontData(hdc, 0, 0, fontHeader, headerSize) == GDI_ERROR)
    {
      delete [] fontHeader;
      wxLogError(wxString(wxT("ExtractFontData: ")) +
                 wxString::Format(_("Error %d on reading font header from TTC."), GetLastError()));
      return false;
    }
    
    // 3. Calculate total font size. 
    //    Tables are padded to 4-byte boundaries, so length should be rounded up to dword.
    DWORD bufferSize = headerSize;
    USHORT i;    
    for (i = 0; i < nTables; i++)
    {
      DWORD tableLength = ReadDWord(fontHeader + sizeOfFixedHeader + i * sizeOfTableEntry + offsetOfTableLength);
      if (i < nTables - 1)
      {
        bufferSize += RoundUpToDWord(tableLength);
      }
      else
      {
        bufferSize += tableLength;
      }
    }
    
    // 4. Copying header into target buffer.
    //    Patch offsets with correct values while copying data.
    BYTE* buffer = new BYTE[bufferSize];
    if (buffer == NULL)
    {
      delete [] fontHeader;
      wxLogError(wxString(wxT("ExtractFontData: ")) +
                 wxString(_("Out of memory while extracting from TTC.")));
      return false;
    }
    memcpy(buffer, fontHeader, headerSize);
    
    // 5. Get table data from GDI, write it into known place 
    //    inside target buffer and fix offset value.
    DWORD runningOffset = headerSize;
    
    for (i = 0; i < nTables; i++)
    {
      BYTE* entryData  = fontHeader + sizeOfFixedHeader + i * sizeOfTableEntry; 
      DWORD tableTag    = ReadTag(entryData + offsetOfTableTag);
      DWORD tableLength = ReadDWord(entryData + offsetOfTableLength);
        
      // Write new offset for this table.
      WriteDWord(buffer + sizeOfFixedHeader + i * sizeOfTableEntry + offsetOfTableOffset, runningOffset);

      // Get font data from GDI and place it into target buffer
      if (GetFontData(hdc, tableTag, 0, buffer + runningOffset, tableLength) == GDI_ERROR)
      {
        delete [] buffer;
        delete [] fontHeader;
        wxLogError(wxString(wxT("ExtractFontData: ")) +
                   wxString::Format(_("Error %d on reading table data from TTC."), GetLastError()));
        return false;
      }
      runningOffset += tableLength;

      // Pad tables (except last) with zero's
      if (i < nTables - 1)
      {
        while (runningOffset % 4 != 0)
        {
          buffer[runningOffset] = 0;
          ++runningOffset;
        } 
      }
    }
    delete [] fontHeader;
    fontDataSize = bufferSize;
    fontData     = buffer;
    ok = true;
  }
  else
  {
    // Check if font is TrueType
    DWORD bufferSize = GetFontData(hdc, 0, 0, NULL, 0);
    if (bufferSize != GDI_ERROR)
    {
      BYTE* buffer = new BYTE[bufferSize];
      if (buffer != NULL)
      {
        ok = (GetFontData(hdc, 0, 0, buffer, bufferSize) != GDI_ERROR);
        if (ok)
        {
          fontDataSize = bufferSize;
          fontData = buffer;
        }
        else
        {
          delete [] buffer;
          wxLogError(wxString(wxT("ExtractFontData: ")) +
                     wxString::Format(_("Error %d on extracting font data."), GetLastError()));
        }
      }
      else
      {
        wxLogError(wxString(wxT("ExtractFontData: ")) +
                   wxString(_("Out of memory.")));
      }
    }
  }
  return ok;
}

wxMemoryInputStream*
wxPdfFontParserTrueType::LoadTrueTypeFontStream(const wxFont& font)
{
  wxMemoryInputStream* fontStream = NULL;
  LOGFONT lf;
  wxFillLogFont(&lf, &font);

  HDC hdc = CreateCompatibleDC(0);
  HFONT hFont = CreateFontIndirect(&lf);
  /*HFONT oldfont = (HFONT)*/ SelectObject(hdc, hFont);

  DWORD fontDataSize;
  BYTE* fontData;
  bool ok = ExtractFontData(hdc, fontDataSize, fontData);
  if (ok)
  {
    wxMemoryOutputStream oStream;
    oStream.Write(fontData, fontDataSize);
    oStream.Close();
    fontStream = new wxMemoryInputStream(oStream);
    delete [] fontData;
  }
  DeleteDC(hdc);
  DeleteObject(hFont);

  return fontStream;
}

#endif

// --- Parsing of TrueType and OpenType

// The components of table 'head'.
class wxPdfFontHeader
{
public:
  int   m_flags;
  int   m_unitsPerEm;
  short m_xMin;
  short m_yMin;
  short m_xMax;
  short m_yMax;
  int   m_macStyle;
};

// The components of table 'hhea'.
class wxPdfHorizontalHeader
{
public:
  short m_ascender;
  short m_descender;
  short m_lineGap;
  int   m_advanceWidthMax;
  short m_minLeftSideBearing;
  short m_minRightSideBearing;
  short m_xMaxExtent;
  short m_caretSlopeRise;
  short m_caretSlopeRun;
  int   m_numberOfHMetrics;
};

// The components of table 'OS/2'.
class wxPdfWindowsMetrics
{
public:
  short m_xAvgCharWidth;
  int   m_usWeightClass;
  int   m_usWidthClass;
  short m_fsType;
  short m_ySubscriptXSize;
  short m_ySubscriptYSize;
  short m_ySubscriptXOffset;
  short m_ySubscriptYOffset;
  short m_ySuperscriptXSize;
  short m_ySuperscriptYSize;
  short m_ySuperscriptXOffset;
  short m_ySuperscriptYOffset;
  short m_yStrikeoutSize;
  short m_yStrikeoutPosition;
  short m_sFamilyClass;
  char  m_panose[10];
  char  m_achVendID[4];
  int   m_fsSelection;
  int   m_usFirstCharIndex;
  int   m_usLastCharIndex;
  short m_sTypoAscender;
  short m_sTypoDescender;
  short m_sTypoLineGap;
  int   m_usWinAscent;
  int   m_usWinDescent;
  int   m_ulCodePageRange1;
  int   m_ulCodePageRange2;
  int   m_sxHeight;
  int   m_sCapHeight;
};

#if 0
static const wxChar codePages[] =
{
  /*  0 */ "1252 Latin 1",                 // wxFONTENCODING_CP1252
  /*  1 */ "1250 Latin 2: Eastern Europe", // wxFONTENCODING_CP1250
  /*  2 */ "1251 Cyrillic",                // wxFONTENCODING_CP1251
  /*  3 */ "1253 Greek",                   // wxFONTENCODING_CP1253
  /*  4 */ "1254 Turkish",                 // wxFONTENCODING_CP1254
  /*  5 */ "1255 Hebrew",                  // wxFONTENCODING_CP1255
  /*  6 */ "1256 Arabic",                  // wxFONTENCODING_CP1256
  /*  7 */ "1257 Windows Baltic",          // wxFONTENCODING_CP1257
  /*  8 */ "1258 Vietnamese",              // 
  /*  9 */ null,
  /* 10 */ null,
  /* 11 */ null,
  /* 12 */ null,
  /* 13 */ null,
  /* 14 */ null,
  /* 15 */ null,
  /* 16 */ "874 Thai",                     // wxFONTENCODING_CP874
  /* 17 */ "932 JIS/Japan",                // wxFONTENCODING_CP932
  /* 18 */ "936 Chinese: Simplified",      // wxFONTENCODING_CP936
  /* 19 */ "949 Korean Wansung",           // wxFONTENCODING_CP949
  /* 20 */ "950 Chinese: Traditional",     // wxFONTENCODING_CP950
  /* 21 */ "1361 Korean Johab",            //
  /* 22 */ null,
  /* 23 */ null,
  /* 24 */ null,
  /* 25 */ null,
  /* 26 */ null,
  /* 27 */ null,
  /* 28 */ null,
  /* 29 */ "Macintosh Character Set (US Roman)", //
  /* 30 */ "OEM Character Set",                  //
  /* 31 */ "Symbol Character Set",               //
  /*  0 */ null,
  /*  1 */ null,
  /*  2 */ null,
  /*  3 */ null,
  /*  4 */ null,
  /*  5 */ null,
  /*  6 */ null,
  /*  7 */ null,
  /*  8 */ null,
  /*  9 */ null,
  /* 10 */ null,
  /* 11 */ null,
  /* 12 */ null,
  /* 13 */ null,
  /* 14 */ null,
  /* 15 */ null,
  /* 16 */ "869 IBM Greek",                   //
  /* 17 */ "866 MS-DOS Russian",              // wxFONTENCODING_CP866
  /* 18 */ "865 MS-DOS Nordic",               // 
  /* 19 */ "864 Arabic",                      // 
  /* 20 */ "863 MS-DOS Canadian French",      // 
  /* 21 */ "862 Hebrew",                      // 
  /* 22 */ "861 MS-DOS Icelandic",            // 
  /* 23 */ "860 MS-DOS Portuguese",           // 
  /* 24 */ "857 IBM Turkish",                 // 
  /* 25 */ "855 IBM Cyrillic",                // wxFONTENCODING_CP855
  /* 26 */ "852 Latin 2",                     // wxFONTENCODING_CP852
  /* 27 */ "775 MS-DOS Baltic",               // 
  /* 28 */ "737 Greek; former 437 G",         // 
  /* 29 */ "708 Arabic; ASMO 708",            // 
  /* 30 */ "850 WE/Latin 1",                  // wxFONTENCODING_CP850
  /* 31 */ "437 US"                           // wxFONTENCODING_CP437
};
#endif

wxPdfFontParserTrueType::wxPdfFontParserTrueType()
  : wxPdfFontParser()
{
  m_tableDirectory = new wxPdfTableDirectory();
  m_isFixedPitch = false;
  m_cmap10 = NULL;
  m_cmap31 = NULL;
  m_cmapExt = NULL;
  m_kp = NULL;
  m_isMacCoreText = false;
  m_savedStream = NULL;
}

wxPdfFontParserTrueType::~wxPdfFontParserTrueType()
{
  wxPdfCMap::iterator cMapIter;
  if (m_cmap10 != NULL)
  {
    for (cMapIter = m_cmap10->begin(); cMapIter != m_cmap10->end(); cMapIter++)
    {
      if (cMapIter->second != NULL)
      {
        delete cMapIter->second;
      }
    }
    delete m_cmap10;
  }
  if (m_cmap31 != NULL)
  {
    for (cMapIter = m_cmap31->begin(); cMapIter != m_cmap31->end(); cMapIter++)
    {
      if (cMapIter->second != NULL)
      {
        delete cMapIter->second;
      }
    }
    delete m_cmap31;
  }
  if (m_cmapExt != NULL)
  {
    for (cMapIter = m_cmapExt->begin(); cMapIter != m_cmapExt->end(); cMapIter++)
    {
      if (cMapIter->second != NULL)
      {
        delete cMapIter->second;
      }
    }
    delete m_cmapExt;
  }

  ClearTableDirectory();
  delete m_tableDirectory;
}

void
wxPdfFontParserTrueType::ClearTableDirectory()
{
  wxPdfTableDirectory::iterator entry = m_tableDirectory->begin();
  for (entry = m_tableDirectory->begin(); entry != m_tableDirectory->end(); entry++)
  {
    if (entry->second != NULL)
    {
      delete entry->second;
      entry->second = NULL;
    }
  }
}

static const wxChar* tableNamesDefault[] = {
  wxT("cvt "), wxT("fpgm"), wxT("glyf"), wxT("head"),
  wxT("hhea"), wxT("hmtx"), wxT("loca"), wxT("maxp"), wxT("prep"),
  NULL
};

void
wxPdfFontParserTrueType::LockTable(const wxString& tableName)
{
#if defined(__WXMAC__)
#if wxPDFMACOSX_HAS_CORE_TEXT
  if (m_isMacCoreText)
  {
    wxCharBuffer asciiTableName = tableName.ToAscii();
    const char* localTableName = asciiTableName;
    CTFontTableTag tableTag = localTableName[0] << 24 |
                              localTableName[1] << 16 |
                              localTableName[2] << 8 |
                              localTableName[3];
    m_tableRef.reset(CTFontCopyTable(m_fontRef, tableTag, 0));
    const UInt8* tableData = CFDataGetBytePtr(m_tableRef);
    CFIndex      tableLen  = CFDataGetLength(m_tableRef);
    m_savedStream = m_inFont;
    m_inFont = new wxMemoryInputStream((const char*) tableData, (size_t) tableLen);
  }
#else
  wxUnusedVar(tableName);
#endif
#else
  wxUnusedVar(tableName);
#endif
}

void
wxPdfFontParserTrueType::ReleaseTable()
{
#if defined(__WXMAC__)
#if wxPDFMACOSX_HAS_CORE_TEXT
  if (m_isMacCoreText)
  {
    delete m_inFont;
    m_inFont = m_savedStream;
  }
#endif
#endif
}

int
wxPdfFontParserTrueType::CalculateChecksum(const char* b, size_t length)
{
  size_t len = length / 4;
  int d0 = 0;
  int d1 = 0;
  int d2 = 0;
  int d3 = 0;
  size_t ptr = 0;
  size_t k;
  for (k = 0; k < len; ++k)
  {
    d3 += (int)b[ptr++] & 0xff;
    d2 += (int)b[ptr++] & 0xff;
    d1 += (int)b[ptr++] & 0xff;
    d0 += (int)b[ptr++] & 0xff;
  }
  return d0 + (d1 << 8) + (d2 << 16) + (d3 << 24);
}

int
wxPdfFontParserTrueType::GetCollectionFontCount(const wxString& fontFileName)
{
  int count = 0;
  wxFileName fileName(fontFileName);
  wxFileSystem fs;

  wxFSFile* fontFile = fs.OpenFile(wxFileSystem::FileNameToURL(fileName));
  if (fontFile != NULL)
  {
    m_inFont = fontFile->GetStream();
    m_inFont->SeekI(0);

    // Check for TrueType collection
    if (fileName.GetExt().Lower().IsSameAs(wxT("ttc")))
    {
      wxString mainTag = ReadString(4);
      if (mainTag == wxT("ttcf"))
      {
        SkipBytes(4);
        count = ReadInt();
      }
    }
    delete fontFile;
  }
  return count;
}

wxPdfFontData*
wxPdfFontParserTrueType::IdentifyFont(const wxString& fontFileName, int fontIndex)
{
  bool ok = true;
  wxPdfFontData* fontData = NULL;
  m_fileName = fontFileName;
  wxFileName fileName(fontFileName);
  wxFileSystem fs;

  // Open font file
  wxFSFile* fontFile = fs.OpenFile(wxFileSystem::FileNameToURL(fileName));
  if (fontFile != NULL)
  {
    m_inFont = fontFile->GetStream();
    m_inFont->SeekI(0);

    // Check for TrueType collection
    if (fileName.GetExt().Lower().IsSameAs(wxT("ttc")))
    {
      if (fontIndex >= 0)
      {
        wxString mainTag = ReadString(4);
        if (mainTag == wxT("ttcf"))
        {
          SkipBytes(4);
          int dirCount = ReadInt();
          if (fontIndex < dirCount)
          {
            SkipBytes(fontIndex * 4);
            m_directoryOffset = ReadInt();
          }
          else
          {
            wxLogError(wxString(wxT("wxPdfFontParserTrueType::IdentifyFont: ")) +
                       wxString::Format(_("Font index %d out of range for font file '%s'."), fontIndex, fontFileName.c_str()));
            ok = false;
          }
        }
        else
        {
          wxLogError(wxString(wxT("wxPdfFontParserTrueType::IdentifyFont: '")) + 
                     wxString::Format(_("Font file '%s' not a valid TrueType collection (TTC) file."), fontFileName.c_str()));
          ok = false;
        }
      }
      else
      {
        wxLogError(wxString(wxT("wxPdfFontParserTrueType::IdentifyFont: ")) +
                   wxString::Format(_("Font index %d out of range for font file '%s'."), fontIndex, fontFileName.c_str()));
        ok = false;
      }
    }
    else
    {
      m_directoryOffset = 0;
      fontIndex = 0;
    }

    // Identify single font
    if (ok)
    {
      fontData = IdentifyFont();
      if (fontData != NULL)
      {
        fontData->SetFontFileName(m_fileName);
        fontData->SetFontIndex(fontIndex);
      }
      else
      {
        wxLogError(wxString(wxT("wxPdfFontParserTrueType::IdentifyFont: ")) +
                   wxString::Format(_("Reading of font directory failed for font file '%s'."), fontFileName.c_str()));
      }
    }
    delete fontFile;
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfFontParserTrueType::IdentifyFont: ")) +
               wxString::Format(_("Font file '%s' not accessible."), fontFileName.c_str()));
  }
  return fontData;
}

#ifdef __WXMSW__
wxPdfFontData*
wxPdfFontParserTrueType::IdentifyFont(const wxFont& font)
{
  bool ok = true;
  wxPdfFontData* fontData = NULL;
  m_fileName = wxEmptyString;

  wxMemoryInputStream* fontStream = LoadTrueTypeFontStream(font);
  if (fontStream != NULL)
  {
    m_inFont = fontStream;
    m_inFont->SeekI(0);

    m_directoryOffset = 0;

    // Identify single font
    if (ok)
    {
      fontData = IdentifyFont();
      if (fontData != NULL)
      {
        fontData->SetFont(font);
        fontData->SetFontIndex(0);
      }
      else
      {
        wxLogError(wxString(wxT("wxPdfFontParserTrueType::IdentifyFont: ")) +
                   wxString::Format(_("Reading of font directory failed for font file '%s'."), font.GetFaceName().c_str()));
      }
    }
    delete fontStream;
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfFontParserTrueType::IdentifyFont: ")) +
               wxString::Format(_("Font file '%s' not accessible."), font.GetFaceName().c_str()));
  }
  return fontData;
}
#endif

#if defined(__WXMAC__)
wxPdfFontData*
wxPdfFontParserTrueType::IdentifyFont(const wxFont& font)
{
  wxPdfFontData* fontData = NULL;
#if wxPDFMACOSX_HAS_CORE_TEXT
#if wxCHECK_VERSION(2,9,0)
// wxWidgets 2.9.x or higher
  m_fontRef = font.OSXGetCTFont();
#else // wxWidgets 2.8.x
  m_fontRef = (const void*) font.MacGetCTFont();
#endif

  m_isMacCoreText = true;
  m_fileName = wxEmptyString;

  m_inFont = NULL;
  m_directoryOffset = 0;

  // Identify single font
  fontData = IdentifyFont();
  if (fontData != NULL)
  {
    fontData->SetFont(font);
    fontData->SetFontIndex(0);
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfFontParserTrueType::IdentifyFont: ")) +
               wxString::Format(_("Reading of font directory failed for font file '%s'."), font.GetFaceName().c_str()));
  }
#endif
  return fontData;
}
#endif

wxPdfFontData*
wxPdfFontParserTrueType::IdentifyFont()
{
  wxPdfFontData* fontData = NULL;
#if wxUSE_UNICODE
  if (ReadTableDirectory())
  {
    if (CheckTables())
    {
      CheckCff();
      if (m_cff)
      {
        wxPdfFontDataOpenTypeUnicode* otfFontData =  new wxPdfFontDataOpenTypeUnicode();
        otfFontData->SetCffOffset(m_cffOffset);
        otfFontData->SetCffLength(m_cffLength);
        fontData = otfFontData;
      }
      else
      {
        fontData = new wxPdfFontDataTrueTypeUnicode();
      }

      // Get PostScript name, font family, font names and font style
      fontData->SetName(GetBaseFont());
      fontData->SetFamily(GetEnglishName(1));
      fontData->SetFullNames(GetUniqueNames(4));
      fontData->SetStyle(GetEnglishName(2));
      m_fontName = fontData->GetName();

      CheckRestrictions();
      fontData->SetEmbedSupported(m_embedAllowed);
      fontData->SetSubsetSupported(m_subsetAllowed);
    }
  }
#endif
  return fontData;
}

bool
wxPdfFontParserTrueType::LoadFontData(wxPdfFontData* fontData)
{
  bool ok = false;
  if (fontData != NULL)
  {
    wxFSFile* fontFile = NULL;
    wxMemoryInputStream* fontStream = NULL;
    m_inFont = NULL;
    int fontIndex = fontData->GetFontIndex();
    m_fileName = fontData->GetFontFileName();
    m_fontName = fontData->GetName();
    if (!m_fileName.IsEmpty())
    {
      wxFileName fileName(m_fileName);
      wxFileSystem fs;
      fontFile = fs.OpenFile(wxFileSystem::FileNameToURL(fileName));
      if (fontFile != NULL)
      {
        m_inFont = fontFile->GetStream();
      }
    }
    else
    {
#if defined(__WXMSW__)
      wxFont font = fontData->GetFont();
      if (font.IsOk())
      {
        fontStream = LoadTrueTypeFontStream(font);
        if (fontStream != NULL)
        {
          m_inFont = fontStream;
        }
      }
#elif defined(__WXMAC__)
#if wxPDFMACOSX_HAS_CORE_TEXT
      wxFont font = fontData->GetFont();
      if (font.IsOk())
      {
#if wxCHECK_VERSION(2,9,0)
        // wxWidgets 2.9.x or higher
        m_fontRef = font.OSXGetCTFont();
#else // wxWidgets 2.8.x
        m_fontRef = (const void*) font.MacGetCTFont();
#endif
        m_isMacCoreText = true;
        fontStream = new wxMemoryInputStream("dummy", 5);
        m_inFont = fontStream;
      }
#endif
#endif
    }
    if (m_inFont != NULL)
    {
      m_inFont->SeekI(0);

      if (fontIndex >= 0)
      {
        // Check whether the font file is a TrueType collection
        wxString mainTag = ReadString(4);
        if (mainTag == wxT("ttcf"))
        {
          SkipBytes(4);
          int dirCount = ReadInt();
          if (fontIndex < dirCount)
          {
            SkipBytes(fontIndex * 4);
            m_directoryOffset = ReadInt();
            ok = true;
          }
          else
          {
            wxLogError(wxString(wxT("wxPdfFontParserTrueType::LoadFontData: ")) +
                       wxString::Format(_("Font index %d out of range for font file '%s'."), fontIndex, m_fileName.c_str()));
          }
        }
        else
        {
          m_directoryOffset = 0;
          ok = (fontIndex == 0);
          if (!ok)
          {
            wxLogError(wxString(wxT("wxPdfFontParserTrueType::LoadFontData: '")) + 
                       wxString::Format(_("Font file '%s' not a valid TrueType collection (TTC) file."), m_fileName.c_str()));
          }
        }
      }
      else
      {
        wxLogError(wxString(wxT("wxPdfFontParserTrueType::LoadFontData: ")) +
                   wxString::Format(_("Font index %d out of range for font file '%s'."), fontIndex, m_fileName.c_str()));
      }

      // Load data for a single font
      if (ok)
      {
        if (ReadTableDirectory())
        {
          if (CheckTables())
          {
            CheckCff();
            if (m_cff)
            {
              ok = fontData->GetType().IsSameAs(wxT("OpenTypeUnicode"));
            }
            else
            {
              ok = fontData->GetType().IsSameAs(wxT("TrueTypeUnicode"));
            }
            if (ok)
            {
              ok = PrepareFontData(fontData);
            }
            else
            {
              wxLogError(wxString(wxT("wxPdfFontParserTrueType::LoadFontData: ")) +
                         wxString::Format(_("Wrong font data type '%s' for font file '%s'."), fontData->GetType().c_str(), m_fileName.c_str()));
            }
          }
          else
          {
            wxLogError(wxString(wxT("wxPdfFontParserTrueType::LoadFontData: ")) +
                       wxString::Format(_("Missing font tables for font file '%s'."), m_fileName.c_str()));
          }
        }
        else
        {
          wxLogError(wxString(wxT("wxPdfFontParserTrueType::LoadFontData: ")) +
                     wxString::Format(_("Reading of font directory failed for font file '%s'."), m_fileName.c_str()));
        }
      }
      if (fontFile != NULL)
      {
        delete fontFile;
      }
      if (fontStream != NULL)
      {
        delete fontStream;
      }
    }
    else
    {
      wxLogError(wxString(wxT("wxPdfFontParserTrueType::LoadFontData: ")) +
                 wxString::Format(_("Font file '%s' not accessible."), m_fileName.c_str()));
    }
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfFontParserTrueType::LoadFontData: ")) +
               wxString(_("No font data instance given.")));
  }
  return ok;
}

bool
wxPdfFontParserTrueType::PrepareFontData(wxPdfFontData* fontData)
{
  bool ok = ReadMaps();
  if (ok)
  {
    wxPdfGlyphWidthMap* widths = new wxPdfGlyphWidthMap();
    wxPdfChar2GlyphMap* glyphs = new wxPdfChar2GlyphMap();

    wxPdfCMap* cMap;
    if (m_cmapExt != NULL)
      cMap = m_cmapExt;
    else if (!m_fontSpecific && m_cmap31 != NULL)
      cMap = m_cmap31;
    else if (m_fontSpecific && m_cmap10 != NULL)
      cMap = m_cmap10;
    else if (m_cmap31 != NULL)
      cMap = m_cmap31;
    else 
      cMap = m_cmap10;
    wxPdfCMap::iterator cMapIter;
    int cc;
    wxPdfCMapEntry* cMapEntry;
    for (cMapIter = cMap->begin(); cMapIter != cMap->end(); cMapIter++)
    {
      cc = cMapIter->first;
      cMapEntry = cMapIter->second;
      (*widths)[cc] = cMapEntry->m_width;
      (*glyphs)[cc] = cMapEntry->m_glyph;
    }

    fontData->SetGlyphWidthMap(widths);
    fontData->SetChar2GlyphMap(glyphs);
    fontData->SetGlyphWidths(m_glyphWidths);
    fontData->SetKernPairMap(m_kp);
    fontData->SetDescription(m_fd);
  }

  m_inFont->SeekI(0);
  size_t len = (m_cff) ? m_cffLength : m_inFont->GetSize();
  fontData->SetSize1(len);
#if wxUSE_UNICODE
  fontData->CreateDefaultEncodingConv();
#endif
  return ok;
}

bool
wxPdfFontParserTrueType::ReadTableDirectory()
{
  ClearTableDirectory();
  bool ok = true;
  if (!m_isMacCoreText)
  {
    m_inFont->SeekI(m_directoryOffset);
    int id = ReadInt();
    //        TrueType            OpenType            Mac TrueType
    if (id == 0x00010000 || id == 0x4F54544F || id == 0x74727565)
    {
      int num_tables = ReadUShort();
      SkipBytes(6);
      int k;
      for (k = 0; k < num_tables; ++k)
      {
        wxString tag = ReadString(4);
        wxPdfTableDirectoryEntry* tableLocation = new wxPdfTableDirectoryEntry();
        tableLocation->m_checksum = ReadInt();
        tableLocation->m_offset = ReadInt();
        tableLocation->m_length = ReadInt();
        (*m_tableDirectory)[tag] = tableLocation;
      }
    }
    else
    {
      if (!m_fileName.IsEmpty())
      {
        wxLogError(wxString(wxT("wxPdfFontParserTrueType::ReadTableDirectory: '")) + 
                   wxString::Format(_("Font file '%s' not a valid TrueType (TTF) or OpenType (OTF) file."), m_fileName.c_str()));
      }
      ok = false;
    }
  }
  else
  {
#if defined(__WXMAC__)
#if wxPDFMACOSX_HAS_CORE_TEXT
    CFArrayRef tables = CTFontCopyAvailableTables(m_fontRef, 0);
    CFIndex nTables = CFArrayGetCount(tables);
    CFIndex n;
    for (n = 0; n < nTables; ++n)
    {
      CTFontTableTag tag = (CTFontTableTag)(uintptr_t) CFArrayGetValueAtIndex(tables, n);
      CFDataRef    tableRef  = CTFontCopyTable(m_fontRef, tag, 0);
      const UInt8* tableData = CFDataGetBytePtr(tableRef);
      CFIndex      tableLen  = CFDataGetLength(tableRef);
      int checksum = CalculateChecksum((const char*) tableData, (size_t) tableLen);
      CFRelease(tableRef);
      char asciiTag[5];
      asciiTag[0] = (tag >> 24) & 0xff;
      asciiTag[1] = (tag >> 16) & 0xff;
      asciiTag[2] = (tag >>  8) & 0xff;
      asciiTag[3] =  tag        & 0xff;
      asciiTag[4] = 0;
      wxString tableTag = wxString::FromAscii(asciiTag);
      wxPdfTableDirectoryEntry* tableLocation = new wxPdfTableDirectoryEntry();
      tableLocation->m_checksum = checksum;
      tableLocation->m_offset = 0;
      tableLocation->m_length = tableLen;
      (*m_tableDirectory)[tableTag] = tableLocation;
    }
#else
    ok = false;
#endif
#endif
  }
  return ok;
}

static const wxChar* checkTableNames[] = {
  wxT("cmap"), wxT("head"), wxT("hhea"), wxT("hmtx"), wxT("name"),
  wxT("post"), 
  wxT("glyf"), wxT("loca"),
  NULL
};

bool
wxPdfFontParserTrueType::CheckTables()
{
  bool ok = true;
  int maxTableCount = (m_tableDirectory->find(wxT("CFF ")) == m_tableDirectory->end()) ? 8 : 6;
  int tableCount = 0;
  while (ok && tableCount < maxTableCount && checkTableNames[tableCount] != NULL)
  {
    if (m_tableDirectory->find(checkTableNames[tableCount]) == m_tableDirectory->end())
    {
      ok = false;
    }
    ++tableCount;
  }
  return ok;
}

void
wxPdfFontParserTrueType::CheckCff()
{
  wxPdfTableDirectoryEntry* tableLocation;
  wxPdfTableDirectory::iterator entry = m_tableDirectory->find(wxT("CFF "));
  if (entry != m_tableDirectory->end())
  {
    tableLocation = entry->second;
    m_cff = true;
    m_cffOffset = tableLocation->m_offset;
    m_cffLength = tableLocation->m_length;
  }
  else
  {
    m_cff = false;
    m_cffOffset = 0;
    m_cffLength = 0;
  }
}

void
wxPdfFontParserTrueType::CheckRestrictions()
{
  wxPdfTableDirectoryEntry* tableLocation;
  wxPdfTableDirectory::iterator entry = m_tableDirectory->find(wxT("OS/2"));
  if (entry != m_tableDirectory->end())
  {
    tableLocation = entry->second;
    LockTable(wxT("OS/2"));
    m_inFont->SeekI(tableLocation->m_offset+8);
    short fsType = ReadShort();
    bool rl = (fsType & 0x0002) != 0; // restricted license
    bool pp = (fsType & 0x0004) != 0; // preview and print embedding
    bool e  = (fsType & 0x0008) != 0; // editable embedding
    bool ns = (fsType & 0x0100) != 0; // no subsetting
    bool b  = (fsType & 0x0200) != 0; // bitmap embedding only
    m_embedAllowed = !((rl && !pp && !e) || b);
    m_subsetAllowed = !ns;
    ReleaseTable();
  }
  else
  {
    m_embedAllowed = true;
    m_subsetAllowed = true;
  }
}

wxString
wxPdfFontParserTrueType::GetBaseFont()
{
  wxString fontName = wxEmptyString;
  wxPdfTableDirectoryEntry* tableLocation;
  wxPdfTableDirectory::iterator entry = m_tableDirectory->find(wxT("name"));
  if (entry != m_tableDirectory->end())
  {
    tableLocation = entry->second;
    LockTable(wxT("name"));
    m_inFont->SeekI(tableLocation->m_offset+2);
    int numRecords = ReadUShort();
    int startOfStorage = ReadUShort();
    int k;
    for (k = 0; k < numRecords; k++)
    {
      int platformID = ReadUShort();
      /* int platformEncodingID = */ ReadUShort();
      /* int languageID = */ ReadUShort();
      int nameID = ReadUShort();
      int length = ReadUShort();
      int offset = ReadUShort();
      if (nameID == 6)
      {
        m_inFont->SeekI(tableLocation->m_offset + startOfStorage + offset);
        if (platformID == 0 || platformID == 3)
        {
          fontName = ReadUnicodeString(length);
        }
        else
        {
          fontName = ReadString(length);
        }
        break;
      }
    }
    if (fontName.IsEmpty())
    {
      wxFileName::SplitPath(m_fileName, NULL, &fontName, NULL);
      fontName.Replace(wxT(" "), wxT("-"));
    }
    ReleaseTable();
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfFontParserTrueType::GetBaseFont: "))+
               wxString::Format(_("Table 'name' does not exist in font file '%s'."), m_fileName.c_str()));
  }
  return fontName;
}
    
bool
wxPdfFontParserTrueType::ReadMaps()
{
  wxPdfFontHeader head;
  wxPdfHorizontalHeader hhea;
  wxPdfWindowsMetrics os_2;
  int realOS2TypoDescender;

  wxPdfTableDirectoryEntry* tableLocation;
  wxPdfTableDirectory::iterator entry = m_tableDirectory->find(wxT("head"));
  if (entry == m_tableDirectory->end())
  {
    wxLogError(wxString(wxT("wxPdfFontParser::ReadMaps: ")) +
               wxString::Format(_("Table 'head' does not exist in '%s,%s'."), m_fileName.c_str(), m_style.c_str()));
    return false;
  }
  tableLocation = entry->second;
  LockTable(wxT("head"));
  m_inFont->SeekI(tableLocation->m_offset+16);
  head.m_flags = ReadUShort();
  head.m_unitsPerEm = ReadUShort();
  SkipBytes(16);
  head.m_xMin = ReadShort();
  head.m_yMin = ReadShort();
  head.m_xMax = ReadShort();
  head.m_yMax = ReadShort();
  head.m_macStyle = ReadUShort();
  ReleaseTable();

  entry = m_tableDirectory->find(wxT("hhea"));
  if (entry == m_tableDirectory->end())
  {
    wxLogError(wxString(wxT("wxPdfFontParser::ReadMaps: ")) +
               wxString::Format(_("Table 'hhea' does not exist in '%s,%s'."), m_fileName.c_str(), m_style.c_str()));
    return false;
  }
  tableLocation = entry->second;
  LockTable(wxT("hhea"));
  m_inFont->SeekI(tableLocation->m_offset+4);
  hhea.m_ascender = ReadShort();
  hhea.m_descender = ReadShort();
  hhea.m_lineGap = ReadShort();
  hhea.m_advanceWidthMax = ReadUShort();
  hhea.m_minLeftSideBearing = ReadShort();
  hhea.m_minRightSideBearing = ReadShort();
  hhea.m_xMaxExtent = ReadShort();
  hhea.m_caretSlopeRise = ReadShort();
  hhea.m_caretSlopeRun = ReadShort();
  SkipBytes(12);
  hhea.m_numberOfHMetrics = ReadUShort();
  ReleaseTable();

  entry = m_tableDirectory->find(wxT("OS/2"));
  if (entry != m_tableDirectory->end())
  {
    tableLocation = entry->second;
    LockTable(wxT("OS/2"));
    m_inFont->SeekI(tableLocation->m_offset);
    int version = ReadUShort();
    os_2.m_xAvgCharWidth = ReadShort();
    os_2.m_usWeightClass = ReadUShort();
    os_2.m_usWidthClass = ReadUShort();
    os_2.m_fsType = ReadShort();
    os_2.m_ySubscriptXSize = ReadShort();
    os_2.m_ySubscriptYSize = ReadShort();
    os_2.m_ySubscriptXOffset = ReadShort();
    os_2.m_ySubscriptYOffset = ReadShort();
    os_2.m_ySuperscriptXSize = ReadShort();
    os_2.m_ySuperscriptYSize = ReadShort();
    os_2.m_ySuperscriptXOffset = ReadShort();
    os_2.m_ySuperscriptYOffset = ReadShort();
    os_2.m_yStrikeoutSize = ReadShort();
    os_2.m_yStrikeoutPosition = ReadShort();
    os_2.m_sFamilyClass = ReadShort();
    m_inFont->Read(os_2.m_panose, 10);
    SkipBytes(16);
    m_inFont->Read(os_2.m_achVendID, 4);
    os_2.m_fsSelection = ReadUShort();
    os_2.m_usFirstCharIndex = ReadUShort();
    os_2.m_usLastCharIndex = ReadUShort();
    os_2.m_sTypoAscender = ReadShort();
    os_2.m_sTypoDescender = ReadShort();
    realOS2TypoDescender = os_2.m_sTypoDescender;
    if (os_2.m_sTypoDescender > 0)
    {
      os_2.m_sTypoDescender = (short)(-os_2.m_sTypoDescender);
    }
    os_2.m_sTypoLineGap = ReadShort();
    os_2.m_usWinAscent = ReadUShort();
    os_2.m_usWinDescent = ReadUShort();
    os_2.m_ulCodePageRange1 = 0;
    os_2.m_ulCodePageRange2 = 0;
    if (version > 0)
    {
      os_2.m_ulCodePageRange1 = ReadInt();
      os_2.m_ulCodePageRange2 = ReadInt();
    }
    if (version > 1)
    {
      os_2.m_sxHeight = ReadShort();
      os_2.m_sCapHeight = ReadShort();
    }
    else
    {
      os_2.m_sxHeight = 0;
      os_2.m_sCapHeight = (int)(0.7 * head.m_unitsPerEm);
    }
    ReleaseTable();
    m_fd.SetOpenTypeMetrics(
      (int) (hhea.m_ascender      * 1000 / head.m_unitsPerEm),
      (int) (hhea.m_descender     * 1000 / head.m_unitsPerEm),
      (int) (hhea.m_lineGap       * 1000 / head.m_unitsPerEm),
      (int) (os_2.m_sTypoAscender * 1000 / head.m_unitsPerEm),
      (int) (realOS2TypoDescender * 1000 / head.m_unitsPerEm),
      (int) (os_2.m_sTypoLineGap  * 1000 / head.m_unitsPerEm),
      (int) (os_2.m_usWinAscent   * 1000 / head.m_unitsPerEm),
      (int) (os_2.m_usWinDescent  * 1000 / head.m_unitsPerEm)
    );
  }
  else
  {
    // TODO: check definition of hhea values, convert if necessary
    os_2.m_sTypoAscender = hhea.m_ascender;
    os_2.m_sTypoDescender = hhea.m_descender;
    if (os_2.m_sTypoDescender > 0)
    {
      os_2.m_sTypoDescender = (short)(-os_2.m_sTypoDescender);
    }
    os_2.m_sxHeight = 0;
    os_2.m_sCapHeight = (int)(0.7 * head.m_unitsPerEm);
    if (os_2.m_sCapHeight > os_2.m_sTypoAscender)
    {
      os_2.m_sCapHeight = os_2.m_sTypoAscender;
    }
  }

  int underlinePosition = -100;
  int underlineThickness = 50;

  double italicAngle;
  entry = m_tableDirectory->find(wxT("post"));
  if (entry == m_tableDirectory->end())
  {
    static double pi = 4. * atan(1.0);
    double caretSlopeRun = (double) hhea.m_caretSlopeRun;
    double caretSlopeRise = (double) hhea.m_caretSlopeRise;
    italicAngle = -atan2(caretSlopeRun, caretSlopeRise) * 180. / pi;
  }
  else
  {
    tableLocation = entry->second;
    LockTable(wxT("post"));
    m_inFont->SeekI(tableLocation->m_offset+4);
    short mantissa = ReadShort();
    int fraction = ReadUShort();
    italicAngle = (double) mantissa + (double) fraction / 16384.0;
    underlinePosition = ReadShort();
    underlineThickness = ReadShort();
    m_isFixedPitch = ReadInt() != 0;
    ReleaseTable();
  }

  ReadGlyphWidths(hhea.m_numberOfHMetrics, head.m_unitsPerEm);
  ReadKerning(head.m_unitsPerEm);

  m_fd.SetAscent((int) (os_2.m_sTypoAscender * 1000 / head.m_unitsPerEm));
  m_fd.SetDescent((int) (os_2.m_sTypoDescender * 1000 / head.m_unitsPerEm));
  m_fd.SetCapHeight((int) (os_2.m_sCapHeight * 1000 / head.m_unitsPerEm));
  m_fd.SetXHeight((int) (os_2.m_sxHeight * 1000 / head.m_unitsPerEm));
  m_fd.SetItalicAngle((int) italicAngle);
  m_fd.SetStemV(80);
  m_fd.SetUnderlinePosition((int) (underlinePosition * 1000 / head.m_unitsPerEm));
  m_fd.SetUnderlineThickness((int) (underlineThickness * 1000 / head.m_unitsPerEm));
  m_fd.SetMissingWidth(GetGlyphWidth(0));

#if 0
  int      m_stemV;                   ///< StemV
#endif

  int flags = 0;
  if (m_isFixedPitch)
  {
    flags |= 1;
  }
  if ((head.m_macStyle & 2) != 0)
  {
    flags |= 64;
  }
  if ((head.m_macStyle & 1) != 0)
  {
    flags |= 262144;
  }
  m_fd.SetFlags(flags);

  wxString fbb = wxString::Format(wxT("[%d %d %d %d]"),
                   (int) (head.m_xMin * 1000 / head.m_unitsPerEm),
                   (int) (head.m_yMin * 1000 / head.m_unitsPerEm),
                   (int) (head.m_xMax * 1000 / head.m_unitsPerEm),
                   (int) (head.m_yMax * 1000 / head.m_unitsPerEm));
  m_fd.SetFontBBox(fbb);

  entry = m_tableDirectory->find(wxT("cmap"));
  if (entry == m_tableDirectory->end())
  {
    wxLogError(wxString(wxT("wxPdfFontParser::ReadMaps: ")) +
               wxString::Format(_("Table 'cmap' does not exist in '%s,%s'."), m_fileName.c_str(), m_style.c_str()));
    return false;
  }
  tableLocation = entry->second;
  LockTable(wxT("cmap"));
  m_inFont->SeekI(tableLocation->m_offset);
  SkipBytes(2);
  int num_tables = ReadUShort();
  m_fontSpecific = false;
  int map10 = 0;
  int map31 = 0;
  int map30 = 0;
  int mapExt = 0;
  int k;
  for (k = 0; k < num_tables; ++k)
  {
    int platId = ReadUShort();
    int platSpecId = ReadUShort();
    int offset = ReadInt();
    if (platId == 3 && platSpecId == 0)
    {
      m_fontSpecific = true;
      map30 = offset;
    }
    else if (platId == 3 && platSpecId == 1)
    {
      map31 = offset;
    }
    else if (platId == 3 && platSpecId == 10)
    {
      mapExt = offset;
    }
    else if (platId == 1 && platSpecId == 0)
    {
      map10 = offset;
    }
    else if (platId == 0) // Apple Unicode
    {
      // Formats 0, 2, 4, 6, 8, 10, 12
      if (platSpecId < 4)
      {
        map31 = offset;
      }
      else if (platSpecId == 4) // UCS32
      {
        mapExt = offset;
      }
    }
  }
  if (map10 > 0 && map30 <= 0)
  {
    m_inFont->SeekI(tableLocation->m_offset + map10);
    int format = ReadUShort();
    switch (format)
    {
      case 0:
        m_cmap10 = ReadFormat0();
        break;
      case 4:
        m_cmap10 = ReadFormat4();
        break;
      case 6:
        m_cmap10 = ReadFormat6();
        break;
    }
  }
  if (map31 > 0)
  {
    m_inFont->SeekI(tableLocation->m_offset + map31);
    int format = ReadUShort();
    if (format == 4)
    {
      m_cmap31 = ReadFormat4();
    }
  }
  if (map30 > 0)
  {
    m_inFont->SeekI(tableLocation->m_offset + map30);
    int format = ReadUShort();
    if (format == 4)
    {
      m_cmap10 = ReadFormat4();
    }
  }
  if (mapExt > 0)
  {
    m_inFont->SeekI(tableLocation->m_offset + mapExt);
    int format = ReadUShort();
    switch (format)
    {
      case 0:
        m_cmapExt = ReadFormat0();
        break;
      case 4:
        m_cmapExt = ReadFormat4();
        break;
      case 6:
        m_cmapExt = ReadFormat6();
        break;
      case 12:
        m_cmapExt = ReadFormat12();
        break;
    }
  }
  ReleaseTable();

  flags = m_fd.GetFlags();
  flags |= m_fontSpecific ? 4 : 32;
  m_fd.SetFlags(flags);

  bool ok = (m_cmap10 != NULL) || (m_cmap31 != NULL) || (m_cmapExt != NULL);
  if (!ok)
  {
    wxLogError(wxString(wxT("wxPdfFontParserTrueType::ReadMaps: ")) +
               wxString::Format(_("No valid 'cmap' table found for font '%s'."),  m_fontName.c_str()));
  }

  return ok;
}

bool
wxPdfFontParserTrueType::ReadGlyphWidths(int numberOfHMetrics, int unitsPerEm)
{
  wxPdfTableDirectoryEntry* tableLocation;
  wxPdfTableDirectory::iterator entry = m_tableDirectory->find(wxT("hmtx"));
  if (entry == m_tableDirectory->end())
  {
    wxLogError(wxString(wxT("wxPdfFontParser::ReadGlyphWidths: ")) +
               wxString::Format(_("Table 'hmtx' does not exist in '%s,%s'."), m_fileName.c_str(), m_style.c_str()));
    return false;
  }
  tableLocation = entry->second;
  LockTable(wxT("hmtx"));
  m_inFont->SeekI(tableLocation->m_offset);

  m_glyphWidths.SetCount(numberOfHMetrics);
  int k;
  for (k = 0; k < numberOfHMetrics; k++)
  {
    m_glyphWidths[k] = (ReadUShort() * 1000) / unitsPerEm;
    ReadUShort();
  }
  ReleaseTable();
  return true;
}

wxPdfCMap*
wxPdfFontParserTrueType::ReadFormat0()
{
  wxPdfCMap* h = new wxPdfCMap();
  SkipBytes(4);
  int k;
  for (k = 0; k < 256; k++)
  {
    wxPdfCMapEntry* r = new wxPdfCMapEntry();
    r->m_glyph = (int) ReadByte();
    r->m_width = GetGlyphWidth(r->m_glyph);
    (*h)[k] = r;
  }
  return h;
}
    
wxPdfCMap*
wxPdfFontParserTrueType::ReadFormat4()
{
  wxPdfCMap* h = new wxPdfCMap();
  int tableLength = ReadUShort();
  SkipBytes(2);
  int segCount = ReadUShort() / 2;
  int glyphIdCount = tableLength / 2 - 8 - segCount * 4;
  SkipBytes(6);

  int* endCount   = new int[segCount];
  int* startCount = new int[segCount];
  int* idDelta    = new int[segCount];
  int* idRO       = new int[segCount];
  int* glyphId    = new int[glyphIdCount];

  int k;
  for (k = 0; k < segCount; k++)
  {
    endCount[k] = ReadUShort();
  }
  SkipBytes(2);
  for (k = 0; k < segCount; k++)
  {
    startCount[k] = ReadUShort();
  }
  for (k = 0; k < segCount; k++)
  {
    idDelta[k] = ReadUShort();
  }
  for (k = 0; k < segCount; k++)
  {
    idRO[k] = ReadUShort();
  }
  for (k = 0; k < glyphIdCount; k++)
  {
    glyphId[k] = ReadUShort();
  }
  for (k = 0; k < segCount; k++)
  {
    int glyph;
    int j;
    for (j = startCount[k]; j <= endCount[k] && j != 0xFFFF; j++)
    {
      if (idRO[k] == 0)
      {
        glyph = (j + idDelta[k]) & 0xFFFF;
      }
      else
      {
        int idx = k + idRO[k] / 2 - segCount + j - startCount[k];
        if (idx >= glyphIdCount)
          continue;
        glyph = (glyphId[idx] + idDelta[k]) & 0xFFFF;
      }
      wxPdfCMapEntry* r = new wxPdfCMapEntry();
      r->m_glyph = glyph;
      r->m_width = GetGlyphWidth(r->m_glyph);
      int idx = m_fontSpecific ? ((j & 0xff00) == 0xf000 ? j & 0xff : j) : j;
      (*h)[idx] = r;
//      wxLogMessage(wxT("C %ld G %ld"), idx, glyph);
    }
  }

  delete [] endCount;
  delete [] startCount;
  delete [] idDelta;
  delete [] idRO;
  delete [] glyphId;

  return h;
}
    
wxPdfCMap*
wxPdfFontParserTrueType::ReadFormat6()
{
  wxPdfCMap* h = new wxPdfCMap();
  SkipBytes(4);
  int startCode = ReadUShort();
  int codeCount = ReadUShort();
  int k;
  for (k = 0; k < codeCount; k++)
  {
    wxPdfCMapEntry* r = new wxPdfCMapEntry();
    r->m_glyph = ReadUShort();
    r->m_width = GetGlyphWidth(r->m_glyph);
    (*h)[k+startCode] = r;
  }
  return h;
}

wxPdfCMap*
wxPdfFontParserTrueType::ReadFormat12()
{
  wxPdfCMap* h = new wxPdfCMap();
  SkipBytes(2);
  /* int tableLength = */ ReadInt();
  SkipBytes(4);
  int nGroups = ReadInt();

  for (int k = 0; k < nGroups; k++)
  {
    int startCharCode = ReadInt();
    int endCharCode = ReadInt();
    int startGlyphID = ReadInt();
    int i;
    for (i = startCharCode; i <= endCharCode; ++i)
    {
      wxPdfCMapEntry* r = new wxPdfCMapEntry();
      r->m_glyph = startGlyphID;
      r->m_width = GetGlyphWidth(r->m_glyph);
      (*h)[i] = r;
      ++startGlyphID;
    }
  }
  return h;
}

void
wxPdfFontParserTrueType::ReadKerning(int unitsPerEm)
{
  wxPdfTableDirectoryEntry* tableLocation;
  wxPdfTableDirectory::iterator entry = m_tableDirectory->find(wxT("kern"));
  if (entry != m_tableDirectory->end())
  {
    tableLocation = entry->second;
    LockTable(wxT("kern"));
    m_kp = new wxPdfKernPairMap();
    wxPdfKernPairMap::iterator kp;
    wxPdfKernWidthMap* kwMap = NULL;
    wxPdfKernWidthMap::iterator kw;
    wxUint32 u1, u2;
    wxUint32 u1prev = 0;

    m_inFont->SeekI(tableLocation->m_offset+2);
    int nTables = ReadUShort();
    int checkpoint = tableLocation->m_offset + 4;
    int length = 0;
    int j, k;
    for (k = 0; k < nTables; ++k)
    {
      checkpoint += length;
      m_inFont->SeekI(checkpoint);
      SkipBytes(2);
      length = ReadUShort();
      int coverage = ReadUShort();
      if ((coverage & 0xfff7) == 0x0001)
      {
        int nPairs = ReadUShort();
        SkipBytes(6);
        for (j = 0; j < nPairs; ++j)
        {
          u1 = ReadUShort();
          u2 = ReadUShort();
          int value = ((int) ReadShort() * 1000) / unitsPerEm;
          if (u1 != u1prev)
          {
            u1prev = u1;
            wxPdfKernPairMap::iterator kp = (*m_kp).find(u1);
            if (kp == (*m_kp).end())
            {
              kwMap = new wxPdfKernWidthMap();
              (*m_kp)[u1] = kwMap;
            }
            else
            {
              kwMap = kp->second;
            }
          }
          (*kwMap)[u2] = value;
        }
      }
    }
    ReleaseTable();
  }
}
    
int
wxPdfFontParserTrueType::GetGlyphWidth(unsigned int glyph)
{
  if (glyph >= m_glyphWidths.GetCount())
  {
    glyph = (unsigned int) m_glyphWidths.GetCount() - 1;
  }
  return m_glyphWidths[glyph];
}

wxArrayString
wxPdfFontParserTrueType::GetUniqueNames(int id)
{
  wxArrayString uniqueNames;
  wxArrayString names = GetNames(id);
  size_t j;
  for (j = 0; j < names.GetCount(); ++j)
  {
    if (uniqueNames.Index(names[j], false) == wxNOT_FOUND)
    {
      uniqueNames.Add(names[j]);
    }
  }
  return uniqueNames;
}

wxArrayString
wxPdfFontParserTrueType::GetNames(int id, bool namesOnly)
{
  wxArrayString names;
  wxPdfTableDirectoryEntry* tableLocation;
  wxPdfTableDirectory::iterator entry = m_tableDirectory->find(wxT("name"));
  if (entry != m_tableDirectory->end())
  {
    tableLocation = entry->second;
    LockTable(wxT("name"));
    m_inFont->SeekI(tableLocation->m_offset+2);
    int numRecords = ReadUShort();
    int startOfStorage = ReadUShort();
    int k;
    for (k = 0; k < numRecords; k++)
    {
      int platformID = ReadUShort();
      int platformEncodingID = ReadUShort();
      int languageID = ReadUShort();
      int nameID = ReadUShort();
      int length = ReadUShort();
      int offset = ReadUShort();
      if (nameID == id)
      {
        off_t pos = m_inFont->TellI();
        m_inFont->SeekI(tableLocation->m_offset + startOfStorage + offset);
        wxString name;
        if (platformID == 0 || platformID == 3 || (platformID == 2 && platformEncodingID == 1))
        {
          name = ReadUnicodeString(length);
        }
        else
        {
          name = ReadString(length);
        }
        if (!namesOnly)
        {
          names.Add(wxString::Format(wxT("%d"), platformID));
          names.Add(wxString::Format(wxT("%d"), platformEncodingID));
          names.Add(wxString::Format(wxT("%d"), languageID));
        }
        names.Add(name);
        m_inFont->SeekI(pos);
      }
    }
    ReleaseTable();
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfFontParser::GetNames: ")) +
               wxString::Format(_("Table 'name' does not exist in '%s,%s'."), m_fileName.c_str(), m_style.c_str()));
  }
  return names;
}

wxString
wxPdfFontParserTrueType::GetEnglishName(int id)
{
  wxString englishName = wxEmptyString;
  wxPdfTableDirectoryEntry* tableLocation;
  wxPdfTableDirectory::iterator entry = m_tableDirectory->find(wxT("name"));
  if (entry != m_tableDirectory->end())
  {
    tableLocation = entry->second;
    LockTable(wxT("name"));
    m_inFont->SeekI(tableLocation->m_offset+2);
    int numRecords = ReadUShort();
    int startOfStorage = ReadUShort();
    bool ready = false;
    int k;
    for (k = 0; !ready && (k < numRecords); k++)
    {
      int platformID = ReadUShort();
      int platformEncodingID = ReadUShort();
      int languageID = ReadUShort();
      int nameID = ReadUShort();
      int length = ReadUShort();
      int offset = ReadUShort();
      if (nameID == id)
      {
        off_t pos = m_inFont->TellI();
        m_inFont->SeekI(tableLocation->m_offset + startOfStorage + offset);
        wxString name;
        if (platformID == 0 || platformID == 3 || (platformID == 2 && platformEncodingID == 1))
        {
          name = ReadUnicodeString(length);
        }
        else
        {
          name = ReadString(length);
        }
        if (!ready)
        {
          if (languageID == 0)
          {
            englishName = name;
            ready = true;
          }
          else if (languageID == 1033)
          {
            englishName = name;
          }
        }
        m_inFont->SeekI(pos);
      }
    }
    ReleaseTable();
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfFontParserTrueType::GetEnglishName: ")) +
               wxString::Format(_("Table 'name' does not exist in '%s,%s'."), m_fileName.c_str(), m_style.c_str()));
  }
  return englishName;
}

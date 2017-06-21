///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontsubsettruetype.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2005-11-20
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontsubsettruetype.h Interface of the TrueType Font subset support classes

#ifndef _PDF_FONT_SUBSET_TRUETYPE_H_
#define _PDF_FONT_SUBSET_TRUETYPE_H_

// wxWidgets headers
#include <wx/string.h>
#include <wx/mstream.h>
#include <wx/wfstream.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdffontparser.h"
#include "wex/pdf/pdffontparsertruetype.h"

/// Class representing TrueType Font Subsets. (For internal use only)
class wxPdfFontSubsetTrueType : public wxPdfFontParserTrueType
{
public:
  /// Default constructor
  wxPdfFontSubsetTrueType(const wxString& fileName, int fontIndex = 0, bool isMacCoreText = false);

  /// Default destructor
  virtual ~wxPdfFontSubsetTrueType();

  /// Create subset of a font
  /**
  * \param inFont stream containing the font data
  * \param glyphsUsed a list of used glyphs
  * \param includeCmap flag whether to include the CMap table
  * \return the stream containing the font subset
  */
  wxMemoryOutputStream* CreateSubset(wxInputStream* inFont,
                                     wxPdfSortedArrayInt* glyphsUsed,
                                     bool includeCmap = false);
#if defined(__WXMAC__)
#if wxPDFMACOSX_HAS_CORE_TEXT
  /// Set Mac Core Text font reference
  void SetCTFontRef(const wxFont& font);                                 
#endif
#endif

protected:
  /// Read 'loca' table
  bool ReadLocaTable();

  /// Check glyphs
  bool CheckGlyphs();

  /// Find the components of a glyph
  void FindGlyphComponents(int glyph);

  /// Create a new table
  void CreateNewTables();

  /// Write the subset of the font
  void WriteSubsetFont();

  /// Write a short integer to the default output stream
  void WriteShort(int n);

  /// Write an integer to the default output stream
  void WriteInt(int n);

  /// Write a stringto the default output stream
  void WriteString(const wxString& s);

  /// Write a short integer to a buffer
  void WriteShortToBuffer(int n, char buffer[2]);

  /// Write an integer to a buffer
  void WriteIntToBuffer(int n, char buffer[4]);

private:
  wxMemoryOutputStream* m_outFont;    ///< Subset output stream

  wxPdfSortedArrayInt*  m_usedGlyphs; ///< list of used glyphs
  
  int    m_fontIndex;                 ///< Index of font in font collection
  bool   m_includeCmap;               ///< Flag whether to include the CMap

  bool   m_locaTableIsShort;          ///< Flag whether the loca table is in short format
  int*   m_locaTable;                 ///< the loca table
  size_t m_locaTableSize;             ///< size of the loca table
  size_t m_locaTableRealSize;         ///< real size of the loca table

  int*   m_newLocaTable;              ///< new loca table
  char*  m_newLocaTableStream;        ///< new loca table stream
  size_t m_newLocaTableStreamSize;    ///< size of the new loca table stream

  int    m_glyfTableOffset;           ///< offset of the glyf table
  char*  m_newGlyfTable;              ///< new glyf table
  size_t m_newGlyfTableSize;          ///< size of the new glyf table
  size_t m_newGlyfTableRealSize;      ///< real size of the new glyf table
};

#endif

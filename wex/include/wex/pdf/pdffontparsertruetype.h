///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontparsertruetype.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-03-04
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontparsertruetype.h Interface of the TrueType Font support classes

#ifndef _PDF_FONT_PARSER_TRUETYPE_H_
#define _PDF_FONT_PARSER_TRUETYPE_H_

// wxWidgets headers
#include <wx/hashmap.h>
#include <wx/mstream.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdffontdata.h"
#include "wex/pdf/pdffontdescription.h"
#include "wex/pdf/pdffontparser.h"

#include "wex/pdf/pdffontmacosx.h"

class WXDLLIMPEXP_FWD_PDFDOC wxPdfFontData;

/// Class representing a table directory entry for TrueType fonts (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfTableDirectoryEntry
{
public:
  /// Default constructor
  wxPdfTableDirectoryEntry() : m_checksum(0), m_offset(0), m_length(0) {}
  int m_checksum;  ///< Table checksum
  int m_offset;    ///< Table offset
  int m_length;    ///< Table length
};

/// Hash map class for used/embedded fonts
WX_DECLARE_STRING_HASH_MAP_WITH_DECL(wxPdfTableDirectoryEntry*, wxPdfTableDirectory, class WXDLLIMPEXP_PDFDOC);

/// Class representing a CMap table entry
class WXDLLIMPEXP_PDFDOC wxPdfCMapEntry
{
public:
  /// Default constructor
  wxPdfCMapEntry() : m_glyph(0), m_width(0) {}
  int m_glyph;  ///< glyph number
  int m_width;  ///< glyph width
};

/// Hash map class for used/embedded fonts
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxPdfCMapEntry*, wxIntegerHash, wxIntegerEqual, wxPdfCMap, class WXDLLIMPEXP_PDFDOC);

/// Class representing a TrueType/OpenType font parser
class WXDLLIMPEXP_PDFDOC wxPdfFontParserTrueType : public wxPdfFontParser
{
public:
  /// Default constructor
  wxPdfFontParserTrueType();

  /// Default destructor
  virtual ~wxPdfFontParserTrueType();

  /// Identify font based on a font file
  /**
  * \param fontFileName fully qualified name of the font file
  * \param fontIndex the index of the font within a font collection (if appropriate)
  * \return a reference to a new font data instance
  */
  wxPdfFontData* IdentifyFont(const wxString& fontFileName, int fontIndex);

#if defined(__WXMSW__) || defined(__WXMAC__)
  /// Identify font based on a wxFont object
  /**
  * \param font the wxFont font object to be identified
  * \return a reference to a new font data instance
  */
  wxPdfFontData* IdentifyFont(const wxFont& font);
#endif

  /// Load the font data
  /**
  * Before using a font it's data have be loaded into memory. This method tries
  * to load the required font data for a previously identified font.
  * \param fontData the font data instance to be loaded
  * \return TRUE if the font data could be loaded successfully, FALSE otherwise
  */
  bool LoadFontData(wxPdfFontData* fontData);

  /// Get the number of fonts within a font collection
  /**
  * \param fontFileName the fully qualified name of the font collection file
  * \return the number of fonts in the collection
  */
  int GetCollectionFontCount(const wxString& fontFileName);

#ifdef __WXMSW__
  /// Load a TrueType font stream based on a wxFont object
  /**
  * \param font the font for which the font stream is to loaded
  * \return the font data as a memory stream
  */
  static wxMemoryInputStream* LoadTrueTypeFontStream(const wxFont& font);
#endif

protected:
  /// Clear the table directory
  void ClearTableDirectory();

  /// Lock font table
  void LockTable(const wxString& tableName);

  /// Release font table
  void ReleaseTable();

  /// Calculate a check sum
  int CalculateChecksum(const char* b, size_t length);

  /// Identify a font
  wxPdfFontData* IdentifyFont();

  /// Prepare the font data
  bool PrepareFontData(wxPdfFontData* fontData);

  /// Check whether the font contains all required tables
  bool CheckTables();

  /// Check whether the font is in compact font format (CFF)
  void CheckCff();

  /// Check the font's access restrictions
  void CheckRestrictions();

  /// Get the base name of the font
  /**
  * Gets the Postscript font name.
  * \return the Postscript font name
  */
  wxString GetBaseFont();

  /// Get a list of unique names
  /**
  * Extracts the names of the font in all the languages available.
  * \param id the name id to retrieve
  * \return a list of unique names
  */    
  wxArrayString GetUniqueNames(int id);

  /// Get font names
  /**
  * Extracts the names of the font in all the languages available.
  * Optionally the platform, encoding and language identifiers are extracted
  * as well. In the latter case 4 consecutive entries in the resulting string
  * array represent a single font name.
  * \param id the name id to retrieve
  * \param namesOnly flag whether to extract only names or names and identifiers
  * \return a list of font names
  */    
  wxArrayString GetNames(int id, bool namesOnly = true);

  /// Get the English language font name
  /**
  * \param id the name id to retrieve
  * \return the English font name
  */
  wxString GetEnglishName(int id);

  /// Read font maps
  /**
  * Reads the tables 'head', 'hhea', 'OS/2' and 'post' filling several variables.
  * \return TRUE if the font maps could be read successfully, FALSE otherwise
  */
  bool ReadMaps();

  /// Read the glyph widths
  /**
  * Reads the glyphs widths. The widths are extracted from the table 'hmtx'.
  * The glyphs are normalized to 1000 units.
  * \param numberOfHMetrics
  * \param unitsPerEm
  * \return TRUE if the glyph widths could be read successfully, FALSE otherwise
  */
  bool ReadGlyphWidths(int numberOfHMetrics, int unitsPerEm);

  /// Read kerning information
  void ReadKerning(int unitsPerEm);

  /// Read table directory
  bool ReadTableDirectory();

  /// Read a Format 0 CMap
  wxPdfCMap* ReadFormat0();

  /// Read a Format 4 CMap
  wxPdfCMap* ReadFormat4();

  /// Read a Format 6 CMap
  wxPdfCMap* ReadFormat6();

  /// Read a Format 12 CMap
  wxPdfCMap* ReadFormat12();

  /// Get the width of a specific glyph
  int GetGlyphWidth(unsigned int glyph);

  size_t                m_directoryOffset;   ///< offset of the table directory
  wxPdfTableDirectory*  m_tableDirectory;    ///< table directory of the font

  bool                  m_isMacCoreText;     ///< Flag whether a Mac Core Text font is to be parsed
#if defined(__WXMAC__)
#if wxPDFMACOSX_HAS_CORE_TEXT
  CTFontRef             m_fontRef;           ///< Mac Core Text font reference
  wxCFRef<CFDataRef>    m_tableRef;          ///< Font table reference
#endif
#endif

private:
  bool                  m_cff;               ///< Flag whether the font is in CFF format
  size_t                m_cffOffset;         ///< offset of the CFF table
  size_t                m_cffLength;         ///< length of the CFF table

  bool                  m_embedAllowed;      ///< Flag whether embedding is allowed
  bool                  m_subsetAllowed;     ///< Flag whether subsetting is allowed
  bool                  m_isFixedPitch;      ///< Flag whether the font has a fixed pitch

  wxPdfFontDescription  m_fd;                ///< font description

  wxPdfArrayUint16      m_glyphWidths;       ///< list of glyph widths
  bool                  m_fontSpecific;      ///< Flag whether font has a specific encoding
  wxPdfCMap*            m_cmap10;            ///< Reference of CMap (Type 1,0 - Postscript)
  wxPdfCMap*            m_cmap31;            ///< Reference of CMap (Type 3,1 - Unicode)
  wxPdfCMap*            m_cmapExt;           ///< Reference of CMap (Type Ext - Extended Unicode)
  wxPdfKernPairMap*     m_kp;                ///< list of kerning pairs

  wxInputStream*        m_savedStream;       ///< Saved input stream
};

#endif

///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontextended.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-02-26
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontextended.h Interface of wxPdfFontExtended class

#ifndef _PDF_FONT_EXTENDED_H_
#define _PDF_FONT_EXTENDED_H_

// wxWidgets headers
#include <wx/mstream.h>
#include <wx/string.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdfarraytypes.h"
#include "wex/pdf/pdffontdata.h"

class WXDLLIMPEXP_FWD_PDFDOC wxPdfEncoding;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfFont;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfFontDescription;

/// Class representing a font providing an extended interface
class WXDLLIMPEXP_PDFDOC wxPdfFontExtended
{
public:
  /// Default constructor
  wxPdfFontExtended();

  /// Constructor
  /**
  * \param font the user font
  */
  wxPdfFontExtended(const wxPdfFont& font);

  /// Default destructor
  virtual ~wxPdfFontExtended() ;

  /// Copy constructor
  wxPdfFontExtended(const wxPdfFontExtended& font);

  /// Assignment
  wxPdfFontExtended& operator=(const wxPdfFontExtended& font);

  /// Check whether font is valid
  /**
  * \return TRUE if the font is valid, FALSE otherwise
  */
  bool IsValid() const;

  /// Get font type
  /**
  * \return the font type
  */
  wxString GetType() const;

  /// Get font family
  /**
  * \return the font family
  */
  wxString GetFamily() const;

  /// Get font name
  /**
  * \return the font name
  */
  wxString GetName() const;

  /// Get font style
  /**
  * \return the font style
  */
  int GetStyle() const;

  /// Check whether font embedding is requested for this font
  /**
  * \return TRUE if embedding of this font is requested, FALSE otherwise
  */
  bool EmbedRequested() const { return m_embed; }

  /// Check whether font subsetting is requested for this font
  /**
  * \return TRUE if subsetting of this font is requested, FALSE otherwise
  */
  bool SubsetRequested() const { return m_subset; }

  /// Get underline position
  /**
  * \return the position of the underline decoration
  */
  int  GetUnderlinePosition() const;

  /// Get underline thickness
  /**
  * \return the thickness of the underline decoration
  */
  int  GetUnderlineThickness() const;

  /// Get bounding box top position
  /**
  * \return the bounding box of the font in string representation
  */
  int GetBBoxTopPosition() const;

  /// Get encoding
  /**
  * \return the name of the font's encoding
  */
  wxString GetEncoding() const;

  /// Get encoding
  /**
  * \return the name of the font's base encoding
  */
  wxString GetBaseEncoding() const;

  /// Check whether the font has differences to WinAnsi encoding
  /**
  * \return TRUE if the font has encoding differences, FALSE otherwise
  */
  bool HasDiffs() const;

  /// Get encoding differences
  /**
  * \return the font encoding differences
  */
  wxString GetDiffs() const;

  /// Get font file size 1
  /**
  * \return the size of the font file
  */
  size_t  GetSize1() const;

  /// Check whether the file has a size 2
  /**
  * \return TRUE if the font has a second size associated, FALSE otherwise
  * \note Usually only Type1 fonts have a second size entry.
  */
  bool HasSize2() const;

  /// Get font file size 2 (Type 1 only)
  /**
  * \return the second size of the font file
  * \note For Type1 fonts this represents the size of the BINARY section.
  */
  size_t  GetSize2() const;

  /// Get the font's CMap (Type 0 only)
  /**
  * return the font's CMap descriptor
  */
  wxString GetCMap() const;

  /// Get font ordering
  /**
  * \return the font's ordering descriptor
  */
  wxString GetOrdering() const;

  /// Get font supplement /Type 0 only)
  /**
  * \return the font's supplement descriptor
  */
  wxString GetSupplement() const;

  /// Get the character width array as string
  /**
  * \param subset flag whether subsetting is enabled
  * \param usedGlyphs the list of used glyphs
  * \param subsetGlyphs the mapping of glyphs to subset glyphs
  * \return the string representation of the character widths
  */
  wxString GetWidthsAsString(bool subset = false, wxPdfSortedArrayInt* usedGlyphs = NULL, wxPdfChar2GlyphMap* subsetGlyphs = NULL) const;
  
  /// Get the width of a string
  /**
  * \param s the string for which the width should be calculated
  * \param withKerning flag indicating whether kerning should be taken into account
  * \return the width of the string
  */
  double GetStringWidth(const wxString& s, bool withKerning = false);

  /// Get kerning width array
  /**
  * \param s the string for which kerning information should be provided
  * \return an array with indices and kerning width of the found kerning pairs. 
  * The array consists of information pairs: the first item (even indices) represents the index
  * of the first character of a kerning pair within the string and the second item (odd indices)
  * represents the kerning value.
  */
  wxArrayInt GetKerningWidthArray(const wxString& s) const;

  /// Check whether the font oan show all characters of a given string
  /**
  * \param s the string to be checked
  * \return TRUE if the font can show all characters of the string, FALSE otherwise
  */
  bool CanShow(const wxString& s) const;

  /// Convert character codes to glyph numbers
  /**
  * \param s the string to be converted
  * \param usedGlyphs the list of used glyphs
  * \param subsetGlyphs the mapping of glyphs to subset glyphs
  * \return the converted string
  */
  wxString ConvertCID2GID(const wxString& s, wxPdfSortedArrayInt* usedGlyphs = NULL, wxPdfChar2GlyphMap* subsetGlyphs = NULL);

  /// Convert glyph number to string
  /**
  * \param glyph the glyph to be converted
  * \param usedGlyphs the list of used glyphs
  * \param subsetGlyphs the mapping of glyphs to subset glyphs
  * \return the converted string
  */
  wxString ConvertGlyph(wxUint32 glyph, wxPdfSortedArrayInt* usedGlyphs = NULL, wxPdfChar2GlyphMap* subsetGlyphs = NULL);

  /// Check whether the font will be embedded
  /**
  * \return TRUE if the font will be embedde, FALSE otherwise
  */
  bool IsEmbedded() const;

  /// Check whether the font supports subsetting
  /**
  * \return TRUE if the font supports subsetting, FALSE otherwise
  */
  bool SupportsSubset() const;

  /// Write font data 
  /**
  * \param fontData the output stream
  * \param usedGlyphs the list of used glyphs
  * \param subsetGlyphs the mapping of glyphs to subset glyphs
  * \return the size of the written font data
  */
  size_t WriteFontData(wxOutputStream* fontData, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs);

  /// Write character/glyph to unicode mapping
  /**
  * \param mapData the output stream
  * \param usedGlyphs the list of used glyphs
  * \param subsetGlyphs the mapping of glyphs to subset glyphs
  * \return the size of the written data
  */
  size_t WriteUnicodeMap(wxOutputStream* mapData, wxPdfSortedArrayInt* usedGlyphs, wxPdfChar2GlyphMap* subsetGlyphs);

  /// Get the font description
  const wxPdfFontDescription& GetDescription() const;

  /// Get user font
  /**
  * \return the font description
  */
  wxPdfFont GetUserFont() const;

  /// Check whether the font has an encoding map
  /**
  * \return TRUE if the font has an encoding map, FALSE otherwise
  */
  bool HasEncodingMap() const;

#if wxUSE_UNICODE
  /// Get the associated encoding converter
  /**
  * \return the encoding converter associated with this font
  */
  virtual wxMBConv* GetEncodingConv() const;
#endif

  bool HasVoltData() const;

  wxString ApplyVoltData(const wxString& txt) const;

protected:

private:
  // Constructor
  wxPdfFontExtended(const wxPdfFontData* fontData);

  bool                 m_embed;        ///< Flag whether the font should be embedded
  bool                 m_subset;       ///< Flag whether the font should be subsetted
  wxPdfFontData*       m_fontData;     ///< Real font data
  const wxPdfEncoding* m_encoding;     ///< Font encoding for Type1 fonts

  friend class wxPdfFontManagerBase;
};

#endif

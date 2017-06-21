///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontdataopentype.h
// Purpose:    Definition of font data for OpenType fonts
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-05-15
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontdataopentype.h Definition of font data for OpenType fonts

#ifndef _PDF_FONT_DATA_OPENTYPE_H_
#define _PDF_FONT_DATA_OPENTYPE_H_

// OpenType fonts are only available in Unicode build
#if wxUSE_UNICODE

// wxWidgets headers
#include <wx/strconv.h>
#include <wx/stream.h>
#include <wx/string.h>
#include <wx/xml/xml.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdfarraytypes.h"
#include "wex/pdf/pdffontdata.h"

/// Class representing Unicode OpenType fonts. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfFontDataOpenTypeUnicode : public wxPdfFontData
{
public:
  ///< Default constructor
  wxPdfFontDataOpenTypeUnicode();

  /// Default destructor
  virtual ~wxPdfFontDataOpenTypeUnicode();

  /// Initialize font data
  /**
  * \return TRUE if the font data are initialized, FALSE otherwise
  */
  virtual bool Initialize();

  /// Get the width of a string
  /**
  * \param s the string for which the width should be calculated
  * \param encoding the character to glyph mapping
  * \param withKerning flag indicating whether kerning should be taken into account
  * \return the width of the string
  */
  virtual double GetStringWidth(const wxString& s, const wxPdfEncoding* encoding = NULL, bool withKerning = false) const;

  /// Check whether the font oan show all characters of a given string
  /**
  * \param s the string to be checked
  * \param encoding the character to glyph mapping
  * \return TRUE if the font can show all characters of the string, FALSE otherwise
  */
  virtual bool CanShow(const wxString& s, const wxPdfEncoding* encoding = NULL) const;

  /// Convert character codes to glyph numbers
  /**
  * \param s the string to be converted
  * \param encoding the character to glyph mapping
  * \param usedGlyphs the list of used glyphs
  * \param subsetGlyphs the mapping of glyphs to subset glyphs
  * \return the converted string
  */
  virtual wxString ConvertCID2GID(const wxString& s, const wxPdfEncoding* encoding = NULL, 
                                  wxPdfSortedArrayInt* usedGlyphs = NULL, 
                                  wxPdfChar2GlyphMap* subsetGlyphs = NULL) const;

  /// Convert glyph number to string
  /**
  * \param glyph the glyph to be converted
  * \param encoding the character to glyph mapping
  * \param usedGlyphs the list of used glyphs
  * \param subsetGlyphs the mapping of glyphs to subset glyphs
  * \return the converted string
  */
  virtual wxString ConvertGlyph(wxUint32 glyph, const wxPdfEncoding* encoding = NULL, 
                                wxPdfSortedArrayInt* usedGlyphs = NULL, 
                                wxPdfChar2GlyphMap* subsetGlyphs = NULL) const;

  /// Get the character width array as string
  /**
  * \param subset flag whether subsetting is enabled
  * \param usedGlyphs the list of used glyphs
  * \param subsetGlyphs the mapping of glyphs to subset glyphs
  * \return the string representation of the character widths
  */
  virtual wxString GetWidthsAsString(bool subset, wxPdfSortedArrayInt* usedGlyphs = NULL, wxPdfChar2GlyphMap* subsetGlyphs = NULL) const;

  /// Load the font metrics XML file
  /**
  * \param root the root node of the XML font metric file
  * \return TRUE if the metric file could be processed successfully, FALSE otherwise
  */
  virtual bool LoadFontMetrics(wxXmlNode* root);

  /// Write font data 
  /**
  * \param fontData the output stream
  * \param usedGlyphs the list of used glyphs
  * \param subsetGlyphs the mapping of glyphs to subset glyphs
  * \return the size of the written font data
  */
  virtual size_t WriteFontData(wxOutputStream* fontData, 
                               wxPdfSortedArrayInt* usedGlyphs = NULL, 
                               wxPdfChar2GlyphMap* subsetGlyphs = NULL);

  /// Write character/glyph to unicode mapping
  /**
  * \param mapData the output stream
  * \param encoding the character to glyph mapping
  * \param usedGlyphs the list of used glyphs
  * \param subsetGlyphs the mapping of glyphs to subset glyphs
  * \return the size of the written data
  */
  virtual size_t WriteUnicodeMap(wxOutputStream* mapData, 
                                 const wxPdfEncoding* encoding = NULL, 
                                 wxPdfSortedArrayInt* usedGlyphs = NULL, 
                                 wxPdfChar2GlyphMap* subsetGlyphs = NULL);

  /// Get the associated encoding converter
  /**
  * \return the encoding converter associated with this font
  */
  virtual wxMBConv* GetEncodingConv() const { return m_conv; }

  /// Create the associated default encoding converter
  virtual void CreateDefaultEncodingConv();

  /// Set the offset of the CFF section
  /**
  * \param cffOffset the offset of the CFF section
  */
  void SetCffOffset(size_t cffOffset) { m_cffOffset = cffOffset; }

  /// Set the lenght of the CFF section
  /**
  * \param cffLength the offset of the CFF section
  */
  void SetCffLength(size_t cffLength) { m_cffLength = cffLength; }

  /// Set glyph widths array
  /**
  * \param glyphWidths array with glyph widths
  */
  virtual void SetGlyphWidths(const wxPdfArrayUint16& glyphWidths);

protected:
  wxPdfArrayUint16*   m_gw;           ///< Array of glyph widths
  wxMBConv*           m_conv;         ///< Associated encoding converter
};

#endif // wxUSE_UNICODE

#endif

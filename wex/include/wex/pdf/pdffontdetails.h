///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontdetails.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-17
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontdetails.h Interface of wxPdfFontDetails class

#ifndef _PDF_FONT_DETAILS_H_
#define _PDF_FONT_DETAILS_H_

// wxWidgets headers

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdfarraytypes.h"
#include "wex/pdf/pdffontdata.h"
#include "wex/pdf/pdffontextended.h"

class WXDLLIMPEXP_FWD_PDFDOC wxPdfFont;

/// Class representing the font manager
class wxPdfFontDetails
{
public:
  /// Constructor
  /**
  * \param index the index of the font
  * \param font the associated font
  */
  wxPdfFontDetails(int index, const wxPdfFont& font);

  /// Default destructor
  ~wxPdfFontDetails();

  /// Copy destructor
  wxPdfFontDetails(const wxPdfFontDetails&);

  /// Assignment operator
  wxPdfFontDetails& operator=(const wxPdfFontDetails&);
  
  /// Set font index
  /**
  * \param index the font index
  */
  void SetIndex(int index) { m_index = index; }

  /// Get font index
  /**
  * \return the font index
  */
  int  GetIndex() const { return m_index; }

  /// Set font object index
  /**
  * \param n the PDF object index
  */
  void SetObjIndex(int n) { m_n = n; }

  /// Get font object index
  /**
  * \return the PDF object index
  */
  int  GetObjIndex() const { return m_n; }

  /// Set font file index
  /**
  * \param fn the index of the associated file object
  */
  void SetFileIndex(int fn) { m_fn = fn; }

  /// Get font file index
  /**
  * \return the index of the associated file object
  */
  int  GetFileIndex() const { return m_fn; }

  /// Set encoding differences index
  /**
  * \param ndiff the index of the associated differences object
  */
  void SetDiffIndex(int ndiff) { m_ndiff = ndiff; }

  /// Get encoding differences index
  /**
  * \return the index of the associated differences object
  */
  int  GetDiffIndex() const { return m_ndiff; }

  /// Get font type
  /**
  * \return the font type
  */
  wxString GetType() const;

  /// Get font name
  /**
  * \return the font name (including subset prefix if appropriate)
  */
  wxString GetName() const;

  /// Get original font name
  /**
  * \return the original font name
  */
  wxString GetOriginalName() const;

  /// Get font family
  /**
  * \return the font family
  */
  wxString GetFontFamily() const;

  /// Get the font description
  /**
  * \return the font description
  */
  const wxPdfFontDescription& GetDescription() const;

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

  /// Get the name of the base encoding of the font
  /**
  * \return the name of the base encoding
  */
  wxString GetBaseEncoding() const;

  /// Get the font as an extended font object
  /**
  * \returns the extended font
  */
  wxPdfFontExtended GetFont() const;

  /// Get the font as a user font object
  /**
  * \return the user font
  */
  wxPdfFont GetUserFont() const;

  /// Create a name prefix for a subset
  /**
  * \return the subset prefix
  */
  wxString CreateSubsetPrefix() const;

  /// Get the width of a string
  /**
  * \param s string for which the width should be calculated
  * \param withKerning flag whether kerning should be taken into account
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

  /// Convert character codes to glyph numbers
  /**
  * \param s the string to be converted
  * \return the converted string
  */
  wxString ConvertCID2GID(const wxString& s);

  /// Convert glyph number to string
  /**
  * \param glyph the glyph to be converted
  * \return the converted string
  */
  wxString ConvertGlyph(wxUint32 glyph);

  /// Get the character width array as string
  /**
  * \return the string representation of the character widths
  */
  wxString GetWidthsAsString() const;

  /// Write font data 
  /**
  * \param fontData the output stream
  * \return the size of the written font data
  */
  size_t WriteFontData(wxOutputStream* fontData);

  /// Write character/glyph to unicode mapping
  /**
  * \param mapData the output stream
  * \return the size of the written data
  */
  size_t WriteUnicodeMap(wxOutputStream* mapData);

#if wxUSE_UNICODE
  /// Get the associated encoding converter
  /**
  * \return the encoding converter associated with this font
  */
  wxMBConv* GetEncodingConv() const;
#endif

protected:
  int                  m_index; ///< Index number of this font
  int                  m_n;     ///< Font object index
  int                  m_fn;    ///< Font file index
  int                  m_ndiff; ///< Index of encoding differences object

  wxPdfFontExtended    m_font;         ///< Extended font for accessing font data
  wxPdfSortedArrayInt* m_usedGlyphs;   ///< Array of used characters
  wxPdfChar2GlyphMap*  m_subsetGlyphs; ///< Glyph substitution map for font subsets

private:
  /// Default constructor not available
  wxPdfFontDetails();
};

#endif

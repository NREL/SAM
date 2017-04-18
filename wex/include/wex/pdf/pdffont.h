///////////////////////////////////////////////////////////////////////////////
// Name:        pdffont.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-10
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffont.h Interface of wxPdfFont class

#ifndef _PDF_FONT_H_
#define _PDF_FONT_H_

// wxWidgets headers
#include <wx/mstream.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdffontdescription.h"
#include "wex/pdf/pdfproperties.h"

class WXDLLIMPEXP_FWD_PDFDOC wxPdfEncoding;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfFontData;

/// Class representing a font providing a basic interface
class WXDLLIMPEXP_PDFDOC wxPdfFont
{
public :
  /// Default constructor
  wxPdfFont() ;

  /// Default destructor
  virtual ~wxPdfFont() ;

  /// Copy constructor
  wxPdfFont(const wxPdfFont& font);

  /// Assignment
  wxPdfFont& operator=(const wxPdfFont& font);

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

  /// Get encoding
  /**
  * \return the name of the font encoding
  */
  wxString GetEncoding() const;

  /// Get the width of a string
  /**
  * \param s string which's width is to be returned
  * \return the width of the string
  */
  virtual double GetStringWidth(const wxString& s) const;

  /// Check whether the font embedding is required
  /**
  * \return TRUE if embedding of this font is required, FALSE otherwise
  */
  bool EmbedRequired() const;

  /// Check whether the font embedding is supported
  /**
  * \return TRUE if embedding of this font is supported/allowed, FALSE otherwise
  */
  bool EmbedSupported() const;

  /// Check whether the font subsetting is supported
  /**
  * \return TRUE if subsetting of this font is supported/allowed, FALSE otherwise
  */
  bool SubsetSupported() const;

  /// Get the font description
  /**
  * \return the font description
  */
  const wxPdfFontDescription GetDescription() const;

  /// Set embed flag
  /**
  * Enables or disables embedding for the font. Embedding can be enabled if and only if
  * the font allows embedding. Embedding can be disabled if and only if the font does
  * not requires embedding.
  * \param embed Indicates whether to embed or not embed the font
  * \see EmbedRequired(), EmbedSupported()
  */
  void SetEmbed(bool embed);

  /// Get embed flag
  /**
  * \return TRUE if the font will be embedded, FALSE otherwise
  * \see EmbedRequired(), EmbedSupported()
  */
  bool GetEmbed() const { return m_embed; }

  /// Set subset flag 
  /**
  * Enables or disables subsetting for the font. Subsetting can be enabled if and only if
  * the font allows subsetting.
  * \param subset indicates whether to subset or not subset the font
  * \see SubsetSupported()
  */
  void SetSubset(bool subset);

  /// Get subset flag
  /**
  * \return TRUE if the font will be subsetted, FALSE otherwise
  * \see SubsetSupported()
  */
  bool GetSubset() const { return m_subset; }

  /// Set encoding
  /**
  * For Type1 and non-Unicode TrueType fonts it is possible to overwrite the default encoding
  * of the font. It's the user's responsibility to ensure the font supports all characters
  * assigned by the encoding.
  * \param encodingName the name of the encoding to use with this font
  * \return TRUE if the encoding could be assigned, FALSE otherwise
  * \note The encoding has to be registered to the font manager in advance
  */
  bool SetEncoding(const wxString& encodingName);

  /// Get encoding
  /**
  * For Type1 and non-Unicode TrueType fonts it is possible to overwrite the default encoding
  * of the font. It's the user's responsibility to ensure the font supports all characters
  * assigned by the encoding.
  * \param[out] encoding the encoding used with this font
  * \return TRUE if an encoding is associated with the font, FALSE otherwise
  */
  bool GetEncoding(wxPdfEncoding& encoding);

  /// Check whether the font oan show all characters of a given string
  /**
  * \param s the string to be checked
  * \return TRUE if the font can show all characters of the string, FALSE otherwise
  */
  bool CanShow(const wxString& s);

  /// Get list of supported unicode characters
  /**
  * \param unicodeCharacters list of supported unicode characters on return
  * \return TRUE if the list could be determined, FALSE otherwise
  * \note Any previous content of the Unicode character list will be deleted
  */
  bool GetSupportedUnicodeCharacters(wxPdfArrayUint32& unicodeCharacters) const;

  /// Force string to valid string in respect of the current font encoding
  /**
  * The given string is converted in such a way that it contains only characters
  * available in the current font encoding
  * \param s the string to be converted
  * \param replace the character used to replace invalid characters
  * \return converted string
  */
  wxString ConvertToValid(const wxString& s, wxChar replace = wxT('?')) const;

  /// Get list of supported glyph names
  /**
  * For dynamically loaded Type1 fonts the list of supported glyph names is provided.
  * For all other font types this information is not available.
  * \return TRUE if the list of glyph names is available, otherwise FALSE
  */
  bool GetGlyphNames(wxArrayString& glyphNames) const;

private:
  /// Constructor creating a reference to the real font data
  wxPdfFont(wxPdfFontData* fontData, int fontStyle = wxPDF_FONTSTYLE_REGULAR);

  bool                 m_embed;     ///< Flag whether the font should be embedded
  bool                 m_subset;    ///< Flag whether the font should be subsetted
  int                  m_fontStyle; ///< Font style flags
  wxPdfFontData*       m_fontData;  ///< Real font data
  const wxPdfEncoding* m_encoding;  ///< Font encoding for Type1 fonts

  friend class wxPdfFontExtended;
  friend class wxPdfFontManagerBase;
};

#endif

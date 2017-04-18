///////////////////////////////////////////////////////////////////////////////
// Name:        pdfencoding.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-05-30
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfencoding.h Definition of encoding class

#ifndef _PDF_ENCODING_H_
#define _PDF_ENCODING_H_

// wxWidgets headers
#include <wx/string.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdfarraytypes.h"
#include "wex/pdf/pdffontdata.h"

/// Class representing a font encoding
class WXDLLIMPEXP_PDFDOC wxPdfEncoding
{
public:
  /// Default constructor
  wxPdfEncoding();

  /// Destructor
  virtual ~wxPdfEncoding();

  /// Copy constructor
  wxPdfEncoding(const wxPdfEncoding& encoding);

  /// Assignment operator
  wxPdfEncoding& operator=(const wxPdfEncoding& encoding);

  /// Check whether the encoding is valid
  /**
  * \return TRUE if the encoding is valid, FALSE otherwise
  */
  bool IsOk() const;

  /// Set the encoding
  /**
  * \param encoding the encoding that should be set
  */
  bool SetEncoding(const wxString& encoding);

  /// Get the name of the encoding
  /**
  * \return the name of the encoding
  */
  wxString GetEncodingName() const;

  /// Get the name of the base encoding
  /**
  * \return the name of the base encoding
  */
  wxString GetBaseEncodingName() const;

  /// Get the differences between the encoding and the associated base encoding
  /**
  * \return the difference string
  */
  wxString GetDifferences() const;

  /// Get the character map of the encoding
  /**
  * \return the character map
  */
  wxPdfArrayUint32 GetCMap() const;

  /// Initialize the encoding map
  void InitializeEncodingMap();

  /// Get the encoding map
  /**
  * \return the encoding map
  */
  const wxPdfChar2GlyphMap* GetEncodingMap() const;

  /// Get the list of glyph names supported by the encoding
  /**
  * \return a list of glyph names
  */
  wxArrayString GetGlyphNames() const;

  /// Convert a glyph name to a unicode character
  /**
  * \param[in] glyphName name of the glyph
  * \param[out] unicode unicode character of the glyph
  * \return TRUE if the conversion was successful, FALSE otherwise
  */
  static bool GlyphName2Unicode(const wxString& glyphName, wxUint32& unicode);

  /// Convert a unicode character to a glyph name
  /**
  * \param[in] unicode unicode character of the glyph
  * \param[out] glyphName name of the glyph
  * \return TRUE if the conversion was successful, FALSE otherwise
  */
  static bool Unicode2GlyphName(wxUint32 unicode, wxString& glyphName);

  /// Get a list of known encodings
  /**
  * \return an array containing all known encodings
  */
  static wxArrayString GetKnownEncodings();

protected:
  /// Create the encoding conversion map for user defined encodings
  void CreateEncodingConvMap();

private:
  wxString            m_encoding;         ///< name of the encoding
  wxString            m_baseEncoding;     ///< name of the base encoding
  wxPdfArrayUint32    m_cmap;             ///< character map of the encoding
  wxPdfArrayUint32    m_cmapBase;         ///< character map of the base encoding
  wxArrayString       m_glyphNames;       ///< array of glyph names in the encoding
  bool                m_specific;         ///< flag whether the encoding is specific
  int                 m_firstChar;        ///< number of the first valid character
  int                 m_lastChar;         ///< number of the last valid character
  wxPdfChar2GlyphMap* m_encodingMap;      ///< Encoding conversion map
};

/// Class representing a font encoding checker
class WXDLLIMPEXP_PDFDOC wxPdfEncodingChecker
{
public:
  /// Default constructor
  wxPdfEncodingChecker();

  /// Destructor
  virtual ~wxPdfEncodingChecker();

  /// Get the name of the encoding used for checking
  /**
  * \return the name of the encoding
  */
  wxString GetEncodingName() const;

  /// Check whether a given Unicode character is included in the encoding
  /**
  * \return TRUE if the Unicode character is included, FALSE otherwise
  */
  virtual bool IsIncluded(wxUint32 unicode) const = 0;

protected:
  wxString         m_encoding;         ///< name of the encoding
};

#endif

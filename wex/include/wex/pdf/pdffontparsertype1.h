///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontparsertype1.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-05-24
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontparsertype1.h Interface of the Type1 font parsing support class

#ifndef _PDF_FONT_PARSER_TYPE1_H_
#define _PDF_FONT_PARSER_TYPE1_H_

// wxWidgets headers
#include <wx/arrstr.h>
#include <wx/string.h>
#include <wx/stream.h>

#if wxUSE_UNICODE

// wxPdfDocument headers
#include "wex/pdf/pdffontdatatype1.h"
#include "wex/pdf/pdffontdescription.h"
#include "wex/pdf/pdffontparser.h"

class wxPdfCffIndexArray;

/// Class representing a Type1 font parser
class WXDLLIMPEXP_PDFDOC wxPdfFontParserType1 : public wxPdfFontParser
{
public:
  /// Default constructor
  wxPdfFontParserType1();

  /// Default destructor
  virtual ~wxPdfFontParserType1();

  /// Identify font based on a font file
  /**
  * \param fontFileName fully qualified name of the font file
  * \param fontIndex the index of the font within a font collection (if appropriate)
  * \return a reference to a new font data instance
  */
  wxPdfFontData* IdentifyFont(const wxString& fontFileName, int fontIndex);

  /// Parse font stream
  /**
  * \param fileName the name of the file containing the font data
  * \param fontFile the stream containing the font data
  * \param metricFile the stream containing the font metric data
  * \param onlyNames flag whether only the font names should be parsed or the full font data
  * \return TRUE if the parsing was successful, FALSE otherwise
  */
  bool ParseFont(const wxString& fileName, wxInputStream* fontFile, wxInputStream* metricFile, bool onlyNames);

  /// Load the font data
  /**
  * Before using a font it's data have be loaded into memory. This method tries
  * to load the required font data for a previously identified font.
  * \param fontData the font data instance to be loaded
  * \return TRUE if the font data could be loaded successfully, FALSE otherwise
  */
  bool LoadFontData(wxPdfFontData* fontData);

  /// Read font metrics from Adobe Font Metric file
  /**
  * \param afmFile stream containing the font metric data
  * \return TRUE if the font metrics are valid, FALSE otherwise
  */
  bool ReadAFM(wxInputStream& afmFile);

  /// Read font metrics from Printer Font Metric file
  /**
  * \param pfmFile stream containing the font metric data
  * \return TRUE if the font metrics are valid, FALSE otherwise
  */
  bool ReadPFM(wxInputStream& pfmFile);

  /// Read font data from font file
  /**
  * \param pfxFile stream containing the font data
  * \param onlyNames flag whether only the font names should be parsed or the full font data
  * \return TRUE if the font metrics are valid, FALSE otherwise
  * \note Type1 fonts exist in either PFA (PostScript Font ASCII) or PFB (PostScript Font Binary) format.
  * Although this method is capable of reading either format, currently only fonts in PFB format
  * can be used in creating PDF documents.
  */
  bool ReadPFX(wxInputStream* pfxFile, bool onlyNames);

private:
  /// Convert Mac Type1 font file to PFB format
  wxMemoryOutputStream* ConvertMACtoPFB(wxInputStream* macFontStream);

  /// Check whether the font stream represents a Type1 font
  bool CheckType1Format(wxInputStream* stream, int& start, int& length);

  /// Check whether the stream represents font metric data in AFM format
  bool MetricIsAFM(wxInputStream* stream);

  /// Check whether the stream represents font metric data in PFM format
  bool MetricIsPFM(wxInputStream* stream);

  /// Identify a data block in a PFB font stream
  bool ReadPfbTag(wxInputStream* stream, unsigned char& blocktype, int& blocksize);

  /// Check whether the font's use is restricted
  void CheckRestrictions(long fsType);

  /// Skip a PostScript comment in the font stream
  void SkipComment(wxInputStream* stream);

  /// Skip white space in the font stream
  void SkipSpaces(wxInputStream* stream);

  /// Skip a literal string in the font stream
  void SkipLiteralString(wxInputStream* stream);

  /// Skip a string in the font stream
  void SkipString(wxInputStream* stream);

  /// Skip a PostScript procedure in the font stream
  void SkipProcedure(wxInputStream* stream);

  /// Skip a PostScript array in the font stream
  void SkipArray(wxInputStream* stream);

  /// Skip all bytes in the stream up to the next token
  void SkipToNextToken(wxInputStream* stream);

  /// Get the next token from the font stream
  wxString GetToken(wxInputStream* stream);

  /// Get a literal string from the font stream
  wxString GetLiteralString(wxInputStream* stream);

  /// Get a value array from the font stream
  wxString GetArray(wxInputStream* stream);

  /// Parse a PostScript dictionary
  bool ParseDict(wxInputStream* stream, int start, int length, bool onlyNames);

  /// Check whether a character represents a valid hexadecimal digit 
  bool IsHexDigit(char digit);

  /// Decode a hexadecimal coded string
  void DecodeHex(wxInputStream* inStream, wxOutputStream* outStream);

  /// Decode the EExec section
  void DecodeEExec(wxMemoryOutputStream* eexecStream, wxOutputStream* outStream, unsigned short seed, int lenIV);

  /// Get the private dictionary 
  bool GetPrivateDict(wxInputStream* stream, int start);

  /// Parse the font matrix from the font stream
  void ParseFontMatrix(wxInputStream* stream);

  /// Parse the font encoding from the font stream
  void ParseEncoding(wxInputStream* stream);

  /// Parse the array of subroutines from the font stream
  void ParseSubrs(wxInputStream* stream);

  /// Parse the charstrings from the font stream
  void ParseCharStrings(wxInputStream* stream);

  /// Parse the private dictionary
  void ParsePrivate(wxInputStream* stream);

  /// Read a binary section from the font stream
  void ReadBinary(wxInputStream& inStream, int start, int size, wxOutputStream& outStream);

  bool                         m_skipArray;            ///< Flag whether to skip arrays when parsing a dictionary

  wxPdfFontDescription         m_fontDesc;             ///< font description
  wxPdfFontDataType1*          m_fontData;             ///< font data
  wxArrayString                m_encodingVector;       ///< encoding vector of the font
  wxPdfFontType1GlyphWidthMap* m_glyphWidthMap;        ///< map of glyph widths
  int                          m_missingWidth;         ///< width of missing characters

  bool                         m_embedAllowed;         ///< Flag whether embedding is allowed
  bool                         m_subsetAllowed;        ///< Flag whether subsetting is allowed

  wxInputStream*               m_privateDict;          ///< Private dictionary stream
  bool                         m_isPFB;                ///< Flag whether the font data are in PFB format
  bool                         m_privateFound;         ///< Flag whether the private dictionary has been found
  bool                         m_fontDirAfterPrivate;  ///< Flag whether the font directory is located before or after the private dictionary
  wxString                     m_encoding;             ///< name of the encoding
  long                         m_lenIV;                ///< initial vector length (decoding)

  wxPdfCffIndexArray*          m_charStringsIndex;     ///< charstring index
  wxPdfCffIndexArray*          m_subrsIndex;           ///< subroutine index
};

#endif // wxUSE_UNICODE

#endif

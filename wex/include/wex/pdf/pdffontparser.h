///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontparser.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2007-06-26
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontparser.h Interface of the TrueType/OpenType font parsing support classes

#ifndef _PDF_FONT_PARSER_H_
#define _PDF_FONT_PARSER_H_

// wxWidgets headers
#include <wx/arrstr.h>
#include <wx/string.h>
#include <wx/stream.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

/// Class representing font parsers
class WXDLLIMPEXP_PDFDOC wxPdfFontParser
{
public:
  /// Default constructor
  wxPdfFontParser();

  /// Default destructor
  virtual ~wxPdfFontParser();

  /// Seek to offset in default input stream
  /**
  * \param offset offset to be seeked
  */
  void SeekI(int offset);

  /// Seek to offset in specific input stream
  /**
  * \param offset offset to be seeked
  * \param stream input stream
  */
  void SeekI(int offset, wxInputStream* stream);

  /// Tell current position in default input stream
  /**
  * \return the current position in the default input stream
  */
  int TellI();

  /// Tell current position in specific input stream
  /**
  * \param stream input stream
  * \return the current position in the input stream
  */
  int TellI(wxInputStream* stream);

  /// Skip bytes in default input stream
  /**
  * \param count number of bytes to skip
  */
  void SkipBytes(int count);

  /// Skip bytes in specific input stream
  /**
  * \param count number of bytes to skip
  * \param stream input stream
  */
  void SkipBytes(int count, wxInputStream* stream);

  /// Read integer from default input stream
  /**
  * \return the integer read
  */
  int ReadInt();

  /// Read short integer from default input stream
  /**
  * \return the short integer read
  */
  short ReadShort();

  /// Read unsigned short integer from default input stream
  /**
  * \return the unsigned short integer read
  */
  unsigned short ReadUShort();

  /// Read byte from default input stream
  /**
  * \return the byte read
  */
  unsigned char ReadByte();

  /// Read byte from specific input stream
  /**
  * \param stream input stream
  * \return the byte read
  */
  unsigned char ReadByte(wxInputStream* stream);

  /// Read string from default input stream
  /**
  * \param length the length of the string to read
  * \return the string read
  */
  wxString ReadString(int length);

  /// Read string from specific input stream
  /**
  * \param length the length of the string to read
  * \param stream input stream
  * \return the string read
  */
  wxString ReadString(int length, wxInputStream* stream);

  /// Read Unicode string from default input stream
  /**
  * \param length the length of the string to read
  * \return the string read
  */
  wxString ReadUnicodeString(int length);

  /// Read short integer from specific input stream in little endian mode
  /**
  * \param stream input stream
  * \return the short integer read
  */
  short ReadShortLE(wxInputStream* stream);

  /// Read unsigned short integer from specific input stream in little endian mode
  /**
  * \param stream input stream
  * \return the unsigned short integer read
  */
  unsigned short ReadUShortLE(wxInputStream* stream);

  /// Read unsigned integer from specific input stream in little endian mode
  /**
  * \param stream input stream
  * \return the unsigned integer read
  */
  unsigned int ReadUIntLE(wxInputStream* stream);

  /// Read string from specific input stream
  /**
  * \param stream input stream
  * \return the string read
  */
  wxString ReadString(wxInputStream& stream);

protected:
  wxString              m_fileName; ///< File name of the font file
  wxString              m_fontName; ///< Name of font
  wxInputStream*        m_inFont;   ///< Font file input stream
  wxString              m_style;    ///< Font style

private:
};

#endif

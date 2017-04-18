///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontparser.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2007-06-26
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontparser.cpp Implementation of font parsing support classes

// For compilers that support precompilation, includes <wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

// includes
#include <wx/zstream.h>

#include "wex/pdf/pdffontparser.h"

#include "wxmemdbg.h"

// --- Font parser base

wxPdfFontParser::wxPdfFontParser()
{
  m_fileName = wxEmptyString;
  m_inFont = NULL;
}

wxPdfFontParser::~wxPdfFontParser()
{
}

void
wxPdfFontParser::SeekI(int offset)
{
  SeekI(offset, m_inFont);
}

void
wxPdfFontParser::SeekI(int offset, wxInputStream* stream)
{
  stream->SeekI(offset);
}

int
wxPdfFontParser::TellI()
{
  return TellI(m_inFont);
}

int
wxPdfFontParser::TellI(wxInputStream* stream)
{
  return stream->TellI();
}

void
wxPdfFontParser::SkipBytes(int count)
{
  SkipBytes(count, m_inFont);
}

void
wxPdfFontParser::SkipBytes(int count, wxInputStream* stream)
{
  if (stream != NULL)
  {
    stream->SeekI(count, wxFromCurrent);
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfFontParser::SkipBytes: ")) +
               wxString(_("Input stream not set.")));
  }
}

int
wxPdfFontParser::ReadInt()
{
  // Read a 4-byte integer from file (big endian)
  int i32;
  m_inFont->Read(&i32, 4);
  return wxINT32_SWAP_ON_LE(i32);
}

short
wxPdfFontParser::ReadShort()
{
  // Read a 2-byte integer from file (big endian)
  short i16;
  m_inFont->Read(&i16, 2);
  return wxINT16_SWAP_ON_LE(i16);
}

unsigned short
wxPdfFontParser::ReadUShort()
{
  // Read a unsigned 2-byte integer from file (big endian)
  unsigned short i16;
  m_inFont->Read(&i16, 2);
  return wxUINT16_SWAP_ON_LE(i16);
}

unsigned char
wxPdfFontParser::ReadByte()
{
  return ReadByte(m_inFont);
}

unsigned char
wxPdfFontParser::ReadByte(wxInputStream* stream)
{
  unsigned char card8;
  stream->Read(&card8, 1);
  return card8;
}

wxString
wxPdfFontParser::ReadString(int length)
{
  return ReadString(length, m_inFont);
}
    
wxString
wxPdfFontParser::ReadString(int length, wxInputStream* stream)
{
  char* buffer = new char[length];
  stream->Read(buffer, length);
  wxString str = wxString(buffer, wxConvISO8859_1, length);
  delete [] buffer;
  return str;
}
    
wxString
wxPdfFontParser::ReadUnicodeString(int length)
{
  wxMBConvUTF16BE conv;
  char* buffer = new char[length];
  m_inFont->Read(buffer, length);
  wxString str = wxString(buffer, conv, length);
  delete [] buffer;
  return str;
}

wxString
wxPdfFontParser::ReadString(wxInputStream& fileStream)
{
  wxString str = wxEmptyString;
  unsigned char c;
  int maxlen = 255;
  int j = 0;
  do
  {
    fileStream.Read(&c, 1);
#if wxCHECK_VERSION(2,9,0)
    if (c > 0) str += wxUniChar((unsigned int) c);
#else
    if (c > 0) str += wxChar(c);
#endif
    j++;
  }
  while (c > 0 && j < maxlen);
  return str;
}

short
wxPdfFontParser::ReadShortLE(wxInputStream* stream)
{
  // Read a 2-byte integer from file (little endian)
  short i16;
  stream->Read(&i16, 2);
  return wxINT16_SWAP_ON_BE(i16);
}

unsigned short
wxPdfFontParser::ReadUShortLE(wxInputStream* stream)
{
  // Read a unsigned 2-byte integer from file (little endian)
  unsigned short i16;
  stream->Read(&i16, 2);
  return wxUINT16_SWAP_ON_BE(i16);
}

unsigned int
wxPdfFontParser::ReadUIntLE(wxInputStream* stream)
{
  // Read a unsigned 4-byte integer from file (little endian)
  unsigned int i32;
  stream->Read(&i32, 4);
  return wxUINT32_SWAP_ON_BE(i32);
}

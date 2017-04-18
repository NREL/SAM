///////////////////////////////////////////////////////////////////////////////
// Name:        pdfcffindex.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-07
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfcffindex.cpp Implementation of index structure for CFF  fonts

// For compilers that support precompilation, includes <wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

// includes

#include "wex/pdf/pdfcffindex.h"

#include "wxmemdbg.h"

#include <wx/arrimpl.cpp> // this is a magic incantation which must be done!
WX_DEFINE_OBJARRAY(wxPdfCffIndexArray);

wxPdfCffIndexElement::wxPdfCffIndexElement(wxInputStream* buf, int offset, int length)
{
  m_offset = offset;
  m_length = length;
  m_buf    = buf;
  m_delete = false;
}

wxPdfCffIndexElement::wxPdfCffIndexElement(wxMemoryOutputStream& buf)
{
  buf.Close();
  m_buf    = new wxMemoryInputStream(buf);
  m_offset = 0;
  m_length = (int) m_buf->GetSize();
  m_delete = true;
}

wxPdfCffIndexElement::wxPdfCffIndexElement(const char* str)
{
  wxMemoryOutputStream buf;
  buf.Write(str, strlen(str));
  buf.Close();
  m_buf    = new wxMemoryInputStream(buf);
  m_offset = 0;
  m_length = (int) m_buf->GetSize();
  m_delete = true;
}

wxPdfCffIndexElement::~wxPdfCffIndexElement()
{
  if (m_delete && m_buf != NULL)
  {
    delete m_buf;
  }
}

wxPdfCffIndexElement::wxPdfCffIndexElement(const wxPdfCffIndexElement& copy)
{
  m_offset = copy.m_offset;
  m_length = copy.m_length;
  if (copy.m_delete)
  {
    wxMemoryOutputStream buffer;
    buffer.Write(*copy.m_buf);
    m_buf    = new wxMemoryInputStream(buffer);
    m_delete = true;
  }
  else
  {
    m_buf    = copy.m_buf;
    m_delete = copy.m_delete;
  }
}

wxPdfCffIndexElement& 
wxPdfCffIndexElement::operator=(const wxPdfCffIndexElement& copy)
{
  m_offset = copy.m_offset;
  m_length = copy.m_length;
  if (copy.m_delete)
  {
    wxMemoryOutputStream buffer;
    buffer.Write(*copy.m_buf);
    m_buf    = new wxMemoryInputStream(buffer);
    m_delete = true;
  }
  else
  {
    m_buf    = copy.m_buf;
    m_delete = copy.m_delete;
  }
  return *this;
}

void
wxPdfCffIndexElement::SetBuffer(wxMemoryOutputStream& buf)
{
  if (m_delete)
  {
    delete m_buf;
  }
  buf.Close();
  m_buf    = new wxMemoryInputStream(buf);
  m_offset = 0;
  m_length = (int) m_buf->GetSize();
  m_delete = true;
}

void
wxPdfCffIndexElement::Emit(wxMemoryOutputStream& buffer)
{
#if 0
  wxLogDebug(wxT("Emit: offset=%d length=%d"), m_offset, m_length);
#endif
  char locBuffer[1024];
  m_buf->SeekI(m_offset);
  int copyLength = m_length;
  int bufferLength;
  while (copyLength > 0)
  {
    bufferLength = (copyLength > 1024) ? 1024 : copyLength;
    m_buf->Read(locBuffer, bufferLength);
    buffer.Write(locBuffer, bufferLength);
    copyLength -= bufferLength;
#if 0
    wxString str;
    int kk;
    for (kk = 0; kk < bufferLength; kk++)
    {
      str += wxString::Format(wxT(" %d"), locBuffer[kk]);
      if (kk % 10 == 9)
      {
        wxLogDebug(str);
        str = wxEmptyString;
      }
    }
    if ((bufferLength-1) % 10 != 9)
    {
      wxLogDebug(str);
    }
#endif
  }
}

///////////////////////////////////////////////////////////////////////////////
// Name:        pdfcffindex.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-01
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfcffindex.h Interface of the index structure for  CFF fonts

#ifndef _PDF_CFF_INDEX_H_
#define _PDF_CFF_INDEX_H_

// wxWidgets headers
#include <wx/dynarray.h>
#include <wx/mstream.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

/// Class representing a CFF index element
class WXDLLIMPEXP_PDFDOC wxPdfCffIndexElement
{
public:
  /// Constructor
  /**
  * \param buf stream buffer containing the index element
  * \param offset offset of the index within the stream buffer
  * \param length length of the index
  */
  wxPdfCffIndexElement(wxInputStream* buf, int offset, int length);

  /// Constructor
  /**
  * \param buf stream buffer containing the index element
  */
  wxPdfCffIndexElement(wxMemoryOutputStream& buf);

  /// Constructor
  /**
  * \param str stream buffer containing the index element
  */
  wxPdfCffIndexElement(const char* str);

  /// Destructor
  virtual ~wxPdfCffIndexElement();

  /// Copy constructor
  wxPdfCffIndexElement(const wxPdfCffIndexElement& copy);

  /// Assignment operator
  wxPdfCffIndexElement& operator=(const wxPdfCffIndexElement& copy);

  /// Set the stream buffer
  /**
  * \param buf the stream buffer containing the index element
  */
  void SetBuffer(wxMemoryOutputStream& buf);
  
  /// Emit the index element into an output stream
  /**
  * \param[out] buffer the output stream
  */
  virtual void Emit(wxMemoryOutputStream& buffer);

  /// Check whether the stream buffer is a copy of another stream buffer
  /**
  * \return a flag whether the stream buffer
  */
  bool IsCopy() { return m_delete; }
  
  /// Get the offset of the index element
  /**
  * \return the offset within the stream buffer
  */
  int GetOffset() { return m_offset; }
  
  /// Get the length of the index element
  /**
  * \return the length of the index element
  */
  int GetLength() { return m_length; }
  
  /// Get the stream buffer
  /**
  * \return the stream buffer
  */
  wxInputStream* GetBuffer() { return m_buf; }

private:
  int            m_offset;  ///< offset of the index element
  int            m_length;  ///< length of the index element
  bool           m_delete;  ///< flag whether the stream buffer is a copy
  wxInputStream* m_buf;     ///< stream buffer containing the index
};

WX_DECLARE_OBJARRAY(wxPdfCffIndexElement, wxPdfCffIndexArray);

#endif

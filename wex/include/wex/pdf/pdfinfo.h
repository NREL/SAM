///////////////////////////////////////////////////////////////////////////////
// Name:        pdfinfo.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-06-24
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfinfo.h Interface of the wxPdfInfo class

#ifndef _PDF_INFO_H_
#define _PDF_INFO_H_

// wxWidgets headers
#include "wex/pdf/pdfdocdef.h"

/// Class representing a PDF document information dictionary.
class WXDLLIMPEXP_PDFDOC wxPdfInfo
{
public:
  /// Default constructor
  wxPdfInfo() {}

  /// Destructor
  virtual~wxPdfInfo() {}

  /// Set title
  void SetTitle(const wxString& title) { m_title = title; }

  /// Set author
  void SetAuthor(const wxString& author) { m_author = author; }

  /// Set subject
  void SetSubject(const wxString& subject) { m_subject = subject; }

  /// Set keywords
  void SetKeywords(const wxString& keywords) { m_keywords = keywords; }

  /// Set the creator of the document
  void SetCreator(const wxString& creator) { m_creator = creator; }

  /// Set the producer of the document
  void SetProducer(const wxString& producer) { m_producer = producer; }

  /// Set the creation date
  void SetCreationDate(const wxString& creationDate) { m_creationDate = creationDate; }

  /// Set the date of last modification
  void SetModDate(const wxString& modDate) { m_modDate = modDate; }

  /// Get title
  const wxString GetTitle() const { return m_title; }

  /// Get author
  const wxString GetAuthor() const { return m_author; }

  /// Get subject
  const wxString GetSubject() const { return m_subject; }

  /// Get keywords
  const wxString GetKeywords() const { return m_keywords; }

  /// Get the creator of the document
  const wxString GetCreator() const { return m_creator; }

  /// Get the producer of the document
  const wxString GetProducer() const { return m_producer; }

  /// Get the creation date
  const wxString GetCreationDate() const { return m_creationDate; }

  /// Get the date of last modification
  const wxString GetModDate() const { return m_modDate; }

private:
  wxString m_title;        ///< The document’s title.
  wxString m_author;       ///< The name of the person who created the document.
  wxString m_subject;      ///< The subject of the document.
  wxString m_keywords;     ///< Keywords associated with the document.
  wxString m_creator;      ///< The name of the application that created the original document.
  wxString m_producer;     ///< The name of the application that produced the document.
  wxString m_creationDate; ///< The date and time the document was created.
  wxString m_modDate;      ///< The date and time the document was modified.
};

#endif

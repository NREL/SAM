///////////////////////////////////////////////////////////////////////////////
// Name:        pdfbookmark.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-06-24
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfbookmark.h Interface of the wxPdfBookmark class

#ifndef _PDF_BOOKMARK_H_
#define _PDF_BOOKMARK_H_

// wxWidgets headers
#include <wx/string.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

/// Class representing bookmarks for defining the document's outline. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfBookmark
{
public:
  /// Constructor
  /**
  * \param txt  text associated with the bookmark
  * \param level hierarchical level
  * \param y offset on the referenced page
  * \param page number of the referenced page
  */
  wxPdfBookmark(const wxString& txt, int level, double y, int page);

  /// Destructor
  virtual ~wxPdfBookmark();

  /// Get the bookmark text
  /**
  * \return the text associated with the bookmark
  */
  wxString GetText() { return m_text; }

  /// Get the associated level
  /**
  * \return the level associated with the bookmark
  */
  int GetLevel() { return m_level; }

  /// Get the Y offset of the bookmark
  /**
  * \return the page offset of the bookmark
  */
  double GetY() { return m_y; }

  /// Get the page number of the bookmark
  /**
  * \return the number of the page referenced by the bookmark
  */
  int GetPage() { return m_page; }

  /// Set the parent of the bookmark
  /**
  * \param parent the number of the parent in the bookmark hierarchy
  */
  void SetParent(int parent) { m_parent = parent; }

  /// Get the parent of the bookmark
  /**
  * \return the number of the parent of the bookmark in the hierarchy
  */
  int GetParent() { return m_parent; }

  /// Set previous bookmark
  /**
  * \param prev the number of the predecessor on the same level in the bookmark hierarchy
  */
  void SetPrev(int prev) { m_prev = prev; }

  /// Get previous bookmark
  /**
  * \return the number of the predecessor of the bookmark on the same level in the hierarchy
  */
  int GetPrev() { return m_prev; }

  /// Set next bookmark
  /**
  * \param next the number of the successor on the same level in the bookmark hierarchy
  */
  void SetNext(int next) { m_next = next; }

  /// Get next bookmark
  /**
  * \return the number of the successor of the bookmark on the same level in the hierarchy
  */
  int GetNext() { return m_next; }

  /// Set first bookmark
  /**
  * \param first the number of the first bookmark on the same level in the hierarchy
  */
  void SetFirst(int first) { m_first = first; }

  /// Get first bookmark
  /**
  * \return the number of the first bookmark on the same level in the hierarchy
  */
  int GetFirst() { return m_first; }

  /// Set last bookmark
  /**
  * \param last the number of the last bookmark on the same level in the hierarchy
  */
  void SetLast(int last) { m_last = last; }

  /// Get last bookmark
  /**
  * \return the number of the last bookmark on the same level in the hierarchy
  */
  int GetLast() { return m_last; }

private:
  wxString m_text;    ///< Text of bookmark
  int      m_level;   ///< Associated level
  double   m_y;       ///< Y offset
  int      m_page;    ///< Page number
  int      m_parent;  ///< Parent bookmark
  int      m_prev;    ///< Previous bookmark
  int      m_next;    ///< Next bookmark
  int      m_first;   ///< First bookmark
  int      m_last;    ///< Last bookmark
};

#endif

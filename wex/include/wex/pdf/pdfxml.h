///////////////////////////////////////////////////////////////////////////////
// Name:        pdfxml.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2005-12-03
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfxml.h Interface of the wxPdfDocument markup

#ifndef _PDF_XML_H_
#define _PDF_XML_H_

// wxWidgets headers
#include <wx/string.h>
#include <wx/wfstream.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdfdocument.h"

class WXDLLIMPEXP_FWD_PDFDOC wxPdfTable;

/// Class representing cell context objects. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfCellContext
{
public:
  /// Constructor
  wxPdfCellContext(double maxWidth, wxPdfAlignment hAlign = wxPDF_ALIGN_JUSTIFY, wxPdfAlignment vAlign = wxPDF_ALIGN_TOP);

  /// Destructor
  virtual ~wxPdfCellContext();

  /// Increase total height of cell
  void AddHeight(double height) { m_height += height; }

  /// Get total height of cell
  double GetHeight() { return m_height; }

  /// Get maximal width of cell
  double GetMaxWidth() { return m_maxWidth; }

  /// Get horizontal alignment
  wxPdfAlignment GetHAlign() { return m_hAlign; }

  /// Set vertical alignment
  void SetVAlign(wxPdfAlignment vAlign) { m_vAlign = vAlign; }

  /// Get vertical alignment
  wxPdfAlignment GetVAlign() { return m_vAlign; }

  /// Add a line to cell
  void AddLine();

  /// Get number of lines in cell
  unsigned int GetLineCount() { return (unsigned int) m_linewidth.GetCount(); }

  /// Add width and number of spaces of the last line of the current context
  void AddLastLineValues(double width, int spaces);

  /// Get the width of the last line
  double GetLastLineWidth();

  /// Mark the current line as the last line
  void MarkLastLine();

  /// Check whether current line is marked as last line
  bool IsCurrentLineMarked();
  
  /// Adjust width of current line
  void AddCurrentLineWidth(double width);

  /// Adjust number of spaces of current line
  void AddCurrentLineSpaces(int spaces);

  /// Get width of current line
  double GetCurrentLineWidth();

  /// Get number of spaces of current line
  int    GetCurrentLineSpaces();

  /// Get number of current line
  unsigned int GetCurrentLine() { return m_currentLine; }

  /// Increment line counter
  void IncrementCurrentLine();

  /// Get line delta measure
  double GetLineDelta();

  /// Check whether alignment has been taken care of
  bool GetAligned() { return m_aligned; }

  /// Set flag that alignment has been taken care of
  void SetAligned() { m_aligned = true; }

  /// Get cell fill style
  int GetFillStyle() { return m_fillStyle; }

  /// Set cell fill style
  void SetFillStyle(int fillStyle) { m_fillStyle = fillStyle; }

  /// Get the number of contexts
  unsigned int GetContextCount() { return (unsigned int) m_contexts.GetCount(); }

  /// Increment current context
  void IncrementCurrentContext();

  /// Get current context
  wxPdfCellContext* GetCurrentContext();

  /// Append context to context list
  void AppendContext(wxPdfCellContext* context);

  /// Remember the last character of the last chunk
  void SetLastChar(wxChar c) { m_lastChar = c; }

  /// Get last character of previous chunk
  wxChar GetLastChar() { return m_lastChar; }

  /// Remember the width of the last space character
  void SetLastSpaceWidth(double w) { m_spaceWidth = w; }

  /// Get width of last space character
  double GetLastSpaceWidth() { return m_spaceWidth; }

  /// Set hyper link reference
  void SetHRef(const wxString& href) { m_href = href; }

  /// Get hyper link reference
  wxString& GetHRef() { return m_href; }

  /// Set table reference
  void SetTable(wxPdfTable* table) { m_table = table; }

  /// Get table reference
  wxPdfTable* GetTable() { return m_table; }

private:
  double           m_maxWidth;        ///< maximal line width
  double           m_lineDelta;       ///< line delta measure
  wxPdfAlignment   m_hAlign;          ///< horizontal alignment
  wxPdfAlignment   m_vAlign;          ///< vertical alignment
  double           m_height;          ///< height of cell
  unsigned int     m_currentContext;  ///< index of current context
  wxArrayPtrVoid   m_contexts;        ///< list of contexts
  unsigned int     m_currentLine;     ///< index of current line
  bool             m_aligned;         ///< alignment flag
  int              m_fillStyle;       ///< cell fill style
  wxPdfArrayDouble m_linewidth;       ///< list of line widths
  wxArrayInt       m_spaces;          ///< list of space counters
  wxChar           m_lastChar;        ///< last character of a chunk
  double           m_spaceWidth;      ///< width of space character
  wxString         m_href;            ///< hyper link reference
  wxPdfTable*      m_table;           ///< table reference
};

/// Class representing table cells. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfTableCell
{
public:
  /// Constructor
  wxPdfTableCell(wxXmlNode* cell, unsigned int row, unsigned int col, unsigned int rows, unsigned int cols);

  /// Destructor
  virtual ~wxPdfTableCell();

  /// Get row of cell
  unsigned int GetRow() const { return m_row; }

  /// Get column of cell
  unsigned int GetCol() const { return m_col; }

  /// Get row span of cell
  unsigned int GetRowSpan() const { return m_rowSpan;};

  /// Get column span of cell
  unsigned int GetColSpan() const { return m_colSpan;};

  /// Set width of cell
  void SetWidth(double w) { m_width = w;};

  /// Set height of cell
  void SetHeight(double h) { m_height = h;};
  
  /// Get height of cell
  double GetHeight() const { return m_height;};

  /// Get width of cell
  double GetWidth() const { return m_width;};

  /// Set cell context
  void SetContext(wxPdfCellContext* context) { m_context = context; }

  /// Get cell context
  wxPdfCellContext* GetContext() { return m_context; }

  /// Set horizontal alignment
  void SetHAlign(wxPdfAlignment hAlign) { m_hAlign = hAlign; }

  /// Get horizontal alignment
  wxPdfAlignment GetHAlign() { return m_hAlign; }

  /// Set vertical alignment
  void SetVAlign(wxPdfAlignment vAlign) { m_vAlign = vAlign; }

  /// Get vertical alignment
  wxPdfAlignment GetVAlign() { return m_vAlign; }

  /// Set border
  void SetBorder(int border) { m_border = border; }

  /// Get border
  int GetBorder() { return m_border; }

  /// Check whether cell has a coloured or transparent background
  bool HasCellColour() const { return m_hasCellColour; };

  /// Set background colour of cell
  void SetCellColour(wxPdfColour colour) { m_hasCellColour = true; m_colourCell = colour;};

  /// Get background colour of cell
  wxPdfColour GetCellColour() const { return m_colourCell; };

  /// Get root node of cell
  wxXmlNode* GetXmlNode() { return m_cell; }

private:
  unsigned int      m_row;           ///< row index
  unsigned int      m_col;           ///< column index
  unsigned int      m_rowSpan;       ///< row span
  unsigned int      m_colSpan;       ///< column span

  wxPdfCellContext* m_context;       ///< table cell context
  wxXmlNode*        m_cell;          ///< xml root node of cell
  wxPdfAlignment    m_hAlign;        ///< horizontal alignment
  wxPdfAlignment    m_vAlign;        ///< vertical alignment
  int               m_border;        ///< border
  double            m_width;         ///< cell width
  double            m_height;        ///< cell height
  
  bool              m_hasCellColour;  ///< flag whether cell has background colour or is transparent
  wxPdfColour       m_colourCell;     ///< cell background colour
}; 

/// Hashmap class for document links
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxPdfTableCell*, wxIntegerHash, wxIntegerEqual, wxPdfCellHashMap, class WXDLLIMPEXP_PDFDOC);

/// Class representing tables within a cell. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfTable
{
public:
  /// Constructor
  wxPdfTable(wxPdfDocument* document);

  /// Destructor
  virtual ~wxPdfTable();

  /// Set minimal required row height
  void SetMinRowHeight(int row, double height) { m_minHeights[row] = height; }

  /// Set width of column
  void SetColumnWidth(int col, double width);

  /// Calculate cell dimensions respecting a maximal allowed width
  void SetCellDimensions(double maxWidth);

  /// Insert a cell into the cell array
  void InsertCell(wxPdfTableCell* c);

  /// Get height of row
  double GetRowHeight(int row) { const double height = m_rowHeights[row]; return height; };

  /// Write table to document
  void Write();

  /// Write one table row to the document
  void WriteRow(unsigned int row, double x, double y);

  /// Set cell padding
  void SetPad(double pad) { m_pad = pad; }

  /// Get cell padding
  double GetPad() { return m_pad; }

  /// Set border
  void SetBorder(bool border) { m_border = border; }

  /// Check whether border should be drawn
  bool HasBorder() { return m_border; }

  /// Get total width of table
  double GetTotalWidth() { return m_totalWidth; }

  /// Get total height of table
  double GetTotalHeight() { return m_totalHeight; }

  /// Set index of first header row
  void SetHeadRowFirst(unsigned int row) { m_headRowFirst = row; }

  /// Set index of last header row
  void SetHeadRowLast(unsigned int row) { m_headRowLast = row; }

  /// Set index of first body row
  void SetBodyRowFirst(unsigned int row) { m_bodyRowFirst = row; }

  /// Set index of last body row
  void SetBodyRowLast(unsigned int row) { m_bodyRowLast = row; }

private:
  wxPdfDocument* m_document;     ///< document reference
  wxPdfDoubleHashMap m_minHeights;   ///< array of minimal row heights
  wxPdfDoubleHashMap m_rowHeights;   ///< array of row heights
  wxPdfDoubleHashMap m_colWidths;    ///< array of column widths

  double             m_maxWidth;     ///< maximal allowed width
  double             m_totalWidth;   ///< total width
  double             m_totalHeight;  ///< total height
  double             m_headHeight;   ///< total height of table header

  unsigned int       m_headRowFirst; ///< index of first header row
  unsigned int       m_headRowLast;  ///< index of last header row
  unsigned int       m_bodyRowFirst; ///< index of first body row
  unsigned int       m_bodyRowLast;  ///< index of last body row

  unsigned int       m_nRows;        ///< number of rows
  unsigned int       m_nCols;        ///< number of columns
  wxPdfCellHashMap   m_table;        ///< array of table cells
  double             m_pad;          ///< cell padding
  bool               m_border;       ///< border flag
};
 
#endif

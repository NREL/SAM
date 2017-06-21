///////////////////////////////////////////////////////////////////////////////
// Name:        pdfspotcolour.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-05-25
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfspotcolour.h Interface of the spot colour class

#ifndef _PDF_SPOT_COLOUR_H_
#define _PDF_SPOT_COLOUR_H_

// wxWidgets headers

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

/// Class representing spot colours.
class WXDLLIMPEXP_PDFDOC wxPdfSpotColour
{
public:
  /// Constructor for spot colour
  wxPdfSpotColour(int index, double cyan, double magenta, double yellow, double black);

  /// Copy constructor
  wxPdfSpotColour(const wxPdfSpotColour& colour);

  /// Set object index
  void SetObjIndex(int index) { m_objIndex = index; }

  /// Get object index
  int GetObjIndex() const { return m_objIndex; }

  /// Get spot colour index
  int GetIndex() const { return m_index; }

  /// Get cyan level
  double GetCyan() const { return m_cyan; }

  /// Get magenta level
  double GetMagenta() const { return m_magenta; }

  /// Get yellow level
  double GetYellow() const { return m_yellow; }

  /// Get black level
  double GetBlack() const { return m_black; }

private:
  int    m_objIndex;   ///< object index
  int    m_index;      ///< colour index
  double m_cyan;       ///< cyan level
  double m_magenta;    ///< magenta level
  double m_yellow;     ///< yellow level
  double m_black;      ///< black level
};

#endif

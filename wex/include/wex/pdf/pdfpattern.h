///////////////////////////////////////////////////////////////////////////////
// Name:        pdfpattern.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-06-18
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfpattern.h Interface of the pattern classes

#ifndef _PDF_PATTERN_H_
#define _PDF_PATTERN_H_

// wxWidgets headers
#include <wx/string.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

class WXDLLIMPEXP_FWD_PDFDOC wxPdfImage;

/// Class representing patterns.
class WXDLLIMPEXP_PDFDOC wxPdfPattern
{
public:
  /// Constructor for pattern
  /**
  * \param index The pattern index
  * \param width The pattern width
  * \param height The pattern height
  */
  wxPdfPattern(int index, double width, double height);

  /// Copy constructor
  wxPdfPattern(const wxPdfPattern& pattern);

  /// Set object index
  void SetObjIndex(int index) { m_objIndex = index; };

  /// Get object index
  int GetObjIndex() const { return m_objIndex; };

  /// Get pattern index
  int GetIndex() const { return m_index; };

  /// Set image
  void SetImage(wxPdfImage* image) { m_image = image; };

  /// Get image
  wxPdfImage* GetImage() const {return m_image; };

  /// Get pattern width
  double GetWidth() const {return m_width; };

  /// Get pattern height
  double GetHeight() const {return m_height; };

private:
  int    m_objIndex;   ///< object index
  int    m_index;      ///< pattern index

  wxPdfImage* m_image; ///< image

  double m_width;      ///< pattern width
  double m_height;     ///< pattern height
  
  double m_xStep;      ///< repeat offset in x direction
  double m_yStep;      ///< repeat offset in y direction
  double m_matrix[6];  ///< transformation matrix
};

#if 0
Pattern dictionary Type 1
-------------------------

/Type /Pattern
/PatternType 1 - tiling pattern
/PaintType 1 - colored
           2 - uncolored
/TilingType 1 - constant spacing
            2 - no distortion
            3 - constant spacing faster tiling
/BBox [ left bottom right top ]
/XStep - horizontal spacing != 0
/YStep - vertical spacing != 0
/Resources
/Matrix [1 0 0 1 0 0]

Pattern dictionary Type 2
-------------------------

/Type /Pattern
/PatternType 2 - shading pattern
/Shading dictionary or stream
/Matrix [1 0 0 1 0 0]
/ExtGState dictionary
            
#endif

#endif

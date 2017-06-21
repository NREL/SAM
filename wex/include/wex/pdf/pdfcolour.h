///////////////////////////////////////////////////////////////////////////////
// Name:        pdfcolour.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2006-07-13
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfcolour.h Interface of the wxPdfColour class

#ifndef _PDF_COLOUR_H_
#define _PDF_COLOUR_H_

// wxWidgets headers
#include <wx/colour.h>
#include <wx/string.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

/// Colour types
enum wxPdfColourType
{
  wxPDF_COLOURTYPE_UNKNOWN,
  wxPDF_COLOURTYPE_GRAY,
  wxPDF_COLOURTYPE_RGB,
  wxPDF_COLOURTYPE_CMYK,
  wxPDF_COLOURTYPE_SPOT,
  wxPDF_COLOURTYPE_PATTERN
};

/// Forward declaration of classes
class WXDLLIMPEXP_FWD_CORE wxColourDatabase;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfPattern;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfSpotColour;

/// Class representing wxPdfDocument colours.
class WXDLLIMPEXP_PDFDOC wxPdfColour
{
public:
  /// Default constructor
  /**
  * Constructs a colour object with an undefined colour
  */
  wxPdfColour();

  /// Constructor for grayscale colour
  /**
  * Defines a grayscale colour
  * \param grayscale indicates the gray level. Value between 0 and 255
  */
  wxPdfColour(const unsigned char grayscale);
  
  /// Constructor for wxColour colour
  /**
  * Defines a wxColour colour.
  * \param colour defines a wxColour colour composed of a red, green and blue component
  */
  wxPdfColour(const wxColour& colour);

  /// Constructor for RGB colour
  /**
  * Defines a RGB colour.
  * \param red indicates the red level. Value between 0 and 255
  * \param green indicates the green level. Value between 0 and 255
  * \param blue indicates the blue level. Value between 0 and 255
  */
  wxPdfColour(const unsigned char red, const unsigned char green, const unsigned char blue);
  
  /// Constructor for CMYK colour
  /**
  * Defines a CMYK colour.
  * \param cyan indicates the cyan level. Value between 0 and 100
  * \param magenta indicates the magenta level. Value between 0 and 100
  * \param yellow indicates the yellow level. Value between 0 and 100
  * \param black indicates the black level. Value between 0 and 100
  */
  wxPdfColour(double cyan, double magenta, double yellow, double black);
  
  /// Constructor for named RGB colour
  /**
  * Defines a named RGB colour.
  * \param name is the name of the requested colour. Use of HTML notation <b><tt>\#rrggbb</tt></b> as colour name is also supported.
  */
  wxPdfColour(const wxString& name);
  
  /// Constructor for named RGB colour
  /**
  * Defines a spot colour.
  * \param spotColour is the spot colour to be used
  * \param tint indicates the tint level. Value between 0 and 100. Default: 100.
  */
  wxPdfColour(const wxPdfSpotColour& spotColour, double tint);

  /// Constructor for pattern based colour
  /**
  * Defines a pattern based colour.
  * \param pattern is the pattern based colour to be used
  */
  wxPdfColour(const wxPdfPattern& pattern);

  /// Copy constructor
  wxPdfColour(const wxPdfColour& colour);

  /// Assignment operator
  wxPdfColour& operator=(const wxPdfColour& colour);

  /// Set grayscale colour
  /**
  * \param grayscale indicates the gray level. Value between 0 and 255. Default: 0 (Black).
  */
  void SetColour(const unsigned char grayscale = 0);
  
  /// Set wxColour colour
  /**
  * \param colour defines a wxColour colour composed of a red, green and blue component
  */
  void SetColour(const wxColour& colour);
  
  /// Set RGB colour
  /**
  * \param red indicates the red level. Value between 0 and 255
  * \param green indicates the green level. Value between 0 and 255
  * \param blue indicates the blue level. Value between 0 and 255
  */
  void SetColour(const unsigned char red, const unsigned char green, const unsigned char blue);
  
  /// Set CMYK colour
  /**
  * \param cyan indicates the cyan level. Value between 0 and 100
  * \param magenta indicates the magenta level. Value between 0 and 100
  * \param yellow indicates the yellow level. Value between 0 and 100
  * \param black indicates the black level. Value between 0 and 100
  */
  void SetColour(double cyan, double magenta, double yellow, double black);

  /// Set a named RGB colour
  /**
  * \param name is the name of the requested colour
  */
  void SetColour(const wxString& name);

  /// Set a spot colour (internal use only)
  /**
  * \param spotColour is the spot colour to be used
  * \param tint indicates the tint level. Value between 0 and 100. Default: 100.
  */
  void SetColour(const wxPdfSpotColour& spotColour, double tint);

  /// Set a pattern based colour (internal use only)
  /**
  * \param pattern is the pattern based colour to be used
  */
  void SetColour(const wxPdfPattern& pattern);

  /// Get internal colour string representation (for internal use only)
  /**
  * \param drawing flag specifying whether the colour is used for drawing operations 
  * \return the string representation of the colour
  */
  const wxString GetColour(bool drawing) const;

  /// Get colour type
  /**
  * \return the colour type of the colour
  */
  wxPdfColourType GetColourType() const { return m_type; }

  /// Get internal colour value string representation (for internal use only)
  /**
  * \return the string representation of the colour value
  * This method works only for grayscale, rgb and cmyk colours. Don't use it for spot or pattern colours.
  */
  const wxString GetColourValue() const;

  /// Compare colour
  /**
  * \param colour colour to which the current colour is compared
  * \return TRUE if the colours match, FALSE otherwise
  */
  bool Equals(const wxPdfColour& colour) const;

protected:
  /// Constructor for internal colour string representation
  wxPdfColour(const wxString& colour, bool intern);

  /// Get a colour database
  static wxColourDatabase* GetColourDatabase();

private:
  wxPdfColourType m_type;   ///< colour type
  wxString        m_prefix; ///< internal colour string prefix
  wxString        m_colour; ///< internal colour string

  static wxColourDatabase* ms_colourDatabase;
};

/// Comparison operator for equality of colour objects
bool operator==(const wxPdfColour& a, const wxPdfColour& b);

/// Comparison operator for inequality of colour objects
bool operator!=(const wxPdfColour& a, const wxPdfColour& b);

#endif

///////////////////////////////////////////////////////////////////////////////
// Name:        pdfutility.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-05-20
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfutility.h Definition of several general utility methods

#ifndef _PDF_UTILITY_H_
#define _PDF_UTILITY_H_

// wxWidgets headers
#include <wx/string.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

/// Class implementing several static utility methods
class WXDLLIMPEXP_PDFDOC wxPdfUtility
{
public:
  /// Create a unique ID
  static wxString GetUniqueId(const wxString& prefix = wxEmptyString);
  
  /// Formats a floating point number with a fixed precision
  /**
  * \param value the value to be formatted
  * \param precision the number of decimal places
  * \return the string representation of the number
  */
  static wxString Double2String(double value, int precision = 0);

  /// Parses a floating point number
  /**
  * \param str the string to be parsed
  * \return the value of floating point number given by the string representation,
  * 0 if the string could not be parsed.
  */
  static double String2Double(const wxString& str);

  /// Converts an integer number to a roman number
  /**
  * \param value integer value to be converted
  * \return the string representation of the integer value as a roman number
  */
  static wxString Convert2Roman(int value);

  /// Forces a floating point number into a fixed range
  /**
  * \param value value to be forced into range
  * \param minValue lower limit
  * \param maxValue upper limit
  * \return value conforming to the given range:
  *   \li the minValue if the value falls below the lower limit
  *   \li the value itself if it is within range
  *   \li the maxValue if the value exceeds the upper limit
  */
  static double ForceRange(double value, double minValue, double maxValue);

  /// Converts a wxColour to the corresponding PDF specification
  /**
  * \param colour colour to be converted to a hexadecimal string representation
  * \return the hexadecimal string representation of the colour
  */
  static wxString RGB2String(const wxColour& colour);

private:
  static bool ms_seeded;  ///< flag whether random number generator is seeded
  static int  ms_s1;      ///< Random number generator seed 1
  static int  ms_s2;      ///< Random number generator seed 2
};

#endif

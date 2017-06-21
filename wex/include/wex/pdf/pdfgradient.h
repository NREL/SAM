///////////////////////////////////////////////////////////////////////////////
// Name:        pdfgradient.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-06-11
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfgradient.h Interface of PDF gradient classes

#ifndef _PDF_GRADIENT_H_
#define _PDF_GRADIENT_H_

// wxWidgets headers

#include <wx/mstream.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdfcolour.h"

enum wxPdfGradientType
{
  wxPDF_GRADIENT_AXIAL,
  wxPDF_GRADIENT_MIDAXIAL,
  wxPDF_GRADIENT_RADIAL,
  wxPDF_GRADIENT_COONS
};

class WXDLLIMPEXP_FWD_PDFDOC wxPdfCoonsPatchMesh;

/// Class representing gradients. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfGradient
{
public:
  /// Constructor
  /**
  * \param type the type of the gradient
  */
  wxPdfGradient(wxPdfGradientType type);

  /// Destructor
  virtual ~wxPdfGradient();

  /// Set gradient object index
  void SetObjIndex(int n) { m_n = n; }

  /// Get gradient object index
  int  GetObjIndex() { return m_n; }

  /// Get the gradient type
  wxPdfGradientType GetType() const { return m_type; };

protected:
  wxPdfGradientType m_type;      ///< Gradient type

private:
  int               m_n;         ///< Gradient index
};

/// Class representing axial gradients. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfAxialGradient : public wxPdfGradient
{
public:
  /// Constructor
  /**
  * \param colour1 first colour
  * \param colour2 second colour
  * \param x1 x ccordinate of the start point
  * \param y1 y ccordinate of the start point
  * \param x2 x ccordinate of the end point
  * \param y2 y ccordinate of the end point
  * \param intexp interpolation exponent
  */
  wxPdfAxialGradient(const wxPdfColour& colour1, const wxPdfColour& colour2, double x1, double y1, double x2, double y2, double intexp);

  /// Destructor
  virtual ~wxPdfAxialGradient();

  /// Get the gradient colour 1
  const wxPdfColour& GetColour1() const { return m_colour1; };

  /// Get the gradient colour 2
  const wxPdfColour& GetColour2() const { return m_colour2; };

  /// Get x coordinate of start point
  double GetX1() const { return m_x1; }

  /// Get y coordinate of start point
  double GetY1() const { return m_y1; }

  /// Get x coordinate of end point
  double GetX2() const { return m_x2; }

  /// Get y coordinate of end point
  double GetY2() const { return m_y2; }

  /// Get the interpolation exponent
  double GetIntExp() const { return m_intexp; }

private:
  wxPdfColour m_colour1;    ///< Gradient colour 1
  wxPdfColour m_colour2;    ///< Gradient colour 2
  double      m_x1;         ///< x coordinate of start point
  double      m_y1;         ///< y coordinate of start point
  double      m_x2;         ///< x coordinate of end point
  double      m_y2;         ///< y coordinate of end point
  double      m_intexp;     ///< interpolation exponent
};

/// Class representing mid axial gradients. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfMidAxialGradient : public wxPdfAxialGradient
{
public:
  /// Constructor
  /**
  * \param colour1 first colour
  * \param colour2 second colour
  * \param x1 x ccordinate of the start point
  * \param y1 y ccordinate of the start point
  * \param x2 x ccordinate of the end point
  * \param y2 y ccordinate of the end point
  * \param midpoint coordinate of mid point
  * \param intexp interpolation exponent
  */
  wxPdfMidAxialGradient(const wxPdfColour& colour1, const wxPdfColour& colour2, double x1, double y1, double x2, double y2, double midpoint, double intexp);

  /// Destructor
  virtual ~wxPdfMidAxialGradient();

  /// Get the coordinate of the mid point
  double GetMidPoint() const { return m_midpoint; }

private:
  double      m_midpoint; ///< coordinate of mid point
};

/// Class representing radial gradients. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfRadialGradient : public wxPdfAxialGradient
{
public:
  /// Constructor
  /**
  * \param colour1 first colour
  * \param colour2 second colour
  * \param x1 x ccordinate of the start point
  * \param y1 y ccordinate of the start point
  * \param r1 radius at start point
  * \param x2 x ccordinate of the end point
  * \param y2 y ccordinate of the end point
  * \param r2 radius at end point
  * \param intexp interpolation exponent
  */
  wxPdfRadialGradient(const wxPdfColour& colour1, const wxPdfColour& colour2, double x1, double y1, double r1, double x2, double y2, double r2, double intexp);

  /// Destructor
  virtual ~wxPdfRadialGradient();

  /// Get radius at start point
  double GetR1() const { return m_r1; }

  /// Get radius at end point
  double GetR2() const { return m_r2; }

private:
  double      m_r1;  ///< radius at start point
  double      m_r2;  ///< radius at end point
};

/// Class representing a coons patch. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfCoonsPatch
{
public:
  /// Constructor
  /**
  * \param edgeFlag
  * \param colours
  * \param x
  * \param y
  */
  wxPdfCoonsPatch(int edgeFlag, wxPdfColour colours[], double x[], double y[]);

  /// Destructor
  virtual ~wxPdfCoonsPatch();

  /// Get the edga flag
  int GetEdgeFlag() { return m_edgeFlag; }

  /// Get the colour array
  wxPdfColour* GetColours() { return m_colours; }

  /// Get the array of x coordinates
  double* GetX() { return m_x; }

  /// Get the array of y coordinates
  double* GetY() { return m_y; }

private:
  int m_edgeFlag;            ///< edge flag
  wxPdfColour m_colours[4];  ///< array of colours
  double m_x[12];            ///< array of x coordinates
  double m_y[12];            ///< array of y coordinates
};

/// Class representing coons patch mesh gradients. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfCoonsPatchGradient : public wxPdfGradient
{
public:
  /// Constructor
  wxPdfCoonsPatchGradient(const wxPdfCoonsPatchMesh& mesh, double minCoord, double maxCoord);

  /// Destructor
  virtual ~wxPdfCoonsPatchGradient();

  /// Get the colour type
  wxPdfColourType GetColourType() { return m_colourType; }

  /// Get the buffer holding the gradient data
  wxMemoryOutputStream* GetBuffer() { return &m_buffer; } 

private:
  wxPdfColourType      m_colourType;  ///< colour type of the gradient
  wxMemoryOutputStream m_buffer;      ///< buffer holding in-memory gradient data
};

#endif

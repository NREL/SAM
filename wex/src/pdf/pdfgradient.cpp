///////////////////////////////////////////////////////////////////////////////
// Name:        pdfgradient.cpp
// Purpose:     Implementation of wxPdfDocument gradient classes
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-06-11
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfgradient.cpp Implementation of the wxPdfDocument gradient classes

// For compilers that support precompilation, includes <wx/wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

#include <wx/tokenzr.h>

#include "wex/pdf/pdfcoonspatchmesh.h"
#include "wex/pdf/pdfgradient.h"
#include "wex/pdf/pdfutility.h"

// --- Gradients

wxPdfGradient::wxPdfGradient(wxPdfGradientType type)
{
  m_type = type;
}

wxPdfGradient::~wxPdfGradient()
{
}

wxPdfAxialGradient::wxPdfAxialGradient(const wxPdfColour& colour1, const wxPdfColour& colour2, double x1, double y1, double x2, double y2, double intexp)
  : wxPdfGradient(wxPDF_GRADIENT_AXIAL)
{
  m_colour1 = colour1;
  m_colour2 = colour2;
  m_x1 = x1;
  m_y1 = y1;
  m_x2 = x2;
  m_y2 = y2;
  m_intexp = intexp;
}

wxPdfAxialGradient::~wxPdfAxialGradient()
{
}

wxPdfMidAxialGradient::wxPdfMidAxialGradient(const wxPdfColour& colour1, const wxPdfColour& colour2, double x1, double y1, double x2, double y2, double midpoint, double intexp)
  : wxPdfAxialGradient(colour1, colour2, x1, y1, x2, y2, intexp)
{
  m_type = wxPDF_GRADIENT_MIDAXIAL;
  m_midpoint = midpoint;
}

wxPdfMidAxialGradient::~wxPdfMidAxialGradient()
{
}

wxPdfRadialGradient::wxPdfRadialGradient(const wxPdfColour& colour1, const wxPdfColour& colour2,
                                         double x1, double y1, double r1,
                                         double x2, double y2, double r2, double intexp)
  : wxPdfAxialGradient(colour1, colour2, x1, y1, x2, y2, intexp)
{
  m_type = wxPDF_GRADIENT_RADIAL;
  m_r1 = r1;
  m_r2 = r2;
}

wxPdfRadialGradient::~wxPdfRadialGradient()
{
}

wxPdfCoonsPatch::wxPdfCoonsPatch(int edgeFlag, wxPdfColour colours[], double x[], double y[])
{
  m_edgeFlag = edgeFlag;
  size_t n = (edgeFlag == 0) ? 4 : 2;
  size_t j;
  for (j = 0; j < n; j++)
  {
    m_colours[j] = colours[j];
  }

  n = (edgeFlag == 0) ? 12 : 8;
  for (j = 0; j < n; j++)
  {
    m_x[j] = x[j];
    m_y[j] = y[j];
  }
}

wxPdfCoonsPatch::~wxPdfCoonsPatch()
{
}

wxPdfCoonsPatchMesh::wxPdfCoonsPatchMesh()
{
  m_ok = false;
  m_colourType = wxPDF_COLOURTYPE_UNKNOWN;
}

wxPdfCoonsPatchMesh::~wxPdfCoonsPatchMesh()
{
  size_t n = m_patches.size();
  if (n > 0)
  {
    size_t j;
    for (j = 0; j < n; j++)
    {
      delete ((wxPdfCoonsPatch*) m_patches[j]);
    }
  }
}

bool
wxPdfCoonsPatchMesh::AddPatch(int edgeFlag, wxPdfColour colours[], double x[], double y[])
{
  wxPdfColourType colourType = m_colourType;
  if (m_patches.size() == 0 && edgeFlag != 0) return false;
  int n = (edgeFlag == 0) ? 4 : 2;
  int j;
  for (j = 0; j < n; j++)
  {
    if (colourType == wxPDF_COLOURTYPE_UNKNOWN)
    {
      colourType = colours[j].GetColourType();
    }
    if (colours[j].GetColourType() != colourType) return false;
  }
  m_colourType = colourType;
  wxPdfCoonsPatch* patch = new wxPdfCoonsPatch(edgeFlag, colours, x, y);
  m_patches.Add(patch);
  m_ok = true;
  return true;
}

wxPdfCoonsPatchGradient::wxPdfCoonsPatchGradient(const wxPdfCoonsPatchMesh& mesh, double minCoord, double maxCoord)
  : wxPdfGradient(wxPDF_GRADIENT_COONS)
{
  int edgeFlag;
  double *x;
  double *y;
  const wxArrayPtrVoid* patches = mesh.GetPatches();
  size_t n = patches->size();
  size_t j, k, nc;
  unsigned char ch;
  int bpcd = 65535; //16 BitsPerCoordinate
  int coord;
  wxPdfColour *colours;

  m_colourType = mesh.GetColourType();
  // build the data stream
  for (j = 0;  j < n; j++)
  {
    wxPdfCoonsPatch* patch = (wxPdfCoonsPatch*) (*patches)[j];
    edgeFlag = patch->GetEdgeFlag();
    ch = edgeFlag;
    m_buffer.Write(&ch,1); //start with the edge flag as 8 bit
    x = patch->GetX();
    y = patch->GetY();
    nc = (edgeFlag == 0) ? 12 : 8;
    for (k = 0; k < nc; k++)
    {
      // each point as 16 bit
      coord = (int) (((x[k] - minCoord) / (maxCoord - minCoord)) * bpcd);
      if (coord < 0)    coord = 0;
      if (coord > bpcd) coord = bpcd;
      ch = (coord >> 8) & 0xFF;
      m_buffer.Write(&ch,1);
      ch = coord & 0xFF;
      m_buffer.Write(&ch,1);
      coord = (int) (((y[k] - minCoord) / (maxCoord - minCoord)) * bpcd);
      if (coord < 0)    coord = 0;
      if (coord > bpcd) coord = bpcd;
      ch = (coord >> 8) & 0xFF;
      m_buffer.Write(&ch,1);
      ch = coord & 0xFF;
      m_buffer.Write(&ch,1);
    }
    colours = patch->GetColours();
    nc = (edgeFlag == 0) ? 4 : 2;
    for (k = 0; k < nc; k++)
    {
      // each colour component as 8 bit
      wxStringTokenizer tkz(colours[k].GetColourValue(), wxT(" "));
      while ( tkz.HasMoreTokens() )
      {
        ch = ((int) (wxPdfUtility::String2Double(tkz.GetNextToken()) * 255)) & 0xFF;
        m_buffer.Write(&ch,1);
      }
    }
  }
}

wxPdfCoonsPatchGradient::~wxPdfCoonsPatchGradient()
{
}

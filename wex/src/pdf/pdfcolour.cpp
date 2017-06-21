///////////////////////////////////////////////////////////////////////////////
// Name:        pdfcolour.cpp
// Purpose:     Implementation of wxPdfDocument colour handling
// Author:      Ulrich Telle
// Modified by:
// Created:     2006-01-27
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfcolour.cpp Implementation of the wxPdfDocument colour handling

// For compilers that support precompilation, includes <wx/wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

#include "wex/pdf/pdfcolour.h"
#include "wex/pdf/pdfpattern.h"
#include "wex/pdf/pdfspotcolour.h"
#include "wex/pdf/pdfutility.h"

#include "pdfcolourdata.inc"

wxColourDatabase* wxPdfColour::ms_colourDatabase = NULL;

wxColourDatabase*
wxPdfColour::GetColourDatabase()
{
  if (ms_colourDatabase == NULL)
  {
    if (wxTheColourDatabase != NULL)
    {
      ms_colourDatabase = wxTheColourDatabase;
    }
    else
    {
      static wxColourDatabase pdfColourDatabase;
      ms_colourDatabase = &pdfColourDatabase;
    }
    size_t n;
    for ( n = 0; n < WXSIZEOF(wxColourTable); n++ )
    {
      const wxColourDesc& cc = wxColourTable[n];
      ms_colourDatabase->AddColour(cc.name, wxColour(cc.r, cc.g, cc.b));
    }
  }
  return ms_colourDatabase;
}

wxPdfColour::wxPdfColour()
{
  m_type   = wxPDF_COLOURTYPE_UNKNOWN;
  m_prefix = wxEmptyString;
  m_colour  = wxT("0");
}

wxPdfColour::wxPdfColour(const unsigned char grayscale)
{
  SetColour(grayscale);
}

wxPdfColour::wxPdfColour(const wxColour& colour)
{
  SetColour(colour);
}

wxPdfColour::wxPdfColour(const unsigned char red, const unsigned char green, const unsigned char blue)
{
  SetColour(red, green, blue);
}

wxPdfColour::wxPdfColour(double cyan, double magenta, double yellow, double black)
{
  SetColour(cyan, magenta, yellow, black);
}

wxPdfColour::wxPdfColour(const wxPdfSpotColour& spot, double tint)
{
  SetColour(spot, tint);
}

wxPdfColour::wxPdfColour(const wxPdfPattern& pattern)
{
  SetColour(pattern);
}

wxPdfColour::wxPdfColour(const wxString& name)
{
  SetColour(name);
}

wxPdfColour::wxPdfColour(const wxPdfColour& colour)
{
  m_type   = colour.m_type;
  m_prefix = colour.m_prefix;
  m_colour = colour.m_colour;
}

wxPdfColour::wxPdfColour(const wxString& colour, bool intern)
{
  wxUnusedVar(intern);
  m_colour = colour;
}

wxPdfColour&
wxPdfColour::operator=(const wxPdfColour& colour)
{
  m_type   = colour.m_type;
  m_prefix = colour.m_prefix;
  m_colour = colour.m_colour;
  return *this;
}

bool
wxPdfColour::Equals(const wxPdfColour& colour) const
{
  return (m_type == colour.m_type) && (m_prefix == colour.m_prefix) && (m_colour == colour.m_colour);
}

void
wxPdfColour::SetColour(const unsigned char grayscale)
{
  m_type   = wxPDF_COLOURTYPE_GRAY;
  m_prefix = wxEmptyString;
  m_colour = wxPdfUtility::Double2String(((double) grayscale)/255.,3);
}

void
wxPdfColour::SetColour(const wxColour& colour)
{
  m_type   = wxPDF_COLOURTYPE_RGB;
  m_prefix = wxEmptyString;
  m_colour = wxPdfUtility::RGB2String(colour);
}

void
wxPdfColour::SetColour(const unsigned char red, const unsigned char green, const unsigned char blue)
{
  SetColour(wxColour(red,green,blue));
}

void
wxPdfColour::SetColour(double cyan, double magenta, double yellow, double black)
{
  m_type   = wxPDF_COLOURTYPE_CMYK;
  m_prefix = wxEmptyString;
  m_colour = wxPdfUtility::Double2String(wxPdfUtility::ForceRange(cyan,    0., 100.)/100.,3) + wxT(" ") +
             wxPdfUtility::Double2String(wxPdfUtility::ForceRange(magenta, 0., 100.)/100.,3) + wxT(" ") +
             wxPdfUtility::Double2String(wxPdfUtility::ForceRange(yellow,  0., 100.)/100.,3) + wxT(" ") +
             wxPdfUtility::Double2String(wxPdfUtility::ForceRange(black,   0., 100.)/100.,3);
}

void
wxPdfColour::SetColour(const wxString& name)
{
  if (name.Length() == 7 && name[0] == wxT('#'))
  {
    unsigned long r = 0, g = 0, b = 0;
    if (name.Mid(1,2).ToULong(&r,16) &&
        name.Mid(3,2).ToULong(&g,16) &&
        name.Mid(5,2).ToULong(&b,16))
    {
      SetColour((unsigned char) r, (unsigned char) g, (unsigned char) b);
    }
    else
    {
     SetColour(0);
    }
  }
  else
  {
    wxColourDatabase* colourDatabase = GetColourDatabase();
    wxColour colour = colourDatabase->Find(name);
    if (colour.Ok())
    {
      SetColour(colour);
    }
    else
    {
      SetColour(0);
    }
  }
}

void
wxPdfColour::SetColour(const wxPdfSpotColour& spot, double tint)
{
  m_type   = wxPDF_COLOURTYPE_SPOT;
  m_prefix = wxString::Format(wxT("/CS%d"), spot.GetIndex());
  m_colour = wxPdfUtility::Double2String(wxPdfUtility::ForceRange(tint, 0., 100.)/100.,3);
}

void
wxPdfColour::SetColour(const wxPdfPattern& pattern)
{
  m_type   = wxPDF_COLOURTYPE_PATTERN;
  m_prefix = wxString(wxT("/Pattern"));
  m_colour = wxString::Format(wxT("/P%d"), pattern.GetIndex());
}

const wxString
wxPdfColour::GetColour(bool drawing) const
{
  wxString colour = wxEmptyString;
  switch (m_type)
  {
    case wxPDF_COLOURTYPE_GRAY:
      colour = m_colour + ((drawing) ? wxString(wxT(" G")) : wxString(wxT(" g")));
      break;
    case wxPDF_COLOURTYPE_RGB:
      colour = m_colour + ((drawing) ? wxString(wxT(" RG")) : wxString(wxT(" rg")));
      break;
    case wxPDF_COLOURTYPE_CMYK:
      colour = m_colour + ((drawing) ? wxString(wxT(" K")) : wxString(wxT(" k")));
      break;
    case wxPDF_COLOURTYPE_SPOT:
      colour = m_prefix + ((drawing) ? wxString(wxT(" CS ")) : wxString(wxT(" cs "))) +
               m_colour + ((drawing) ? wxString(wxT(" SCN")) : wxString(wxT(" scn")));
      break;
    case wxPDF_COLOURTYPE_PATTERN:
      colour = m_prefix + ((drawing) ? wxString(wxT(" CS ")) : wxString(wxT(" cs "))) +
               m_colour + ((drawing) ? wxString(wxT(" SCN")) : wxString(wxT(" scn")));
      break;
    default:
      colour = ((drawing) ? wxString(wxT("0 G")) : wxString(wxT("0 g")));
      break;
  }
  return colour;
}

const wxString
wxPdfColour::GetColourValue() const
{
  return m_colour;
}

wxPdfSpotColour::wxPdfSpotColour(int index, double cyan, double magenta, double yellow, double black)
  : m_objIndex(0), m_index(index), m_cyan(cyan), m_magenta(magenta), m_yellow(yellow), m_black(black)
{
}

wxPdfSpotColour::wxPdfSpotColour(const wxPdfSpotColour& colour)
{
  m_objIndex = colour.m_objIndex;
  m_index    = colour.m_index;
  m_cyan     = colour.m_cyan;
  m_magenta  = colour.m_magenta;
  m_yellow   = colour.m_yellow;
  m_black    = colour.m_black;
}

bool operator==(const wxPdfColour& a, const wxPdfColour& b)
{
  return a.Equals(b);
}

bool operator!=(const wxPdfColour& a, const wxPdfColour& b)
{
  return !a.Equals(b);
}

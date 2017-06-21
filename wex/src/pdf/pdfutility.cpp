///////////////////////////////////////////////////////////////////////////////
// Name:        pdfutility.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-05-20
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfutility.cpp Implementation of wxPdfUtility methods

// For compilers that support precompilation, includes <wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

// includes

#include "wex/pdf/pdfutility.h"

bool wxPdfUtility::ms_seeded = false;
int  wxPdfUtility::ms_s1     = 0;
int  wxPdfUtility::ms_s2     = 0;

#define MODMULT(a, b, c, m, s) q = s / a; s = b * (s - a * q) - c * q; if (s < 0) s += m

wxString
wxPdfUtility::GetUniqueId(const wxString& prefix)
{
  wxString uid = (prefix.Length() <= 114) ? prefix : prefix.Left(114);

  wxDateTime ts;
  ts.SetToCurrent();

  int q;
  int z;
  if (!ms_seeded)
  {
    ms_seeded = true;
    ms_s1 = ts.GetSecond() ^ (~ts.GetMillisecond());
    if (ms_s1 == 0) ms_s1 = 1;
    ms_s2 = wxGetProcessId();
  }
  MODMULT(53668, 40014, 12211, 2147483563L, ms_s1);
  MODMULT(52774, 40692,  3791, 2147483399L, ms_s2);

  z = ms_s1 - ms_s2;
  if (z < 1)
  {
    z += 2147483562;
  }

  uid += wxString::Format(wxT("%08x%05x"), ts.GetSecond(), ts.GetMillisecond());
  uid += Double2String(z * 4.656613e-9,8);

  return uid;
}

wxString
wxPdfUtility::Double2String(double value, int precision)
{
  wxString number;
  if (precision < 0)
  {
    precision = 0;
  }
  else if (precision > 16)
  {
    precision = 16;
  }

  // Use absolute value locally
  double localValue = fabs(value);
  double localFraction = (localValue - floor(localValue)) +(5. * pow(10.0, -precision-1));
  if (localFraction >= 1)
  {
    localValue += 1.0;
    localFraction -= 1.0;
  }
  localFraction *= pow(10.0, precision);

  if (value < 0)
  {
    number += wxString(wxT("-"));
  }

  number += wxString::Format(wxT("%.0f"), floor(localValue));

  // generate fraction, padding with zero if necessary.
  if (precision > 0)
  {
    number += wxString(wxT("."));
    wxString fraction = wxString::Format(wxT("%.0f"), floor(localFraction));
    if (fraction.Length() < ((size_t) precision))
    {
      number += wxString(wxT('0'), precision-fraction.Length());
    }
    number += fraction;
  }

  return number;
}

double
wxPdfUtility::String2Double(const wxString& str)
{
  wxString value = str.Strip(wxString::both);
  double result = 0;
  double sign = 1;
  int scale = 0;
  int exponent = 0;
  int expsign = 1;
  int j = 0;
  int jMax = (int) value.Length();
  if (jMax > 0)
  {
    if (value[j] == wxT('+'))
    {
      j++;
    }
    else if (value[j] == wxT('-'))
    {
      sign = -1;
      j++;
    }
    while (j < jMax && wxIsdigit(value[j]))
    {
      result = result*10 + (value[j] - wxT('0'));
      j++;
    }
    if (j < jMax && value[j] == wxT('.'))
    {
      j++;
      while (j < jMax && wxIsdigit(value[j]))
      {
        result = result*10 + (value[j] - wxT('0'));
        scale++;
        j++;
      }
    }
    if (j < jMax && (value[j] == wxT('E') || value[j] == wxT('e')))
    {
      j++;
      if (value[j] == wxT('+'))
      {
        j++;
      }
      else if (value[j] == wxT('-'))
      {
        expsign = -1;
        j++;
      }
      while (j < jMax && wxIsdigit(value[j]))
      {
        exponent = exponent*10 + (value[j] - wxT('0'));
        j++;
      }
      exponent *= expsign;
    }
    result = sign * result * pow(10.0, exponent-scale);
  }
  return result;
}

wxString
wxPdfUtility::Convert2Roman(int value)
{
  wxString result = wxEmptyString;

  if (value > 0 && value < 4000)
  {
    static wxString romans = wxT("MDCLXVI");
    int pos = 6;  // Point to LAST character in 'romans'
    int currentDigit;

    while (value > 0)
    {
      currentDigit = value % 10;   
      if (currentDigit == 4 || currentDigit == 9)
      {
        result.Prepend(romans.Mid(pos  - currentDigit / 4, 1));
        result.Prepend(romans.Mid(pos, 1));
      }
      else
      {
        int x = currentDigit % 5;
        while (x-- > 0)
        {
          result.Prepend(romans.Mid(pos, 1));
        }
        if (currentDigit >= 5)
        {
          result.Prepend(romans.Mid(pos - 1, 1));
        }
      }
      value /= 10;
      pos -= 2;
    }
  }
  else
  {
    result = wxT("???");
  }
  return result;
}

double
wxPdfUtility::ForceRange(double value, double minValue, double maxValue)
{
  if (value < minValue) 
  {
    value = minValue;
  }
  else if (value > maxValue)
  {
    value = maxValue;
  }
  return value;
}

wxString
wxPdfUtility::RGB2String(const wxColour& colour)
{
  double r = colour.Red();
  double g = colour.Green();
  double b = colour.Blue();
  wxString rgb = Double2String(r/255.,3) + wxT(" ") + 
                 Double2String(g/255.,3) + wxT(" ") + 
                 Double2String(b/255.,3);
  return rgb;
}

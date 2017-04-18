///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontvolt.cpp
// Purpose:     Implementation of VOLT data handling
// Author:      Ulrich Telle
// Modified by:
// Created:     2010-06-23
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontvolt.cpp Implementation of the VOLT data handling

// For compilers that support precompilation, includes <wx/wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

#include <wx/regex.h>
#include <wx/wfstream.h>
#include <wx/txtstrm.h>

#include "wex/pdf/pdffontvolt.h"

#include "wxmemdbg.h"

class wxPdfVoltRule
{
public :
  /// Default constructor
  wxPdfVoltRule()
    : m_repeat(false), m_match(wxT("")), m_replace(wxT(""))
  {
  }

  wxPdfVoltRule(bool repeat, const wxString& match, const wxString& replace)
    : m_repeat(repeat), m_match(match), m_replace(replace)
  {
    m_re.Compile(m_match);
  }

  ~wxPdfVoltRule()
  {
  }

public:
  bool     m_repeat;
  wxString m_match;
  wxString m_replace;
  wxRegEx  m_re;
};

wxPdfVolt::wxPdfVolt()
{
}

wxPdfVolt::~wxPdfVolt()
{
  size_t n = m_rules.GetCount();
  size_t j;
  for (j = 0; j < n; ++j)
  {
    delete (wxPdfVoltRule*) m_rules.Item(j);
  }
}

void
wxPdfVolt::LoadVoltData(wxXmlNode* volt)
{
  wxString repeat, match, replace;
  bool doRepeat;
  wxXmlNode* child = volt->GetChildren();
  while (child)
  {
    if (child->GetName() == wxT("ruleset"))
    {
      wxXmlNode* rule = child->GetChildren();
      while (rule)
      {
        if (rule->GetName() == wxT("rule"))
        {
#if wxCHECK_VERSION(2,9,0)
          repeat  = rule->GetAttribute(wxT("repeat"), wxT("false"));
          match   = rule->GetAttribute(wxT("match"), wxT(""));
          replace = rule->GetAttribute(wxT("replace"), wxT(""));
#else
          repeat  = rule->GetPropVal(wxT("repeat"), wxT("false"));
          match   = rule->GetPropVal(wxT("match"), wxT(""));
          replace = rule->GetPropVal(wxT("replace"), wxT(""));
#endif
          doRepeat = repeat.IsSameAs(wxT("true"));
          wxPdfVoltRule* voltRule = new wxPdfVoltRule(doRepeat, match, replace);
          m_rules.Add(voltRule);
        }
        rule = rule->GetNext();
      }
    }
    child = child->GetNext();
  }
}

wxString
wxPdfVolt::ProcessRules(const wxString& text)
{
#if 0
  wxFFileOutputStream dbgOut( wxT("d:/temp/pdfdoc-indic-dbg.txt"), wxT("a"));
  wxTextOutputStream dbgIndic( dbgOut );

  wxString str;
#endif
  wxString processText = text;
  size_t n = m_rules.GetCount();
  size_t j;
  for (j = 0; j < n; ++j)
  {
    wxPdfVoltRule* rule = (wxPdfVoltRule*) m_rules.Item(j);
    int matchCount;
    do
    {
      matchCount = rule->m_re.Replace(&processText, rule->m_replace);
#if 0
      str = wxEmptyString;
      wxString::const_iterator ch;
      for (ch = processText.begin(); ch != processText.end(); ++ch)
      {
        str += wxString::Format(wxT(" %04x"), *ch);
      }
      str += wxT("\n");
      dbgIndic.WriteString(str);
#endif
    }
    while (rule->m_repeat && matchCount > 0);
  }
#if 0
  dbgOut.Close();
#endif
  return processText;
}

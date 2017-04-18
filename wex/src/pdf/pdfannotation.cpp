///////////////////////////////////////////////////////////////////////////////
// Name:        pdfannotation.cpp
// Purpose:     Implementation of wxPdfDocument helper classes
// Author:      Ulrich Telle
// Modified by:
// Created:     2006-01-27
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfannotation.cpp Implementation of the wxPdfLink, wxPdfPageLink and wxPdfAnnotation classes

// For compilers that support precompilation, includes <wx/wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

#include "wex/pdf/pdfannotation.h"
#include "wex/pdf/pdfbookmark.h"
#include "wex/pdf/pdfdocument.h"
#include "wex/pdf/pdflinks.h"

// ----------------------------------------------------------------------------
// wxPdfLink: class representing internal or external links
// ----------------------------------------------------------------------------

wxPdfLink::wxPdfLink(int linkRef)
  : m_isRef(true), m_linkRef(linkRef), m_linkURL(wxEmptyString)
{
  m_isValid = linkRef > 0;
  m_page = 0;
  m_ypos = 0;
}

wxPdfLink::wxPdfLink(const wxString& linkURL)
  : m_isRef(false), m_linkRef(0), m_linkURL(linkURL)
{
  m_isValid = linkURL.Length() > 0;
}

wxPdfLink::wxPdfLink(const wxPdfLink& pdfLink)
{
  m_isValid = pdfLink.m_isValid;
  m_isRef   = pdfLink.m_isRef;
  m_linkRef = pdfLink.m_linkRef;
  m_linkURL = pdfLink.m_linkURL;
  m_page    = pdfLink.m_page;
  m_ypos    = pdfLink.m_ypos;
}

wxPdfLink::~wxPdfLink()
{
}

wxPdfPageLink::wxPdfPageLink(double x, double y, double w, double h, const wxPdfLink& pdfLink)
  : wxPdfLink(pdfLink)
{
  m_x = x;
  m_y = y;
  m_w = w;
  m_h = h;
}

wxPdfPageLink::~wxPdfPageLink()
{
}

// ----------------------------------------------------------------------------
// wxPdfAnnotation: class representing text annotations
// ----------------------------------------------------------------------------

wxPdfAnnotation::wxPdfAnnotation(double x, double y, const wxString& text)
  : m_x(x), m_y(y), m_text(text)
{
}

wxPdfAnnotation::wxPdfAnnotation(const wxPdfAnnotation& annotation)
{
  m_x    = annotation.GetX();
  m_y    = annotation.GetY();
  m_text = annotation.GetText();
}

// ----------------------------------------------------------------------------
// wxPdfBookmark: class representing bookmark objects for the document outline
// ----------------------------------------------------------------------------

wxPdfBookmark::wxPdfBookmark(const wxString& txt, int level, double y, int page)
{
  m_text = txt;
  m_level = level;
  m_y = y;
  m_page = page;

  m_parent = -1;
  m_prev   = -1;
  m_next   = -1;
  m_first  = -1;
  m_last   = -1;
}

wxPdfBookmark::~wxPdfBookmark()
{
}

// ---

int
wxPdfDocument::AddLink()
{
  if (m_inTemplate)
  {
    wxLogError(wxString(wxT("wxPdfDocument::AddLink: ")) +
               wxString::Format(_("Adding links in templates is impossible. Current template ID is %d."), m_templateId));
    return -1;
  }

  // Create a new internal link
  int n = (int) (*m_links).size()+1;
  (*m_links)[n] = new wxPdfLink(n);
  return n;
}

bool
wxPdfDocument::SetLink(int link, double ypos, int page)
{
  if (m_inTemplate)
  {
    wxLogError(wxString(wxT("wxPdfDocument::SetLink: ")) +
               wxString::Format(_("Setting links in templates is impossible. Current template ID is %d."), m_templateId));
    return false;
  }

  bool isValid = false;
  // Set destination of internal link
  if (ypos == -1)
  {
    ypos = m_y;
  }
  if (page == -1)
  {
    page = m_page;
  }
  wxPdfLinkHashMap::iterator pLink = (*m_links).find(link);
  if (pLink != (*m_links).end())
  {
    isValid = true;
    wxPdfLink* currentLink = pLink->second;
    currentLink->SetLink(page,ypos);
  }
  return isValid;
}

void
wxPdfDocument::Link(double x, double y, double w, double h, const wxPdfLink& link)
{
  if (m_inTemplate)
  {
    wxLogError(wxString(wxT("wxPdfDocument::Link: ")) +
               wxString::Format(_("Using links in templates is impossible. Current template ID is %d."), m_templateId));
    return;
  }

  // Put a link on the page
  wxArrayPtrVoid* pageLinkArray = NULL;
  double yPos = (m_yAxisOriginTop) ? m_h - y : y;
  wxPdfPageLink* pageLink = new wxPdfPageLink(x*m_k, yPos*m_k, w*m_k, h*m_k, link);
  wxPdfPageLinksMap::iterator pageLinks = (*m_pageLinks).find(m_page);
  if (pageLinks != (*m_pageLinks).end())
  {
    pageLinkArray = pageLinks->second;
  }
  else
  {
    pageLinkArray = new wxArrayPtrVoid;
    (*m_pageLinks)[m_page] = pageLinkArray;
  }
  pageLinkArray->Add(pageLink);
}

void
wxPdfDocument::Bookmark(const wxString& txt, int level, double y)
{
  if (y < 0)
  {
    y = GetY();
  }
  wxPdfBookmark* bookmark = new wxPdfBookmark(txt, level, y, PageNo());
  m_outlines.Add(bookmark);
  if (level > m_maxOutlineLevel)
  {
    m_maxOutlineLevel = level;
  }
}

void
wxPdfDocument::Annotate(double x, double y, const wxString& text)
{
  wxArrayPtrVoid* annotationArray = NULL;
  double yPos = (m_yAxisOriginTop) ? m_h - y : y;
  wxPdfAnnotation* annotation = new wxPdfAnnotation(x*m_k, yPos*m_k, text);
  wxPdfAnnotationsMap::iterator pageAnnots = (*m_annotations).find(m_page);
  if (pageAnnots != (*m_annotations).end())
  {
    annotationArray = pageAnnots->second;
  }
  else
  {
    annotationArray = new wxArrayPtrVoid;
    (*m_annotations)[m_page] = annotationArray;
  }
  annotationArray->Add(annotation);
}



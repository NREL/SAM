///////////////////////////////////////////////////////////////////////////////
// Name:        pdfdocument.cpp
// Purpose:     Implementation of wxPdfDocument (public methods)
// Author:      Ulrich Telle
// Modified by:
// Created:     2005-08-04
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfdocument.cpp Implementation of the wxPdfDocument class

// For compilers that support precompilation, includes <wx/wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

#include <wx/image.h>
#include <wx/paper.h>
#include <wx/wfstream.h>

#include "wex/pdf/pdfbookmark.h"
#include "wex/pdf/pdfdocument.h"
#include "wex/pdf/pdffont.h"
#include "wex/pdf/pdffontdetails.h"
#include "wex/pdf/pdffontmanager.h"
#include "wex/pdf/pdfform.h"
#include "wex/pdf/pdfgradient.h"
#include "wex/pdf/pdfgraphics.h"
#include "wex/pdf/pdflayer.h"
#include "wex/pdf/pdfparser.h"
#include "wex/pdf/pdfpattern.h"
#include "wex/pdf/pdfspotcolour.h"
#include "wex/pdf/pdftemplate.h"
#include "wex/pdf/pdffontparser.h"
#include "wex/pdf/pdfutility.h"

#if WXPDFDOC_INHERIT_WXOBJECT
IMPLEMENT_DYNAMIC_CLASS(wxPdfDocument, wxObject)
#endif

// ----------------------------------------------------------------------------
// wxPdfDocument: class representing a PDF document
// ----------------------------------------------------------------------------

wxPdfDocument::wxPdfDocument(int orientation, const wxString& unit, wxPaperSize format)
#if WXPDFDOC_INHERIT_WXOBJECT
  : wxObject()
#endif
{
  m_yAxisOriginTop = true;
  SetScaleFactor(unit);

  // Page format
  m_defPageSize = CalculatePageSize(format);
  Initialize(orientation);
}

wxPdfDocument::wxPdfDocument(int orientation, double pageWidth, double pageHeight, const wxString& unit)
#if WXPDFDOC_INHERIT_WXOBJECT
  : wxObject()
#endif
{
  m_yAxisOriginTop = true;
  SetScaleFactor(unit);
  m_defPageSize = CalculatePageSize(pageWidth, pageHeight);
  Initialize(orientation);
}

void
wxPdfDocument::SetScaleFactor(const wxString& unit)
{
  // Scale factor
  if (unit == wxT("pt"))
  {
    m_k = 1.;
  }
  else if (unit == wxT("in"))
  {
    m_k = 72.;
  }
  else if (unit == wxT("cm"))
  {
    m_k = 72. / 2.54;
  }
  else // if (unit == "mm") or unknown
  {
    m_k = 72. / 25.4;
  }
}

wxSize
wxPdfDocument::CalculatePageSize(wxPaperSize format)
{
  bool deletePaperDatabase = false;
  wxPrintPaperDatabase* printPaperDatabase = wxThePrintPaperDatabase;
  if (printPaperDatabase == NULL)
  {
    printPaperDatabase = new wxPrintPaperDatabase;
    printPaperDatabase->CreateDatabase();
    deletePaperDatabase = true;
  }
  wxPrintPaperType* paperType = printPaperDatabase->FindPaperType(format);
  if (paperType == NULL)
  {
    paperType = printPaperDatabase->FindPaperType(wxPAPER_A4);
  }
  wxSize paperSize = paperType->GetSize();
  if (deletePaperDatabase)
  {
    delete printPaperDatabase;
  }
  return paperSize;
}

wxSize
wxPdfDocument::CalculatePageSize(double pageWidth, double pageHeight)
{
  int width  = (int) (pageWidth  * (m_k * 254. / 72.));
  int height = (int) (pageHeight * (m_k * 254. / 72.));
  return wxSize(width,height);
}

void
wxPdfDocument::Initialize(int orientation)
{
  // Allocate arrays
  m_currentFont = NULL;
  m_buffer = new wxMemoryOutputStream();

  m_page       = 0;
  m_n          = 2;
  m_offsets = new wxPdfOffsetHashMap();

  m_pages = new wxPdfPageHashMap();
  m_pageSizes = new wxPdfPageSizeMap();
  m_orientationChanges = new wxPdfBoolHashMap();

  m_state            = 0;
  m_fonts            = new wxPdfFontHashMap();
  m_images           = new wxPdfImageHashMap();
  m_pageLinks        = new wxPdfPageLinksMap();
  m_links            = new wxPdfLinkHashMap();
  m_namedLinks       = new wxPdfNamedLinksMap();
  m_diffs            = new wxPdfStringHashMap();
  m_winansi          = new wxPdfBoolHashMap();
  m_extGStates       = new wxPdfExtGStateMap();
  m_extGSLookup      = new wxPdfExtGSLookupMap();
  m_currentExtGState = 0;
  m_gradients        = new wxPdfGradientMap();
  m_annotations      = new wxPdfAnnotationsMap();
  m_formAnnotations  = new wxPdfFormAnnotsMap();
  m_formFields       = new wxPdfFormFieldsMap();
  m_radioGroups      = new wxPdfRadioGroupMap();
  m_templates        = new wxPdfTemplatesMap();
  m_parsers          = new wxPdfParserMap();
  m_spotColours      = new wxPdfSpotColourMap();
  m_patterns         = new wxPdfPatternMap();
  m_ocgs             = new wxPdfOcgMap();
  m_rgLayers         = new wxPdfLayerRGMap();
  m_lockedLayers     = NULL;
  m_attachments      = new wxPdfAttachmentMap();

  m_outlineRoot      = -1;
  m_maxOutlineLevel  = 0;

  m_inFooter   = false;
  m_lasth      = 0;
  m_fontFamily = wxEmptyString;
  m_fontStyle  = wxPDF_FONTSTYLE_REGULAR;
  m_fontSizePt = 12;
  m_decoration = wxPDF_FONTSTYLE_REGULAR;
  m_fontSubsetting = true;

  m_drawColour  = wxPdfColour();
  m_fillColour  = wxPdfColour();
  m_textColour  = wxPdfColour();
  m_colourFlag  = false;
  m_ws          = 0;
  m_textRenderMode = wxPDF_TEXT_RENDER_FILL;

  // Initialize image scale factor
  m_imgscale = 1.;

  // Page format
  m_curPageSize = m_defPageSize;
  m_fwPt = m_defPageSize.GetWidth() / 254. * 72.;
  m_fhPt = m_defPageSize.GetHeight() / 254. * 72.;
  m_fw = m_fwPt / m_k;
  m_fh = m_fhPt / m_k;

  // Page orientation
  if (orientation == wxLANDSCAPE)
  {
    m_defOrientation = wxLANDSCAPE;
    m_wPt = m_fhPt;
    m_hPt = m_fwPt;
  }
  else // orientation == wxPORTRAIT or unknown
  {
    m_defOrientation = wxPORTRAIT;
    m_wPt = m_fwPt;
    m_hPt = m_fhPt;
  }
  
  m_curOrientation = m_defOrientation;
  m_w = m_wPt / m_k;
  m_h = m_hPt / m_k;
  m_angle = 0;
  m_fillRule = wxWINDING_RULE;
  m_inTransform = 0;

  // Page margins (1 cm)
  double margin = 28.35 / m_k;
  SetMargins(margin, margin);
  
  // Interior cell margin (1 mm)
  m_cMargin = margin / 10;
  
  // Line width (0.2 mm)
  m_lineWidth = .567 / m_k;
  
  // Automatic page break
  SetAutoPageBreak(true, 2*margin);
  
  // Full width display mode
  SetDisplayMode(wxPDF_ZOOM_FULLWIDTH);
  m_zoomFactor = 100.;

  // Default viewer preferences
  m_viewerPrefs = 0;

  // Disable kerning
  SetKerning(false);

  // Enable compression
  SetCompression(true);

  // Set default PDF version number
  m_PDFVersion = wxT("1.3");
  m_importVersion = m_PDFVersion;

  m_encrypted = false;
  m_encryptor = NULL;

  m_javascript = wxEmptyString;

  m_inTemplate = false;
  m_templateId = 0;
  m_templatePrefix = wxT("/TPL");

  m_currentParser = NULL;
  m_currentSource = wxEmptyString;

  m_translate = false;

  m_zapfdingbats = 0;
}

wxPdfDocument::~wxPdfDocument()
{
  wxPdfFontHashMap::iterator font = m_fonts->begin();
  for (font = m_fonts->begin(); font != m_fonts->end(); font++)
  {
    if (font->second != NULL)
    {
      delete font->second;
    }
  }
  delete m_fonts;

  wxPdfImageHashMap::iterator image = m_images->begin();
  for (image = m_images->begin(); image != m_images->end(); image++)
  {
    if (image->second != NULL)
    {
      delete image->second;
    }
  }
  delete m_images;

  wxPdfPageHashMap::iterator page = m_pages->begin();
  for (page = m_pages->begin(); page != m_pages->end(); page++)
  {
    if (page->second != NULL)
    {
      delete page->second;
    }
  }
  delete m_pages;

  wxPdfPageLinksMap::iterator pageLinks = m_pageLinks->begin();
  for (pageLinks = m_pageLinks->begin(); pageLinks != m_pageLinks->end(); pageLinks++)
  {
    if (pageLinks->second != NULL)
    {
      delete pageLinks->second;
    }
  }
  delete m_pageLinks;

  wxPdfLinkHashMap::iterator link = m_links->begin();
  for (link = m_links->begin(); link != m_links->end(); link++)
  {
    if (link->second != NULL)
    {
      delete link->second;
    }
  }
  delete m_links;

  delete m_namedLinks;

  size_t j;
  for (j = 0; j < m_outlines.GetCount(); j++)
  {
    wxPdfBookmark* bookmark = (wxPdfBookmark*) m_outlines[j];
    delete bookmark;
  }

  wxPdfStringHashMap::iterator diff = m_diffs->begin();
  for (diff = m_diffs->begin(); diff != m_diffs->end(); diff++)
  {
    if (diff->second != NULL)
    {
      delete diff->second;
    }
  }
  delete m_diffs;

  delete m_winansi;

  wxPdfExtGStateMap::iterator extGState = m_extGStates->begin();
  for (extGState = m_extGStates->begin(); extGState != m_extGStates->end(); extGState++)
  {
    if (extGState->second != NULL)
    {
      delete extGState->second;
    }
  }
  delete m_extGStates;

  delete m_extGSLookup;

  wxPdfGradientMap::iterator gradient = m_gradients->begin();
  for (gradient = m_gradients->begin(); gradient != m_gradients->end(); gradient++)
  {
    if (gradient->second != NULL)
    {
      delete gradient->second;
    }
  }
  delete m_gradients;

  wxPdfAnnotationsMap::iterator annotation = m_annotations->begin();
  for (annotation = m_annotations->begin(); annotation != m_annotations->end(); annotation++)
  {
    if (annotation->second != NULL)
    {
      delete annotation->second;
    }
  }
  delete m_annotations;

  wxPdfFormAnnotsMap::iterator formAnnotation = m_formAnnotations->begin();
  for (formAnnotation = m_formAnnotations->begin(); formAnnotation != m_formAnnotations->end(); formAnnotation++)
  {
    if (formAnnotation->second != NULL)
    {
      delete formAnnotation->second;
    }
  }
  delete m_formAnnotations;

  wxPdfFormFieldsMap::iterator formField = m_formFields->begin();
  for (formField = m_formFields->begin(); formField != m_formFields->end(); formField++)
  {
    if (formField->second != NULL)
    {
      delete formField->second;
    }
  }
  delete m_formFields;

  wxPdfRadioGroupMap::iterator radioGroup = m_radioGroups->begin();
  for (radioGroup = m_radioGroups->begin(); radioGroup != m_radioGroups->end(); radioGroup++)
  {
    if (radioGroup->second != NULL)
    {
      delete radioGroup->second;
    }
  }
  delete m_radioGroups;

  wxPdfTemplatesMap::iterator templateIter = m_templates->begin();
  for (templateIter = m_templates->begin(); templateIter != m_templates->end(); templateIter++)
  {
    if (templateIter->second != NULL)
    {
      delete templateIter->second;
    }
  }
  delete m_templates;

  wxPdfParserMap::iterator parser = m_parsers->begin();
  for (parser = m_parsers->begin(); parser != m_parsers->end(); parser++)
  {
    if (parser->second != NULL)
    {
      delete parser->second;
    }
  }
  delete m_parsers;

  wxPdfSpotColourMap::iterator spotColour = m_spotColours->begin();
  for (spotColour = m_spotColours->begin(); spotColour != m_spotColours->end(); spotColour++)
  {
    if (spotColour->second != NULL)
    {
      delete spotColour->second;
    }
  }
  delete m_spotColours;

  wxPdfPatternMap::iterator pattern = m_patterns->begin();
  for (pattern = m_patterns->begin(); pattern != m_patterns->end(); pattern++)
  {
    if (pattern->second != NULL)
    {
      delete pattern->second;
    }
  }
  delete m_patterns;

  wxPdfOcgMap::iterator ocg = m_ocgs->begin();
  for (ocg = m_ocgs->begin(); ocg != m_ocgs->end(); ++ocg)
  {
    if (ocg->second != NULL)
    {
      delete ocg->second;
    }
  }
  delete m_ocgs;

  wxPdfLayerRGMap::iterator rg;
  for (rg = m_rgLayers->begin(); rg != m_rgLayers->end(); ++rg)
  {
    if (rg->second != NULL)
    {
      delete rg->second;
    }
  }
  delete m_rgLayers;

  if (m_lockedLayers != NULL)
  {
    delete m_lockedLayers;
  }

  wxPdfAttachmentMap::iterator attach;
  for (attach = m_attachments->begin(); attach != m_attachments->end(); ++attach)
  {
    if (attach->second != NULL)
    {
      delete attach->second;
    }
  }
  delete m_attachments;

  delete m_orientationChanges;
  delete m_pageSizes;

  delete m_offsets;

  if (m_encryptor != NULL)
  {
    delete m_encryptor;
  }

  if (m_buffer != NULL)
  {
    delete m_buffer;
  }
}

// --- Public methods

void
wxPdfDocument::SetProtection(int permissions,
                             const wxString& userPassword,
                             const wxString& ownerPassword,
                             wxPdfEncryptionMethod encryptionMethod,
                             int keyLength)
{
  if (m_encryptor == NULL)
  {
    int revision = (keyLength > 0) ? 3 : 2;
    switch (encryptionMethod)
    {
      case wxPDF_ENCRYPTION_AESV2:
        revision = 4;
        if (m_PDFVersion < wxT("1.6"))
        {
          m_PDFVersion = wxT("1.6");
        }
        break;
      case wxPDF_ENCRYPTION_RC4V2:
        revision = 3;
        break;
      case wxPDF_ENCRYPTION_RC4V1:
      default:
        revision = 2;
        break;
    }
    m_encryptor = new wxPdfEncrypt(revision, keyLength);
    m_encrypted = true;
    int allowedFlags = wxPDF_PERMISSION_PRINT | wxPDF_PERMISSION_MODIFY |
                       wxPDF_PERMISSION_COPY  | wxPDF_PERMISSION_ANNOT;
    int protection = 192;
    protection += (permissions & allowedFlags);
    wxString ownerPswd = ownerPassword;
    if (ownerPswd.Length() == 0)
    {
      ownerPswd = wxPdfUtility::GetUniqueId(wxT("wxPdfDoc"));
    }
    m_encryptor->GenerateEncryptionKey(userPassword, ownerPswd, protection);
  }
}

void
wxPdfDocument::SetImageScale(double scale)
{
  m_imgscale = scale;
}

double
wxPdfDocument::GetImageScale()
{
  return m_imgscale;
}

double
wxPdfDocument::GetPageWidth()
{
  return m_w;
}

double
wxPdfDocument::GetPageHeight()
{
  return m_h;
}

double
wxPdfDocument::GetBreakMargin()
{
  return m_bMargin;
}

double
wxPdfDocument::GetScaleFactor()
{
  return m_k;
}

void
wxPdfDocument::AliasNbPages(const wxString& alias)
{
  // Define an alias for total number of pages
  m_aliasNbPages = alias;
}

void
wxPdfDocument::Open()
{
  // Begin document
  m_state = 1;
}

void
wxPdfDocument::AddPage(int orientation)
{
  AddPage(orientation, m_defPageSize);
}

void
wxPdfDocument::AddPage(int orientation, wxPaperSize format)
{
  wxSize pageSize = CalculatePageSize(format);
  AddPage(orientation, pageSize);
}

void
wxPdfDocument::AddPage(int orientation, double pageWidth, double pageHeight)
{
  if (pageWidth > 0 && pageHeight > 0)
  {
    wxSize pageSize = CalculatePageSize(pageWidth, pageHeight);
    AddPage(orientation, pageSize);
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfDocument::AddPage: ")) +
               wxString::Format(_("Invalid page size (%.1f,%.1f)."), pageWidth, pageHeight));
  }
}

void
wxPdfDocument::AddPage(int orientation, wxSize pageSize)
{
  if (m_inTemplate)
  {
    wxLogError(wxString(wxT("wxPdfDocument::AddPage: ")) +
               wxString::Format(_("Adding pages in templates is impossible. Current template ID is %d."), m_templateId));
    return;
  }

  // Start a new page
  if (m_state == 0)
  {
    Open();
  }
  wxPdfFontDetails* currentFont = m_currentFont;
  wxString family = m_fontFamily;
  int style = m_fontStyle;
  if (m_decoration & wxPDF_FONTSTYLE_UNDERLINE)
  {
    style |= wxPDF_FONTSTYLE_UNDERLINE;
  }
  if (m_decoration & wxPDF_FONTSTYLE_OVERLINE)
  {
    style |= wxPDF_FONTSTYLE_OVERLINE;
  }
  if (m_decoration & wxPDF_FONTSTYLE_STRIKEOUT)
  {
    style |= wxPDF_FONTSTYLE_STRIKEOUT;
  }
  double size = m_fontSizePt;
  double lw = m_lineWidth;
  wxPdfColour dc = m_drawColour;
  wxPdfColour fc = m_fillColour;
  wxPdfColour tc = m_textColour;
  bool cf = m_colourFlag;

  if (m_page > 0)
  {
    // Page footer
    m_inFooter = true;
    Footer();
    m_inFooter = false;
    // Close page
    EndPage();
  }

  // Start new page
  BeginPage(orientation, pageSize);
  
  // Set line cap style to square
  Out("2 J");
  
  // Set line width
  m_lineWidth = lw;
  OutAscii(wxPdfUtility::Double2String(lw*m_k,2)+wxString(wxT(" w")));

  // Set font
  if (currentFont != NULL)
  {
    m_currentFont = currentFont;
    m_fontStyle = style;
    m_fontSizePt = size;
    ForceCurrentFont();
  }
  
  // Set colours
  m_drawColour = dc;
  if (dc != wxPdfColour(0))
  {
    OutAscii(dc.GetColour(true));
  }
  m_fillColour = fc;
  if (fc != wxPdfColour(0))
  {
    OutAscii(fc.GetColour(false));
  }
  m_textColour = tc;
  m_colourFlag = cf;

  // Page header
  Header();

  // Restore line width
  if (m_lineWidth != lw)
  {
    m_lineWidth = lw;
    OutAscii(wxPdfUtility::Double2String(lw*m_k,2)+wxString(wxT(" w")));
  }

  // Restore font
  if(family.Length() > 0)
  {
    SetFont(family, style, size);
  }
  if (currentFont != NULL)
  {
    SetFont(currentFont->GetUserFont(), style, size);
  }
  
  // Restore colours
  if (m_drawColour != dc)
  {
    m_drawColour = dc;
    OutAscii(dc.GetColour(true));
  }
  if (m_fillColour != fc)
  {
    m_fillColour = fc;
    OutAscii(fc.GetColour(false));
  }
  m_textColour = tc;
  m_colourFlag = cf;
}

void
wxPdfDocument::SetLineWidth(double width)
{
  // Set line width
  m_lineWidth = width;
  if (m_page > 0)
  {
    OutAscii(wxPdfUtility::Double2String(width*m_k,2)+ wxString(wxT(" w")));
  }
}

double
wxPdfDocument::GetLineWidth()
{
  return m_lineWidth;
}

bool
wxPdfDocument::AddFont(const wxString& family, const wxString& style, const wxString& file)
{
  bool ok = !family.IsEmpty();
  if (ok)
  {
    wxPdfFont regFont = wxPdfFontManager::GetFontManager()->GetFont(family, style);
    if (!regFont.IsValid())
    {
      wxString fileName = file;
      if (fileName.IsEmpty())
      {
        fileName = family.Lower() + style.Lower() + wxString(wxT(".xml"));
        fileName.Replace(wxT(" "),wxT(""));
      }
      regFont = wxPdfFontManager::GetFontManager()->RegisterFont(fileName, family);
      ok = regFont.IsValid();
    }
  }
  return ok;
}

#if wxUSE_UNICODE

bool
wxPdfDocument::AddFontCJK(const wxString& family)
{
  bool ok = !family.IsEmpty();
  if (ok)
  {
    wxPdfFont regFont = wxPdfFontManager::GetFontManager()->GetFont(family);
    if (!regFont.IsValid())
    {
      ok = wxPdfFontManager::GetFontManager()->RegisterFontCJK(family);
    }
  }
  return ok;
}

#endif // wxUSE_UNICODE

bool
wxPdfDocument::SetFont(const wxString& family, const wxString& style, double size)
{
  return SelectFont(family, style, size);
}

bool
wxPdfDocument::SetFont(const wxString& family, int style, double size)
{
  return SelectFont(family, style, size);
}

bool
wxPdfDocument::SetFont(const wxPdfFont& font, int style, double size)
{
  return SelectFont(font, style, size);
}

bool
wxPdfDocument::SetFont(const wxFont& font)
{
  return SelectFont(font);
}

void
wxPdfDocument::SetFontSize(double size)
{
  SetFontSize(size, true);
}

void
wxPdfDocument::SetFontSize(double size, bool setSize)
{
  if (m_currentFont == NULL)
  {
    wxLogError(wxString(wxT("wxPdfDocument::SetFontSize: ")) +
      wxString(_("No font selected.")));
    return;
  }
  // Set font size in points
  if (m_fontSizePt == size)
  {
    return;
  }
  m_fontSizePt = size;
  m_fontSize = size / m_k;
  if (setSize && m_page > 0)
  {
    OutAscii(wxString::Format(wxT("BT /F%d "),m_currentFont->GetIndex()) +
             wxPdfUtility::Double2String(m_fontSizePt,2) + wxString(wxT(" Tf ET")));
  }
}

wxPdfFont
wxPdfDocument::GetCurrentFont() const
{
  if (m_currentFont == NULL)
  {
    wxLogError(wxString(wxT("wxPdfDocument::GetCurrentFont: ")) +
               wxString(_("No font selected.")));
    return wxPdfFont();
  }
  return m_currentFont->GetUserFont();
}

const wxPdfFontDescription&
wxPdfDocument::GetFontDescription() const
{
  if (m_currentFont == NULL)
  {
    wxLogError(wxString(wxT("wxPdfDocument::SetFontSize: ")) +
               wxString(_("No font selected.")));
    static wxPdfFontDescription dummy;
    return dummy;
  }
  return m_currentFont->GetDescription();
}

const wxString
wxPdfDocument::GetFontFamily()
{
  return m_fontFamily;
}

const wxString
wxPdfDocument::GetFontStyle() const
{
  wxString style = wxEmptyString;
  int styles = GetFontStyles();
  if (styles & wxPDF_FONTSTYLE_BOLD)
  {
    style += wxString(wxT("B"));
  }
  if (styles & wxPDF_FONTSTYLE_ITALIC)
  {
    style += wxString(wxT("I"));
  }
  if (styles & wxPDF_FONTSTYLE_UNDERLINE)
  {
    style += wxString(wxT("U"));
  }
  if (styles & wxPDF_FONTSTYLE_OVERLINE)
  {
    style += wxString(wxT("O"));
  }
  if (styles & wxPDF_FONTSTYLE_STRIKEOUT)
  {
    style += wxString(wxT("S"));
  }
  return style;
}

int
wxPdfDocument::GetFontStyles() const
{
  return m_fontStyle | m_decoration;
}

double
wxPdfDocument::GetFontSize() const
{
  return m_fontSizePt;
}

double
wxPdfDocument::GetStringWidth(const wxString& s)
{
  wxString voText = ApplyVisualOrdering(s);
  return DoGetStringWidth(voText);
}

double
wxPdfDocument::DoGetStringWidth(const wxString& s)
{
  double w = 0;
  if (m_currentFont != 0)
  {
    w = m_currentFont->GetStringWidth(s, m_kerning) * m_fontSize;
  }
  return w;
}

void
wxPdfDocument::Text(double x, double y, const wxString& txt)
{
  // Output a string
  wxString voText = ApplyVisualOrdering(txt);

  if (m_colourFlag)
  {
    Out("q ", false);
    OutAscii(m_textColour.GetColour(false), false);
    Out(" ", false);
  }
  if (m_yAxisOriginTop)
  {
    OutAscii(wxString(wxT("BT 1 0 0 -1 ")) +
             wxPdfUtility::Double2String(x*m_k,2) + wxString(wxT(" ")) +
             wxPdfUtility::Double2String(y*m_k,2) + wxString(wxT(" Tm ")), false);
  }
  else
  {
    OutAscii(wxString(wxT("BT ")) +
             wxPdfUtility::Double2String(x*m_k,2) + wxString(wxT(" ")) +
             wxPdfUtility::Double2String(y*m_k,2) + wxString(wxT(" Td ")), false);
  }
  OutAscii(wxString::Format(wxT("%d Tr "), m_textRenderMode), false);
  ShowText(voText);
  Out("ET", false);

  if ((m_decoration & wxPDF_FONTSTYLE_DECORATION_MASK) && voText.Length() > 0)
  {
    Out(" ", false);
    OutAscii(DoDecoration(x, y, voText), false);
  }

  if (m_colourFlag)
  {
    Out(" Q", false);
  }
  Out("\n", false);
}

void
wxPdfDocument::RotatedText(double x, double y, const wxString& txt, double angle)
{
  // Text rotated around its origin
  if (angle == 0)
  {
    Text(x, y, txt);
  }
  else
  {
    StartTransform();
    Rotate(angle, x, y);
    Text(x, y, txt);
    StopTransform();
  }
}

void
wxPdfDocument::RotatedText(double textX, double textY, double rotationX, double rotationY, const wxString& txt, double angle)
{
  // Text rotated around its origin
  if (angle == 0)
  {
    Text(textX, textY, txt);
  }
  else
  {
    StartTransform();
    Rotate(angle, rotationX, rotationY);
    Text(textX, textY, txt);
    StopTransform();
  }
}

bool
wxPdfDocument::AcceptPageBreak()
{
  // Accept automatic page break or not
  return m_autoPageBreak;
}

void
wxPdfDocument::Cell(double w, double h, const wxString& txt, int border, int ln, int align, int fill, const wxPdfLink& link)
{
  wxString voText = ApplyVisualOrdering(txt);
  DoCell(w, h, voText, border, ln, align, fill, link);
}

void
wxPdfDocument::DoCell(double w, double h, const wxString& txt, int border, int ln, int align, int fill, const wxPdfLink& link)
{
  // Output a cell
  double x, y;
  double k = m_k;
  bool doPageBreak = (m_yAxisOriginTop) ? (m_y+h > m_pageBreakTrigger) : (m_y-h < m_pageBreakTrigger);
  if (doPageBreak && !m_inFooter && AcceptPageBreak())
  {
    // Automatic page break
    x = m_x;
    double ws = m_ws;
    if (ws > 0)
    {
      m_ws = 0;
      Out("0 Tw");
    }
    AddPage(m_curOrientation);
    m_x = x;
    if (ws > 0)
    {
      m_ws = ws;
      OutAscii(wxPdfUtility::Double2String(ws*k,3)+wxString(wxT(" Tw")));
    }
  }
  if ( w == 0)
  {
    w = m_w - m_rMargin - m_x;
  }
  wxString s = wxEmptyString;
  if (fill == 1 || border == wxPDF_BORDER_FRAME)
  {
    s = wxPdfUtility::Double2String(m_x*k,2) + wxString(wxT(" ")) +
        wxPdfUtility::Double2String(m_y*k,2) + wxString(wxT(" ")) +
        wxPdfUtility::Double2String(w*k,2) + wxString(wxT(" ")) +
        wxPdfUtility::Double2String(h*k,2);
    if (fill == 1)
    {
      if (border == wxPDF_BORDER_FRAME)
      {
        s += wxString(wxT(" re B "));
      }
      else
      {
        s += wxString(wxT(" re f "));
      }
    }
    else
    {
      s += wxString(wxT(" re S "));
    }
  }
  if (border != wxPDF_BORDER_NONE && border != wxPDF_BORDER_FRAME)
  {
    x = m_x;
    y = m_y;
    if (border & wxPDF_BORDER_LEFT)
    {
      s += wxPdfUtility::Double2String(x*k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(y*k,2) + wxString(wxT(" m ")) +
           wxPdfUtility::Double2String(x*k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((y+h)*k,2) + wxString(wxT(" l S "));
    }
    if (border & wxPDF_BORDER_TOP)
    {
      s += wxPdfUtility::Double2String(x*k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(y*k,2) + wxString(wxT(" m ")) +
           wxPdfUtility::Double2String((x+w)*k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(y*k,2) + wxString(wxT(" l S "));
    }
    if (border & wxPDF_BORDER_RIGHT)
    {
      s += wxPdfUtility::Double2String((x+w)*k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(y*k,2) + wxString(wxT(" m ")) +
           wxPdfUtility::Double2String((x+w)*k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((y+h)*k,2) + wxString(wxT(" l S "));
    }
    if (border & wxPDF_BORDER_BOTTOM)
    {
      s += wxPdfUtility::Double2String(x*k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((y+h)*k,2) + wxString(wxT(" m ")) +
           wxPdfUtility::Double2String((x+w)*k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((y+h)*k,2) + wxString(wxT(" l S "));
    }
  }
  if (s.Length() > 0)
  {
    bool newline = txt.Length() == 0;
    OutAscii(s, newline);
    s = wxT("");
  }
  
  if (txt.Length() > 0)
  {
    double width = DoGetStringWidth(txt);
    double dx;
    if (align == wxPDF_ALIGN_RIGHT)
    {
      dx = w - m_cMargin - width;
    }
    else if (align == wxPDF_ALIGN_CENTER)
    {
      dx = (w - width) / 2;
    }
    else
    {
      dx = m_cMargin;
    }
    if (m_colourFlag)
    {
      s += wxString(wxT("q ")) + m_textColour.GetColour(false) + wxString(wxT(" "));
    }
    if (m_yAxisOriginTop)
    {
      s += wxString(wxT("BT 1 0 0 -1 ")) +
           wxPdfUtility::Double2String((m_x+dx)*k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((m_y+.5*h+.3*m_fontSize)*k,2) + wxString(wxT(" Tm "));
    }
    else
    {
      s += wxString(wxT("BT ")) +
           wxPdfUtility::Double2String((m_x+dx)*k,2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String((m_y+.5*h+.3*m_fontSize)*k,2) + wxString(wxT(" Td "));
    }
    OutAscii(s,false);
    OutAscii(wxString::Format(wxT("%d Tr "), m_textRenderMode), false);
    ShowText(txt);
    s = wxT(" ET");

    if (m_decoration & wxPDF_FONTSTYLE_DECORATION_MASK)
    {
      s += wxString(wxT(" ")) + DoDecoration(m_x+dx,m_y+.5*h+.3*m_fontSize,txt);
    }
    if (m_colourFlag)
    {
      s += wxString(wxT(" Q"));
    }
    if (link.IsValid())
    {
      Link(m_x+dx,m_y+.5*h-.5*m_fontSize,width,m_fontSize,link);
    }
    OutAscii(s);
  }
  m_lasth = h;
  if (ln > 0)
  {
    // Go to next line
    if (m_yAxisOriginTop)
    {
      m_y += h;
    }
    else
    {
      m_y -= h;
    }
    if ( ln == 1)
    {
      m_x = m_lMargin;
    }
  }
  else
  {
    m_x += w;
  }
}

int
wxPdfDocument::MultiCell(double w, double h, const wxString& txt, int border, int align, int fill, int maxline)
{
  // Output text with automatic or explicit line breaks
  if (w == 0)
  {
    w = m_w - m_rMargin - m_x;
  }

  double wmax = (w - 2 * m_cMargin);
  wxString s = ApplyVisualOrdering(txt);
  s.Replace(wxT("\r"),wxT("")); // remove carriage returns
  int nb = (int) s.Length();
  if (nb > 0 && s[nb-1] == wxT('\n'))
  {
    nb--;
  }

  int b = wxPDF_BORDER_NONE;
  int b2 = wxPDF_BORDER_NONE;
  if (border != wxPDF_BORDER_NONE)
  {
    if (border == wxPDF_BORDER_FRAME)
    {
      b = wxPDF_BORDER_LEFT | wxPDF_BORDER_RIGHT | wxPDF_BORDER_TOP;
      b2 = wxPDF_BORDER_LEFT | wxPDF_BORDER_RIGHT;
    }
    else
    {
      b2 = wxPDF_BORDER_NONE;
      if (border & wxPDF_BORDER_LEFT)
      {
        b2 = b2 | wxPDF_BORDER_LEFT;
      }
      if (border & wxPDF_BORDER_RIGHT)
      {
        b2 = b2 | wxPDF_BORDER_RIGHT;
      }
      b = (border & wxPDF_BORDER_TOP) ? b2 | wxPDF_BORDER_TOP : b2;
    }
  }
  int sep = -1;
  int i = 0;
  int j = 0;
  double len = 0;
  double ls = 0;
  int ns = 0;
  int nl = 1;
  wxChar c;
  while (i < nb)
  {
    // Get next character
    c = s[i];
    if (c == wxT('\n'))
    {
      // Explicit line break
      if (m_ws > 0)
      {
        m_ws = 0;
        Out("0 Tw");
      }
      DoCell(w,h,s.SubString(j,i-1),b,2,align,fill);
      i++;
      sep = -1;
      j = i;
      len = 0;
      ns = 0;
      nl++;
      if (border != wxPDF_BORDER_NONE && nl == 2)
      {
        b = b2;
      }
      if (maxline > 0 && nl > maxline)
      {
        return j;
      }
      continue;
    }
    if (c == wxT(' '))
    {
      sep = i;
      ls = len;
      ns++;
    }
    len = DoGetStringWidth(s.SubString(j, i));

    if (len > wmax)
    {
      // Automatic line break
      if (sep == -1)
      {
        if (i == j)
        {
          i++;
        }
        if (m_ws > 0)
        {
          m_ws=0;
          Out("0 Tw");
        }
        DoCell(w,h,s.SubString(j,i-1),b,2,align,fill);
      }
      else
      {
        if (align == wxPDF_ALIGN_JUSTIFY)
        {
          m_ws = (ns > 1) ? (wmax - ls)/(ns-1) : 0;
          OutAscii(wxPdfUtility::Double2String(m_ws*m_k,3)+wxString(wxT(" Tw")));
        }
        DoCell(w,h,s.SubString(j,sep-1),b,2,align,fill);
        i = sep + 1;
      }
      sep = -1;
      j = i;
      len = 0;
      ns = 0;
      nl++;
      if (border != wxPDF_BORDER_NONE && nl == 2)
      {
        b = b2;
      }
      if (maxline > 0 && nl > maxline)
      {
        return j;
      }
    }
    else
    {
      i++;
    }
  }
  // Last chunk
  if (m_ws > 0)
  {
    m_ws = 0;
    Out("0 Tw");
  }
  if ((border != wxPDF_BORDER_NONE) && (border & wxPDF_BORDER_BOTTOM))
  {
    b = b | wxPDF_BORDER_BOTTOM;
  }
  DoCell(w,h,s.SubString(j,i-1),b,2,align,fill);
  m_x = m_lMargin;
  return i;
}

int
wxPdfDocument::LineCount(double w, const wxString& txt)
{
  // Output text with automatic or explicit line breaks
  if (w == 0)
  {
    w = m_w - m_rMargin - m_x;
  }

  double wmax = (w - 2 * m_cMargin);
  wxString s = txt;
  s.Replace(wxT("\r"),wxT("")); // remove carriage returns
  int nb = (int) s.Length();
  if (nb > 0 && s[nb-1] == wxT('\n'))
  {
    nb--;
  }

  int sep = -1;
  int i = 0;
  int j = 0;
  double len = 0;
  int nl = 1;
  wxChar c;
  while (i < nb)
  {
    // Get next character
    c = s[i];
    if (c == wxT('\n'))
    {
      // Explicit line break
      i++;
      sep = -1;
      j = i;
      len = 0;
      nl++;
      continue;
    }
    if (c == wxT(' '))
    {
      sep = i;
    }
    len = DoGetStringWidth(s.SubString(j, i));

    if (len > wmax)
    {
      // Automatic line break
      if (sep == -1)
      {
        if (i == j)
        {
          i++;
        }
      }
      else
      {
        i = sep + 1;
      }
      sep = -1;
      j = i;
      len = 0;
      nl++;
    }
    else
    {
      i++;
    }
  }
  return nl;
}

int
wxPdfDocument::TextBox(double w, double h, const wxString& txt,
                       int halign, int valign, int border, int fill)
{
  double xi = m_x;
  double yi = m_y;
  
  double hrow  = m_fontSize;
  int textrows = LineCount(w, txt);
  int maxrows  = (int) floor(h / hrow);
  int rows     = (textrows < maxrows) ? textrows : maxrows;

  double dy = 0;
  if (valign == wxPDF_ALIGN_MIDDLE)
  {
    dy = (h - rows * hrow) / 2;
  }
  else if (valign == wxPDF_ALIGN_BOTTOM)
  {
    dy = h - rows * hrow;
  }

  SetY(yi+dy);
  SetX(xi);
  int trail = MultiCell(w, hrow, txt, 0, halign, fill, rows);

  if (border == wxPDF_BORDER_FRAME)
  {
    Rect(xi, yi, w, h);
  }
  else
  {
    if (border & wxPDF_BORDER_LEFT)   Line(xi,yi,xi,yi+h);
    if (border & wxPDF_BORDER_RIGHT)  Line(xi+w,yi,xi+w,yi+h);
    if (border & wxPDF_BORDER_TOP)    Line(xi,yi,xi+w,yi);
    if (border & wxPDF_BORDER_BOTTOM) Line(xi,yi+h,xi+w,yi+h);
  }

  return trail;
}

void
wxPdfDocument::Write(double h, const wxString& txt, const wxPdfLink& link)
{
  WriteCell(h, txt, wxPDF_BORDER_NONE, 0, link);
}

void
wxPdfDocument::WriteCell(double h, const wxString& txt, int border, int fill, const wxPdfLink& link)
{
  // Output text in flowing mode
  wxString s = ApplyVisualOrdering(txt);

  s.Replace(wxT("\r"),wxT("")); // remove carriage returns
  int nb = (int) s.Length();

  // handle single space character
  if ((nb == 1) && s[0] == wxT(' '))
  {
    m_x += DoGetStringWidth(s);
    return;
  }

  double saveCellMargin = GetCellMargin();
  SetCellMargin(0);

  double w = m_w - m_rMargin - m_x;
  double wmax = (w - 2 * m_cMargin) + wxPDF_EPSILON;

  int sep = -1;
  int i = 0;
  int j = 0;
  double len=0;
  int nl = 1;
  wxChar c;
  while (i < nb)
  {
    // Get next character
    c = s[i];
    if (c == wxT('\n'))
    {
      // Explicit line break
      DoCell(w, h, s.SubString(j,i-1), border, 2, wxPDF_ALIGN_LEFT, fill, link);
      i++;
      sep = -1;
      j = i;
      len = 0;
      if (nl == 1)
      {
        m_x = m_lMargin;
        w = m_w - m_rMargin - m_x;
        wmax = (w - 2 * m_cMargin);
      }
      nl++;
      continue;
    }
    if (c == wxT(' '))
    {
      sep = i;
    }
    len = DoGetStringWidth(s.SubString(j, i));
    if (len > wmax)
    {
      // Automatic line break
      if (sep == -1)
      {
        if (m_x > m_lMargin)
        {
          // Move to next line
          m_x = m_lMargin;
          if (m_yAxisOriginTop)
          {
            m_y += h;
          }
          else
          {
            m_y -= h;
          }
          w = m_w - m_rMargin - m_x;
          wmax = (w - 2 * m_cMargin);
          i++;
          nl++;
          continue;
        }
        if (i == j)
        {
          i++;
        }
        DoCell(w, h,s.SubString(j, i-1), border, 2, wxPDF_ALIGN_LEFT, fill, link);
      }
      else
      {
        DoCell(w, h, s.SubString(j, sep-1), border, 2, wxPDF_ALIGN_LEFT, fill, link);
        i = sep + 1;
      }
      sep = -1;
      j = i;
      len = 0;
      if (nl == 1)
      {
        m_x = m_lMargin;
        w = m_w - m_rMargin - m_x;
        wmax = (w - 2 * m_cMargin);
      }
      nl++;
    }
    else
    {
      i++;
    }
  }
  // Last chunk
  if (i != j)
  {
    DoCell(len, h, s.SubString(j,i-1), border, 0, wxPDF_ALIGN_LEFT, fill, link);
  }

  // Following statement was in PHP code, but seems to be in error.
  // m_x += GetStringWidth(s.SubString(j, i-1));
  SetCellMargin(saveCellMargin);
}

bool
wxPdfDocument::WriteGlyphArray(wxPdfArrayDouble& x, wxPdfArrayDouble& y, wxPdfArrayUint32& glyphs)
{
  bool ok = m_currentFont != NULL;
#if wxUSE_UNICODE
  if (ok)
  {
    // Check whether the current font is valid for this method
    wxString fontType = m_currentFont->GetType();
    if (fontType.IsSameAs(wxT("TrueTypeUnicode")) || fontType.IsSameAs(wxT("OpenTypeUnicode")))
    {
      // if the arrays have different sizes use only the smallest size 
      size_t nx = x.GetCount();
      size_t ny = y.GetCount();
      size_t ng = glyphs.GetCount();
      size_t n = (nx > ny) ? ((ny > ng) ? ng : ny) : ((nx > ng) ? ng : nx);
      double xp, yp;
      size_t j;
      for (j = 0; j < n; ++j)
      {
        xp = m_x + x[j];
        yp = m_y + y[j];
        if (m_yAxisOriginTop)
        {
          Out("BT 1 0 0 -1 ", false);
        }
        else
        {
          Out("BT ", false);
        }
        OutAscii(wxPdfUtility::Double2String(xp*m_k,2), false);
        Out(" ", false);
        OutAscii(wxPdfUtility::Double2String(yp*m_k,2), false);
        if (m_yAxisOriginTop)
        {
          Out(" Tm ", false);
        }
        else
        {
          Out(" Td ", false);
        }
        ShowGlyph(glyphs[j]);
        Out(" ET");
      }
    }
    else
    {
      ok = false;
      wxLogError(wxString(wxT("wxPdfDocument::WriteGlyphArray: ")) +
                 wxString::Format(_("Font type '%s' not supported."), fontType.c_str()));
    }
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfDocument::WriteGlyphArray: ")) +
               wxString(_("No font selected.")));
  }
#else
  wxUnusedVar(x);
  wxUnusedVar(y);
  wxUnusedVar(glyphs);
  wxLogError(wxString(wxT("wxPdfDocument::WriteGlyphArray: ")) +
             wxString(_("Supported in Unicode build only.")));
#endif // wxUSE_UNICODE
  return ok;
}

bool
wxPdfDocument::Image(const wxString& file, double x, double y, double w, double h,
                     const wxString& type, const wxPdfLink& link, int maskImage)
{
  wxPdfImage* currentImage = NULL;
  // Put an image on the page
  wxPdfImageHashMap::iterator image = (*m_images).find(file);
  if (image == (*m_images).end())
  {
    // First use of image, get info
    int i = (int) (*m_images).size() + 1;
    currentImage = new wxPdfImage(this, i, file, type);
    if (!currentImage->Parse())
    {
      bool isValid = false;
      delete currentImage;

      if (wxImage::FindHandler(wxBITMAP_TYPE_PNG) == NULL)
      {
        wxImage::AddHandler(new wxPNGHandler());
      }
      wxImage tempImage;
      tempImage.LoadFile(file);
      if (tempImage.Ok())
      {
        isValid = Image(file, tempImage, x, y, w, h, link, maskImage);
      }
      return isValid;
    }
    if (maskImage > 0)
    {
      currentImage->SetMaskImage(maskImage);
    }
    (*m_images)[file] = currentImage;
  }
  else
  {
    currentImage = image->second;
    if (maskImage > 0 && currentImage->GetMaskImage() != maskImage)
    {
      currentImage->SetMaskImage(maskImage);
    }
  }
  OutImage(currentImage, x, y, w, h, link);
  return true;
}

bool
wxPdfDocument::Image(const wxString& name, const wxImage& img, double x, double y, double w, double h,
                     const wxPdfLink& link, int maskImage, bool jpegFormat, int jpegQuality)
{
  bool isValid = false;
  if (img.Ok())
  {
    wxImage tempImage = img.Copy();
    wxPdfImage* currentImage = NULL;
    // Put an image on the page
    wxPdfImageHashMap::iterator image = (*m_images).find(name);
    if (image == (*m_images).end())
    {
      if (tempImage.HasAlpha())
      {
        if (maskImage <= 0)
        {
          maskImage = ImageMask(name+wxString(wxT(".mask")), tempImage);
        }
        if(!tempImage.ConvertAlphaToMask(0))
        {
          return false;
        }
      }
      else if (tempImage.HasMask() && maskImage <= 0)
      {
        // Extract the mask
        wxImage mask = tempImage.ConvertToMono(tempImage.GetMaskRed(), tempImage.GetMaskGreen(), tempImage.GetMaskBlue());
        // Invert the mask
        mask = mask.ConvertToMono(0, 0, 0);
        maskImage = ImageMask(name+wxString(wxT(".mask")), mask);
      }
      // First use of image, get info
      tempImage.SetMask(false);
      if (jpegFormat)
      {
        tempImage.SetOption(wxIMAGE_OPTION_QUALITY, jpegQuality);
      }
      int i = (int) (*m_images).size() + 1;
      currentImage = new wxPdfImage(this, i, name, tempImage, jpegFormat);
      if (!currentImage->Parse())
      {
        delete currentImage;
        return false;
      }
      if (maskImage > 0)
      {
        currentImage->SetMaskImage(maskImage);
      }
      (*m_images)[name] = currentImage;
    }
    else
    {
      currentImage = image->second;
      if (maskImage > 0 && currentImage->GetMaskImage() != maskImage)
      {
        currentImage->SetMaskImage(maskImage);
      }
    }
    OutImage(currentImage, x, y, w, h, link);
    isValid = true;
  }
  return isValid;
}

bool
wxPdfDocument::Image(const wxString& name, wxInputStream& stream,
                     const wxString& mimeType,
                     double x, double y, double w, double h,
                     const wxPdfLink& link, int maskImage)
{
  bool isValid = false;
  wxPdfImage* currentImage = NULL;
  // Put an image on the page
  wxPdfImageHashMap::iterator image = (*m_images).find(name);
  if (image == (*m_images).end())
  {
    // First use of image, get info
    int i = (int) (*m_images).size() + 1;
    currentImage = new wxPdfImage(this, i, name, stream, mimeType);
    if (!currentImage->Parse())
    {
      delete currentImage;
      if (wxImage::FindHandler(wxBITMAP_TYPE_PNG) == NULL)
      {
        wxImage::AddHandler(new wxPNGHandler());
      }
      wxImage tempImage;
      tempImage.LoadFile(stream, mimeType);
      if (tempImage.Ok())
      {
        isValid = Image(name, tempImage, x, y, w, h, link, maskImage);
      }
      return isValid;

    }
    if (maskImage > 0)
    {
      currentImage->SetMaskImage(maskImage);
    }
    (*m_images)[name] = currentImage;
  }
  else
  {
    currentImage = image->second;
    if (maskImage > 0 && currentImage->GetMaskImage() != maskImage)
    {
      currentImage->SetMaskImage(maskImage);
    }
  }
  OutImage(currentImage, x, y, w, h, link);
  isValid = true;
  return isValid;
}

int
wxPdfDocument::ImageMask(const wxString& file, const wxString& type)
{
  int n = 0;
  wxPdfImage* currentImage = NULL;
  // Put an image on the page
  wxPdfImageHashMap::iterator image = (*m_images).find(file);
  if (image == (*m_images).end())
  {
    // First use of image, get info
    n = (int) (*m_images).size() + 1;
    currentImage = new wxPdfImage(this, n, file, type);
    if (!currentImage->Parse())
    {
      delete currentImage;
      return 0;
    }
    // Check whether this is a gray scale image (must be)
    if (currentImage->GetColourSpace() != wxT("DeviceGray"))
    {
      delete currentImage;
      return 0;
    }
    (*m_images)[file] = currentImage;
  }
  else
  {
    currentImage = image->second;
    n = currentImage->GetIndex();
  }
  if (m_PDFVersion < wxT("1.4"))
  {
    m_PDFVersion = wxT("1.4");
  }
  return n;
}

int
wxPdfDocument::ImageMask(const wxString& name, const wxImage& img)
{
  int n = 0;
  if (img.Ok())
  {
    wxPdfImage* currentImage = NULL;
    // Put an image on the page
    wxPdfImageHashMap::iterator image = (*m_images).find(name);
    if (image == (*m_images).end())
    {
      wxImage tempImage;
      if (img.HasAlpha())
      {
        int x, y;
        int w = img.GetWidth();
        int h = img.GetHeight();
        tempImage = wxImage(w, h);
        unsigned char alpha;
        for (x = 0; x < w; x++)
        {
          for (y = 0; y < h; y++)
          {
            alpha = img.GetAlpha(x, y);
            tempImage.SetRGB(x, y, alpha, alpha, alpha);
          }
        }
        tempImage.SetOption(wxIMAGE_OPTION_PNG_FORMAT, wxPNG_TYPE_GREY_RED);
      }
      else
      {
        tempImage = img.ConvertToGreyscale();
        tempImage.SetOption(wxIMAGE_OPTION_PNG_FORMAT, wxPNG_TYPE_GREY_RED);
      }
      tempImage.SetMask(false);
      // First use of image, get info
      n = (int) (*m_images).size() + 1;
      currentImage = new wxPdfImage(this, n, name, tempImage);
      if (!currentImage->Parse())
      {
        delete currentImage;
        return 0;
      }
      (*m_images)[name] = currentImage;
    }
    else
    {
      currentImage = image->second;
      n = currentImage->GetIndex();
    }
    if (m_PDFVersion < wxT("1.4"))
    {
      m_PDFVersion = wxT("1.4");
    }
  }
  return n;
}

int
wxPdfDocument::ImageMask(const wxString& name, wxInputStream& stream, const wxString& mimeType)
{
  int n = 0;
  wxPdfImage* currentImage = NULL;
  // Put an image on the page
  wxPdfImageHashMap::iterator image = (*m_images).find(name);
  if (image == (*m_images).end())
  {
    // First use of image, get info
    n = (int) (*m_images).size() + 1;
    currentImage = new wxPdfImage(this, n, name, stream, mimeType);
    if (!currentImage->Parse())
    {
      delete currentImage;
      return 0;
    }
    // Check whether this is a gray scale image (must be)
    if (currentImage->GetColourSpace() != wxT("DeviceGray"))
    {
      delete currentImage;
      return 0;
    }
    (*m_images)[name] = currentImage;
  }
  else
  {
    currentImage = image->second;
    n = currentImage->GetIndex();
  }
  if (m_PDFVersion < wxT("1.4"))
  {
    m_PDFVersion = wxT("1.4");
  }
  return n;
}

void
wxPdfDocument::RotatedImage(const wxString& file, double x, double y, double w, double h,
                            double angle, const wxString& type, const wxPdfLink& link, int maskImage)
{
  // Image rotated around its upper-left corner
  StartTransform();
  Rotate(angle, x, y);
  Image(file, x, y, w, h, type, link, maskImage);
  StopTransform();
}

void
wxPdfDocument::Ln(double h)
{
  // Line feed; default value is last cell height
  m_x = m_lMargin;
  if (h < 0)
  {
    if (m_yAxisOriginTop)
    {
      m_y += m_lasth;
    }
    else
    {
      m_y -= m_lasth;
    }
  }
  else
  {
    if (m_yAxisOriginTop)
    {
      m_y += h;
    }
    else
    {
      m_y -= h;
    }
  }
}

void
wxPdfDocument::SaveAsFile(const wxString& name)
{
  wxString fileName = name;
  // Normalize parameters
  if(fileName.Length() == 0)
  {
    fileName = wxT("doc.pdf");
  }

  wxFileOutputStream outfile(fileName);

  // Finish document if necessary
  if (m_state < 3)
  {
    if (m_buffer != NULL)
    {
      delete m_buffer;
    }
    m_buffer = &outfile;
    Close();
    m_buffer = NULL;
  }
  else
  {
    // Save to local file
    wxMemoryInputStream tmp(*((wxMemoryOutputStream*) m_buffer));
    outfile.Write(tmp);
  }
  outfile.Close();
}

const wxMemoryOutputStream&
wxPdfDocument::CloseAndGetBuffer()
{
  if (m_state < 3)
  {
    Close();
  }
  
  return *((wxMemoryOutputStream*) m_buffer);
}

void
wxPdfDocument::SetViewerPreferences(int preferences)
{
  m_viewerPrefs = (preferences > 0) ? preferences : 0;
  if (((m_viewerPrefs & wxPDF_VIEWER_DISPLAYDOCTITLE) != 0) && (m_PDFVersion < wxT("1.4")))
  {
    m_PDFVersion = wxT("1.4");
  }
}

void
wxPdfDocument::SetTitle(const wxString& title)
{
  // Title of document
  m_title = title;
}

void
wxPdfDocument::SetSubject(const wxString& subject)
{
  // Subject of document
  m_subject = subject;
}

void
wxPdfDocument::SetAuthor(const wxString& author)
{
  // Author of document
  m_author = author;
}

void
wxPdfDocument::SetKeywords(const wxString& keywords)
{
  // Keywords of document
  m_keywords = keywords;
}

void
wxPdfDocument::SetCreator(const wxString& creator)
{
  // Creator of document
  m_creator = creator;
}

void
wxPdfDocument::SetMargins(double left, double top, double right)
{
  // Set left, top and right margins
  m_lMargin = left;
  m_tMargin = top;
  if (right == -1)
  {
    right = left;
  }
  m_rMargin = right;
}

void
wxPdfDocument::SetLeftMargin(double margin)
{
  // Set left margin
  m_lMargin = margin;
  if (m_page > 0 && m_x < margin)
  {
    m_x = margin;
  }
}

double
wxPdfDocument::GetLeftMargin()
{
  return m_lMargin;
}

void
wxPdfDocument::SetTopMargin(double margin)
{
  // Set top margin
  m_tMargin = margin;
}

double
wxPdfDocument::GetTopMargin()
{
  return m_tMargin;
}

void
wxPdfDocument::SetRightMargin(double margin)
{
  // Set right margin
  m_rMargin = margin;
}

double
wxPdfDocument::GetRightMargin()
{
  return m_rMargin;
}

void
wxPdfDocument::SetCellMargin(double margin)
{
  // Set cell margin
  m_cMargin = margin;
}

double
wxPdfDocument::GetCellMargin()
{
  return m_cMargin;
}

void
wxPdfDocument::SetLineHeight(double height)
{
  m_lasth = height;
}

double
wxPdfDocument::GetLineHeight()
{
  return m_lasth;
}

void
wxPdfDocument::SetAutoPageBreak(bool autoPageBreak, double margin)
{
  // Set auto page break mode and triggering margin
  m_autoPageBreak = autoPageBreak;
  m_bMargin = margin;
  m_pageBreakTrigger = (m_yAxisOriginTop) ? m_h - margin : margin;
}

void
wxPdfDocument::SetDisplayMode(wxPdfZoom zoom, wxPdfLayout layout, double zoomFactor)
{
  // Set display mode in viewer
  switch (zoom)
  {
    case wxPDF_ZOOM_FULLPAGE:
    case wxPDF_ZOOM_FULLWIDTH:
    case wxPDF_ZOOM_REAL:
    case wxPDF_ZOOM_DEFAULT:
      m_zoomMode = zoom;
      break;
    case wxPDF_ZOOM_FACTOR:
      m_zoomMode = zoom;
      m_zoomFactor = (zoomFactor > 0) ? zoomFactor : 100.;
      break;
    default:
      m_zoomMode = wxPDF_ZOOM_FULLWIDTH;
      break;
  }

  switch (layout)
  {
    case wxPDF_LAYOUT_SINGLE:
    case wxPDF_LAYOUT_TWO:
    case wxPDF_LAYOUT_DEFAULT:
    case wxPDF_LAYOUT_CONTINUOUS:
      m_layoutMode = layout;
      break;
    default:
      m_layoutMode = wxPDF_LAYOUT_CONTINUOUS;
      break;
  }
}

void
wxPdfDocument::Close()
{
  // Terminate document
  if (m_state == 3)
  {
    return;
  }
  if (m_page == 0)
  {
    AddPage();
  }
  
  // Page footer
  m_inFooter = true;
  Footer();
  m_inFooter = false;

  // Close page
  EndPage();

  // Close document
  EndDoc();
}

void
wxPdfDocument::Header()
{
  // To be implemented in your own inherited class
}

void
wxPdfDocument::Footer()
{
  // To be implemented in your own inherited class
}

bool
wxPdfDocument::IsInFooter()
{
  return m_inFooter;
}

int
wxPdfDocument::PageNo()
{
  // Get current page number
  return m_page;
}

double
wxPdfDocument::GetX()
{
  // Get x position
  return m_x;
}

void
wxPdfDocument::SetX(double x)
{
  // Set x position
  if ( x >= 0.0)
  {
    m_x = x;
  }
  else
  {
    m_x = m_w + x;
  }
}

double
wxPdfDocument::GetY()
{
  // Get y position
  return m_y;
}

void
wxPdfDocument::SetY(double y)
{
  // Set y position and reset x
  m_x = m_lMargin;
  if ( y >= 0)
  {
    m_y = y;
  }
  else
  {
    m_y = m_h + y;
  }
}

void
wxPdfDocument::SetXY(double x, double y)
{
  // Set x and y positions
  SetY(y);
  SetX(x);
}

void
wxPdfDocument::SetKerning(bool kerning)
{
  m_kerning = kerning;
}

void
wxPdfDocument::SetCompression(bool compress)
{
  m_compress = compress;
}

void
wxPdfDocument::AppendJavascript(const wxString& javascript)
{
  m_javascript += javascript;
}

bool
wxPdfDocument::AttachFile(const wxString& fileName, const wxString& attachName, const wxString& description)
{
  wxFileName attachFile(fileName);
  bool ok = attachFile.FileExists();
  if (ok)
  {
    wxArrayString* attachment = new wxArrayString();
    attachment->Add(fileName);
    if (!attachName.IsEmpty())
    {
      attachment->Add(attachName);
    }
    else
    {
      attachment->Add(attachFile.GetFullName());
    }
    attachment->Add(description);

    int index = (int) (m_attachments->size() + 1);
    (*m_attachments)[index] = attachment;
  }
  else
  {
    wxLogDebug(wxT("*** Attachment file '%s' does not exist."), fileName.c_str());
  }
  return ok;
}

// ---

void
wxPdfDocument::AddSpotColour(const wxString& name, double cyan, double magenta, double yellow, double black)
{
  wxPdfSpotColourMap::iterator spotColour = (*m_spotColours).find(name);
  if (spotColour == (*m_spotColours).end())
  {
    int i = (int) (*m_spotColours).size() + 1;
    (*m_spotColours)[name] = new wxPdfSpotColour(i, cyan, magenta, yellow, black);
  }
}
 
bool
wxPdfDocument::AddPattern(const wxString& patternName, const wxImage& image, double width, double height)
{
  bool isValid = true;
  wxPdfPatternMap::iterator patternIter = m_patterns->find(patternName);
  if (patternIter == m_patterns->end())
  {
    if (image.IsOk() && width > 0 && height > 0)
    {
      wxString imageName = wxString(wxT("pattern:")) + patternName;
      wxPdfImage* currentImage = NULL;
      wxPdfImageHashMap::iterator imageIter = (*m_images).find(imageName);
      if (imageIter == (*m_images).end())
      {
        // Prepare new image
        int maskImage = 0;
        wxImage tempImage = image.Copy();
        if (tempImage.HasAlpha())
        {
          maskImage = ImageMask(imageName+wxString(wxT(".mask")), tempImage);
          tempImage.ConvertAlphaToMask(0);
        }
        tempImage.SetMask(false);
        int i = (*m_images).size() + 1;
        currentImage = new wxPdfImage(this, i, imageName, tempImage);
        currentImage->Parse();
        if (maskImage > 0)
        {
          currentImage->SetMaskImage(maskImage);
        }
        (*m_images)[imageName] = currentImage;
      }
      else
      {
        // Use existing image
        currentImage = imageIter->second;
      }

      // Register new pattern
      wxPdfPattern* pattern;
      int i = (int) m_patterns->size() + 1;
      pattern = new wxPdfPattern(i, width, height);
      pattern->SetImage(currentImage);
      (*m_patterns)[patternName] = pattern;
    }
    else
    {
      isValid = false;
      if (!image.IsOk())
      {
        wxLogError(wxString(wxT("wxPdfDocument::AddPattern: ")) +
                   wxString(_("Invalid image.")));
      }
      else
      {
        wxLogError(wxString(wxT("wxPdfDocument::AddPattern: ")) +
                   wxString::Format(_("Invalid width (%.1f) and/or height (%.1f)."), width, height));
      }
    }
  }
  return isValid;
}
 
void
wxPdfDocument::SetDrawColour(const wxColour& colour)
{
  wxPdfColour tempColour(colour);
  m_drawColour = tempColour;
  if (m_page > 0)
  {
    OutAscii(m_drawColour.GetColour(true));
  }
}

void
wxPdfDocument::SetDrawColour(const unsigned char grayscale)
{
  wxPdfColour tempColour(grayscale);
  m_drawColour = tempColour;
  if (m_page > 0)
  {
    OutAscii(m_drawColour.GetColour(true));
  }
}

void
wxPdfDocument::SetDrawColour(const unsigned char red, const unsigned char green, const unsigned char blue)
{
  SetDrawColour(wxColour(red, green, blue));
}

void
wxPdfDocument::SetDrawColour(double cyan, double magenta, double yellow, double black)
{
  SetDrawColour(wxPdfColour(cyan, magenta, yellow, black));
}

void
wxPdfDocument::SetDrawColour(const wxPdfColour& colour)
{
  m_drawColour = colour;
  if (m_page > 0)
  {
    OutAscii(m_drawColour.GetColour(true));
  }
}

void
wxPdfDocument::SetDrawColour(const wxString& name, double tint)
{
  wxPdfSpotColourMap::iterator spotColour = (*m_spotColours).find(name);
  if (spotColour != (*m_spotColours).end())
  {
    wxPdfColour tempColour(*(spotColour->second), tint);
    m_drawColour = tempColour;
    if (m_page > 0)
    {
      OutAscii(m_drawColour.GetColour(true));
    }
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfDocument::SetDrawColour: ")) +
               wxString::Format(_("Undefined spot colour: '%s'."), name.c_str()));
  }
}

void
wxPdfDocument::SetDrawPattern(const wxString& name)
{
  wxPdfPatternMap::iterator pattern = m_patterns->find(name);
  if (pattern != m_patterns->end())
  {
    wxPdfColour tempColour(*(pattern->second));
    m_drawColour = tempColour;
    if (m_page > 0)
    {
      OutAscii(m_drawColour.GetColour(true));
    }
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfDocument::SetDrawPattern: ")) +
               wxString::Format(_("Undefined pattern: '%s'."), name.c_str()));
  }
}

const wxPdfColour
wxPdfDocument::GetDrawColour()
{
  return wxPdfColour(m_drawColour);
}

void
wxPdfDocument::SetFillColour(const wxColour& colour)
{
  wxPdfColour tempColour(colour);
  m_fillColour = tempColour;
  m_colourFlag = (m_fillColour != m_textColour);
  if (m_page > 0)
  {
    OutAscii(m_fillColour.GetColour(false));
  }
}

void
wxPdfDocument::SetFillColour(const unsigned char grayscale)
{
  wxPdfColour tempColour(grayscale);
  m_fillColour = tempColour;
  m_colourFlag = (m_fillColour != m_textColour);
  if (m_page > 0)
  {
    OutAscii(m_fillColour.GetColour(false));
  }
}

void
wxPdfDocument::SetFillColour(const wxPdfColour& colour)
{
  m_fillColour = colour;
  m_colourFlag = (m_fillColour != m_textColour);
  if (m_page > 0)
  {
    OutAscii(m_fillColour.GetColour(false));
  }
}

void
wxPdfDocument::SetFillColour(const unsigned char red, const unsigned char green, const unsigned char blue)
{
  SetFillColour(wxColour(red, green, blue));
}

void
wxPdfDocument::SetFillColour(double cyan, double magenta, double yellow, double black)
{
  SetFillColour(wxPdfColour(cyan, magenta, yellow, black));
}

void
wxPdfDocument::SetFillColour(const wxString& name, double tint)
{
  wxPdfSpotColourMap::iterator spotColour = (*m_spotColours).find(name);
  if (spotColour != (*m_spotColours).end())
  {
    wxPdfColour tempColour(*(spotColour->second), tint);
    m_fillColour = tempColour;
    m_colourFlag = (m_fillColour != m_textColour);
    if (m_page > 0)
    {
      OutAscii(m_fillColour.GetColour(false));
    }
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfDocument::SetFillColour: ")) +
               wxString::Format(_("Undefined spot colour: '%s'."), name.c_str()));
  }
}

void
wxPdfDocument::SetFillPattern(const wxString& name)
{
  wxPdfPatternMap::iterator pattern = m_patterns->find(name);
  if (pattern != m_patterns->end())
  {
    wxPdfColour tempColour(*(pattern->second));
    m_fillColour = tempColour;
    m_colourFlag = (m_fillColour != m_textColour);
    if (m_page > 0)
    {
      OutAscii(m_fillColour.GetColour(false));
    }
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfDocument::SetFillPattern: ")) +
               wxString::Format(_("Undefined pattern: '%s'."), name.c_str()));
  }
}

const wxPdfColour
wxPdfDocument::GetPatternColour(const wxString& name)
{
  wxPdfColour colour(0);
  wxPdfPatternMap::iterator pattern = m_patterns->find(name);
  if (pattern != m_patterns->end())
  {
    wxPdfColour tempColour(*(pattern->second));
    colour = tempColour;
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfDocument::GetPatternColour: ")) +
               wxString::Format(_("Undefined pattern: '%s'."), name.c_str()));
  }
  return colour;
}

const wxPdfColour
wxPdfDocument::GetFillColour()
{
  return wxPdfColour(m_fillColour);
}

void
wxPdfDocument::SetTextColour(const wxColour& colour)
{
  wxPdfColour tempColour(colour);
  m_textColour = tempColour;
  m_colourFlag = (m_fillColour != m_textColour);
}

void
wxPdfDocument::SetTextColour(const unsigned char grayscale)
{
  wxPdfColour tempColour(grayscale);
  m_textColour = tempColour;
  m_colourFlag = (m_fillColour != m_textColour);
}

void
wxPdfDocument::SetTextColour(const wxPdfColour& colour)
{
  m_textColour = colour;
  m_colourFlag = (m_fillColour != m_textColour);
}

void
wxPdfDocument::SetTextColour(const unsigned char red, const unsigned char green, const unsigned char blue)
{
  SetTextColour(wxColour(red, green, blue));
}

void
wxPdfDocument::SetTextColour(double cyan, double magenta, double yellow, double black)
{
  SetTextColour(wxPdfColour(cyan, magenta, yellow, black));
}

void
wxPdfDocument::SetTextColour(const wxString& name, double tint)
{
  wxPdfSpotColourMap::iterator spotColour = (*m_spotColours).find(name);
  if (spotColour != (*m_spotColours).end())
  {
    wxPdfColour tempColour(*(spotColour->second), tint);
    m_textColour = tempColour;
    m_colourFlag = (m_fillColour != m_textColour);
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfDocument::SetTextColour: ")) +
               wxString::Format(_("Undefined spot colour: '%s'."), name.c_str()));
  }
}

void
wxPdfDocument::SetTextPattern(const wxString& name)
{
  wxPdfPatternMap::iterator pattern = m_patterns->find(name);
  if (pattern != m_patterns->end())
  {
    wxPdfColour tempColour(*(pattern->second));
    m_textColour = tempColour;
    m_colourFlag = (m_fillColour != m_textColour);
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfDocument::SetFillPattern: ")) +
               wxString::Format(_("Undefined pattern: '%s'."), name.c_str()));
  }
}

const wxPdfColour
wxPdfDocument::GetTextColour()
{
  return wxPdfColour(m_textColour);
}

void
wxPdfDocument::SetTextRenderMode(wxPdfTextRenderMode mode)
{
  m_textRenderMode = mode;
}
  
wxPdfTextRenderMode
wxPdfDocument::GetTextRenderMode() const
{
  return m_textRenderMode;
}

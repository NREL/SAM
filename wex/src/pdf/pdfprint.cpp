/////////////////////////////////////////////////////////////////////////////
// Name:        wx/pdfprint.h
// Purpose:     wxPdfPrinter, wxPdfPrintNativeData
// Author:      Mark Dootson
// Modified by:
// Created:     11 May 2012
// SVN-ID:      $Id$
// Copyright:   (c) 2012 Ulrich Telle
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

// For compilers that support precompilation, includes <wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

#include <wx/utils.h>
#include <wx/dc.h>
#include <wx/app.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/log.h>

#include <wx/printdlg.h>
#include <wx/generic/prntdlgg.h>
#include <wx/progdlg.h>
#include <wx/paper.h>

#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/textctrl.h>
#include <wx/radiobox.h>
#include <wx/filedlg.h>
#include <wx/combobox.h>
#include <wx/sizer.h>
#include <wx/cmndata.h>

#include <wx/prntbase.h>
#include <wx/printdlg.h>
#include <wx/paper.h>
#include <wx/filename.h>
#include <wx/mimetype.h>
#include <wx/tokenzr.h>
#include <wx/imaglist.h>

#include <stdlib.h>
#include <string.h>

#include "wex/pdf/pdfdoc.h"
#include "wex/pdf/pdfdc.h"
#include "wex/pdf/pdfprint.h"
#include <wx/intl.h>

#if !wxCHECK_VERSION(2, 9, 0)
    #define wxPENSTYLE_USER_DASH wxUSER_DASH
#endif

// ----------------------------------------------------------------------------
// PdfPrint data
// ----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxPdfPrintData, wxObject)

wxPdfPrintData::wxPdfPrintData()
{    
  Init();    
}

wxPdfPrintData::wxPdfPrintData(wxPdfPrintData* pdfPrintData)
{
  m_documentTitle     = pdfPrintData->m_documentTitle;
  m_documentSubject   = pdfPrintData->m_documentSubject;
  m_documentAuthor    = pdfPrintData->m_documentAuthor;
  m_documentKeywords  = pdfPrintData->m_documentKeywords;
  m_documentCreator   = pdfPrintData->m_documentCreator;
  m_protectionEnabled = pdfPrintData->m_protectionEnabled;
  m_userPassword      = pdfPrintData->m_userPassword;
  m_ownerPassword     = pdfPrintData->m_ownerPassword;
  m_permissions       = pdfPrintData->m_permissions;
  m_encryptionMethod  = pdfPrintData->m_encryptionMethod;
  m_keyLength         = pdfPrintData->m_keyLength;

  m_printOrientation  = pdfPrintData->m_printOrientation;
  m_printQuality      = pdfPrintData->m_printQuality;
  m_paperId           = pdfPrintData->m_paperId;
  m_filename          = pdfPrintData->m_filename;

  m_printFromPage     = pdfPrintData->m_printFromPage;
  m_printToPage       = pdfPrintData->m_printToPage;
  m_printMinPage      = pdfPrintData->m_printMinPage;
  m_printMaxPage      = pdfPrintData->m_printMaxPage;

  m_printDialogFlags  = pdfPrintData->m_printDialogFlags;
  m_launchViewer      = pdfPrintData->m_launchViewer;
  m_templateDocument  = pdfPrintData->m_templateDocument;
  m_templateWidth     = pdfPrintData->m_templateWidth;
  m_templateHeight    = pdfPrintData->m_templateHeight;
  m_templateMode      = pdfPrintData->m_templateMode;
}

wxPdfPrintData::wxPdfPrintData(wxPrintData* printData)
{
  Init();
  m_printOrientation = printData->GetOrientation();
  m_paperId          = printData->GetPaperId();
  if (!printData->GetFilename().IsEmpty())
  {
    m_filename       = printData->GetFilename();
  }
  m_printQuality     = printData->GetQuality();
}

wxPdfPrintData::wxPdfPrintData(wxPrintDialogData* printDialogData)
{
  Init();

  wxPrintData printData = printDialogData->GetPrintData();

  if (printData.IsOk())
  {
    m_printOrientation = printData.GetOrientation();
    m_paperId          = printData.GetPaperId();
    if (!printData.GetFilename().IsEmpty())
    {
      m_filename       = printData.GetFilename();
    }
    m_printQuality     = printData.GetQuality();
  }

  m_printFromPage = printDialogData->GetFromPage();
  m_printToPage   = printDialogData->GetToPage();
  m_printMinPage  = printDialogData->GetMinPage();
  m_printMaxPage  = printDialogData->GetMaxPage();
}

wxPdfPrintData::wxPdfPrintData(wxPageSetupDialogData* pageSetupDialogData)
{
  Init();

  wxPrintData printData = pageSetupDialogData->GetPrintData();

  if( printData.IsOk() )
  {
    m_printOrientation = printData.GetOrientation();
    m_paperId          = printData.GetPaperId();
    if (!printData.GetFilename().IsEmpty())
    {
      m_filename       = printData.GetFilename();
    }
    m_printQuality     = printData.GetQuality();
  }
}

void
wxPdfPrintData::Init()
{
  m_documentTitle     = wxT("PDF Document");
  m_documentSubject   = wxEmptyString;
  m_documentAuthor    = wxEmptyString;
  m_documentKeywords  = wxEmptyString;
  m_documentCreator   = wxT("wxPdfDC");
  m_protectionEnabled = false;
  m_userPassword      = wxEmptyString;
  m_ownerPassword     = wxEmptyString;
  m_permissions       = wxPDF_PERMISSION_NONE;
  m_encryptionMethod  = wxPDF_ENCRYPTION_RC4V1;
  m_keyLength         = 40;

  m_printOrientation  = wxPORTRAIT;
  m_paperId           = wxPAPER_A4;
  m_filename          = wxT("default.pdf");
  m_printQuality      = wxPDF_PRINTER_DEFAULT_RESOLUTION;

  m_printFromPage = 1;
  m_printToPage  = 9999;
  m_printMinPage = 1;
  m_printMaxPage = 9999;

  m_printDialogFlags  = wxPDF_PRINTDIALOG_ALLOWALL;
  m_launchViewer      = false;

  m_templateDocument  = NULL;
  m_templateWidth     = 0.0;
  m_templateHeight    = 0.0;
  m_templateMode      = false;
}

void
wxPdfPrintData::SetDocumentProtection(int permissions, const wxString& userPassword,
                                      const wxString& ownerPassword, wxPdfEncryptionMethod encryptionMethod,
                                      int keyLength)
{
  m_permissions       = permissions;
  m_userPassword      = userPassword;
  m_ownerPassword     = ownerPassword;
  m_encryptionMethod  = encryptionMethod;
  m_keyLength         = keyLength;
  m_protectionEnabled = true;
}

void
wxPdfPrintData::ClearDocumentProtection()
{
  SetDocumentProtection(0);
  m_protectionEnabled = false;
}

void
wxPdfPrintData::UpdateDocument(wxPdfDocument* pdfDoc)
{
  if(!m_templateMode)
  {
    pdfDoc->SetTitle(m_documentTitle);
    pdfDoc->SetAuthor(m_documentAuthor);
    pdfDoc->SetSubject(m_documentSubject);
    pdfDoc->SetCreator(m_documentCreator);
    pdfDoc->SetKeywords(m_documentKeywords);
    if(m_protectionEnabled)
    {
      pdfDoc->SetProtection(m_permissions, m_userPassword, m_ownerPassword, m_encryptionMethod , m_keyLength);
    }
  }
}

wxPrintData*
wxPdfPrintData::CreatePrintData() const
{
  wxPrintData* printData = new wxPrintData();
  printData->SetQuality(GetQuality());
  printData->SetOrientation(GetOrientation());
  printData->SetPaperId(GetPaperId());
  printData->SetFilename(GetFilename());
  return printData;
}

int
wxPdfPrintData::GetPrintResolution() const
{
  int resolution;
  // Make resolutions appear to be what user would expect
  switch ( m_printQuality )
  {
    case wxPRINT_QUALITY_HIGH:
      resolution = 1200;
      break;

    case wxPRINT_QUALITY_MEDIUM:
      resolution = 600;
      break;

    case wxPRINT_QUALITY_LOW:
      resolution = 300;
      break;

    case wxPRINT_QUALITY_DRAFT:
      resolution = 150;
      break;

    default:
      if ( m_printQuality >= 72 )
      {
        resolution = m_printQuality;
      }
      else
      {
        resolution = wxPDF_PRINTER_DEFAULT_RESOLUTION;
      }
      break;
  }
  return resolution;
}

void
wxPdfPrintData::SetTemplate(wxPdfDocument* pdfDocument, double templateWidth, double templateHeight)
{
  m_templateDocument = pdfDocument;
  m_templateWidth    = templateWidth;
  m_templateHeight   = templateHeight;
  m_templateMode     = true;
}


// ----------------------------------------------------------------------------
// wxPdfPrinter
// ----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxPdfPrinter, wxPrinterBase)

wxPdfPrinter::wxPdfPrinter()
{
  m_currentPrintout = NULL;
  sm_abortWindow = NULL;
  sm_abortIt = false;
  sm_lastError = wxPRINTER_NO_ERROR;
}

wxPdfPrinter::wxPdfPrinter(wxPrintDialogData* printDialogData)
{
  m_currentPrintout = NULL;
  sm_abortWindow = NULL;
  sm_abortIt = false;
  m_printDialogData = (*printDialogData);
  m_pdfPrintData = wxPdfPrintData(printDialogData);
  sm_lastError = wxPRINTER_NO_ERROR;
}

wxPdfPrinter::wxPdfPrinter(wxPdfPrintData* printData)
{
  m_currentPrintout = NULL;
  sm_abortWindow = NULL;
  sm_abortIt = false;
  m_pdfPrintData = (*printData);
  sm_lastError = wxPRINTER_NO_ERROR;
}

wxPdfPrinter::wxPdfPrinter(wxPrintData* data)
{
  m_currentPrintout = NULL;
  sm_abortWindow = NULL;
  sm_abortIt = false;
  m_pdfPrintData = wxPdfPrintData( data );;
  m_printDialogData.SetPrintData(*data);
  sm_lastError = wxPRINTER_NO_ERROR;
}

bool wxPdfPrinter::Print(wxWindow* parent, wxPrintout* printout, bool prompt)
{
  wxProgressDialog* progressDialog = NULL;

  sm_abortIt = false;
  sm_abortWindow = NULL;

  if (!printout)
  {
    sm_lastError = wxPRINTER_ERROR;
    return false;
  }

  wxPdfDC* dc;

  if( m_pdfPrintData.GetTemplateMode() )
  {
    dc = new wxPdfDC(m_pdfPrintData.GetTemplateDocument(),
                     m_pdfPrintData.GetTemplateWidth(),
                     m_pdfPrintData.GetTemplateHeight());
  }
  else if (prompt)
  {
    dc = (wxPdfDC*) PrintDialog(parent);
    if (!dc)
    {
      return false;
    }
  }
  else
  {
    wxPrintData* dcPrintData = m_pdfPrintData.CreatePrintData();
    dc = new wxPdfDC(*dcPrintData);
    delete dcPrintData;
  }

  if (m_pdfPrintData.GetMinPage() < 1)
  {
    m_pdfPrintData.SetMinPage(1);
  }
  if (m_pdfPrintData.GetMaxPage() < 1)
  {
    m_pdfPrintData.SetMaxPage(9999);
  }

  // May have pressed cancel.
  if (!dc || !dc->IsOk())
  {
    if (dc) delete dc;
    sm_lastError = wxPRINTER_ERROR;
    return false;
  }

  // Get the printdata used and check for extra info
  // update the document if it is present
  // we need a startdoc to get a valid dc

  dc->StartDoc(wxT("PDF Document"));
  m_pdfPrintData.UpdateDocument( dc->GetPdfDocument() );

  // ---------------------------------------------------------------
  // determine and set our printout params
  // ---------------------------------------------------------------
  int screenppiX, screenppiY, deviceW, deviceH, devicemmX, devicemmY;
  GetPdfScreenPPI(&screenppiX, &screenppiY);

  int resolution = m_pdfPrintData.GetPrintResolution();    
  dc->SetResolution(resolution); 
  dc->GetSize( &deviceW, &deviceH );
  dc->GetSizeMM(&devicemmX, &devicemmY);

  printout->SetPPIScreen(screenppiX, screenppiY);
  printout->SetPPIPrinter(resolution,  resolution);
  printout->SetPageSizePixels(deviceW, deviceH);
  printout->SetPaperRectPixels(wxRect(0, 0, deviceW, deviceH));
  printout->SetPageSizeMM(devicemmX, devicemmY);
  printout->SetDC(dc);

  // ---------------------------------------------------------------
  // determine which pages to print
  // ---------------------------------------------------------------
  wxBeginBusyCursor();

  // the printout will probably do all its sizing calcs on this call
  printout->OnPreparePrinting();

  int fromPage, toPage, minPage, maxPage;
  printout->GetPageInfo(&minPage, &maxPage, &fromPage, &toPage);

  if (maxPage == 0)
  {
    sm_lastError = wxPRINTER_ERROR;
    wxEndBusyCursor();
    return false;
  }

  // merge info from printout with user / programmer choices in the dialog info
  m_printDialogData.SetMinPage(minPage);
  m_printDialogData.SetMaxPage(maxPage);
  if ((m_printDialogData.GetFromPage() < minPage ) ||  (m_printDialogData.GetFromPage() < 1 ))
  {
    m_printDialogData.SetFromPage(minPage);
  }
  if ((m_printDialogData.GetToPage() > maxPage ) || ( m_printDialogData.GetToPage() < 1 ))
  {
    m_printDialogData.SetToPage(maxPage);
  }

  int numberofpages = m_printDialogData.GetToPage() - m_printDialogData.GetFromPage() +1;
  int printedPages = 0;

  // ---------------------------------------------------------------
  // do the actual printing
  // ---------------------------------------------------------------
  if (m_showProgressDialog)
  {
    progressDialog = 
      new wxProgressDialog(printout->GetTitle(),
                           _("Printing..."),
                           numberofpages,
                           parent,
                           wxPD_AUTO_HIDE|wxPD_APP_MODAL);
  }

  printout->OnBeginPrinting();

  sm_lastError = wxPRINTER_NO_ERROR;

  if (printout->OnBeginDocument(m_printDialogData.GetFromPage(), m_printDialogData.GetToPage()))
  {
    // print our range of chosen pages or until HasPage returns false
    int currentpage;
    for (currentpage  = m_printDialogData.GetFromPage();
         (currentpage <= m_printDialogData.GetToPage()) && printout->HasPage( currentpage );
         currentpage++)
    {
      if (m_showProgressDialog)
      {
        wxString msg;
        msg.Printf(_("Printing page %d..."), printedPages + 1);
        progressDialog->Update(printedPages++, msg);
      }
      dc->StartPage();
      printout->OnPrintPage(currentpage);
      dc->EndPage();
      wxYield();
    }
  }
  else
  {
    // OnBeginDocument returned false
    wxEndBusyCursor();
    wxLogError(_("Could not start printing."));
    sm_lastError = wxPRINTER_ERROR;
  }

  printout->OnEndDocument();
  printout->OnEndPrinting();

  // ---------------------------------------------------------------
  // cleanup
  // ---------------------------------------------------------------
  if (m_showProgressDialog)
  {
    delete progressDialog;
  }

  wxEndBusyCursor();
  delete dc;

  // ---------------------------------------------------------------
  // Launch Viewer
  // ---------------------------------------------------------------

  if (m_pdfPrintData.GetLaunchDocumentViewer() && !m_pdfPrintData.GetTemplateMode())
  {
    wxFileName fileName = wxFileName(m_pdfPrintData.GetFilename());  
    wxFileType* fileType = wxTheMimeTypesManager->GetFileTypeFromExtension(wxT("pdf"));
    if (fileType != NULL)
    {
      wxString cmd = fileType->GetOpenCommand(fileName.GetFullPath());
      if (!cmd.IsEmpty())
      {
        wxExecute(cmd);
      }
      delete fileType;
    }
    else
    {
      // fallback
      wxString fileURL;
      if (wxIsAbsolutePath(m_pdfPrintData.GetFilename()))
      {
        fileURL = wxT("file://") + m_pdfPrintData.GetFilename();
      }
      else
      {
        fileURL = wxT("file://") + wxGetCwd() + wxFILE_SEP_PATH + m_pdfPrintData.GetFilename();
      }
      wxLaunchDefaultBrowser(fileURL);
    }
  }

  return (sm_lastError == wxPRINTER_NO_ERROR);
}

wxDC*
wxPdfPrinter::PrintDialog(wxWindow* parent)
{
  wxPdfDC* dc = NULL;

  wxPdfPrintDialog dialog( parent, &m_pdfPrintData );
  if (dialog.ShowModal() == wxID_OK)
  {
    dc = (wxPdfDC*) dialog.GetPrintDC();
    m_pdfPrintData = dialog.GetPdfPrintData();

    sm_lastError = (dc == NULL) ? wxPRINTER_ERROR : wxPRINTER_NO_ERROR;
  }
  else
  {
    sm_lastError = wxPRINTER_CANCELLED;
  }

  return dc;
}

bool
wxPdfPrinter::Setup(wxWindow* WXUNUSED(parent))
{
  return false;
}

void
wxPdfPrinter::GetPdfScreenPPI(int* x, int* y)
{
  wxScreenDC dc;

  if (x)
  {
    *x = dc.GetPPI().GetWidth();
  }

  if (y)
  {
    *y = dc.GetPPI().GetHeight();
  }
}

// ----------------------------------------------------------------------------
// wxPdfPrintPreview
// ----------------------------------------------------------------------------

IMPLEMENT_CLASS(wxPdfPrintPreview, wxPrintPreview)

wxPdfPrintPreview::wxPdfPrintPreview(wxPrintout* printout,
                                     wxPrintout* printoutForPrinting)
  : wxPrintPreviewBase(printout, printoutForPrinting)
{
  m_pimpl = new wxPdfPrintPreviewImpl(printout, printoutForPrinting);
}

wxPdfPrintPreview::wxPdfPrintPreview(wxPrintout* printout,
                                     wxPrintout* printoutForPrinting,
                                     wxPrintData* data)
  : wxPrintPreviewBase(printout, printoutForPrinting, data)
{
  m_pimpl = new wxPdfPrintPreviewImpl(printout, printoutForPrinting, data);
}

wxPdfPrintPreview::wxPdfPrintPreview(wxPrintout* printout,
                                     wxPrintout* printoutForPrinting,
                                     wxPrintDialogData* data)
  : wxPrintPreviewBase(printout, printoutForPrinting, data)
{
  m_pimpl = new wxPdfPrintPreviewImpl(printout, printoutForPrinting, data);
}

wxPdfPrintPreview::wxPdfPrintPreview(wxPrintout* printout,
                                     wxPrintout* printoutForPrinting,
                                     wxPdfPrintData* pdfPrintData)
  : wxPrintPreviewBase(printout, printoutForPrinting)
{
  m_pimpl = new wxPdfPrintPreviewImpl(printout, printoutForPrinting, pdfPrintData);
}

wxPdfPrintPreview::~wxPdfPrintPreview()
{
  delete m_pimpl;

  // don't delete twice
  m_printPrintout = NULL;
  m_previewPrintout = NULL;
  m_previewBitmap = NULL;
}

bool
wxPdfPrintPreview::SetCurrentPage(int pageNum)
{
  return m_pimpl->SetCurrentPage(pageNum);
}

int
wxPdfPrintPreview::GetCurrentPage() const
{
  return m_pimpl->GetCurrentPage();
}

void
wxPdfPrintPreview::SetPrintout(wxPrintout* printout)
{
  m_pimpl->SetPrintout(printout);
}

wxPrintout*
wxPdfPrintPreview::GetPrintout() const
{
  return m_pimpl->GetPrintout();
}

wxPrintout*
wxPdfPrintPreview::GetPrintoutForPrinting() const
{
  return m_pimpl->GetPrintoutForPrinting();
}

void
wxPdfPrintPreview::SetFrame(wxFrame* frame)
{
  m_pimpl->SetFrame(frame);
}

void
wxPdfPrintPreview::SetCanvas(wxPreviewCanvas* canvas)
{
  m_pimpl->SetCanvas(canvas);
}

wxFrame*
wxPdfPrintPreview::GetFrame() const
{
  return m_pimpl->GetFrame();
}

wxPreviewCanvas*
wxPdfPrintPreview::GetCanvas() const
{
  return m_pimpl->GetCanvas();
}

bool
wxPdfPrintPreview::PaintPage(wxPreviewCanvas* canvas, wxDC& dc)
{
  return m_pimpl->PaintPage(canvas, dc);
}

#if wxCHECK_VERSION(2,9,0)
bool
wxPdfPrintPreview::UpdatePageRendering()
{
  return m_pimpl->UpdatePageRendering();
}
#endif

bool
wxPdfPrintPreview::DrawBlankPage(wxPreviewCanvas* canvas, wxDC& dc)
{
  return m_pimpl->DrawBlankPage(canvas, dc);
}

void
wxPdfPrintPreview::AdjustScrollbars(wxPreviewCanvas* canvas)
{
  m_pimpl->AdjustScrollbars(canvas);
}

bool
wxPdfPrintPreview::RenderPage(int pageNum)
{
  return m_pimpl->RenderPage(pageNum);
}

void
wxPdfPrintPreview::SetZoom(int percent)
{
  m_pimpl->SetZoom(percent);
}

int
wxPdfPrintPreview::GetZoom() const
{
  return m_pimpl->GetZoom();
}

wxPrintDialogData&
wxPdfPrintPreview::GetPrintDialogData()
{
  return m_pimpl->GetPrintDialogData();
}

int
wxPdfPrintPreview::GetMaxPage() const
{
  return m_pimpl->GetMaxPage();
}

int
wxPdfPrintPreview::GetMinPage() const
{
  return m_pimpl->GetMinPage();
}

bool
wxPdfPrintPreview::IsOk() const
{
  return m_pimpl->IsOk();
}

void
wxPdfPrintPreview::SetOk(bool ok)
{
  m_pimpl->SetOk( ok );
}

bool
wxPdfPrintPreview::Print(bool interactive)
{
  return m_pimpl->Print(interactive);
}

void
wxPdfPrintPreview::DetermineScaling()
{
  m_pimpl->DetermineScaling();
}


// ----------------------------------------------------------------------------
// wxPdfPrintPreview Implementation
// ----------------------------------------------------------------------------


IMPLEMENT_CLASS(wxPdfPrintPreviewImpl, wxPrintPreviewBase)

wxPdfPrintPreviewImpl::wxPdfPrintPreviewImpl(wxPrintout* printout,
                                             wxPrintout* printoutForPrinting)
  : wxPrintPreviewBase(printout, printoutForPrinting)
{
    m_pdfPrintData  = new wxPdfPrintData();
    m_pdfPreviewDC  = NULL;
    m_pdfPreviewDoc = NULL;
    DetermineScaling();
}

wxPdfPrintPreviewImpl::wxPdfPrintPreviewImpl(wxPrintout* printout,
                                             wxPrintout* printoutForPrinting,
                                             wxPrintData* data)
  : wxPrintPreviewBase(printout, printoutForPrinting, data)
{
  m_pdfPrintData  = new wxPdfPrintData(data);
  m_pdfPreviewDC  = NULL;
  m_pdfPreviewDoc = NULL;
  DetermineScaling();
}

wxPdfPrintPreviewImpl::wxPdfPrintPreviewImpl(wxPrintout* printout,
                                             wxPrintout* printoutForPrinting,
                                             wxPrintDialogData* data)
  : wxPrintPreviewBase(printout, printoutForPrinting, data)
{
  m_pdfPrintData  = new wxPdfPrintData(data);
  m_pdfPreviewDC  = NULL;
  m_pdfPreviewDoc = NULL;
  DetermineScaling();
}

wxPdfPrintPreviewImpl::wxPdfPrintPreviewImpl(wxPrintout* printout,
                                             wxPrintout* printoutForPrinting,
                                             wxPdfPrintData* pdfPrintData)
  : wxPrintPreviewBase(printout, printoutForPrinting)
{
  m_pdfPrintData = new wxPdfPrintData( pdfPrintData );
  m_pdfPreviewDC  = NULL;
  m_pdfPreviewDoc = NULL;
  DetermineScaling();
}

wxPdfPrintPreviewImpl::~wxPdfPrintPreviewImpl()
{
  if (m_pdfPreviewDC)
  {
    delete m_pdfPreviewDC;
  }

  if (m_pdfPreviewDoc)
  {
    delete m_pdfPreviewDoc;
  }

  if (m_pdfPrintData)
  {
    delete m_pdfPrintData;
  }
}

bool
wxPdfPrintPreviewImpl::Print(bool interactive)
{
  if (!m_printPrintout)
  {
    return false;
  }

  wxPdfPrinter printer(m_pdfPrintData);
  return printer.Print(m_previewFrame, m_printPrintout, interactive);
}

void
wxPdfPrintPreviewImpl::GetPdfScreenPPI(int* x, int* y)
{
  wxScreenDC dc;

  if (x)
  {
    *x = dc.GetPPI().GetWidth();
  }

  if (y)
  {
    *y = dc.GetPPI().GetHeight();
  }
}

void
wxPdfPrintPreviewImpl::DetermineScaling()
{
  int screenppiX, screenppiY, deviceW, deviceH, devicemmX, devicemmY;
  GetPdfScreenPPI(&screenppiX, &screenppiY);
  int resolution = m_pdfPrintData->GetPrintResolution();
  if (!m_pdfPreviewDC)
  {
    // In template mode get a document with the same metrics
    // as the template for the preview dc
    if (m_pdfPrintData->GetTemplateMode())
    {
      wxString scaleMode;
      switch ((int) m_pdfPrintData->GetTemplateDocument()->GetScaleFactor())
      {
        case 1:
          scaleMode = wxT("pt");
          break;
        case 72:
          scaleMode = wxT("in");
          break;
        case 28:
          scaleMode = wxT("cm");
          break;
        default:
          scaleMode = wxT("mm");
          break;
      }

      m_pdfPreviewDoc = new wxPdfDocument(wxPORTRAIT, m_pdfPrintData->GetTemplateWidth(),
                                          m_pdfPrintData->GetTemplateHeight(), scaleMode);

      m_pdfPreviewDC = new wxPdfDC(m_pdfPreviewDoc,
                                   m_pdfPrintData->GetTemplateWidth(),
                                   m_pdfPrintData->GetTemplateHeight());
    }
    else
    {
      wxPrintData* pdata = m_pdfPrintData->CreatePrintData();
      m_pdfPreviewDC = new wxPdfDC(*pdata);
      m_pdfPreviewDC->StartDoc(wxT("unused name"));
      delete pdata;
    }
  }

  m_pdfPreviewDC->SetResolution(resolution);
  m_pdfPreviewDC->GetSize(&deviceW, &deviceH);
  m_pdfPreviewDC->GetSizeMM(&devicemmX, &devicemmY);

  m_previewPrintout->SetPPIScreen(screenppiX, screenppiY);
  m_previewPrintout->SetPPIPrinter(resolution, resolution);
  m_previewPrintout->SetPageSizePixels(deviceW, deviceH);
  m_previewPrintout->SetPaperRectPixels(wxRect(0, 0, deviceW, deviceH));
  m_previewPrintout->SetPageSizeMM(devicemmX, devicemmY);

  m_pageWidth  = deviceW;
  m_pageHeight = deviceH;

  m_previewScaleX = (double) screenppiX / resolution;
  m_previewScaleY = (double) screenppiY / resolution;

  // override some base values
  m_currentZoom = 100;
}

#if wxCHECK_VERSION(2,9,0)
// Version 2.9.x

bool
wxPdfPrintPreviewImpl::RenderPageIntoBitmap(wxBitmap& bmp, int pageNum)
{
  wxMemoryDC memoryDC;
  memoryDC.SelectObject(bmp);
  memoryDC.Clear();
  wxPdfPreviewDC previewDC(memoryDC, m_pdfPreviewDC);

  return RenderPageIntoDC(previewDC, pageNum);
}

#else
// Version 2.8.x

bool
wxPdfPrintPreviewImpl::RenderPageIntoDCImpl(wxDC& dc, int pageNum)
{
  m_previewPrintout->SetDC(&dc);
  m_previewPrintout->SetPageSizePixels(m_pageWidth, m_pageHeight);

  // Need to delay OnPreparePrinting() until here, so we have enough
  // information.
  if (!m_printingPrepared)
  {
    m_previewPrintout->OnPreparePrinting();
    int selFrom, selTo;
    m_previewPrintout->GetPageInfo(&m_minPage, &m_maxPage, &selFrom, &selTo);
    m_printingPrepared = true;
  }

  m_previewPrintout->OnBeginPrinting();

  if (!m_previewPrintout->OnBeginDocument(m_printDialogData.GetFromPage(), m_printDialogData.GetToPage()))
  {
    wxMessageBox(wxT("Could not start document preview."), wxT("Print Preview Failure"), wxOK);
    return false;
  }

  m_previewPrintout->OnPrintPage(pageNum);
  m_previewPrintout->OnEndDocument();
  m_previewPrintout->OnEndPrinting();

  m_previewPrintout->SetDC(NULL);

  return true;
}

bool
wxPdfPrintPreviewImpl::RenderPageIntoBitmapImpl(wxBitmap& bmp, int pageNum)
{
  wxMemoryDC memoryDC;
  memoryDC.SelectObject(bmp);
  memoryDC.Clear();
  wxPdfPreviewDC previewDC(memoryDC, m_pdfPreviewDC);
  return RenderPageIntoDCImpl(previewDC, pageNum);
}

bool
wxPdfPrintPreviewImpl::RenderPage(int pageNum)
{
  wxBusyCursor busy;

  if (!m_previewCanvas)
  {
    wxFAIL_MSG(wxT("wxPrintPreviewBase::RenderPage: must use wxPrintPreviewBase::SetCanvas to let me know about the canvas!"));
    return false;
  }

  wxRect pageRect, paperRect;
  CalcRects(m_previewCanvas, pageRect, paperRect);

  if (!m_previewBitmap)
  {
    m_previewBitmap = new wxBitmap(pageRect.width, pageRect.height);

    if (!m_previewBitmap || !m_previewBitmap->Ok())
    {
      if (m_previewBitmap)
      {
        delete m_previewBitmap;
        m_previewBitmap = NULL;
      }
      wxMessageBox(wxT("Sorry, not enough memory to create a preview."), wxT("Print Preview Failure"), wxOK);
      return false;
    }
  }

  if (!RenderPageIntoBitmapImpl(*m_previewBitmap, pageNum))
  {
    wxMessageBox(wxT("Could not start document preview."), wxT("Print Preview Failure"), wxOK);

    delete m_previewBitmap;
    m_previewBitmap = NULL;
    return false;
  }

#if wxUSE_STATUSBAR
  wxString status;
  if (m_maxPage != 0)
  {
    status = wxString::Format(wxT("Page %d of %d"), pageNum, m_maxPage);
  }
  else
  {
    status = wxString::Format(wxT("Page %d"), pageNum);
  }

  if (m_previewFrame)
  {
    m_previewFrame->SetStatusText(status);
  }
#endif

  return true;
}

#endif


//----------------------------------------------------------------------------
// wxPdfPrintDialog
//----------------------------------------------------------------------------

IMPLEMENT_CLASS(wxPdfPrintDialog, wxPrintDialogBase)

BEGIN_EVENT_TABLE(wxPdfPrintDialog, wxPrintDialogBase)
    EVT_BUTTON(wxID_OK, wxPdfPrintDialog::OnOK)
    EVT_CHECKBOX(wxPDF_PRINTDIALOG_CTRLID_PROTECT, wxPdfPrintDialog::OnProtectCheck)
    EVT_FILEPICKER_CHANGED(wxPDF_PRINTDIALOG_CTRLID_FILEPICKER, wxPdfPrintDialog::OnFilepathChanged)
END_EVENT_TABLE()

wxPdfPrintDialog::wxPdfPrintDialog(wxWindow* parent, wxPdfPrintData* data)
  : wxPrintDialogBase(parent,
                      wxID_ANY, _("PDF Document Output"),
                      wxPoint(0,0), wxSize(600, 600),
                      wxDEFAULT_DIALOG_STYLE |
                      wxTAB_TRAVERSAL)
{
  m_pdfPrintData = *data;
  Init(parent);
}


void
wxPdfPrintDialog::Init(wxWindow* WXUNUSED(parent))
{
  int dialogFlags = m_pdfPrintData.GetPrintDialogFlags();

  wxBoxSizer* mainsizer = new wxBoxSizer(wxVERTICAL);
  wxFlexGridSizer* flex = new wxFlexGridSizer(2);
  flex->AddGrowableCol(1);

  // File Path

  flex->Add( new wxStaticText(this, wxID_ANY, _("Output File")), 0, wxALL, 5 );
  wxBoxSizer* filesizer = new wxBoxSizer(wxHORIZONTAL);
  m_filepath = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxSize(320, -1), wxTE_READONLY);
  filesizer->Add(m_filepath, 1, wxEXPAND|wxALL, 0);

  if (dialogFlags & wxPDF_PRINTDIALOG_FILEPATH)
  {
    m_filepicker = 
      new wxFilePickerCtrl(this, wxPDF_PRINTDIALOG_CTRLID_FILEPICKER, m_pdfPrintData.GetFilename(), 
                           _("Choose a file name for the PDF output"),
                           _("PDF Files (*.pdf)|*.pdf"), wxDefaultPosition, wxDefaultSize,
                           wxFLP_SAVE|wxFLP_OVERWRITE_PROMPT);

    filesizer->Add(m_filepicker, 0, wxEXPAND|wxLEFT, 4);
  }

  flex->Add(filesizer,1, wxALL|wxEXPAND, 3);

  if (dialogFlags & wxPDF_PRINTDIALOG_OPENDOC)
  {
    flex->AddSpacer(1);
    m_launchViewer = new wxCheckBox(this, wxID_ANY, _("Open Document in Default Viewer"));
    flex->Add(m_launchViewer, 1, wxALL|wxEXPAND, 5);
  }

  // Document Properties

  if (dialogFlags & wxPDF_PRINTDIALOG_PROPERTIES)
  {
    flex->Add(new wxStaticText(this, wxID_ANY, _("Title")), 0, wxALL, 5);
    m_title = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize);
    flex->Add(m_title, 1, wxALL|wxEXPAND, 3);

    flex->Add(new wxStaticText(this, wxID_ANY, _("Author")), 0, wxALL, 5);
    m_author = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize);
    flex->Add(m_author, 1, wxALL|wxEXPAND, 3);

    flex->Add(new wxStaticText(this, wxID_ANY, _("Subject")), 0, wxALL, 5);
    m_subject = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize);
    flex->Add(m_subject, 1, wxALL|wxEXPAND, 3);

    flex->Add(new wxStaticText(this, wxID_ANY, _("Keywords")), 0, wxALL, 5);
    m_keywords = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxSize(400,50), wxTE_MULTILINE);
    flex->Add(m_keywords, 1, wxALL|wxEXPAND, 3);
  }

  mainsizer->Add(flex, 0, wxLEFT|wxTOP|wxRIGHT|wxGROW, 10);

  // Document Protection
  wxString* compatchoices = new wxString[3];

  if (dialogFlags & wxPDF_PRINTDIALOG_PROTECTION)
  {
    wxFlexGridSizer* flex2 = new wxFlexGridSizer(2);
    flex2->AddGrowableCol(0);
    flex2->AddGrowableCol(1);

    m_protect = new wxCheckBox(this, wxPDF_PRINTDIALOG_CTRLID_PROTECT, _("Password Protect and Encrypt Document"));
    flex2->Add( m_protect, 0, wxEXPAND|wxALL, 5);

    compatchoices[0] = _("PDF Reader 7.0 Compatible");
    compatchoices[1] = _("PDF Reader 5.0 Compatible");
    compatchoices[2] = _("PDF Reader 3.0 Compatible");

    m_compat = new wxChoice(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, 3, compatchoices);

    m_compat->SetSelection(0);
    flex2->Add(m_compat, 1, wxEXPAND|wxALL, 5);

    mainsizer->Add(flex2, 0, wxEXPAND|wxTOP|wxLEFT|wxRIGHT, 10);

    wxFlexGridSizer* flex3 = new wxFlexGridSizer(4);
    flex3->AddGrowableCol(1);
    flex3->AddGrowableCol(3);

    flex3->Add(new wxStaticText(this, wxID_ANY, _("User Password")), 0, wxALL, 5);
    m_userpwd = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PASSWORD);
    flex3->Add(m_userpwd, 1, wxALL|wxEXPAND, 3);

    flex3->Add(new wxStaticText(this, wxID_ANY, _("Confirm")), 0, wxALL, 5);
    m_userpwdconfirm = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PASSWORD);
    flex3->Add(m_userpwdconfirm, 1, wxALL|wxEXPAND, 3);

    flex3->Add(new wxStaticText(this, wxID_ANY, _("Owner Password")), 0, wxALL, 5);
    m_ownerpwd = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PASSWORD);
    flex3->Add(m_ownerpwd, 1, wxALL|wxEXPAND, 3);

    flex3->Add( new wxStaticText(this, wxID_ANY, _("Confirm")), 0, wxALL, 5);
    m_ownerpwdconfirm = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PASSWORD);
    flex3->Add(m_ownerpwdconfirm, 1, wxALL|wxEXPAND, 3);

    mainsizer->Add(flex3, 0, wxEXPAND|wxTOP|wxLEFT|wxRIGHT, 10);

    wxFlexGridSizer* flex4 = new wxFlexGridSizer(3);
    flex4->AddGrowableCol(0);
    flex4->AddGrowableCol(1);
    flex4->AddGrowableCol(2);

    m_canprint = new wxCheckBox(this, wxID_ANY, _("Allow Printing"));
    flex4->Add(m_canprint, 0, wxEXPAND|wxLEFT|wxRIGHT|wxTOP, 5);

    m_canmodify = new wxCheckBox(this, wxID_ANY, _("Allow Modification"));
    flex4->Add(m_canmodify, 0, wxEXPAND|wxLEFT|wxRIGHT|wxTOP, 5);

    m_cancopy = new wxCheckBox(this, wxID_ANY, _("Allow Content Copying"));
    flex4->Add(m_cancopy, 0, wxEXPAND|wxLEFT|wxRIGHT|wxTOP, 5);

    m_canannot = new wxCheckBox(this, wxID_ANY, _("Allow Comments"));
    flex4->Add(m_canannot, 0, wxEXPAND|wxLEFT|wxRIGHT|wxTOP, 5);

    m_canform = new wxCheckBox(this, wxID_ANY, _("Allow Fill Form Fields"));
    flex4->Add(m_canform, 0, wxEXPAND|wxLEFT|wxRIGHT|wxTOP, 5);

    m_canextract = new wxCheckBox(this, wxID_ANY, _("Allow Content Extract"));
    flex4->Add(m_canextract, 0, wxEXPAND|wxLEFT|wxRIGHT|wxTOP, 5);

    m_canassemble = new wxCheckBox(this, wxID_ANY, _("Allow Assembly"));
    flex4->Add(m_canassemble, 0, wxEXPAND|wxLEFT|wxRIGHT|wxTOP, 5);

    mainsizer->Add(flex4, 0, wxEXPAND|wxTOP|wxLEFT|wxRIGHT, 10);
  }

  // Standard Dialog Buttons

  //wxBoxSizer* bottomsizer = new wxBoxSizer(wxHORIZONTAL);

  wxSizer* sizerBtn = CreateSeparatedButtonSizer(wxOK|wxCANCEL);
  if (sizerBtn)
  {
    mainsizer->Add(sizerBtn, 0, wxEXPAND|wxALL, 10);
  }

  SetAutoLayout(true);
  SetSizer(mainsizer);

  mainsizer->Fit(this);
  Centre(wxBOTH);

  // Calls wxWindow::OnInitDialog and then wxPdfPrintDialog::TransferDataToWindow
  InitDialog();
  delete[] compatchoices;
}

int
wxPdfPrintDialog::ShowModal()
{
  return wxDialog::ShowModal();
}

wxPdfPrintDialog::~wxPdfPrintDialog()
{
}

void
wxPdfPrintDialog::OnOK(wxCommandEvent& WXUNUSED(event))
{
  if (!TransferDataFromWindow())
    return;

  EndModal(wxID_OK);
}

void
wxPdfPrintDialog::OnProtectCheck(wxCommandEvent& WXUNUSED(event))
{
  UpdateProtectionControls();
}

void
wxPdfPrintDialog::OnFilepathChanged(wxFileDirPickerEvent& event)
{
  m_filepath->ChangeValue(event.GetPath());
}

void
wxPdfPrintDialog::UpdateProtectionControls()
{
  int dialogFlags = m_pdfPrintData.GetPrintDialogFlags();

  if (dialogFlags & wxPDF_PRINTDIALOG_PROTECTION)
  {
    bool protectionEnabled = m_protect->GetValue();

    m_canprint->Enable(protectionEnabled);
    m_canmodify->Enable(protectionEnabled);
    m_cancopy->Enable(protectionEnabled);
    m_canannot->Enable(protectionEnabled);
    m_canform->Enable(protectionEnabled);
    m_canextract->Enable(protectionEnabled);
    m_canassemble->Enable(protectionEnabled);

    m_ownerpwd->Enable(protectionEnabled);
    m_userpwd->Enable(protectionEnabled);
    m_ownerpwdconfirm->Enable(protectionEnabled);
    m_userpwdconfirm->Enable(protectionEnabled);

    m_compat->Enable(protectionEnabled);
  }
}

bool
wxPdfPrintDialog::TransferDataToWindow()
{
  int dialogFlags = m_pdfPrintData.GetPrintDialogFlags();

  m_filepath->ChangeValue(m_pdfPrintData.GetFilename());

  if (dialogFlags & wxPDF_PRINTDIALOG_OPENDOC)
  {
    m_launchViewer->SetValue(m_pdfPrintData.GetLaunchDocumentViewer());
  }

  if (dialogFlags & wxPDF_PRINTDIALOG_PROPERTIES)
  {
    m_title->ChangeValue(m_pdfPrintData.GetDocumentTitle()) ;
    m_subject->ChangeValue(m_pdfPrintData.GetDocumentSubject()) ;
    m_author->ChangeValue(m_pdfPrintData.GetDocumentAuthor()) ;
    m_keywords->ChangeValue(m_pdfPrintData.GetDocumentKeywords()) ;
  }

  bool protectionEnabled = m_pdfPrintData.IsProtectionEnabled();

  if (dialogFlags & wxPDF_PRINTDIALOG_PROTECTION)
  {
    m_protect->SetValue(protectionEnabled);

    int permFlags = m_pdfPrintData.GetPermissions();
    m_canprint->SetValue((permFlags & wxPDF_PERMISSION_PRINT) || (permFlags & wxPDF_PERMISSION_HLPRINT));
    m_canmodify->SetValue((permFlags & wxPDF_PERMISSION_MODIFY) != 0);
    m_cancopy->SetValue((permFlags & wxPDF_PERMISSION_COPY) != 0);
    m_canannot->SetValue((permFlags & wxPDF_PERMISSION_ANNOT) != 0);
    m_canform->SetValue((permFlags & wxPDF_PERMISSION_FILLFORM) != 0);
    m_canextract->SetValue((permFlags & wxPDF_PERMISSION_EXTRACT) != 0);
    m_canassemble->SetValue((permFlags & wxPDF_PERMISSION_ASSEMBLE) != 0);

    m_ownerpwd->ChangeValue(m_pdfPrintData.GetOwnerPassword());
    m_userpwd->ChangeValue(m_pdfPrintData.GetUserPassword());
    m_ownerpwdconfirm->ChangeValue(m_pdfPrintData.GetOwnerPassword());
    m_userpwdconfirm->ChangeValue(m_pdfPrintData.GetUserPassword());

    switch (m_pdfPrintData.GetEncryptionMethod())
    {
      case wxPDF_ENCRYPTION_RC4V1:
        m_compat->SetSelection(2);
        break;
      case wxPDF_ENCRYPTION_RC4V2:
        m_compat->SetSelection(1);
        break;
      default:
        m_compat->SetSelection(0);
        break;
    }

    UpdateProtectionControls();
  }

  return true;
}

bool
wxPdfPrintDialog::TransferDataFromWindow()
{
  int dialogFlags = m_pdfPrintData.GetPrintDialogFlags();

  // Do protection first so that we may exit
  // without having changed any values if
  // passwords & confirms are not equal

  if (dialogFlags & wxPDF_PRINTDIALOG_PROTECTION)
  {
    if (m_protect->GetValue())
    {
      if (m_userpwd->GetValue().Cmp(m_userpwdconfirm->GetValue()) != 0)
      {
        wxLogError(_("Your values for User Password and the confirmation are not the same."));
        return false;
      }

      if (m_ownerpwd->GetValue().Cmp(m_ownerpwdconfirm->GetValue()) != 0)
      {
        wxLogError(_("Your values for Owner Password and the confirmation are not the same."));
        return false;
      }

      int permissions = wxPDF_PERMISSION_NONE;

      if (m_canprint->GetValue())
      {
        permissions |= wxPDF_PERMISSION_PRINT;
      }

      if (m_canmodify->GetValue())
      {
        permissions |= wxPDF_PERMISSION_MODIFY;
      }

      if (m_cancopy->GetValue())
      {
        permissions |= wxPDF_PERMISSION_COPY;
      }

      if (m_canannot->GetValue())
      {
        permissions |= wxPDF_PERMISSION_ANNOT;
      }

      if (m_canform->GetValue())
      {
        permissions |= wxPDF_PERMISSION_FILLFORM;
      }

      if (m_canextract->GetValue())
      {
        permissions |= wxPDF_PERMISSION_EXTRACT;
      }

      if (m_canassemble->GetValue())
      {
        permissions |= wxPDF_PERMISSION_ASSEMBLE;
      }

      int keylength;
      wxPdfEncryptionMethod encryptMethod;

      switch (m_compat->GetSelection())
      {
        case 0:
          keylength = 128;
          encryptMethod = wxPDF_ENCRYPTION_AESV2;
          break;
        case 1:
          keylength = 128;
          encryptMethod = wxPDF_ENCRYPTION_RC4V2;
          break;
        default:
          keylength = 40;
          encryptMethod = wxPDF_ENCRYPTION_RC4V1;
          break;
      }

      m_pdfPrintData.SetDocumentProtection(permissions,
                                           m_userpwd->GetValue(),
                                           m_ownerpwd->GetValue(),
                                           encryptMethod,
                                           keylength);
    }
    else
    {
      m_pdfPrintData.ClearDocumentProtection();
    }
  }

  // Filepath
  if (dialogFlags & wxPDF_PRINTDIALOG_FILEPATH)
  {
    m_pdfPrintData.SetFilename(m_filepath->GetValue());
  }

  if (dialogFlags & wxPDF_PRINTDIALOG_OPENDOC)
  {
    m_pdfPrintData.SetLaunchDocumentViewer(m_launchViewer->GetValue());
  }

  // Properties

  if (dialogFlags & wxPDF_PRINTDIALOG_PROPERTIES)
  {
    m_pdfPrintData.SetDocumentTitle(m_title->GetValue());
    m_pdfPrintData.SetDocumentSubject(m_subject->GetValue());
    m_pdfPrintData.SetDocumentAuthor(m_author->GetValue());
    m_pdfPrintData.SetDocumentKeywords(m_keywords->GetValue());
  }

  return true;
}

wxPdfDC*
wxPdfPrintDialog::GetPrintDC()
{
  return new wxPdfDC(*GetPdfPrintData().CreatePrintData());
}

// ---------------------------------------------------------------------------
// wxPdfPageSetupDialogCanvas: paper drawing canvas
// ---------------------------------------------------------------------------

IMPLEMENT_CLASS(wxPdfPageSetupDialogCanvas, wxWindow)

BEGIN_EVENT_TABLE(wxPdfPageSetupDialogCanvas, wxWindow)
    EVT_PAINT(wxPdfPageSetupDialogCanvas::OnPaint)
END_EVENT_TABLE()

wxPdfPageSetupDialogCanvas::wxPdfPageSetupDialogCanvas(wxWindow* parent)
  : wxWindow( parent, wxID_ANY, wxDefaultPosition, wxSize(300,200), wxBORDER_THEME)
{
  m_paperWidth = 210;
  m_paperHeight = 297;
  m_marginLeft = 25;
  m_marginTop = 25;
  m_marginRight = 25;
  m_marginBottom = 25;
}

wxPdfPageSetupDialogCanvas::~wxPdfPageSetupDialogCanvas()
{
}

void
wxPdfPageSetupDialogCanvas::OnPaint(wxPaintEvent& WXUNUSED(event))
{
  wxPaintDC dc(this);

  //------------------------------------------
  // Get the sizes and bounds
  //------------------------------------------

  int px = m_paperWidth;
  int py = m_paperHeight;
  int ps = (px > py) ? px : py;

  int w, h;
  dc.GetSize(&w, &h);

  double scale = ((double)h - 10.0 ) / (double)ps;

  int rw = int( px * scale );
  int rh = int( py * scale );
  int rx = int( (w - rw) / 2 );
  int ry = int( (h - rh) / 2 );

  int ml = int( m_marginLeft * scale );
  int mr = int( m_marginRight * scale );
  int mt = int( m_marginTop * scale );
  int mb = int( m_marginBottom * scale );

  //------------------------------------------
  // save current dc objects
  //------------------------------------------

  wxBrush restorebackground = dc.GetBackground();
  wxBrush restorebrush = dc.GetBrush();
  wxPen restorepen = dc.GetPen();

  wxBrush* lightBrush = new wxBrush(wxColour(220,220,220));

  dc.SetBackground(*lightBrush);
  dc.Clear();

  int clipx, clipy, clipw, cliph;
  dc.GetClippingBox(&clipx, &clipy, &clipw, &cliph);

  //------------------------------------------
  // Draw a 'shadow' paper
  //------------------------------------------

  wxBrush* shadowBrush = new wxBrush(wxColour(175,175,175));

  dc.SetBrush(*shadowBrush);
  dc.SetPen(*wxTRANSPARENT_PEN);

  dc.DrawRectangle(rx + 3, ry + 3, rw, rh);

  //------------------------------------------
  // Draw paper
  //------------------------------------------

  dc.SetBrush(*wxWHITE_BRUSH);
  dc.SetPen(*wxBLACK_PEN);

  dc.DrawRectangle(rx, ry, rw, rh);

  //------------------------------------------
  // Draw margins
  //------------------------------------------

  wxPen* dashpen = new wxPen(wxColour(255,0,125), 1 , wxPENSTYLE_USER_DASH);
  wxDash pDash[2] = { 3, 3 };
  dashpen->SetDashes(2, pDash);
  dc.SetPen(*dashpen);
  dc.DrawLine(rx + ml, ry + 1, rx + ml, ( ry + rh ) - 2 ); // left margin
  dc.DrawLine(rx + 1, ry + mt, ( rx + rw ) -1, ry + mt );  // top margin
  dc.DrawLine((rx + rw) - mr, ry + 1, (rx + rw) - mr, ( ry + rh ) - 2 );// right margin
  dc.DrawLine(rx + 1, (ry + rh) - mb, ( rx + rw ) -1, (ry + rh) - mb );  // bottom margin
  dc.SetPen(*wxTRANSPARENT_PEN);

  //------------------------------------------
  // Draw fake content
  //------------------------------------------

  int linewidth = 4;

  int sx = rx + ml + 2;
  int sw = rw - ( ml + mr + 4 );
  int sy = ry + mt + 2;
  int sh = rh - ( mt + mb + 4 );
  int smax = (ry + rh) - mb;

  dc.SetBrush(*lightBrush);
  dc.SetPen(*wxTRANSPARENT_PEN);
  dc.SetClippingRegion( sx, sy, sw, sh );

  while ( sy < smax )
  {
    dc.DrawRectangle(sx, sy, sw, linewidth);
    sy += (linewidth + 3);
  }

  //------------------------------------------
  // Restore DC objects
  //------------------------------------------

  dc.DestroyClippingRegion();
  dc.SetClippingRegion(clipx, clipy, clipw, cliph);
  dc.SetBrush(restorebrush);
  dc.SetPen(restorepen);
  dc.SetBackground(restorebackground);

  delete lightBrush;
  delete shadowBrush;
  delete dashpen;
}

// ---------------------------------------------------------------------------
// wxPdfPageSetupDialog: the page setup dialog
// ---------------------------------------------------------------------------

IMPLEMENT_CLASS(wxPdfPageSetupDialog, wxDialog)

BEGIN_EVENT_TABLE(wxPdfPageSetupDialog, wxDialog)
    EVT_BUTTON(wxID_OK, wxPdfPageSetupDialog::OnOK)
    EVT_CHOICE(wxPDF_PAGEDIALOG_CTRLID_MARGINUNIT, wxPdfPageSetupDialog::OnMarginUnit)
    EVT_CHOICE(wxPDF_PAGEDIALOG_CTRLID_PAPER, wxPdfPageSetupDialog::OnPaperType)
    EVT_CHOICE(wxPDF_PAGEDIALOG_CTRLID_ORIENTATION, wxPdfPageSetupDialog::OnOrientation)
    EVT_TEXT(wxPDF_PAGEDIALOG_CTRLID_MARGINLEFT, wxPdfPageSetupDialog::OnMarginText)
    EVT_TEXT(wxPDF_PAGEDIALOG_CTRLID_MARGINRIGHT, wxPdfPageSetupDialog::OnMarginText)
    EVT_TEXT(wxPDF_PAGEDIALOG_CTRLID_MARGINTOP, wxPdfPageSetupDialog::OnMarginText)
    EVT_TEXT(wxPDF_PAGEDIALOG_CTRLID_MARGINBOTTOM, wxPdfPageSetupDialog::OnMarginText)
END_EVENT_TABLE()

wxPdfPageSetupDialog::wxPdfPageSetupDialog(wxWindow* parent, wxPageSetupDialogData* data, const wxString& title)
  : wxDialog(parent, wxID_ANY, title) 
{
  if (title.IsEmpty())
  {
    SetTitle(_("PDF Document Page Setup"));
  }

  m_pageData = *data;
  Init();
}

wxPdfPageSetupDialog::~wxPdfPageSetupDialog()
{
}

void
wxPdfPageSetupDialog::Init()
{
  if (wxLocale::GetSystemLanguage() == wxLANGUAGE_ENGLISH_US)
  {
    m_defaultUnitSelection = 2;
    m_defaultPaperId = wxPAPER_LETTER;
  }
  else
  {
    m_defaultUnitSelection = 0;
    m_defaultPaperId = wxPAPER_A4;
  }

  wxBoxSizer* mainsizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* canvassizer = new wxBoxSizer(wxHORIZONTAL);

  m_paperCanvas = new wxPdfPageSetupDialogCanvas(this);
  canvassizer->Add(m_paperCanvas,1, wxEXPAND|wxALL, 0);

  wxString* marginchoices = NULL;
  wxString* orientchoices = NULL;
  wxString* paperchoices  = NULL;

  if (m_pageData.GetEnableMargins())
  {
    wxBoxSizer *marginsizer = new wxBoxSizer(wxVERTICAL);
    marginsizer->Add(new wxStaticText(this, wxID_ANY, _("Margins")), 0, wxEXPAND|wxTOP, 15);
    marginchoices = new wxString[3];
    marginchoices[0] = _("Millimetres");
    marginchoices[1] = _("Centimetres");
    marginchoices[2] = _("Inches");

    m_marginUnits = new wxChoice(this, wxPDF_PAGEDIALOG_CTRLID_MARGINUNIT, 
                                 wxDefaultPosition, wxDefaultSize, 3, marginchoices);

    marginsizer->Add(m_marginUnits, 0, wxEXPAND|wxALL, 0);

    wxFlexGridSizer *marginflex = new wxFlexGridSizer(0, 2, 3, 3);
    marginflex->AddGrowableCol(1);

    marginflex->Add(new wxStaticText(this, wxID_ANY, _("Left")), 0, wxTOP|wxEXPAND, 3);
    m_marginLeftText = new wxTextCtrl(this, wxPDF_PAGEDIALOG_CTRLID_MARGINLEFT, wxEmptyString, wxDefaultPosition, wxSize(50, -1));
    m_marginLeftText->SetValidator(wxTextValidator(wxFILTER_NUMERIC));
    marginflex->Add(m_marginLeftText, 0, wxALL|wxEXPAND, 0);

    marginflex->Add(new wxStaticText( this, wxID_ANY, _("Top")), 0, wxTOP|wxEXPAND, 3);
    m_marginTopText = new wxTextCtrl(this, wxPDF_PAGEDIALOG_CTRLID_MARGINTOP, wxEmptyString, wxDefaultPosition, wxSize(50, -1));
    m_marginTopText->SetValidator(wxTextValidator(wxFILTER_NUMERIC));
    marginflex->Add(m_marginTopText, 0, wxALL|wxEXPAND, 0);

    marginflex->Add(new wxStaticText( this, wxID_ANY, _("Right") ), 0, wxTOP|wxEXPAND, 3);
    m_marginRightText = new wxTextCtrl(this, wxPDF_PAGEDIALOG_CTRLID_MARGINRIGHT, wxEmptyString, wxDefaultPosition, wxSize(50, -1));
    m_marginRightText->SetValidator(wxTextValidator(wxFILTER_NUMERIC));
    marginflex->Add(m_marginRightText, 0, wxALL|wxEXPAND, 0);

    marginflex->Add(new wxStaticText( this, wxID_ANY, _("Bottom") ), 0, wxTOP|wxEXPAND, 3);
    m_marginBottomText = new wxTextCtrl(this, wxPDF_PAGEDIALOG_CTRLID_MARGINBOTTOM, wxEmptyString, wxDefaultPosition, wxSize(50, -1));
    m_marginBottomText->SetValidator(wxTextValidator(wxFILTER_NUMERIC));
    marginflex->Add(m_marginBottomText, 0, wxALL|wxEXPAND, 0);

    marginsizer->Add(marginflex, 0, wxEXPAND|wxTOP, 5);
    canvassizer->Add(marginsizer, 0, wxEXPAND|wxLEFT, 10);
  }

  mainsizer->Add(canvassizer, 0, wxEXPAND|wxALL, 10);

  wxFlexGridSizer* papersizer = NULL;

  if (m_pageData.GetEnableOrientation() || m_pageData.GetEnablePaper())
  {
    papersizer = new wxFlexGridSizer( 0,2,5,20 );
    papersizer->AddGrowableCol(1);
  }

  if (m_pageData.GetEnablePaper())
  {
    papersizer->Add(new wxStaticText(this, wxID_ANY, _("Paper Size")), 0, wxTOP|wxEXPAND, 3);
    size_t n = wxThePrintPaperDatabase->GetCount();
    paperchoices  = new wxString [n];
    for (size_t i = 0; i < n; ++i)
    {
      wxPrintPaperType *paper = wxThePrintPaperDatabase->Item(i);
      paperchoices[i] = paper->GetName();
    }

    m_paperTypeChoice = new wxChoice(this, wxPDF_PAGEDIALOG_CTRLID_PAPER, 
                                     wxDefaultPosition, wxDefaultSize, n, paperchoices);
    papersizer->Add(m_paperTypeChoice,  1, wxEXPAND|wxALL, 0);
  }

  if (m_pageData.GetEnableOrientation())
  {
    papersizer->Add(new wxStaticText(this, wxID_ANY, _("Orientation")), 0, wxTOP|wxEXPAND, 3);
    orientchoices = new wxString [2];
    orientchoices[0] = _("Portrait");
    orientchoices[1] = _("Landscape");

    m_orientationChoice = new wxChoice(this, wxPDF_PAGEDIALOG_CTRLID_ORIENTATION, 
                                       wxDefaultPosition, wxDefaultSize, 2, orientchoices);
    papersizer->Add(m_orientationChoice,  1, wxEXPAND|wxALL, 0);
  }

  if (m_pageData.GetEnableOrientation() || m_pageData.GetEnablePaper())
  {
    mainsizer->Add(papersizer, 0, wxEXPAND|wxALL, 10);
  }

  // Standard Dialog Buttons
  //wxBoxSizer* bottomsizer = new wxBoxSizer(wxHORIZONTAL);

  wxSizer* sizerBtn = CreateSeparatedButtonSizer(wxOK|wxCANCEL);
  if (sizerBtn)
  {
    mainsizer->Add(sizerBtn, 0, wxEXPAND|wxALL, 10 );
  }

  SetAutoLayout(true);
  SetSizer(mainsizer);

  mainsizer->Fit(this);
  Centre(wxBOTH);

  // Calls wxWindow::OnInitDialog and then wxPdfPrintDialog::TransferDataToWindow
  InitDialog();

  if (marginchoices)
  {
    delete[] marginchoices;
  }

  if (orientchoices)
  {
    delete[] orientchoices;
  }
    
  if (paperchoices)
  {
    delete[] paperchoices;
  }
}

wxPageSetupDialogData&
wxPdfPageSetupDialog::GetPageSetupDialogData()
{
  return m_pageData;
}

// old name
wxPageSetupDialogData&
wxPdfPageSetupDialog::GetPageSetupData()
{
  return GetPageSetupDialogData();
}

void
wxPdfPageSetupDialog::OnOK(wxCommandEvent& WXUNUSED(event))
{
  if (!TransferDataFromWindow())
    return;

  EndModal(wxID_OK);
}

void
wxPdfPageSetupDialog::OnMarginUnit(wxCommandEvent& WXUNUSED(event))
{
  TransferMarginsToControls();
}

void
wxPdfPageSetupDialog::OnPaperType(wxCommandEvent& WXUNUSED(event))
{
  wxPrintPaperType *paper = wxThePrintPaperDatabase->FindPaperType(m_paperTypeChoice->GetStringSelection());
  if (!paper)
  {
    wxLogError(_("Unknown Paper Type Selected"));
    return;
  }
  m_paperId = paper->GetId();
  wxSize pSize = paper->GetSizeMM();
  m_pageWidth = pSize.GetWidth();
  m_pageHeight = pSize.GetHeight();
  if (m_pageData.GetEnableMargins())
  {
    TransferControlsToMargins();
    TransferMarginsToControls();
  }
  UpdatePaperCanvas();
}

void
wxPdfPageSetupDialog::OnOrientation(wxCommandEvent& WXUNUSED(event))
{
  if (m_orientationChoice->GetSelection() == 1)
  {
    m_orientation = wxLANDSCAPE;
  }
  else
  {
    m_orientation = wxPORTRAIT;
  }
  if (m_pageData.GetEnableMargins())
  {
    TransferControlsToMargins();
    TransferMarginsToControls();
  }
  UpdatePaperCanvas();
}

void
wxPdfPageSetupDialog::OnMarginText(wxCommandEvent& WXUNUSED(event))
{
  TransferControlsToMargins();
  UpdatePaperCanvas();
}

void
wxPdfPageSetupDialog::UpdatePaperCanvas()
{
  if (m_orientation == wxPORTRAIT)
  {
    m_paperCanvas->UpdatePageMetrics(m_pageWidth, m_pageHeight, m_marginLeft,
                                     m_marginRight, m_marginTop, m_marginBottom);
  }
  else
  {
    m_paperCanvas->UpdatePageMetrics(m_pageHeight, m_pageWidth, m_marginLeft,
                                     m_marginRight, m_marginTop, m_marginBottom);        
  }

  m_paperCanvas->Refresh();
}

bool
wxPdfPageSetupDialog::TransferDataFromWindow()
{
  if (m_pageData.GetEnableMargins())
  {
    TransferControlsToMargins();
    m_pageData.SetMarginTopLeft(wxPoint(m_marginLeft, m_marginTop));
    m_pageData.SetMarginBottomRight(wxPoint(m_marginRight, m_marginBottom));
  }

  if (m_pageData.GetEnablePaper())
  {
    m_pageData.GetPrintData().SetPaperId(m_paperId);
    m_pageData.SetPaperId(m_paperId);
  }
    
  if ( m_pageData.GetEnableOrientation() )
  {
    m_pageData.GetPrintData().SetOrientation(m_orientation);
  }

  return true;
}

bool
wxPdfPageSetupDialog::TransferDataToWindow()
{
  // work out defaults
  int useUnitSelection = false;

  wxPrintData printData = m_pageData.GetPrintData();

  m_paperId = printData.GetPaperId();
  m_orientation = printData.GetOrientation();

  wxPrintPaperType* paper = wxThePrintPaperDatabase->FindPaperType(m_paperId);
  if (!paper)
  {
    paper = wxThePrintPaperDatabase->FindPaperType(m_defaultPaperId);
    m_paperId = paper->GetId();
  }

  wxSize pSize = paper->GetSizeMM();
  m_pageWidth = pSize.GetWidth();
  m_pageHeight = pSize.GetHeight();

  if ((m_orientation != wxPORTRAIT) && (m_orientation != wxLANDSCAPE))
  {
    m_orientation = wxPORTRAIT;
  }

  m_marginLeft    = m_pageData.GetMarginTopLeft().x;
  m_marginTop     = m_pageData.GetMarginTopLeft().y;
  m_marginRight   = m_pageData.GetMarginBottomRight().x;
  m_marginBottom  = m_pageData.GetMarginBottomRight().y;

  if (m_pageData.GetEnableMargins())
  {
    m_marginUnits->SetSelection(useUnitSelection);
    TransferMarginsToControls();
  }

  if (m_pageData.GetEnableOrientation())
  {
    if (m_orientation == wxLANDSCAPE)
    {
      m_orientationChoice->SetSelection(1);
    }
    else
    {
      m_orientationChoice->SetSelection(0);
    }
  }

  m_paperTypeChoice->SetStringSelection(paper->GetName());
  UpdatePaperCanvas();
  return true;
}

void
wxPdfPageSetupDialog::TransferMarginsToControls()
{
  int unitSelection = m_marginUnits->GetSelection();
  double marginScaleToUnit;
  wxChar* formatS;
    
  switch (unitSelection)
  {
    case 0:
      // mm
      marginScaleToUnit = 1.0;
      formatS = (wxChar*)wxT("%.0f");
      break;
    case 1:
      // cm
      marginScaleToUnit = 0.1;
      formatS = (wxChar*)wxT("%#.1f");
      break;
    case 2:
      // inch
      marginScaleToUnit = 1.0 / 25.4;
      formatS = (wxChar*)wxT("%#.2f");
      break;
    default:
      wxLogError(_("Unknown margin unit format in margin to control transfer."));
      return;
  }

  m_marginLeftText->ChangeValue(wxString::Format(formatS, (double) m_marginLeft * marginScaleToUnit));
  m_marginTopText->ChangeValue(wxString::Format(formatS, (double) m_marginTop * marginScaleToUnit));
  m_marginRightText->ChangeValue(wxString::Format(formatS, (double) m_marginRight * marginScaleToUnit));
  m_marginBottomText->ChangeValue(wxString::Format(formatS, (double) m_marginBottom * marginScaleToUnit));
}

void
wxPdfPageSetupDialog::TransferControlsToMargins()
{   
  int unitSelection = m_marginUnits->GetSelection();
  double marginScaleToMM = 1.0;
  //int defaultMargin = 10;
  double strDbl;

  int maxX, maxY;

  if (m_orientation == wxPORTRAIT)
  {
    maxX = ( m_pageWidth / 2 ) -1;
    maxY = ( m_pageHeight / 2 ) -1;
  }
  else
  {
    maxX = ( m_pageHeight / 2 ) -1;
    maxY = ( m_pageWidth / 2 ) -1;
  }

  switch (unitSelection)
  {
    case 0:
      marginScaleToMM = 1.0;
      break;
    case 1:
      // cm
      marginScaleToMM = 10.0;
      break;
    case 2:
      // inch
      marginScaleToMM = 25.4;
      break;
    default:
      wxLogError(_("Unknown margin unit format in control to margin transfer."));
      break;
  }

  if (m_marginLeftText->GetValue().ToDouble(&strDbl))
  {
    m_marginLeft = abs(wxRound( strDbl * marginScaleToMM ));
    if (m_marginLeft > maxX)
    {
      m_marginLeft = maxX;
    }
  }

  if (m_marginTopText->GetValue().ToDouble(&strDbl))
  {
    m_marginTop = abs(wxRound( strDbl * marginScaleToMM ));
    if (m_marginTop > maxY)
    {
      m_marginTop = maxY;
    }
  }

  if (m_marginRightText->GetValue().ToDouble(&strDbl))
  {
    m_marginRight = abs(wxRound( strDbl * marginScaleToMM ));
    if (m_marginRight > maxX)
    {
      m_marginRight = maxX;
    }
  }

  if (m_marginBottomText->GetValue().ToDouble(&strDbl))
  {
    m_marginBottom = abs(wxRound(strDbl * marginScaleToMM));
    if (m_marginBottom > maxY)
    {
      m_marginBottom = maxY;
    }
  }
}

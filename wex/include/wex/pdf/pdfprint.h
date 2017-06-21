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

#ifndef _PDF_PRINTING_H_
#define _PDF_PRINTING_H_

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

#include <wx/defs.h>

#include <wx/dialog.h>
#include <wx/cmndata.h>
#include <wx/prntbase.h>
#include <wx/printdlg.h>
#include <wx/listctrl.h>
#include <wx/filepicker.h>
#include <wx/checkbox.h>

#include "wex/pdf/pdfdc.h"

#define wxPDF_PRINTER_DEFAULT_RESOLUTION 600

//----------------------------------------------------------------------------
// wxPdfPrintData
//----------------------------------------------------------------------------

#define wxPDF_PRINTDIALOG_ALLOWNONE  0
#define wxPDF_PRINTDIALOG_FILEPATH   0x01
#define wxPDF_PRINTDIALOG_PROPERTIES 0x02
#define wxPDF_PRINTDIALOG_PROTECTION 0x04
#define wxPDF_PRINTDIALOG_OPENDOC    0x08
#define wxPDF_PRINTDIALOG_ALLOWALL   wxPDF_PRINTDIALOG_FILEPATH   \
                                    |wxPDF_PRINTDIALOG_PROPERTIES \
                                    |wxPDF_PRINTDIALOG_PROTECTION \
                                    |wxPDF_PRINTDIALOG_OPENDOC

class WXDLLIMPEXP_PDFDOC wxPdfPrintData : public wxObject
{
public:
  wxPdfPrintData();
  wxPdfPrintData(wxPdfPrintData* pdfPrintData);
  wxPdfPrintData(wxPrintData* printData);
  wxPdfPrintData(wxPrintDialogData* printDialogData);
  wxPdfPrintData(wxPageSetupDialogData* pageSetupDialogData);

  bool Ok() const { return IsOk(); }
  bool IsOk() const { return true; }

  // wxPrintData compatibility

#if wxCHECK_VERSION(2,9,0)
  wxPrintOrientation GetOrientation() const { return m_printOrientation; }
  void SetOrientation(wxPrintOrientation orient) { m_printOrientation = orient; }
#else
  int GetOrientation() const { return m_printOrientation; }
  void SetOrientation(int orient) { m_printOrientation = orient; }
#endif

  wxPaperSize GetPaperId() const { return m_paperId; }
  void SetPaperId(wxPaperSize sizeId) { m_paperId = sizeId; }

  wxPrintQuality GetQuality() const { return m_printQuality; }
  void SetQuality(wxPrintQuality quality) { m_printQuality = quality; }

  wxString GetFilename() const { return m_filename; }
  void SetFilename( const wxString &filename ) { m_filename = filename; }

  // wxPrintDialogData compatibility

  int GetFromPage() const { return m_printFromPage; }
  int GetToPage() const { return m_printToPage; }
  int GetMinPage() const { return m_printMinPage; }
  int GetMaxPage() const { return m_printMaxPage; }
  void SetFromPage(int v) { m_printFromPage = v; }
  void SetToPage(int v) { m_printToPage = v; }
  void SetMinPage(int v) { m_printMinPage = v; }
  void SetMaxPage(int v) { m_printMaxPage = v; }

  // wxPdfPrintDialog flags

  int GetPrintDialogFlags() const { return m_printDialogFlags; }
  void SetPrintDialogFlags(int flags) { m_printDialogFlags = flags; }

  // wxPdfDocument specific

  void SetTemplate(wxPdfDocument* pdfDocument, double templateWidth, double templateHeight);
  wxPdfDocument* GetTemplateDocument() const { return m_templateDocument; }
  double GetTemplateWidth() const { return m_templateWidth; }
  double GetTemplateHeight() const { return m_templateHeight; }
  bool GetTemplateMode() const { return m_templateMode; }

  int GetPrintResolution() const;
  void SetPrintResolution( int resolution ) { m_printQuality = resolution; }

  bool GetLaunchDocumentViewer() const { return m_launchViewer; }
  void SetLaunchDocumentViewer( bool enable ) { m_launchViewer = enable; }

  const wxString& GetDocumentTitle() const { return m_documentTitle; }
  const wxString& GetDocumentSubject() const { return m_documentSubject; }
  const wxString& GetDocumentAuthor() const { return m_documentAuthor; }
  const wxString& GetDocumentKeywords() const { return m_documentKeywords; }
  const wxString& GetDocumentCreator() const { return m_documentCreator; }

  void SetDocumentTitle(const wxString& title) { m_documentTitle = title; }
  void SetDocumentSubject(const wxString& subject) { m_documentSubject = subject; }
  void SetDocumentAuthor(const wxString& author) { m_documentAuthor = author; }
  void SetDocumentKeywords(const wxString& keywords) { m_documentKeywords = keywords; }
  void SetDocumentCreator(const wxString& creator) { m_documentCreator = creator; }

  bool IsProtectionEnabled() const { return  m_protectionEnabled; }

  void SetDocumentProtection(int permissions,
                             const wxString& userPassword = wxEmptyString,
                             const wxString& ownerPassword = wxEmptyString,
                             wxPdfEncryptionMethod encryptionMethod = wxPDF_ENCRYPTION_RC4V1,
                             int keyLength = 0);

  const wxString& GetUserPassword() const { return m_userPassword; }
  const wxString& GetOwnerPassword() const { return m_ownerPassword; }
  int GetPermissions() const { return m_permissions; }
  wxPdfEncryptionMethod GetEncryptionMethod() const { return m_encryptionMethod; }
  int GetKeyLength() const { return m_keyLength; }

  void ClearDocumentProtection();

  void UpdateDocument(wxPdfDocument* pdfDoc);

  // Utility methods
  wxPrintData* CreatePrintData() const;

private:
  void Init();

  wxString              m_documentTitle;
  wxString              m_documentSubject;
  wxString              m_documentAuthor;
  wxString              m_documentKeywords;
  wxString              m_documentCreator;
  bool                  m_protectionEnabled;
  wxString              m_userPassword;
  wxString              m_ownerPassword;
  int                   m_permissions;
  wxPdfEncryptionMethod m_encryptionMethod;
  int                   m_keyLength;
#if wxCHECK_VERSION(2,9,0)
  wxPrintOrientation    m_printOrientation;
#else
  int                   m_printOrientation;
#endif
  int                   m_printQuality;
  wxPaperSize           m_paperId;
  wxString              m_filename;

  int                   m_printFromPage;
  int                   m_printToPage;
  int                   m_printMinPage;
  int                   m_printMaxPage;

  int                   m_printDialogFlags;
  bool                  m_launchViewer;

  wxPdfDocument*        m_templateDocument;
  double                m_templateHeight;
  double                m_templateWidth;
  bool                  m_templateMode;

private:
  DECLARE_DYNAMIC_CLASS(wxPdfPrintData)
};


//----------------------------------------------------------------------------
// wxPdfPrinter
//----------------------------------------------------------------------------

class WXDLLIMPEXP_PDFDOC wxPdfPrinter : public wxPrinterBase
{
public:
  wxPdfPrinter();
  wxPdfPrinter(wxPrintDialogData* data);
  wxPdfPrinter(wxPdfPrintData* data);
  wxPdfPrinter(wxPrintData* data);

  // standard implementation
  virtual bool Print(wxWindow* parent, wxPrintout* printout, bool prompt = true);
  virtual wxDC* PrintDialog(wxWindow* parent);
  virtual bool Setup(wxWindow* parent);

  // our additions
  void ShowProgressDialog(bool show) { m_showProgressDialog = show; }

private:

  void GetPdfScreenPPI(int* x, int* y);

  bool            m_showProgressDialog;
  wxPdfPrintData  m_pdfPrintData;

  DECLARE_DYNAMIC_CLASS(wxPdfPrinter)
};

// ----------------------------------------------------------------------------
// wxPdfPrintPreview: programmer creates an object of this class to preview a
// wxPrintout.
// ----------------------------------------------------------------------------

class WXDLLIMPEXP_PDFDOC wxPdfPrintPreview : public wxPrintPreviewBase
{
public:
  wxPdfPrintPreview(wxPrintout* printout,
                    wxPrintout* printoutForPrinting );

  wxPdfPrintPreview(wxPrintout* printout,
                    wxPrintout* printoutForPrinting,
                    wxPrintDialogData* data);

  wxPdfPrintPreview(wxPrintout* printout,
                    wxPrintout* printoutForPrinting,
                    wxPrintData* data);

  wxPdfPrintPreview(wxPrintout* printout,
                    wxPrintout* printoutForPrinting,
                    wxPdfPrintData* data);

  virtual ~wxPdfPrintPreview();

  virtual bool SetCurrentPage(int pageNum);
  virtual int GetCurrentPage() const;
  virtual void SetPrintout(wxPrintout* printout);
  virtual wxPrintout* GetPrintout() const;
  virtual wxPrintout* GetPrintoutForPrinting() const;
  virtual void SetFrame(wxFrame* frame);
  virtual void SetCanvas(wxPreviewCanvas* canvas);

  virtual wxFrame* GetFrame() const;
  virtual wxPreviewCanvas* GetCanvas() const;
  virtual bool PaintPage(wxPreviewCanvas* canvas, wxDC& dc);
#if wxCHECK_VERSION(2,9,0)
  virtual bool UpdatePageRendering();
#endif
  virtual bool DrawBlankPage(wxPreviewCanvas* canvas, wxDC& dc);
  virtual void AdjustScrollbars(wxPreviewCanvas* canvas);
  virtual bool RenderPage(int pageNum);
  virtual void SetZoom(int percent);
  virtual int GetZoom() const;

  virtual bool Print(bool interactive);
  virtual void DetermineScaling();

  virtual wxPrintDialogData& GetPrintDialogData();

  virtual int GetMaxPage() const;
  virtual int GetMinPage() const;

  virtual bool Ok() const { return IsOk(); }
  virtual bool IsOk() const;
  virtual void SetOk(bool ok);

private:
  wxPrintPreviewBase* m_pimpl;

private:
  DECLARE_CLASS(wxPdfPrintPreview)
  DECLARE_NO_COPY_CLASS(wxPdfPrintPreview);
};


class WXDLLIMPEXP_PDFDOC wxPdfPrintPreviewImpl : public wxPrintPreviewBase
{
public:
  wxPdfPrintPreviewImpl(wxPrintout* printout,
                        wxPrintout* printoutForPrinting );

  wxPdfPrintPreviewImpl(wxPrintout* printout,
                        wxPrintout* printoutForPrinting,
                        wxPrintDialogData* data);

  wxPdfPrintPreviewImpl(wxPrintout* printout,
                        wxPrintout* printoutForPrinting,
                        wxPrintData* data);

  wxPdfPrintPreviewImpl(wxPrintout* printout,
                        wxPrintout* printoutForPrinting,
                        wxPdfPrintData* data);

  virtual ~wxPdfPrintPreviewImpl();

  virtual bool Print(bool interactive);
  virtual void DetermineScaling();

#if wxCHECK_VERSION(2,9,0)

protected:
  virtual bool RenderPageIntoBitmap(wxBitmap& bmp, int pageNum);

#else

public:
  bool RenderPageIntoDCImpl(wxDC& dc, int pageNum);
  bool RenderPageIntoBitmapImpl(wxBitmap& bmp, int pageNum);
  virtual bool RenderPage(int pageNum);

#endif

private:

  void GetPdfScreenPPI(int* x, int* y);

private:

  wxPdfPrintData*  m_pdfPrintData;
  wxPdfDC*         m_pdfPreviewDC;
  wxPdfDocument*   m_pdfPreviewDoc;

  DECLARE_CLASS(wxPdfPrintPreviewImpl)
};

enum
{
  wxPDF_PRINTDIALOG_CTRLID_FILEPICKER = 30,
  wxPDF_PRINTDIALOG_CTRLID_PROTECT
};

// -------------------------------------------------------------------------
// wxPdfPrintDialog: populate wxPdfPrintData with user choices
// -------------------------------------------------------------------------

class WXDLLIMPEXP_PDFDOC wxPdfPrintDialog : public wxPrintDialogBase
{
public:

  wxPdfPrintDialog(wxWindow* parent, wxPdfPrintData* data);

  virtual ~wxPdfPrintDialog();

  void OnOK(wxCommandEvent& event);

  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();

  virtual int ShowModal();

  wxPdfDC* GetPrintDC();

  wxPrintData& GetPrintData() { return m_printDialogData.GetPrintData(); }

  wxPrintDialogData& GetPrintDialogData() { return m_printDialogData; }

  wxPdfPrintData& GetPdfPrintData() { return m_pdfPrintData; }

private:

  wxTextCtrl*         m_title;
  wxTextCtrl*         m_subject;
  wxTextCtrl*         m_author;
  wxTextCtrl*         m_creator;
  wxTextCtrl*         m_keywords;

  wxTextCtrl*         m_filepath;
  wxFilePickerCtrl*   m_filepicker;
  wxCheckBox*         m_launchViewer;

  wxTextCtrl*         m_ownerpwd;
  wxTextCtrl*         m_userpwd;
  wxTextCtrl*         m_ownerpwdconfirm;
  wxTextCtrl*         m_userpwdconfirm;

  wxChoice*           m_compat;

  wxCheckBox*         m_protect;
  wxCheckBox*         m_canprint;
  wxCheckBox*         m_canmodify;
  wxCheckBox*         m_cancopy;
  wxCheckBox*         m_canannot;
  wxCheckBox*         m_canform;
  wxCheckBox*         m_canextract;
  wxCheckBox*         m_canassemble;

  wxPrintDialogData   m_printDialogData;
  wxPdfPrintData      m_pdfPrintData;

protected:
  void Init(wxWindow* parent);

private:

  void UpdateProtectionControls();
  void OnProtectCheck(wxCommandEvent&);
  void OnFilepathChanged(wxFileDirPickerEvent&);

  DECLARE_EVENT_TABLE()
  DECLARE_DYNAMIC_CLASS(wxPdfPrintDialog)
};

// ----------------------------------------------------------------------------
// wxPdfPageSetupDialogCanvas: internal use only
// ----------------------------------------------------------------------------

class WXDLLIMPEXP_PDFDOC wxPdfPageSetupDialogCanvas : public wxWindow
{

  DECLARE_CLASS(wxPdfPageSetupDialogCanvas)

public:
  wxPdfPageSetupDialogCanvas(wxWindow *parent);

  virtual ~wxPdfPageSetupDialogCanvas();

  void OnPaint(wxPaintEvent& event);

  void UpdatePageMetrics(int paperWidth, int paperHeight, int marginLeft,
                         int marginRight, int marginTop, int marginBottom)
  {
    m_paperWidth   = paperWidth;
    m_paperHeight  = paperHeight;
    m_marginLeft   = marginLeft;
    m_marginRight  = marginRight;
    m_marginTop    = marginTop;
    m_marginBottom = marginBottom;
  }

private:

  int m_paperWidth;
  int m_paperHeight;
  int m_marginLeft;
  int m_marginRight;
  int m_marginTop;
  int m_marginBottom;

  DECLARE_EVENT_TABLE()
  DECLARE_NO_COPY_CLASS(wxPdfPageSetupDialogCanvas)
};

// ----------------------------------------------------------------------------
// wxPdfPageSetupDialog: a wxPdfDocument friendly page setup dialog
// ----------------------------------------------------------------------------

enum
{
  wxPDF_PAGEDIALOG_CTRLID_MARGINUNIT = 30,
  wxPDF_PAGEDIALOG_CTRLID_MARGINLEFT,
  wxPDF_PAGEDIALOG_CTRLID_MARGINRIGHT,
  wxPDF_PAGEDIALOG_CTRLID_MARGINTOP,
  wxPDF_PAGEDIALOG_CTRLID_MARGINBOTTOM,
  wxPDF_PAGEDIALOG_CTRLID_PAPER,
  wxPDF_PAGEDIALOG_CTRLID_ORIENTATION,
};

class WXDLLIMPEXP_PDFDOC wxPdfPageSetupDialog : public wxDialog
{
  DECLARE_CLASS(wxPdfPageSetupDialog)

public:
  wxPdfPageSetupDialog(wxWindow *parent,
                       wxPageSetupDialogData* data,
                       const wxString& title = wxEmptyString);

  virtual ~wxPdfPageSetupDialog();

  void OnOK(wxCommandEvent& event);
  void OnMarginUnit(wxCommandEvent& event);
  void OnPaperType(wxCommandEvent& event);
  void OnOrientation(wxCommandEvent& event);
  void OnMarginText(wxCommandEvent& event);

  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();

  virtual wxPageSetupDialogData& GetPageSetupDialogData();
  // support old name used in wxPageSetupDialog
  wxPageSetupDialogData& GetPageSetupData();

protected:

  wxChoice*       m_orientationChoice;
  wxChoice*       m_marginUnits;
  wxTextCtrl*     m_marginLeftText;
  wxTextCtrl*     m_marginTopText;
  wxTextCtrl*     m_marginRightText;
  wxTextCtrl*     m_marginBottomText;
  wxChoice*       m_paperTypeChoice;

private:
  int             m_marginLeft;
  int             m_marginTop;
  int             m_marginRight;
  int             m_marginBottom;
#if wxCHECK_VERSION(2,9,0)
  wxPrintOrientation m_orientation;
#else
  int             m_orientation;
#endif
  wxPaperSize     m_paperId;
  wxPaperSize     m_defaultPaperId;
  int             m_defaultUnitSelection;
  int             m_pageWidth;
  int             m_pageHeight;

  wxPdfPageSetupDialogCanvas*       m_paperCanvas;
  wxPageSetupDialogData             m_pageData;

  void Init();

  void TransferMarginsToControls();
  void TransferControlsToMargins();
  void UpdatePaperCanvas();

  DECLARE_EVENT_TABLE()
  DECLARE_NO_COPY_CLASS(wxPdfPageSetupDialog)
};


// ----------------------------------------------------------------------------
// wxPdfPreviewDC allows to return text extents from a wxPdfDC
// Internal use Only
// ----------------------------------------------------------------------------

#if wxCHECK_VERSION(2,9,0)
// 2.9.x implementation

class WXDLLIMPEXP_PDFDOC wxPdfPreviewDCImpl : public wxDCImpl
{
public:

  wxPdfPreviewDCImpl(wxDC* owner, wxDCImpl& dc, wxPdfDC* pdfdc)
    : wxDCImpl(owner), m_dc(dc)
  {
    m_pdfdc = pdfdc;
  }

  //////////////////////////////////////////////////////////////////
  // Overrides passed to wxPdfDC
  //////////////////////////////////////////////////////////////////

  wxPdfDocument* GetPdfDocument() const
  {
    return m_pdfdc->GetPdfDocument();
  }

  virtual wxRect GetPaperRect() const
  {
    int w = 0;
    int h = 0;
    m_pdfdc->GetImpl()->DoGetSize(&w, &h);
    return wxRect(0,0,w,h);
  }

  virtual int GetResolution() const { return m_pdfdc->GetResolution(); }

  virtual bool CanGetTextExtent() const { return m_pdfdc->CanGetTextExtent(); }

  virtual wxCoord GetCharHeight() const { return m_pdfdc->GetCharHeight(); }
  virtual wxCoord GetCharWidth() const { return m_pdfdc->GetCharWidth(); }

  virtual void DoGetFontMetrics(int* height,
                                int* ascent,
                                int* descent,
                                int* internalLeading,
                                int* externalLeading,
                                int* averageWidth) const
  {
    m_pdfdc->GetImpl()->DoGetFontMetrics(height, ascent, descent, internalLeading, externalLeading, averageWidth);
  }

  virtual void DoGetTextExtent(const wxString& string,
                               wxCoord* x, wxCoord* y,
                               wxCoord* descent = NULL,
                               wxCoord* externalLeading = NULL,
                               const wxFont* theFont = NULL) const
  {
    m_pdfdc->GetTextExtent(string, x, y, descent, externalLeading, theFont);
  }

  virtual bool DoGetPartialTextExtents(const wxString& text, wxArrayInt& widths) const
  {
    return m_pdfdc->GetPartialTextExtents(text, widths);
  }

  virtual void GetMultiLineTextExtent(const wxString& string,
                                      wxCoord* width,
                                      wxCoord* height,
                                      wxCoord* heightLine = NULL,
                                      const wxFont* font = NULL) const
  {
    m_pdfdc->GetMultiLineTextExtent(string, width, height, heightLine, font);
  }

  virtual wxSize GetPPI() const { return m_pdfdc->GetPPI(); }

private:

  void UpdateBoundingBox() const
  {
    ((wxDCImpl*)this)->wxDCImpl::CalcBoundingBox(m_dc.MinX(), m_dc.MinY());
    ((wxDCImpl*)this)->wxDCImpl::CalcBoundingBox(m_dc.MaxX(), m_dc.MaxY());
  }

public:

  //////////////////////////////////////////////////////////////////
  // Overrides passed to wxMemory DC
  //////////////////////////////////////////////////////////////////

  virtual void DoGetSize(int* w, int* h) const
  {
    m_dc.DoGetSize(w,h);
  }

  virtual void DoGetSizeMM(int* w, int* h) const
  {
    m_dc.DoGetSizeMM(w,h);
  }

  virtual bool IsOk() const { return m_dc.IsOk(); }

  // wxDCBase operations
  virtual bool CanDrawBitmap() const { return m_dc.CanDrawBitmap(); }

  virtual void Clear() { m_dc.Clear(); }

  virtual int GetDepth() const { return m_dc.GetDepth(); }

  virtual void CalcBoundingBox(wxCoord x, wxCoord y)
  {
    m_dc.CalcBoundingBox( x, y);
    UpdateBoundingBox();
  }

  virtual void SetFont(const wxFont& font)
  {
    m_dc.SetFont(font);
    m_pdfdc->SetFont(font);
  }

  virtual const wxFont& GetFont() const { return m_dc.GetFont(); }

  virtual void SetPen(const wxPen& pen) { m_dc.SetPen(pen); }
  virtual const wxPen& GetPen() const { return m_dc.GetPen(); }
  virtual void SetBrush(const wxBrush& brush) { m_dc.SetBrush(brush); }
  virtual const wxBrush& GetBrush() const { return m_dc.GetBrush(); }
  virtual void SetBackground(const wxBrush& brush) { m_dc.SetBackground(brush); }
  virtual const wxBrush& GetBackground() const { return m_dc.GetBackground(); }
  virtual void SetBackgroundMode(int mode){ m_dc.SetBackgroundMode(mode); }
  virtual int GetBackgroundMode() const { return m_dc.GetBackgroundMode(); }

  virtual void SetTextForeground(const wxColour& colour) { m_dc.SetTextForeground(colour); }
  virtual const wxColour& GetTextForeground() const { return m_dc.GetTextForeground(); }

  virtual void SetTextBackground(const wxColour& colour) { m_dc.SetTextBackground(colour); }
  virtual const wxColour& GetTextBackground() const { return m_dc.GetTextBackground(); }

#if wxUSE_PALETTE
  virtual void SetPalette(const wxPalette& palette) { m_dc.SetPalette(palette); }
#endif // wxUSE_PALETTE

  virtual void InheritAttributes(wxWindow* win) { m_dc.InheritAttributes(win); }

  virtual void SetLogicalFunction(wxRasterOperationMode function) { m_dc.SetLogicalFunction(function); }
  virtual wxRasterOperationMode GetLogicalFunction() const { return m_dc.GetLogicalFunction(); }

  virtual void DoSetClippingRegion(wxCoord x, wxCoord y, wxCoord w, wxCoord h)
  {
    m_dc.DoSetClippingRegion(x,y,w,h);
    UpdateBoundingBox();
  }

  virtual void DoSetDeviceClippingRegion(const wxRegion& region)
  {
    m_dc.DoSetDeviceClippingRegion(region);
    UpdateBoundingBox();
  }

  virtual void DoGetClippingBox(wxCoord* x, wxCoord* y, wxCoord* w, wxCoord* h) const
  {
    m_dc.DoGetClippingBox(x, y, w, h);
    UpdateBoundingBox();
  }

  virtual void DestroyClippingRegion()
  {
    m_dc.DestroyClippingRegion();
    UpdateBoundingBox();
  }

  virtual wxCoord DeviceToLogicalX(wxCoord x) const { return m_dc.DeviceToLogicalX(x); }
  virtual wxCoord DeviceToLogicalY(wxCoord y) const { return m_dc.DeviceToLogicalY(y); }
  virtual wxCoord DeviceToLogicalXRel(wxCoord x) const { return m_dc.DeviceToLogicalXRel(x); }
  virtual wxCoord DeviceToLogicalYRel(wxCoord y) const { return m_dc.DeviceToLogicalYRel(y); }
  virtual wxCoord LogicalToDeviceX(wxCoord x) const { return m_dc.LogicalToDeviceX(x); }
  virtual wxCoord LogicalToDeviceY(wxCoord y) const { return m_dc.LogicalToDeviceY(y); }
  virtual wxCoord LogicalToDeviceXRel(wxCoord x) const { return m_dc.LogicalToDeviceXRel(x); }
  virtual wxCoord LogicalToDeviceYRel(wxCoord y) const { return m_dc.LogicalToDeviceYRel(y); }

  virtual void SetMapMode(wxMappingMode mode) { m_dc.SetMapMode(mode); }
  virtual wxMappingMode GetMapMode() const { return m_dc.GetMapMode(); }

  virtual void SetUserScale(double x, double y) { m_dc.SetUserScale(x,y); }
  virtual void GetUserScale(double* x, double* y) const { m_dc.GetUserScale(x, y); }

  virtual void SetLogicalScale(double x, double y) { m_dc.SetLogicalScale(x,y); }
  virtual void GetLogicalScale(double* x, double* y) const { m_dc.GetLogicalScale(x,y); }

  virtual void SetLogicalOrigin(wxCoord x, wxCoord y) { m_dc.SetLogicalOrigin(x,y); }
  virtual void DoGetLogicalOrigin(wxCoord* x, wxCoord* y) const { m_dc.DoGetLogicalOrigin(x,y); }

  virtual void SetDeviceOrigin(wxCoord x, wxCoord y) { m_dc.SetDeviceOrigin(x,y); }
  virtual void DoGetDeviceOrigin(wxCoord* x, wxCoord* y) const { m_dc.DoGetDeviceOrigin(x,y); }

  virtual void SetDeviceLocalOrigin( wxCoord x, wxCoord y ) { m_dc.SetDeviceLocalOrigin(x,y); }
  virtual void ComputeScaleAndOrigin() { m_dc.ComputeScaleAndOrigin(); }
  virtual void SetAxisOrientation(bool x, bool y) { m_dc.SetAxisOrientation(x,y); }

  virtual bool DoFloodFill(wxCoord x, wxCoord y, const wxColour& col,
                           wxFloodFillStyle style = wxFLOOD_SURFACE)
  {
    bool rval = m_dc.DoFloodFill(x, y, col, style);
    UpdateBoundingBox();
    return rval;
  }

  virtual void DoGradientFillLinear(const wxRect& rect,
                                    const wxColour& initialColour,
                                    const wxColour& destColour,
                                    wxDirection nDirection = wxEAST)
  {
    m_dc.DoGradientFillLinear(rect, initialColour, destColour, nDirection);
    UpdateBoundingBox();
  }

  virtual void DoGradientFillConcentric(const wxRect& rect,
                                        const wxColour& initialColour,
                                        const wxColour& destColour,
                                        const wxPoint& circleCenter)
  {
    m_dc.DoGradientFillConcentric(rect, initialColour, destColour, circleCenter);
    UpdateBoundingBox();
  }

  virtual bool DoGetPixel(wxCoord x, wxCoord y, wxColour* col) const
  {
    bool rval = m_dc.DoGetPixel(x, y, col);
    UpdateBoundingBox();
    return rval;
  }

  virtual void DoDrawPoint(wxCoord x, wxCoord y)
  {
    m_dc.DoDrawPoint(x,y);
    UpdateBoundingBox();
  }

  virtual void DoDrawLine(wxCoord x1, wxCoord y1, wxCoord x2, wxCoord y2)
  {
    m_dc.DoDrawLine(x1,y1,x2,y2);
    UpdateBoundingBox();
  }

  virtual void DoDrawArc(wxCoord x1, wxCoord y1,
                         wxCoord x2, wxCoord y2,
                         wxCoord xc, wxCoord yc)
  {
    m_dc.DoDrawArc(x1,y1, x2,y2, xc, yc);
    UpdateBoundingBox();
  }

  virtual void DoDrawCheckMark(wxCoord x, wxCoord y,
                               wxCoord w, wxCoord h)
  {
    m_dc.DoDrawCheckMark(x,y,w,h);
    UpdateBoundingBox();
  }

  virtual void DoDrawEllipticArc(wxCoord x, wxCoord y, wxCoord w, wxCoord h,
                                 double sa, double ea)
  {
    m_dc.DoDrawEllipticArc(x,y,w,h,sa, ea);
    UpdateBoundingBox();
  }

  virtual void DoDrawRectangle(wxCoord x, wxCoord y, wxCoord w, wxCoord h)
  {
    m_dc.DoDrawRectangle(x,y,w,h);
    UpdateBoundingBox();
  }

  virtual void DoDrawRoundedRectangle(wxCoord x, wxCoord y,
                                      wxCoord w, wxCoord h,
                                      double radius)
  {
    m_dc.DoDrawRoundedRectangle(x,y,w,h,radius);
    UpdateBoundingBox();
  }

  virtual void DoDrawEllipse(wxCoord x, wxCoord y, wxCoord w, wxCoord h)
  {
    m_dc.DoDrawEllipse(x,y,w,h);
    UpdateBoundingBox();
  }

  virtual void DoCrossHair(wxCoord x, wxCoord y)
  {
    m_dc.DoCrossHair(x,y);
    UpdateBoundingBox();
  }

  virtual void DoDrawIcon(const wxIcon& icon, wxCoord x, wxCoord y)
  {
    m_dc.DoDrawIcon(icon, x,y);
    UpdateBoundingBox();
  }

  virtual void DoDrawBitmap(const wxBitmap &bmp, wxCoord x, wxCoord y,
                            bool useMask = false)
  {
    m_dc.DoDrawBitmap(bmp, x,y, useMask);
    UpdateBoundingBox();
  }

  virtual void DoDrawText(const wxString& text, wxCoord x, wxCoord y)
  {
    m_dc.DoDrawText(text, x, y);
    UpdateBoundingBox();
  }

  virtual void DoDrawRotatedText(const wxString& text,
                                 wxCoord x, wxCoord y, double angle)
  {
    m_dc.DoDrawRotatedText(text, x, y, angle);
    UpdateBoundingBox();
  }

  virtual bool DoBlit(wxCoord xdest, wxCoord ydest,
                      wxCoord w, wxCoord h,
                      wxDC* source, wxCoord xsrc, wxCoord ysrc,
                      wxRasterOperationMode rop = wxCOPY,
                      bool useMask = false,
                      wxCoord xsrcMask = wxDefaultCoord, wxCoord ysrcMask = wxDefaultCoord)
  {
    bool rval = m_dc.DoBlit(xdest, ydest, w, h, source, xsrc, ysrc, rop, useMask, xsrcMask, ysrcMask);
    UpdateBoundingBox();
    return rval;
  }

  virtual bool DoStretchBlit(wxCoord xdest, wxCoord ydest,
                             wxCoord dstWidth, wxCoord dstHeight,
                             wxDC* source,
                             wxCoord xsrc, wxCoord ysrc,
                             wxCoord srcWidth, wxCoord srcHeight,
                             wxRasterOperationMode rop = wxCOPY,
                             bool useMask = false,
                             wxCoord xsrcMask = wxDefaultCoord,
                             wxCoord ysrcMask = wxDefaultCoord)
  {
    bool rval = m_dc.DoStretchBlit(xdest, ydest, dstWidth, dstHeight, source, xsrc, ysrc,
                                   srcWidth, srcHeight, rop, useMask, xsrcMask, ysrcMask);
    UpdateBoundingBox();
    return rval;
  }

#if wxCHECK_VERSION(2,9,5)
  virtual void DoDrawLines(int n, const wxPoint points[],
                           wxCoord xoffset, wxCoord yoffset)
#else
  virtual void DoDrawLines(int n, wxPoint points[],
                           wxCoord xoffset, wxCoord yoffset)
#endif // wxCHECK_VERSION
  {
    m_dc.DoDrawLines(n, points,xoffset, yoffset);
    UpdateBoundingBox();
  }

#if wxCHECK_VERSION(2,9,5)
  virtual void DoDrawPolygon(int n, const wxPoint points[],
                             wxCoord xoffset, wxCoord yoffset,
                             wxPolygonFillMode fillStyle = wxODDEVEN_RULE)
#else
  virtual void DoDrawPolygon(int n, wxPoint points[],
                             wxCoord xoffset, wxCoord yoffset,
                             wxPolygonFillMode fillStyle = wxODDEVEN_RULE)
#endif // wxCHECK_VERSION
  {
    m_dc.DoDrawPolygon(n, points, xoffset, yoffset, fillStyle);
    UpdateBoundingBox();
  }

  virtual void DoSetClippingRegionAsRegion(const wxRegion& region)
  {
    wxCoord x, y, w, h;
    region.GetBox(x, y, w, h);
    m_dc.DoSetClippingRegion(x, y, w, h);
    UpdateBoundingBox();
  }

private:
  wxDCImpl&    m_dc;
  wxPdfDC*  m_pdfdc;

  wxDECLARE_NO_COPY_CLASS(wxPdfPreviewDCImpl);
};

class WXDLLIMPEXP_PDFDOC wxPdfPreviewDC : public wxDC
{
public:
  wxPdfPreviewDC(wxDC& dc, wxPdfDC* pdfdc)
    : wxDC(new wxPdfPreviewDCImpl(this, *dc.GetImpl(), pdfdc)) { }

private:

    wxDECLARE_NO_COPY_CLASS(wxPdfPreviewDC);
};

#else

/////////////////////////////////////////////////////////////////
// 2.8.x wxPdfPreview implementation
/////////////////////////////////////////////////////////////////

class WXDLLIMPEXP_PDFDOC wxPdfPreviewDC : public wxDC
{
public:
  wxPdfPreviewDC(wxDC& dc, wxPdfDC* pdfdc)
    : m_dc((wxPdfPreviewDC&)dc)
  {
    m_pdfdc = pdfdc;
  }

  //////////////////////////////////////////////////////////////////
  // Overrides passed to wxPdfDC
  //////////////////////////////////////////////////////////////////

  wxPdfDocument* GetPdfDocument() const
  {
    return m_pdfdc->GetPdfDocument();
  }

  virtual wxRect GetPaperRect() const
  {
    int w = 0;
    int h = 0;
    m_pdfdc->GetSize(&w, &h);
    return wxRect(0, 0, w, h);
  }

  virtual int GetResolution() const { return m_pdfdc->GetResolution(); }

  virtual bool CanGetTextExtent() const { return m_pdfdc->CanGetTextExtent(); }

  virtual wxCoord GetCharHeight() const { return m_pdfdc->GetCharHeight(); }
  virtual wxCoord GetCharWidth() const { return m_pdfdc->GetCharWidth(); }

protected:

  virtual void DoGetTextExtent(const wxString& string,
                               wxCoord* x, wxCoord* y,
                               wxCoord* descent = NULL,
                               wxCoord* externalLeading = NULL,
                               wxFont* theFont = NULL) const
  {
    m_pdfdc->GetTextExtent(string, x, y, descent, externalLeading, theFont);
  }

  virtual bool DoGetPartialTextExtents(const wxString& text, wxArrayInt& widths) const
  {
    return m_pdfdc->GetPartialTextExtents(text, widths);
  }

public:

  virtual void GetMultiLineTextExtent(const wxString& string,
                                      wxCoord* width,
                                      wxCoord* height,
                                      wxCoord* heightLine = NULL,
                                      wxFont* font = NULL) const
  {
    m_pdfdc->GetMultiLineTextExtent(string, width, height, heightLine, font);
  }

private:

  void UpdateBoundingBox() const
  {
    ((wxDC*)this)->wxDC::CalcBoundingBox( m_dc.MinX(), m_dc.MinY());
    ((wxDC*)this)->wxDC::CalcBoundingBox( m_dc.MaxX(), m_dc.MaxY());
  }

public:

  //////////////////////////////////////////////////////////////////
  // Overrides passed to wxMemory DC
  //////////////////////////////////////////////////////////////////

  virtual wxSize GetPPI() const { return m_dc.GetPPI(); }

  // wxDCBase operations
  virtual bool CanDrawBitmap() const { return m_dc.CanDrawBitmap(); }
  virtual void Clear() { m_dc.Clear(); }

  virtual int GetDepth() const { return m_dc.GetDepth(); }
  virtual bool IsOk() const { return m_dc.IsOk(); }

  virtual void CalcBoundingBox(wxCoord x, wxCoord y)
  {
    m_dc.CalcBoundingBox(x, y);
    UpdateBoundingBox();
  }

  virtual void SetFont(const wxFont& font)
  {
    m_dc.SetFont(font);
    m_pdfdc->SetFont(font);
  }
  virtual const wxFont& GetFont() const { return m_dc.GetFont(); }

  virtual void SetPen(const wxPen& pen) { m_dc.SetPen(pen); }
  virtual const wxPen& GetPen() const { return m_dc.GetPen(); }
  virtual void SetBrush(const wxBrush& brush) { m_dc.SetBrush(brush); }
  virtual const wxBrush& GetBrush() const { return m_dc.GetBrush(); }
  virtual void SetBackground(const wxBrush& brush) { m_dc.SetBackground(brush); }
  virtual const wxBrush& GetBackground() const { return m_dc.GetBackground(); }
  virtual void SetBackgroundMode(int mode){ m_dc.SetBackgroundMode(mode); }
  virtual int GetBackgroundMode() const { return m_dc.GetBackgroundMode(); }

  virtual void SetTextForeground(const wxColour& colour) { m_dc.SetTextForeground(colour); }
  virtual const wxColour& GetTextForeground() const { return m_dc.GetTextForeground(); }

  virtual void SetTextBackground(const wxColour& colour) { m_dc.SetTextBackground(colour); }
  virtual const wxColour& GetTextBackground() const { return m_dc.GetTextBackground(); }

#if wxUSE_PALETTE
  virtual void SetPalette(const wxPalette& palette) { m_dc.SetPalette(palette); }
#endif // wxUSE_PALETTE

  virtual void DestroyClippingRegion()
  {
    m_dc.DestroyClippingRegion();
    UpdateBoundingBox();
  }

  virtual wxCoord DeviceToLogicalX(wxCoord x) const { return m_dc.DeviceToLogicalX(x); }
  virtual wxCoord DeviceToLogicalY(wxCoord y) const { return m_dc.DeviceToLogicalY(y); }
  virtual wxCoord DeviceToLogicalXRel(wxCoord x) const { return m_dc.DeviceToLogicalXRel(x); }
  virtual wxCoord DeviceToLogicalYRel(wxCoord y) const { return m_dc.DeviceToLogicalYRel(y); }
  virtual wxCoord LogicalToDeviceX(wxCoord x) const { return m_dc.LogicalToDeviceX(x); }
  virtual wxCoord LogicalToDeviceY(wxCoord y) const { return m_dc.LogicalToDeviceY(y); }
  virtual wxCoord LogicalToDeviceXRel(wxCoord x) const { return m_dc.LogicalToDeviceXRel(x); }
  virtual wxCoord LogicalToDeviceYRel(wxCoord y) const { return m_dc.LogicalToDeviceYRel(y); }

  virtual void SetMapMode(int mode) { m_dc.SetMapMode(mode); }
  virtual int GetMapMode() const { return m_dc.GetMapMode(); }

  virtual void SetUserScale(double x, double y) { m_dc.SetUserScale(x,y); }
  virtual void GetUserScale(double* x, double* y) const { m_dc.GetUserScale(x, y); }

  virtual void SetLogicalScale(double x, double y) { m_dc.SetLogicalScale(x,y); }
  virtual void GetLogicalScale(double* x, double* y) const { m_dc.GetLogicalScale(x,y); }

  virtual void SetLogicalOrigin(wxCoord x, wxCoord y) { m_dc.SetLogicalOrigin(x,y); }
  virtual void DoGetLogicalOrigin(wxCoord* x, wxCoord* y) const { m_dc.DoGetLogicalOrigin(x,y); }

  virtual void SetDeviceOrigin(wxCoord x, wxCoord y) { m_dc.SetDeviceOrigin(x,y); }
  virtual void DoGetDeviceOrigin(wxCoord* x, wxCoord* y) const { m_dc.DoGetDeviceOrigin(x,y); }

  virtual void SetDeviceLocalOrigin( wxCoord x, wxCoord y ) { m_dc.SetDeviceLocalOrigin(x,y); }
  virtual void ComputeScaleAndOrigin() { m_dc.ComputeScaleAndOrigin(); }
  virtual void SetAxisOrientation(bool x, bool y) { m_dc.SetAxisOrientation(x,y); }

  virtual void SetLogicalFunction(int function) { m_dc.SetLogicalFunction(function); }

protected:

  // wxDCBase functions
  virtual bool DoFloodFill(wxCoord x, wxCoord y, const wxColour& col,
                           int style = wxFLOOD_SURFACE)
  {
    bool rval = m_dc.DoFloodFill(x,y, col, style);
    UpdateBoundingBox();
    return rval;
  }

  virtual void DoGradientFillLinear(const wxRect& rect,
                                    const wxColour& initialColour,
                                    const wxColour& destColour,
                                    wxDirection nDirection = wxEAST)
  {
    m_dc.DoGradientFillLinear(rect, initialColour, destColour, nDirection);
    UpdateBoundingBox();
  }

  virtual void DoGradientFillConcentric(const wxRect& rect,
                                        const wxColour& initialColour,
                                        const wxColour& destColour,
                                        const wxPoint& circleCenter)
  {
    m_dc.DoGradientFillConcentric(rect, initialColour, destColour, circleCenter);
    UpdateBoundingBox();
  }

  virtual bool DoGetPixel(wxCoord x, wxCoord y, wxColour* col) const
  {
    bool rval = m_dc.DoGetPixel(x, y, col);
    UpdateBoundingBox();
    return rval;
  }

  virtual void DoDrawPoint(wxCoord x, wxCoord y)
  {
    m_dc.DoDrawPoint(x,y);
    UpdateBoundingBox();
  }

  virtual void DoDrawLine(wxCoord x1, wxCoord y1, wxCoord x2, wxCoord y2)
  {
    m_dc.DoDrawLine(x1,y1,x2,y2);
    UpdateBoundingBox();
  }

  virtual void DoDrawArc(wxCoord x1, wxCoord y1,
                         wxCoord x2, wxCoord y2,
                         wxCoord xc, wxCoord yc)
  {
    m_dc.DoDrawArc(x1, y1, x2, y2, xc, yc);
    UpdateBoundingBox();
  }

  virtual void DoDrawCheckMark(wxCoord x, wxCoord y,
                               wxCoord w, wxCoord h)
  {
    m_dc.DoDrawCheckMark(x,y,w,h);
    UpdateBoundingBox();
  }

  virtual void DoDrawEllipticArc(wxCoord x, wxCoord y, wxCoord w, wxCoord h,
                                 double sa, double ea)
  {
    m_dc.DoDrawEllipticArc(x, y, w, h, sa, ea);
    UpdateBoundingBox();
  }

  virtual void DoDrawRectangle(wxCoord x, wxCoord y, wxCoord w, wxCoord h)
  {
    m_dc.DoDrawRectangle(x, y, w, h);
    UpdateBoundingBox();
  }

  virtual void DoDrawRoundedRectangle(wxCoord x, wxCoord y,
                                      wxCoord w, wxCoord h,
                                      double radius)
  {
    m_dc.DoDrawRoundedRectangle(x, y, w, h, radius);
    UpdateBoundingBox();
  }

  virtual void DoDrawEllipse(wxCoord x, wxCoord y, wxCoord w, wxCoord h)
  {
    m_dc.DoDrawEllipse(x, y, w, h);
    UpdateBoundingBox();
  }

  virtual void DoCrossHair(wxCoord x, wxCoord y)
  {
    m_dc.DoCrossHair(x, y);
    UpdateBoundingBox();
  }

  virtual void DoDrawIcon(const wxIcon& icon, wxCoord x, wxCoord y)
  {
    m_dc.DoDrawIcon(icon, x, y);
    UpdateBoundingBox();
  }

  virtual void DoDrawBitmap(const wxBitmap &bmp, wxCoord x, wxCoord y,
                            bool useMask = false)
  {
    m_dc.DoDrawBitmap(bmp, x, y, useMask);
    UpdateBoundingBox();
  }

  virtual void DoDrawText(const wxString& text, wxCoord x, wxCoord y)
  {
    m_dc.DoDrawText(text, x, y);
    UpdateBoundingBox();
  }

  virtual void DoDrawRotatedText(const wxString& text,
                                 wxCoord x, wxCoord y, double angle)
  {
    m_dc.DoDrawRotatedText(text, x, y, angle);
    UpdateBoundingBox();
  }

  virtual bool DoBlit(wxCoord xdest, wxCoord ydest,
                      wxCoord w, wxCoord h,
                      wxDC* source, wxCoord xsrc, wxCoord ysrc,
                      int rop = wxCOPY,
                      bool useMask = false,
                      wxCoord xsrcMask = wxDefaultCoord, wxCoord ysrcMask = wxDefaultCoord)
  {
    bool rval = m_dc.DoBlit(xdest, ydest, w, h, source, xsrc, ysrc, rop, useMask, xsrcMask, ysrcMask);
    UpdateBoundingBox();
    return rval;
  }

  virtual void DoGetSize(int* w, int* h) const
  {
    m_dc.DoGetSize(w,h);
  }

  virtual void DoGetSizeMM(int* w, int* h) const
  {
    m_dc.DoGetSizeMM(w,h);
  }

  virtual void DoDrawLines(int n, wxPoint points[],
                           wxCoord xoffset, wxCoord yoffset)
  {
    m_dc.DoDrawLines(n, points,xoffset, yoffset);
    UpdateBoundingBox();
  }

  virtual void DoDrawPolygon(int n, wxPoint points[],
                             wxCoord xoffset, wxCoord yoffset,
                             int fillStyle = wxODDEVEN_RULE)
  {
    m_dc.DoDrawPolygon(n, points, xoffset, yoffset, fillStyle);
    UpdateBoundingBox();
  }

  virtual void DoSetClippingRegion(wxCoord x, wxCoord y,
                                   wxCoord w, wxCoord h)
  {
    m_dc.DoSetClippingRegion(x,y,w,h);
  }

  virtual void DoSetClippingRegionAsRegion(const wxRegion& region)
  {
    wxCoord x, y, w, h;
    region.GetBox(x, y, w, h);
    m_dc.DoSetClippingRegion(x, y, w, h);
    UpdateBoundingBox();
  }

  virtual void DoSetDeviceClippingRegion(const wxRegion& region)
  {
    m_dc.DoSetDeviceClippingRegion(region);
    UpdateBoundingBox();
  }

  virtual void DoGetClippingBox(wxCoord* x, wxCoord* y,
                                wxCoord* w, wxCoord* h) const
  {
    m_dc.DoGetClippingBox(x, y, w, h);
    UpdateBoundingBox();
  }

private:
  wxPdfPreviewDC&  m_dc;
  wxPdfDC*         m_pdfdc;

  DECLARE_NO_COPY_CLASS(wxPdfPreviewDC);
};

#endif   // end of 2.8.x wxPdfPreviewDC

#endif //  _PDF_PRINTING_H_



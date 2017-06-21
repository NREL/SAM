///////////////////////////////////////////////////////////////////////////////
// Name:        pdfdocument.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2005-08-04
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfdocument.h Interface of the wxPdfDocument class

#ifndef _PDF_DOCUMENT_H_
#define _PDF_DOCUMENT_H_

// wxWidgets headers
#include <wx/dynarray.h>
#include <wx/hashmap.h>
#include <wx/mstream.h>
#include <wx/string.h>
#include <wx/xml/xml.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdfarraytypes.h"
#include "wex/pdf/pdfarraydouble.h"
#include "wex/pdf/pdfcolour.h"
#include "wex/pdf/pdfencrypt.h"
#include "wex/pdf/pdfimage.h"
#include "wex/pdf/pdflinestyle.h"
#include "wex/pdf/pdflinks.h"
#include "wex/pdf/pdfproperties.h"

#define wxPDF_PRODUCER       wxT("wxPdfDocument 0.9.5")

#define wxPDF_EPSILON        1e-6

class WXDLLIMPEXP_FWD_PDFDOC wxPdfCoonsPatchMesh;

class WXDLLIMPEXP_FWD_PDFDOC wxPdfFont;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfFontDescription;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfFontDetails;

class WXDLLIMPEXP_FWD_PDFDOC wxPdfExtGState;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfGradient;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfInfo;

class WXDLLIMPEXP_FWD_PDFDOC wxPdfShape;

class WXDLLIMPEXP_FWD_PDFDOC wxPdfSpotColour;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfPattern;

class WXDLLIMPEXP_FWD_PDFDOC wxPdfCellContext;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfTable;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfIndirectObject;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfAnnotationWidget;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfTemplate;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfParser;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfObject;

class WXDLLIMPEXP_FWD_PDFDOC wxPdfArrayLayer;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfOcg;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfLayer;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfLayerMembership;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfLayerGroup;

/// Hashmap class for offset values
WX_DECLARE_HASH_MAP_WITH_DECL(long, int, wxIntegerHash, wxIntegerEqual, wxPdfOffsetHashMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for document pages
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxMemoryOutputStream*, wxIntegerHash, wxIntegerEqual, wxPdfPageHashMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for document pages
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxSize, wxIntegerHash, wxIntegerEqual, wxPdfPageSizeMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for boolean values
WX_DECLARE_HASH_MAP_WITH_DECL(long, bool, wxIntegerHash, wxIntegerEqual, wxPdfBoolHashMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for double values
WX_DECLARE_HASH_MAP_WITH_DECL(long, double, wxIntegerHash, wxIntegerEqual, wxPdfDoubleHashMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for document links
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxPdfLink*, wxIntegerHash, wxIntegerEqual, wxPdfLinkHashMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for page links
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxArrayPtrVoid*, wxIntegerHash, wxIntegerEqual, wxPdfPageLinksMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for document annotations
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxArrayPtrVoid*, wxIntegerHash, wxIntegerEqual, wxPdfAnnotationsMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for document annotations
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxArrayPtrVoid*, wxIntegerHash, wxIntegerEqual, wxPdfFormAnnotsMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for form fields
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxPdfIndirectObject*, wxIntegerHash, wxIntegerEqual, wxPdfFormFieldsMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for templates
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxPdfTemplate*, wxIntegerHash, wxIntegerEqual, wxPdfTemplatesMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for font encoding differences
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxString*, wxIntegerHash, wxIntegerEqual, wxPdfStringHashMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for extended graphics states
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxPdfExtGState*, wxIntegerHash, wxIntegerEqual, wxPdfExtGStateMap, class WXDLLIMPEXP_PDFDOC);
WX_DECLARE_HASH_MAP_WITH_DECL(long, int, wxIntegerHash, wxIntegerEqual, wxPdfExtGSLookupMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for gradients
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxPdfGradient*, wxIntegerHash, wxIntegerEqual, wxPdfGradientMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for optional content groups
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxPdfOcg*, wxIntegerHash, wxIntegerEqual, wxPdfOcgMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for optional content groups
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxPdfLayerGroup*, wxIntegerHash, wxIntegerEqual, wxPdfLayerRGMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for file attachments
WX_DECLARE_HASH_MAP_WITH_DECL(long, wxArrayString*, wxIntegerHash, wxIntegerEqual, wxPdfAttachmentMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for core fonts
WX_DECLARE_STRING_HASH_MAP_WITH_DECL(int, wxPdfCoreFontMap, class WXDLLIMPEXP_PDFDOC);

/// Hashmap class for core fonts
WX_DECLARE_STRING_HASH_MAP_WITH_DECL(int, wxPdfNamedLinksMap, class WXDLLIMPEXP_PDFDOC);

/// Hash map class for used/embedded fonts
WX_DECLARE_STRING_HASH_MAP_WITH_DECL(wxPdfFontDetails*, wxPdfFontHashMap, class WXDLLIMPEXP_PDFDOC);

/// Hash map class for embedded images
WX_DECLARE_STRING_HASH_MAP_WITH_DECL(wxPdfImage*, wxPdfImageHashMap, class WXDLLIMPEXP_PDFDOC);

/// Hash map class for spot colours
WX_DECLARE_STRING_HASH_MAP_WITH_DECL(wxPdfSpotColour*, wxPdfSpotColourMap, class WXDLLIMPEXP_PDFDOC);

/// Hash map class for spot colours
WX_DECLARE_STRING_HASH_MAP_WITH_DECL(wxPdfPattern*, wxPdfPatternMap, class WXDLLIMPEXP_PDFDOC);

/// Hash map class for spot colours
WX_DECLARE_STRING_HASH_MAP_WITH_DECL(wxPdfIndirectObject*, wxPdfRadioGroupMap, class WXDLLIMPEXP_PDFDOC);

/// Hash map class for parsers
WX_DECLARE_STRING_HASH_MAP_WITH_DECL(wxPdfParser*, wxPdfParserMap, class WXDLLIMPEXP_PDFDOC);

/// Class representing a PDF document.
#if WXPDFDOC_INHERIT_WXOBJECT
class WXDLLIMPEXP_PDFDOC wxPdfDocument : public wxObject
#else
class WXDLLIMPEXP_PDFDOC wxPdfDocument
#endif
{
public:
  /// Constructor
  /**
  * \param orientation Defines the default page orientation. Possible values are:
  *   \li wxPORTRAIT portrait layout (default)
  *   \li wxLANDSCAPE landscape layout
  *
  * \param unit Defines the user units. Possible values are:
  *   \li "mm" millimeter (1 mm = 0.0394 in = 2.833 pt = 0.1 cm) (default)
  *   \li "cm" centimeter (1 cm = 0.394 in = 28.33 pt = 10 mm)
  *   \li "pt" points (1 pt = 1/72 in = 0.0353 cm = 0.353 mm)
  *   \li "in" inch   (1 in = 72 pt = 2.54 cm = 25.4 mm)
  * \param format Defines the page format. All known wxWidgets paper types are allowed. (Default: wxPAPER_A4)
  */
  wxPdfDocument(int orientation = wxPORTRAIT, 
                const wxString& unit = wxString(wxT("mm")), 
                wxPaperSize format = wxPAPER_A4);

  wxPdfDocument(int orientation, double pageWidth, double pageHeight,
                const wxString& unit = wxString(wxT("mm")));

  virtual ~wxPdfDocument();

  /// Set permissions as well as user and owner passwords.
  /**
  * \param permissions flag indicating permissions.
  *                    Flags from the following list may be combined as needed
  *                    If a value is present it means that the permission is granted
  * \param userPassword user password if applicable.
  *                     If a user password is set, user will be prompted before document is opened
  * \param ownerPassword owner password.if applicable
  *                      If an owner password is set, the document can be opened
  *                      in privilege mode with no restriction if that password is entered
  * \param encryptionMethod selects the encryption method. Possible values are:
  *   \li wxPDF_ENCRYPTION_RC4V1 RC4 method, version 1, with 40 bit encryption key (default)
  *   \li wxPDF_ENCRYPTION_RC4V2 RC4 method, version 2, with 40..128 bit encryption key
  *   \li wxPDF_ENCRYPTION_AESV2 AES method, with 128 bit encryption key
  * \param keyLength Length of the key used for encryption (Default: 0)
  *                  The default value selects the standard encryption method revision 2 with a key length of 40 bits.
  *                  Specifying a value > 0 selects the standard encryption method revision 3 with the given key length,
  *                  the key length has to be in the range 40..128 and has to be dividable by 8.
  *                  The key length is adjusted accordingly if these conditions are not met.
  * NOTE: Adobe Reader supports only 40- and 128-bit encryption keys.
  */
  virtual void SetProtection(int permissions,
                             const wxString& userPassword = wxEmptyString,
                             const wxString& ownerPassword = wxEmptyString,
                             wxPdfEncryptionMethod encryptionMethod = wxPDF_ENCRYPTION_RC4V1,
                             int keyLength = 0);

  /// Set the image scale.
  /**
  * \param[in] scale image scale.
  */
  virtual void SetImageScale(double scale);

  /// Returns the image scale.
  /**
  * \return image scale.
  */
  virtual double GetImageScale();

  /// Returns the page width in units.
  /**
  * \return int page width.
  */
  virtual double GetPageWidth();
  
  /// Returns the page height in units.
  /**
  * \return int page height.
  */
  virtual double GetPageHeight();
  
  /// Returns the page break margin.
  /**
  * \return int page break margin.
  */
  virtual double GetBreakMargin();
  
  /// Returns the scale factor (number of points in user unit).
  /**
  * \return int scale factor.
  */
  virtual double GetScaleFactor();

  /// Defines the left, top and right margins.
  /**
  * By default, they equal 1 cm.
  * Call this method to change them.
  * \param left Left margin.
  * \param top Top margin.
  * \param right Right margin. Default value is the left one.
  * \see SetLeftMargin(), SetTopMargin(), SetRightMargin(), SetAutoPageBreak()
  */
  virtual void SetMargins(double left, double top, double right = -1);
  
  /// Defines the left margin.
  /**
  * The method can be called before creating the first page.
  * If the current abscissa gets out of page, it is brought back to the margin.
  * \param margin The margin.
  * \see SetTopMargin(), SetRightMargin(), SetAutoPageBreak(), SetMargins()
  */
  virtual void SetLeftMargin(double margin);
  
  /// Returns the left margin.
  /**
  * \return double left margin.
  */
  virtual double GetLeftMargin();

  /// Defines the top margin.
  /**
  * The method can be called before creating the first page.
  * \param margin The margin.
  * \see SetLeftMargin(), SetRightMargin(), SetAutoPageBreak(), SetMargins()
  */
  virtual void SetTopMargin(double margin);
  
  /// Returns the top margin.
  /**
  * \return double top margin.
  */
  virtual double GetTopMargin();

  /// Defines the right margin.
  /**
  * The method can be called before creating the first page.
  * \param margin The margin.
  * \see SetLeftMargin(), SetTopMargin(), SetAutoPageBreak(), SetMargins()
  */
  virtual void SetRightMargin(double margin);

  /// Returns the right margin.
  /**
  * \return double right margin.
  */
  virtual double GetRightMargin();

  /// Defines the cell margin.
  /**
  * The method can be called before creating the first page.
  * \param margin The margin.
  */
  virtual void SetCellMargin(double margin);

  /// Returns the cell margin.
  /**
  * \return double cell margin.
  */
  virtual double GetCellMargin();

  /// Sets the height of a text line
  /**
  * \param height The line height.
  */
  virtual void SetLineHeight(double height);

  /// Returns the height of a text line
  /**
  * \return double line height
  */
  virtual double GetLineHeight();

  /// Enables or disables the automatic page breaking mode.
  /**
  * When enabling, the second parameter
  * is the distance from the bottom of the page that defines the triggering limit.
  * By default, the mode is on and the margin is 2 cm.
  * \param autoPageBreak Boolean indicating if mode should be on or off.
  * \param margin Distance from the bottom of the page.
  * \see Cell(), MultiCell(), AcceptPageBreak()
  */
  virtual void SetAutoPageBreak(bool autoPageBreak, double margin = 0);

  /// Defines the way the document is to be displayed by the viewer.
  /**
  * The zoom level can be set:pages can be displayed entirely on screen, occupy the full width
  * of the window, use real size, be scaled by a specific zooming factor or use viewer default
  * (configured in the Preferences menu of Acrobat). The page layout can be specified too:
  * single at once, continuous display, two columns or viewer default. By default, documents
  * use the full width mode with continuous display.
  * \param zoom The zoom to use. It can be one of the following string values or a number
  * indicating the zooming factor to use. 
  *   \li wxPDF_ZOOM_FULLPAGE: displays the entire page on screen
  *   \li wxPDF_ZOOM_FULLWIDTH: uses maximum width of window
  *   \li wxPDF_ZOOM_REAL: uses real size (equivalent to 100% zoom)
  *   \li wxPDF_ZOOM_DEFAULT: uses viewer default mode
  *   \li wxPDF_ZOOM_FACTOR: uses viewer default mode
  * \param layout The page layout. Possible values are:
  *   \li wxPDF_LAYOUT_SINGLE: displays one page at once
  *   \li wxPDF_LAYOUT_CONTINUOUS: displays pages continuously (default)
  *   \li wxPDF_LAYOUT_TWO: displays two pages on two columns
  *   \li wxPDF_LAYOUT_DEFAULT: uses viewer default mode
  * \param zoomFactor specifies the zoom factor in percent if layout is wxPDF_ZOOM_FACTOR
  */
  virtual void SetDisplayMode(wxPdfZoom zoom,
                              wxPdfLayout layout = wxPDF_LAYOUT_CONTINUOUS,
                              double zoomFactor = 100.);

  /// Enable or disable kerning.
  /**
  * When enabled, kerning is applied to text output.
  * Kerning is disabled by default.
  * \param kerning Boolean indicating if kerning should be enabled.
  */
  virtual void SetKerning(bool kerning);

  /// Activates or deactivates page compression.
  /**
  * When activated, the internal representation of each
  * page is compressed, which leads to a compression ratio of about 2 for the resulting document.
  * Compression is on by default.
  * \param compress Boolean indicating if compression must be enabled.
  */
  virtual void SetCompression(bool compress);

  /// Defines the viewer preferences.
  /**
  * \param preferences A set of viewer preferences options.
  *   \li wxPDF_VIEWER_HIDETOOLBAR:     Hide tool bar
  *   \li wxPDF_VIEWER_HIDEMENUBAR:     Hide menu bar
  *   \li wxPDF_VIEWER_HIDEWINDOWUI:    Hide user interface
  *   \li wxPDF_VIEWER_FITWINDOW:       Fit window to page size
  *   \li wxPDF_VIEWER_CENTERWINDOW:    Center window on screen
  *   \li wxPDF_VIEWER_DISPLAYDOCTITLE: Display document title in title bar
  */
  virtual void SetViewerPreferences(int preferences = 0);

  /// Defines the title of the document.
  /**
  * \param title The title.
  * \see SetAuthor(), SetCreator(), SetKeywords(), SetSubject()
  */
  virtual void SetTitle(const wxString& title);
  
  /// Defines the subject of the document.
  /**
  * \param subject The subject.
  * \see SetAuthor(), SetCreator(), SetKeywords(), SetTitle()
  */
  virtual void SetSubject(const wxString& subject);
  
  /// Defines the author of the document.
  /**
  * \param author The name of the author.
  * \see SetCreator(), SetKeywords(), SetSubject(), SetTitle()
  */
  virtual void SetAuthor(const wxString& author);

  /// Associates keywords with the document,
  /**
  * Generally keywords are in the form 'keyword1 keyword2 ...'.
  * \param keywords The list of keywords.
  * \see SetAuthor(), SetCreator(), SetSubject(), SetTitle()
  */
  virtual void SetKeywords(const wxString& keywords);
  
  /// Defines the creator of the document.
  /**
  * This is typically the name of the application that generates the PDF.
  * \param creator The name of the creator.
  * \see SetAuthor(), SetKeywords(), SetSubject(), SetTitle()
  */
  virtual void SetCreator(const wxString& creator);
  
  /// Defines an alias for the total number of pages.
  /**
  * It will be substituted as the document is closed.
  * \param alias The alias. Default value: {nb}.
  * \see PageNo(), Footer()
  */
  virtual void AliasNbPages(const wxString& alias = wxString(wxT("{nb}")));

  /// This method begins the generation of the PDF document.
  /**
  * It is not necessary to call it explicitly
  * because AddPage() does it automatically.
  * Note: no page is created by this method
  * \see AddPage(), Close()
  */
  virtual void Open();
  
  /// Terminates the PDF document.
  /**
  * It is not necessary to call this method explicitly because SaveAsFile()
  * does it automatically. If the document contains no page, AddPage() is called to prevent from getting
  * an invalid document.
  * \see Open(), SaveAsFile()
  */
  virtual void Close();
  
  /// Adds a new page to the document.
  /**
  * If a page is already present, the Footer() method is called first
  * to output the footer. Then the page is added, the current position set to the top-left corner according
  * to the left and top margins, and Header() is called to display the header.
  * The font which was set before calling is automatically restored. There is no need to call SetFont()
  * again if you want to continue with the same font. The same is true for colours and line width.
  * The origin of the coordinate system is at the top-left corner and increasing ordinates go downwards.
  * \param orientation Page orientation. Possible values are:
  *   \li wxPORTRAIT
  *   \li wxLANDSCAPE
  * The default value is the one passed to the constructor.
  * \see FPDF(), Header(), Footer(), SetMargins()
  */
  virtual void AddPage(int orientation = -1);

  virtual void AddPage(int orientation, wxPaperSize format);

  virtual void AddPage(int orientation, double pageWidth, double pageHeight);

  /// This method is used to render the page header.
  /**
  * It is automatically called by AddPage() and should not be called directly by the application. 
  * The implementation in wxPdfDocument is empty, so you have to subclass it and override the method
  * if you want a specific processing.
  * \see Footer()
  */
  virtual void Header();
  
  /// This method is used to render the page footer.
  /**
  * It is automatically called by AddPage() and Close() and should not be called directly by
  * the application. The implementation in wxPdfDocument is empty, so you have to subclass it
  * and override the method if you want a specific processing.
  * \see Header()
  */
  virtual void Footer();
  
  /// Returns whether footer output is in progress
  /**
  * \return true if footer output is in progress, false otherwise
  * \see Header()
  */
  virtual bool IsInFooter();
  
  /// Returns the current page number.
  /**
  * \return page number
  * \see AliasNbPages()
  */
  virtual int  PageNo();
  
  /// Add spot colour
  /**
  * Add a spot colour which can be referenced in colour setting methods
  * \param name the name of the spot colour (case sensitive)
  * \param cyan indicates the cyan level. Value between 0 and 100
  * \param magenta indicates the magenta level. Value between 0 and 100
  * \param yellow indicates the yellow level. Value between 0 and 100
  * \param black indicates the black level. Value between 0 and 100
  * \see SetDrawColour(), SetFillColour(), SetTextColour(), Line(), Rect(), Cell(), MultiCell()
  */
  virtual void AddSpotColour(const wxString& name, double cyan, double magenta, double yellow, double black);

  /// Add an image pattern
  /**
  * Add a pattern which can be reference in draw or fill pattern methods
  * \param patternName the name of the pattern (case sensitive)
  * \param image the image to use for the pattern
  * \param width the display width
  * \param height the display height
  */
  virtual bool AddPattern(const wxString& patternName, const wxImage& image, double width, double height);

  /// Defines the colour used for all drawing operations.
  /**
  * Affected drawing operations are: lines, rectangles and cell borders. It can be expressed in RGB
  * components or gray scale. The method can be called before the first page is created and the value
  * is retained from page to page.
  * \param grayscale indicates the gray level. Value between 0 and 255
  * \see SetFillColour(), SetTextColour(), Line(), Rect(), Cell(), MultiCell()
  */
  virtual void SetDrawColour(const unsigned char grayscale);
  
  /// Defines the colour used for all drawing operations.
  /**
  * Affected drawing operations are: lines, rectangles and cell borders. It can be expressed in RGB
  * components or gray scale. The method can be called before the first page is created and the value
  * is retained from page to page.
  * \param colour defines a colour composed of a red, green and blue component
  * \see SetFillColour(), SetTextColour(), Line(), Rect(), Cell(), MultiCell()
  */
  virtual void SetDrawColour(const wxColour& colour);
  
  /// Defines the colour used for all drawing operations.
  /**
  * Affected drawing operations are: lines, rectangles and cell borders. It can be expressed in RGB
  * components or gray scale. The method can be called before the first page is created and the value
  * is retained from page to page.
  * \param colour defines a colour using the class wxPdfColour
  * \see SetFillColour(), SetTextColour(), Line(), Rect(), Cell(), MultiCell()
  */
  virtual void SetDrawColour(const wxPdfColour& colour);
  
  /// Defines the colour used for all drawing operations.
  /**
  * Affected drawing operations are: lines, rectangles and cell borders. It can be expressed in RGB
  * components or gray scale. The method can be called before the first page is created and the value
  * is retained from page to page.
  * \param red indicates the red level. Value between 0 and 255
  * \param green indicates the green level. Value between 0 and 255
  * \param blue indicates the blue level. Value between 0 and 255
  * \see SetFillColour(), SetTextColour(), Line(), Rect(), Cell(), MultiCell()
  */
  virtual void SetDrawColour(const unsigned char red, const unsigned char green, const unsigned char blue);

  /// Defines the colour used for all drawing operations.
  /**
  * Affected drawing operations are: lines, rectangles and cell borders. It can be expressed in RGB
  * components or gray scale. The method can be called before the first page is created and the value
  * is retained from page to page.
  * \param cyan indicates the cyan level. Value between 0 and 100
  * \param magenta indicates the magenta level. Value between 0 and 100
  * \param yellow indicates the yellow level. Value between 0 and 100
  * \param black indicates the black level. Value between 0 and 100
  * \see SetFillColour(), SetTextColour(), Line(), Rect(), Cell(), MultiCell()
  */
  virtual void SetDrawColour(double cyan, double magenta, double yellow, double black);

  /// Defines the <b>spot colour</b> used for all drawing operations.
  /**
  * Affected drawing operations are: lines, rectangles and cell borders. It can be expressed in RGB
  * components or gray scale. The method can be called before the first page is created and the value
  * is retained from page to page.
  * \param name the name of the spot colour
  * \param tint indicates the tint level. Value between 0 and 100. Default: 100.
  * \see SetFillColour(), SetTextColour(), Line(), Rect(), Cell(), MultiCell()
  */
  virtual void SetDrawColour(const wxString& name, double tint = 100);

  /// Defines the <b>pattern</b> used for all drawing operations.
  /**
  * The given pattern is used for all drawing operations instead of a colour.
  * Affected drawing operations are: lines, rectangles and cell borders.
  * The method can be called before the first page is created and the value
  * is retained from page to page.
  * \param name the name of the pattern
  * \see SetFillPattern()
  */
  virtual void SetDrawPattern(const wxString& name);

  /// Gets the colour used for all drawing operations.
  /**
  * \see SetDrawColour()
  */
  virtual const wxPdfColour GetDrawColour();
  
  /// Defines the colour used for all filling operations.
  /**
  * Affected filling operations are: filled rectangles and cell backgrounds. It can be expressed in RGB
  * components or gray scale. The method can be called before the first page is created and the value is
  * retained from page to page.
  * \param grayscale indicates the gray level. Value between 0 and 255
  * \see SetDrawColour(), SetTextColour(), Rect(), Cell(), MultiCell()
  */
  virtual void SetFillColour(const unsigned char grayscale);
  
  /// Defines the colour used for all filling operations.
  /**
  * Affected filling operations are: filled rectangles and cell backgrounds. It can be expressed in RGB
  * components or gray scale. The method can be called before the first page is created and the value is
  * retained from page to page.
  * \param colour defines a colour composed of a red, green and blue component
  * \see SetDrawColour(), SetTextColour(), Rect(), Cell(), MultiCell()
  */
  virtual void SetFillColour(const wxColour& colour);
  
  /// Defines the colour used for all filling operations.
  /**
  * Affected filling operations are: filled rectangles and cell backgrounds. It can be expressed in RGB
  * components or gray scale. The method can be called before the first page is created and the value is
  * retained from page to page.
  * \param colour defines a colour using the class wxPdfColour
  * \see SetDrawColour(), SetTextColour(), Rect(), Cell(), MultiCell()
  */
  virtual void SetFillColour(const wxPdfColour& colour);
  
  /// Defines the colour used for all filling operations.
  /**
  * Affected filling operations are: filled rectangles and cell backgrounds. It can be expressed in RGB
  * components or gray scale. The method can be called before the first page is created and the value is
  * retained from page to page.
  * \param red indicates the red level. Value between 0 and 255
  * \param green indicates the green level. Value between 0 and 255
  * \param blue indicates the blue level. Value between 0 and 255
  * \see SetDrawColour(), SetTextColour(), Rect(), Cell(), MultiCell()
  */
  virtual void SetFillColour(const unsigned char red, const unsigned char green, const unsigned char blue);

  /// Defines the colour used for all filling operations.
  /**
  * Affected filling operations are: filled rectangles and cell backgrounds. It can be expressed in RGB
  * components or gray scale. The method can be called before the first page is created and the value is
  * retained from page to page.
  * \param cyan indicates the cyan level. Value between 0 and 100
  * \param magenta indicates the magenta level. Value between 0 and 100
  * \param yellow indicates the yellow level. Value between 0 and 100
  * \param black indicates the black level. Value between 0 and 100
  * \see SetDrawColour(), SetTextColour(), Rect(), Cell(), MultiCell()
  */
  virtual void SetFillColour(double cyan, double magenta, double yellow, double black);

  /// Defines the <b>spot colour</b> used for all filling operations.
  /**
  * Affected filling operations are: filled rectangles and cell backgrounds. It can be expressed in RGB
  * components or gray scale. The method can be called before the first page is created and the value is
  * retained from page to page.
  * \param name is the name of the spot colour
  * \param tint indicates the tint level. Value between 0 and 100. Default: 100.
  * \see SetDrawColour(), SetTextColour(), Rect(), Cell(), MultiCell()
  */
  virtual void SetFillColour(const wxString& name, double tint = 100);

  /// Defines the <b>pattern</b> used for all filling operations.
  /**
  * The given pattern is used for all filling operations instead of a colour.
  * Affected filling operations are: filled rectangles and cell backgrounds. 
  * The method can be called before the first page is created and the value
  * is retained from page to page.
  * \param name the name of the pattern
  * \see SetDrawPattern()
  */
  virtual void SetFillPattern(const wxString& name);

  /// Gets the named pattern as a colour.
  /**
  * \param patternName the name of the pattern
  * \see SetDrawPattern(), SetFillPattern(), AddPattern()
  */
  virtual const wxPdfColour GetPatternColour(const wxString& patternName);
  
  /// Gets the colour used for all filling operations.
  /**
  * \see SetFillColour()
  */
  virtual const wxPdfColour GetFillColour();
  
  /// Defines the colour used for text.
  /**
  * It can be expressed in RGB components or gray scale. The method can be called before the first page is
  * created and the value is retained from page to page.
  * \param grayscale indicates the gray level. Value between 0 and 255
  * \see SetDrawColour(), SetFillColour(), Text(), Cell(), MultiCell()
  */
  virtual void SetTextColour(const unsigned char grayscale);
  
  /// Defines the colour used for text.
  /**
  * It can be expressed in RGB components or gray scale. The method can be called before the first page is
  * created and the value is retained from page to page.
  * \param colour defines a colour composed of a red, green and blue component
  * \see SetDrawColour(), SetFillColour(), Text(), Cell(), MultiCell()
  */
  virtual void SetTextColour(const wxColour& colour);
  
  /// Defines the colour used for text.
  /**
  * It can be expressed in RGB components or gray scale. The method can be called before the first page is
  * created and the value is retained from page to page.
  * \param colour defines a colour using the class wxPdfColour
  * \see SetDrawColour(), SetFillColour(), Text(), Cell(), MultiCell()
  */
  virtual void SetTextColour(const wxPdfColour& colour);
  
  /// Defines the colour used for text.
  /**
  * It can be expressed in RGB components or gray scale. The method can be called before the first page is
  * created and the value is retained from page to page.
  * \param red indicates the red level. Value between 0 and 255
  * \param green indicates the green level. Value between 0 and 255
  * \param blue indicates the blue level. Value between 0 and 255
  * \see SetDrawColour(), SetFillColour(), Text(), Cell(), MultiCell()
  */
  virtual void SetTextColour(const unsigned char red, const unsigned char green, const unsigned char blue);

  /// Defines the colour used for text.
  /**
  * It can be expressed in RGB components or gray scale. The method can be called before the first page is
  * created and the value is retained from page to page.
  * \param cyan indicates the cyan level. Value between 0 and 100
  * \param magenta indicates the magenta level. Value between 0 and 100
  * \param yellow indicates the yellow level. Value between 0 and 100
  * \param black indicates the black level. Value between 0 and 100
  * \see SetDrawColour(), SetFillColour(), Text(), Cell(), MultiCell()
  */
  virtual void SetTextColour(double cyan, double magenta, double yellow, double black);

  /// Defines the <b>spot colour</b> used for text.
  /**
  * It can be expressed in RGB components or gray scale. The method can be called before the first page is
  * created and the value is retained from page to page.
  * \param name the name of the spot colour
  * \param tint indicates the tint level. Value between 0 and 100. Default: 100.
  * \see SetDrawColour(), SetFillColour(), Text(), Cell(), MultiCell()
  */
  virtual void SetTextColour(const wxString& name, double tint = 100);

  /// Defines the <b>pattern</b> used for text output.
  /**
  * The given pattern is used for text output instead of a colour.
  * The method can be called before the first page is created and the value
  * is retained from page to page.
  * \param name the name of the pattern
  * \see SetDrawPattern()
  */
  virtual void SetTextPattern(const wxString& name);

  /// Gets the colour used for text output.
  /**
  * \see SetTextColour()
  */
  virtual const wxPdfColour GetTextColour();
  
  /// Sets the text render mode
  /**
  * \param mode the text render mode
  */
  virtual void SetTextRenderMode(wxPdfTextRenderMode mode = wxPDF_TEXT_RENDER_FILL);
  
  /// Returns the text render mode
  /**
  */
  virtual wxPdfTextRenderMode GetTextRenderMode() const;

  /// Returns the length of a string in user unit.
  /**
  * A font must be selected.
  * \param s The string whose length is to be computed
  * \return int
  */
  virtual double GetStringWidth(const wxString& s);

  /// Defines the line width.
  /**
  * By default, the value equals 0.2 mm. The method can be called before the first page is created
  * and the value is retained from page to page.
  * \param width The width.
  * \see Line(), Rect(), Cell(), MultiCell()
  */
  virtual void SetLineWidth(double width);

  /// Gets the current line width.
  /**
  * \return current line width
  * \see Line(), Rect(), Cell(), MultiCell()
  */
  virtual double GetLineWidth();

  /// Sets line style
  /**
  * \param linestyle: Line style. \see wxPdfLineStale
  */
  virtual void SetLineStyle(const wxPdfLineStyle& linestyle);

  /// Get current line style
  /**
  * \return current line style.
  */
  virtual const wxPdfLineStyle& GetLineStyle();

  /// Set the filling rule
  /**
  * \param rule The filling rule to be used in complex filling operations. Possible values are:
  *   \li wxWINDING_RULE (default)
  *   \li wxODDEVEN_RULE
  */
  virtual void SetFillingRule(int rule = wxWINDING_RULE);

  /// Get the current filling rule
  /**
  * \return the current filling rule
  */
  virtual int GetFillingRule();

  /// Draws a line between two points.
  /**
  * \param x1 Abscissa of first point
  * \param y1 Ordinate of first point
  * \param x2 Abscissa of second point
  * \param y2 Ordinate of second point
  * \see SetLineWidth(), SetDrawColour()
  */
  virtual void Line(double x1, double y1, double x2, double y2);
  
  /// Draws an arrow line between two points.
  /**
  * \param x1 Abscissa of first point
  * \param y1 Ordinate of first point
  * \param x2 Abscissa of second point
  * \param y2 Ordinate of second point
  * \param linewidth line width
  * \param height height of the arrow head
  * \param width width of the arrow head
  * \see SetLineWidth(), SetDrawColour(), SetFillColour()
  */
  virtual void Arrow(double x1, double y1, double x2, double y2, double linewidth, double height, double width);

  /// Outputs a rectangle.
  /**
  * It can be drawn (border only), filled (with no border) or both.
  * \param x Abscissa of upper-left corner
  * \param y Ordinate of upper-left corner
  * \param w Width
  * \param h Height
  * \param style Style of rendering. Possible values are:
  *   \li wxPDF_STYLE_DRAW (default)
  *   \li wxPDF_STYLE_FILL: fill
  *   \li wxPDF_STYLE_FILLDRAW: draw and fill
  * \see SetLineWidth(), SetDrawColour(), SetFillColour()
  */
  virtual void Rect(double x, double y, double w, double h, int style = wxPDF_STYLE_DRAW);

  /// Draws a rounded rectangle
  /**
  * \param x Abscissa of upper-left corner
  * \param y Ordinate of upper-left corner
  * \param w Width
  * \param h Height
  * \param r: Radius of the rounded corners
  * \param roundCorner: Draws rounded corner or not.
  *   \li wxPDF_CORNER_NONE          no rounded corners
  *   \li wxPDF_CORNER_TOP_LEFT      top left corner
  *   \li wxPDF_CORNER_TOP_RIGHT     top right corner
  *   \li wxPDF_CORNER_BOTTOM_LEFT   bottom left corner
  *   \li wxPDF_CORNER_BOTTOM_RIGHT  bottom right corner
  *   \li wxPDF_CORNER_ALL           all corners
  * \param style: Style of rectangle (draw and/or fill)
  */
  virtual void RoundedRect(double x, double y, double w, double h,
                           double r, int roundCorner = wxPDF_CORNER_ALL, int style = wxPDF_STYLE_DRAW);

  /// Draws a Bezier curve
  /**
  * A Bezier curve is tangent to the line between the control points at either end of the curve.
  * \param x0: Abscissa of start point
  * \param y0: Ordinate of start point
  * \param x1: Abscissa of control point 1
  * \param y1: Ordinate of control point 1
  * \param x2: Abscissa of control point 2
  * \param y2: Ordinate of control point 2
  * \param x3: Abscissa of end point
  * \param y3: Ordinate of end point
  * \param style: Style of rectangle (draw and/or fill)
  */
  virtual void Curve(double x0, double y0, double x1, double y1,
                     double x2, double y2, double x3, double y3,
                     int style = wxPDF_STYLE_DRAW);

  /// Draws an ellipse
  /**
  * \param x0: Abscissa of Center point
  * \param y0: Ordinate of Center point
  * \param rx: Horizontal radius
  * \param ry: Vertical radius (if ry = 0, draws a circle)
  * \param angle: Orientation angle (anti-clockwise)
  * \param astart: Start angle
  * \param afinish: Finish angle
  * \param style: Style of rectangle (draw and/or fill)
  * \param nSeg: Ellipse is made up of nSeg Bezier curves
  * \param doSector: connect end points of elliptic arc with center point
  */
  virtual void Ellipse(double x0, double y0, double rx, double ry = 0, 
                       double angle = 0, double astart = 0, double afinish = 360,
                       int style = wxPDF_STYLE_DRAW, int nSeg = 8, bool doSector = false);

  /// Draws a circle
  /**
  * \param x0: Abscissa of Center point
  * \param y0: Ordinate of Center point
  * \param r: Radius
  * \param astart: Start angle
  * \param afinish: Finish angle
  * \param style: Style of rectangle (draw and/or fill)
  * \param nSeg: Circle is made up of nSeg Bezier curves
  */
  virtual void Circle(double x0, double y0, double r, 
                      double astart = 0, double afinish = 360,
                      int style = wxPDF_STYLE_DRAW, int nSeg = 8);

  /// Draws a sector
  /**
  * \param x0: Abscissa of Center point
  * \param y0: Ordinate of Center point
  * \param r: Radius
  * \param astart: Start angle
  * \param afinish: Finish angle
  * \param style: Style of rectangle (draw and/or fill, default: fill&draw)
  * \param clockwise: indicates whether to go clockwise (default: true)
  * \param origin: origin of angles (0 for 3 o'clock, 90 for noon, 180 for 9 o'clock, 270 for 6 o'clock; default: 90)
  */
  virtual void Sector(double x0, double y0, double r, double astart, double afinish,
                      int style = wxPDF_STYLE_FILLDRAW, bool clockwise = true, double origin = 90.);

  /// Draws a polygon
  /**
  * \param x Array with abscissa values
  * \param y Array with ordinate values
  * \param style: Style of polygon (draw and/or fill)
  */
  virtual void Polygon(const wxPdfArrayDouble& x, const wxPdfArrayDouble& y,
                       int style = wxPDF_STYLE_DRAW);

   /// Draws a regular polygon
  /**
  * \param x0: Abscissa of Center point
  * \param y0: Ordinate of Center point
  * \param r: Radius of circumscribed circle
  * \param ns: Number of sides
  * \param angle: Orientation angle (anti-clockwise)
  * \param circle: Flag whether to draw circumscribed circle or not
  * \param style: Style of polygon (draw and/or fill)
  * \param circleStyle: Style of circumscribed circle (draw and/or fill) (if draw)
  * \param circleLineStyle: Line style for circumscribed circle. (if draw)
  * \param circleFillColour: Fill colour for circumscribed circle. (if draw fill circle)
  */
  virtual void RegularPolygon(double x0, double y0, double r, int ns, double angle = 0, bool circle = false,
                              int style = wxPDF_STYLE_DRAW, 
                              int circleStyle = wxPDF_STYLE_DRAW,
                              const wxPdfLineStyle& circleLineStyle = wxPdfLineStyle(),
                              const wxPdfColour& circleFillColour = wxPdfColour());

  /// Draws a star polygon
  /**
  * \param x0: Abscissa of Center point
  * \param y0: Ordinate of Center point
  * \param r: Radius of circumscribed circle
  * \param nv: Number of vertices
  * \param ng: Number of gaps (ng % nv = 1 => regular polygon)
  * \param angle: Orientation angle (anti-clockwise)
  * \param circle: Flag whether to draw circumscribed circle or not
  * \param style: Style of polygon (draw and/or fill)
  * \param circleStyle: Style of circumscribed circle (draw and/or fill) (if draw)
  * \param circleLineStyle: Line style for circumscribed circle. (if draw)
  * \param circleFillColour: Fill colour for circumscribed circle. (if draw fill circle)
  */
  virtual void StarPolygon(double x0, double y0, double r, int nv, int ng, double angle = 0, bool circle = false,
                           int style = wxPDF_STYLE_DRAW, 
                           int circleStyle = wxPDF_STYLE_DRAW,
                           const wxPdfLineStyle& circleLineStyle = wxPdfLineStyle(),
                           const wxPdfColour& circleFillColour = wxPdfColour());

  /// Draws a Bezier spline through a list of points
  /**
  * \param x Array with abscissa values
  * \param y Array with ordinate values
  * \param style: Style of the spline (draw and/or fill)
  */
  virtual void BezierSpline(const wxPdfArrayDouble& x, const wxPdfArrayDouble& y, int style);

  /// Draws a closed Bezier spline through a list of points
  /**
  * \param x Array with abscissa values
  * \param y Array with ordinate values
  * \param style: Style of the spline (draw and/or fill)
  */
  virtual void ClosedBezierSpline(const wxPdfArrayDouble& x, const wxPdfArrayDouble& y, int style);

  /// Draws a shape
  /**
  * \param shape: shape to be drawn
  * \param style Style of rendering. Possible values are:
  *   \li wxPDF_STYLE_DRAW (default)
  *   \li wxPDF_STYLE_FILL: fill
  *   \li wxPDF_STYLE_FILLDRAW: draw and fill
  *   \li wxPDF_STYLE_DRAWCLOSE: close path and draw (can be combined with wxPDF_STYLE_FILL
  */
  virtual void Shape(const wxPdfShape& shape, int style = wxPDF_STYLE_DRAW);

  /// Performs a rotation around a given center.
  /**
  * \param angle angle in degrees.
  * \param x abscissa of the rotation center. Default value: current position.
  * \param y ordinate of the rotation center. Default value: current position.
  *
  * The rotation affects all elements which are printed after the method call
  * (with the exception of the clickable areas).
  *
  * Remarks:
  * \li Only the display is altered. The GetX() and GetY() methods are not affected,
  *  nor the automatic page break mechanism.
  * \li Rotation is not kept from page to page. Each page begins with a null rotation. 
  */
  virtual void Rotate(double angle, double x = -1, double y = -1);

  /// Sets the font embedding mode
  /**
  * If other fonts than the 14 Adobe core fonts are used in a document, they are usually
  * embedded into the PDF file, often resulting in rather large PDF files. This is
  * especially true for Unicode fonts with thousands of glyphs. To reduce the size of
  * the resulting PDF file fonts may be subsetted, that is, only those glyphs actually
  * used in the document are embedded.
  *
  * Currently wxPdfDocument supports font subsetting for TrueType Unicode fonts only.
  *
  * \param fontSubsetting Boolean indicating whether font subsetting should be used or not.
  */
  virtual void SetFontSubsetting(bool fontSubsetting = true) { m_fontSubsetting = fontSubsetting; }

  /// Returns the font embedding mode
  /**
  * \return true if font subsetting is enabled, false otherwise
  */
  virtual bool GetFontSubsetting() const { return m_fontSubsetting; }

  /// Imports a TrueType, TrueTypeUnicode or Type1 font and makes it available.
  /**
  * It is necessary to generate a font definition file first with the makefont utility.
  * The definition file (and the font file itself when embedding) must be present either
  * in the subdirectory 'fonts' of the current working directory or in the one indicated
  * by WXPDF_FONTPATH if this environment variable is defined.
  * \param family Font family. The name can be chosen arbitrarily. If it is a standard family name,
  * it will override the corresponding font.
  * \param style Font style. Possible values are (case insensitive):
  *   \li empty string: regular (default)
  *   \li B: bold
  *   \li I: italic
  *   \li BI or IB: bold italic
  * \param file The font definition file. By default, the name is built from the family and style,
  *  in lower case with no space. 
  * \see SetFont()
  * \deprecated use the wxPdfFontManager to register fonts for use in creating PDF documents
  */
  virtual bool AddFont(const wxString& family, 
                       const wxString& style = wxEmptyString, 
                       const wxString& file = wxEmptyString);

  /// Imports a CJK (Chinese, Japanese or Korean) font and makes it available.
  /**
  * It is necessary to generate a font definition file first with the makefont utility.
  * The definition file (and the font file itself when embedding) must be present either
  * in the current directory or in the one indicated by WXPDF_FONTPATH if the constant is
  * defined.
  * \param family Font family. The name can be chosen arbitrarily. If it is a standard family name,
  * it will override the corresponding font.
  *
  * All font styles (regular, bold, italic and bold-italic) are made available
  * The font definition file name is built from the family in lower case with no space.
  * There are several predefined font definition files available:
  * \li BIG5 Chinese (traditional)
  * \li BIG5-HW  Chinese (traditional) half-width ASCII characters
  * \li GB Chinese (simplified)
  * \li GB-HW Chinese (simplified) half-width ASCII characters
  * \li SJIS Japanese
  * \li SJIS-HW Japanese, half-width ASCII characters
  * \li UHC Korean
  * \li UHC-HW Korean, half-width ASCII characters
  *
  * These fonts require that the Adobe CJK font support is installed
  * \see SetFont()
  * \deprecated use the wxPdfFontManager to register fonts for use in creating PDF documents
  */
#if wxUSE_UNICODE
  virtual bool AddFontCJK(const wxString& family);
#else
  virtual bool AddFontCJK(const wxString& family) 
  { 
    wxUnusedVar(family);
    return false;
  }
#endif
  
  /// Sets the font used to print character strings. 
  /**
  * It is mandatory to call a variant of this method at least once before printing text or the
  * resulting document would not be valid.
  * The font can be either a standard one or a font added via the AddFont() method.
  * Standard fonts use Windows encoding cp1252 (Western Europe).
  * The method can be called before the first page is created and the font is retained from page to page.
  * If you just wish to change the current font size, it is simpler to call SetFontSize().
  *
  * \param family Family font. It can be either a name defined by AddFont() or one of the standard
  * families (case insensitive):
  *  \li Courier (fixed-width)
  *  \li Helvetica or Arial (synonymous; sans serif)
  *  \li Times (serif)
  *  \li Symbol (symbolic)
  *  \li ZapfDingbats (symbolic)
  *
  * It is also possible to pass an empty string. In that case, the current family is retained.
  * \param style Font style. Possible values are (case insensitive):
  *   \li empty string: regular (default)
  *   \li B: bold
  *   \li I: italic
  *   \li BI or IB: bold italic
  *   \li U: underline
  * or any combination. The default value is regular. Bold and italic styles do not apply to Symbol and ZapfDingbats
  * \param size Font size in points. The default value is the current size. If no size has been
  * specified since the beginning of the document, the value taken is 12
  * \see AddFont(), SetFontSize(), Cell(), MultiCell(), Write()
  */
  virtual bool SetFont(const wxString& family,
                       const wxString& style = wxEmptyString,
                       double size = 0);
  
  /// Sets the font used to print character strings. 
  /**
  * It is mandatory to call a variant of this method at least once before printing text or the
  * resulting document would not be valid.
  * The font can be either a standard one or a font added via the AddFont() method.
  * Standard fonts use Windows encoding cp1252 (Western Europe).
  * The method can be called before the first page is created and the font is retained from page to page.
  * If you just wish to change the current font size, it is simpler to call SetFontSize().
  *
  * \param family Family font. It can be either a name defined by AddFont() or one of the standard
  * families (case insensitive):
  *  \li Courier (fixed-width)
  *  \li Helvetica or Arial (synonymous; sans serif)
  *  \li Times (serif)
  *  \li Symbol (symbolic)
  *  \li ZapfDingbats (symbolic)
  *
  * It is also possible to pass an empty string. In that case, the current family is retained.
  * \param style Font style. Possible values are (case insensitive):
  *   \li wxPDF_FONTSTYLE_REGULAR    : regular (default)
  *   \li wxPDF_FONTSTYLE_ITALIC     : italic
  *   \li wxPDF_FONTSTYLE_BOLD       : bold
  *   \li wxPDF_FONTSTYLE_UNDERLINE  : underline
  *   \li wxPDF_FONTSTYLE_OVERLINE   : overline
  *   \li wxPDF_FONTSTYLE_STRIKEOUT  : strike through
  * or any combination.
  * \param size Font size in points. The default value is the current size. If no size has been
  * specified since the beginning of the document, the value taken is 12
  * \see AddFont(), SetFontSize(), Cell(), MultiCell(), Write()
  */
  virtual bool SetFont(const wxString& family, int style, double size = 0);
  
  /// Sets the font used to print character strings based on a wxFont object.
  /**
  * The font referenced by the wxFont object will be added to the document if necessary.
  *
  * \param font The font to be set.
  * \param style Font style. Possible values are (case insensitive):
  *   \li empty string: regular (default)
  *   \li B: bold
  *   \li I: italic
  *   \li BI or IB: bold italic
  *   \li U: underline
  * or any combination. The default value is regular. Bold and italic styles do not apply to Symbol and ZapfDingbats
  * \param size Font size in points. The default value is the current size. If no size has been
  * \see AddFont(), SetFont(), SetFontSize(), Cell(), MultiCell(), Write()
  */
  virtual bool SetFont(const wxPdfFont& font, int style = wxPDF_FONTSTYLE_REGULAR, double size = 0);

  /// Sets the font used to print character strings based on a wxFont object.
  /**
  * The font referenced by the wxFont object will be added to the document if necessary.
  *
  * \param font The font to be set.
  * \see AddFont(), SetFont(), SetFontSize(), Cell(), MultiCell(), Write()
  */
  virtual bool SetFont(const wxFont& font);

  /// Defines the size of the current font.
  /**
  * \param size The size (in points)
  * \see SetFont()
  */
  virtual void SetFontSize(double size);
  
  /// Returns the current font.
  /**
  * \return The current font.
  * \see SetFont()
  */  
  virtual wxPdfFont GetCurrentFont() const;

  /// Returns the current font description instance.
  /**
  * \return The current font description.
  * \see SetFont()
  */  
  virtual const wxPdfFontDescription& GetFontDescription() const;
  
  /// Gets the font family of the current font.
  /**
  * \return The font family of the current font
  * \see SetFont()
  */
  virtual const wxString GetFontFamily();

  /// Gets the style of the current font.
  /**
  * \return The style of the current font as a string
  * \see SetFont()
  */
  virtual const wxString GetFontStyle() const;
  
  /// Gets the style of the current font.
  /**
  * \return The style of the current font
  * \see SetFont()
  */
  virtual int GetFontStyles() const;
  
  /// Gets the size of the current font.
  /**
  * \return The size (in points) of the current font
  * \see SetFont()
  */
  virtual double GetFontSize() const;

  /// Creates a new internal link and returns its identifier.
  /**
  * An internal link is a clickable area which directs to another place within the document.
  * The identifier can then be passed to Cell(), Write(), Image() or Link().
  * The destination is defined with SetLink().
  * \see Cell(), Write(), Image(), Link(), SetLink()
  */
  virtual int AddLink();
  
  /// Defines the page and position a link points to.
  /**
  * \param link The link identifier returned by AddLink()
  * \param y Ordinate of target position; -1 indicates the current position. The default value is 0 (top of page)
  * \param page Number of target page; -1 indicates the current page. This is the default value
  * \see AddLink()
  */
  virtual bool SetLink(int link, double y = 0., int page = -1);
  
  /// Puts a link on a rectangular area of the page.
  /**
  * Text or image links are generally put via Cell(), Write() or Image(), but this method can be useful
  * for instance to define a clickable area inside an image.
  * \param x Abscissa of the upper-left corner of the rectangle
  * \param y Ordinate of the upper-left corner of the rectangle
  * \param w Width of the rectangle
  * \param h Height of the rectangle
  * \param link URL or identifier returned by AddLink()
  * \see AddLink(), Cell(), Write(), Image()
  */
  virtual void Link(double x, double y, double w, double h, const wxPdfLink& link);

  /// Adds a bookmark to the document outline
  /**
  * \param txt: the bookmark title.
  * \param level: the bookmark level (0 is top level, 1 is just below, and so on).
  * \param y: the y position of the bookmark destination in the current page.
  *   -1 means the current position. Default value: 0.
  */
  virtual void Bookmark(const wxString& txt, int  level = 0, double y = 0);

  /// Prints a character string.
  /**
  * The origin is on the left of the first charcter, on the baseline.
  * This method allows to place a string precisely on the page, but it is usually easier to use Cell(),
  * MultiCell() or Write() which are the standard methods to print text.
  * \param x Abscissa of the origin
  * \param y Ordinate of the origin
  * \param txt String to print
  * \see SetFont(), SetTextColour(), Cell(), MultiCell(), Write()
  */
  virtual void Text(double x, double y, const wxString& txt);

  /// Prints a rotated text string
  /**
  * \param x: abscissa of the rotation center.
  * \param y: ordinate of the rotation center.
  * \param txt String to print
  * \param angle: angle in degrees.
  */
  virtual void RotatedText(double x, double y, const wxString& txt, double angle);

  /// Prints a rotated text string with explicit rotation center
  /**
  * \param textX Abscissa of the origin
  * \param textY Ordinate of the origin
  * \param rotationX: abscissa of the rotation center.
  * \param rotationY: ordinate of the rotation center.
  * \param txt String to print
  * \param angle: angle in degrees.
  */
  virtual void RotatedText(double textX, double textY, double rotationX, double rotationY, const wxString& txt, double angle);

  /// Whenever a page break condition is met,
  /**
  * Whenever a page break condition is met, the method is called, and the break is issued or not
  * depending on the returned value. The default implementation returns a value according to the
  * mode selected by SetAutoPageBreak()
  *
  * This method is called automatically and should not be called directly by the application.
  * \return boolean
  * \see SetAutoPageBreak()
  */
  virtual bool AcceptPageBreak();
  
  /// Prints a cell (rectangular area) with optional borders, background colour and character string.
  /**
  * The upper-left corner of the cell corresponds to the current position. The text can be aligned
  * or centered. After the call, the current position moves to the right or to the next line.
  * It is possible to put a link on the text.
  * If automatic page breaking is enabled and the cell goes beyond the limit, a page break is done
  * before outputting.
  * \param w Cell width. If 0, the cell extends up to the right margin.
  * \param h Cell height. Default value: 0.
  * \param txt String to print. Default value: empty string.
  * \param border Indicates if borders must be drawn around the cell. The value can be 
  *   \li wxPDF_BORDER_NONE no border
  *   \li wxPDF_BORDER_LEFT left border
  *   \li wxPDF_BORDER_RIGHT right border
  *   \li wxPDF_BORDER_TOP top border
  *   \li wxPDF_BORDER_BOTTOM bottom border
  *   \li wxPDF_BORDER_FRAME border on all sides
  * 
  * or a combination of them.
  * \param ln Indicates where the current position should go after the call. Possible values are:
  *   \li 0: to the right
  *   \li 1: to the beginning of the next line
  *   \li 2: below
  *
  * Putting 1 is equivalent to putting 0 and calling Ln() just after. Default value: 0.
  * \param align Allows to center or align the text. Possible values are:<ul><li>L or empty string: left align (default value)</li><li>C: center</li><li>R: right align</li></ul>
  *   \li wxPDF_ALIGN_LEFT align the text at the left margin
  *   \li wxPDF_ALIGN_CENTER center the text
  *   \li wxPDF_ALIGN_RIGHT align the text at the right margin
  *   \li wxPDF_ALIGN_JUSTIFY justify the text
  *
  * \param fill Indicates if the cell background must be painted (1) or transparent (0). Default value: 0.
  * \param link URL or identifier returned by AddLink().
  * \see SetFont(), SetDrawColour(), SetFillColour(), SetTextColour(), SetLineWidth(), AddLink(), Ln(), MultiCell(), Write(), SetAutoPageBreak()
  */
  virtual void Cell(double w, double h = 0., const wxString& txt = wxEmptyString,
                    int border = wxPDF_BORDER_NONE, int ln = 0, 
                    int align = wxPDF_ALIGN_LEFT, int fill = 0, 
                    const wxPdfLink& link = wxPdfLink(-1));

  /// This method allows printing text with line breaks.
  /**
  * They can be automatic (as soon as the text reaches the right border of the cell) or explicit
  * (via the \n character). As many cells as necessary are output, one below the other.
  * Text can be aligned, centered or justified. The cell block can be framed and the background painted.
  * \param w Width of cells. If 0, they extend up to the right margin of the page.
  * \param h Height of cells.
  * \param txt String to print
  * \param border Indicates if borders must be drawn around the cell. The value can be 
  *   \li wxPDF_BORDER_NONE no border
  *   \li wxPDF_BORDER_LEFT left border
  *   \li wxPDF_BORDER_RIGHT right border
  *   \li wxPDF_BORDER_TOP top border
  *   \li wxPDF_BORDER_BOTTOM bottom border
  *   \li wxPDF_BORDER_FRAME border on all sides
  * 
  * or a combination of them.
  * \param align Allows to center or align the text. Possible values are:
  *   \li wxPDF_ALIGN_LEFT align the text at the left margin
  *   \li wxPDF_ALIGN_CENTER center the text
  *   \li wxPDF_ALIGN_RIGHT align the text at the right margin
  *   \li wxPDF_ALIGN_JUSTIFY justify the text (default)
  *
  * \param fill Indicates if the cell background must be painted (1) or transparent (0). Default value: 0.
  * \param maxline Defines the maximum number of lines which should be printed.
  *        If maxline is 0 then the number of lines is not restricted. Default value: 0.
  * \return position in text string txt where output ended due to reaching the maximum number of lines
  * \see SetFont(), SetDrawColour(), SetFillColour(), SetTextColour(), SetLineWidth(), Cell(), Write(), SetAutoPageBreak()
  */
  virtual int MultiCell(double w, double h, const wxString& txt,
                        int border = 0, int align = wxPDF_ALIGN_JUSTIFY,
                        int fill = 0, int maxline = 0);

  /// This method counts the number of lines a text would occupy in respect to a given maximal width
  /**
  * \param w Width of cells. If 0, they extend up to the right margin of the page.
  * \param txt String for which the number of lines is to be counted
  * \return Number of lines this text would occupy
  */
  virtual int LineCount(double w, const wxString& txt);

  /// This method counts the number of lines a text will occupy in respect to a given maximal width
  /**
  * \param w Width of cells. If 0, they extend up to the right margin of the page.
  * \param h Height of cells.
  * \param txt String to print
  * \param halign Allows to center or align the text. Possible values are:
  *   \li wxPDF_ALIGN_LEFT align the text at the left margin
  *   \li wxPDF_ALIGN_CENTER center the text
  *   \li wxPDF_ALIGN_RIGHT align the text at the right margin
  *   \li wxPDF_ALIGN_JUSTIFY justify the text (default)
  *
  * \param valign Allows to vertical align the text. Possible values are:
  *   \li wxPDF_ALIGN_TOP align the text at the top of the box
  *   \li wxPDF_ALIGN_MIDDLE center the text vertically in the box
  *   \li wxPDF_ALIGN_BOTTOM align the text at the bottom of the box
  *
  * \param border Indicates if borders must be drawn around the text box. The value can be 
  *   \li wxPDF_BORDER_NONE no border
  *   \li wxPDF_BORDER_LEFT left border
  *   \li wxPDF_BORDER_RIGHT right border
  *   \li wxPDF_BORDER_TOP top border
  *   \li wxPDF_BORDER_BOTTOM bottom border
  *   \li wxPDF_BORDER_FRAME border on all sides
  * 
  * or a combination of them.
  * \param fill Indicates if the cell background must be painted (1) or transparent (0). Default value: 0.
  */
  virtual int TextBox(double w, double h, const wxString& txt,
                      int halign = wxPDF_ALIGN_JUSTIFY, int valign = wxPDF_ALIGN_TOP,
                      int border = 0, int fill = 0);

  /// This method prints text from the current position.
  /**
  * When the right margin is reached (or the \n character is met) a line break occurs and text continues
  * from the left margin. Upon method exit, the current position is left just at the end of the text.
  * It is possible to put a link on the text.
  * \param h Line height
  * \param txt String to print
  * \param link URL or identifier returned by AddLink()
  * \see SetFont(), SetTextColour(), AddLink(), MultiCell(), SetAutoPageBreak()
  */
  virtual void Write(double h, const wxString& txt, const wxPdfLink& link = wxPdfLink(-1));

  /// This method prints text with cell attributes from the current position.
  /**
  * When the right margin is reached (or the \n character is met) a line break occurs and text continues
  * from the left margin. Upon method exit, the current position is left just at the end of the text.
  * It is possible to put a link on the text.
  * \param h Line height
  * \param txt String to print
  * \param border Indicates if borders must be drawn around the cell. The value can be 
  *   \li wxPDF_BORDER_NONE no border
  *   \li wxPDF_BORDER_LEFT left border
  *   \li wxPDF_BORDER_RIGHT right border
  *   \li wxPDF_BORDER_TOP top border
  *   \li wxPDF_BORDER_BOTTOM bottom border
  *   \li wxPDF_BORDER_FRAME border on all sides
  * 
  * or a combination of them.
  * \param fill Indicates if the cell background must be painted (1) or transparent (0). Default value: 0.
  * \param link URL or identifier returned by AddLink()
  * \see SetFont(), SetTextColour(), AddLink(), MultiCell(), SetAutoPageBreak()
  */
  virtual void WriteCell(double h, const wxString& txt, int border = wxPDF_BORDER_NONE, int fill = 0, const wxPdfLink& link = wxPdfLink(-1));

  /// Prints an array of glyphs identified by glyph numbers from the current position.
  /**
  * The glyphs are positioned according to the given offsets in horizontal (x) and vertical (y) direction.
  * \note This method only works for dynamically loaded TrueType and OpenType Unicode fonts.
  */
  virtual bool WriteGlyphArray(wxPdfArrayDouble& x, wxPdfArrayDouble& y, wxPdfArrayUint32& glyphs);

  /// Puts an image in the page. 
  /**
  * The upper-left corner must be given. The dimensions can be specified in different ways:
  *   \li explicit width and height (expressed in user unit)
  *   \li one explicit dimension, the other being calculated automatically in order to keep the original proportions
  *   \li no explicit dimension, in which case the image is put at 72 dpi
  *
  * Supported formats are JPEG, PNG, GIF and WMF.
  * For JPEG, all flavors are allowed:
  *   \li gray scales,
  *   \li true colours (24 bits),
  *   \li CMYK (32 bits)
  *
  * For PNG, the following flavors are allowed:
  *   \li gray scales on at most 8 bits (256 levels)
  *   \li indexed colours
  *   \li true colours (24 bits)
  *
  * but the following options are not supported:
  *   \li Interlacing
  *   \li Alpha channel
  *
  * If a transparent colour is defined, it will be taken into account (but will be only interpreted
  * by Acrobat 4 and above).
  *
  * For GIF, all flavors the wsWidgets GIF decoder is able to handle are supported
  *
  * For WMF: WMF files contain vector data described in terms of Windows Graphics Device Interface
  * (GDI) commands. There are approximately 80 different GDI commands allowed for in the WMF standard.
  * This method interprets only a small subset of these, but is sufficient to display most WMF images.
  * Please feel free to add further functionality.
  *
  * The format can be specified explicitly or inferred from the file extension.
  *
  * It is possible to put a link on the image.
  *
  * Remark: if an image is used several times, only one copy will be embedded in the file.
  *
  * \param file Name of the file containing the image.
  * \param x Abscissa of the upper-left corner.
  * \param y Ordinate of the upper-left corner.
  * \param w Width of the image in the page. If not specified or equal to zero, it is automatically calculated.
  * \param h Height of the image in the page. If not specified or equal to zero, it is automatically calculated.
  * \param mimeType Image format. Possible values are: image/jpeg, image/png, image/gif, image/wmf.
  * If not specified, the type is inferred from the file extension.
  * \param link URL or identifier returned by AddLink().
  * \param maskImage Id of an image mask created previously by ImageMask().
  * \see AddLink()
  */
  virtual bool Image(const wxString& file, double x, double y, double w = 0, double h = 0, 
                     const wxString& mimeType = wxEmptyString,
                     const wxPdfLink& link = wxPdfLink(-1),
                     int maskImage = 0);

  /**
  * Puts an image in the page
  * The image is given by an wxImage-Object
  * \param name Name of the image to be used as an identifier for this image object.
  * \param image wxImage object which will be embedded as PNG
  * \param x Abscissa of the upper-left corner.
  * \param y Ordinate of the upper-left corner.
  * \param w Width of the image in the page. If not specified or equal to zero, it is automatically calculated.
  * \param h Height of the image in the page. If not specified or equal to zero, it is automatically calculated.
  * \param link URL or identifier returned by AddLink().
  * \param maskImage Id of an image mask created previously by ImageMask().
  * \param jpegFormat Switch whether to save image as JPEG (true) or PNG (false, default).
  * \param jpegQuality Quality in case of JPEG format (0 = poor, 100 = best, default 75).
  */
  virtual bool Image(const wxString& name, const wxImage& image,
                     double x, double y, double w = 0, double h = 0,
                     const wxPdfLink& link = wxPdfLink(-1),
                     int maskImage = 0, bool jpegFormat = false, int jpegQuality = 75);

  /**
  * Puts an image in the page
  * The image is given by an wxInputStream-Object containing the raw image data.
  * \param name Name of the image to be used as an identifier for this image object.
  * \param stream wxInputStream object containing the raw image data
  * \param mimeType Image format. Possible values are: image/jpeg, image/png, image/gif, image/wmf.
  * \param x Abscissa of the upper-left corner.
  * \param y Ordinate of the upper-left corner.
  * \param w Width of the image in the page. If not specified or equal to zero, it is automatically calculated.
  * \param h Height of the image in the page. If not specified or equal to zero, it is automatically calculated.
  * \param link URL or identifier returned by AddLink().
  * \param maskImage Id of an image mask created previously by ImageMask().
  */
  virtual bool Image(const wxString& name, wxInputStream& stream,
                     const wxString& mimeType,
                     double x, double y, double w = 0, double h = 0,
                     const wxPdfLink& link = wxPdfLink(-1),
                     int maskImage = 0);

  /**
  * Prepares an image for use as an image mask
  * The image is given as the name of the file conatining the image
  * \param file Name of the file containing the image.
  * \param mimeType Image format. Possible values are: image/jpeg, image/png, image/gif, image/wmf.
  * \return id of the image mask, or 0 in case of an error
  */
  virtual int ImageMask(const wxString& file, const wxString& mimeType = wxEmptyString);

  /**
  * Prepares an image for use as an image mask
  * The image is given by an wxImage-Object
  * \param name Name of the image.
  * \param image wxImage object.
  * \return id of the image mask, or 0 in case of an error
  */
  virtual int ImageMask(const wxString& name, const wxImage& image);

  /**
  * Prepares an image for use as an image mask
  * The image is given by an wxInputStream-Object containing the raw image data.
  * \param name Name of the image.
  * \param stream wxInputStream object containing the raw image data
  * \param mimeType Image format. Possible values are: image/jpeg, image/png, image/gif, image/wmf.
  * \return id of the image mask, or 0 in case of an error
  */
  virtual int ImageMask(const wxString& name, wxInputStream& stream, const wxString& mimeType);

  /// Puts a rotated image in the page. 
  /**
  * The upper-left corner must be given.
  *
  * The format can be specified explicitly or inferred from the file extension.
  *
  * It is possible to put a link on the image.
  *
  * Remark: if an image is used several times, only one copy will be embedded in the file.
  *
  * \param file Name of the file containing the image.
  * \param x Abscissa of the upper-left corner.
  * \param y Ordinate of the upper-left corner.
  * \param w Width of the image in the page. If not specified or equal to zero, it is automatically calculated.
  * \param h Height of the image in the page. If not specified or equal to zero, it is automatically calculated.
  * \param angle Angle of rotation
  * \param type Image format. Possible values are (case insensitive): JPG, JPEG, PNG, GIF, WMF.
  * If not specified, the type is inferred from the file extension.
  * \param link URL or identifier returned by AddLink().
  * \param maskImage Id of an image mask created previously by ImageMask().
  * \see Image(), AddLink()
  */
  virtual void RotatedImage(const wxString& file, double x, double y, double w, double h,
                            double angle,
                            const wxString& type = wxEmptyString,
                            const wxPdfLink& link = wxPdfLink(-1),
                            int maskImage = 0);

  /// Get x coordinate of bottom right corner of last inserted image
  /**
  */
  virtual double GetLastImageBottomRightX() { return m_img_rb_x; }

  /// Get y coordinate of bottom right corner of last inserted image
  /**
  */
  virtual double GetLastImageBottomRightY() { return m_img_rb_y; }

  /// Performs a line break.
  /**
  * The current abscissa goes back to the left margin and the ordinate increases by the amount passed in parameter.
  * \param h The height of the break. By default, the value equals the height of the last printed cell.
  * \see Cell()
  */
  virtual void Ln(double h = -1);

  /// Returns the abscissa of the current position.
  /**
  * \return float
  * \see SetX(), GetY(), SetY()
  */
  virtual double GetX();
  
  /// Defines the abscissa of the current position. 
  /**
  * If the passed value is negative, it is relative to the right of the page.
  * \param x The value of the abscissa.
  * \see GetX(), GetY(), SetY(), SetXY()
  */
  virtual void SetX(double x);
  
  /// Returns the ordinate of the current position.
  /**
  * \return float
  * \see SetY(), GetX(), SetX()
  */
  virtual double GetY();
  
  /// Moves the current abscissa back to the left margin and sets the ordinate. 
  /**
  * If the passed value is negative, it is relative to the bottom of the page.
  * \param y The value of the ordinate.
  * \see GetX(), GetY(), SetY(), SetXY()
  */
  virtual void SetY(double y);
  
  /// Defines the abscissa and ordinate of the current position. 
  /**
  * If the passed values are negative, they are relative respectively to the right and bottom of the page.
  * \param x The value of the abscissa
  * \param y The value of the ordinate
  * \see SetX(), SetY()
  */
  virtual void SetXY(double x, double y);
  
  /// Saves the document to a file on disk
  /**
  * The method first calls Close() if necessary to terminate the document.
  * \param name The name of the file. If not given, the document will be named 'doc.pdf'
  * \see Close()
  */
  virtual void SaveAsFile(const wxString& name = wxEmptyString);
  
  /// Closes the document and returns the memory buffer containing the document
  /**
  * The method first calls Close() if necessary to terminate the document.
  * \return const wxMemoryOutputStream reference to the buffer containing the PDF document.
  * \see Close()
  */
  virtual const wxMemoryOutputStream& CloseAndGetBuffer();

  /// Define text as clipping area
  /**
  * A clipping area restricts the display and prevents any elements from showing outside of it.
  * \param x Abscissa of the origin
  * \param y Ordinate of the origin
  * \param txt String to print
  * \param outline Draw the outline or not.
  */
  virtual void ClippingText(double x, double y, const wxString& txt, bool outline = false);

  /// Define rectangle as clipping area
  /**
  * A clipping area restricts the display and prevents any elements from showing outside of it.
  * \param x Abscissa of the upper-left corner
  * \param y Ordinate of the upper-left corner
  * \param w Width of the rectangle
  * \param h Height of the rectangle
  * \param outline Draw the outline or not.
  */
  virtual void ClippingRect(double x, double y, double w, double h, bool outline = false);

  /// Define ellipse as clipping area
  /**
  * A clipping area restricts the display and prevents any elements from showing outside of it.
  * \param x Abscissa of the Center point
  * \param y Ordinate of the Center point
  * \param rx: Horizontal radius
  * \param ry: Vertical radius (if ry = 0, draws a circle)
  * \param outline Draw the outline or not. (Default false)
  */
  virtual void ClippingEllipse(double x, double y, double rx, double ry = 0, bool outline = false);

  /// Define polygon as clipping area
  /**
  * A clipping area restricts the display and prevents any elements from showing outside of it.
  * \param x Array with abscissa values
  * \param y Array with ordinate values
  * \param outline Draw the outline or not. (Default false)
  */
  virtual void ClippingPolygon(const wxPdfArrayDouble& x, const wxPdfArrayDouble& y, bool outline = false);

  /// Start defining a clipping path
  /**
  * A clipping area restricts the display and prevents any elements from showing outside of it.
  * The clipping path may consist of one or more subpaths.
  */
  virtual void ClippingPath();

  /// Begin a new subpath
  /**
  * Move to the starting point of a new (sub)path.
  * The new current point is (x, y).
  * \param x abscissa value
  * \param y ordinate value
  * \remark This must be the first operation after ClippingPath().
  */
  virtual void MoveTo(double x, double y);

  /// Append a straight line segment to the current (sub)path
  /**
  * Append a straight line segment from the current point to the point (x, y).
  * The new current point is (x, y).
  * \param x abscissa value
  * \param y ordinate value
  */
  virtual void LineTo(double x, double y);

  /// Append a cubic Bezier curve to the current (sub)path
  /**
  * Append a cubic Bezier curve to the current path. The curve extends
  * from the current point to the point (x3, y3), using (x1, y1) and (x2, y2)
  * as the B�zier control points. The new current point is (x3, y3).
  * \param x1: Abscissa of control point 1
  * \param y1: Ordinate of control point 1
  * \param x2: Abscissa of control point 2
  * \param y2: Ordinate of control point 2
  * \param x3: Abscissa of end point
  * \param y3: Ordinate of end point
  */
  virtual void CurveTo(double x1, double y1, double x2, double y2, double x3, double y3);

  /// End a graphics path
  /**
  * \param style Style of rendering. Possible values are:
  *   \li wxPDF_STYLE_NOOP
  *   \li wxPDF_STYLE_DRAW: draw the outline of the path  (default)
  *   \li wxPDF_STYLE_FILL: fill the area enclosed by the path
  *   \li wxPDF_STYLE_FILLDRAW: draw and fill
  */
  virtual void EndPath(int style = wxPDF_STYLE_DRAW);

  /// Close the clipping path
  /**
  * A clipping area restricts the display and prevents any elements from showing outside of it.
  * \param style Style of rendering. Possible values are:
  *   \li wxPDF_STYLE_NOOP (default)
  *   \li wxPDF_STYLE_DRAW: draw the outline of the clipping path 
  *   \li wxPDF_STYLE_FILL: fill the area enclosed by the clipping path
  *   \li wxPDF_STYLE_FILLDRAW: draw and fill
  */
  virtual void ClosePath(int style = wxPDF_STYLE_NOOP);

  /// Define clipping area using a shape
  /**
  * A clipping area restricts the display and prevents any elements from showing outside of it.
  * \param shape shape defining the clipping path
  * \param style Style of rendering. Possible values are:
  *   \li wxPDF_STYLE_NOOP (default)
  *   \li wxPDF_STYLE_DRAW: draw the outline of the clipping path 
  *   \li wxPDF_STYLE_FILL: fill the area enclosed by the clipping path
  *   \li wxPDF_STYLE_FILLDRAW: draw and fill
  */
  virtual void ClippingPath(const wxPdfShape& shape, int style = wxPDF_STYLE_NOOP);

  /// Remove clipping area
  /**
  * Once you have finished using the clipping, you must remove it with UnsetClipping().
  */
  virtual void UnsetClipping();

  /// Prints a cell clipped to a rectangular area
  /**
  * The upper-left corner of the cell corresponds to the current position. The text can be aligned
  * or centered. After the call, the current position moves to the right or to the next line.
  * It is possible to put a link on the text.
  * If automatic page breaking is enabled and the cell goes beyond the limit, a page break is done
  * before outputting.
  * \param w Cell width.
  * \param h Cell height.
  * \param txt String to print. Default value: empty string.
  * \param border Indicates if borders must be drawn around the cell. The value can be 
  *   \li wxPDF_BORDER_NONE no border
  *   \li wxPDF_BORDER_LEFT left border
  *   \li wxPDF_BORDER_RIGHT right border
  *   \li wxPDF_BORDER_TOP top border
  *   \li wxPDF_BORDER_BOTTOM bottom border
  *   \li wxPDF_BORDER_FRAME border on all sides
  * 
  * or a combination of them.
  * \param ln Indicates where the current position should go after the call. Possible values are:
  *   \li 0: to the right
  *   \li 1: to the beginning of the next line
  *   \li 2: below
  *
  * Putting 1 is equivalent to putting 0 and calling Ln() just after. Default value: 0.
  * \param align Allows to center or align the text. Possible values are:<ul><li>L or empty string: left align (default value)</li><li>C: center</li><li>R: right align</li></ul>
  *   \li wxPDF_ALIGN_LEFT align the text at the left margin
  *   \li wxPDF_ALIGN_CENTER center the text
  *   \li wxPDF_ALIGN_RIGHT align the text at the right margin
  *   \li wxPDF_ALIGN_JUSTIFY justify the text
  *
  * \param fill Indicates if the cell background must be painted (1) or transparent (0). Default value: 0.
  * \param link URL or identifier returned by AddLink().
  * \see SetFont(), SetDrawColour(), SetFillColour(), SetTextColour(), SetLineWidth(), AddLink(), Ln(), MultiCell(), Write(), SetAutoPageBreak()
  */
  virtual void ClippedCell(double w, double h = 0., const wxString& txt = wxEmptyString,
                           int border = wxPDF_BORDER_NONE, int ln = 0, 
                           int align = wxPDF_ALIGN_LEFT, int fill = 0, 
                           const wxPdfLink& link = wxPdfLink(-1));

  /// Enters a transformation environment
  /**
  * Before applying any transformation this method should be invoked.
  * All transformation method invoke it implicitly if necessary.
  * All open transformation environments are closed implicitly on page end.
  */
  virtual void StartTransform();

  /// Performs scaling in X direction only
  /**
  * A scaling transformation is applied for the X direction.
  * \param sx: scaling factor for width as percent. 0 is not allowed.
  * \param x: abscissa of the scaling center. Default is current x position
  * \param y: ordinate of the scaling center. Default is current y position
  */
  virtual bool ScaleX(double sx, double x = -1, double y = -1);

  /// Performs scaling in Y direction only
  /**
  * A scaling transformation is applied for the Y direction.
  * \param sy: scaling factor for height as percent. 0 is not allowed.
  * \param x: abscissa of the scaling center. Default is current x position
  * \param y: ordinate of the scaling center. Default is current y position
  */
  virtual bool ScaleY(double sy, double x = -1, double y = -1);

  /// Performs equal scaling in X and Y direction
  /**
  * A scaling transformation is applied for both - X and Y - directions.
  * \param s: scaling factor for width and height as percent. 0 is not allowed.
  * \param x: abscissa of the scaling center. Default is current x position
  * \param y: ordinate of the scaling center. Default is current y position
  */
  virtual bool ScaleXY(double s, double x = -1, double y = -1);

  /// Performs scaling in X and Y direction
  /**
  * A scaling transformation is applied independently for X and Y direction.
  * \param sx: scaling factor for width in percent. 0 is not allowed.
  * \param sy: scaling factor for height in percent. 0 is not allowed.
  * \param x: abscissa of the scaling center. Default is current x position
  * \param y: ordinate of the scaling center. Default is current y position
  */
  virtual bool Scale(double sx, double sy, double x = -1, double y = -1);

  /// Performs a horizontal mirroring transformation
  /**
  * Alias for scaling -100% in x-direction
  * \param x: abscissa of the axis of reflection
  */
  virtual void MirrorH(double x = -1);

  /// Performs a vertical mirroring transformation
  /**
  * Alias for scaling -100% in y-direction
  * \param y: abscissa of the axis of reflection
  */
  virtual void MirrorV(double y = -1);

  /// Moves the X origin
  /**
  * \param tx: movement to the right
  */
  virtual void TranslateX(double tx);

  /// Moves the Y origin
  /**
  * \param ty: movement to the bottom
  */
  virtual void TranslateY(double ty);

  /// Moves the origin
  /**
  * \param tx: movement to the right
  * \param ty: movement to the bottom
  */
  virtual void Translate(double tx, double ty);

  /// Performs a skewing in both X direction only
  /**
  * \param xAngle: angle in degrees between -90 (skew to the left) and 90 (skew to the right)
  * \param x: abscissa of the skewing center. default is current x position
  * \param y: ordinate of the skewing center. default is current y position
  */
  virtual bool SkewX(double xAngle, double x = -1, double y = -1);

  /// Performs a skewing in Y direction only
  /**
  * \param yAngle: angle in degrees between -90 (skew to the bottom) and 90 (skew to the top)
  * \param x: abscissa of the skewing center. default is current x position
  * \param y: ordinate of the skewing center. default is current y position
  */
  virtual bool SkewY(double yAngle, double x = -1, double y = -1);

  /// Performs a skewing in both X and Y directions
  /**
  * \param xAngle: angle in degrees between -90 (skew to the left) and 90 (skew to the right)
  * \param yAngle: angle in degrees between -90 (skew to the bottom) and 90 (skew to the top)
  * \param x: abscissa of the skewing center. default is current x position
  * \param y: ordinate of the skewing center. default is current y position
  */
  virtual bool Skew(double xAngle, double yAngle, double x = -1, double y = -1);

  virtual void Transform( double a, double b, double c, double d, double tx, double ty );

  /// Leaves a transformation environment
  /**
  * This method should be invoked to cancel a transformation environment
  * opened by StartTransform.
  * All open transformation environments are closed implicitly on page end.
  */
  virtual void StopTransform();

  /// Sets alpha values and blend mode
  /**
  * \param lineAlpha alpha value for stroking operations, from 0 (transparent) to 1 (opaque)
  * \param fillAlpha alpha value for non-stroking operations, from 0 (transparent) to 1 (opaque)
  * \param blendMode one of the following:
  *   Normal, Multiply, Screen, Overlay, Darken, Lighten, ColorDodge, ColorBurn,
  *   HardLight, SoftLight, Difference, Exclusion, Hue, Saturation, Color, Luminosity
  */
  virtual int SetAlpha(double lineAlpha = 1, double fillAlpha = 1, wxPdfBlendMode blendMode = wxPDF_BLENDMODE_NORMAL);

  /// Sets a previously defined alpha state
  /**
  * \param alphaState id of alpha state
  */
  virtual void SetAlphaState(int alphaState);

  /// Defines a linear gradient shading
  /**
  * \param col1 first colour (RGB or CMYK).
  * \param col2 second colour (RGB or CMYK).
  * \param gradientType Type of the gradient
  */
  virtual int LinearGradient(const wxPdfColour& col1, const wxPdfColour& col2,
                             wxPdfLinearGradientType gradientType = wxPDF_LINEAR_GRADIENT_HORIZONTAL);

  /// Defines a axial gradient shading
  /**
  * \param col1 first colour (RGB or CMYK).
  * \param col2 second colour (RGB or CMYK).
  * \param x1 start point of gradient vector, default: 0 (range 0 .. 1)
  * \param y1 start point of gradient vector, default: 0 (range 0 .. 1)
  * \param x2 end point of gradient vector, default: 1 (range 0 .. 1)
  * \param y2 end point of gradient vector, default: 0 (range 0 .. 1)
  * \param intexp interpolation exponent, default: 1
  */
  virtual int AxialGradient(const wxPdfColour& col1, const wxPdfColour& col2,
                            double x1 = 0, double y1 = 0,
                            double x2 = 1, double y2 = 0,
                            double intexp = 1);

  /// Defines a axial gradient shading
  /**
  * \param col1 first colour (RGB or CMYK).
  * \param col2 second colour (RGB or CMYK).
  * \param x1 start point of gradient vector, default: 0 (range 0 .. 1)
  * \param y1 start point of gradient vector, default: 0 (range 0 .. 1)
  * \param x2 end point of gradient vector, default: 1 (range 0 .. 1)
  * \param y2 end point of gradient vector, default: 0 (range 0 .. 1)
  * \param midpoint position of the mirror point, default: 0.5 (range 0 .. 1)
  * \param intexp interpolation exponent, default: 1
  */
  virtual int MidAxialGradient(const wxPdfColour& col1, const wxPdfColour& col2,
                               double x1 = 0, double y1 = 0,
                               double x2 = 1, double y2 = 0,
                               double midpoint = 0.5, double intexp = 1);

  /// Defines a radial gradient shading
  /**
  * \param col1 first colour (RGB or CMYK).
  * \param col2 second colour (RGB or CMYK).
  * \param x1 center point of circle 1, default: 0.5 (range 0 .. 1)
  * \param y1 center point of circle 1, default: 0.5 (range 0 .. 1)
  * \param r1 radius of circle 1, default: 0
  * \param x2 center point of circle 2, default: 0.5 (range 0 .. 1)
  * \param y2 center point of circle 2, default: 0.5 (range 0 .. 1)
  * \param r2 radius of circle 2, default: 1
  * \param intexp interpolation exponent, default: 1
  */
  virtual int RadialGradient(const wxPdfColour& col1, const wxPdfColour& col2, 
                              double x1 = 0.5, double y1 = 0.5, double r1 = 0,
                              double x2 = 0.5, double y2 = 0.5, double r2 = 1,
                              double intexp = 1);

  /// Defines a coons patch mesh gradient shading
  /**
  * \param mesh coons patch mesh to be used for the gradient
  * \param minCoord minimal coordinate of the mesh
  * \param maxCoord maximal coordinate of the mesh
  */
  virtual int CoonsPatchGradient(const wxPdfCoonsPatchMesh& mesh, double minCoord = 0, double maxCoord = 1);

  /// Paints a gradient shading to rectangular area
  /**
  * \param x abscissa of the top left corner of the rectangle.
  * \param y ordinate of the top left corner of the rectangle.
  * \param w width of the rectangle.
  * \param h height of the rectangle.
  * \param gradient id of the gradient.
  */
  virtual void SetFillGradient(double x, double y, double w, double h, int gradient);

  /// Draws a graphical marker symbol
  /**
  * \param x abscissa of the marker's center 
  * \param y ordinate of the marker's center
  * \param markerType type of the marker 
  * \param size size of the marker
  */
  virtual void Marker(double x, double y, wxPdfMarker markerType, double size);

  /// Adds a text annotation
  /**
  * \param x abscissa of the annotation symbol
  * \param y ordinate of the annotation symbol
  * \param text annotation text
  */
  virtual void Annotate(double x, double y, const wxString& text);

  /// Appends Javascript
  /**
  * Allows to append Javascript code to a Javascript object at the document level.
  * \param javascript Javascript code to be appended
  */
  virtual void AppendJavascript(const wxString& javascript);

  /// Prints a string containing simple XML markup
  /**
  * Output starts at the current position.
  * \param str string containing text with simple XML markup
  * \see \ref writexml
  */
  virtual void WriteXml(const wxString& str);

  /// Adds a check box field at the current position
  /**
  * Adds a check box to the list of form fields at the current position
  * \param name field name of the check box
  * \param width width of the check box
  * \param checked default value of the check box
  */
  virtual void CheckBox(const wxString& name, double width, bool checked = false);

  /// Adds a check box field
  /**
  * Adds a check box to the list of form fields
  * \param name field name of the check box
  * \param x abscissa of the check box position
  * \param y ordinate of the check box position
  * \param width width of the check box
  * \param checked default value of the check box
  */
  virtual void CheckBox(const wxString& name, double x, double y, double width, bool checked = false);
  
  /// Adds a combo box field at the current position
  /**
  * Adds a combo box to the list of form fields at the current position
  * \param name field name of the combo box
  * \param width width of the combo box
  * \param height height of the combo box
  * \param values array of option values of the combo box
  */
  virtual void ComboBox(const wxString& name, double width, double height, const wxArrayString& values);

  /// Adds a combo box field
  /**
  * Adds a combo box to the list of form fields
  * \param name field name of the combo box
  * \param x abscissa of the combo box position
  * \param y ordinate of the combo box position
  * \param width width of the combo box
  * \param height height of the combo box
  * \param values array of option values of the combo box
  */
  virtual void ComboBox(const wxString& name, 
                        double x, double y, double width, double height, 
                        const wxArrayString& values);
  
  /// Adds a push button at the current position
  /**
  * Adds a push button to the list of form fields at the current position
  * \param name field name of the push button
  * \param width width of the push button
  * \param height height of the push button
  * \param caption caption of the push button
  * \param action associated Javascript action
  */
  virtual void PushButton(const wxString& name, double width, double height, 
                          const wxString& caption, const wxString& action);

  /// Adds a push button
  /**
  * Adds a push button to the list of form fields
  * \param name field name of the push button
  * \param x abscissa of the push button position
  * \param y ordinate of the push button position
  * \param width width of the push button
  * \param height height of the push button
  * \param caption caption of the push button
  * \param action associated Javascript action
  */
  virtual void PushButton(const wxString& name, double x, double y, double width, double height, 
                          const wxString& caption, const wxString& action);
  
  /// Adds a radio button at the current position
  /**
  * Adds a radio button to the list of form fields at the current position
  * \param group name of the radio button group this radio button belongs to
  * \param name field name of the radio button
  * \param width width of the radio button
  */
  virtual void RadioButton(const wxString& group, const wxString& name, double width);
  
  /// Adds a radio button
  /**
  * Adds a radio button to the list of form fields
  * \param group name of the radio button group this radio button belongs to
  * \param name field name of the radio button
  * \param x abscissa of the radio button position
  * \param y ordinate of the radio button position
  * \param width width of the radio button
  */
  virtual void RadioButton(const wxString& group, const wxString& name, 
                           double x, double y, double width);

  /// Adds a text field at the current position
  /**
  * Adds a text field to the list of form fields at the current position
  * \param name field name of the text field
  * \param width width of the text field
  * \param height height of the text field
  * \param value default value of the text field
  * \param multiline flag whether the text field is a multiline field or not
  */
  virtual void TextField(const wxString& name, double width, double height,
                         const wxString& value = wxEmptyString, bool multiline = false);

  /// Adds a text field
  /**
  * Adds a text field to the list of form fields
  * \param name field name of the text field
  * \param x abscissa of the text field position
  * \param y ordinate of the text field position
  * \param width width of the text field
  * \param height height of the text field
  * \param value default value of the text field
  * \param multiline flag whether the text field is a multiline field or not
  */
  virtual void TextField(const wxString& name, 
                         double x, double y, double width, double height,
                         const wxString& value = wxEmptyString, bool multiline = false);

  /// Sets colours for form fields
  /**
  * Sets the border, background and text colour to be used
  * for all subsequent form field additions until this method is called again
  * with different values.
  * \param borderColour colour of the form field's border
  * \param backgroundColour colour of the form field's background
  * \param textColour colour of the form field's font
  */
  virtual void SetFormColours(const wxPdfColour& borderColour = wxPdfColour(),
                     const wxPdfColour& backgroundColour = wxPdfColour(250),
                     const wxPdfColour& textColour = wxPdfColour());

  /// Sets the border style for form fields
  /**
  * Sets the border width and style to be used
  * for all subsequent form field additions until this method is called again
  * with different values.
  * \param borderStyle style of the form field's border
  *   \li wxPDF_BORDER_SOLID - solid border
  *   \li wxPDF_BORDER_DASHED - dashed border
  *   \li wxPDF_BORDER_BEVELED - beveled border
  *   \li wxPDF_BORDER_INSET - inset border
  *   \li wxPDF_BORDER_UNDERLINE - border on the bottom side only
  * \param borderWidth width of the form field's border
  */
  virtual void SetFormBorderStyle(wxPdfBorderStyle borderStyle = wxPDF_BORDER_SOLID,
                                  double borderWidth = -1);

  /// Starts a new Template
  /**
  * Starts a new template, optionally with own dimensions.
  * The margins have to adapted to the new template size.
  * For writing outside the template, for example to build a clipped template,
  * the margins and "cursor" position have to be set manually after
  * the call to BeginTemplate().
  *
  * If no dimensions are given, the template uses the current page size.
  * The method returns the ID of the current template.
  * The ID is used to reference a template in the UseTemplate() method.
  * Warning: A template once created is embedded in the resulting PDF document
  * at all events, even if it is not used.
  *
  * \param x The x-coordinate given in user units
  * \param y The y-coordinate given in user units
  * \param width The width given in user units
  * \param height The height given in user units
  * \return int The ID of the created template
  * \see EndTemplate(), UseTemplate()
  *
  * Attention: Calls to BeginTemplate can not be nested!
  */
  virtual int BeginTemplate(double x = 0, double y = 0, double width = 0, double height = 0);

  /// Terminates a template
  /**
  * Terminates the creation of a template and reset initiated variables on beginTemplate.
  *
  * \return If a template was under construction, its ID is returned, otherwise a 0 is returned.
  * \see BeginTemplate(), UseTemplate()
  */
  virtual int EndTemplate();

  /// Get the calculated size of a template
  /**
  * Retrieves the size of a template.
  *
  * \param templateId A valid template ID
  * \param width The width of the template
  * \param height The height of the template
  * \see BeginTemplate(), EndTemplate(), UseTemplate(), ImportPage()
  *
  * Attention: The width and/or height parameters have to be set to a value <= 0
  * prior to calling this method, otherwise they will not be calculated.
  * If one dimension, i.e. width, is passed with a value > 0,
  * the other one, i.e. height, is calculated accordingly.
  */
  virtual void GetTemplateSize(int templateId, double& width, double& height);

  /// Uses a template in current page or in another template
  /**
  * Uses the specified template just like an image in the current page or
  * in another template.
  *
  * All parameters are optional. The width or height is calculated using
  * GetTemplateSize internally.
  * By default the size as defined by BeginTemplate is used.
  *
  * \param templateId A valid template ID
  * \param x The x coordinate
  * \param y The y coordinate
  * \param width The new width of the template
  * \param height The new height of the template
  * \see BeginTemplate(), EndTemplate(), ImportPage()
  *
  * Attention: The template may be displayed distorted, if both width and height
  * are given with values > 0 and do not correspond to the dimensions of the template.
  */
  virtual void UseTemplate(int templateId, double x = -1, double y = -1, double width = 0, double height = 0);

  /// Sets a source file for the external template feature.
  /**
  * Selects the source for the external template feature.
  * A parser is setup for importing pages from the PDF document.
  * Although wxPdfDocument usually creates PDF documents conforming to version 1.3
  * of the PDF standard, parsing of documents conforming to versions up to 1.6 is
  * supported. If pages are aimported from documents conforming to a higher version
  * than 1.3 the version used by wxPdDocument is updated accordingly.
  *
  * \param filename a valid filename
  * \param password a valid user or owner password if the PDF document is encrypted
  * \return the number of available pages, or 0 if the document could not be opened
  * \see ImportPage(), UseTemplate()
  *
  * Attention: Access permissions for printing, copying and extracting text or graphics
  * are required. If a PDF document does not have these access permissions, it cannot
  * be used as a source for the external template feature.
  */
  virtual int SetSourceFile(const wxString& filename, const wxString& password = wxEmptyString);
  
  /// Gets the document information dictionary of the current external PDF document.
  /**
  * Gets the values of the Info dictionary of the current external document, if available.
  *
  * \param info the info dictionary object receiving the document information
  * \return true if the info dictionary was available, false otherwise
  * \see SetSourceFile()
  */
  virtual bool GetSourceInfo(wxPdfInfo& info);

  /// Imports a page from an external PDF document
  /**
  * Imports a page from the current external PDF document. As the bounding box of the
  * template the ArtBox of the imported page is used. If the page does not have an
  * explicit ArtBox, the CropBox will be used instead; if there is no explicit CropBox
  * then the MediaBox will be used.
  *
  * \param pageno page number of the page to be imported
  * \param pageBox page box which should be used (default: ArtBox)
  * \return Index of imported page - for use in UseTemplate().
  * A value of 0 is returned if the page number is out of range or no source was selected.
  * \see SetSourceFile()
  */
  virtual int ImportPage(unsigned int pageno, wxPdfPageBox pageBox = wxPDF_PAGEBOX_ARTBOX);

  /// Gets the bounding box of a template
  /**
  * Especially for pages imported from an external PDF document the size of the bounding
  * box might be of interest. The values returned correspond to the coordinates of the
  * lower left corner and the width and height of the template.
  *
  * \param templateId A valid template ID
  * \param x The x coordinate of the lower left corner
  * \param y The y coordinate of the lower left corner
  * \param width The width of the template
  * \param height The height of the template
  * \see SetTemplateBBox(), BeginTemplate(), ImportPage()
  */
  virtual void GetTemplateBBox(int templateId, double& x, double& y, double& width, double& height);

  /// Sets the bounding box of a template
  /**
  * As long as a template hasn't been used it is possible to change the bounding box of
  * the template. This may be useful for pages imported from an external PDF document
  * allowing to set the visible portion of the template.
  * \b Note: Setting the bounding box influences only the visible area of the template,
  * not the real size it occupies.
  *
  * \param templateId A valid template ID
  * \param x The x coordinate of the lower left corner
  * \param y The y coordinate of the lower left corner
  * \param width The width of the template
  * \param height The height of the template
  * \see GetTemplateBBox(), BeginTemplate(), ImportPage()
  */
  virtual void SetTemplateBBox(int templateId, double x, double y, double width, double height);

  /// Prints a text string along a path defined by a shape
  /**
  * \param shape shape defining a path along which the text is printed
  * \param text text string to be printed
  * \param mode flag how to handle the text string
  *   \li wxPDF_SHAPEDTEXTMODE_ONETIME: the text should be printed at most one time depending on the path length
  *   \li wxPDF_SHAPEDTEXTMODE_STRETCHTOFIT: the text should be stretched to fit exactly along the given path (default)
  *   \li wxPDF_SHAPEDTEXTMODE_REPEAT: the text should be repeated if the text length is shorter than the path length
  */
  virtual void ShapedText(const wxPdfShape& shape, const wxString& text, wxPdfShapedTextMode mode = wxPDF_SHAPEDTEXTMODE_STRETCHTOFIT);

  /// Add a layer
  /**
  * An optional content group represents a collection of graphics
  * that can be made visible or invisible dynamically by users of viewer applications.
  * In wxPdfDocument they are referenced as layers.
  * \param layerName the name of the layer
  * \return a reference to the new layer
  */
  virtual wxPdfLayer* AddLayer(const wxString& layerName);

  /// Add a title layer
  /**
  * Creates a title layer. A title layer is not really a layer but a collection of layers
  * under the same title heading.
  * \param layerTitle the title text
  * \return a reference to the title layer 
  */
  virtual wxPdfLayer* AddLayerTitle(const wxString& layerTitle);

  /// Add a layer membership container
  /**
  * Content typically belongs to a single optional content group,
  * and is visible when the group is <B>ON</B> and invisible when it is <B>OFF</B>.
  * To express more complex visibility policies, content should not declare itself
  * to belong to an optional content group directly, but rather to an optional
  * content membership container.
  * \return a reference to the new layer membership container
  */
  virtual wxPdfLayerMembership* AddLayerMembership();

  /// Lock a layer
  /**
  * Use this method to lock an optional content group.
  * The state of a locked group cannot be changed through the user interface
  * of a viewer application. Producers can use this entry to prevent the visibility
  * of content that depends on these groups from being changed by users.
  * \param layer the layer that needs to be added to the array of locked OCGs
  */
  virtual void LockLayer(wxPdfLayer* layer);

  /// Add a radio group of layers
  /**
  * Use this method to set a collection of optional content groups
  * whose states are intended to follow a "radio button" paradigm.
  * That is, the state of at most one optional content group
  * in the array should be ON at a time: if one group is turned
  * ON, all others must be turned OFF.
  * \param radioGroup the radio group
  */
  virtual void AddLayerRadioGroup(const wxPdfLayerGroup& radioGroup);

  /// Enter a layer
  /**
  * Begins a graphic block whose visibility is controlled by the <CODE>layer</CODE>.
  * Blocks can be nested. Each block must be terminated by LeaveLayer().<p>
  * Note that nested layers only require a single
  * call to this method and a single call to LeaveLayer(); all the nesting control
  * is built in.
  * \param layer the layer
  */
  virtual void EnterLayer(wxPdfLayer* layer);
  virtual void EnterLayer(wxPdfLayerMembership* layer);

  /// Leave a layer
  /**
  * Ends a layer controlled graphic block. It will end the most recent open block.
  */
  virtual void LeaveLayer();

  /// Attach file
  /**
  * Attaches a file to the PDF document.
  * \param fileName path to the file to attach
  * \param attachName the name under which the file will be attached (dfeault: filename)
  * \param description an optional description
  */
  virtual bool AttachFile(const wxString& fileName, 
                          const wxString& attachName = wxEmptyString,
                          const wxString& description = wxEmptyString);

  /// Set message translation mode
  /**
  * Sets the message translation mode which controls the handling of msg tags in XML output
  * \param translate flag specifying whether messages translation should be enabled
  */
  void SetMessageTranslateMode(bool translate) { m_translate = translate; }

  /// Get message translation mode
  /**
  * Gets the message translation mode which controls the handling of msg tags in XML output
  * \return TRUE if messages are translated, FALSE otherwise
  */
  bool GetMessageTranslateMode() { return m_translate; }

protected:
  /// Initialize
  virtual void Initialize(int orientation);

  /// Save graphic state
  virtual void SaveGraphicState();

  /// Restore graphic state
  virtual void RestoreGraphicState();

  /// Clear graphic state
  virtual void ClearGraphicState();

  /// Select font
  virtual bool SelectFont(const wxString& family,
                          const wxString& style = wxEmptyString,
                          double size = 0, bool setFont = true);

  /// Select font
  virtual bool SelectFont(const wxString& family, int style,
                          double size = 0, bool setFont = true);

  /// Select font based on wxFont object
  virtual bool SelectFont(const wxFont& font, bool setFont = true);

  /// Select font based on wxFont object
  virtual bool SelectFont(const wxPdfFont& font, int style, double size = 0, bool setFont = true);

  /// Force selecting the current font
  virtual void ForceCurrentFont();

  /// Defines the size of the current font.
  /**
  * \param size The size (in points)
  * \param setSize flag whether to actually set the font size in PDF output
  * \see SetFont()
  */
  virtual void SetFontSize(double size, bool setSize);
  
  /// Apply visual ordering
  virtual wxString ApplyVisualOrdering(const wxString& txt);

  /// Returns the length of a string in user unit.
  /**
  * A font must be selected.
  * \param s The string whose length is to be computed
  * \return int
  * \note This method expects the text already to be preprocessed in respect to visual layout.
  */
  virtual double DoGetStringWidth(const wxString& s);

  /// Prints a cell (rectangular area) with optional borders, background colour and character string.
  /**
  * \note This method expects the text already to be preprocessed in respect to visual layout.
  */
  virtual void DoCell(double w, double h = 0., const wxString& txt = wxEmptyString,
                      int border = wxPDF_BORDER_NONE, int ln = 0, 
                      int align = wxPDF_ALIGN_LEFT, int fill = 0, 
                      const wxPdfLink& link = wxPdfLink(-1));

  /// Start document
  virtual void BeginPage(int orientation, wxSize pageSize);
  
  /// End of page contents
  virtual void EndPage();
  
  /// End dociment
  virtual void EndDoc();

  /// Add header
  virtual void PutHeader();
  
  /// Add pages.
  virtual void PutPages();

  /// Replace page number aliases
  virtual void ReplaceNbPagesAlias();
  
  /// Add resources
  virtual void PutResources();

  /// Add bookmarks
  virtual void PutBookmarks();

  /// Add extended graphics states
  virtual void PutExtGStates();

  /// Add shaders
  virtual void PutShaders();

  /// Adds fonts
  virtual void PutFonts();
  
  /// Add images
  virtual void PutImages();

  /// Add templates
  virtual void PutTemplates();

  /// Add imported objects
  virtual void PutImportedObjects();

  virtual void WriteObjectValue(wxPdfObject* value, bool newline = true);

  /// Add spot colours
  virtual void PutSpotColours();

  /// Add patterns
  virtual void PutPatterns();

  /// Add layers
  virtual void PutLayers();

  /// Add optional content properties
  void PutOCProperties();

  /// Add optional content order
  void PutOCGOrder(wxPdfLayer* layer);

  /// Add automatic state event
  void PutASEvent(const wxString& situation, const wxString& category, bool& first);

  /// Add Javascript (document level)
  virtual void PutJavaScript();

  /// Add attached files
  virtual void PutFiles();

  /// Add resource dictionary
  virtual void PutResourceDict();
  
  /// Add encryption info.
  virtual void PutEncryption();

  /// Add form fields
  virtual void PutFormFields();

  /// Add info.
  virtual void PutInfo();
  
  /// Addcatalog
  virtual void PutCatalog();
  
  /// Add object dictionary
  virtual void PutXObjectDict();
  
  /// Add trailer
  virtual void PutTrailer();

  /// Calculate stream size
  size_t CalculateStreamLength(size_t len);

  /// Calculate stream offset
  size_t CalculateStreamOffset();

  /// Get new object id
  int GetNewObjId();

  /// Begin a new object
  void NewObj(int objId = 0);
  
  /// Decorate text
  wxString DoDecoration(double x, double y, const wxString& txt);

  /// Show text string, with kerning if requested
  void ShowText(const wxString& txt);

  /// Show a single glyph
  void ShowGlyph(wxUint32 glyph);

  /// Format a text string
  void TextEscape(const wxString& s, bool newline = true);

  /// Add byte stream
  void PutStream(wxMemoryOutputStream& s);
  
  /// Add a text string to the document
  void OutTextstring(const wxString& s, bool newline = true);

  /// Add a raw text string to the document (without charset conversion)
  void OutRawTextstring(const wxString& s, bool newline = true);

  /// Add a hex text string to the document (without charset conversion)
  void OutHexTextstring(const wxString& s, bool newline = true);

  /// Add an ASCII text string to the document
  void OutAsciiTextstring(const wxString& s, bool newline = true);
  
  /// Add \ before \, ( and )
  void OutEscape(const char* s, size_t len);

  /// Add ASCII string
  void OutAscii(const wxString& s, bool newline = true);

  /// Add character string
  void Out(const char* s, bool newline = true);

  /// Add len characters
  void Out(const char* s, size_t len, bool newline = true);

  /// Sets a draw point
  void OutPoint(double x, double y);

  /// Sets a draw point relative to current position
  void OutPointRelative(double dx, double dy);

  /// Draws a line from last draw point
  void OutLine(double x, double y);

  /// Draws a line relative from last draw point
  void OutLineRelative(double dx, double dy);

  /// Draws a B�zier curve from last draw point
  void OutCurve(double x1, double y1, double x2, double y2, double x3, double y3);

  /// Perform transformation
  void Transform(double tm[6]);

  /// Adds a form field to the document
  void AddFormField(wxPdfAnnotationWidget* field, bool setFormField = true);

  /// Add an indirect object to the document
  void OutIndirectObject(wxPdfIndirectObject* object);

  /// Add an image object to the document
  void OutImage(wxPdfImage* currentImage,
                double x, double y, double w, double h, const wxPdfLink& link);

  /// Write XML using XML node
  void WriteXml(wxXmlNode* node);

  /// Prepare an XML cell for output
  void PrepareXmlCell(wxXmlNode* node, wxPdfCellContext& context);

  /// Output a prepared XML cell
  void WriteXmlCell(wxXmlNode* node, wxPdfCellContext& context);

  /// Take alignment of an XML cell into account
  void DoXmlAlign(wxPdfCellContext& context);

  /// Prepare an XML table for output
  void PrepareXmlTable(wxXmlNode* node, wxPdfCellContext& context);

  /// Output a prepared XML table
  void WriteXmlTable(wxPdfCellContext& context);

  /// Set internal scale factor
  void SetScaleFactor(const wxString& unit);

  wxSize CalculatePageSize(wxPaperSize format);

  wxSize CalculatePageSize(double pageWidth, double pageHeight);

  virtual void AddPage(int orientation, wxSize pageSize);

  void LoadZapfDingBats();

private:
  bool                 m_yAxisOriginTop;      ///< flag whether the origin of the y axis resides at the top (or bottom) of the page
  int                  m_page;                ///< current page number
  int                  m_n;                   ///< current object number
  int                  m_firstPageId;         ///< object id of the first page
 
  wxPdfOffsetHashMap*  m_offsets;             ///< array of object offsets

  wxOutputStream*      m_buffer;              ///< buffer holding in-memory PDF
  wxPdfPageHashMap*    m_pages;               ///< array containing pages
  int                  m_state;               ///< current document state

  bool                 m_kerning;             ///< kerning flag
  bool                 m_compress;            ///< compression flag
  int                  m_defOrientation;      ///< default orientation
  int                  m_curOrientation;      ///< current orientation
  wxPdfBoolHashMap*    m_orientationChanges;  ///< array indicating orientation changes
  wxSize               m_defPageSize;         ///< default page width
  wxSize               m_curPageSize;         ///< current page width
  wxPdfPageSizeMap*    m_pageSizes;           ///< array indicating page size changes

  double               m_k;                   ///< scale factor (number of points in user unit)
  double               m_fwPt;                ///< width of page format in points
  double               m_fhPt;                ///< height of page format in points
  double               m_fw;                  ///< width of page format in user unit
  double               m_fh;                  ///< height of page format in user unit
  double               m_wPt;                 ///< current width of page in points
  double               m_hPt;                 ///< current height of page in points
  double               m_w;                   ///< current width of page in user unit
  double               m_h;                   ///< current height of page in user unit
  double               m_imgscale;            ///< image scale factor

  double               m_tMargin;             ///< top margin
  double               m_bMargin;             ///< page break margin
  double               m_lMargin;             ///< left margin
  double               m_rMargin;             ///< right margin
  double               m_cMargin;             ///< cell margin

  double               m_x;                   ///< current x position in user unit for cell positioning
  double               m_y;                   ///< current y position in user unit for cell positioning
  double               m_angle;               ///< current rotation angle
  double               m_lasth;               ///< height of last cell printed
  double               m_lineWidth;           ///< line width in user units
  wxPdfLineStyle       m_lineStyle;           ///< current line style
  int                  m_fillRule;            ///< current filling rule

  int                  m_inTransform;         ///< flag for transformation state
  wxPdfFontHashMap*    m_fonts;               ///< array of used fonts
  wxPdfStringHashMap*  m_diffs;               ///> array of encoding differences
  wxPdfBoolHashMap*    m_winansi;             ///> array of flags whether encoding differences are based on WinAnsi
  wxPdfImageHashMap*   m_images;              ///< array of used images
  wxPdfPageLinksMap*   m_pageLinks;           ///< array of links in pages
  wxPdfLinkHashMap*    m_links;               ///< array of internal links
  wxPdfNamedLinksMap*  m_namedLinks;          ///< array of named internal links

  wxPdfExtGStateMap*   m_extGStates;          ///< array of extended graphics states
  wxPdfExtGSLookupMap* m_extGSLookup;         ///< array for fast lookup of extended graphics states
  int                  m_currentExtGState;    ///< current extended graphics state

  wxPdfGradientMap*    m_gradients;           ///< array of gradients
  wxPdfSpotColourMap*  m_spotColours;          ///< array of spot colours
  wxPdfPatternMap*     m_patterns;            ///< array of patterns

  wxPdfAnnotationsMap* m_annotations;         ///< array of text annotations
  
  wxArrayPtrVoid       m_outlines;            ///< array of bookmarks
  int                  m_outlineRoot;         ///< number of root node
  int                  m_maxOutlineLevel;     ///< max. occuring outline level

  wxArrayPtrVoid       m_graphicStates;       ///< array of graphic states
  wxString             m_fontPath;            ///< current default path for font files
  wxString             m_fontFamily;          ///< current font family
  int                  m_fontStyle;           ///< current font style
  int                  m_decoration;          ///< font decoration flags
  bool                 m_fontSubsetting;      ///< flag whether to use font subsetting

  wxPdfFontDetails*    m_currentFont;         ///< current font info

  double               m_fontSizePt;          ///< current font size in points
  double               m_fontSize;            ///< current font size in user unit
  wxPdfColour          m_drawColour;          ///< commands for drawing colour
  wxPdfColour          m_fillColour;          ///< commands for filling colour
  wxPdfColour          m_textColour;          ///< commands for text colour
  bool                 m_colourFlag;          ///< indicates whether fill and text colours are different
  double               m_ws;                  ///< word spacing
  wxPdfTextRenderMode  m_textRenderMode;      ///< text render mode

  bool                 m_autoPageBreak;       ///< automatic page breaking
  double               m_pageBreakTrigger;    ///< threshold used to trigger page breaks
  bool                 m_inFooter;            ///< flag set when processing footer
  wxPdfZoom            m_zoomMode;            ///< zoom display mode
  double               m_zoomFactor;          ///< zoom factor
  wxPdfLayout          m_layoutMode;          ///< layout display mode
  int                  m_viewerPrefs;         ///< viewer preferences

  wxString             m_title;               ///< title
  wxString             m_subject;             ///< subject
  wxString             m_author;              ///< author
  wxString             m_keywords;            ///< keywords
  wxString             m_creator;             ///< creator
  wxString             m_aliasNbPages;        ///< alias for total number of pages
  wxString             m_PDFVersion;          ///< PDF version number

  double               m_img_rb_x;            ///< right-bottom corner X coordinate of inserted image
  double               m_img_rb_y;            ///< right-bottom corner Y coordinate of inserted image

  // Encryption
  bool                 m_encrypted;           ///< flag whether document is protected
  wxPdfEncrypt*        m_encryptor;           ///< encryptor instance
  int                  m_encObjId;            ///< encrypted object id

  // Javascript
  int                  m_nJS;                 ///< Javascript object number
  wxString             m_javascript;          ///< Javascript string

  // Forms
  int                  m_zapfdingbats;        ///< index of font ZapfDingBats
  wxPdfFormFieldsMap*  m_formFields;          ///< array of form fields
  wxPdfFormAnnotsMap*  m_formAnnotations;     ///< array of form field annotations
  wxPdfRadioGroupMap*  m_radioGroups;         ///< array of radio button groups
  wxString             m_formBorderColour;     ///< form field border colour
  wxString             m_formBackgroundColour; ///< form field background colour
  wxString             m_formTextColour;       ///< form field text colour
  wxString             m_formBorderStyle;     ///< form field border style
  double               m_formBorderWidth;     ///< form field border width

  // Templates
  bool                 m_inTemplate;          ///< flag whether template mode is on
  wxPdfTemplatesMap*   m_templates;           ///< array of templates
  wxString             m_templatePrefix;      ///< prefix used for template object names
  int                  m_templateId;          ///< Id of current template
  wxPdfTemplate*       m_currentTemplate;     ///< current template

  wxPdfParserMap*      m_parsers;             ///< array of parsers
  wxPdfParser*         m_currentParser;       ///< current parser
  wxString             m_currentSource;       ///< current import source file name
  wxString             m_importVersion;       ///< highest PDF version of imported files

  // OCG / layer
  wxPdfOcgMap*         m_ocgs;                ///< array of OCGs
  wxPdfLayerRGMap*     m_rgLayers;            ///< array of radio group layers
  wxPdfLayerGroup*     m_lockedLayers;        ///< array of locked layers
  wxArrayInt           m_layerDepth;          ///< stack for nested layers

  // File attachments
  int                  m_nAttachments;        ///< attachments object number
  wxPdfAttachmentMap*  m_attachments;         ///< array of file attachments

  bool                 m_translate;           ///< flag whether messages in msg tags should be translated

private:
#if WXPDFDOC_INHERIT_WXOBJECT
  DECLARE_DYNAMIC_CLASS(wxPdfDocument)
#endif
  DECLARE_NO_COPY_CLASS(wxPdfDocument)

  friend class wxPdfImage;
  friend class wxPdfTable;
};

#endif

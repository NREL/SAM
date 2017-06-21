///////////////////////////////////////////////////////////////////////////////
// Name:        pdfdocdef.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2005-08-04
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfdocdef.h Compile time switches for the \b wxPdfDocument component

/** \mainpage wxPdfDocument

\section intro What is wxPdfDocument?

wxPdfDocument is a C++ class which allows wxWidgets applications to generate PDF files.
The code is a port of <a href="http://www.fpdf.org"><b>FPDF</b></a> - a free PHP class for
generating PDF files - to C++ using the <a href="http://www.wxwidgets.org"><b>wxWidgets</b></a>
library. wxPdfDocument does not make use of any libraries like
<a href="http://www.pdflib.com"><b>PDFlib</b></a> or
<a href="http://www.fastio.com"><b>ClibPDF</b></a> which require a fee at least for
commercial usage. wxPdfDocument is published under the <b>wxWidgets (formerly wxWindows)
license</b>. This means you may use it for any kind of usage and modify it to suit your needs.

wxPdfDocument offers all advantages of \b FPDF.  Several add-on PHP scripts found on the
FPDF web site are incorporated into wxPdfDocument. The main features are:

- Choice of measure unit, page format and margins 
- Page header and footer management 
- Automatic page break 
- Automatic line break and text justification 
- Image support (GIF, JPEG, PNG and WMF) 
- Colours (Grayscale, RGB, CMYK, Spot colours)
- Links (internal and external)
- 14 Adobe standard fonts
- TrueType and Type1 fonts (with or without embedding) and encoding support
- TrueType Unicode and Type0 fonts (for Chinese, Japanese and Korean) support in the Unicode build
- OpenType Unicode fonts support in the Unicode build
- Page compression 
- Graphics primitives for the creation of simple drawings
- Definition of clipping areas
- Bookmarks for outlining the document 
- Rotation
- Protecting the document by passwords and/or access permissions
- Text annotations
- PDF forms (supported field types: text, combo box, check box, radio button, push button)
- JavaScript
- Fill gradients
- Templates
- Layers (optional content groups)
- Simple bitmap patterns as draw and fill colours

The class can produce documents in many languages other than the Western European ones:
Central European, Cyrillic, Greek, Baltic and Thai, provided you own TrueType or Type1
fonts with the desired character set. In the Unicode build Chinese, Japanese and Korean
are supported, too.

A \ref overview showing all available methods in alphabetical order is provided.
A sample application including more than 20 examples demonstrates the different features.
Separate detailed descriptions are available for the \ref makefont and the \ref showfont.
The chapter \ref writexml describes the supported tags of the simple XML markup language
used by the method wxPdfDocument::WriteXml.

wxPdfDocument is hosted as a component of <a href="http://wxcode.sourceforge.net"><b>wxCode</b></a>.
For any remark, question or problem, you can leave a message on the appropriate \b wxCode
tracker accessible from the <a href="http://wxcode.sourceforge.net/support.php"> wxCode support</a>
page. Or you can send a mail to me 
<a href="&#109;&#97;&#105;&#108;&#116;&#111;:&#117;&#108;&#114;&#105;&#99;&#104;&#46;&#116;&#101;&#108;&#108;&#101;&#64;&#103;&#109;&#120;&#46;&#100;&#101;">directly</a>.

\section version Version history

<dl>
<dt><b>0.9.5</b> - <i>December 2015</i></dt>
<dd>
wxPdfDocument is compatible with wxWidgets version 2.8.12 and version 3.0.2. 

General changes:<br>
- respect "join" and "cap" attributes of wxPen in wxPdfDC (2.9)
- cleaned up the use of wxMemoryOutputStream instances
- cleaned up dependencies on wxWidgets libraries for graphics formats GIF and JPEG

Fixed bugs:<br>
- fixed a bug in positioning rotated text
- fixed a bug in determining line lengths in XML formatted output
- fixed a bug in drawing a point in wxPdfDC (2.9)
- fixed a bug in drawing elliptic arcs in wxPdfDC (2.9)

</dd>

<dt><b>0.9.4</b> - <i>August 2013</i></dt>
<dd>
wxPdfDocument is compatible with wxWidgets version 2.8.12 and version 2.9.5. 
Compatibility with older wxWidgets versions is not guaranteed, but it should
work with all 2.8.x versions.

General changes:<br>
- added handling of Unicode surrogates for TrueType Unicode fonts (in wxPdfDocument library and in ShowFont utility)
- added missing initialization for image format type to use in wxPdfDC
- added support to write wxImage objects in JPEG format to PDF (instead of PNG format only)

Fixed bugs:<br>
- corrected the handling of sectors in method wxPdfDocument::Ellipse
- fixed a bug in ShowFont (font index for TrueType collections was not selectable)
- fixed a bug in the Type1 font parser (parsing /Subrs could hang)
- fixed a bug in the Type1 font parser (endless loop when parsing Type1 multi master font files)
- fixed a bug in TrueType font parser reading wrong values from OS/2 table version 0
- adjusted default font metrics in wxPdfDC and external leading to always >= 0
- fixed a GDI object leak in TrueType font parser (Windows only)

</dd>

<dt><b>0.9.3</b> - <i>June 2012</i></dt>
<dd>
wxPdfDocument is compatible with wxWidgets version 2.8.12 and version 2.9.3. 
Compatibility with older wxWidgets versions is not guaranteed, but it should
work with all 2.8.x versions.

General changes:<br>
- added methods to access the bottom right coordinates of the last inserted image
- added span tag to XML markup
- added method wxPdfDocument::AttachFile to attach files to PDF documents
- added compile time option to derive wxPdfDocument from wxObject (makes interfacing to wxPerl easier)
- integrated enhancements to wxPdfDC and MakeFont contributed by Mark Dootson
- added support for the wxWidgets printing framework (contributed by Mark Dootson)
- enhanced wxPdfDC sample application to demonstrate the integration with the printing framework (contributed by Mark Dootson)

Fixed bugs:<br>
- fixed a bug in pdffontdatacore.cpp (non-ASCII characters didn't show on OSX)
- fixed a bug in pdfencrypt.cpp (setting a non-empty document id)
- fixed a bug in pdfxml.cpp (force line break for words too long to fit on a line)
- fixed in bug in handling external templates in conjunction with protection (crypting used the wrong object id for strings and streams)

</dd>

<dt><b>0.9.2</b> - <i>September 2011</i></dt>
<dd>
wxPdfDocument is compatible with wxWidgets version 2.8.12 and version 2.9.2. 
Compatibility with older wxWidgets versions is not guaranteed, but it should
work with all 2.8.x versions.

General changes:<br>
- added method wxPdfDC::GetPdfDocument for builds based on wxWidgets 2.9.x
- added method wxPdfDocument::WriteXml for direct handling of wxXmlNode instances
- added support for optionally activating/deactivating message translation (<b>msg</b> tag) in XML output method
- added optional document id parameter for method wxPdfEncrypt::GenerateEncryptionKey

Fixed bugs:<br>
- fixed a bug in method wxPdfDocument::CalculatePageSize
- fixed a bug in wxPdfFontDataCore::ConvertCID2GID
- fixed several scaling bugs in wxPdfDC
- fixed several warnings in pdfkernel regarding formatted output of size_t variables
- fixed minor issues with Bengali font

</dd>

<dt><b>0.9.1</b> - <i>January 2011</i></dt>
<dd>
wxPdfDocument is compatible with wxWidgets version 2.8.11 and version 2.9.1. 
Compatibility with older wxWidgets versions is not guaranteed, but it should
now work with all 2.8.x versions.

Added features:<br>
- added support for Apple Unicode TrueType fonts
- added the \ref showfont

General changes:<br>
- optimized the processing speed of VOLT rules
- modified the code for wxMac support
- modified the sample tutorial7 to test the new wxMac font loading code
- added check for valid 'cmap' table in wxPdfFontParserTrueType
- added call to method wc_str for wxString parameters in calls to FromWChar
- implemented method RegisterSystemFonts for wxMac
- samples changed to set the executable path as the current working directory

Fixed bugs:<br>
- fixed a memory leak on registering a font identified by a wxFont object
- fixed a bug in method ShowGlyph
- fixed several wxMac compile time bugs (missing includes, some typos)
- changed the wxMac print dialog includes in the printing sample

</dd>

<dt><b>0.9.0</b> - <i>December 2010</i></dt>
<dd>
wxPdfDocument is compatible with wxWidgets version 2.8.11 and version 2.9.1. 
Compatibility with older wxWidgets versions is not guaranteed.

This is the first release of wxPdfDocument containing a <b>PDF drawing context</b> (wxPdfDC).
There are implementations for wxWidgets 2.8.x and 2.9.x; the matching implementation
is selected automatically at compile time. Please report your experiences with wxPdfDC
to the author of wxPdfDocument, be it bug reports, contributions or feature requests.

\b Note: A <b>PDF graphics context</b> is planned for one of the next releases of wxPdfDocument.
Most likely only wxWidgets 2.9.x will be supported since the internals of the base class
wxGraphicsContext differ considerably between wxWidgets 2.8.x and 2.9.x.

Added features:<br>
- methods to draw Bezier splines through a list of points;
the drawing sample has been extended to show the new functionality
- PDF drawing context (wxPdfDC); not yet all methods are implemented
- support for fonts with VOLT (Visual Ordering and Layout Tables) data
(currently visual ordering only, preprocessing of the fonts required);
fonts for 9 Indic scripts are included to demonstrate this feature

General changes:<br>
- increased output speed for method SaveAsFile (if large graphics files are involved)
- all currently supported CJK font families are now registered automatically at startup of the font manager
- MS CJK fonts aren't automatically registered as Type0 fonts,
since this conflicts with registering these fonts as TrueType Unicode fonts
- handling of image masks has been improved

Fixed bugs:<br>
- opening font files could fail if the file path contained non-latin characters
Now wxFileSystem::FileNameToURL is used to create valid file names for use in method OpenFile of wxFileSystem
- invalid format codes in method wxPdfUtility::Double2String could cause problems in MinGW environment
- registering half-width CJK fonts didn't work
- bug in page size handling
- no file was written when Close was called before SaveAsFile
- bug in the handling of transparency for image masks
- uninitialized member variables in layer objects possibly causing invisibility of layers
- cleaned up output formatting codes for building on 64-bit systems
- compile time bugs for wxWidgets built with wxUSE_STL
- several minor bugs

</dd>

<dt><b>0.8.5</b> - <i>October 2009</i></dt>
<dd>
wxPdfDocument is compatible with wxWidgets version 2.8.10. Some preparations were done
to make wxPdfDocument compatible with version 2.9.x and above, too.

Added features in <b>all</b> builds:<br>
- support for individual page sizes
- support for setting fill rule to <i>odd/even</i> or <i>winding</i>
- support for setting the text render mode
- support for layers (optional content groups)
- support for patterns as draw and fill colours
- support for Code 128 barcodes

Added features in <b>Unicode</b> build:<br>
- support for kerning
- support for different encodings for Type1 and TrueType fonts
- support for using TrueType and OpenType fonts loaded directly from .ttf or .otf file
- support for using Type1 fonts loaded directly from .pfb and .afm file
- support for using TrueType and OpenType fonts defined by a wxFont object
- font subsetting for OpenType Unicode fonts (experimental, currently non-CID fonts only)
- direct positioning and writing of glyph numbers for TrueType/OpenType Unicode fonts<br>
this may be used in conjunction with tools for writing complex scripts like ICU

Added features in <b>ANSI</b> build:<br>
- support for fonts defined by a wxFont object (mapped by family, weight and style to the Adobe core fonts)

General changes:<br>
- coordinate transformation (location of origin and y axis orientation)
is now done directly in PDF. This was a prerequisite to add wxGraphicsContext support
in an upcoming version. User unit scaling is done programmatically.
- unified the naming of all methods manipulating colours. Now always the
British spelling is used, i.e. <i>colour</i> instead of <i>color</i>.

Fixed bugs:<br>
- line style measurements did not use user units
- encryption support for big endian platforms.
- method wxPdfDocument::ClippingText.
</dd>

<dt><b>0.8.0</b> - <i>December 2006</i></dt>
<dd>
Added features:<br>
- support for external templates: pages of existing PDF documents may be imported and used as templates
- font subsetting for TrueType and TrueType Unicode fonts, often resulting in much smaller PDF file sizes
- support for using and embedding OpenType Unicode fonts
- enhanced support for password based encryption, encryption key length freely definable between 40 and 128 bits (<b>Attention</b>: Adobe Reader supports only keys with 40 or 128 bits.)
- support for AES encryption (additional standard encryption method in PDF version 1.6 and above)

wxPdfDocument is compatible with wxWidgets version 2.8.0 as well as with version 2.6.x.

As an add-on preprocessed font files for the free <a href="http://dejavu.sourceforge.net">DejaVu fonts</a>
(version 2.12) are provided in the file release <b><a href="http://sourceforge.net/project/showfiles.php?group_id=51305&package_id=45182&release_id=468705">wxPdfDocument Add-Ons</a></b>.

<b>Attention</b>: For supporting font subsetting for ordinary non-Unicode TrueType fonts
the format of the font definition files has been extended. Font definition files created
with prior versions of the \ref makefont are still usable, but do not support font subsetting.
It is recommended to regenerate own font definition files. Unfortunately common AFM font metric
files do not contain glyph information which is required by the \ref makefont to create the
character-to-glyph mapping. Therefore the utility <tt>ttf2ufm</tt> had to be changed.
The modified version including a Windows executable is available in the file release <b><a href="http://sourceforge.net/project/showfiles.php?group_id=51305&package_id=45182&release_id=468705">wxPdfDocument Add-Ons</a></b>.
</dd>

<dt><b>0.7.6</b> - <i>October 2006</i></dt>
<dd>
Added features (thanks to Stuart Smith):<br>
- setting/getting the default path used for loading font files
- getting the description of the current font
- loading images from a wxInputStream (in addition to loading from file or wxImage)

<b>Attention</b>: The structure of the font definition files has changed due to
the addition of the font attribute <tt>xHeight</tt>. It is necessary to regenerate own
font definition files using the \ref makefont. To support the <tt>xHeight</tt> attribute the utility
<tt>ttf2ufm</tt> had to be changed.

wxPdfDocument is now compatible with wxWidgets version 2.7.1 and above as well as with version 2.6.x.

Fixed several bugs
</dd>

<dt><b>0.7.5</b> - <i>September 2006</i></dt>
<dd>
Added or enhanced features:<br>
- support for tables in simple XML markup spanning more than a page
- support for internal links in simple XML markup
- support for transparency
- support for image masks
- support for internal templates
- support for polygon and shape clipping
- support for printing text along a path
- extended support for fill gradients (<b>API changed!</b>)
- internal colour management reworked

Fixed some minor bugs
</dd>

<dt><b>0.7</b> - <i>April 2006</i></dt>
<dd>
Added features:<br>
- support for CMYK and spot colours
- support for named colours (486 predefined names for RGB colours) (wxPdfColour)
- support for colour names in HTML notation (\#rrggbb) (wxPdfColour)
- text annotations
- additional font decorations: overline, strikeout
- PDF forms
- JavaScript at the document level
- Simple XML markup language for styling and structuring text

Added or modified methods:<br>
- wxPdfDocument::LineCount, wxPdfDocument::TextBox and several getter methods were added for convenience
- wxPdfDocument::MultiCell now respects a maximal line count
- wxPdfDocument::WriteXml allows to print text containing simple XML markup
</dd>

<dt><b>0.6</b> - <i>November 2005</i></dt>
<dd>
Added features:
- gradients
- transformations
- barcodes<br>
- \ref makefont<br>

Changed API of graphics primitives: line style and fill colour parameters deleted,
line style and fill colour have to be set using wxPdfDocument::SetLineStyle and wxPdfDocument::SetFillColour.
</dd>

  <dt><b>0.5</b> - <i>September 2005</i></dt>
<dd>
First public release
</dd>

<dt><b>0.4</b> - <i>August 2005</i></dt>
<dd>
Support for embedding fonts
</dd>

<dt><b>0.3</b> - <i>July 2005</i></dt>
<dd>
Support for embedding images
</dd>

<dt><b>0.2</b> - <i>June 2005</i></dt>
<dd>
Several add-ons implemented
</dd>

<dt><b>0.1</b> - <i>May 2005</i></dt>
<dd>
Planning and basic PDF features implemented
</dd>
</dl>

\author Ulrich Telle (<a href="&#109;&#97;&#105;&#108;&#116;&#111;:&#117;&#108;&#114;&#105;&#99;&#104;&#46;&#116;&#101;&#108;&#108;&#101;&#64;&#103;&#109;&#120;&#46;&#100;&#101;">ulrich DOT telle AT gmx DOT de</a>)

\section issues Known issues

Currently there are no known issues regarding the functionality of the wxPdfDocument component.
All features were thoroughly tested individually, but it's almost impossible to check all
potential combinations. <b>If you find bugs please report them to the author!</b>

\section acknowledgement Acknowledgements

I'm very grateful to <b>Bruno Lowagie</b>, the main author of the <b>iText Java library</b>
(http://www.lowagie.com/iText), for allowing to take lots of ideas and inspirations
from this great Java PDF library. Especially the font handling and font subsetting
was influenced in that way.

Many thanks go to <b>Ben Moores</b> who provided code for layers and patterns he wrote for
his PDF extension for <b>Mapnik</b> (http://www.mapnik.org). This code has been extended
based on ideas from the <b>iText Java library</b> and was incorporated into wxPdfDocument.

Support for Indic scripts is based on the efforts of <b>Ian Back</b>, creator of the PHP library \b mPDF
(http://mpdf.bpm1.com); special thanks to <b>K Vinod Kumar</b> of the Centre for Development of Advanced
Computing, Mumbai (http://www.cdacmumbai.in), for clearing license issues of the Raghu font series.

Kudos to <b>Mark Dootson</b> for contributing major enhancements of wxPdfDC and it's integration
into the wxWidgets printing framework.

Since wxPdfDocument is based on the great \b FPDF PHP class and several of the contributions to it
found on the <a href="http://www.fpdf.org"><b>FPDF website</b></a> I would like to thank 

- Olivier Plathey (FPDF, Barcodes, Bookmarks, Rotation),
- Maxime Delorme (Sector)
- Johannes Guentert (JavaScript)
- Martin Hall-May (WMF images, Transparency)
- Emmanuel Havet (Code39 barcodes)
- Shailesh Humbad (POSTNET barcodes)
- Matthias Lau (i25 barcodes)
- Pierre Marletta (Diagrams)
- Laurent Passebecq (Labels)
- David Hernandez Sanz (additional graphics primitives)
- Valentin Schmidt (Transparency, Alpha channel)
- Jan Slabon (FPDI)
- Klemen Vodopivec (Protection)
- Moritz Wagner (Transformation)
- Andreas Wuermser (Clipping, Gradients, Transformation)

The wxPdfDocument encryption methods use the RSA Data Security, Inc. MD5 Message
Digest Algorithm (RSA Data Security license) and the Rijndael cipher implementation
of Szymon Stefanek (Public Domain). For detailed license information \see files
pdfencrypt.cpp and pdfrijndael.h.

*/

/** \page overview Reference Manual
The documentation of wxPdfDocument is created by Doxygen. To make it easier to locate the description
of a specific method the following alphabetical list shows all available methods:

\section refpdfdoc wxPdfDocument

\li wxPdfDocument::AcceptPageBreak - accept or not automatic page break
\li wxPdfDocument::AddFont - add a new font
\li wxPdfDocument::AddFontCJK - add a CJK (Chinese, Japanese or Korean) font
\li wxPdfDocument::AddLayer - add a layer (optional content group)
\li wxPdfDocument::AddLayerTitle - add a layer title
\li wxPdfDocument::AddLayerMembership - add a layer group
\li wxPdfDocument::AddLayerRadioGroup - add a layer radio group
\li wxPdfDocument::AddLink - create an internal link
\li wxPdfDocument::AddPage - add a new page
\li wxPdfDocument::AddPattern - add a simple pattern
\li wxPdfDocument::AddSpotColour - add a spot colour
\li wxPdfDocument::AliasNbPages - define an alias for number of pages
\li wxPdfDocument::Annotate - add a text annotation
\li wxPdfDocument::AppendJavascript - add document level JavaScript
\li wxPdfDocument::Arrow - draw an arrow
\li wxPdfDocument::AttachFile - add a file attachment
\li wxPdfDocument::AxialGradient - define axial gradient shading

\li wxPdfDocument::BeginTemplate - start template creation
\li wxPdfDocument::Bookmark - add a bookmark

\li wxPdfDocument::Cell - print a cell
\li wxPdfDocument::CheckBox - add a check box to a form
\li wxPdfDocument::Circle - draw a circle
\li wxPdfDocument::ClippingText - define text as clipping area
\li wxPdfDocument::ClippingRect - define rectangle as clipping area
\li wxPdfDocument::ClippingEllipse - define ellipse as clipping area
\li wxPdfDocument::ClippingPath - start defining a clipping path
\li wxPdfDocument::ClippingPolygon - define polygon as clipping area
\li wxPdfDocument::ClippedCell - print a clipped cell
\li wxPdfDocument::Close - terminate the document
\li wxPdfDocument::CloseAndGetBuffer - terminate the document and return the document buffer
\li wxPdfDocument::ClosePath - close a clipping path
\li wxPdfDocument::ComboBox - add a combo box to a form
\li wxPdfDocument::CoonsPatchGradient - define coons patch mesh gradient shading
\li wxPdfDocument::Curve - draw a Bezier curve
\li wxPdfDocument::CurveTo - append a cubic Bezier curve to a clipping path

\li wxPdfDocument::Ellipse - draw an ellipse
\li wxPdfDocument::EndTemplate - end template creation
\li wxPdfDocument::EnterLayer - enter a layer

\li wxPdfDocument::Footer - page footer.

\li wxPdfDocument::GetBreakMargin - get the page break margin
\li wxPdfDocument::GetCellMargin - get the cell margin
\li wxPdfDocument::GetDrawColour - get current draw colour
\li wxPdfDocument::GetFillColour - get current fill colour
\li wxPdfDocument::GetFillingRule - get current filling rule
\li wxPdfDocument::GetFontDescription - get description of current font
\li wxPdfDocument::GetFontFamily - get current font family
\li wxPdfDocument::GetFontSize - get current font size in points
\li wxPdfDocument::GetFontStyle - get current font style
\li wxPdfDocument::GetFontStyles - get current font styles
\li wxPdfDocument::GetFontSubsetting - get font embedding mode
\li wxPdfDocument::GetImageScale - get image scale
\li wxPdfDocument::GetLastImageBottomRightX - get the X coordinate of the bottom right corner of the last inserted image
\li wxPdfDocument::GetLastImageBottomRightY - get the Y coordinate of the bottom right corner of the last inserted image
\li wxPdfDocument::GetLeftMargin - get the left margin
\li wxPdfDocument::GetLineHeight - get line height
\li wxPdfDocument::GetLineStyle - get current line style
\li wxPdfDocument::GetLineWidth - get current line width
\li wxPdfDocument::GetPageHeight - get page height
\li wxPdfDocument::GetPageWidth - get page width
\li wxPdfDocument::GetPatternColour - get pattern as colour
\li wxPdfDocument::GetRightMargin - get the right margin
\li wxPdfDocument::GetScaleFactor - get scale factor
\li wxPdfDocument::GetSourceInfo - get info dictionary of external document
\li wxPdfDocument::GetStringWidth - compute string length
\li wxPdfDocument::GetTemplateBBox - get bounding box of template
\li wxPdfDocument::GetTemplateSize - get size of template
\li wxPdfDocument::GetTextColour - get current text colour
\li wxPdfDocument::GetTextRenderMode - get current text render mode
\li wxPdfDocument::GetTopMargin - get the top margin
\li wxPdfDocument::GetX - get current x position
\li wxPdfDocument::GetY - get current y position

\li wxPdfDocument::Header - page header

\li wxPdfDocument::Image - output an image
\li wxPdfDocument::ImageMask - define an image mask
\li wxPdfDocument::ImportPage - import page of external document for use as template
\li wxPdfDocument::IsInFooter - check whether footer output is in progress

\li wxPdfDocument::LeaveLayer - leave layer
\li wxPdfDocument::Line - draw a line
\li wxPdfDocument::LinearGradient - define linear gradient shading
\li wxPdfDocument::LineCount - count the number of lines a text would occupy
\li wxPdfDocument::LineTo - append straight line segment to a clipping path
\li wxPdfDocument::Link - put a link
\li wxPdfDocument::Ln - line break
\li wxPdfDocument::LockLayer - lock a layer

\li wxPdfDocument::Marker - draw a marker symbol
\li wxPdfDocument::MidAxialGradient - define mid axial gradient shading
\li wxPdfDocument::MirrorH - mirror horizontally
\li wxPdfDocument::MirrorV - mirror vertically
\li wxPdfDocument::MoveTo - begin new subpath of a clipping path
\li wxPdfDocument::MultiCell - print text with line breaks

\li wxPdfDocument::Open - start output to the PDF document

\li wxPdfDocument::PageNo - page number
\li wxPdfDocument::Polygon - draw a polygon
\li wxPdfDocument::PushButton - add a push button to a form

\li wxPdfDocument::RadialGradient - define radial gradient shading
\li wxPdfDocument::RadioButton - add a radio button to a form
\li wxPdfDocument::Rect - draw a rectangle
\li wxPdfDocument::RegularPolygon -  draw a regular polygon
\li wxPdfDocument::Rotate - rotate around a given center
\li wxPdfDocument::RotatedImage - rotate image
\li wxPdfDocument::RotatedText - rotate text string
\li wxPdfDocument::RoundedRect - draw a rounded rectangle

\li wxPdfDocument::SaveAsFile - save the document to a file
\li wxPdfDocument::Scale - scale in X and Y direction
\li wxPdfDocument::ScaleX - scale in X direction only
\li wxPdfDocument::ScaleXY - scale equally in X and Y direction
\li wxPdfDocument::ScaleY - scale in Y direction only
\li wxPdfDocument::Sector - draw a sector
\li wxPdfDocument::SetAlpha - set alpha transparency
\li wxPdfDocument::SetAlphaState - set alpha state
\li wxPdfDocument::SetAuthor - set the document author
\li wxPdfDocument::SetAutoPageBreak - set the automatic page breaking mode
\li wxPdfDocument::SetCellMargin - set cell margin
\li wxPdfDocument::SetCompression - turn compression on or off
\li wxPdfDocument::SetCreator - set document creator
\li wxPdfDocument::SetDisplayMode - set display mode
\li wxPdfDocument::SetDrawColour - set drawing colour
\li wxPdfDocument::SetDrawPattern - set draw colour pattern
\li wxPdfDocument::SetFillColour - set filling colour
\li wxPdfDocument::SetFillGradient - paint a rectangular area using a fill gradient
\li wxPdfDocument::SetFillingRule - set filling rule
\li wxPdfDocument::SetFillPattern - set fill colour pattern
\li wxPdfDocument::SetFont - set font
\li wxPdfDocument::SetFontSize - set font size
\li wxPdfDocument::SetFontSubsetting - set font embedding mode
\li wxPdfDocument::SetFormBorderStyle - set form field border style
\li wxPdfDocument::SetFormColours - set form field colours (border, background, text)
\li wxPdfDocument::SetImageScale - set image scale
\li wxPdfDocument::SetKerning - set kerning mode
\li wxPdfDocument::SetKeywords - associate keywords with document
\li wxPdfDocument::SetLeftMargin - set left margin
\li wxPdfDocument::SetLineHeight - set line height
\li wxPdfDocument::SetLineStyle - set line style
\li wxPdfDocument::SetLineWidth - set line width
\li wxPdfDocument::SetLink - set internal link destination
\li wxPdfDocument::SetMargins - set margins
\li wxPdfDocument::SetProtection - set permissions and/or passwords
\li wxPdfDocument::SetRightMargin - set right margin
\li wxPdfDocument::SetSourceFile - set source file of external template document
\li wxPdfDocument::SetSubject - set document subject
\li wxPdfDocument::SetTemplateBBox - set bounding box of template
\li wxPdfDocument::SetTextColour - set text colour
\li wxPdfDocument::SetTextPattern - set text colour pattern
\li wxPdfDocument::SetTextRenderMode - set text render mode
\li wxPdfDocument::SetTitle - set document title
\li wxPdfDocument::SetTopMargin - set top margin
\li wxPdfDocument::SetViewerPreferences - set viewer preferences
\li wxPdfDocument::SetX - set current x position
\li wxPdfDocument::SetXY - set current x and y positions
\li wxPdfDocument::SetY - set current y position
\li wxPdfDocument::Shape - draw shape
\li wxPdfDocument::ShapedText - print text along a path
\li wxPdfDocument::Skew - skew in X and Y direction
\li wxPdfDocument::SkewX - skew in Y direction only
\li wxPdfDocument::SkewY - skew in Y direction only
\li wxPdfDocument::StarPolygon - draw star polygon
\li wxPdfDocument::StartTransform - begin transformation
\li wxPdfDocument::StopTransform - end transformation

\li wxPdfDocument::Text - print a string
\li wxPdfDocument::TextBox - print a string horizontally and vertically aligned in a box
\li wxPdfDocument::TextField - add a text field to a form
\li wxPdfDocument::Transform - set transformation matrix
\li wxPdfDocument::Translate - move the origin 
\li wxPdfDocument::TranslateX - move the X origin only
\li wxPdfDocument::TranslateY - move the Y origin only

\li wxPdfDocument::UnsetClipping - remove clipping area
\li wxPdfDocument::UseTemplate - use template

\li wxPdfDocument::Write - print flowing text
\li wxPdfDocument::WriteCell - print flowing text with cell attributes
\li wxPdfDocument::WriteGlyphArray - print array of glyphs
\li wxPdfDocument::WriteXml - print flowing text containing simple XML markup

\li wxPdfDocument::wxPdfDocument - constructor
\li wxPdfDocument::~wxPdfDocument - destructor

\section refpdffontmanager wxPdfFontManager

\li wxPdfFontManager::AddSearchPath - add path entries to the font search path list

\li wxPdfFontManager::GetDefaultEmbed - get the default embedding mode
\li wxPdfFontManager::GetDefaultSubset - get the default subsetting mode
\li wxPdfFontManager::GetFont - get a font by name and style or index
\li wxPdfFontManager::GetFontCount - get the number of registered fonts
\li wxPdfFontManager::GetFontManager - get the font manager

\li wxPdfFontManager::InitializeFontData - initialize the font data of a font

\li wxPdfFontManager::RegisterFont - register a font
\li wxPdfFontManager::RegisterFontCJK - register a CJK font family
\li wxPdfFontManager::RegisterFontCollection - register a font collection
\li wxPdfFontManager::RegisterFontDirectory - register all fonts located in a directory
\li wxPdfFontManager::RegisterSystemFonts - register the fonts known to the operating system

\li wxPdfFontManager::SetDefaultEmbed - set the default embedding mode
\li wxPdfFontManager::SetDefaultSubset - set the default subsetting mode

\section refpdfbarcode wxPdfBarCodeCreator

\li wxPdfBarCodeCreator::Code128
\li wxPdfBarCodeCreator::Code128A
\li wxPdfBarCodeCreator::Code128B
\li wxPdfBarCodeCreator::Code128C
\li wxPdfBarCodeCreator::Code39
\li wxPdfBarCodeCreator::EAN128
\li wxPdfBarCodeCreator::EAN13
\li wxPdfBarCodeCreator::UPC_A
\li wxPdfBarCodeCreator::I25
\li wxPdfBarCodeCreator::PostNet

*/

/** \page makefont MakeFont Utility
\section mkfontadd Adding new fonts and encoding support

This section explains how to use \b TrueType or \b Type1 fonts so that you are not
limited to the standard fonts any more. The other interest is that you can
choose the font encoding, which allows you to use other languages than the
Western ones (the standard fonts having too few available characters). 
 
There are two ways to use a new font: embedding it in the PDF or not. When a
font is not embedded, it is sought in the system. The advantage is that the
PDF file is lighter; on the other hand, if it is not available, a substitution
font is used. So it is preferable to ensure that the needed font is installed
on the client systems. If the file is to be viewed by a large audience, it is
better to embed the fonts. 
 
Adding a new font requires three steps for \b TrueType fonts: 

\li Generation of the metric file (.afm) 
\li Generation of the font definition file (.xml) 
\li Declaration of the font in the program 

For \b Type1, the first one is theoretically not necessary because the AFM file is
usually shipped with the font. In case you have only a metric file in PFM format,
it must be converted to AFM first.

\section mkfontgen1 Generation of the metric file

The first step for a \b TrueType font consists in generating the AFM file (or UFM file in case of a 
<b>Unicode TrueType</b> font). A utility exists to do this task: <tt>ttf2ufm</tt> - a special version of
<tt>ttf2pt1</tt> - allowing to create AFM and/or UFM files. <tt>ttf2ufm</tt> has been modified to
generate AFM and UFM files containing all the information which is required by the utility program
\b makefont. An archive containing the modified source code of <tt>ttf2ufm</tt> and a Windows executable can be
downloaded from <b><a href="http://wxcode.sourceforge.net/docs/wxpdfdoc/ttf2ufm.zip">here</a></b>.
The command line to use is the following: 
 
<tt>ttf2ufm -a font.ttf font </tt>
 
For example, for Comic Sans MS Regular: 
 
<tt>ttf2ufm -a c:/windows/fonts/comic.ttf comic </tt>
 
Two files are created; the one we are interested in is comic.afm. 

\remark Starting with wxPdfDocument version 0.8.5 this step may be ommitted for
TrueType and OpenType fonts.

\section mkfontgen2 Generation of the font definition file

The second step consists in generating a wxPdfDocument font metrics XML file containing
all the information needed by wxPdfDocument; in addition, the font file is compressed.
To do this, a utility program, \b makefont, is provided.

<tt>makefont {-a font.afm | -u font.ufm | -i } [-f font.{ttf|pfb}] [-e encoding] [-p patch] [-t {ttf|otf|t1}] [-o outdir]</tt>

<table border=0>
<tr><td><tt>-a font.afm</tt></td><td>AFM font metric file for \b TrueType or \b Type1 fonts</td></tr>
<tr><td><tt>-u font.ufm</tt></td><td>UFM font metric file for <b>TrueType Unicode</b> or <b>OpenType Unicode</b> fonts</td></tr>
<tr><td><tt>-i</tt></td><td>Extract font metrics directly from <b>TrueType Unicode</b> or <b>OpenType Unicode</b> fonts</td></tr>
<tr><td valign="top"><tt>-f font.{ttf|otf|pfb}</tt></td><td>font file (<tt>.ttf</tt> = TrueType, <tt>.otf</tt> = OpenType, <tt>.pfb</tt> = Type1).
<br>If you own a Type1 font in ASCII format (<tt>.pfa</tt>), you can convert it to binary format with
<a href="http://www.lcdf.org/~eddietwo/type/#t1utils">t1utils</a>.
<br>If you don't want to embed the font, omit this parameter. In this case, type is given by the type parameter. 
</td></tr>
<tr><td valign="top"><tt>-e encoding</tt></td><td>font encoding, i.e. cp1252. Omit this parameter for a symbolic font.like <i>Symbol</i>
or <i>ZapfDingBats</i>.

The encoding defines the association between a code (from 0 to 255) and a character.
The first 128 are fixed and correspond to ASCII; the following are variable.
The encodings are stored in .map files. Those available are: 

\li cp1250 (Central Europe) 
\li cp1251 (Cyrillic) 
\li cp1252 (Western Europe) 
\li cp1253 (Greek) 
\li cp1254 (Turkish) 
\li cp1255 (Hebrew) 
\li cp1257 (Baltic) 
\li cp1258 (Vietnamese) 
\li cp874 (Thai) 
\li iso-8859-1 (Western Europe) 
\li iso-8859-2 (Central Europe) 
\li iso-8859-4 (Baltic) 
\li iso-8859-5 (Cyrillic) 
\li iso-8859-7 (Greek) 
\li iso-8859-9 (Turkish) 
\li iso-8859-11 (Thai) 
\li iso-8859-15 (Western Europe) 
\li iso-8859-16 (Central Europe) 
\li koi8-r (Russian) 
\li koi8-u (Ukrainian) 

Of course, the font must contain the characters corresponding to the chosen encoding. 
The encodings which begin with cp are those used by Windows; Linux systems usually use ISO. 
Remark: the standard fonts use cp1252. 

\b Note: For TrueType Unicode and OpenType Unicode fonts this parameter is ignored.
</td></tr>
<tr><td valign="top"><tt>-p patch</tt></td><td>patch file for individual encoding changes.
Use the same format as the <tt>.map</tt> files for encodings.
A patch file gives the possibility to alter the encoding.
Sometimes you may want to add some characters. For instance, ISO-8859-1 does not contain
the euro symbol. To add it at position 164, create a file containing the line
<p><tt>!A0 U+20AC Euro</tt>
<p>\b Note: The Unicode character id will not be interpreted.

For TrueType Unicode and OpenType Unicode fonts this parameter is ignored.
</td></tr>
<tr><td><tt>-t {ttf|otf|t1}</tt></td><td>font type (ttf = TrueType, otf = OpenType, t1 = Type1). Only needed if omitting the font file.</td></tr>
<tr><td><tt>-o outdir</tt></td><td>the output directory for all generated files (default: current working directory)</td></tr>
</table>

\b Note: in the case of a font with the same name as a standard one, for instance arial.ttf,
it is mandatory to embed. If you don't, Acrobat will use its own font. 

Executing <tt>makefont</tt> generates an .xml file, with the same name as the
<tt>.afm</tt> file resp. <tt>.ufm</tt> file. You may rename it if you wish. In case of
embedding the font file is compressed and gives a file with <tt>.z</tt> as extension.
For <b>Unicode TrueType</b> fonts a file with extension <tt>.ctg.z</tt> is created containing
the character to glyph mapping.You may rename these files, too, but in this case you have to
alter the file name(s) in the file tag in the <tt>.xml</tt> file accordingly.

You have to copy the generated file(s) to the font directory.

\section mkfontdecl Declaration of the font in the script

The last step is the most simple. You just need to call the AddFont() method. For instance: 
 
<tt>pdf.AddFont(wxT("Comic"),wxT(""),wxT("comic.xml"));</tt>
  
or simply: 
 
<tt>pdf.AddFont(wxT("Comic"));</tt>
  
And the font is now available (in regular and underlined styles), usable like the others.
If we had worked with Comic Sans MS Bold (comicbd.ttf), we would have put: 
 
<tt>pdf.AddFont(wxT("Comic"),wxT("B"),wxT("comicbd.xml"));</tt>
  
\section mkfontreduce Reducing the size of TrueType fonts

Font files are often quite voluminous; this is due to the
fact that they contain the characters corresponding to many encodings. zlib compression
reduces them but they remain fairly big. A technique exists to reduce them further.
It consists in converting the font to the \b Type1 format with <tt>ttf2pt1</tt> by specifying the
encoding you are interested in; all other characters will be discarded. 
For instance, the arial.ttf font shipped with Windows 98 is 267KB (it contains 1296
characters). After compression it gives 147. Let's convert it to \b Type1 by keeping
only cp1250 characters: 
 
<tt>ttf2ufm -b -L cp1250.map c:/windows/fonts/arial.ttf arial </tt>
 
The <tt>.map</tt> files are located in the <tt>makefont</tt> directory.
The command produces arial.pfb and arial.afm. The arial.pfb file is only 35KB,
and 30KB after compression. 
 
It is possible to go even further. If you are interested only in a subset of the
encoding (you probably don't need all 217 characters), you can open the .map file
and remove the lines you are not interested in. This will reduce the file size
accordingly. 

Since wxPdfDocument version 0.8.0 automatic font subsetting is supported for
TrueType und TrueType Unicode fonts. Since version 0.8.5 subsetting of OpenType Unicode
fonts is supported as well. <b>Note</b>: The font license must allow embedding and
subsetting.
*/

/** \page showfont ShowFont Utility

\b ShowFont can be used to generate font samples in PDF form showing the Unicode
coverage of the font similar in appearance to the Unicode charts. The concept of
this application is based on <a href="http://fntsample.sourceforge.net">FntSample</a>,
developed by Eugeniy Meshcheryakov for use with <a href="http://dejavu-fonts.org">DejaVu Fonts</a>
project, but the code is written from scratch in C++ using <a href="http://www.wxwidgets.org">wxWidgets</a>
and wxPdfDocument.

\section useshowfont Usage

<tt>showfont -f FONTFILE -o OUTPUTFILE [-n FONTINDEX] [-e ENCODING] [-i RANGES] [-x RANGES]</tt>

<tt>showfont { -h | --help }</tt>

<table border=0>
<tr><td valign="top"><tt>-f&nbsp;FONTFILE</tt></td><td>The font file for which a sample should be generated.
It can be the name of a \b TrueType, \b OpenType or \b Type1 font file, but wxPdfDocument's
font description files are supported, too.</td></tr>
<tr><td valign="top"><tt>-o&nbsp;OUTFILE</tt></td><td>The name of the file to which the PDF output is written.
\note It should have the extension \b .pdf.</td></tr>
<tr><td valign="top"><tt>-n&nbsp;INDEX</tt></td><td>The index of the font within the FONTFILE in case of
TrueType Collections (.ttc) which contain multiple fonts. By default font with index 0 is used.</td></tr>

<tr><td valign="top"><tt>-e&nbsp;ENCODING</tt></td><td>the font encoding of the font.
\note This option is required only for \b Type1 fonts and is ignored for other font types.

The encoding defines the association between a code (from 0 to 255) and an Unicode character.
The first 128 are fixed and correspond to ASCII; the next 128 are variable. The following
encodings are supported by \b ShowFont: 

<table border="0">
<tr bgcolor="#6699dd"><td><b>Encoding</b></td><td><b>Description</b></td><td>&nbsp;</td><td><b>Encoding</b></td><td><b>Description</b></td></tr>
<tr bgcolor="#eeeeee"><td><tt>standard</tt></td><td>Adobe standard Latin encoding</td><td>&nbsp;</td><td><tt>iso-8859-1</tt></td><td>Western European / Latin-1</td></tr>
<tr bgcolor="#ddeeff"><td><tt>winansi</tt></td><td>Windows ANSI aka Windows Code Page 1252</td><td>&nbsp;</td><td><tt>iso-8859-2</tt></td><td>Central European / Latin-2</td></tr>
<tr bgcolor="#eeeeee"><td><tt>macroman</tt></td><td>Mac OS encoding for Latin</td><td>&nbsp;</td><td><tt>iso-8859-3</tt></td><td>South European / Latin-3</td></tr>
<tr bgcolor="#ddeeff"><td><tt>symbol</tt></td><td>Symbol set encoding</td><td>&nbsp;</td><td><tt>iso-8859-4</tt></td><td>Baltic</td></tr>
<tr bgcolor="#eeeeee"><td><tt>zapfdingbats</tt></td><td>ZapfDingbats encoding</td><td>&nbsp;</td><td><tt>iso-8859-5</tt></td><td>Cyrillic</td></tr>
<tr bgcolor="#ddeeff"><td><tt>cp-1250</tt></td><td>Central and East European Latin</td><td>&nbsp;</td><td><tt>iso-8859-6</tt></td><td>Arabic</td></tr>
<tr bgcolor="#eeeeee"><td><tt>cp-1251</tt></td><td>Cyrillic</td><td>&nbsp;</td><td><tt>iso-8859-7</tt></td><td>Greek</td></tr>
<tr bgcolor="#ddeeff"><td><tt>cp-1252</tt></td><td>Western European Latin</td><td>&nbsp;</td><td><tt>iso-8859-8</tt></td><td>Hebrew</td></tr>
<tr bgcolor="#eeeeee"><td><tt>cp-1253</tt></td><td>Greek</td><td>&nbsp;</td><td><tt>iso-8859-9</tt></td><td>Turkish</td></tr>
<tr bgcolor="#ddeeff"><td><tt>cp-1254</tt></td><td>Turkish</td><td>&nbsp;</td><td><tt>iso-8859-10</tt></td><td>Nordic</td></tr>
<tr bgcolor="#eeeeee"><td><tt>cp-1255</tt></td><td>Hebrew</td><td>&nbsp;</td><td><tt>iso-8859-11</tt></td><td>Thai</td></tr>
<tr bgcolor="#ddeeff"><td><tt>cp-1256</tt></td><td>Arabic</td><td>&nbsp;</td><td><tt>iso-8859-13</tt></td><td>Baltic Rim</td></tr>
<tr bgcolor="#eeeeee"><td><tt>cp-1257</tt></td><td>Baltic</td><td>&nbsp;</td><td><tt>iso-8859-14</tt></td><td>Celtic</td></tr>
<tr bgcolor="#ddeeff"><td><tt>cp-1258</tt></td><td>Vietnamese</td><td>&nbsp;</td><td><tt>iso-8859-15</tt></td><td>Western European / Latin-9</td></tr>
<tr bgcolor="#eeeeee"><td><tt>cp-874</tt></td><td>Thai</td><td>&nbsp;</td><td><tt>iso-8859-16</tt></td><td>South Eastern European / Latin-10</td></tr>
<tr bgcolor="#ddeeff"><td><tt>cp-932</tt></td><td>Japanese</td><td>&nbsp;</td><td><tt>koi8-r</tt></td><td>Russian</td></tr>
<tr bgcolor="#eeeeee"><td><tt>cp-936</tt></td><td>Simplified Chinese</td><td>&nbsp;</td><td><tt>koi8-u</tt></td><td>Ukrainian</td></tr>
<tr bgcolor="#ddeeff"><td><tt>cp-949</tt></td><td>Korean</td><td>&nbsp;</td><td></td><td></td></tr>
<tr bgcolor="#eeeeee"><td><tt>cp-950</tt></td><td>Traditional Chinese</td><td>&nbsp;</td><td></td><td></td></tr>
</table>

</td></tr>

<tr><td valign="top"><tt>-i RANGES</tt></td><td>Show character codes in RANGES. (see \ref showfontranges)</td></tr>
<tr><td valign="top"><tt>-x RANGES</tt></td><td>Don't show character codes in RANGES. (see \ref showfontranges)</td></tr>

<tr><td valign="top"><tt>-h | --help</tt></td><td>Display a usage information and exit.</td></tr>
</table>

\section showfontranges Ranges

The parameter RANGES for \b -i (--include-range) and \b -x (--exclude-range) can be given
as a list of one or more ranges delimited by a comma (,). 

Each range can be given as a single integer or a pair of integers delimited by minus sign (-).

Integers can be specified in decimal, hexadecimal (0x...) or octal (0...) format.

One integer of a pair can be omitted (-N specifies all characters with codes less or equal
to N, and N- all characters with codes greater or equal to N).

\section showfontcolour Colours

Character code cells can have one of several background colours:

\li <tt>white</tt> = the character code is present in the font,
\li <tt>light grey</tt> = the character code is defined in Unicode but not present in the font,
\li <tt>blue-grey</tt> = the character code is a control character,
\li <tt>dark grey</tt> = the character code is not defined in Unicode.

\section showfontexample Examples

Show all character codes of myfont.ttf in output file myfont.pdf:

<tt>showfont -f myfont.ttf -o myfont.pdf</tt>

Show all character codes of myfont.ttf less than or equal to U+05FF
but exclude U+0300-U+036F in output file myfont.pdf:

<tt>showfont -f myfont.ttf -o myfont.pdf -i -0x05FF -x 0x0300-0x036F</tt>

*/

/** \page writexml Styling text using a simple markup language
\section tagoverview Overview

The method wxPdfDocument::WriteXML allows to write text to PDF using a simple markup language.
This allows for example to change font attributes within a cell, which is not supported by
methods like wxPdfDocument::WriteCell or wxPdfDocument::MultiCell. The supported markup
language consists of a small subset of HTML. Although the subset might be extended in future
versions of \b wxPdfDocument, it is not the goal of this method to allow to convert
full fledged HTML pages to PDF. 

\b Important! The XML dialect used is very strict. Each tag must have a corresponding closing tag
and all attribute values must be enclosed in double quotes.

Usually the current position should be at the left margin when calling wxPdfDocument::WriteXML.
If the current position is \b not at left margin and the text passed to wxPdfDocument::WriteXML
occupies more than a single line, you may get strange results. Until version \b 1.0 of wxPdfDocument
will be released the behaviour of wxPdfDocument::WriteXML might change without prior notice.

Currently there is only limited error handling. You will get strange results or no results at all
if tags are incorrectly used. Unknown tags and all their content are silently ignored.

\section tagref Reference of supported tags

The following sections describe the tags supported by the wxPdfDocument markup language.

\subsection simpletags Simple text markup

There are several tags to influence the size and weight of the font used for displaying the text
and the relative vertical position within a line:

<table border="0">
<tr bgcolor="#6699dd"><td><b>Tag</b></td><td><b>Description</b></td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;b&gt; ... &lt;/b&gt;</tt></td><td>bold text</td></tr>
<tr bgcolor="#ddeeff"><td><tt>&lt;i&gt; ... &lt;/i&gt;</tt></td><td>italic text</td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;u&gt; ... &lt;/u&gt;</tt></td><td>underlined text</td></tr>
<tr bgcolor="#ddeeff"><td><tt>&lt;o&gt; ... &lt;/o&gt;</tt></td><td>overlined text</td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;s&gt; ... &lt;/s&gt;</tt></td><td>strike-through text</td></tr>
<tr bgcolor="#ddeeff"><td><tt>&lt;strong&gt; ... &lt;/strong&gt;</tt></td><td>bold text (same as <tt>&lt;b&gt;</tt>)</td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;em&gt; ... &lt;/em&gt;</tt></td><td>emphasized text (same as <tt>&lt;i&gt;</tt>)</td></tr>
<tr bgcolor="#ddeeff"><td><tt>&lt;small&gt; ... &lt;/small&gt;</tt></td><td>text with reduced font size</td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;sup&gt; ... &lt;/sup&gt;</tt></td><td>superscripted text</td></tr>
<tr bgcolor="#ddeeff"><td><tt>&lt;sub&gt; ... &lt;/sub&gt;</tt></td><td>subscripted text</td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;h1&gt; ... &lt;/h1&gt;</tt></td><td>headline level 1</td></tr>
<tr bgcolor="#ddeeff"><td><tt>&lt;h2&gt; ... &lt;/h2&gt;</tt></td><td>headline level 2</td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;h3&gt; ... &lt;/h3&gt;</tt></td><td>headline level 3</td></tr>
<tr bgcolor="#ddeeff"><td><tt>&lt;h4&gt; ... &lt;/h4&gt;</tt></td><td>headline level 4</td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;h5&gt; ... &lt;/h5&gt;</tt></td><td>headline level 5</td></tr>
<tr bgcolor="#ddeeff"><td><tt>&lt;h6&gt; ... &lt;/h6&gt;</tt></td><td>headline level 6</td></tr>
</table>

\subsection structtags Structuring text markup

Some tags for structuring the text layout are available. Most of these tags have one or more
attributes to change its properties. Click on the tag description to see a detailed description
of the attributes.

<table border="0">
<tr bgcolor="#6699dd"><td><b>Tag</b></td><td><b>Description</b></td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;ul&gt; ... &lt;/ul&gt;</tt></td><td>\ref ulist</td></tr>
<tr bgcolor="#ddeeff"><td><tt>&lt;ol&gt; ... &lt;/ol&gt;</tt></td><td>\ref olist</td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;li&gt; ... &lt;/li&gt;</tt></td><td><b>List item</b> of an ordered or unordered list</td></tr>
<tr bgcolor="#ddeeff"><td><tt>&lt;br /&gt;</tt></td><td><b>Line break</b>, positions the current position to the left margin of the next line</td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;p&gt; ... &lt;/p&gt;</tt></td><td>\ref ptag</td></tr>
<tr bgcolor="#ddeeff"><td><tt>&lt;hr /&gt;</tt></td><td>\ref hrtag</td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;a&gt; ... &lt;/a&gt;</tt></td><td>\ref atag</td></tr>
<tr bgcolor="#ddeeff"><td><tt>&lt;font&gt; ... &lt;/font&gt;</tt></td><td>\ref fonttag</td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;table&gt; ... &lt;/table&gt;</tt></td><td>\ref tabletag</td></tr>
</table>

\subsection misctags Miscelleaneous text markup

This section lists a few additional tags not fitting in any other category.
Click on the tag description to see a detailed description of the attributes.

<table border="0">
<tr bgcolor="#6699dd"><td><b>Tag</b></td><td><b>Description</b></td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;msg&gt; ... &lt;/msg&gt;</tt></td><td>\ref msgtag</td></tr>
<tr bgcolor="#ddeeff"><td><tt>&lt;img ... /&gt;</tt></td><td>\ref imgtag</td></tr>
</table>

\subsection ulist Unordered lists

Unordered lists start on a new line. Each list item is preceded by a list item marker and the content of the
list item is indented.

<table border="0">
<tr bgcolor="#6699dd"><td colspan="2"><b>Tag</b></td></tr>
<tr bgcolor="#eeeeee"><td colspan="2"><b>&lt;ul&gt;</b></td></tr>
<tr bgcolor="#6699dd"><td><b>Attribute</b></td><td><b>Description</b></td></tr>
<tr bgcolor="#eeeeee"><td valign="top"><tt>type="bullet|dash|<i>number</i>"</tt></td><td>Sets the type of the list item marker
<p><tt><b>bullet</b></tt> displays a bullet character</p>
<p><tt><b>dash</b></tt> displays a dash character</p>
<p><tt><i>number</i></tt> has a value between 0 and 255. The corresponding character of the \b ZapfDingBats font
is used as the list item marker</p></td></tr>
</table>

\subsection olist Ordered lists

Ordered lists start on a new line. Each list item is preceded by a list item enumerator and the content of the
list item is indented.

<table border="0">
<tr bgcolor="#6699dd"><td colspan="2"><b>Tag</b></td></tr>
<tr bgcolor="#eeeeee"><td colspan="2"><b>&lt;ol&gt;</b></td></tr>
<tr bgcolor="#6699dd"><td><b>Attribute</b></td><td><b>Description</b></td></tr>
<tr bgcolor="#eeeeee"><td valign="top"><tt>type="1|a|A|i|I|z1|z2|z3|z4"</tt></td><td>Sets the type of the list item enumerator
<p><tt><b>1</b></tt> displays a decimal number as the list item enumerator</p>
<p><tt><b>a</b></tt> displays a lowercase alphabetic character as the list item enumerator</p>
<p><tt><b>A</b></tt> displays a uppercase alphabetic character as the list item enumerator</p>
<p><tt><b>i</b></tt> displays a lowercase roman number as the list item enumerator</p>
<p><tt><b>I</b></tt> displays a uppercase roman number as the list item enumerator</p>
<p><tt><b>z1|z2|z3|z4</b></tt> displays number symbols of one of the 4 number series in the \b ZapfDingBats font. This option should only be used for lists of at most 10 items.</p>
</td></tr>
<tr bgcolor="#ddeeff"><td><tt>start="<i>number</i>"</tt></td><td><i>number</i> represents the enumerator value of the first list item</td></tr>
</table>

\subsection ptag Paragraph

A paragraph starts on a new line and forces an empty line after the closing paragraph tag.

<table border="0">
<tr bgcolor="#6699dd"><td colspan="2"><b>Tag</b></td></tr>
<tr bgcolor="#eeeeee"><td colspan="2"><b>&lt;p&gt;</b></td></tr>
<tr bgcolor="#6699dd"><td><b>Attribute</b></td><td><b>Description</b></td></tr>
<tr bgcolor="#eeeeee"><td valign="top"><tt>align="left|right|center|justify"</tt></td><td>As specified by this
option the content of the paragraph will be \b left or \b right aligned, \b centered or \b justified.
The default is \b left aligned.</td></tr>
</table>

\subsection hrtag Horizontal rule

A horizontal rule is a line of specified width which is drawn on a separate line.

<table border="0">
<tr bgcolor="#6699dd"><td colspan="2"><b>Tag</b></td></tr>
<tr bgcolor="#eeeeee"><td colspan="2"><b>&lt;hr&gt;</b></td></tr>
<tr bgcolor="#6699dd"><td><b>Attribute</b></td><td><b>Description</b></td></tr>
<tr bgcolor="#eeeeee"><td valign="top"><tt>width="<i>number</i>"</tt></td><td>The width of the horizontal rule
is an integer <i>number</i> between 1 and 100 giving the width in percent of the available width (from left to right margin).
The default value is 100.</td></tr>
</table>

\subsection atag Internal or external link

An internal or external link is displayed as blue underlined text. Clicking on the text opens a browser window
loading the referenced URL.

<table border="0">
<tr bgcolor="#6699dd"><td colspan="2"><b>Tag</b></td></tr>
<tr bgcolor="#eeeeee"><td colspan="2"><b>&lt;a&gt;</b></td></tr>
<tr bgcolor="#6699dd"><td><b>Attribute</b></td><td><b>Description</b></td></tr>
<tr bgcolor="#eeeeee"><td><tt>href="<i>url</i>"</tt></td><td><i>url</i> is an unified resource locator.
If <i>url</i> starts with <b>#</b> it is interpreted as a reference to an internal link anchor;
the characters following <b>#</b> are used as the name of the anchor.</td></tr>
<tr bgcolor="#6699dd"><td><tt>name="<i>anchor</i>"</tt></td><td><i>anchor</i> is the name of an internal link anchor.</td></tr>
</table>

<b>Note:</b> Either the <b><tt>name</tt></b> or the <b><tt>href</tt></b> attribute may be specified, but not both.

\subsection fonttag Font specification

This tag allows to specify several font attributes for the embedded content. Font family,
font size and colour can be set. Attributes not given retain their previous value.

<table border="0">
<tr bgcolor="#6699dd"><td colspan="2"><b>Tag</b></td></tr>
<tr bgcolor="#eeeeee"><td colspan="2"><b>&lt;font&gt;</b></td></tr>
<tr bgcolor="#6699dd"><td><b>Attribute</b></td><td><b>Description</b></td></tr>
<tr bgcolor="#eeeeee"><td valign="top"><tt>face="<i>fontfamily</i>"</tt></td><td>The name of the font family. It can be the name of one of the
14 core fonts or the name of a font previously added by wxPdfDocument::AddFont.</td></tr>
<tr bgcolor="#ddeeff"><td><tt>size="<i>fontsize</i>"</tt></td><td>The font size in points</td></tr>
<tr bgcolor="#eeeeee"><td><tt>color="<i>fontcolour</i>"</tt></td><td>The font colour in HTML notation, i.e. <b><i>\#rrggbb</i></b>,
or as a named colour, i.e. <b><i>red</i></b>.</td></tr>
</table>

\subsection msgtag Translatable text

For international applications a simple mechanism is provided to pass a string to <b><code>wxGetTranslation</code></b>.

<table border="0">
<tr bgcolor="#6699dd"><td colspan="2"><b>Tag</b></td></tr>
<tr bgcolor="#eeeeee"><td colspan="2"><b>&lt;msg&gt;</b></td></tr>
</table>

The text string included in the <b><tt>msg</tt></b> tag will be translated if a translation is available before it is written to PDF.

<b>Note:</b> Within the <b><tt>msg</tt></b> tag additional markup is not allowed.

\subsection imgtag Images

In the current implementation output of an image always starts on a new line.

<table border="0">
<tr bgcolor="#6699dd"><td colspan="2"><b>Tag</b></td></tr>
<tr bgcolor="#eeeeee"><td colspan="2"><b>&lt;img&gt;</b></td></tr>
<tr bgcolor="#6699dd"><td><b>Attribute</b></td><td><b>Description</b></td></tr>
<tr bgcolor="#eeeeee"><td valign="top"><tt>src="<i>imagefile</i>"</tt></td><td>The name of the image file.</td></tr>
<tr bgcolor="#ddeeff"><td valign="top"><tt>width="<i>image width</i>"</tt></td><td>The width of the image measured in pixels.</td></tr>
<tr bgcolor="#eeeeee"><td valign="top"><tt>height="<i>image height</i>"</tt></td><td>The height of the image measured in pixels.</td></tr>
<tr bgcolor="#ddeeff"><td valign="top"><tt>align="left|right|center"</tt></td><td>As specified by this
option the image will be \b left or \b right aligned, or \b centered.
The default is \b left aligned.</td></tr>
</table>

\section tabletag Tables

Very often information is presented in a tabular structure. This is also supported by the wxPdfDocument markup language
by using a specific kind of HTML table syntax. The structure is as follows:
<pre>
    &lt;table&gt;
      &lt;colgroup&gt;
        &lt;col ... /&gt;
        ...
      &lt;/colgroup&gt;
      &lt;thead&gt;
        &lt;tr&gt;&lt;td&gt; ... &lt;/td&gt;&lt;/tr&gt;
        ...      
      &lt;/thead&gt;
      &lt;tbody&gt;
        &lt;tr&gt;&lt;td&gt; ... &lt;/td&gt;&lt;/tr&gt;
        ...      
      &lt;/tbody&gt;
    &lt;/table&gt;
</pre>
The <b><tt>colgroup</tt></b> tag and embedded <b><tt>col</tt></b> tags are always required since all column widths have to be specified
a priori. <b><tt>width</tt></b> attributes are not interpreted when used in other table tags.

The <b><tt>thead</tt></b> tag and embedded table rows and cells are allowed, but since the current implementation only supports
tables fitting completely on one page, the rows are handled as ordinary rows. (A future release will support tables
spanning more than one page. Header rows will be repeated on each page.)

The use of the <b><tt>tbody</tt></b> tag is always required.

Nested tables are supported.

The <b><tt>table</tt></b> tag may have the following attributes:

<table border="0">
<tr bgcolor="#6699dd"><td colspan="2"><b>Tag</b></td></tr>
<tr bgcolor="#eeeeee"><td colspan="2"><b>&lt;table&gt;</b></td></tr>
<tr bgcolor="#6699dd"><td><b>Attribute</b></td><td><b>Description</b></td></tr>
<tr bgcolor="#eeeeee"><td valign="top"><tt>border="<i>number</i>"</tt></td><td>Table cells may have borders on each side.
This attribute specifies whether cells will have borders on every side or not. This may be overriden for each individual cell.
The attribute value consists of the combination of up to 4 letters:
<p>\b 0 - no borders<br>
<b> &gt; 0</b> - borders on all sides of each cell<br>
</td></tr>
<tr bgcolor="#ddeeff"><td valign="top"><tt>align="left|right|center"</tt></td><td>Defines the horizontal alignment of the table. Default is the alignment of the surrounding context.</td></tr>
<tr bgcolor="#eeeeee"><td><tt>valign="top|middle|bottom"</tt></td><td>Defines the vertical alignment of the table. Default is <i>top</i>.</td></tr>
<tr bgcolor="#ddeeff"><td valign="top"><tt>cellpadding="<i>number</i>"</tt></td><td><i>Number</i> defines the padding width on each side of a cell. Default is 0.</td></tr>
</table>

The supported tags and their attributes are shown in the following tables:

<table border="0">
<tr bgcolor="#6699dd"><td><b>Tag</b></td><td><b>Description</b></td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;table&gt; ... &lt;/table&gt;</tt></td><td>Groups the definitions of column widths. Contains one or more &lt;col&gt; tags.</td></tr>
<tr bgcolor="#eeeeee"><td><tt>&lt;colgroup&gt; ... &lt;/colgroup&gt;</tt></td><td>Groups the definitions of column widths. Contains one or more &lt;col&gt; tags.</td></tr>
<tr bgcolor="#ddeeff"><td><tt>&lt;col width="<i>width</i>" span="<i>number</i>"&gt; ... &lt;/col&gt;</tt></td><td>
Defines the <i>width</i> of one or more columns. <i>number</i> specifies for how many columns the width is specified, default is 1.
</td></tr>
<tr bgcolor="#eeeeee"><td valign="top"><tt>&lt;thead odd="<i>background colour for odd numbered rows</i>" even="<i>background colour for even numbered rows</i>"&gt; ... &lt;/thead&gt;</tt></td>
<td>Defines a group of table header rows.
Contains one or more &lt;tr&gt; tags. If a table does not fit on a single page these rows are repeated on each page.
The attributes <b><tt>odd</tt></b> and <b><tt>even</tt></b> are optional.
</td></tr>
<tr bgcolor="#ddeeff"><td valign="top"><tt>&lt;tbody odd="<i>background colour for odd numbered rows</i>" even="<i>background colour for even numbered rows</i>"&gt; ... &lt;/tbody&gt;</tt></td>
<td>Defines a group of table body rows. Contains one or more &lt;tr&gt; tags.
The attributes <b><tt>odd</tt></b> and <b><tt>even</tt></b> are optional.
</td></tr>
<tr bgcolor="#eeeeee"><td valign="top"><tt>&lt;tr bgcolor="<i>background colour</i>" height="<i>height</i>"&gt; ... &lt;/tr&gt;</tt></td>
<td>Defines a table row. Contains one or more &lt;td&gt; tags.
<p>The <i>background colour</i> may be specified in HTML notation, i.e. <b><i>\#rrggbb</i></b>,
or as a named colour, i.e. <b><i>red</i></b>. If no background colour is given the background is transparent.</p>
<p>Usually the height of the highest cell in a row is used as the row height, but a minimal row <i>height</i> may be specified, too</p>
</td></tr>
<tr bgcolor="#ddeeff"><td valign="top"><tt>&lt;td&gt; ... &lt;/td&gt;</tt></td><td>Defines a table cell. 
<p>The available attributes are described in section \ref tdtag.</p></td></tr>
</table>

\subsection tdtag Table cells

A table cell can have several attributes:

<table border="0">
<tr bgcolor="#6699dd"><td colspan="2"><b>Tag</b></td></tr>
<tr bgcolor="#eeeeee"><td colspan="2"><b>&lt;td&gt;</b></td></tr>
<tr bgcolor="#6699dd"><td><b>Attribute</b></td><td><b>Description</b></td></tr>
<tr bgcolor="#eeeeee"><td valign="top"><tt>border="LTBR"</tt></td><td>A cell may have a border on each side.
This attribute overrides the border specification in the &lt;table&gt; tag. The attribute value consists of 
the combination of up to 4 letters:
<p>\b L - border on the left side of the cell<br>
\b T - border on the top side of the cell<br>
\b B - border on the bottom side of the cell<br>
\b R - border on the right side of the cell</p>
.</td></tr>
<tr bgcolor="#ddeeff"><td valign="top"><tt>align="left|right|center"</tt></td><td>Defines the horizontal alignment of the cell content. Default is <i>left</i>.</td></tr>
<tr bgcolor="#eeeeee"><td><tt>valign="top|middle|bottom"</tt></td><td>Defines the vertical alignment of the cell content. Default is <i>top</i>.</td></tr>
<tr bgcolor="#ddeeff"><td valign="top"><tt>bgcolor="<i>background colour</i>"</tt></td><td>The background colour of the cell in HTML notation, i.e. <b><i>\#rrggbb</i></b>,
or as a named colour, i.e. <b><i>red</i></b>. This attribute overrides the background colour specification of the row.
If neither a row nor a cell background colour is specified the background is transparent.</td></tr>
<tr bgcolor="#eeeeee"><td valign="top"><tt>rowspan="<i>number</i>"</tt></td><td><i>Number</i> of rows this cell should span. Default is 1.</td></tr>
<tr bgcolor="#ddeeff"><td valign="top"><tt>colspan="<i>number</i>"</tt></td><td><i>Number</i> of columns this cell should span. Default is 1.</td></tr>
</table>

*/

#ifndef _PDFDOC_DEF_H_
#define _PDFDOC_DEF_H_

#if defined(WXMAKINGLIB_PDFDOC)
  #define WXDLLIMPEXP_PDFDOC
  #define WXDLLIMPEXP_DATA_PDFDOC(type) type
#elif defined(WXMAKINGDLL_PDFDOC)
  #define WXDLLIMPEXP_PDFDOC WXEXPORT
  #define WXDLLIMPEXP_DATA_PDFDOC(type) WXEXPORT type
#elif defined(WXUSINGDLL) && !defined(WXUSINGLIB_PDFDOC)
  #define WXDLLIMPEXP_PDFDOC WXIMPORT
  #define WXDLLIMPEXP_DATA_PDFDOC(type) WXIMPORT type
#else // not making nor using DLL
  #define WXDLLIMPEXP_PDFDOC
  #define WXDLLIMPEXP_DATA_PDFDOC(type) type
#endif

// Setting inheritance of wxPdfDocument
// 0 = do not derive wxPdfDocument from wxObject (default)
// 1 = derive wxPdfDocumentinherit from wxObject
#ifndef WXPDFDOC_INHERIT_WXOBJECT
#define WXPDFDOC_INHERIT_WXOBJECT 0
#endif

/*
  GCC warns about using __declspec on forward declarations
  while MSVC complains about forward declarations without
  __declspec for the classes later declared with it. To hide this
  difference a separate macro for forward declarations is defined:
 */
#if defined(HAVE_VISIBILITY) || (defined(__WINDOWS__) && defined(__GNUC__))
  #define WXDLLIMPEXP_FWD_PDFDOC
#else
  #define WXDLLIMPEXP_FWD_PDFDOC WXDLLIMPEXP_PDFDOC
#endif

#endif // _PDFDOC_DEF_H_

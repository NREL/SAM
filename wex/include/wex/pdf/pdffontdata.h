///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontdata.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-08
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontdata.h Definition of font data

#ifndef _PDF_FONT_DATA_H_
#define _PDF_FONT_DATA_H_

// wxWidgets headers
#include <wx/dynarray.h>
#include <wx/hashmap.h>
#include <wx/mstream.h>
#include <wx/object.h>
#include <wx/xml/xml.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdfarraytypes.h"
#include "wex/pdf/pdffontdescription.h"

class WXDLLIMPEXP_FWD_PDFDOC wxPdfEncoding;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfEncodingChecker;

WX_DECLARE_HASH_MAP_WITH_DECL(wxUint32, int, wxIntegerHash, wxIntegerEqual, wxPdfKernWidthMap, class WXDLLIMPEXP_PDFDOC);
WX_DECLARE_HASH_MAP_WITH_DECL(wxUint32, wxPdfKernWidthMap*, wxIntegerHash, wxIntegerEqual, wxPdfKernPairMap, class WXDLLIMPEXP_PDFDOC);

WX_DECLARE_HASH_MAP_WITH_DECL(wxUint32, wxUint16, wxIntegerHash, wxIntegerEqual, wxPdfGlyphWidthMap, class WXDLLIMPEXP_PDFDOC);
WX_DECLARE_HASH_MAP_WITH_DECL(wxUint32, wxUint32, wxIntegerHash, wxIntegerEqual, wxPdfChar2GlyphMap, class WXDLLIMPEXP_PDFDOC);

/// Class representing a glyph list entry
class WXDLLIMPEXP_PDFDOC wxPdfGlyphListEntry
{
public:
  /// Default constructor
  wxPdfGlyphListEntry() {};

  /// Destructor
  ~wxPdfGlyphListEntry() {};

  int m_gid; ///< glyph number
  int m_uid; ///< unicode character
};

WX_DEFINE_SORTED_USER_EXPORTED_ARRAY(wxPdfGlyphListEntry*, wxPdfGlyphList, WXDLLIMPEXP_PDFDOC);

/// Base class for all fonts. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfFontData
{
public:
  /// Default constructor
  wxPdfFontData();

  /// Default destructor
  virtual ~wxPdfFontData();

  /// Check initialization state
  /**
  * \return TRUE if the font data are initialized, FALSE otherwise
  */
  bool IsInitialized() const { return m_initialized; }

  /// Set initialization state
  /**
  * \param initialized state of initialization to set
  */
  void SetInitialized(bool initialized);

  /// Set font type
  /**
  * \param type type of the font
  * \note Valid types are
  * \li core - Adobe Standard font (i.e. Courier, Helvetica, ...)
  * \li OpenTypeUnicode - TrueType/OpenType font using the compact font format (CFF)
  * \li TrueType - TrueType/OpenType font using the TrueType font format and standard encoding
  * \li TrueTypeUnicode - TrueType/OpenType font using the TrueType font format and Unicode encoding
  * \li Type0 - Type0 font, usually a CJK font
  * \li Type1 - PostScript font
  */
  void SetType(const wxString& type);

  /// Get font type
  /**
  * \return type of the font
  * \see SetType()
  */
  wxString GetType() const;

  /// Set font family name
  /**
  * \param name the name of the font family
  */
  void SetFamily(const wxString& name);

  /// Get font family name
  /**
  * \return the name of the font family
  */
  wxString GetFamily() const;

  /// Set font family alias
  /**
  * \param alias the alias name of the font family
  */
  void SetAlias(const wxString& alias);

  /// Get font family alias
  /**
  * \return the alias of the font family name or an empty string
  */
  wxString GetAlias() const;

  /// Set font name
  /**
  * \param name the full name of the font, usually the PostScript name
  */
  void SetName(const wxString& name);

  /// Get font name
  /**
  * \return the full name of the font, usually the PostScript name
  */
  wxString GetName() const;

  /// Set list of full font names
  /**
  * \param fullNames a list of full font names
  */
  void SetFullNames(const wxArrayString& fullNames);

  /// Get list of full font names
  /**
  * \return a list of full names of the font
  */
  wxArrayString GetFullNames() const;

  /// Set font style
  /**
  * \param style the style of the font. Vaild values are
  *   \li wxPDF_FONTSTYLE_REGULAR
  *   \li wxPDF_FONTSTYLE_BOLD
  *   \li wxPDF_FONTSTYLE_ITALIC
  *   \li wxPDF_FONTSTYLE_BOLDITALIC
  */
  void SetStyle(int style);

  /// Set font style
  /**
  * \param style the style of the font. If the style string
  *   \li contains "bold" or "black" the style is set to bold
  *   \li contains "italic" or "oblique" the style is set to italic
  *   \li equals "b" the style is set to bold
  *   \li equals "i" the style is set to italic
  *   \li equals "bi" or "ib" ´the style is set to bolditalic
  */
  void SetStyle(const wxString& style);

  /// Set font style based on font name
  /**
  * The font's full name is searched for substrings "regular", "bold", "black", "italic" and "oblique".
  * Depending on the findings the style is set.
  */
  void SetStyleFromName();

  /// Get font style
  /**
  * \return the font style
  * \see SetStyle()
  */
  int GetStyle() const;

  /// Set embed support flag
  /**
  * \return TRUE if embedding of this font is required, FALSE otherwise
  */
  bool EmbedRequired() { return m_embedRequired; }

  /// Check whether the font embedding is required
  /**
  * \return TRUE if embedding of this font is supported/allowed, FALSE otherwise
  */
  bool EmbedSupported() const { return m_embedSupported; }

  /// Check whether the font subsetting is supported
  /**
  * \return TRUE if subsetting of this font is supported/allowed, FALSE otherwise
  */
  bool SubsetSupported() const { return m_subsetSupported; }

  /// Set embed support flag
  /**
  * \param embedSupported flag whether the font supports embedding
  */
  void SetEmbedSupported(bool embedSupported) { m_embedSupported = embedSupported; }

  /// Set subset support flag
  /**
  * \param subsetSupported flag whether the font supports subsetting
  */
  void SetSubsetSupported(bool subsetSupported) { m_subsetSupported = subsetSupported; }

#if defined(__WXMSW__) || defined(__WXMAC__)
  /// Set associated wxFont object
  /**
  * \param font the wxFont to be associated with the font
  */
  void SetFont(const wxFont& font) { m_font = font; }

  /// Get associated wxFont object
  /**
  * \return the associated wxFont
  */
  wxFont GetFont() const { return m_font; }
#endif

  /// Set fully qualified font file name
  /**
  * \param fontFileName the fully qualified name of the font file
  */
  void SetFontFileName(const wxString& fontFileName) { m_fontFileName = fontFileName; }

  /// Get fully qualified font file name
  /**
  * \return the fully qualified name of the font file
  */
  wxString GetFontFileName() const { return m_fontFileName; }

  /// Set font index in case the font is member of a font collection
  /**
  * \param fontIndex the index of the font if it is a member of a font collection
  */
  void SetFontIndex(int fontIndex) { m_fontIndex = fontIndex; }

  /// Get font index
  /**
  * \return the index of the font within a font collection
  */
  int GetFontIndex() const { return m_fontIndex; }

  /// Check whether the font is in compact font format
  /**
  * \return TRUE if the font is in CFF format, FALSE otherwise
  */
  bool HasCff() const { return m_cff; }

  /// Get the offset of the CFF section within the font file
  /**
  * \return the offset of the CFF section
  */
  size_t GetCffOffset() const { return m_cffOffset; }

  /// Get the length of the CFF section
  /**
  * \return the length of the CFF section
  */
  size_t GetCffLength() const { return m_cffLength; }

  /// Set underline position
  /**
  * \param up the position of the underline decoration
  */
  void SetUnderlinePosition(int up);

  /// Get underline position
  /**
  * \return the position of the underline decoration
  */
  int  GetUnderlinePosition() const;

  /// Set underline thickness
  /**
  * \param ut the thickness of the underline decoration
  */
  void SetUnderlineThickness(int ut);

  /// Get underline thickness
  /**
  * \return the thickness of the underline decoration
  */
  int  GetUnderlineThickness() const;

  /// Get bounding box top position
  /**
  * \return the top position from the bounding box
  */
  int GetBBoxTopPosition() const;

  /// Set encoding
  /**
  * \param encoding the name of the font encoding
  */
  void SetEncoding(const wxString& encoding);

  /// Get encoding
  /**
  * \return the name of the font encoding
  */
  wxString GetEncoding() const;

  /// Get encoding
  /**
  * \return the name of the font encoding
  */
  const wxPdfEncoding* GetBaseEncoding() const;

  /// Check whether the font has differences to WinAnsi encoding
  /**
  * \return TRUE if the font has differences to the WinAnsi encoding, FALSE otherwise
  */
  bool HasDiffs() const;

  /// Set encoding differences
  /**
  * \param diffs the string describing the differences to the WinAnsi encoding
  */
  void SetDiffs(const wxString& diffs);

  /// Get encoding differences
  /**
  * \return the string describing the differences to the WinAnsi encoding
  */
  wxString GetDiffs() const;

  /// Set path of font files
  /**
  * \param path the path where the font file is located
  */
  void SetFilePath(const wxString& path);

  /// Get path of font files
  /**
  * \return the path where the font file is located
  */
  wxString GetFilePath() const;

  /// Check whether the font has an associated font file
  /**
  * \return TRUE if the font has an associated font file, FALSE otherwise
  */
  bool HasFile() const;

  /// Set the name of the font file created by MakeFont
  /**
  * \param file the name of the font file created by the MakeFont utility
  */
  void SetFontFile(const wxString& file);

  /// Get the name of the associated font file
  /**
  * \return the name of the font file created by the MakeFont utility
  */
  wxString GetFontFile() const;

  /// Set the name of the character to glyph mapping file created by MakeFont
  /**
  * \param ctg the name of the character to glyph mapping file created by the MakeFont utility
  */
  void SetCtgFile(const wxString& ctg);

  /// Get the name of the character to glyph mapping file created by MakeFont
  /**
  * \return the name of the character to glyph mapping file created by the MakeFont utility
  */
  wxString GetCtgFile() const;

  /// Get font file size 1
  /**
  * \param size1 the size of the font file
  * \note For Type1 fonts this represents the size of the ASCII section.
  */
  void SetSize1(size_t size1);

  /// Get font file size 1
  /**
  * \return the size of the font file
  */
  size_t  GetSize1() const;

  /// Check whether the file has a size 2
  /**
  * \return TRUE if the font has a second size associated, FALSE otherwise
  * \note Usually only Type1 fonts have a second size entry.
  */
  bool HasSize2() const;

  /// Set font file size 2
  /**
  * \param size2 the second size of the font file
  * \note For Type1 fonts this represents the size of the BINARY section.
  */
  void SetSize2(size_t size2);

  /// Get font file size 2 (Type 1 only)
  /**
  * \return the second size of the font file
  */
  size_t  GetSize2() const;

  /// Set font CMap
  /**
  * \param cmap the CMap descriptor
  * \note For Type0 fonts only
  */
  void SetCMap(const wxString& cmap);

  /// Get the font's CMap (Type 0 only)
  /**
  * return the font's CMap descriptor
  */
  wxString GetCMap() const;

  /// Set font ordering
  /**
  * \param ordering the font's ordering descriptor
  */
  void SetOrdering(const wxString& ordering);

  /// Get font ordering
  /**
  * \return the font's ordering descriptor
  */
  wxString GetOrdering() const;

  /// Set font supplement
  /**
  * \param supplement the font's supplement descriptor
  * \note For Type0 fonts only
  */
  void SetSupplement(const wxString& supplement);

  /// Get font supplement (Type 0 only)
  /**
  * \return the font's supplement descriptor
  * \note For Type0 fonts only
  */
  wxString GetSupplement() const;

  /// Set glyph width map
  /**
  * \param cw the glyph width map
  */
  void SetGlyphWidthMap(wxPdfGlyphWidthMap* cw);

  /// Get glyph width map
  /**
  * \return the glyph width map
  */
  const wxPdfGlyphWidthMap* GetGlyphWidthMap() const;

  /// Set character to glyph number map
  /**
  * \param gn the character to glyph number map
  */
  void SetChar2GlyphMap(wxPdfChar2GlyphMap* gn);

  /// Get character to glyph number map
  /**
  * \return the character to glyph number map
  */
  const wxPdfChar2GlyphMap* GetChar2GlyphMap() const;

  /// Set kerning pair map
  /**
  * \param kp the kerning pair map
  */
  void SetKernPairMap(wxPdfKernPairMap* kp);

  /// Get kerning pair map
  /**
  * \return the kerning pair map
  */
  const wxPdfKernPairMap* GetKernPairMap() const;

  /// Get width of string taking kerning into account
  /**
  * \param s the string which's width is to be calculated
  * \return the width of the string with kerning taken into account
  */
  int GetKerningWidth(const wxString& s) const;

  /// Get kerning width array
  /**
  * \param s the string for which kerning information should be provided
  * \return an array with indices and kerning width of the found kerning pairs. 
  * The array consists of information pairs: the first item (even indices) represents the index
  * of the first character of a kerning pair within the string and the second item (odd indices)
  * represents the kerning value.
  */
  wxArrayInt GetKerningWidthArray(const wxString& s) const;

  /// Set subset flag if font subsetting is supported
  /**
  * \param subset flag whether subsetting should be used
  */
  void SetSubset(bool subset);

  /// Initialize font data
  /**
  * \return TRUE if the font data has been initialized successfully, FALSE otherwise
  */
  virtual bool Initialize();

  /// Check whether VOLT data are available
  /**
  * \return TRUE if the font data contain VOLT data, FALSE otherwise
  */
  virtual bool HasVoltData() const { return false; }

  /// Applay VOLT data
  /**
  * \param s text string for which VOLT data should be applied
  * \return text string modified according to the VOLT data
  */
  virtual wxString ApplyVoltData(const wxString& s) const { return s; }

  /// Get the width of a string
  /**
  * \param s the string for which the width should be calculated
  * \param encoding the character to glyph mapping
  * \param withKerning flag indicating whether kerning should be taken into account
  * \return the width of the string
  */
  virtual double GetStringWidth(const wxString& s, const wxPdfEncoding* encoding = NULL, bool withKerning = false) const;

  /// Check whether the font can show all characters of a given string
  /**
  * \param s the string to be checked
  * \param encoding the character to glyph mapping
  * \return TRUE if the font can show all characters of the string, FALSE otherwise
  */
  virtual bool CanShow(const wxString& s, const wxPdfEncoding* encoding = NULL) const;

  /// Force string to valid string in respect of the current font encoding
  /**
  * The given string is converted in such a way that it contains only characters
  * available in the current font encoding
  * \param s the string to be converted
  * \param replace the character used to replace invalid characters
  * \return converted string
  */
  virtual wxString ConvertToValid(const wxString& s, wxChar replace = wxT('?')) const;

  /// Convert character codes to glyph numbers
  /**
  * \param s the string to be converted
  * \param encoding the character to glyph mapping
  * \param usedGlyphs the list of used glyphs
  * \param subsetGlyphs the mapping of glyphs to subset glyphs
  * \return the converted string
  */
  virtual wxString ConvertCID2GID(const wxString& s, const wxPdfEncoding* encoding = NULL, 
                                  wxPdfSortedArrayInt* usedGlyphs = NULL, 
                                  wxPdfChar2GlyphMap* subsetGlyphs = NULL) const;

  /// Convert glyph number to string
  /**
  * \param glyph the glyph to be converted
  * \param encoding the character to glyph mapping
  * \param usedGlyphs the list of used glyphs
  * \param subsetGlyphs the mapping of glyphs to subset glyphs
  * \return the converted string
  */
  virtual wxString ConvertGlyph(wxUint32 glyph, const wxPdfEncoding* encoding = NULL, 
                                wxPdfSortedArrayInt* usedGlyphs = NULL, 
                                wxPdfChar2GlyphMap* subsetGlyphs = NULL) const;

  /// Get the character width array as string
  /**
  * \param subset flag whether subsetting is enabled
  * \param usedGlyphs the list of used glyphs
  * \param subsetGlyphs the mapping of glyphs to subset glyphs
  * \return the string representation of the character widths
  */
  virtual wxString GetWidthsAsString(bool subset = false, 
                                     wxPdfSortedArrayInt* usedGlyphs = NULL, 
                                     wxPdfChar2GlyphMap* subsetGlyphs = NULL) const;
  
  /// Get list of glyph names supported by this font
  /**
  * \param[out] glyphNames a list of glyph names
  * \return TRUE if the list of glyph names is available, FALSE otherwise
  */
  virtual bool GetGlyphNames(wxArrayString& glyphNames) const;

  /// Write font data 
  /**
  * \param fontData the output stream
  * \param usedGlyphs the list of used glyphs
  * \param subsetGlyphs the mapping of glyphs to subset glyphs
  * \return the size of the written font data
  */
  virtual size_t WriteFontData(wxOutputStream* fontData, 
                               wxPdfSortedArrayInt* usedGlyphs = NULL, 
                               wxPdfChar2GlyphMap* subsetGlyphs = NULL);

  /// Write character/glyph to unicode mapping
  /**
  * \param mapData the output stream
  * \param encoding the character to glyph mapping
  * \param usedGlyphs the list of used glyphs
  * \param subsetGlyphs the mapping of glyphs to subset glyphs
  * \return the size of the written data
  */
  virtual size_t WriteUnicodeMap(wxOutputStream* mapData, 
                                 const wxPdfEncoding* encoding = NULL, 
                                 wxPdfSortedArrayInt* usedGlyphs = NULL, 
                                 wxPdfChar2GlyphMap* subsetGlyphs = NULL);

  /// Set the font description
  /**
  * \param desc the font description
  */
  void SetDescription(const wxPdfFontDescription& desc);

  /// Get the font description
  /**
  * \return the font description
  */
  virtual const wxPdfFontDescription& GetDescription() const;

  /// Load the font metrics XML file
  /**
  * \param root the root node of the XML font metric file
  * \return TRUE if the metric file could be processed successfully, FALSE otherwise
  */
  virtual bool LoadFontMetrics(wxXmlNode* root);

#if wxUSE_UNICODE
  /// Get the associated encoding converter
  /**
  * \return the encoding converter associated with this font
  */
  virtual wxMBConv* GetEncodingConv() const;

  /// Create the associated default encoding converter
  virtual void CreateDefaultEncodingConv();
#endif

  /// Set the glyph widths
  /**
  * \param glyphWidths array with glyph widths
  */
  virtual void SetGlyphWidths(const wxPdfArrayUint16& glyphWidths);

  /// Get the font description from XML
  /**
  * \param node root node of the XML font description
  * \param[out] fontDescription the resulting font description
  * \return TRUE if the font description could be processed successfully, FALSE otherwise
  */
  bool GetFontDescription(const wxXmlNode *node, wxPdfFontDescription& fontDescription);

  /// Get the default WinAnsi encoding converter
  /**
  * \return the default WinAnsi converter
  */
  static wxMBConv* GetWinEncodingConv();

  /// Get the content of an XML node
  /**
  * \param node the XML node containing content
  * \return the content of the XML node
  */
  static wxString GetNodeContent(const wxXmlNode *node);

protected:
  /// Find the encoding map to be used for character to glyph conversion
  const wxPdfChar2GlyphMap* FindEncodingMap(const wxPdfEncoding* encoding) const;

  /// Determine font style from font name
  static int FindStyleFromName(const wxString& name);

  /// Compare glyph list entries
  static int CompareGlyphListEntries(wxPdfGlyphListEntry* item1, wxPdfGlyphListEntry* item2);

  /// Write a buffer to a stream
  static void WriteStreamBuffer(wxOutputStream& stream, const char* buffer);

  /// Write a mapping from glyphs to unicode to a stream
  static void WriteToUnicode(wxPdfGlyphList& glyphs, wxMemoryOutputStream& toUnicode, bool simple = false);

  wxString              m_type;      ///< Font type
  wxString              m_family;    ///< Font family
  wxString              m_alias;     ///< Font family alias
  wxString              m_name;      ///< Font name
  wxArrayString         m_fullNames; ///< List of full font names
  int                   m_style;     ///< Font style flags

  bool                  m_initialized;     ///< Flag whether the font has been initialized
  bool                  m_embedRequired;   ///< Flag whether embedding of the font is allowed and supported
  bool                  m_embedSupported;  ///< Flag whether embedding of the font is allowed and supported
  bool                  m_subsetSupported; ///< Flag whether subsetting of the font is allowed and supported

  wxString              m_fontFileName; ///< Qualified name of the font file
  int                   m_fontIndex;    ///< Index of the font in case of a font collection
  wxFont                m_font;         ///< Associated wxFont object (currently used by wxMSW only)

  wxPdfGlyphWidthMap*   m_cw;    ///< Mapping of character ids to character widths
  wxPdfChar2GlyphMap*   m_gn;    ///< Mapping of character ids to glyph numbers
  wxPdfKernPairMap*     m_kp;    ///< Kerning pair map

  wxPdfFontDescription  m_desc;  ///< Font description

  wxString              m_enc;   ///< Encoding
  wxString              m_diffs; ///< Encoding differences

  wxString              m_path;  ///< Path of font files
  wxString              m_file;  ///< Filename of font program
  wxString              m_ctg;   ///< Filename of char to glyph mapping
  size_t                m_size1; ///< TrueType file size or Type1 file size 1
  size_t                m_size2; ///< Type1 file size 2

  bool                  m_cff;             ///< Flag whether the font has a CFF table
  size_t                m_cffOffset;       ///< Offset of the CFF table of a TrueType/OpenType font
  size_t                m_cffLength;       ///< Lenght of the CFF table of a TrueType/OpenType font

  wxString              m_cmap;            ///< CMap of a CID font
  wxString              m_ordering;        ///< Ordering of a CID font 
  wxString              m_supplement;      ///< Supplement of a CID font

  wxPdfEncoding*        m_encoding;        ///< Encoding
  wxPdfEncodingChecker* m_encodingChecker; ///< Encoding checker
  static wxMBConv*      ms_winEncoding;    ///< WinAnsi converter

private:
  /// Thread safe increment of the reference count
  int IncrementRefCount();

  /// Thread safe decrement of the reference count
  int DecrementRefCount();

  int                   m_refCount;        ///< Reference count

  void SetEncoding(wxPdfEncoding* encoding);
  void SetEncodingChecker(wxPdfEncodingChecker* encodingChecker);
  wxPdfEncodingChecker* GetEncodingChecker() const;

  friend class WXDLLIMPEXP_FWD_PDFDOC wxPdfFont;
  friend class WXDLLIMPEXP_FWD_PDFDOC wxPdfFontExtended;
  friend class WXDLLIMPEXP_FWD_PDFDOC wxPdfFontListEntry;
  friend class                        wxPdfFontManagerBase;
};

#endif

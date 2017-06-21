///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontmanager.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-10
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontmanager.h Interface of wxPdfFontManager class

#ifndef _PDF_FONT_MANAGER_H_
#define _PDF_FONT_MANAGER_H_

// wxWidgets headers
#include <wx/arrstr.h>
#include <wx/string.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdffont.h"

class WXDLLIMPEXP_FWD_PDFDOC wxPdfFontData;
class wxPdfFontManagerBase;

/// Class representing the font manager
class WXDLLIMPEXP_PDFDOC wxPdfFontManager
{
public:
  /// Get the font manager
  /**
  * \return a reference to the font manager
  */
  static wxPdfFontManager* GetFontManager();

  /// Add a path entry to the font search path list
  /**
  * \param path path to add to the search list
  */
  void AddSearchPath(const wxString& path);

  /// Add a list of path entries to the font search path list
  /**
  * \param pathArray a list of path entries to add to the search list
  */
  void AddSearchPath(const wxArrayString& pathArray);

  /// Set the default embedding mode
  /**
  * Usually it is recommended to enable embedding, since this guarantees that
  * no font substitution takes place on a target system not providing a required font.
  *
  * The default embedding mode is enabled by default, but the embedding mode
  * can be set for each font individually.
  *
  * \note For most fonts the embedding mode is predefined. For example the Adobe Standard
  * fonts and the Adobe CJK fonts are never embedded independently of the setting of the
  * default embedding mode, and TrueType/OpenType Unicode fonts need to be always embedded.
  *
  * \param embed flag whether to enable or disable embedding
  * \return the state of the default embedding mode prior to this call
  */
  bool SetDefaultEmbed(bool embed);

  /// Get the default embedding mode
  /**
  * \return the current state of the default embedding mode
  */
  bool GetDefaultEmbed();

  /// Set the default subsetting mode
  /**
  * Usually it is recommended to enable subsetting, since embedding only a font subset
  * reduces the size of the resulting PDF file significantly.
  *
  * The default subsetting mode is enabled by default, but the subsetting mode
  * can be set for each font individually.
  *
  * \param subset flag whether to enable or disable font subsetting
  * \return the state of the default subsetting mode prior to this call
  */
  bool SetDefaultSubset(bool subset);

  /// Get the default subsetting mode
  /**
  * \return the state of the default subsetting mode
  */
  bool GetDefaultSubset();

  /// Register a font
  /**
  * Register a font for later use in creating a PDF document.
  * \param fontFileName the name of the font file (if the name is not fully qualified,
  *                     it will be searched in font search path)
  * \param aliasName an alias name for the font family (default: no alias)
  * \param fontIndex the index of the font in a font collection (default: 0)
  * \return the registered font
  */
  wxPdfFont RegisterFont(const wxString& fontFileName, const wxString& aliasName = wxEmptyString, int fontIndex = 0);

  /// Register a font basedon a wxFont object
  /**
  * Register a font based on a wxFont object for later use in creating a PDF document.
  * \param font the wxFont object to be registered
  * \param aliasName an alias name for the font family (default: no alias)
  * \return the registered font
  */
  wxPdfFont RegisterFont(const wxFont& font, const wxString& aliasName = wxEmptyString);

  /// Register a font collection
  /**
  * Register all fonts from a font collection for later use in creating a PDF document.
  * \param fontCollectionFileName the name of the font collection file (if the name is
  *                     not fully qualified, it will be searched in font search path)
  * \return the number of registered fonts
  */
  int RegisterFontCollection(const wxString& fontCollectionFileName);

  /// Register a CJK font family
  /**
  * Register all fonts from a font collection for later use in creating a PDF document.
  * The following CJK font families are currently known to wxPdfDocument:
  * \li GB, GB-HW - Simplified Chinese (Adobe-Font STSongStd-Light-Acro)
  * \li BIG5, BIG5-HW - Traditional Chinese (Adobe-Font MSungStd-Light-Acro)
  * \li SJIS - Japanese (Adobe-Font KozMinPro-Regular-Acro)
  * \li UHC, UHC-HW - Korean (Adobe-Font HYSMyeongJoStd-Medium-Acro)
  * \li GOTHIC, PGOTHIC, UIGOTHIC - Japanese (Microsoft-Font MS-Gothic/MS-PGothic/MS-UIGothic)
  * \li MINCHO, PMINCHO - Japanese (Microsoft-Font MS-Mincho/MS-PMincho)
  *
  * \param family the name of the CJK font family
  * \return TRUE if the CJK family could be registered successfully, FALSE otherwise
  */
  bool RegisterFontCJK(const wxString& family);

  /// Register the fonts known to the operating system
  /*
  * This method tries to register all installed fonts. On Windows platforms the registry
  * is searched to identify installed fonts, on Linux platforms the fontconfig library
  * is used to identify installed fonts.
  *
  * \note This method is currently implemented only for wxMSW and wxGTK.
  * \return the number of registered fonts
  */
  int RegisterSystemFonts();

  /// Register all fonts located in a directory
  /**
  * \param directory the directory path to be searched for font files
  * \param recursive flag whether the search should include subdirectories or not (Default: false)
  * \return the number of registered fonts
  */
  int RegisterFontDirectory(const wxString& directory, bool recursive = false);

  /// Get a font by name and style
  /**
  * Based on the given name and style the list of registered fonts is searched for a matching font.
  * The name is checked whether it represents a font family. If the name is not a family name
  * then the name is checked whether it represents an alias for a font family. If the name is not
  * an alias either the name is checked whether it is a full font name.
  * \param fontName the name of the requested font
  * \param fontStyle the style of the requested font
  * \return the requested font
  * \note If no appropriate font is registered an invalid font object is returned.
  * It's the user's responsibility to check the font object before further using it.
  */
  wxPdfFont GetFont(const wxString& fontName, int fontStyle = wxPDF_FONTSTYLE_REGULAR) const;

  /// Get a font by name and style
  /**
  * Based on the given name and style the list of registered fonts is searched for a matching font.
  * The name is checked whether it represents a font family. If the name is not a family name
  * then the name is checked whether it represents an alias for a font family. If the name is not
  * an alias either the name is checked whether it is a full font name.
  * \param fontName the name of the requested font
  * \param fontStyle the style of the requested font (string representation)
  * \return the requested font
  * \note If no appropriate font is registered an invalid font object is returned.
  * It's the user's responsibility to check the font object before further using it.
  */
  wxPdfFont GetFont(const wxString& fontName, const wxString& fontStyle) const;

  /// Get a font by index
  /**
  * \param fontIndex the index of the requested font in the list of registered fonts
  * \return the requested font
  * \note If the index is out of range an invalid font object is returned.
  * It's the user's responsibility to check the font object before further using it.
  */
  wxPdfFont GetFont(size_t fontIndex) const;

  /// Get the number of registered fonts
  /**
  * \return the number of registered fonts
  */
  size_t GetFontCount() const;

  /// Initialize the font data of a font
  /**
  * On registering a font the associated font data are not completely loaded into memory
  * to save time and memory resources, but as soon as the font is actually used it must
  * be fully initialized. Usually this method is called automatically.
  * \param font the font to be initialized
  * \return TRUE if the font could be initialized successfully, FALSE otherwise
  */
  bool InitializeFontData(const wxPdfFont& font);

  /// Register a font encoding
  /**
  * 
  * \param encoding the encoding to be registered
  * \return TRUE if the encoding could be registered successfully
  */
  bool RegisterEncoding(const wxPdfEncoding& encoding);

  /// Get the encoding having the given name
  /**
  * \param encodingName name of the encoding
  * \return the encoding 
  */
  const wxPdfEncoding* GetEncoding(const wxString& encodingName);

private:
  /// Default constructor
  wxPdfFontManager();
  
  /// Default destructor
  ~wxPdfFontManager();

  /// Copy destructor
  wxPdfFontManager(const wxPdfFontManager&);

  /// Assignment operator
  wxPdfFontManager& operator=(const wxPdfFontManager&);
  
private:
  wxPdfFontManagerBase*    m_fontManagerBase; ///< Font manager implementation
  
  static wxPdfFontManager* ms_fontManager;    ///< Font manager singleton

  friend class wxPdfDocumentModule;
};

#endif

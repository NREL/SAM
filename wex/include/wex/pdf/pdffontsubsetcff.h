///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontsubsetcfft.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-06-24
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontsubsetcff.h Interface of the OpenType Font support classes

#ifndef _PDF_FONT_SUBSET_CFF_H_
#define _PDF_FONT_SUBSET_CFF_H_

// wxWidgets headers
#include <wx/dynarray.h>
#include <wx/mstream.h>
#include <wx/string.h>
#include <wx/wfstream.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdffontdata.h"

class WXDLLIMPEXP_FWD_PDFDOC wxPdfCffDecoder;

class wxPdfCffDictElement;
class wxPdfCffFontObject;
class wxPdfCffIndexArray;

WX_DECLARE_HASH_MAP(long, wxPdfCffDictElement*, wxIntegerHash, wxIntegerEqual, wxPdfCffDictionary);

/// Class representing OpenType Font Subsets. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfFontSubsetCff
{
public:
  /// Default constructor
  wxPdfFontSubsetCff(const wxString& fileName);

  /// Default destructor
  virtual ~wxPdfFontSubsetCff();

  /// Destruct dictionary
  /**
  * \param dict reference of the dictionary to be destructed
  */
  void DestructDictionary(wxPdfCffDictionary* dict);

  /// Create subset of a font
  /**
  * \param inFont stream containing the font data
  * \param glyphsUsed a list of used glyphs
  * \param includeCmap flag whether to include the CMap table
  * \return the stream containing the font subset
  */
  wxMemoryOutputStream* CreateSubset(wxInputStream* inFont,
                                     wxPdfChar2GlyphMap* glyphsUsed,
                                     bool includeCmap = false);

protected:
  /// Read a font which is in CFF format
  bool ReadCffFont();

  /// Read the font header
  bool ReadHeader();

  /// Read the font name
  bool ReadFontName();

  /// Read the top dictionary
  bool ReadTopDict();

  /// Read the list of strings
  bool ReadStrings();

  /// read the global subroutines
  bool ReadGlobalSubroutines();

  /// Read a font index
  bool ReadFontIndex(wxPdfCffIndexArray* index);

  /// Read a font dictionary
  bool ReadFontDict(wxPdfCffDictionary* dict, int dictOffset, int dictSize);

  /// Read a FD selector
  bool ReadFdSelect();

  /// Read the font dictionary of a CID font
  bool ReadCidFontDict();

  /// Read a private dictionary
  bool ReadPrivateDict(wxPdfCffDictionary* privateDict, wxPdfCffIndexArray* localSubIndex, int offset, int size);

  /// Find a dictionary element
  wxPdfCffDictElement* FindDictElement(wxPdfCffDictionary* dict, int key);

  /// Set the argument of a dictionary element
  void SetDictElementArgument(wxPdfCffDictionary* dict, int key, wxMemoryOutputStream& buffer);

  /// Remove an element from a dictionary
  void RemoveDictElement(wxPdfCffDictionary* dict, int key);

  /// Decode an integer
  int DecodeInteger();

  /// Encode an integer using maximal size
  void EncodeIntegerMax(int value, wxMemoryOutputStream& buffer);

  /// Encode an integer
  void EncodeInteger(int value, wxMemoryOutputStream& buffer);

  /// Seek to offset in the default font input stream
  void SeekI(int offset);

  /// Get the current position in the default font input stream
  int TellI();

  /// Get the size of the default font input stream
  int GetSizeI();

  /// Read a byte from the default font input stream
  unsigned char ReadByte();

  /// Read a short integer from the default font input stream
  short ReadShort();

  /// Read an integer from the default font input stream
  int ReadInt();

  /// Read an offset of specific size from the default font input stream
  int ReadOffset(int offSize);

  /// Read the length of an operand from the default font input stream
  int ReadOperandLength();

  /// Read an operator from the default font input stream
  int ReadOperator();

  /// Read a string from the default font input stream
  wxString ReadString(int length);

  /// Generate the subset
  void GenerateFontSubset();

  /// Set the ROS strings
  void SetRosStrings();

  /// Build the subset of the charstrings' list
  void SubsetCharstrings();

  /// Build the subset of the font dictionary
  void SubsetFontDict();

  /// Create a dictionary for a CID font
  void CreateCidFontDict();

  /// Build the subset of the strings' list
  void SubsetStrings();

  /// Build the subset of a dictionary string
  void SubsetDictString(wxPdfCffDictionary* dict, int op);

  /// Build the subset of the strings' dictionary
  void SubsetDictStrings(wxPdfCffDictionary* dict);

  /// Seek offset in the default output stream
  void SeekO(int offset);

  /// Get the current position in the default output stream
  int TellO();

  /// Write the font's subset to the default output stream
  void WriteFontSubset();

  /// Write the font header
  void WriteHeader();

  /// Write the font's name
  void WriteName();

  /// Write the top dictionary
  void WriteTopDict();

  /// Write a dictionary
  void WriteDict(wxPdfCffDictionary* dict);

  /// Write a dictionary operator
  void WriteDictOperator(wxPdfCffDictElement* op);
  
  /// Set a top dictionary operator to the current position
  void SetTopDictOperatorToCurrentPosition(int op);

  /// Get the location of a dictionary in the default output stream
  int GetLocation(wxPdfCffDictionary* dict, int op);

  /// Write the list of strings
  void WriteStrings();

  /// Write the list of global subroutines
  void WriteGlobalSubrs();

  /// Write the charset
  void WriteCharset();

  /// Write the FD selector
  void WriteFdSelect();

  /// Write the list of charstrings
  void WriteCharStrings();

  /// Write the CID font dictionary
  void WriteCidFontDict();

  /// Write a CID private dictionary and local subroutines
  void WriteCidPrivateDictAndLocalSub();

  /// Write a private dictionary
  void WritePrivateDict(int dictNum, wxPdfCffDictionary* parentDict, wxPdfCffDictionary* privateDict);

  /// Write the local subroutines
  void WriteLocalSub(int dictNum, wxPdfCffDictionary* privateDict, wxPdfCffIndexArray* localSubIndex);

  /// Write an index
  void WriteIndex(wxPdfCffIndexArray* index);

  /// Write an integer
  void WriteInteger(int value, int size, wxMemoryOutputStream* buffer);

  /// Find  the local and global subroutines used
  void FindLocalAndGlobalSubrsUsed();

  /// Build the subset of the subroutines
  void SubsetSubrs(wxPdfCffIndexArray& subrIndex, wxPdfSortedArrayInt& subrsUsed);

  /// Find the subroutines used
  void FindSubrsUsed(int fd, wxPdfCffIndexArray& localSubIndex, 
                     wxPdfSortedArrayInt& hSubrsUsed, wxArrayInt& lSubrsUsed);

  /// Find the global subroutines use
  void FindGlobalSubrsUsed();

private:
  wxString              m_fileName;                ///< File name of the font file
  wxInputStream*        m_inFont;                  ///< Font file input stream
  wxMemoryOutputStream* m_outFont;                 ///< Subset output stream

  wxPdfCffDecoder*      m_decoder;                 ///< Decoder for CFF format

  int                   m_numGlyphsUsed;           ///< number of used glyphs
  wxArrayInt            m_usedGlyphs;              ///< array of used glyphs
  bool                  m_includeCmap;             ///< flag whether to include the CMap

  wxString              m_fontName;                ///< font name
  wxPdfCffDictionary*   m_topDict;                 ///< reference of the top dictionary
  wxPdfCffDictionary*   m_privateDict;             ///< reference of the private dictionary

  wxPdfCffIndexArray*   m_stringsIndex;            ///< index of strings
  wxPdfCffIndexArray*   m_charstringsIndex;        ///< index of charstrings
  wxPdfCffIndexArray*   m_globalSubrIndex;         ///< index of global subroutines
  wxPdfCffIndexArray*   m_localSubrIndex;          ///< index of local subroutines

  wxPdfCffIndexArray*   m_stringsSubsetIndex;      ///< index of strings for subset
  wxPdfCffIndexArray*   m_charstringsSubsetIndex;  ///< index of charstrings for subset

  wxArrayInt            m_fdSelect;                ///< array of FD selectors

  int                   m_hdrSize;                 ///< Header size
  bool                  m_isCid;                   ///< Flag whether the font is a CID font
  int                   m_numGlyphs;               ///< number of glyphs in the font
  int                   m_numFontDicts;            ///< number of font dictionaries

  wxArrayPtrVoid        m_fdDict;                  ///< FD dictionary
  wxArrayPtrVoid        m_fdPrivateDict;           ///< FD private dictionary
  wxArrayPtrVoid        m_fdLocalSubrIndex;        ///< FD index of local subroutines

  wxArrayInt            m_fdSelectSubset;          ///< FD selectors of subset
  int                   m_numSubsetFontDicts;      ///< number of font dictionaries for subset
  wxArrayInt            m_fdSubsetMap;             ///< map of FD selectors for subset
  wxArrayInt            m_privateDictOffset;       ///< offsets in private dictionary

  int                   m_globalBias;              ///< The bias for the global subroutines
  int                   m_numHints;                ///< Number of arguments to the stem operators in a subroutine calculated recursively

  wxPdfSortedArrayInt*  m_hGlobalSubrsUsed;        ///< A HashMap for keeping the Global subroutines used in the font
  wxArrayInt            m_lGlobalSubrsUsed;        ///< The Global SubroutinesUsed HashMaps as ArrayLists
  wxPdfSortedArrayInt*  m_hLocalSubrsUsed;         ///< A HashMap for keeping the subroutines used in a non-cid font
  wxArrayInt            m_lLocalSubrsUsed;         ///< The SubroutinesUsed HashMap as ArrayList
};

#endif

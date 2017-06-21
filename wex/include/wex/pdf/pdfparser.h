///////////////////////////////////////////////////////////////////////////////
// Name:        pdfparser.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2006-05-15
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfparser.h Interface of the wxPdfParser classes

#ifndef _PDF_PARSER_H_
#define _PDF_PARSER_H_

// wxWidgets headers
#include <wx/dynarray.h>
#include <wx/filesys.h>
#include <wx/mstream.h>
#include <wx/string.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdfarraydouble.h"
#include "wex/pdf/pdfobjects.h"

class WXDLLIMPEXP_FWD_PDFDOC wxPdfArray;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfDictionary;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfEncrypt;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfInfo;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfObject;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfObjectQueue;
class WXDLLIMPEXP_FWD_PDFDOC wxPdfStream;

/// Permissions required for import of a document
// Permission bit  3: Print
// Permission bit  5: Copy or extract text and graphics
// Permission bit 10: Extract text and graphics
// THIS MUST NOT BE CHANGED!
#define REQUIRED_PERMISSIONS 0x0214

/// Token types
#define TOKEN_COMMENT           1
#define TOKEN_BOOLEAN           2
#define TOKEN_NUMBER            3
#define TOKEN_STRING            4
#define TOKEN_NAME              5
#define TOKEN_START_ARRAY       6
#define TOKEN_END_ARRAY         7
#define TOKEN_START_DICTIONARY  8
#define TOKEN_END_DICTIONARY    9
#define TOKEN_REFERENCE        10
#define TOKEN_NULL             12
#define TOKEN_OTHER            13

/// Class representing a tokenizer for parsing PDF documenst.
class WXDLLIMPEXP_PDFDOC wxPdfTokenizer
{
public:
  /// Constructor
  wxPdfTokenizer(wxInputStream* inputStream);

  /// Destructor
  virtual ~wxPdfTokenizer();

  /// Set current offset position in stream
  off_t Seek(off_t pos);

  /// Get current offset position in stream
  off_t Tell();

  /// Go back one position in the stream
  void BackOnePosition(int ch);

  /// Get length of stream
  off_t GetLength();

  /// Read one byte from stream
  int ReadChar();

  /// Read size bytes from stream
  wxMemoryOutputStream* ReadBuffer(size_t size);

  /// Find the offset of the startxref tag
  off_t GetStartXRef();

  /// Read a string
  wxString ReadString(int size);

  /// Check the header of the document stream
  wxString CheckPdfHeader();

  /// Get the next token
  bool NextToken();

  /// Get the next valid token
  void NextValidToken();

  /// Get the type of the last token
  int GetTokenType();
    
  /// Get the token value as a string
  wxString GetStringValue();

  /// Get the token value as an integer
  int GetIntValue();

  /// Check whether the token is a hexadecimal string
  bool IsHexString() { return m_hexString; }
    
  /// Get object reference
  int GetReference();

  /// Get object generation
  int GetGeneration();
    
  /// Check byte whether it represents a white space character
  static bool IsWhitespace(int ch);

  /// Check byte whether it is a delimiter
  static bool IsDelimiter(int ch);

  /// Check byte whether it is a delimiter or a whitespace character
  static bool IsDelimiterOrWhitespace(int ch);

  /// Get hexadecimal character
  static int GetHex(int v);

private:
  wxInputStream* m_inputStream; ///< Stream of document data
  int            m_type;        ///< Type of last token
  wxString       m_stringValue; ///< Value of last token
  int            m_reference;   ///< Reference number of object
  int            m_generation;  ///< Generation number of object
  bool           m_hexString;   ///< Flag for hexadeciaml strings

};

/// Class representing an XRef entry (for internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfXRefEntry
{
public:
  /// Constructor
  wxPdfXRefEntry();

  /// Destructor
  virtual ~wxPdfXRefEntry();

  int m_type;    ///< Type of XRef entry
  int m_ofs_idx; ///< Offset or index of object
  int m_gen_ref; ///< Generation of object or reference of object stream containing the object
};

WX_DECLARE_USER_EXPORTED_OBJARRAY(wxPdfXRefEntry, wxPdfXRef, WXDLLIMPEXP_PDFDOC);

/// Class representing a PDF parser. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfParser
{
public:
  /// Constructor
  wxPdfParser(const wxString& filename,
              const wxString& password = wxEmptyString);

  /// Destructor
  virtual ~wxPdfParser();

  /// Check whether the PDF document to be parsed is valid
  bool IsOk();

  /// Get PDF version of parsed document
  wxString GetPdfVersion() { return m_pdfVersion; }

  /// Get number of pages in the parsed document
  unsigned int GetPageCount();

  /// Get the document information dictionary
  bool GetSourceInfo(wxPdfInfo& info);

  /// Get the queue of referenced objects
  wxPdfObjectQueue* GetObjectQueue() { return m_objectQueue; }

  /// Get the map of referenced objects
  wxPdfObjectMap* GetObjectMap() { return m_objectMap; }

  /// Append a referenced object to the queue
  void AppendObject(int originalObjectId, int actualObjectId, wxPdfObject* obj);

  /// Get the resources of a specific page
  wxPdfObject* GetPageResources(unsigned int pageno);

  /// Get the content stream collection of a specific page
  void GetContent(unsigned int pageno, wxArrayPtrVoid& contents);

  /// Get the media box of a specific page
  wxPdfArrayDouble* GetPageMediaBox(unsigned int pageno);

  /// Get the crop box of a specific page
  wxPdfArrayDouble* GetPageCropBox(unsigned int pageno);

  /// Get the bleed box of a specific page
  wxPdfArrayDouble* GetPageBleedBox(unsigned int pageno);

  /// Get the trim box of a specific page
  wxPdfArrayDouble* GetPageTrimBox(unsigned int pageno);

  /// Get the art box of a specific page
  wxPdfArrayDouble* GetPageArtBox(unsigned int pageno);

  /// Get the rotation of a specific page
  int GetPageRotation (unsigned int pageno);

  /// Resolve an object
  wxPdfObject* ResolveObject(wxPdfObject* obj);

  /// Set flag whether a stream should be decoded or not
  void SetUseRawStream(bool useRawStream) { m_useRawStream = useRawStream; }

  /// Get flag whether a stream should be decoded or not
  bool GetUseRawStream() { return m_useRawStream; }

protected:
  /// Get the resources of a specific page identified by a page object
  wxPdfObject* GetPageResources(wxPdfObject* page);

  /// Get the content stream collection of a specific page
  void GetPageContent(wxPdfObject* contentRef, wxArrayPtrVoid& contents);

  /// Get a page box
  wxPdfArrayDouble* GetPageBox(wxPdfDictionary* page, const wxString& boxIndex);

  /// Get a page rotation
  int GetPageRotation (wxPdfDictionary* page);

  /// Parse PDF document
  bool ParseDocument();

  /// Setup a decryptor
  bool SetupDecryptor();

  /// Parse the cross reference
  bool ParseXRef();

  /// Parse the page tree of the PDF document
  bool ParsePageTree(wxPdfDictionary* pages);

  /// Parse a cross reference section
  wxPdfDictionary* ParseXRefSection();

  /// Parse a cross reference stream
  bool ParseXRefStream(int ptr, bool setTrailer);

  /// Parse an object
  wxPdfObject* ParseObject();

  /// Parse a dictionary
  wxPdfDictionary* ParseDictionary();

  /// Parse an array
  wxPdfArray* ParseArray();

  /// Parse a specific object
  wxPdfObject* ParseSpecificObject(int idx);

  /// Parse a direct object
  wxPdfObject* ParseDirectObject(int k);

  /// Parse an object from an object stream
  wxPdfObject* ParseObjectStream(wxPdfStream* stream, int idx);

  /// Parse the content of a stream object
  void GetStreamBytes(wxPdfStream* stream);

  /// Parse the raw content of a stream object
  void GetStreamBytesRaw(wxPdfStream* stream);

  /// Decode a stream predictor
  wxMemoryOutputStream* DecodePredictor(wxMemoryOutputStream* in, wxPdfObject* dicPar);

  /// Decode a stream that has the FlateDecode filter.
  /**
   * \param osIn the input data
   * \return the decoded data
   */
  wxMemoryOutputStream* FlateDecode(wxMemoryOutputStream* osIn);

  /// Decode a stream that has the ASCIIHexDecode filter.
  /**
   * \param osIn the input data
   * \return the decoded data
   */
  wxMemoryOutputStream* ASCIIHexDecode(wxMemoryOutputStream* osIn);

  /// Decode a stream that has the ASCII85Decode filter.
  /**
   * \param osIn the input data
   * \return the decoded data
   */
  wxMemoryOutputStream* ASCII85Decode(wxMemoryOutputStream* osIn);

  /// Decode a stream that has the ASCII85Decode filter.
  /**
   * \param osIn the input data
   * \return the decoded data
   */
  wxMemoryOutputStream* LZWDecode(wxMemoryOutputStream* osIn);

  /// Get wxWidgets file system
  static wxFileSystem* GetFileSystem();

private:
  /// Reserve at least count cross reference entries
  void ReserveXRef(size_t count);

  bool              m_initialized;     ///< Flag whether parser is properly initialized
  int               m_fileSize;        ///< File size
  wxString          m_filename;        ///< File name of PDF document
  wxString          m_password;        ///<
  wxString          m_pdfVersion;      ///< Version of PDF document
  wxFSFile*         m_pdfFile;         ///< File system file object of PDF document
  wxPdfTokenizer*   m_tokens;          ///< Tokenizer
  wxPdfDictionary*  m_trailer;         ///< Trailer dictionary
  wxPdfDictionary*  m_root;            ///< Root object
  wxArrayPtrVoid    m_pages;           ///< Array of page objects
  unsigned int      m_currentPage;     ///< Number of current page
  bool              m_useRawStream;    ///< Flag whether to use raw stream data (without decoding)

  bool              m_encrypted;       ///< Flag whether the document is encrypted
  wxPdfEncrypt*     m_decryptor;       ///< decryptor instance
  wxPdfDictionary*  m_encryption;      ///< Encryption dictionary

  wxPdfObjectQueue* m_objectQueue;     ///< Queue of referenced objects
  wxPdfObjectQueue* m_objectQueueLast; ///< Pointer to last queue element
  wxPdfObjectMap*   m_objectMap;       ///< Map for object queue elements
  wxPdfObjStmMap*   m_objStmCache;     ///< Cache for object streams
  bool              m_cacheObjects;    ///< Flag whether object streams should be cached

  int               m_objNum;          ///< Number of current object
  int               m_objGen;          ///< Generation of current object

  wxPdfXRef         m_xref;            ///< Cross reference

  static wxFileSystem* ms_fileSystem; ///< wxWidgets file system
};

#define WXPDF_LZW_STRINGTABLE_SIZE 8192

/// Class representing an LZW decoder. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfLzwDecoder
{
public:
  /// Constructor
  wxPdfLzwDecoder();

  /// Destructor
  virtual ~wxPdfLzwDecoder();

  /// Get next code
  int GetNextCode();

  /// Decode a byte stream
  bool Decode(wxMemoryInputStream* dataIn, wxMemoryOutputStream* dataOut);

  /// Initialize the string table
  void InitializeStringTable();

  /// Write decoded string into output buffer
  void WriteString(int code);

  /// Add string to string table
  void AddStringToTable(int oldCode, char newString);

private:
  wxMemoryInputStream*  m_dataIn;       ///< Encoded data stream
  wxMemoryOutputStream* m_dataOut;      ///< Decoded data stream
  size_t                m_dataSize;     ///< Length of encoded data stream
  wxArrayInt            m_stringTable[WXPDF_LZW_STRINGTABLE_SIZE]; ///< Table of decoded strings

  int                   m_tableIndex;   ///< Current string table index
  int                   m_bitsToGet;    ///< Number of bits to get from stream
  int                   m_bytePointer;  ///< Offset in encoded data stream
  int                   m_bitPointer;   ///< Bit offset
  int                   m_nextData;     ///< Next data item
  int                   m_nextBits;     ///< Next bits

  static int            ms_andTable[4]; ///< Static array of string table offsets
};

#endif

///////////////////////////////////////////////////////////////////////////////
// Name:        pdfcffdecoder.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-01
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfcffdecoder.h Interface of the Type1 and Type2 CFF decoder class

#ifndef _PDF_CFF_DECODER_H_
#define _PDF_CFF_DECODER_H_

// wxWidgets headers
#include <wx/dynarray.h>
#include <wx/string.h>
#include <wx/mstream.h>
#include <wx/wfstream.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdfarraytypes.h"
#include "wex/pdf/pdfcffindex.h"

// Forward declaration of internal classes
class wxPdfCffFontObject;

/// Class representing CFF decoder for charstrings of type 1 and 2. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfCffDecoder
{
public:
  /// Default constructor
  wxPdfCffDecoder();

  /// Constructor
  /**
  * \param globalSubrIndex the global subroutine index
  * \param hGlobalSubrsUsed sorted list of used global subroutines
  * \param lGlobalSubrsUsed unordered list of used global subroutines
  */
  wxPdfCffDecoder(wxPdfCffIndexArray* globalSubrIndex,
                  wxPdfSortedArrayInt* hGlobalSubrsUsed,
                  wxArrayInt* lGlobalSubrsUsed);

  /// Default destructor
  virtual ~wxPdfCffDecoder();

  /// Get character width and composites
  /**
  * From a charstring entry the character width and - if appropriate - the composites are extracted.
  * \param[in] charstring the charstring entry
  * \param[out] width the character width
  * \param[out] isComposite flag whether the charstring describes a composite
  * \param[out] bchar the base character of the composite
  * \param[out] achar the accent character of the composite
  */
  bool GetCharWidthAndComposite(wxPdfCffIndexElement& charstring, int& width, bool& isComposite, int& bchar, int& achar);

  /// Calculate the bias
  /**
  * \param nSubrs the number of subroutines
  * \return the bias
  */
  int CalcBias(int nSubrs);

  /// Read a subroutine from an input stream
  /**
  * \param stream the input stream
  * \param begin start position of the subroutine in the input stream
  * \param end end position of the subroutine in the input stream
  * \param globalBias value of the global bias
  * \param localBias value of the local bias
  * \param hSubrsUsed sorted list of used subroutines
  * \param lSubrsUsed unordered list of used subroutines
  * \param localSubIndex the local subroutine index
  */
  void ReadASubr(wxInputStream* stream, int begin, int end,
                 int globalBias, int localBias, 
                 wxPdfSortedArrayInt& hSubrsUsed, wxArrayInt& lSubrsUsed,
                 wxPdfCffIndexArray& localSubIndex);

protected:
  /// Read a byte from an input stream
  unsigned char ReadByte(wxInputStream* stream);

  /// Read a short integer from an input stream
  short ReadShort(wxInputStream* stream);

  /// Read an integer from an input stream
  int ReadInt(wxInputStream* stream);

  /// Handle the stack based on the last operator on stack
  void HandleStack();

  /// Get the type of the last operator on stack
  int StackOpp();

  /// Empty the stack
  void EmptyStack();

  /// Pop an entry from the stack
  void PopStack();

  /// Push an entry on the stack
  void PushStack();

  /// Read a command from the input stream
  void ReadCommand(wxInputStream* stream);

  /// Calculate the hints
  int CalcHints(wxInputStream* stream, int begin, int end, int globalBias, int localBias, wxPdfCffIndexArray& localSubIndex);

private:
  wxInputStream*        m_stream;              ///< the input stream

  wxPdfCffIndexArray*   m_globalSubrIndex;     ///< index of the global subroutines

  int                   m_charstringType;      ///< charstring type (distinguishes type 1 and type 2 font formats)

  wxString              m_key;                 ///< command key word
  wxPdfCffFontObject*   m_args;                ///< argument stack
  int                   m_argCount;            ///< argument count

  int                   m_globalBias;          ///< The bias for the global subroutines
  int                   m_numHints;            ///< Number of arguments to the stem operators in a subroutine calculated recursively

  wxPdfSortedArrayInt*  m_hGlobalSubrsUsed;    ///< A HashMap for keeping the Global subroutines used in the font
  wxArrayInt*           m_lGlobalSubrsUsed;    ///< The Global SubroutinesUsed HashMaps as ArrayLists
};

#endif

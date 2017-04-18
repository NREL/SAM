///////////////////////////////////////////////////////////////////////////////
// Name:        pdffontsubsetcff.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-06-24
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdffontsubsetcff.cpp Implementation of subset support for CFF fonts

/*
 * This Class subsets a CFF Type Font. The code is based on code and ideas from the iText project.
 */

// For compilers that support precompilation, includes <wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

// includes

#include "wex/pdf/pdfarraytypes.h"
#include "wex/pdf/pdfcffdecoder.h"
#include "wex/pdf/pdfcffindex.h"
#include "wex/pdf/pdffontdata.h"
#include "wex/pdf/pdffontsubsetcff.h"

#include "wxmemdbg.h"

static int
CompareInts(int n1, int n2)
{
  return n1 - n2;
}

// --- Implementation of OpenType Subset

// CFF Dict Operators
// If the high byte is 0 the command is encoded with a single byte.

const int BASEFONTNAME_OP = 0x0c16;
const int CHARSET_OP      = 0x000f;
const int CHARSTRINGS_OP  = 0x0011;
const int CIDCOUNT_OP     = 0x0c22;
const int COPYRIGHT_OP    = 0x0c00;
const int ENCODING_OP     = 0x0010;
const int FAMILYNAME_OP   = 0x0003;
const int FDARRAY_OP      = 0x0c24;
const int FDSELECT_OP     = 0x0c25;
const int FONTBBOX_OP     = 0x0005;
const int FONTNAME_OP     = 0x0c26;
const int FULLNAME_OP     = 0x0002;
const int LOCAL_SUB_OP    = 0x0013;
const int NOTICE_OP       = 0x0001;
const int POSTSCRIPT_OP   = 0x0c15;
const int PRIVATE_OP      = 0x0012;
const int ROS_OP          = 0x0c1e;
const int UNIQUEID_OP     = 0x000d;
const int VERSION_OP      = 0x0000;
const int WEIGHT_OP       = 0x0004;
const int XUID_OP         = 0x000e;

const int NUM_STD_STRINGS = 391;

const char SUBR_RETURN_OP = 11;

class wxPdfCffFontObject
{
public:
  wxPdfCffFontObject() {}
  int      m_type;
  int      m_intValue;
  wxString m_strValue;
};

class wxPdfCffDictElement
{
public:
  wxPdfCffDictElement(int op, wxInputStream* buf, int argStart, int argTotal)
  {
    m_operator = op;
    m_argument = new wxPdfCffIndexElement(buf, argStart, argTotal);
    m_argOffset = -1;
  }

  wxPdfCffDictElement(int op, wxPdfCffIndexElement* argument)
  {
    m_operator = op;
    m_argument = argument;
    m_argOffset = -1;
  }

  virtual ~wxPdfCffDictElement()
  {
    delete m_argument;
  }

  void SetOperator(int op) { m_operator = op; }
  int GetOperator() const { return m_operator; }
  wxPdfCffIndexElement* GetArgument() const { return m_argument; }
  int GetArgumentLength() const { return m_argument->GetLength(); }
  void SetArgumentOffset(int argOffset) { m_argOffset = argOffset; }
  int GetArgumentOffset() const { return m_argOffset; }

private:
  int                   m_operator;
  wxPdfCffIndexElement* m_argument;
  int                   m_argOffset;
};

wxPdfFontSubsetCff::wxPdfFontSubsetCff(const wxString& fileName)
{
  m_fileName = fileName;
  
  m_topDict = new wxPdfCffDictionary();
  m_privateDict = new wxPdfCffDictionary();

  m_stringsIndex     = new wxPdfCffIndexArray();
  m_charstringsIndex = new wxPdfCffIndexArray();
  m_globalSubrIndex  = new wxPdfCffIndexArray();
  m_localSubrIndex   = new wxPdfCffIndexArray();

  m_stringsSubsetIndex     = new wxPdfCffIndexArray();
  m_charstringsSubsetIndex = new wxPdfCffIndexArray();

  m_hGlobalSubrsUsed = new wxPdfSortedArrayInt(CompareInts);
  m_hLocalSubrsUsed = new wxPdfSortedArrayInt(CompareInts);
  m_decoder = new wxPdfCffDecoder(m_globalSubrIndex, m_hGlobalSubrsUsed, &m_lGlobalSubrsUsed);
}

void
wxPdfFontSubsetCff::DestructDictionary(wxPdfCffDictionary* dict)
{
  wxPdfCffDictionary::iterator dictEntry = dict->begin();
  for (dictEntry = dict->begin(); dictEntry != dict->end(); dictEntry++)
  {
    if (dictEntry->second != NULL)
    {
      delete dictEntry->second;
    }
  }
  delete dict;
}

wxPdfFontSubsetCff::~wxPdfFontSubsetCff()
{
  if (m_fdDict.GetCount() > 0)
  {
    size_t j;
    for (j = 0; j < m_fdDict.GetCount(); j++)
    {
      if (m_fdDict[j] != NULL)
      {
        DestructDictionary((wxPdfCffDictionary*) m_fdDict[j]);
      }
    }
    m_fdDict.Clear();
  }
  if (m_fdPrivateDict.GetCount() > 0)
  {
    size_t j;
    for (j = 0; j < m_fdPrivateDict.GetCount(); j++)
    {
      if (m_fdPrivateDict[j] != NULL)
      {
        DestructDictionary((wxPdfCffDictionary*) m_fdPrivateDict[j]);
      }
    }
    m_fdPrivateDict.Clear();
  }
  if (m_fdLocalSubrIndex.GetCount() > 0)
  {
    size_t j;
    for (j = 0; j < m_fdLocalSubrIndex.GetCount(); j++)
    {
      if (m_fdLocalSubrIndex[j] != NULL)
      {
        delete ((wxPdfCffIndexArray*) m_fdLocalSubrIndex[j]);
      }
    }
    m_fdLocalSubrIndex.Clear();
  }

  DestructDictionary(m_topDict);
  DestructDictionary(m_privateDict);

  delete m_stringsIndex;
  delete m_charstringsIndex;
  delete m_globalSubrIndex;
  delete m_localSubrIndex;
  delete m_stringsSubsetIndex;
  delete m_charstringsSubsetIndex;

  delete m_hGlobalSubrsUsed;
  delete m_hLocalSubrsUsed;

  delete m_decoder;
}

wxMemoryOutputStream*
wxPdfFontSubsetCff::CreateSubset(wxInputStream* inFont, wxPdfChar2GlyphMap* usedGlyphs, bool includeCmap)
{
  m_inFont = inFont;
  m_numGlyphsUsed = (int) usedGlyphs->size();
  m_usedGlyphs.SetCount(m_numGlyphsUsed);
  wxPdfChar2GlyphMap::const_iterator glyphIter;
  for (glyphIter = usedGlyphs->begin(); glyphIter != usedGlyphs->end(); ++glyphIter)
  {
    m_usedGlyphs[glyphIter->second] = glyphIter->first;
  }
  m_includeCmap = includeCmap;
  m_outFont = NULL;

  bool ok = ReadCffFont();
  if (ok)
  {
    GenerateFontSubset();
    WriteFontSubset();
  }

  return m_outFont;
}

// --- Read original CFF stream

bool
wxPdfFontSubsetCff::ReadCffFont()
{
  bool    ok = ReadHeader(); // cairo_cff_font_read_header,
  if (ok) ok = ReadFontName(); //  cairo_cff_font_read_name,
  if (ok) ok = ReadTopDict(); //  cairo_cff_font_read_top_dict,
  if (ok) ok = ReadStrings(); //  cairo_cff_font_read_strings,
  if (ok) ok = ReadGlobalSubroutines(); //  cairo_cff_font_read_global_subroutines,
  return ok;
}

bool
wxPdfFontSubsetCff::ReadFontIndex(wxPdfCffIndexArray* index)
{
  int maxLength = GetSizeI();
  if (TellI() + 2 > maxLength)
  {
    wxLogError(wxString(wxT("wxPdfCffSubset::ReadFontIndex: ")) +
               wxString(_("Premature end of CFF stream reached while reading index count.")));
    return false;
  }
  int count = ReadShort();
  if (count > 0)
  {
    int offsetSize = ReadByte();
    if (TellI() + (count+1)*offsetSize > maxLength)
    {
      wxLogError(wxString(wxT("wxPdfCffSubset::ReadFontIndex: ")) +
                 wxString(_("Premature end of CFF stream reached while reading index data.")));
      return false;
    }
    int data = TellI() + offsetSize*(count+1) - 1;
    int start = ReadOffset(offsetSize);
    int end = 0;
    int j;
    for (j = 0; j < count; j++)
    {
      end = ReadOffset(offsetSize);
      index->Add(wxPdfCffIndexElement(m_inFont, data+start, end-start));
      start = end;
    }
    SeekI(data + end);
  }
  return true;
}

bool
wxPdfFontSubsetCff::ReadHeader()
{
  bool ok = GetSizeI() > 4;
  if (ok)
  {
    SeekI(0);
    ReadByte(); // Major
    ReadByte(); // Minor
    m_hdrSize = ReadByte();
    ReadByte(); // OffSize
    SeekI(m_hdrSize);
  }
  return ok;
}

bool
wxPdfFontSubsetCff::ReadFontName()
{
  // Original font name not used in subset. Skip name index.
  wxPdfCffIndexArray index;
  bool ok = ReadFontIndex(&index);
  if (ok)
  {
    int currentPosition = TellI();
    wxPdfCffIndexElement& element = index[0];
    SeekI(element.GetOffset());
    m_fontName = ReadString(element.GetLength());
    m_fontName += wxT("-Subset");
    SeekI(currentPosition);
  }
  return ok;
}

bool
wxPdfFontSubsetCff::ReadFontDict(wxPdfCffDictionary* dict, int dictOffset, int dictSize)
{
  bool ok = true;
  SeekI(dictOffset);
  int end = dictOffset + dictSize;
  int argStart, argSize, argTotal, op;
  while (TellI() < end)
  {
    argStart = TellI();
    argTotal = 0;
    do
    {
      argSize = ReadOperandLength();
      argTotal += argSize;
      SeekI(argStart + argTotal);
    }
    while (argSize > 0);
    op = ReadOperator();
    wxPdfCffDictElement* dictElement = new wxPdfCffDictElement(op, m_inFont, argStart, argTotal);
    (*dict)[op] = dictElement;
  }
  return ok;
}

wxPdfCffDictElement*
wxPdfFontSubsetCff::FindDictElement(wxPdfCffDictionary* dict, int key)
{
  wxPdfCffDictElement* dictElement = NULL;
  wxPdfCffDictionary::iterator dictIter = dict->find(key);
  if (dictIter != dict->end())
  {
    dictElement = dictIter->second;
  }
  return dictElement;
}

void
wxPdfFontSubsetCff::SetDictElementArgument(wxPdfCffDictionary* dict, int key, wxMemoryOutputStream& buffer)
{
  wxPdfCffDictElement* dictElement = FindDictElement(dict, key);
  if (dictElement != NULL)
  {
    dictElement->GetArgument()->SetBuffer(buffer);
  }
  else
  {
    wxPdfCffIndexElement* argument = new wxPdfCffIndexElement(buffer);
    dictElement = new wxPdfCffDictElement(key, argument);
    (*dict)[key] = dictElement;
  }
}

void
wxPdfFontSubsetCff::RemoveDictElement(wxPdfCffDictionary* dict, int key)
{
  wxPdfCffDictionary::iterator dictIter = dict->find(key);
  if (dictIter != dict->end())
  {
    delete dictIter->second;
    dict->erase(dictIter);
  }
}

bool
wxPdfFontSubsetCff::ReadFdSelect()
{
  bool ok = true;
  int type, numRanges, first, last, fd, j, k;

  m_fdSelect.SetCount(m_numGlyphs);

  type = ReadByte();
  if (type == 0)
  {
    for (j = 0; j < m_numGlyphs; j++)
    {
      m_fdSelect[j] = ReadByte();
    }
  }
  else if (type == 3)
  {
    numRanges = ReadShort();
    last = ReadShort();
    for  (k = 0; k < numRanges; k++)
    {
      first = last;
      fd = ReadByte();
      last = ReadShort();
      for (j = first; j < last; j++)
      {
        m_fdSelect[j] = fd;
      }
    }
  }
  else
  {
    ok = false;
  }
  return ok;
}

bool
wxPdfFontSubsetCff::ReadPrivateDict(wxPdfCffDictionary* privateDict, wxPdfCffIndexArray* localSubrIndex, int offset, int size)
{
  bool ok = ReadFontDict(privateDict, offset, size);
  if (ok)
  {
    wxPdfCffDictElement* localSubOp = FindDictElement(privateDict, LOCAL_SUB_OP);
    if (localSubOp != NULL)
    {
      SeekI((localSubOp->GetArgument())->GetOffset());
      int localSubOffset = DecodeInteger();
      SeekI(offset+localSubOffset);
      ok = ReadFontIndex(localSubrIndex);
      if (ok)
      {
        wxMemoryOutputStream buffer;
        EncodeIntegerMax(0, buffer);
        SetDictElementArgument(privateDict, LOCAL_SUB_OP, buffer);
      }
    }
  }
  return ok;
}

bool
wxPdfFontSubsetCff::ReadCidFontDict()
{
  wxPdfCffIndexArray index;
  bool ok = ReadFontIndex(&index);

  m_numFontDicts = (int) index.GetCount();
  m_fdDict.SetCount(m_numFontDicts);
  m_fdPrivateDict.SetCount(m_numFontDicts);
  m_fdLocalSubrIndex.SetCount(m_numFontDicts);

  int j, size, offset;
  for (j = 0; ok && (j < m_numFontDicts); j++)
  {
    m_fdDict[j] = new wxPdfCffDictionary();
    wxPdfCffIndexElement& element = index[j];
    ok = ReadFontDict((wxPdfCffDictionary*) m_fdDict[j], element.GetOffset(), element.GetLength());
    if (ok)
    {
      wxPdfCffDictElement* privateOp = FindDictElement((wxPdfCffDictionary*) m_fdDict[j], PRIVATE_OP);
      ok = (privateOp == NULL);
      if (ok)
      {
        SeekI((privateOp->GetArgument())->GetOffset());
        size = DecodeInteger();
        offset = DecodeInteger();
        SeekI(offset);
        m_fdPrivateDict[j] = new wxPdfCffDictionary();
        m_fdLocalSubrIndex[j] = new wxPdfCffIndexArray();
        ok = ReadPrivateDict((wxPdfCffDictionary*) m_fdPrivateDict[j], (wxPdfCffIndexArray*) m_fdLocalSubrIndex[j], offset, size);
        if (ok)
        {
          wxMemoryOutputStream buffer;
          EncodeIntegerMax(0, buffer);
          EncodeIntegerMax(0, buffer);
          SetDictElementArgument((wxPdfCffDictionary*) m_fdDict[j], PRIVATE_OP, buffer);
        }
      }
    }
  }
  return ok;
}

bool
wxPdfFontSubsetCff::ReadTopDict()
{
  wxPdfCffIndexArray index;
  bool ok = ReadFontIndex(&index);
  int savePosition = TellI();
  // index now contains the top dictionary index

  // Now search the first entry (of a multi font index)
  if (!ok || index.IsEmpty())
  {
    return false;
  }
  wxPdfCffIndexElement& element = index[0];

  ReadFontDict(m_topDict, element.GetOffset(), element.GetLength());

  wxPdfCffDictElement* rosOp = FindDictElement(m_topDict, ROS_OP);
  m_isCid = rosOp != NULL;

  int offset, size;
  wxPdfCffDictElement* charstringsOp = FindDictElement(m_topDict, CHARSTRINGS_OP);
  ok = (charstringsOp != NULL);
  if (ok)
  {
    SeekI((charstringsOp->GetArgument())->GetOffset());
    offset = DecodeInteger();
    SeekI(offset);
    ok = ReadFontIndex(m_charstringsIndex);
  }
  if (!ok)
  {
    return false;
  }
  m_numGlyphs = (int) m_charstringsIndex->GetCount();

  if (m_isCid)
  {
    wxPdfCffDictElement* fdselectOp = FindDictElement(m_topDict, FDSELECT_OP);
    ok = (fdselectOp != NULL);
    if (ok)
    {
      SeekI((fdselectOp->GetArgument())->GetOffset());
      offset = DecodeInteger();
      SeekI(offset);
      ok = ReadFdSelect();
    }
    if (ok)
    {
      wxPdfCffDictElement* fdarrayOp = FindDictElement(m_topDict, FDARRAY_OP);
      ok = (fdarrayOp != NULL);
      if (ok)
      {
        SeekI((fdarrayOp->GetArgument())->GetOffset());
        offset = DecodeInteger();
        SeekI(offset);
        ok = ReadCidFontDict();
      }
    }
  }
  else
  {
    wxPdfCffDictElement* privateOp = FindDictElement(m_topDict, PRIVATE_OP);
    SeekI((privateOp->GetArgument())->GetOffset());
    size = DecodeInteger();
    offset = DecodeInteger();
    SeekI(offset);
    ok = ReadPrivateDict(m_privateDict, m_localSubrIndex, offset, size);
  }

  if (ok)
  {
    // Use maximum sized encoding to reserve space for later modification
    wxMemoryOutputStream buffer;
    EncodeIntegerMax(0, buffer);
    SetDictElementArgument(m_topDict, CHARSTRINGS_OP, buffer);
    SetDictElementArgument(m_topDict, FDSELECT_OP, buffer);
    SetDictElementArgument(m_topDict, FDARRAY_OP, buffer);
    SetDictElementArgument(m_topDict, CHARSET_OP, buffer);
    RemoveDictElement(m_topDict, ENCODING_OP);
    RemoveDictElement(m_topDict, PRIVATE_OP);
    // Remove unique identifier operators as the subsetted font is not the same as the original font
    RemoveDictElement(m_topDict, UNIQUEID_OP);
    RemoveDictElement(m_topDict, XUID_OP);
  }
  SeekI(savePosition);
  return ok;
}

bool
wxPdfFontSubsetCff::ReadStrings()
{
  return ReadFontIndex(m_stringsIndex);
}

bool
wxPdfFontSubsetCff::ReadGlobalSubroutines()
{
  return ReadFontIndex(m_globalSubrIndex);
}

void
wxPdfFontSubsetCff::SeekI(int offset)
{
  m_inFont->SeekI(offset);
}

int
wxPdfFontSubsetCff::TellI()
{
  return (int) m_inFont->TellI();
}

void
wxPdfFontSubsetCff::SeekO(int offset)
{
  m_outFont->SeekO(offset);
}

int
wxPdfFontSubsetCff::TellO()
{
  return (int) m_outFont->TellO();
}

int
wxPdfFontSubsetCff::GetSizeI()
{
  return (int) m_inFont->GetSize();
}

unsigned char
wxPdfFontSubsetCff::ReadByte()
{
  unsigned char card8;
  m_inFont->Read(&card8, 1);
  return card8;
}

short
wxPdfFontSubsetCff::ReadShort()
{
  // Read a 2-byte integer from file (big endian)
  short i16;
  m_inFont->Read(&i16, 2);
  return wxINT16_SWAP_ON_LE(i16);
}

int
wxPdfFontSubsetCff::ReadInt()
{
  // Read a 4-byte integer from file (big endian)
  int i32;
  m_inFont->Read(&i32, 4);
  return wxINT32_SWAP_ON_LE(i32);
}

int
wxPdfFontSubsetCff::ReadOffset(int offSize)
{
  int offset = 0;
  int j;
  for (j = 0; j < offSize; j++)
  {
    offset *= 256;
    offset += (int) ReadByte();
  }
  return offset;
}

/* return 0 if not an operand */
int
wxPdfFontSubsetCff::ReadOperandLength()
{
  int operandLength = 0;
  int begin = TellI();
  unsigned char b0 = ReadByte();

  if (b0 == 28)
  {
    operandLength = 3;
  }
  else if (b0 == 29)
  {
    operandLength = 5;
  }
  else if (b0 >= 32 && b0 <= 246)
  {
    operandLength = 1;
  }
  else if (b0 >= 247 && b0 <= 254)
  {
    operandLength = 2;
  }
  else if (b0 == 30)
  {

    while ((b0 & 0x0f) != 0x0f)
    {
      b0 = ReadByte();
    }
    operandLength = TellI() - begin;
  }
  return operandLength;
}

int
wxPdfFontSubsetCff::ReadOperator()
{
  int op = ReadByte();
  if (op == 12)
  {
    op <<= 8;
    op |= ReadByte();;
  }
  return op;
}

wxString
wxPdfFontSubsetCff::ReadString(int length)
{
  wxString str = wxEmptyString;
  if (length > 0)
  {
    char* buffer = new char[length];
    m_inFont->Read(buffer, length);
    int j;
    for (j = 0; j < length; j++)
    {
      str.Append(buffer[j]);
    }
    delete [] buffer;
  }
  return str;
}

int 
wxPdfFontSubsetCff::DecodeInteger()
{
  int result = 0;
  unsigned char b0, b1;

  b0 = ReadByte();
  if (b0 == 28)
  {
    result = ReadShort();
  }
  else if (b0 == 29)
  {
    result = ReadInt();
  }
  else if (b0 >= 32 && b0 <= 246)
  {
    result = b0 - 139;
  }
  else if (b0 >= 247 && b0 <= 250)
  {
    b1 = ReadByte();
    result = (b0-247)*256 + b1 + 108;
  }
  else if (b0 >= 251 && b0 <= 254)
  {
    b1 = ReadByte();
    result = -(b0-251)*256 - b1 - 108;
  }
  else
  {
    result = 0;
  }
  return result;
}

void
wxPdfFontSubsetCff::EncodeIntegerMax(int value, wxMemoryOutputStream& buffer)
{
  char locBuffer[5];
  locBuffer[0] = 29;
  locBuffer[1] = (char)((value >> 24) & 0xff);
  locBuffer[2] = (char)((value >> 16) & 0xff);
  locBuffer[3] = (char)((value >>  8) & 0xff);
  locBuffer[4] = (char)((value      ) & 0xff);
  buffer.Write(locBuffer, 5);
}

void
wxPdfFontSubsetCff::EncodeInteger(int value, wxMemoryOutputStream& buffer)
{
  char locBuffer[5];
  size_t count;
  if (value >= -107 && value <= 107)
  {
    locBuffer[0] = value + 139;
    count = 1;
  }
  else if (value >= 108 && value <= 1131)
  {
    value -= 108;
    locBuffer[0] = (char)(((value >> 8) + 247) & 0xff);
    locBuffer[1] = (char)(((value     )      ) & 0xff);
    count = 2;
  }
  else if (value >= -1131 && value <= -108)
  {
    value = -value - 108;
    locBuffer[0] = (char)(((value >> 8) + 251) & 0xff);
    locBuffer[1] = (char)(((value     )      ) & 0xff);
    count = 2;
  }
  else if (value >= -32768 && value <= 32767)
  {
    locBuffer[0] = 28;
    locBuffer[1] = (char)((value >> 8) & 0xff);
    locBuffer[2] = (char)((value     ) & 0xff);
    count = 3;
  }
  else
  {
    locBuffer[0] = 29;
    locBuffer[1] = (char)((value >> 24) & 0xff);
    locBuffer[2] = (char)((value >> 16) & 0xff);
    locBuffer[3] = (char)((value >>  8) & 0xff);
    locBuffer[4] = (char)((value      ) & 0xff);
    count = 5;
  }
  buffer.Write(locBuffer, count);
}

// --- Generate subset

void
wxPdfFontSubsetCff::SetRosStrings()
{
  int sid1, sid2;
  sid1 = NUM_STD_STRINGS + (int) m_stringsSubsetIndex->GetCount();
  wxPdfCffIndexElement* registry = new wxPdfCffIndexElement("Adobe");
  m_stringsSubsetIndex->Add(registry);

  sid2 = NUM_STD_STRINGS + (int) m_stringsSubsetIndex->GetCount();
  wxPdfCffIndexElement* ordering = new wxPdfCffIndexElement("Identity");
  m_stringsSubsetIndex->Add(ordering);

  wxMemoryOutputStream rosBuffer;
  EncodeInteger(sid1, rosBuffer);
  EncodeInteger(sid2, rosBuffer);
  EncodeInteger(0, rosBuffer);
  SetDictElementArgument(m_topDict, ROS_OP, rosBuffer);

  wxMemoryOutputStream cidBuffer;
  EncodeInteger(m_numGlyphsUsed, cidBuffer);
  SetDictElementArgument(m_topDict, CIDCOUNT_OP, cidBuffer);
}

void
wxPdfFontSubsetCff::SubsetCharstrings()
{
  int j, k;
  int numGlyphsUsed = (int) m_usedGlyphs.GetCount();
  int usedGlyph;
  k = 0;
  for (j = 0; j < numGlyphsUsed; j++)
  {
    usedGlyph = m_usedGlyphs[j];
    m_charstringsSubsetIndex->Add((*m_charstringsIndex)[usedGlyph]);
  }
}

void
wxPdfFontSubsetCff::SubsetFontDict()
{
  m_fdSelectSubset.SetCount(m_numGlyphsUsed);
  m_fdSubsetMap.SetCount(m_numFontDicts);
  m_privateDictOffset.SetCount(m_numFontDicts);
  wxArrayInt reverseMap;
  reverseMap.SetCount(m_numFontDicts);
  int j;
  for (j = 0; j < m_numFontDicts; j++)
  {
    reverseMap[j] = -1;
  }
  m_numSubsetFontDicts = 0;
  int fd;
  for (j = 0; j < m_numGlyphsUsed; j++)
  {
    fd = m_fdSelect[m_usedGlyphs[j]];
    if (reverseMap[fd] < 0)
    {
      m_fdSubsetMap[m_numSubsetFontDicts] = fd;
      reverseMap[fd] = m_numSubsetFontDicts++;
    }
    m_fdSelectSubset[j] = reverseMap[fd];
  }
}

void
wxPdfFontSubsetCff::CreateCidFontDict()
{
  m_numFontDicts = 1;
  wxPdfCffDictionary* fdDict = new wxPdfCffDictionary();
  m_fdDict.Add(fdDict); 
  m_fdSubsetMap.SetCount(1);
  m_fdSubsetMap[0] = 0;
  m_privateDictOffset.SetCount(1);
  m_numSubsetFontDicts = 1;

  wxMemoryOutputStream buffer;
  EncodeIntegerMax(0, buffer);
  EncodeIntegerMax(0, buffer);
  SetDictElementArgument(fdDict, PRIVATE_OP, buffer);
}

void
wxPdfFontSubsetCff::SubsetDictString(wxPdfCffDictionary* dict, int op)
{
  wxPdfCffDictElement* element = FindDictElement(dict, op);
  if (element != NULL)
  {
    SeekI((element->GetArgument())->GetOffset());
    int sid = DecodeInteger();
    if (sid >= NUM_STD_STRINGS)
    {
      int sidNew = NUM_STD_STRINGS + (int) m_stringsSubsetIndex->GetCount();
      m_stringsSubsetIndex->Add((*m_stringsIndex)[sid-NUM_STD_STRINGS]);
      wxMemoryOutputStream buffer;
      EncodeInteger(sidNew, buffer);
      SetDictElementArgument(dict, op, buffer);
    }
  }
}

void
wxPdfFontSubsetCff::SubsetDictStrings(wxPdfCffDictionary* dict)
{
  static const int dictStrings[] = {
    VERSION_OP, NOTICE_OP,     COPYRIGHT_OP,    FULLNAME_OP, FAMILYNAME_OP,
    WEIGHT_OP,  POSTSCRIPT_OP, BASEFONTNAME_OP, FONTNAME_OP,
    -1
  };
  int j;
  for (j = 0; dictStrings[j] >= 0; j++)
  {
    SubsetDictString(dict, dictStrings[j]);
  }
}

void
wxPdfFontSubsetCff::SubsetStrings()
{
  int j;
  SubsetDictStrings(m_topDict);
  if (m_isCid)
  {
    for (j = 0; j < m_numSubsetFontDicts; j++)
    {
      SubsetDictStrings((wxPdfCffDictionary*) m_fdDict[m_fdSubsetMap[j]]);
      SubsetDictStrings((wxPdfCffDictionary*) m_fdPrivateDict[m_fdSubsetMap[j]]);
    }
  }
  else
  {
    SubsetDictStrings(m_privateDict);
  }
}

void
wxPdfFontSubsetCff::GenerateFontSubset()
{
  FindLocalAndGlobalSubrsUsed();
  SetRosStrings();
  SubsetCharstrings();
  if (m_isCid)
  {
    SubsetFontDict();
  }
  else
  {
    CreateCidFontDict();
  }
  SubsetStrings();
}

// --- Write subset

void
wxPdfFontSubsetCff::WriteInteger(int value, int size, wxMemoryOutputStream* buffer)
{
  char locBuffer[4];
  int i = 0;
  switch (size)
  {
    case 4:
      locBuffer[i] = (char)((value >> 24) & 0xff);
      i++;
    case 3:
      locBuffer[i] = (char)((value >> 16) & 0xff);
      i++;
    case 2:
      locBuffer[i] = (char)((value >>  8) & 0xff);
      i++;
    case 1:
      locBuffer[i] = (char)((value      ) & 0xff);
      i++;
    default:
      break;
  }
  buffer->Write(locBuffer, i);
}


void
wxPdfFontSubsetCff::WriteIndex(wxPdfCffIndexArray* index)
{
  int numElements = (int) index->GetCount();
  WriteInteger(numElements, 2, m_outFont);
  if (numElements == 0)
  {
    return;
  }
  // Find maximum offset to determine offset size
  int j, offsetSize;
  int offset = 1;
  for (j = 0; j < numElements; j++)
  {
    offset += (*index)[j].GetLength();
  }
  if (offset < 0x100)
  {
    offsetSize = 1;
  }
  else if (offset < 0x10000)
  {
    offsetSize = 2;
  }
  else if (offset < 0x1000000)
  {
    offsetSize = 3;
  }
  else
  {
    offsetSize = 4;
  }
  WriteInteger(offsetSize, 1, m_outFont);
  offset = 1;
  WriteInteger(offset, offsetSize, m_outFont);
  for (j = 0; j < numElements; j++)
  {
    offset += (*index)[j].GetLength();
    WriteInteger(offset, offsetSize, m_outFont);
  }
  for (j = 0; j < numElements; j++)
  {
    (*index)[j].Emit(*m_outFont);
  }
}

void
wxPdfFontSubsetCff::WriteHeader()
{
  wxPdfCffIndexElement header(m_inFont, 0, m_hdrSize);
  header.Emit(*m_outFont);
}

void
wxPdfFontSubsetCff::WriteName()
{
  // TODO: Write subset font name
  //wxMemoryOutputStream nameBuffer;
  //nameBuffer.Write("font-name-dummy",strlen("font-name-dummy"));
  wxMemoryOutputStream buffer;
  int len = (int) m_fontName.Length();
  int j;
  for (j = 0; j < len; j++)
  {
    char ch = (char)(m_fontName[j]) & 0xff;
    buffer.Write(&ch, 1);
  }
  wxPdfCffIndexArray index;
  index.Add(wxPdfCffIndexElement(buffer));
  WriteIndex(&index);
}

void
wxPdfFontSubsetCff::WriteTopDict()
{
  int offsetSize = 4;
  int offsetIndex;
  int dictStart, dictSize;
  WriteInteger(1, 2, m_outFont);
  WriteInteger(offsetSize, 1, m_outFont);
  WriteInteger(1, offsetSize, m_outFont);
  offsetIndex = TellO();
  WriteInteger(0, offsetSize, m_outFont);
  dictStart = TellO();
  WriteDict(m_topDict);
  int lastPosition = TellO();
  dictSize = lastPosition - dictStart + 1;
  SeekO(offsetIndex);
  WriteInteger(dictSize, offsetSize, m_outFont);
  SeekO(lastPosition);
}

void
wxPdfFontSubsetCff::WriteDict(wxPdfCffDictionary* dict)
{
  // If there is a ROS operator it has to be the first according to the CFF specification
  wxPdfCffDictElement* rosOp = FindDictElement(dict, ROS_OP);
  if (rosOp != NULL)
  {
    WriteDictOperator(rosOp);
  }
  wxPdfCffDictionary::iterator dictEntry = dict->begin();
  for (dictEntry = dict->begin(); dictEntry != dict->end(); dictEntry++)
  {
    // The ROS operator is handled separately
    if ((dictEntry->second)->GetOperator() != ROS_OP)
    {
      WriteDictOperator(dictEntry->second);
    }
  }
}

void
wxPdfFontSubsetCff::WriteDictOperator(wxPdfCffDictElement* dictElement)
{
  int offset = TellO();
  dictElement->SetArgumentOffset(offset);
  dictElement->GetArgument()->Emit(*m_outFont);
  int op = dictElement->GetOperator();
  if (op & 0xff00)
  {
    WriteInteger((op >> 8) & 0xff, 1, m_outFont);
  }
  WriteInteger(op & 0xff, 1, m_outFont);
}

void
wxPdfFontSubsetCff::WriteStrings()
{
  WriteIndex(m_stringsSubsetIndex);
}

void
wxPdfFontSubsetCff::WriteGlobalSubrs()
{
  WriteIndex(m_globalSubrIndex);
}

/* Set the operand of the specified operator in the (already written)
 * top dict to point to the current position in the output
 * array. Operands updated with this function must have previously
 * been encoded with the 5-byte (max) integer encoding. */
int
wxPdfFontSubsetCff::GetLocation(wxPdfCffDictionary* dict, int op)
{
  int offset = -1;
  wxPdfCffDictElement* dictElement = FindDictElement(dict, op);
  if (dictElement != NULL)
  {
    offset = dictElement->GetArgumentOffset();
  }
  return offset;
}

void
wxPdfFontSubsetCff::SetTopDictOperatorToCurrentPosition(int op)
{
  int currentPosition = TellO();
  int offset = GetLocation(m_topDict, op);
  if (offset >= 0)
  {
    SeekO(offset);
    EncodeIntegerMax(currentPosition, *m_outFont);
    SeekO(currentPosition);
  }
}

void
wxPdfFontSubsetCff::WriteCharset()
{
  SetTopDictOperatorToCurrentPosition(CHARSET_OP);
  WriteInteger(2, 1, m_outFont);
  WriteInteger(1, 2, m_outFont);
  WriteInteger(m_numGlyphsUsed-2, 2, m_outFont);
}

void
wxPdfFontSubsetCff::WriteFdSelect()
{
  SetTopDictOperatorToCurrentPosition(FDSELECT_OP);
  if (m_isCid)
  {
    WriteInteger(0, 1, m_outFont);
    int j;
    for (j = 0; j < m_numGlyphsUsed; j++)
    {
      WriteInteger(m_fdSelectSubset[j], 1, m_outFont);
    }
  }
  else
  {
    WriteInteger(3, 1, m_outFont);
    WriteInteger(1, 2, m_outFont);
    WriteInteger(0, 2, m_outFont);
    WriteInteger(0, 1, m_outFont);
    WriteInteger(m_numGlyphsUsed, 2, m_outFont);
  }
}

void
wxPdfFontSubsetCff::WriteCharStrings()
{
  SetTopDictOperatorToCurrentPosition(CHARSTRINGS_OP);
  WriteIndex(m_charstringsSubsetIndex);
}

void
wxPdfFontSubsetCff::WriteCidFontDict()
{
  int offsetSize = 4;
  SetTopDictOperatorToCurrentPosition(FDARRAY_OP);
  WriteInteger(m_numSubsetFontDicts, 2, m_outFont);
  WriteInteger(offsetSize, 1, m_outFont);

  int offsetBase = TellO();
  WriteInteger(1, offsetSize, m_outFont);
  int j;
  for (j = 0; j < m_numSubsetFontDicts; j++)
  {
    WriteInteger(0, offsetSize, m_outFont);
  }
  int offset = 0;
  int offsetDict;
  for (j = 0; j < m_numSubsetFontDicts; j++)
  {
    WriteDict((wxPdfCffDictionary*) m_fdDict[m_fdSubsetMap[j]]);
    offsetDict = TellO();
    offset += offsetSize;
    SeekO(offsetBase+offset);
    WriteInteger(offsetDict-offsetBase+1, offsetSize, m_outFont);
    SeekO(offsetDict);
  }
}

void
wxPdfFontSubsetCff::WritePrivateDict(int dictNum, wxPdfCffDictionary* parentDict, wxPdfCffDictionary* privateDict)
{
  // Write private dict and update offset and size in top dict
  m_privateDictOffset[dictNum] = TellO();
  WriteDict(privateDict);

  // private entry has two operands - size and offset
  int lastPosition = TellO();
  int size = lastPosition - m_privateDictOffset[dictNum];
  int offset = GetLocation(parentDict, PRIVATE_OP);
  SeekO(offset);
  EncodeIntegerMax(size, *m_outFont);
  EncodeIntegerMax(m_privateDictOffset[dictNum], *m_outFont);
  SeekO(lastPosition);
}

void
wxPdfFontSubsetCff::WriteLocalSub(int dictNum, wxPdfCffDictionary* privateDict, wxPdfCffIndexArray* localSubrIndex)
{
  if (localSubrIndex->GetCount() > 0)
  {
    // Write local subroutines and update offset in private dict
    // Local subroutines offset is relative to start of private dict
    int lastPosition = TellO();
    int offset = lastPosition - m_privateDictOffset[dictNum];
    int localSubOffset = GetLocation(privateDict, LOCAL_SUB_OP);
    SeekO(localSubOffset);
    EncodeIntegerMax(offset, *m_outFont);
    SeekO(lastPosition);
    WriteIndex(localSubrIndex);
  }
}

void
wxPdfFontSubsetCff::WriteCidPrivateDictAndLocalSub()
{
  if (m_isCid)
  {
    int j;
    for (j = 0; j < m_numSubsetFontDicts; j++)
    {
      WritePrivateDict(j, (wxPdfCffDictionary*) m_fdDict[m_fdSubsetMap[j]], (wxPdfCffDictionary*) m_fdPrivateDict[m_fdSubsetMap[j]]);
    }
    for (j = 0; j < m_numSubsetFontDicts; j++)
    {
      WriteLocalSub(j, (wxPdfCffDictionary*) m_fdPrivateDict[m_fdSubsetMap[j]], (wxPdfCffIndexArray*) m_fdLocalSubrIndex[m_fdSubsetMap[j]]);
    }
  }
  else
  {
    WritePrivateDict(0, (wxPdfCffDictionary*) m_fdDict[0], m_privateDict);
    WriteLocalSub(0, m_privateDict, m_localSubrIndex);
  }
}

#include <wx/zstream.h>

void
wxPdfFontSubsetCff::WriteFontSubset()
{
  m_outFont = new wxMemoryOutputStream();
  WriteHeader();
  WriteName();
  WriteTopDict();
  WriteStrings();
  WriteGlobalSubrs();
  WriteCharset();
  WriteFdSelect();
  WriteCharStrings();
  WriteCidFontDict();
  WriteCidPrivateDictAndLocalSub();

#if 0
  wxFileOutputStream cffSubset(wxT("cffsubset.dat"));
  wxMemoryInputStream tmp(*m_outFont);
  cffSubset.Write(tmp);
  cffSubset.Close();
#endif
#if 0
  wxLogMessage(wxT("-- Dump outFont --"));
    char locBuffer[1024];
    tmp.SeekI(0);
    int copyLength = tmp.GetLength();
    int bufferLength;
    while (copyLength > 0)
    {
      bufferLength = (copyLength > 1024) ? 1024 : copyLength;
      tmp.Read(locBuffer, bufferLength);
      copyLength -= bufferLength;
      wxString str;
      int kk;
      for (kk = 0; kk < bufferLength; kk++)
      {
        str += wxString::Format(wxT(" %d"), locBuffer[kk]);
        if (kk % 10 == 9)
        {
          wxLogDebug(str);
          str = wxEmptyString;
        }
      }
      if ((bufferLength-1) % 10 != 9)
      {
        wxLogDebug(str);
      }
    }
#endif
#if 0
  wxFileInputStream cairoIn(wxT("gfsdidot-test-cairo2.pdf"));
  cairoIn.SeekI(960);
  char cairoBuffer[18710];
  cairoIn.Read(cairoBuffer,18710);
  wxMemoryOutputStream cairoOut;
  cairoOut.Write(cairoBuffer,18710);
  cairoOut.Close();
  wxMemoryInputStream inCairo(cairoOut);
  wxZlibInputStream zinCairo(inCairo);
  wxFileOutputStream cairoCff(wxT("cairo-cff.dat"));
  cairoCff.Write(zinCairo);
  cairoCff.Close();
#endif
}

// -- Subset global and local subroutines

void
wxPdfFontSubsetCff::FindLocalAndGlobalSubrsUsed()
{
  int nGlobalSubrs = (int) m_globalSubrIndex->GetCount();
  // Calc the Bias for the global subr index
  m_globalBias = m_decoder->CalcBias(nGlobalSubrs);

  // If the font is CID then the lsubrs are divided into FontDicts.
  // for each FD array the lsubrs will be subsetted.
  if (m_isCid)
  {
    bool* fdDictUsed = new bool[m_numFontDicts];
    int j;
    for (j = 0; j < m_numFontDicts; j++)
    {
      fdDictUsed[j] = false;
    }
    int glyph, fd;
    for (j = 0; j < m_numGlyphsUsed; j++)
    {
      glyph = m_usedGlyphs[j];
      fd = m_fdSelect[glyph];
      fdDictUsed[fd] = true;
    }
    // For each FD array which is used subset the lsubr 
    for (j = 0; j < m_numFontDicts; j++)
    {
      if (fdDictUsed[j])
      {
        wxPdfSortedArrayInt hSubrsUsed(CompareInts);
        wxArrayInt lSubrsUsed;
        //Scans the Charsting data storing the used Local and Global subroutines 
        // by the glyphs. Scans the Subrs recursivley. 
        FindSubrsUsed(j, *((wxPdfCffIndexArray*) m_fdLocalSubrIndex[j]), hSubrsUsed, lSubrsUsed);
        // Builds the New Local Subrs index
        SubsetSubrs(*((wxPdfCffIndexArray*) m_fdLocalSubrIndex[j]), hSubrsUsed);
      }
    }
    delete [] fdDictUsed;
  }
  else
  {
    //Scans the Charsting data storing the used Local and Global subroutines 
    // by the glyphs. Scans the Subrs recursivley.
    FindSubrsUsed(-1, *m_localSubrIndex, *m_hLocalSubrsUsed, m_lLocalSubrsUsed);
  }
  // Subset the Global Subroutines
  // Scan the Global Subr Hashmap recursivly on the Gsubrs
  FindGlobalSubrsUsed();
  SubsetSubrs(*m_globalSubrIndex, *m_hGlobalSubrsUsed);
  if (!m_isCid)
  {
    SubsetSubrs(*m_localSubrIndex, *m_hLocalSubrsUsed);
  }
}

void
wxPdfFontSubsetCff::SubsetSubrs(wxPdfCffIndexArray& subrIndex, wxPdfSortedArrayInt& subrsUsed)
{
  size_t nSubrs = subrIndex.GetCount();
  if (nSubrs > 0)
  {
    size_t j;
    bool* isSubrUsed = new bool[nSubrs];
    int subr;
    for (j = 0; j < nSubrs; j++)
    {
      isSubrUsed[j] = false;
    }
    for (j = 0; j < subrsUsed.GetCount(); j++)
    {
      subr = subrsUsed[j];
      isSubrUsed[subr] = true;
    }
    wxMemoryOutputStream buffer;
    char subrReturnOp = SUBR_RETURN_OP;
    buffer.Write(&subrReturnOp, 1);
    buffer.Close();
    for (j = 0; j < nSubrs; j++)
    {
      if (!isSubrUsed[j])
      {
        wxPdfCffIndexElement& subrElement = subrIndex[j];
        subrElement.SetBuffer(buffer);
      }
    }
    delete [] isSubrUsed;
  }
}

void
wxPdfFontSubsetCff::FindSubrsUsed(int fd, wxPdfCffIndexArray& localSubrIndex, 
                              wxPdfSortedArrayInt& hSubrsUsed, wxArrayInt& lSubrsUsed)
{
  // Calc the Bias for the subr index
  int nSubrs = (int) localSubrIndex.GetCount();
  int localBias = m_decoder->CalcBias(nSubrs);
    
  // For each glyph used find its GID, start & end pos
  size_t j;
  for (j = 0; j < m_usedGlyphs.GetCount(); j++)
  {
    int glyph = m_usedGlyphs.Item(j);
    int fdGlyph = (m_isCid) ? m_fdSelect[glyph] : -1;

    if (fdGlyph == fd)
    {
      wxPdfCffIndexElement& charstring = (*m_charstringsIndex)[glyph];
      int beginChar = charstring.GetOffset();
      int endChar = beginChar + charstring.GetLength();
      m_decoder->ReadASubr(m_inFont, beginChar, endChar, m_globalBias, localBias, hSubrsUsed, lSubrsUsed, localSubrIndex);
    }
  }
  // For all Lsubrs used, check recusrivly for Lsubr & Gsubr used
  for (j = 0; j < lSubrsUsed.GetCount(); j++)
  {
    // Pop the subr value from the hash
    int subr = lSubrsUsed.Item(j);
    // Ensure the Lsubr call is valid
    if (subr < nSubrs && subr >= 0)
    {
      // Read and process the subr
      wxPdfCffIndexElement& localSub = localSubrIndex[subr];
      int start = localSub.GetOffset();
      int end = start + localSub.GetLength();
      m_decoder->ReadASubr(m_inFont, start, end, m_globalBias, localBias, hSubrsUsed, lSubrsUsed, localSubrIndex);
    }
  }
}
  
void
wxPdfFontSubsetCff::FindGlobalSubrsUsed()
{
  int nGlobalSubrs = (int) m_globalSubrIndex->GetCount();
  int nLocalSubrs = (int) m_localSubrIndex->GetCount();
  int localBias = 0;
  size_t sizeOfNonCIDSubrsUsed = 0;
  if (!m_isCid)
  {
    // Calc the Bias for the local subr index
    localBias = m_decoder->CalcBias(nLocalSubrs);
    sizeOfNonCIDSubrsUsed = m_lLocalSubrsUsed.GetCount();
  }
    
  // For each global subr used 
  size_t k;
  for (k = 0; k < m_lGlobalSubrsUsed.GetCount(); k++)
  {
    //Pop the value + check valid 
    int subr = m_lGlobalSubrsUsed.Item(k);
    if (subr < nGlobalSubrs && subr >= 0)
    {
      // Read the subr and process
      wxPdfCffIndexElement& globalSubr = (*m_globalSubrIndex)[subr];
      int start = globalSubr.GetOffset();
      int end = start + globalSubr.GetLength();
        
      if (m_isCid)
      {
        wxPdfCffIndexArray dummy;
        m_decoder->ReadASubr(m_inFont, start, end, m_globalBias, 0, *m_hGlobalSubrsUsed, m_lGlobalSubrsUsed, dummy);
      }
      else
      {
#if 0
        wxLogDebug(wxT("Call ReadASubr i=%d subr=%d"), i, subr);
#endif
        m_decoder->ReadASubr(m_inFont, start, end, m_globalBias, localBias, 
                             *m_hLocalSubrsUsed, m_lLocalSubrsUsed, *m_localSubrIndex);
        if (sizeOfNonCIDSubrsUsed < m_lLocalSubrsUsed.GetCount())
        {
          size_t j;
          for (j = sizeOfNonCIDSubrsUsed; j < m_lLocalSubrsUsed.GetCount(); j++)
          {
            //Pop the value + check valid 
            int lSubr = m_lLocalSubrsUsed.Item(j);
#if 0
            wxLogDebug(wxT("Call ReadASubr j=%d subr=%d"), i, subr);
#endif
            if (lSubr < nLocalSubrs && lSubr >= 0)
            {
              // Read the subr and process
              wxPdfCffIndexElement& localSubr = (*m_localSubrIndex)[lSubr];
              int lStart = localSubr.GetOffset();
              int lEnd = lStart + localSubr.GetLength();
              m_decoder->ReadASubr(m_inFont, lStart, lEnd, m_globalBias, localBias,
                                   *m_hLocalSubrsUsed, m_lLocalSubrsUsed, *m_localSubrIndex);
            }
          }
          sizeOfNonCIDSubrsUsed = m_lLocalSubrsUsed.GetCount();
        }
      }
    }
  }
}

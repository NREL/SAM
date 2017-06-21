///////////////////////////////////////////////////////////////////////////////
// Name:        pdfcffdecoder.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2008-08-01
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfcffdecoder.cpp Implementation of Type1 and Type2 CFF decoder class

/*
 * This Class decodes a Type1 or Type2 CFF string. The code is based on code and ideas from the iText project.
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
#include "wex/pdf/pdfcffindex.h"
#include "wex/pdf/pdfcffdecoder.h"

#include "wxmemdbg.h"

// --- Implementation of CFF Decoder for charstring types 1 and 2

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

#if 0
static const wxChar* gs_standardStrings[] = {
    // Generated from Appendix A of the CFF specification. Size should be 391.
    wxT(".notdef"),             wxT("space"),              wxT("exclam"),           wxT("quotedbl"),       wxT("numbersign"),
    wxT("dollar"),              wxT("percent"),            wxT("ampersand"),        wxT("quoteright"),     wxT("parenleft"),
    wxT("parenright"),          wxT("asterisk"),           wxT("plus"),             wxT("comma"),          wxT("hyphen"), 
    wxT("period"),              wxT("slash"),              wxT("zero"),             wxT("one"),            wxT("two"),
    wxT("three"),               wxT("four"),               wxT("five"),             wxT("six"),            wxT("seven"),
    wxT("eight"),               wxT("nine"),               wxT("colon"),            wxT("semicolon"),      wxT("less"),
    wxT("equal"),               wxT("greater"),            wxT("question"),         wxT("at"),             wxT("A"),
    wxT("B"),                   wxT("C"),                  wxT("D"),                wxT("E"),              wxT("F"),
    wxT("G"),                   wxT("H"),                  wxT("I"),                wxT("J"),              wxT("K"),
    wxT("L"),                   wxT("M"),                  wxT("N"),                wxT("O"),              wxT("P"),
    wxT("Q"),                   wxT("R"),                  wxT("S"),                wxT("T"),              wxT("U"),
    wxT("V"),                   wxT("W"),                  wxT("X"),                wxT("Y"),              wxT("Z"),
    wxT("bracketleft"),         wxT("backslash"),          wxT("bracketright"),     wxT("asciicircum"),    wxT("underscore"),
    wxT("quoteleft"),           wxT("a"),                  wxT("b"),                wxT("c"),              wxT("d"),
    wxT("e"),                   wxT("f"),                  wxT("g"),                wxT("h"),              wxT("i"),
    wxT("j"),                   wxT("k"),                  wxT("l"),                wxT("m"),              wxT("n"),
    wxT("o"),                   wxT("p"),                  wxT("q"),                wxT("r"),              wxT("s"),
    wxT("t"),                   wxT("u"),                  wxT("v"),                wxT("w"),              wxT("x"),
    wxT("y"),                   wxT("z"),                  wxT("braceleft"),        wxT("bar"),            wxT("braceright"),
    wxT("asciitilde"),          wxT("exclamdown"),         wxT("cent"),             wxT("sterling"),       wxT("fraction"),
    wxT("yen"),                 wxT("florin"),             wxT("section"),          wxT("currency"),       wxT("quotesingle"),
    wxT("quotedblleft"),        wxT("guillemotleft"),      wxT("guilsinglleft"),    wxT("guilsinglright"), wxT("fi"),
    wxT("fl"),                  wxT("endash"),             wxT("dagger"),           wxT("daggerdbl"),      wxT("periodcentered"),
    wxT("paragraph"),           wxT("bullet"),             wxT("quotesinglbase"),   wxT("quotedblbase"),   wxT("quotedblright"),
    wxT("guillemotright"),      wxT("ellipsis"),           wxT("perthousand"),      wxT("questiondown"),   wxT("grave"),
    wxT("acute"),               wxT("circumflex"),         wxT("tilde"),            wxT("macron"),         wxT("breve"),
    wxT("dotaccent"),           wxT("dieresis"),           wxT("ring"),             wxT("cedilla"),        wxT("hungarumlaut"),
    wxT("ogonek"),              wxT("caron"),              wxT("emdash"),           wxT("AE"),             wxT("ordfeminine"),
    wxT("Lslash"),              wxT("Oslash"),             wxT("OE"),               wxT("ordmasculine"),   wxT("ae"),
    wxT("dotlessi"),            wxT("lslash"),             wxT("oslash"),           wxT("oe"),             wxT("germandbls"),
    wxT("onesuperior"),         wxT("logicalnot"),         wxT("mu"),               wxT("trademark"),      wxT("Eth"),
    wxT("onehalf"),             wxT("plusminus"),          wxT("Thorn"),            wxT("onequarter"),     wxT("divide"),
    wxT("brokenbar"),           wxT("degree"),             wxT("thorn"),            wxT("threequarters"),  wxT("twosuperior"),
    wxT("registered"),          wxT("minus"),              wxT("eth"),              wxT("multiply"),       wxT("threesuperior"),
    wxT("copyright"),           wxT("Aacute"),             wxT("Acircumflex"),      wxT("Adieresis"),      wxT("Agrave"),
    wxT("Aring"),               wxT("Atilde"),             wxT("Ccedilla"),         wxT("Eacute"),         wxT("Ecircumflex"),
    wxT("Edieresis"),           wxT("Egrave"),             wxT("Iacute"),           wxT("Icircumflex"),    wxT("Idieresis"),
    wxT("Igrave"),              wxT("Ntilde"),             wxT("Oacute"),           wxT("Ocircumflex"),    wxT("Odieresis"),
    wxT("Ograve"),              wxT("Otilde"),             wxT("Scaron"),           wxT("Uacute"),         wxT("Ucircumflex"),
    wxT("Udieresis"),           wxT("Ugrave"),             wxT("Yacute"),           wxT("Ydieresis"),      wxT("Zcaron"),
    wxT("aacute"),              wxT("acircumflex"),        wxT("adieresis"),        wxT("agrave"),         wxT("aring"),
    wxT("atilde"),              wxT("ccedilla"),           wxT("eacute"),           wxT("ecircumflex"),    wxT("edieresis"),
    wxT("egrave"),              wxT("iacute"),             wxT("icircumflex"),      wxT("idieresis"),      wxT("igrave"),
    wxT("ntilde"),              wxT("oacute"),             wxT("ocircumflex"),      wxT("odieresis"),      wxT("ograve"),
    wxT("otilde"),              wxT("scaron"),             wxT("uacute"),           wxT("ucircumflex"),    wxT("udieresis"),
    wxT("ugrave"),              wxT("yacute"),             wxT("ydieresis"),        wxT("zcaron"),         wxT("exclamsmall"),
    wxT("Hungarumlautsmall"),   wxT("dollaroldstyle"),     wxT("dollarsuperior"),   wxT("ampersandsmall"), wxT("Acutesmall"),
    wxT("parenleftsuperior"),   wxT("parenrightsuperior"), wxT("twodotenleader"),   wxT("onedotenleader"), wxT("zerooldstyle"),
    wxT("oneoldstyle"),         wxT("twooldstyle"),        wxT("threeoldstyle"),    wxT("fouroldstyle"),   wxT("fiveoldstyle"),
    wxT("sixoldstyle"),         wxT("sevenoldstyle"),      wxT("eightoldstyle"),    wxT("nineoldstyle"),   wxT("commasuperior"),
    wxT("threequartersemdash"), wxT("periodsuperior"),     wxT("questionsmall"),    wxT("asuperior"),      wxT("bsuperior"),
    wxT("centsuperior"),        wxT("dsuperior"),          wxT("esuperior"),        wxT("isuperior"),      wxT("lsuperior"),
    wxT("msuperior"),           wxT("nsuperior"),          wxT("osuperior"),        wxT("rsuperior"),      wxT("ssuperior"),
    wxT("tsuperior"),           wxT("ff"),                 wxT("ffi"),              wxT("ffl"),            wxT("parenleftinferior"),
    wxT("parenrightinferior"),  wxT("Circumflexsmall"),    wxT("hyphensuperior"),   wxT("Gravesmall"),     wxT("Asmall"),
    wxT("Bsmall"),              wxT("Csmall"),             wxT("Dsmall"),           wxT("Esmall"),         wxT("Fsmall"),
    wxT("Gsmall"),              wxT("Hsmall"),             wxT("Ismall"),           wxT("Jsmall"),         wxT("Ksmall"),
    wxT("Lsmall"),              wxT("Msmall"),             wxT("Nsmall"),           wxT("Osmall"),         wxT("Psmall"),
    wxT("Qsmall"),              wxT("Rsmall"),             wxT("Ssmall"),           wxT("Tsmall"),         wxT("Usmall"),
    wxT("Vsmall"),              wxT("Wsmall"),             wxT("Xsmall"),           wxT("Ysmall"),         wxT("Zsmall"),
    wxT("colonmonetary"),       wxT("onefitted"),          wxT("rupiah"),           wxT("Tildesmall"),     wxT("exclamdownsmall"),
    wxT("centoldstyle"),        wxT("Lslashsmall"),        wxT("Scaronsmall"),      wxT("Zcaronsmall"),    wxT("Dieresissmall"),
    wxT("Brevesmall"),          wxT("Caronsmall"),         wxT("Dotaccentsmall"),   wxT("Macronsmall"),    wxT("figuredash"),
    wxT("hypheninferior"),      wxT("Ogoneksmall"),        wxT("Ringsmall"),        wxT("Cedillasmall"),   wxT("questiondownsmall"),
    wxT("oneeighth"),           wxT("threeeighths"),       wxT("fiveeighths"),      wxT("seveneighths"),   wxT("onethird"),
    wxT("twothirds"),           wxT("zerosuperior"),       wxT("foursuperior"),     wxT("fivesuperior"),   wxT("sixsuperior"),
    wxT("sevensuperior"),       wxT("eightsuperior"),      wxT("ninesuperior"),     wxT("zeroinferior"),   wxT("oneinferior"),
    wxT("twoinferior"),         wxT("threeinferior"),      wxT("fourinferior"),     wxT("fiveinferior"),   wxT("sixinferior"),
    wxT("seveninferior"),       wxT("eightinferior"),      wxT("nineinferior"),     wxT("centinferior"),   wxT("dollarinferior"),
    wxT("periodinferior"),      wxT("commainferior"),      wxT("Agravesmall"),      wxT("Aacutesmall"),    wxT("Acircumflexsmall"),
    wxT("Atildesmall"),         wxT("Adieresissmall"),     wxT("Aringsmall"),       wxT("AEsmall"),        wxT("Ccedillasmall"),
    wxT("Egravesmall"),         wxT("Eacutesmall"),        wxT("Ecircumflexsmall"), wxT("Edieresissmall"), wxT("Igravesmall"),
    wxT("Iacutesmall"),         wxT("Icircumflexsmall"),   wxT("Idieresissmall"),   wxT("Ethsmall"),       wxT("Ntildesmall"),
    wxT("Ogravesmall"),         wxT("Oacutesmall"),        wxT("Ocircumflexsmall"), wxT("Otildesmall"),    wxT("Odieresissmall"),
    wxT("OEsmall"),             wxT("Oslashsmall"),        wxT("Ugravesmall"),      wxT("Uacutesmall"),    wxT("Ucircumflexsmall"),
    wxT("Udieresissmall"),      wxT("Yacutesmall"),        wxT("Thornsmall"),       wxT("Ydieresissmall"), wxT("001.000"),
    wxT("001.001"),             wxT("001.002"),            wxT("001.003"),          wxT("Black"),          wxT("Bold"),
    wxT("Book"),                wxT("Light"),              wxT("Medium"),           wxT("Regular"),        wxT("Roman"),
    wxT("Semibold")
  };
static int gs_standardStringsCount = sizeof(gs_standardStrings) / sizeof(wxChar*);
#endif

// The Strings in this array represent Type1/Type2 operator names
static const wxChar* gs_subrsFunctions[] = {
    wxT("RESERVED_0"),  wxT("hstem"),       wxT("RESERVED_2"),  wxT("vstem"),          wxT("vmoveto"),
    wxT("rlineto"),     wxT("hlineto"),     wxT("vlineto"),     wxT("rrcurveto"),      wxT("RESERVED_9"),
    wxT("callsubr"),    wxT("return"),      wxT("escape"),      wxT("hsbw"),/*RES_13*/ wxT("endchar"),
    wxT("RESERVED_15"), wxT("RESERVED_16"), wxT("RESERVED_17"), wxT("hstemhm"),        wxT("hintmask"),
    wxT("cntrmask"),    wxT("rmoveto"),     wxT("hmoveto"),     wxT("vstemhm"),        wxT("rcurveline"),
    wxT("rlinecurve"),  wxT("vvcurveto"),   wxT("hhcurveto"),   wxT("shortint"),       wxT("callgsubr"),
    wxT("vhcurveto"),   wxT("hvcurveto")
  };
#if 0
static int gs_subrsFunctionsCount = sizeof(gs_subrsFunctions) / sizeof(wxChar*);
#endif

// The Strings in this array represent Type1/Type2 escape operator names
static const wxChar* gs_subrsEscapeFuncs[] = {
    wxT("RESERVED_0"),  wxT("RESERVED_1"),    wxT("RESERVED_2"),   wxT("and"),           wxT("or"), 
    wxT("not"),         wxT("seac"),/*RES_6*/ wxT("sbw"),/*RES_7*/ wxT("RESERVED_8"),    wxT("abs"),
    wxT("add"),         wxT("sub"),           wxT("div"),          wxT("RESERVED_13"),   wxT("neg"),
    wxT("eq"),          wxT("RESERVED_16"),   wxT("RESERVED_17"),  wxT("drop"),          wxT("RESERVED_19"),
    wxT("put"),         wxT("get"),           wxT("ifelse"),       wxT("random"),        wxT("mul"),
    wxT("RESERVED_25"), wxT("sqrt"),          wxT("dup"),          wxT("exch"),          wxT("index"), 
    wxT("roll"),        wxT("RESERVED_31"),   wxT("RESERVED_32"),  wxT("RESERVED_33"),   wxT("hflex"),
    wxT("flex"),        wxT("hflex1"),        wxT("flex1"),        wxT("RESERVED_REST")
  };
static int gs_subrsEscapeFuncsCount = sizeof(gs_subrsEscapeFuncs) / sizeof(wxChar*);

#if 0
static wxChar* gs_operatorNames[] = {
    wxT("version"),           wxT("Notice"),             wxT("FullName"),      wxT("FamilyName"),     wxT("Weight"),
    wxT("FontBBox"),          wxT("BlueValues"),         wxT("OtherBlues"),    wxT("FamilyBlues"),    wxT("FamilyOtherBlues"),
    wxT("StdHW"),             wxT("StdVW"),              wxT("UNKNOWN_12"),    wxT("UniqueID"),       wxT("XUID"),
    wxT("charset"),           wxT("Encoding"),           wxT("CharStrings"),   wxT("Private"),        wxT("Subrs"),
    wxT("defaultWidthX"),     wxT("nominalWidthX"),      wxT("UNKNOWN_22"),    wxT("UNKNOWN_23"),     wxT("UNKNOWN_24"),
    wxT("UNKNOWN_25"),        wxT("UNKNOWN_26"),         wxT("UNKNOWN_27"),    wxT("UNKNOWN_28"),     wxT("UNKNOWN_29"),
    wxT("UNKNOWN_30"),        wxT("UNKNOWN_31"),         wxT("Copyright"),     wxT("isFixedPitch"),   wxT("ItalicAngle"),
    wxT("UnderlinePosition"), wxT("UnderlineThickness"), wxT("PaintType"),     wxT("CharstringType"), wxT("FontMatrix"),
    wxT("StrokeWidth"),       wxT("BlueScale"),          wxT("BlueShift"),     wxT("BlueFuzz"),       wxT("StemSnapH"),
    wxT("StemSnapV"),         wxT("ForceBold"),          wxT("UNKNOWN_12_15"), wxT("UNKNOWN_12_16"),  wxT("LanguageGroup"),
    wxT("ExpansionFactor"),   wxT("initialRandomSeed"),  wxT("SyntheticBase"), wxT("PostScript"),     wxT("BaseFontName"),
    wxT("BaseFontBlend"),     wxT("UNKNOWN_12_24"),      wxT("UNKNOWN_12_25"), wxT("UNKNOWN_12_26"),  wxT("UNKNOWN_12_27"),
    wxT("UNKNOWN_12_28"),     wxT("UNKNOWN_12_29"),      wxT("ROS"),           wxT("CIDFontVersion"), wxT("CIDFontRevision"),
    wxT("CIDFontType"),       wxT("CIDCount"),           wxT("UIDBase"),       wxT("FDArray"),        wxT("FDSelect"),
    wxT("FontName")
  };
static int gs_operatorNamesCount = sizeof(gs_operatorNames) / sizeof(wxChar*);
#endif

class wxPdfCffFontObject
{
public:
  wxPdfCffFontObject() {}
  int      m_type;
  int      m_intValue;
  wxString m_strValue;
};

wxPdfCffDecoder::wxPdfCffDecoder()
{
  m_charstringType = 1;
  
  m_globalSubrIndex  = NULL;
  m_hGlobalSubrsUsed = NULL;
  m_lGlobalSubrsUsed = NULL;

  m_args = new wxPdfCffFontObject[48];
  m_argCount = 0;
}

wxPdfCffDecoder::wxPdfCffDecoder(wxPdfCffIndexArray* globalSubrIndex,
                                 wxPdfSortedArrayInt* hGlobalSubrsUsed,
                                 wxArrayInt* lGlobalSubrsUsed)
{
  m_charstringType = 2;
  
  m_globalSubrIndex  = globalSubrIndex;
  m_hGlobalSubrsUsed = hGlobalSubrsUsed;
  m_lGlobalSubrsUsed = lGlobalSubrsUsed;

  m_args = new wxPdfCffFontObject[48];
  m_argCount = 0;
}

wxPdfCffDecoder::~wxPdfCffDecoder()
{
  delete [] m_args;
}

// --- Read original CFF stream

unsigned char
wxPdfCffDecoder::ReadByte(wxInputStream* stream)
{
  unsigned char card8;
  stream->Read(&card8, 1);
  return card8;
}

short
wxPdfCffDecoder::ReadShort(wxInputStream* stream)
{
  // Read a 2-byte integer from file (big endian)
  short i16;
  stream->Read(&i16, 2);
  return wxINT16_SWAP_ON_LE(i16);
}

int
wxPdfCffDecoder::ReadInt(wxInputStream* stream)
{
  // Read a 4-byte integer from file (big endian)
  int i32;
  stream->Read(&i32, 4);
  return wxINT32_SWAP_ON_LE(i32);
}

// -- Subset global and local subroutines

int
wxPdfCffDecoder::CalcBias(int nSubrs)
{
  int bias;
  // If type == 1 then bias = 0, else calc according to the count
  if (m_charstringType == 1)
  {
    bias = 0;
  }
  else if (nSubrs < 1240)
  {
    bias = 107;
  }
  else if (nSubrs < 33900)
  {
    bias = 1131;
  }
  else
  {
    bias = 32768;
  }
  return bias;
}

bool
wxPdfCffDecoder::GetCharWidthAndComposite(wxPdfCffIndexElement& charstring, int& width, bool& isComposite, int& bchar, int& achar)
{
  bool ok = false;
  width = -1;
  isComposite = false;
  bchar = -1;
  achar = -1;

  wxInputStream* stream = charstring.GetBuffer();
  int begin = charstring.GetOffset();
  int end   = begin + charstring.GetLength();

  // Clear the stack
  EmptyStack();
  m_numHints = 0;

  stream->SeekI(begin);
  ReadCommand(stream);
  wxPdfCffFontObject* element = NULL;
  int numArgs = m_argCount;
  HandleStack();
  if (m_key == wxT("hsbw"))
  {
    if (numArgs == 2)
    {
      ok = true;
      element = &m_args[1]; // 2nd argument is width
      width = element->m_intValue;
    }
  }
  else if (m_key == wxT("sbw"))
  {
    if (numArgs == 4)
    {
      ok = true;
      element = &m_args[2]; // 3rd argument is width
      width = element->m_intValue;
    }
  }
  if (ok && (stream->TellI() < end))
  {
    ReadCommand(stream);
    numArgs = m_argCount;
    // Check the modification needed on the Argument Stack according to key;
    HandleStack();
    if (m_key == wxT("seac"))
    {
      if (numArgs == 5)
      {
        isComposite = true;
        // third argument
        element = &m_args[3];
        bchar = element->m_intValue;
        element = &m_args[4];
        achar = element->m_intValue;
      }
    }
  }
  return ok;
}

void
wxPdfCffDecoder::ReadASubr(wxInputStream* stream, int begin, int end,
                           int globalBias, int localBias, 
                           wxPdfSortedArrayInt& hSubrsUsed, wxArrayInt& lSubrsUsed,
                           wxPdfCffIndexArray& localSubrIndex)
{
  int beginSubr, endSubr;
#if 0
  wxLogDebug(wxT("ReadAsubr %d %d %d %d"), begin, end, globalBias, localBias);
#endif
  // Clear the stack for the subrs
  EmptyStack();
  m_numHints = 0;
  // Goto begining of the subr
  stream->SeekI(begin);
  while (stream->TellI() < end)
  {
    // Read the next command
    ReadCommand(stream);
    int pos = stream->TellI();
    wxPdfCffFontObject* topElement = NULL;
    if (m_argCount > 0)
    {
      topElement = &m_args[m_argCount-1];
    }
    int numArgs = m_argCount;
    // Check the modification needed on the Argument Stack according to key;
    HandleStack();
    // a call to a Lsubr
    if (m_key == wxT("callsubr")) 
    {
      // Verify that arguments are passed 
      if (numArgs > 0)
      {
        // Calc the index of the Subrs
        int subr = topElement->m_intValue + localBias;
        // If the subr isn't in the HashMap -> Put in
        if (hSubrsUsed.Index(subr) == wxNOT_FOUND)
        {
#if 0
          wxLogDebug(wxT("Add hSubr: %s %d"), m_key.c_str(), subr);
#endif
          hSubrsUsed.Add(subr);
          lSubrsUsed.Add(subr);
        }
        wxPdfCffIndexElement& localSubr = localSubrIndex[subr];
        beginSubr = localSubr.GetOffset();
        endSubr = beginSubr + localSubr.GetLength();
        CalcHints(localSubr.GetBuffer(), beginSubr, endSubr, globalBias, localBias, localSubrIndex);
        stream->SeekI(pos);
      }            
    }
    // a call to a Gsubr
    else if (m_key == wxT("callgsubr"))
    {
      // Verify that arguments are passed 
      if (numArgs > 0)
      {
        // Calc the index of the Subrs
        int subr = topElement->m_intValue + globalBias;
        // If the subr isn't in the HashMap -> Put in
        if (m_hGlobalSubrsUsed->Index(subr) == wxNOT_FOUND)
        {
#if 0
          wxLogDebug(wxT("Add hGSubr: %s %d"), m_key.c_str(), subr);
#endif
          m_hGlobalSubrsUsed->Add(subr);
          m_lGlobalSubrsUsed->Add(subr);
        }
        wxPdfCffIndexElement& globalSubr = (*m_globalSubrIndex)[subr];
        beginSubr = globalSubr.GetOffset();
        endSubr = beginSubr + globalSubr.GetLength();
        CalcHints(globalSubr.GetBuffer(), beginSubr, endSubr, globalBias, localBias, localSubrIndex);
        stream->SeekI(pos);
      }
    }
    // A call to "stem"
    else if (m_key == wxT("hstem") || m_key == wxT("vstem") || m_key == wxT("hstemhm") || m_key == wxT("vstemhm"))
    {
      // Increment the NumOfHints by the number couples of of arguments
      m_numHints += numArgs / 2;
    }
          // A call to "mask"
    else if (m_key == wxT("hintmask") || m_key == wxT("cntrmask"))
    {
      // Compute the size of the mask
      int sizeOfMask = m_numHints / 8;
      if (m_numHints % 8 != 0 || sizeOfMask == 0)
      {
        sizeOfMask++;
      }
      // Continue the pointer in SizeOfMask steps
      int i;
      for (i = 0; i < sizeOfMask; i++)
      {
        ReadByte(stream);
      }
    }
  }
#if 0
  wxLogDebug(wxT("ReadASubr end"));
#endif
}

void
wxPdfCffDecoder::HandleStack()
{
  // Findout what the operator does to the stack
  int stackHandle = StackOpp();
  if (stackHandle < 2)
  {
    // The operators that enlarge the stack by one
    if (stackHandle == 1)
    {
      PushStack();
    }
    // The operators that pop the stack
    else
    {
      // Abs value for the for loop
      stackHandle *= -1;
      int i;
      for (i = 0; i < stackHandle; i++)
      {
        PopStack();
      }
    }
  }
  // All other flush the stack
  else
  {
    EmptyStack();
  }
}
  
int
wxPdfCffDecoder::StackOpp()
{
  int op;
  if (m_key == wxT("ifelse"))
  {
    op = -3;
  }
  else if (m_key == wxT("roll") || m_key == wxT("put"))
  {
    op = -2;
  }
  else if (m_key == wxT("callsubr") || m_key == wxT("callgsubr") || m_key == wxT("add")  ||
           m_key == wxT("sub")      || m_key == wxT("div")       || m_key == wxT("mul")  ||
           m_key == wxT("drop")     || m_key == wxT("and")       || m_key == wxT("or")   ||
           m_key == wxT("eq"))
  {
    op = -1;
  }
  else if (m_key == wxT("abs")  || m_key == wxT("neg")   || m_key == wxT("sqrt") ||
           m_key == wxT("exch") || m_key == wxT("index") || m_key == wxT("get")  ||
           m_key == wxT("not")  || m_key == wxT("return"))
  {
    op = 0;
  }
  else if (m_key == wxT("random") || m_key == wxT("dup"))
  {
    op = 1;
  }
  else
  {
    op = 2;
  }
  return op;
}
  
void
wxPdfCffDecoder::EmptyStack()
{
  m_argCount = 0;    
}
  
void
wxPdfCffDecoder::PopStack()
{
  if (m_argCount > 0)
  {
    m_argCount--;
  }
}
  
void
wxPdfCffDecoder::PushStack()
{
  m_argCount++;
}
  
void
wxPdfCffDecoder::ReadCommand(wxInputStream* stream)
{
  m_key = wxEmptyString;
  bool gotKey = false;
  // Until a key is found
  while (!gotKey)
  {
    // Read the first Char
    unsigned char b0 = ReadByte(stream);
    // decode according to the type1/type2 format
    if (b0 == 28) // the two next bytes represent a short int;
    {
      int first = ReadByte(stream) & 0xff;
      int second = ReadByte(stream) & 0xff;
      m_args[m_argCount].m_type = 0;
      m_args[m_argCount].m_intValue = first << 8 | second;
      m_argCount++;
      continue;
    }
    if (b0 >= 32 && b0 <= 246) // The byte read is the byte;
    {
      m_args[m_argCount].m_type = 0;
      m_args[m_argCount].m_intValue = (int) (b0 - 139);
      m_argCount++;
      continue;
    }
    if (b0 >= 247 && b0 <= 250) // The byte read and the next byte constetute a short int
    {
      unsigned char b1 = ReadByte(stream);
      short item = (short) ((b0-247)*256 + b1 + 108);
      m_args[m_argCount].m_type = 0;
      m_args[m_argCount].m_intValue = item;
      m_argCount++;
      continue;
    }
    if (b0 >= 251 && b0 <= 254)// Same as above except negative
    {
      unsigned char b1 = ReadByte(stream);
      short item = (short) (-(b0-251)*256-b1-108);
      m_args[m_argCount].m_type = 0;
      m_args[m_argCount].m_intValue = item;
      m_argCount++;
      continue;
    }
    if (b0 == 255)// The next for bytes represent a double.
    {
      int item = ReadInt(stream);
      m_args[m_argCount].m_type = 0;
      m_args[m_argCount].m_intValue = item;
      m_argCount++;
      continue;
    }
    if (b0 <= 31 && b0 != 28) // An operator was found.. Set Key.
    {
      gotKey = true;
      // 12 is an escape command therefor the next byte is a part
      // of this command
      if (b0 == 12)
      {
        unsigned char b1 = ReadByte(stream);
        if (b1 > gs_subrsEscapeFuncsCount-1)
        {
          b1 = gs_subrsEscapeFuncsCount-1;
        }
        m_key = gs_subrsEscapeFuncs[b1];
      }
      else
      {
        m_key = gs_subrsFunctions[b0];
      }
      continue;
    }
  }
}
  
int
wxPdfCffDecoder::CalcHints(wxInputStream* stream, int begin, int end, int globalBias, int localBias, wxPdfCffIndexArray& localSubrIndex)
{
  int beginSubr, endSubr;
  // Goto begining of the subr
  stream->SeekI(begin);
  while (stream->TellI() < end)
  {
    // Read the next command
    ReadCommand(stream);
    int pos = stream->TellI();
    wxPdfCffFontObject* topElement = NULL;
    if (m_argCount > 0)
    {
      topElement = &m_args[m_argCount-1];
    }
    int numArgs = m_argCount;
    //Check the modification needed on the Argument Stack according to key;
    HandleStack();
    // a call to a Lsubr
    if (m_key == wxT("callsubr")) 
    {
      if (numArgs > 0)
      {
        int subr = topElement->m_intValue + localBias;
        wxPdfCffIndexElement& localSubr = localSubrIndex[subr];
        beginSubr = localSubr.GetOffset();
        endSubr = beginSubr + localSubr.GetLength();
        CalcHints(localSubr.GetBuffer(), beginSubr, endSubr, globalBias, localBias, localSubrIndex);
        stream->SeekI(pos);
      }
    }
    // a call to a Gsubr
    else if (m_key == wxT("callgsubr"))
    {
      if (numArgs > 0)
      {
        int subr = topElement->m_intValue + globalBias;
        wxPdfCffIndexElement& globalSubr = (*m_globalSubrIndex)[subr];
        beginSubr = globalSubr.GetOffset();
        endSubr = beginSubr + globalSubr.GetLength();
        CalcHints(globalSubr.GetBuffer(), beginSubr, endSubr, globalBias, localBias, localSubrIndex);
        stream->SeekI(pos);
      }
    }
    // A call to "stem"
    else if (m_key == wxT("hstem") || m_key == wxT("vstem") || m_key == wxT("hstemhm") || m_key == wxT("vstemhm"))
    {
      // Increment the NumOfHints by the number couples of of arguments
      m_numHints += numArgs / 2;
    }
    // A call to "mask"
    else if (m_key == wxT("hintmask") || m_key == wxT("cntrmask"))
    {
      // Compute the size of the mask
      int sizeOfMask = m_numHints / 8;
      if (m_numHints % 8 != 0 || sizeOfMask == 0)
      {
        sizeOfMask++;
      }
      // Continue the pointer in SizeOfMask steps
      int i;
      for (i = 0; i < sizeOfMask; i++)
      {
        ReadByte(stream);
      }
    }
  }
  return m_numHints;
}

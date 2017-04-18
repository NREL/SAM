///////////////////////////////////////////////////////////////////////////////
// Name:        pdfdecode.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2006-10-15
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfdecode.cpp Implementation of the PDF decoding algorithms

// For compilers that support precompilation, includes <wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

// includes

#include <wx/zstream.h>

#include "wex/pdf/pdfobjects.h"
#include "wex/pdf/pdfparser.h"

// --- Flate Decode method

wxMemoryOutputStream*
wxPdfParser::FlateDecode(wxMemoryOutputStream* osIn)
{
  wxMemoryInputStream in(*osIn);
  wxZlibInputStream zin(in);
  wxMemoryOutputStream* osOut = new wxMemoryOutputStream();;
  osOut->Write(zin);
  osOut->Close();
  return osOut;
}

// --- ASCII Hexadecimal Decode method

wxMemoryOutputStream*
wxPdfParser::ASCIIHexDecode(wxMemoryOutputStream* osIn)
{
  wxMemoryInputStream in(*osIn);
  wxMemoryOutputStream* osOut = new wxMemoryOutputStream();
  size_t inLength = in.GetSize();
  size_t k;
  bool first = true;
  int n1 = 0;
  for (k = 0; k < inLength; ++k)
  {
    int ch = in.GetC() & 0xff;
    if (ch == '>')
      break;
    if (wxPdfTokenizer::IsWhitespace(ch))
      continue;
    int n = wxPdfTokenizer::GetHex(ch);
    if (n == -1)
    {
      wxLogError(wxString(wxT("wxPdfParser::ASCIIHexDecode: ")) + 
                 wxString(_("Illegal character.")));
      osOut->Close();
      delete osOut;
      return NULL;
    }
    if (first)
    {
      n1 = n;
    }
    else
    {
      osOut->PutC((char)(((n1 << 4) + n) & 0xff));
    }
    first = !first;
  }
  if (!first)
  {
    osOut->PutC((char)((n1 << 4) & 0xff));
  }
  osOut->Close();
  return osOut;
}

// --- ASCII 85 Decode method

//  Test case:
//  wxMemoryOutputStream ascii85Test("9jqo^BlbD-BleB1DJ+*+F(f,q/0JhKF<GL>Cj@.4Gp$d7F!,L7@<6@)/0JDEF<G%<+EV:2F!,O<DJ+*.@<*K0@<6L(Df-\\\\0Ec5e;DffZ(EZee.Bl.9pF\"AGXBPCsi+DGm>@3BB/F*&OCAfu2/AKYi(DIb:@FD,*)+C]U=@3BN#EcYf8ATD3s@q?d$AftVqCh[NqF<G:8+EV:.+Cf>-FD5W8ARlolDIal(DId<j@<?3r@:F%a+D58'ATD4$Bl@l3De:,-DJs`8ARoFb/0JMK@qB4^F!,R<AKZ&-DfTqBG%G>uD.RTpAKYo'+CT/5+Cei#DII?(E,9)oF*2M7/c         ", 340);
//  char ascii85_originalText[] = "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.";
//  wxMemoryOutputStream* out = ASCII85Decode(&ascii85Test);

wxMemoryOutputStream*
wxPdfParser::ASCII85Decode(wxMemoryOutputStream* osIn)
{
  wxMemoryInputStream in(*osIn);
  wxMemoryOutputStream* osOut = new wxMemoryOutputStream();
  int state = 0;
  int chn[5];
  size_t inLength = in.GetSize();
  size_t k;
  for (k = 0; k < inLength; k++)
  {
    int ch = in.GetC() & 0xff;
    if (ch == '~')
      break;
    if (wxPdfTokenizer::IsWhitespace(ch))
      continue;
    if (ch == 'z' && state == 0)
    {
      osOut->PutC(0);
      osOut->PutC(0);
      osOut->PutC(0);
      osOut->PutC(0);
      continue;
    }
    if (ch < '!' || ch > 'u')
    {
      wxLogError(wxString(wxT("wxPdfParser::ASCII85Decode: ")) + 
                 wxString(_("Illegal character.")));
      osOut->Close();
      delete osOut;
      return NULL;
    }
    chn[state] = ch - '!';
    ++state;
    if (state == 5)
    {
      state = 0;
      int r = 0;
      for (int j = 0; j < 5; ++j)
      {
        r = r * 85 + chn[j];
      }
      osOut->PutC((char)((r >> 24) & 0xff));
      osOut->PutC((char)((r >> 16) & 0xff));
      osOut->PutC((char)((r >>  8) & 0xff));
      osOut->PutC((char)( r        & 0xff));
    }
  }
  int r = 0;
  if (state == 1)
  {
    wxLogError(wxString(wxT("wxPdfParser::ASCII85Decode: ")) +
               wxString(_("Illegal length.")));
    osOut->Close();
    delete osOut;
    return NULL;
  }
  if (state == 2)
  {
    r = chn[0] * 85 * 85 * 85 * 85 + chn[1] * 85 * 85 * 85;
    osOut->PutC((char)((r >> 24) & 0xff));
  }
  else if (state == 3)
  {
    r = chn[0] * 85 * 85 * 85 * 85 + chn[1] * 85 * 85 * 85  + chn[2] * 85 * 85;
    osOut->PutC((char)((r >> 24) & 0xff));
    osOut->PutC((char)((r >> 16) & 0xff));
  }
  else if (state == 4)
  {
    r = chn[0] * 85 * 85 * 85 * 85 + chn[1] * 85 * 85 * 85  + chn[2] * 85 * 85  + chn[3] * 85 ;
    osOut->PutC((char)((r >> 24) & 0xff));
    osOut->PutC((char)((r >> 16) & 0xff));
    osOut->PutC((char)((r >>  8) & 0xff));
  }

  osOut->Close();
  return osOut;
}

// --- Predictor Decode method

wxMemoryOutputStream*
wxPdfParser::DecodePredictor(wxMemoryOutputStream* osIn, wxPdfObject* dicPar)
{
  if (dicPar == NULL || dicPar->GetType() != OBJTYPE_DICTIONARY)
  {
    return osIn;
  }

  wxPdfDictionary* dic = (wxPdfDictionary*) dicPar;
  wxPdfObject* obj = ResolveObject(dic->Get(wxT("Predictor")));
  if (obj == NULL || obj->GetType() != OBJTYPE_NUMBER)
  {
    return osIn;
  }
  int predictor = ((wxPdfNumber*)obj)->GetInt();
  if (predictor < 10)
  {
    return osIn;
  }

  int width = 1;
  obj = ResolveObject(dic->Get(wxT("Columns")));
  if (obj != NULL && obj->GetType() == OBJTYPE_NUMBER)
  {
    width = ((wxPdfNumber*) obj)->GetInt();
  }
  int colours = 1;
  obj = ResolveObject(dic->Get(wxT("Colors")));
  if (obj != NULL && obj->GetType() == OBJTYPE_NUMBER)
  {
    colours = ((wxPdfNumber*) obj)->GetInt();
  }
  int bpc = 8;
  obj = ResolveObject(dic->Get(wxT("BitsPerComponent")));
  if (obj != NULL && obj->GetType() == OBJTYPE_NUMBER)
  {
    bpc = ((wxPdfNumber*) obj)->GetInt();
  }

  wxMemoryInputStream dataStream(*osIn);
  wxMemoryOutputStream* osOut = new wxMemoryOutputStream();;

  int bytesPerPixel = colours * bpc / 8;
  int bytesPerRow = (colours * width * bpc + 7) / 8;
  char* curr = new char[bytesPerRow];
  char* prior = new char[bytesPerRow];

  int i;
  for (i = 0; i < bytesPerRow; i++)
  {
    prior[i] = 0;
  }
  // Decode the (sub)image row-by-row
  while (true)
  {
    // Read the filter type byte and a row of data
    int filter = 0;
    filter = dataStream.GetC();
    if (dataStream.LastRead() == 0)
    {
      break;
    }
    dataStream.Read(curr, bytesPerRow);
    if (dataStream.LastRead() != (size_t) bytesPerRow)
    {
      break;
    }

    switch (filter)
    {
      case 0: //PNG_FILTER_NONE
        break;
      case 1: //PNG_FILTER_SUB
        for (i = bytesPerPixel; i < bytesPerRow; i++)
        {
          curr[i] += curr[i - bytesPerPixel];
        }
        break;
      case 2: //PNG_FILTER_UP
        for (i = 0; i < bytesPerRow; i++)
        {
          curr[i] += prior[i];
        }
        break;
      case 3: //PNG_FILTER_AVERAGE
        for (i = 0; i < bytesPerPixel; i++)
        {
          curr[i] += prior[i] / 2;
        }
        for (i = bytesPerPixel; i < bytesPerRow; i++)
        {
          curr[i] += ((curr[i - bytesPerPixel] & 0xff) + (prior[i] & 0xff))/2;
        }
        break;
      case 4: //PNG_FILTER_PAETH
        for (i = 0; i < bytesPerPixel; i++)
        {
          curr[i] += prior[i];
        }

        for (i = bytesPerPixel; i < bytesPerRow; i++)
        {
          int a = curr[i - bytesPerPixel] & 0xff;
          int b = prior[i] & 0xff;
          int c = prior[i - bytesPerPixel] & 0xff;

          int p = a + b - c;
          int pa = (p > a) ? p - a : a - p;
          int pb = (p > b) ? p - b : b - p;
          int pc = (p > c) ? p - c : c - p;

          int ret;

          if ((pa <= pb) && (pa <= pc))
          {
            ret = a;
          }
          else if (pb <= pc)
          {
            ret = b;
          }
          else
          {
            ret = c;
          }
          curr[i] += (char)(ret);
        }
        break;
      default:
        wxLogError(wxString(wxT("wxPdfParser::DecodePredictor: ")) +
                   wxString(_("PNG filter unknown.")));
        // TODO: Should set error flag and abort method
        break;
    }
    osOut->Write(curr, bytesPerRow);

    // Swap curr and prior
    char* tmp = prior;
    prior = curr;
    curr = tmp;
  }
  delete [] curr;
  delete [] prior;

  return osOut;
}

// --- LZW Decode method

wxMemoryOutputStream*
wxPdfParser::LZWDecode(wxMemoryOutputStream* osIn)
{
  wxMemoryInputStream in(*osIn);
  wxMemoryOutputStream* osOut = new wxMemoryOutputStream();
  wxPdfLzwDecoder lzw;
  if (!lzw.Decode(&in, osOut))
  {
    delete osOut;
    osOut = osIn;
  }
  return osOut;
}

int wxPdfLzwDecoder::ms_andTable[4] = { 511, 1023, 2047, 4095 };

wxPdfLzwDecoder::wxPdfLzwDecoder()
{
  m_bitsToGet = 9;
  m_nextData  = 0;
  m_nextBits  = 0;
}

wxPdfLzwDecoder::~wxPdfLzwDecoder()
{
  size_t j;
  for (j = 0; j < WXPDF_LZW_STRINGTABLE_SIZE; j++)
  {
    m_stringTable[j].Clear();
  }
}

int
wxPdfLzwDecoder::GetNextCode()
{
  // Returns the next 9, 10, 11 or 12 bits
  int ch;
  int code = 257;

  if ((size_t) m_bytePointer < m_dataSize)
  {
    ch = m_dataIn->GetC() & 0xff;
    m_bytePointer++;
    m_nextData = (m_nextData << 8) | ch;
    m_nextBits += 8;
            
    if (m_nextBits < m_bitsToGet)
    {
      ch = m_dataIn->GetC() & 0xff;
      m_bytePointer++;
      m_nextData = (m_nextData << 8) | ch;
      m_nextBits += 8;
    }
            
    code = (m_nextData >> (m_nextBits - m_bitsToGet)) & ms_andTable[m_bitsToGet-9];
    m_nextBits -= m_bitsToGet;
  }

  return code;
}
    
bool
wxPdfLzwDecoder::Decode(wxMemoryInputStream* dataIn, wxMemoryOutputStream* dataOut)
{
  m_dataOut  = dataOut;
  m_dataIn   = dataIn;
  m_dataSize = m_dataIn->GetSize();
  int ch1 = m_dataIn->GetC() & 0xff;
  int ch2 = m_dataIn->GetC() & 0xff;
  m_dataIn->SeekI(0);
  if (ch1 == 0 && ch2 == 1)
  {
    wxLogError(wxString(wxT("wxPdfLzwDecoder::Decode: ")) +
               wxString(_("LZW flavour not supported.")));
    return false;
  }

  InitializeStringTable();
        
  // Initialize pointers
  m_bytePointer = 0;
  m_bitPointer = 0;
        
  m_nextData = 0;
  m_nextBits = 0;
        
  int code;
  int oldCode = 0;
        
  while ((code = GetNextCode()) != 257)
  {
    if (code == 256)
    {
      InitializeStringTable();
      code = GetNextCode();
                
      if (code == 257)
      {
        break;
      }
                
      WriteString(code);
      oldCode = code;
    }
    else
    {
      if (code < m_tableIndex)
      {
        WriteString(code);
        AddStringToTable(oldCode, m_stringTable[code][0]);
        oldCode = code;
      }
      else
      {
        int index = m_tableIndex;
        AddStringToTable(oldCode, m_stringTable[oldCode][0]);
        WriteString(index);
        oldCode = code;
      }
    }
  }
  return true;
}
    
void
wxPdfLzwDecoder::InitializeStringTable()
{
  unsigned int j;
  for (j = 0; j < WXPDF_LZW_STRINGTABLE_SIZE; j++)
  {
    m_stringTable[j].Empty();
  }

  for (j = 0; j < 256; j++)
  {
    m_stringTable[j].Add(j);
  }
        
  m_tableIndex = 258;
  m_bitsToGet = 9;
}

void
wxPdfLzwDecoder::WriteString(int code)
{
  size_t length = m_stringTable[code].GetCount();
  size_t j;
  for (j = 0; j < length; j++)
  {
    m_dataOut->PutC((char) m_stringTable[code][j]);
  }
}

void
wxPdfLzwDecoder::AddStringToTable(int oldCode, char newString)
{
  size_t j;
  size_t length = m_stringTable[oldCode].GetCount();
  m_stringTable[m_tableIndex].Empty();
  for (j = 0; j < length; j++)
  {
    m_stringTable[m_tableIndex].Add(m_stringTable[oldCode][j]);
  }
  m_stringTable[m_tableIndex].Add(newString);
  m_tableIndex++;
        
  if (m_tableIndex == 511)
  {
    m_bitsToGet = 10;
  }
  else if (m_tableIndex == 1023)
  {
    m_bitsToGet = 11;
  }
  else if (m_tableIndex == 2047)
  {
    m_bitsToGet = 12;
  }
}

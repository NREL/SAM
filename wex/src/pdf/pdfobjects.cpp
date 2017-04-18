///////////////////////////////////////////////////////////////////////////////
// Name:        pdfobjects.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2006-07-13
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfobjects.cpp Implementation of PDF objects

// For compilers that support precompilation, includes <wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

// includes

#include "wex/pdf/pdfobjects.h"
#include "wex/pdf/pdfutility.h"

#include "wxmemdbg.h"

// --- Object queue for processing the resource tree

wxPdfObjectQueue::wxPdfObjectQueue(int originalObjectId, int actualObjectId, wxPdfObject* object)
{
  m_originalObjectId = originalObjectId;
  m_actualObjectId = actualObjectId;
  m_object = object;
  m_next = NULL;
}

// --- Base object

wxPdfObject::wxPdfObject(int type)
{
  m_type = type;
  m_objNum = -1;
  m_objGen = -1;
  m_actualId = -1;
  m_indirect = false;
}

wxPdfObject::~wxPdfObject()
{
}

bool
wxPdfObject::CanBeInObjStm()
{
  return (m_type >= 1 && m_type <= 7);
}

void
wxPdfObject::SetObjNum(int objNum, int objGen)
{
  m_objNum = objNum;
  m_objGen = objGen;
}

// --- Null object

wxPdfNull::wxPdfNull()
  : wxPdfObject(OBJTYPE_NULL)
{
}

wxPdfNull::~wxPdfNull()
{
}

// --- Indirect Reference

wxPdfIndirectReference::wxPdfIndirectReference(int number, int generation)
  : wxPdfObject(OBJTYPE_INDIRECT)
{
  SetObjNum(number, generation);
}

wxPdfIndirectReference::~wxPdfIndirectReference()
{
}

// --- Literal

wxPdfLiteral::wxPdfLiteral(int type, const wxString& value)
  : wxPdfObject(type)
{
  m_value = value;
}

wxPdfLiteral::~wxPdfLiteral()
{
}

// --- Boolean

wxPdfBoolean::wxPdfBoolean(bool value)
: wxPdfObject(OBJTYPE_BOOLEAN)
{
  m_value = value;
}

wxPdfBoolean::~wxPdfBoolean()
{
}

wxString
wxPdfBoolean::GetAsString()
{
  return (m_value) ? wxT("true") : wxT("false");
}

// --- String / Hex string

wxPdfString::wxPdfString(const wxString& value)
: wxPdfObject(OBJTYPE_STRING)
{
  m_value = value;
}

wxPdfString::~wxPdfString()
{
}

// --- Number

wxPdfNumber::wxPdfNumber(const wxString& value)
  : wxPdfObject(OBJTYPE_NUMBER)
{
  m_value = wxPdfUtility::String2Double(value);
  m_string = value;
  m_isInt = false;
}

wxPdfNumber::wxPdfNumber(int value)
  : wxPdfObject(OBJTYPE_NUMBER)
{
  m_value = value;
  m_string = wxString::Format(wxT("%d"), value);
  m_isInt = true;
}

wxPdfNumber::wxPdfNumber(double value)
  : wxPdfObject(OBJTYPE_NUMBER)
{
  m_value = value;
  m_string = wxPdfUtility::Double2String(value, 5);
  m_isInt = false;
}

wxPdfNumber::~wxPdfNumber()
{
}

// --- Name

wxPdfName::wxPdfName()
  : wxPdfObject(OBJTYPE_NAME)
{
}

wxPdfName::wxPdfName(const wxString& name)
  : wxPdfObject(OBJTYPE_NAME)
{
  m_name = name;
}

wxPdfName::~wxPdfName()
{
}

// --- Array

wxPdfArray::wxPdfArray()
  : wxPdfObject(OBJTYPE_ARRAY)
{
}

wxPdfArray::~wxPdfArray()
{
  wxPdfObject* obj;
  size_t j;
  for (j = 0; j < m_array.GetCount(); j++)
  {
    obj = (wxPdfObject*) m_array.Item(j);
    if (obj != NULL)
    {
      delete obj;
    }
  }
  m_array.Clear();
}

void
wxPdfArray::Add(wxPdfObject* obj)
{
  m_array.Add(obj);
}

void
wxPdfArray::Add(int value)
{
  wxPdfNumber* obj = new wxPdfNumber(value);
  m_array.Add(obj);
}

void
wxPdfArray::Add(double value)
{
  wxPdfNumber* obj = new wxPdfNumber(value);
  m_array.Add(obj);
}

wxPdfObject*
wxPdfArray::Get(size_t index)
{
  wxPdfObject* obj = NULL;
  if (index < m_array.GetCount())
  {
    obj = (wxPdfObject*) m_array.Item(index);
  }
  return obj;
}

// --- Dictionary

wxPdfDictionary::wxPdfDictionary()
  : wxPdfObject(OBJTYPE_DICTIONARY)
{
  m_hashMap = new wxPdfDictionaryMap();
}

wxPdfDictionary::wxPdfDictionary(const wxString& type)
  : wxPdfObject(OBJTYPE_DICTIONARY)
{
  m_hashMap = new wxPdfDictionaryMap();
  Put(wxT("Type"), new wxPdfName(type));
}

wxPdfDictionary::~wxPdfDictionary()
{
  wxPdfDictionaryMap::iterator entry = m_hashMap->begin();
  for (entry = m_hashMap->begin(); entry != m_hashMap->end(); entry++)
  {
    wxPdfObject* obj = entry->second;
    delete obj;
  }
  delete m_hashMap;
}

void
wxPdfDictionary::Put(wxPdfName* key, wxPdfObject* value)
{
  (*m_hashMap)[key->GetName()] = value;
}

void
wxPdfDictionary::Put(const wxString& key, wxPdfObject* value)
{
  (*m_hashMap)[key] = value;
}

wxPdfObject*
wxPdfDictionary::Get(const wxString& key)
{
  wxPdfObject* value = NULL;
  wxPdfDictionaryMap::iterator entry = m_hashMap->find(key);
  if (entry != m_hashMap->end())
  {
    value = entry->second;
  }
  return value;
}

// --- Stream

wxPdfStream::wxPdfStream()
  : wxPdfObject(OBJTYPE_STREAM)
{
  m_offset     = 0;
  m_dictionary = NULL;
  m_buffer     = NULL;
  m_hasObjOffsets = false;
}

wxPdfStream::wxPdfStream(off_t offset)
  : wxPdfObject(OBJTYPE_STREAM)
{
  m_offset     = offset;
  m_dictionary = NULL;
  m_buffer     = NULL;
  m_hasObjOffsets = false;
}

wxPdfStream::~wxPdfStream()
{
  if (m_dictionary != NULL)
  {
    delete m_dictionary;
  }
  if (m_buffer != NULL)
  {
    delete m_buffer;
  }
  m_objOffsets.Clear();
}

wxPdfObject*
wxPdfStream::Get(const wxString& key)
{
  wxPdfObject* obj = (m_dictionary != NULL) ? m_dictionary->Get(key) : NULL;
  return obj;
}

  
int
wxPdfStream::GetObjOffset(int index) const
{
  int objOffset = -1;
  if (index >= 0 && (size_t) index < m_objOffsets.GetCount())
  {
    objOffset = m_objOffsets[index];
  }
  return objOffset;
}

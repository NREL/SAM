///////////////////////////////////////////////////////////////////////////////
// Name:        pdftemplate.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2006-07-13
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdftemplate.cpp Implementation of the PDF templates

// For compilers that support precompilation, includes <wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

// includes

#include "wex/pdf/pdfdocument.h"
#include "wex/pdf/pdfobjects.h"
#include "wex/pdf/pdfparser.h"
#include "wex/pdf/pdftemplate.h"
#include "wex/pdf/pdfutility.h"

wxPdfTemplate::wxPdfTemplate(int templateId)
{
  m_templateId = templateId;
  m_fonts      = new wxPdfFontHashMap();
  m_images     = new wxPdfImageHashMap();
  m_templates  = new wxPdfTemplatesMap();

  m_parser    = NULL;
  m_resources = NULL;
  m_used = false;
}

wxPdfTemplate::~wxPdfTemplate()
{
  delete m_fonts;
  delete m_images;
  delete m_templates;

  if (m_resources != NULL)
  {
    if (m_resources->IsCreatedIndirect())
    {
      delete m_resources;
    }
  }
}

int
wxPdfDocument::BeginTemplate(double x, double y, double w, double h)
{
  // Save settings
  m_templateId++;
  m_currentTemplate = new wxPdfTemplate(m_templateId);

  m_currentTemplate->m_stateSave = m_state;
  m_currentTemplate->m_xSave = m_x;
  m_currentTemplate->m_ySave = m_y;
  m_currentTemplate->m_hSave = m_h;
  m_currentTemplate->m_wSave = m_w;
  m_currentTemplate->m_autoPageBreakSave = m_autoPageBreak;
  m_currentTemplate->m_bMarginSave = m_bMargin;
  m_currentTemplate->m_tMarginSave = m_tMargin;
  m_currentTemplate->m_lMarginSave = m_lMargin;
  m_currentTemplate->m_rMarginSave = m_rMargin;

  if (m_page <= 0)
  {
    m_state = 2;
  }
  SetAutoPageBreak(false);
        
  if (x <= 0) x = 0;
  if (y <= 0) y = 0;
  if (w <= 0) w = m_w;
  if (h <= 0) h = m_h;

  // Define own height and width to calculate positions correctly
  m_h = h;
  m_w = w;

  m_currentTemplate->m_x = x;
  m_currentTemplate->m_y = y;
  m_currentTemplate->m_w = w;
  m_currentTemplate->m_h = h;

  m_inTemplate = true;
  if (m_yAxisOriginTop)
  {
    StartTransform();
    Transform(1.0, 0.0, 0.0, -1.0, 0.0, m_h*m_k);
  }
  SetXY(x + m_lMargin, y + m_tMargin);
  SetRightMargin(m_w - w + m_rMargin);

  (*m_templates)[m_templateId] = m_currentTemplate;

  return m_templateId;
}

int
wxPdfDocument::EndTemplate()
{
  int templateId = 0;
  if (m_inTemplate)
  {
    if (m_yAxisOriginTop)
    {
      StopTransform();
    }
    m_inTemplate = false;
    m_state = m_currentTemplate->m_stateSave;
    if (m_currentTemplate->m_stateSave == 2)
    {
      SetXY(m_currentTemplate->m_xSave, m_currentTemplate->m_ySave);
    }
    SetAutoPageBreak(m_currentTemplate->m_autoPageBreakSave, m_currentTemplate->m_bMarginSave);
    m_tMargin = m_currentTemplate->m_tMarginSave;
    m_lMargin = m_currentTemplate->m_lMarginSave;
    m_rMargin = m_currentTemplate->m_rMarginSave;
    m_h = m_currentTemplate->m_hSave;
    m_w = m_currentTemplate->m_wSave;
    templateId = m_templateId;
  }
  return templateId;
}

void
wxPdfDocument::UseTemplate(int templateId, double x, double y, double w, double h)
{
  if (m_page <= 0)
  {
    wxLogError(wxString(wxT("wxPdfDocument::UseTemplate: ")) +
               wxString(_("You have to add a page first!")));
    return;
  }

  wxPdfTemplate* tpl;
  wxPdfTemplatesMap::iterator templateIter = (*m_templates).find(templateId);
  if (templateIter != (*m_templates).end())
  {
    tpl = templateIter->second;
  }
  else
  {
    wxLogWarning(wxString(wxT("wxPdfDocument::UseTemplate: ")) +
      wxString::Format(_("Template %d does not exist!"), templateId));
    return;
  }
            
  if (m_inTemplate)
  {
    (*(m_currentTemplate->m_templates))[templateId] = tpl;
  }

  if (x < 0) x = tpl->GetX();
  if (y < 0) y = tpl->GetY();
  GetTemplateSize(templateId, w, h);

  double xScale = w / tpl->GetWidth();
  double yScale = h / tpl->GetHeight();
  if (m_yAxisOriginTop)
  {
    yScale *= -1;
  }
  double xTrans = (x - xScale * tpl->GetX()) * m_k;
  double yTrans = (y + h - yScale * tpl->GetY()) * m_k;
  OutAscii(wxString(wxT("q ")) +
           wxPdfUtility::Double2String(xScale, 4) + wxString(wxT(" 0 0 ")) +
           wxPdfUtility::Double2String(yScale, 4) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(xTrans, 2) + wxString(wxT(" ")) +
           wxPdfUtility::Double2String(yTrans, 2) +  wxString(wxT(" cm"))); // Translate
  OutAscii(m_templatePrefix + wxString::Format(wxT("%d Do Q"), tpl->GetIndex()));
  tpl->m_used = true;
}
    
void
wxPdfDocument::GetTemplateSize(int templateId, double& w, double& h)
{
  wxPdfTemplate* tpl;
  wxPdfTemplatesMap::iterator templateIter = (*m_templates).find(templateId);
  if (templateIter != (*m_templates).end())
  {
    tpl = templateIter->second;
    if (w <= 0 && h <= 0)
    {
      w = tpl->GetWidth();
      h = tpl->GetHeight();
    }
    if (w <= 0)
    {
      w = h * tpl->GetWidth() / tpl->GetHeight();
    }
    if (h <= 0)
    {
      h = w * tpl->GetHeight() / tpl->GetWidth();
    }
  }
  else
  {
    wxLogWarning(wxString(wxT("wxPdfDocument::UseTemplate: ")) +
      wxString::Format(_("Template %d does not exist!"), templateId));
    w = 0;
    h = 0;
  }
}

void
wxPdfDocument::GetTemplateBBox(int templateId, double& x, double& y, double& w, double& h)
{
  wxPdfTemplate* tpl;
  wxPdfTemplatesMap::iterator templateIter = (*m_templates).find(templateId);
  if (templateIter != (*m_templates).end())
  {
    tpl = templateIter->second;
    x = tpl->GetX();
    y = tpl->GetY();
    w = tpl->GetWidth();
    h = tpl->GetHeight();
  }
  else
  {
    x = 0;
    y = 0;
    w = 0;
    h = 0;
    wxLogWarning(wxString(wxT("wxPdfDocument::GetTemplateBBox: ")) +
                 wxString::Format(_("Template %d does not exist!"), templateId));
  }
}

void
wxPdfDocument::SetTemplateBBox(int templateId, double x, double y, double w, double h)
{
  wxPdfTemplate* tpl;
  wxPdfTemplatesMap::iterator templateIter = (*m_templates).find(templateId);
  if (templateIter != (*m_templates).end())
  {
    tpl = templateIter->second;
    if (!tpl->m_used)
    {
      if (!tpl->m_used && w > 0 && h > 0)
      {
        tpl->m_x = x;
        tpl->m_y = y;
        tpl->m_w = w;
        tpl->m_h = h;
      }
      else
      {
        wxLogWarning(wxString(wxT("wxPdfDocument::SetTemplateBBox: ")) +
                     wxString::Format(_("Invalid width and/or height, BBox not changed for template %d!"), templateId));
      }
    }
    else
    {
      wxLogWarning(wxString(wxT("wxPdfDocument::SetTemplateBBox: ")) +
                   wxString::Format(_("Template %d has already been used, BBox can't be changed!"), templateId));
    }
  }
  else
  {
    wxLogWarning(wxString(wxT("wxPdfDocument::SetTemplateBBox: ")) +
                 wxString::Format(_("Template %d does not exist!"), templateId));
  }
}

int
wxPdfDocument::SetSourceFile(const wxString& filename, const wxString& password)
{
  int pageCount = 0;
  if (filename != wxEmptyString)
  {
    m_currentSource = filename;
    m_currentParser = new wxPdfParser(filename, password);
    if (m_currentParser->IsOk())
    {
      (*m_parsers)[filename] = m_currentParser;
      pageCount = m_currentParser->GetPageCount();
    }
    else
    {
      wxLogError(wxString(wxT("wxPdfDocument::SetSourceFile: ")) +
                 wxString(_("Parser creation failed.")));
      m_currentSource = wxEmptyString;
      delete m_currentParser;
      m_currentParser = NULL;
    }
  }
  else
  {
    wxLogError(wxString(wxT("wxPdfDocument::SetSourceFile: ")) +
               wxString(_("No source file name given.")));
  }
  return pageCount;
}

bool
wxPdfDocument::GetSourceInfo(wxPdfInfo& info)
{
  bool ok = false;
  if (m_currentParser != NULL && m_currentParser->IsOk())
  {
    ok = m_currentParser->GetSourceInfo(info);
  }
  return ok;
}
  
int
wxPdfDocument::ImportPage(unsigned int pageno, wxPdfPageBox pageBox)
{
  int templateId = 0;
  if (m_currentParser != NULL)
  {
    if (pageno > 0 && pageno <= m_currentParser->GetPageCount())
    {
      wxPdfObject* resources = m_currentParser->GetPageResources(pageno-1);
      wxPdfArrayDouble* artBox = NULL;
      switch (pageBox)
      {
        case wxPDF_PAGEBOX_MEDIABOX:
          artBox = m_currentParser->GetPageMediaBox(pageno-1);
          break;
        case wxPDF_PAGEBOX_CROPBOX:
          artBox = m_currentParser->GetPageCropBox(pageno-1);
          break;
        case wxPDF_PAGEBOX_BLEEDBOX:
          artBox = m_currentParser->GetPageBleedBox(pageno-1);
          break;
        case wxPDF_PAGEBOX_TRIMBOX:
          artBox = m_currentParser->GetPageTrimBox(pageno-1);
          break;
        case wxPDF_PAGEBOX_ARTBOX:
        default:
          artBox = m_currentParser->GetPageArtBox(pageno-1);
          break;
      }
      m_templateId++;
      wxPdfTemplate* pageTemplate = new wxPdfTemplate(m_templateId);
      pageTemplate->SetParser(m_currentParser);
      pageTemplate->SetResources(resources);
      if (artBox != NULL)
      {
        double x1 = (*artBox)[0];
        double x2 = (*artBox)[2];
        double y1 = (*artBox)[1];
        double y2 = (*artBox)[3];
        double swap;
        if (x1 > x2)
        {
          swap = x1;
          x1 = x2;
          x2 = swap;
        }
        if (y1 > y2)
        {
          swap = y1;
          y1 = y2;
          y2 = swap;
        }
        pageTemplate->m_x = x1 / m_k;
        pageTemplate->m_y = y1 / m_k;
        pageTemplate->m_w = (x2 - x1) / m_k;
        pageTemplate->m_h = (y2 - y1) / m_k;
        delete artBox;
      }
      else
      {
        pageTemplate->m_x = 0;
        pageTemplate->m_y = 0;
        pageTemplate->m_w = m_w;
        pageTemplate->m_h = m_h;
      }

      // fix for rotated pages
      int rotation = m_currentParser->GetPageRotation(pageno-1);
      int rotationAngle = rotation % 360;
      if (rotationAngle != 0)
      {
        int steps = rotationAngle / 90;
        double w = pageTemplate->m_w;
        double h = pageTemplate->m_h;
        pageTemplate->m_w =  (steps % 2 == 0) ? w : h;
        pageTemplate->m_h =  (steps % 2 == 0) ? h : w;
        double x = pageTemplate->m_w;
        double y = pageTemplate->m_h;

        if (steps % 2 != 0)
        {
          x = y = (steps == 1 || steps == -3) ? pageTemplate->m_h : pageTemplate->m_w;
        }
        double cx = (x / 2 + pageTemplate->m_x) * m_k;
        double cy = (y / 2 + pageTemplate->m_y) * m_k;
        double angle = -rotationAngle * (atan(1.) / 45.);
        double c = cos(angle);
        double s = sin(angle);
        wxString strRotation = wxString(wxT("q ")) + 
                               wxPdfUtility::Double2String(  c,3) + wxString(wxT(" ")) +
                               wxPdfUtility::Double2String(  s,3) + wxString(wxT(" ")) +
                               wxPdfUtility::Double2String( -s,3) + wxString(wxT(" ")) +
                               wxPdfUtility::Double2String(  c,3) + wxString(wxT(" ")) +
                               wxPdfUtility::Double2String( cx,3) + wxString(wxT(" ")) + 
                               wxPdfUtility::Double2String( cy,3) + wxString(wxT(" cm 1 0 0 1 ")) +
                               wxPdfUtility::Double2String(-cx,3) + wxString(wxT(" ")) +
                               wxPdfUtility::Double2String(-cy,3) + wxString(wxT(" cm "));
        pageTemplate->m_buffer.Write((const char*) strRotation.ToAscii(), strRotation.Length());
      }

      wxArrayPtrVoid contents;
      m_currentParser->GetContent(pageno-1, contents);
      size_t j;
      for (j = 0; j < contents.GetCount(); j++)
      {
        wxPdfStream* pageContent = (wxPdfStream*) contents[j];
        wxMemoryOutputStream* os = pageContent->GetBuffer();
        wxMemoryInputStream is(*os);
        pageTemplate->m_buffer.Write(is);
        delete pageContent;
      }

      if (rotationAngle != 0)
      {
        pageTemplate->m_buffer.Write(" Q", 2);
      }

      (*m_templates)[m_templateId] = pageTemplate;

      if (m_importVersion < m_currentParser->GetPdfVersion())
      {
        m_importVersion = m_currentParser->GetPdfVersion();
      }

      templateId = m_templateId; 
    }
  }
  return templateId;
}

void
wxPdfDocument::WriteObjectValue(wxPdfObject* obj, bool newline)
{
  switch(obj->GetType())
  {
    case OBJTYPE_NULL:
      {
        // The null object.
        Out("null", newline);
      }
      break;

    case OBJTYPE_BOOLEAN:
      {
        wxPdfBoolean* boolean = (wxPdfBoolean*) obj;
        OutAscii(boolean->GetAsString(), newline);
      }
      break;

    case OBJTYPE_NUMBER:
      {
        // A numeric value
        wxPdfNumber* number = (wxPdfNumber*) obj;
        OutAscii(number->GetAsString(), newline);
      }
      break;

    case OBJTYPE_STRING:
      {
        // A string.
        int nSave = m_n;
        int actualId = obj->GetActualId();
        if (actualId != -1)
        {
          m_n = actualId;
        }
        if (((wxPdfString*) obj)->IsHexString())
        {
          OutHexTextstring(((wxPdfString*) obj)->GetValue(), newline);
        }
        else
        {
          OutRawTextstring(((wxPdfString*) obj)->GetValue(), newline);
        }
        if (actualId != -1)
        {
          m_n = nSave;
        }
      }
      break;

    case OBJTYPE_NAME:
      {
        // A name.
        Out("/", false);
        OutAscii(((wxPdfName*) obj)->GetName(), newline);
      }
      break;

    case OBJTYPE_ARRAY:
      {
        // An array. Output the proper structure and move on.
        wxPdfArray* array = (wxPdfArray*) obj;
        Out("[", false);
        size_t j;
        for (j = 0; j < array->GetSize(); j++)
        {
          WriteObjectValue(array->Get(j), false);
          Out(" ");
        }
        Out("]");
      }
      break;

    case OBJTYPE_DICTIONARY:
      {
        // A dictionary.
        wxPdfDictionary* dictionary = (wxPdfDictionary*) obj;
        wxPdfDictionaryMap* dictionaryMap = dictionary->GetHashMap();
        wxPdfDictionaryMap::iterator entry = dictionaryMap->begin();
        Out("<<", false);
        for (entry = dictionaryMap->begin(); entry != dictionaryMap->end(); entry++)
        {
          Out("/", false);
          OutAscii(entry->first, false);
          Out(" ", false);
          WriteObjectValue(entry->second);
        }
        Out(">>");
      }
      break;

    case OBJTYPE_STREAM:
      {
        // A stream. First, output the stream dictionary,
        // then the stream data itself.
        wxPdfStream* stream = (wxPdfStream*) obj;
        wxMemoryOutputStream* buffer = stream->GetBuffer();

        // Save the 'Length' entry of the stream dictionary
        wxPdfDictionary* dictionary = (wxPdfDictionary*) stream->GetDictionary();
        wxPdfObject* originalLength = dictionary->Get(wxT("Length"));

        // Calculate the actual stream length and update the stream dictionary
        int streamLength = (int) CalculateStreamLength(buffer->TellO());
        wxPdfNumber actualLength(streamLength);
        wxPdfName lengthKey(wxT("Length"));
        dictionary->Put(&lengthKey, &actualLength);

        // Write the stream dictionary
        WriteObjectValue(stream->GetDictionary());

        // Write the stream
        int nSave = m_n;
        int actualId = obj->GetActualId();
        if (actualId != -1)
        {
          m_n = actualId;
        }
        PutStream(*buffer);
        if (actualId != -1)
        {
          m_n = nSave;
        }

        // Restore the 'Length' entry of the stream dictionary
        dictionary->Put(&lengthKey, originalLength);
      }
      break;

    case OBJTYPE_INDIRECT:
      {
        // An indirect object reference
        // Fill the object stack if needed
        wxPdfIndirectReference* ref = (wxPdfIndirectReference*) obj;
        int originalObjectId = ref->GetNumber();

        int actualObjectId;
        wxPdfObjectMap* objectMap = m_currentParser->GetObjectMap();
        wxPdfObjectMap::iterator mapEntry = objectMap->find(originalObjectId);
        if (mapEntry == objectMap->end())
        {
          actualObjectId = GetNewObjId();
          m_currentParser->AppendObject(originalObjectId, actualObjectId, obj);
        }
        else
        {
          actualObjectId = mapEntry->second->GetActualObjectId();
        }
        OutAscii(wxString::Format(wxT("%d 0 R"), actualObjectId), newline);
      }
      break;

    default:
      break;
  }
}

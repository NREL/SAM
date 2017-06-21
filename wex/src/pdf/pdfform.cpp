///////////////////////////////////////////////////////////////////////////////
// Name:        pdfform.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2006-01-18
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfform.cpp Implementation of the wxPdfDocument form fields

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
#include "wex/pdf/pdffontdetails.h"
#include "wex/pdf/pdfform.h"
#include "wex/pdf/pdfutility.h"

wxPdfIndirectObject::wxPdfIndirectObject(int objectId, int generationId)
{
  m_type = wxPDF_OBJECT_INDIRECT;
  m_objectId = objectId;
  m_generationId = generationId;
}

wxPdfIndirectObject::~wxPdfIndirectObject()
{
}

wxPdfAnnotationObject::wxPdfAnnotationObject(int objectId, int generationId)
  : wxPdfIndirectObject(objectId, generationId)
{
  SetType(wxPDF_OBJECT_ANNOTATION);
}

wxPdfAnnotationObject::~wxPdfAnnotationObject()
{
}

void
wxPdfAnnotationObject::SetRectangle(double x, double y, double width, double height)
{
  m_x = x;
  m_y = y;
  m_w = width;
  m_h = height;
}

wxPdfAnnotationWidget::wxPdfAnnotationWidget(int objectId, int generationId)
  : wxPdfAnnotationObject(objectId, generationId)
{
  SetType(wxPDF_OBJECT_WIDGET);
}

wxPdfAnnotationWidget::~wxPdfAnnotationWidget()
{
}

wxPdfCheckBox::wxPdfCheckBox(int objectId, int generationId)
  : wxPdfAnnotationWidget(objectId, generationId)
{
  SetType(wxPDF_OBJECT_WIDGET_CHECKBOX);
}

wxPdfCheckBox::~wxPdfCheckBox()
{
}

wxPdfComboBox::wxPdfComboBox(int objectId,
                             int fontindex, double fontsize, 
                             int generationId)
  : wxPdfAnnotationWidget(objectId, generationId)
{
  SetType(wxPDF_OBJECT_WIDGET_COMBOBOX);
  m_fontindex = fontindex;
  m_fontsize  = fontsize;
}

wxPdfComboBox::~wxPdfComboBox()
{
}

wxPdfPushButton::wxPdfPushButton(int objectId,
                                 int fontindex, double fontsize, 
                                 int generationId)
  : wxPdfAnnotationWidget(objectId, generationId)
{
  SetType(wxPDF_OBJECT_WIDGET_PUSHBUTTON);
  m_fontindex = fontindex;
  m_fontsize  = fontsize;
}

wxPdfPushButton::~wxPdfPushButton()
{
}

wxPdfRadioButton::wxPdfRadioButton(int objectId, int index, int generationId)
  : wxPdfAnnotationWidget(objectId, generationId)
{
  SetType(wxPDF_OBJECT_WIDGET_RADIOBUTTON);
  m_index = index;
}

wxPdfRadioButton::~wxPdfRadioButton()
{
}

wxPdfRadioGroup::wxPdfRadioGroup(int objectId, const wxString& groupName, int generationId)
  :  wxPdfIndirectObject(objectId, generationId)
{
  SetType(wxPDF_OBJECT_RADIOGROUP);
  m_groupName = groupName;
}

wxPdfRadioGroup::~wxPdfRadioGroup()
{
}

void
wxPdfRadioGroup::Add(wxPdfRadioButton* radio)
{
  m_radios.Add(radio);
  radio->SetParent(this);
}

wxPdfTextField::wxPdfTextField(int objectId,
                               int fontindex, double fontsize, 
                               const wxString& value, int generationId)
  : wxPdfAnnotationWidget(objectId, generationId)
{
  SetType(wxPDF_OBJECT_WIDGET_TEXTFIELD);
  m_fontindex = fontindex;
  m_fontsize  = fontsize;
  m_value = value;
}

wxPdfTextField::~wxPdfTextField()
{
}

// wxPdfDocument methods for forms

void
wxPdfDocument::OutIndirectObject(wxPdfIndirectObject* object)
{
  int objectId = object->GetObjectId();
  int generationId = object->GetGenerationId();
  if (m_offsets->count(objectId-1) == 0)
  {
    (*m_offsets)[objectId-1] = m_buffer->TellO();
    OutAscii(wxString::Format(wxT("%d %d obj"), objectId, generationId));
    switch (object->GetType())
    {
      case wxPDF_OBJECT_RADIOGROUP:
        {
          wxPdfRadioGroup* obj = static_cast<wxPdfRadioGroup*>(object);
/// TODO: support for radio button groups
// Currently radio button groups do not function
          Out("<<");
          Out("/FT /Btn");
          OutAscii(wxString::Format(wxT("/Ff %d"), (1 << 14) | (1 << 15)));
//          Out("/F 4");
          Out("/T ", false);
          OutAsciiTextstring(obj->GetName());
#if 0
          OutAscii(wxString(wxT("/BS << /W ") + 
                   Double2String(obj->GetBorderWidth() +
                   wxString(wxT("/S/")) + obj->GetBorderStyle() + 
                   wxString(wxT(">>")));
#endif
//          OutAscii(wxString(wxT("/MK <</BC [")) + obj->GetBorder() +
//                   wxString(wxT("]/BG [")) + obj->GetBackground() + wxString(wxT("] /CA ")), false);
//          OutAsciiTextstring(wxString(wxT("4")), false);
//          Out(">>");
          Out("/DR 2 0 R");
          Out("/DA ", false);
//          OutAsciiTextstring(wxString::Format(wxT("/F%d 9 Tf "), m_zapfdingbats) + obj->GetTextColour());
//          Out("/AP <</N <</Yes <<>>>> >>");
//          Out("/AS /Off");
          Out("/V /V1");
          Out("/Kids [", false);
          size_t j;
          wxPdfRadioButton* radio;
          wxArrayPtrVoid kids = obj->GetKids();
          for (j = 0; j < kids.GetCount(); j++)
          {
            radio = static_cast<wxPdfRadioButton*>(kids[j]);
            OutAscii(wxString::Format(wxT("%d %d R "), radio->GetObjectId(), radio->GetGenerationId()), false);
          }
          Out("]");
          Out(">>");
          Out("endobj");
          for (j = 0; j < kids.GetCount(); j++)
          {
            radio = static_cast<wxPdfRadioButton*>(kids[j]);
            OutIndirectObject(radio);
          }
        }
        break;

      case wxPDF_OBJECT_WIDGET_CHECKBOX:
        {
          wxPdfCheckBox* obj = static_cast<wxPdfCheckBox*>(object);
          Out("<<");
          Out("/Type /Annot");
          Out("/Subtype /Widget");
          double yPos = obj->GetY();
          if (m_yAxisOriginTop)
          {
            yPos = m_h - yPos;
          }
          OutAscii(wxString(wxT("/Rect [")) +
                   wxPdfUtility::Double2String(obj->GetX()*m_k,2) + wxString(wxT(" ")) +
                   wxPdfUtility::Double2String(yPos*m_k,2) + wxString(wxT(" ")) +
                   wxPdfUtility::Double2String((obj->GetX() + obj->GetWidth())*m_k,2) +  wxString(wxT(" ")) +
                   wxPdfUtility::Double2String((yPos - obj->GetHeight())*m_k,2) + wxString(wxT("]")));
          Out("/FT /Btn");
          Out("/Ff 0");
          Out("/F 4");
          Out("/T ", false);
          OutAsciiTextstring(obj->GetName());
          // Border style
          OutAscii(wxString(wxT("/BS << /W ")) + 
                   wxPdfUtility::Double2String(obj->GetBorderWidth(), 2) +
                   wxString(wxT("/S/")) + obj->GetBorderStyle() + 
                   wxString(wxT(">>")));
          OutAscii(wxString(wxT("/MK <</BC [")) + obj->GetBorderColour() +
                   wxString(wxT("]/BG [")) + obj->GetBackgroundColour() + wxString(wxT("] /CA ")), false);
          OutAsciiTextstring(wxString(wxT("4")), false);
          Out(">>");
          Out("/DR 2 0 R");
          Out("/DA ", false);
          OutAsciiTextstring(wxString::Format(wxT("/F%d 9 Tf "), m_zapfdingbats) + obj->GetTextColour());
          Out("/AP <</N <</Yes <<>>>> >>");
          Out("/AS /Off");
          if (obj->GetValue())
          {
            Out("/DV /Yes");
            Out("/V /Yes");
          }
          else
          {
            Out("/DV /Off");
            Out("/V /Off");
          }
          Out(">>");
          Out("endobj");
        }
        break;

      case wxPDF_OBJECT_WIDGET_COMBOBOX:
        {
          wxPdfComboBox* obj = static_cast<wxPdfComboBox*>(object);
          Out("<<");
          Out("/Type /Annot");
          Out("/Subtype /Widget");
          double yPos = obj->GetY();
          if (m_yAxisOriginTop)
          {
            yPos = m_h - yPos;
          }
          OutAscii(wxString(wxT("/Rect [")) +
                   wxPdfUtility::Double2String(obj->GetX()*m_k,2) + wxString(wxT(" ")) +
                   wxPdfUtility::Double2String(yPos*m_k,2) + wxString(wxT(" ")) +
                   wxPdfUtility::Double2String((obj->GetX() + obj->GetWidth())*m_k,2) +  wxString(wxT(" ")) +
                   wxPdfUtility::Double2String((yPos - obj->GetHeight())*m_k,2) + wxString(wxT("]")));
          Out("/FT /Ch");
          Out("/Ff 67764224");
          Out("/F 4");
          Out("/T ", false);
          OutAsciiTextstring(obj->GetName());
          OutAscii(wxString(wxT("/MK <</BC [")) + obj->GetBorderColour() +
                   wxString(wxT("]/BG [")) + obj->GetBackgroundColour() + wxString(wxT("]>>")));
          // Border style
          OutAscii(wxString(wxT("/BS << /W ")) + 
                   wxPdfUtility::Double2String(obj->GetBorderWidth(), 2) +
                   wxString(wxT("/S/")) + obj->GetBorderStyle() + 
                   wxString(wxT(">>")));
          Out("/DR 2 0 R");
          Out("/DA ", false);
          OutAsciiTextstring(wxString::Format(wxT("/F%d "), obj->GetFontIndex()) +
                             wxPdfUtility::Double2String(obj->GetFontSize(),2) +
                             wxString(wxT(" Tf ")) + obj->GetTextColour()); 
          wxArrayString options = obj->GetValue();
          Out("/DV ", false);
          OutTextstring(options[0]);
          Out("/V ", false);
          OutTextstring(options[0]);
// Option list alternative (transfer value, display value)
//   /Op t [ [(1)(Socks)][(2)(Shoes)] [(3)(Pants)][(4)(Shirt)][(5)(Tie)][(6)(Belt)][(7)(Shorts)]]
//
          Out("/Opt [", false);
          size_t j;
          for (j = 0; j < options.GetCount(); j++)
          {
            OutTextstring(options[j], false);
          }
          Out("]");
          Out(">>");
          Out("endobj");
        }
        break;

      case wxPDF_OBJECT_WIDGET_PUSHBUTTON:
        {
          wxPdfPushButton* obj = static_cast<wxPdfPushButton*>(object);
          Out("<<");
          Out("/Type /Annot");
          Out("/Subtype /Widget");
          double yPos = obj->GetY();
          if (m_yAxisOriginTop)
          {
            yPos = m_h - yPos;
          }
          OutAscii(wxString(wxT("/Rect [")) +
                   wxPdfUtility::Double2String(obj->GetX()*m_k,2) + wxString(wxT(" ")) +
                   wxPdfUtility::Double2String(yPos*m_k,2) + wxString(wxT(" ")) +
                   wxPdfUtility::Double2String((obj->GetX() + obj->GetWidth())*m_k,2) +  wxString(wxT(" ")) +
                   wxPdfUtility::Double2String((yPos - obj->GetHeight())*m_k,2) + wxString(wxT("]")));
          Out("/FT /Btn");
          Out("/Ff 65536"); // (1 << 16),
          Out("/F 4");
// Highlight button
//        Out("/H");
          Out("/T ", false);
          OutAsciiTextstring(obj->GetName());
          Out("/BS << /W 1 /S /B >>");
          OutAscii(wxString(wxT("/MK <</BC [")) + obj->GetBorderColour() +
                   wxString(wxT("]/BG [")) + obj->GetBackgroundColour() + 
                   wxString(wxT("] /CA ")), false);
          OutTextstring(obj->GetCaption(), false);
          Out(">>");
          Out("/DR 2 0 R");
          Out("/DA ", false);
          OutAsciiTextstring(wxString::Format(wxT("/F%d "), obj->GetFontIndex()) +
                             wxPdfUtility::Double2String(obj->GetFontSize(),2) + 
                             wxString(wxT(" Tf ")) + obj->GetTextColour()); 
          Out("/A <</S /JavaScript /JS", false);
          OutTextstring(obj->GetAction());
          Out(">>");
          Out(">>");
          Out("endobj");
        }
        break;

      case wxPDF_OBJECT_WIDGET_RADIOBUTTON:
        {
          wxPdfRadioButton* obj = static_cast<wxPdfRadioButton*>(object);
          Out("<<");
          Out("/Type /Annot");
          Out("/Subtype /Widget");
          double yPos = obj->GetY();
          if (m_yAxisOriginTop)
          {
            yPos = m_h - yPos;
          }
          OutAscii(wxString(wxT("/Rect [")) +
                   wxPdfUtility::Double2String(obj->GetX()*m_k,2) + wxString(wxT(" ")) +
                   wxPdfUtility::Double2String(yPos*m_k,2) + wxString(wxT(" ")) +
                   wxPdfUtility::Double2String((obj->GetX() + obj->GetWidth())*m_k,2) +  wxString(wxT(" ")) +
                   wxPdfUtility::Double2String((yPos - obj->GetHeight())*m_k,2) + wxString(wxT("]")));
          Out("/FT /Btn");
          Out("/Ff 49152");
          Out("/F 4");
          wxPdfRadioGroup* parent = obj->GetParent();
#if 0
// TODO: integrate radio button groups into PDF document
          OutAscii(wxString::Format(wxT("/Parent [%d %d R]"), parent->GetObjectId(), parent->GetGenerationId()));
//          Out("/P ??? 0 R /H /T ");
#endif
          Out("/T ", false);
          OutAsciiTextstring(parent->GetName());
          Out("/AS /Off");
          OutAscii(wxString(wxT("/MK <</BC [")) + obj->GetBorderColour() +
                   wxString(wxT("]/BG [")) + obj->GetBackgroundColour() + 
                   wxString(wxT("] /CA ")), false);
          OutAsciiTextstring(wxString(wxT("l")), false);
          Out(">>");
          // Border style
          OutAscii(wxString(wxT("/BS << /W ")) + 
                   wxPdfUtility::Double2String(obj->GetBorderWidth(), 2) +
                   wxString(wxT("/S/")) + obj->GetBorderStyle() + 
                   wxString(wxT(">>")));
          Out("/DR 2 0 R");
          Out("/DA ", false);
          OutAsciiTextstring(wxString::Format(wxT("/F%d 9 Tf "), m_zapfdingbats) + obj->GetTextColour());
          OutAscii(wxString::Format(wxT("/AP <</N <</V%d <<>> >> >>"), obj->GetIndex()));
          // Set first button in group as selected
          if (obj->GetIndex() == 1)
          {
            OutAscii(wxString::Format(wxT("/V /V%d"), obj->GetIndex()));
          }
          Out(">>");
          Out("endobj");
        }
        break;

      case wxPDF_OBJECT_WIDGET_TEXTFIELD:
        {
          wxPdfTextField* obj = static_cast<wxPdfTextField*>(object);
          Out("<<");
          Out("/Type /Annot");
          Out("/Subtype /Widget");
          double yPos = obj->GetY();
          if (m_yAxisOriginTop)
          {
            yPos = m_h - yPos;
          }
          OutAscii(wxString(wxT("/Rect [")) +
                   wxPdfUtility::Double2String(obj->GetX()*m_k,2) + wxString(wxT(" ")) +
                   wxPdfUtility::Double2String(yPos*m_k,2) + wxString(wxT(" ")) +
                   wxPdfUtility::Double2String((obj->GetX() + obj->GetWidth())*m_k,2) +  wxString(wxT(" ")) +
                   wxPdfUtility::Double2String((yPos - obj->GetHeight())*m_k,2) + wxString(wxT("]")));
          Out("/FT /Tx");
          if (obj->GetMultiLine())
          {
            Out("/Ff 4096");
          }
          Out("/F 4");
          Out("/T ", false);
          OutAsciiTextstring(obj->GetName());
          Out("/V ", false);
          OutTextstring(obj->GetValue()); // Current value
          Out("/DV ", false);
          OutTextstring(obj->GetValue()); // Default value
// Maximal length of text input
//          OutAscii(wxString::Format(wxT("/MaxLen %d"), obj->GetMaxLength());
          OutAscii(wxString(wxT("/MK <</BC [")) + obj->GetBorderColour() +
                   wxString(wxT("]/BG [")) + obj->GetBackgroundColour() + wxString(wxT("]>>")));
          // Border style
          OutAscii(wxString(wxT("/BS << /W ")) + 
                   wxPdfUtility::Double2String(obj->GetBorderWidth(), 2) +
                   wxString(wxT("/S/")) + obj->GetBorderStyle() + 
                   wxString(wxT(">>")));
          Out("/DR 2 0 R");
          Out("/DA ", false);
          OutAsciiTextstring(wxString::Format(wxT("/F%d "), obj->GetFontIndex()) +
                             wxPdfUtility::Double2String(obj->GetFontSize(),2) + 
                             wxString(wxT(" Tf ")) + obj->GetTextColour()); 
          Out(">>");
          Out("endobj");
        }
        break;

      case wxPDF_OBJECT_WIDGET:
      case wxPDF_OBJECT_ANNOTATION:
      case wxPDF_OBJECT_INDIRECT:
      default:
        Out("endobj");
        break;
    }
  }
}

void
wxPdfDocument::CheckBox(const wxString& name, double width, bool checked)
{
  CheckBox(name, m_x, m_y, width, checked);
}

void
wxPdfDocument::CheckBox(const wxString& name, double x, double y, double width, bool checked)
{
  wxPdfCheckBox* field = new wxPdfCheckBox(GetNewObjId());
  field->SetName(name);
  field->SetValue(checked);
  field->SetRectangle(x, y, width, width);
  AddFormField(field);

  // Font ZapfDingBats is required to display check boxes
  LoadZapfDingBats();
}

void
wxPdfDocument::ComboBox(const wxString& name, double width, double height, const wxArrayString& values)
{
  ComboBox(name, m_x, m_y, width, height, values);
}

void
wxPdfDocument::ComboBox(const wxString& name, 
                        double x, double y, double width, double height, 
                        const wxArrayString& values)
{
  wxPdfComboBox* field = new wxPdfComboBox(GetNewObjId(), m_currentFont->GetIndex(), m_fontSizePt);
  field->SetName(name);
  field->SetValue(values);
  field->SetRectangle(x, y, width, height);
  AddFormField(field);
}

void
wxPdfDocument::PushButton(const wxString& name, double width, double height, 
                          const wxString& caption, const wxString& action)
{
  PushButton(name, m_x, m_y, width, height, caption, action);
}

void
wxPdfDocument::PushButton(const wxString& name, 
                          double x, double y, double w, double h, 
                          const wxString& caption, const wxString& action)
{
  wxPdfPushButton* field = new wxPdfPushButton(GetNewObjId(), m_currentFont->GetIndex(), m_fontSizePt);
  field->SetName(name);
  field->SetCaption(caption);
  field->SetAction(action);
  field->SetRectangle(x, y, w, h);
  AddFormField(field);
}

void
wxPdfDocument::RadioButton(const wxString& group, const wxString& name, double width)
{
  RadioButton(group, name, m_x, m_y, width);
}

void
wxPdfDocument::RadioButton(const wxString& group, const wxString& name, 
                           double x, double y, double width)
{
  wxPdfRadioGroup* currentGroup;
  wxPdfRadioGroupMap::iterator radioGroup = (*m_radioGroups).find(group);
  if (radioGroup != (*m_radioGroups).end())
  {
    currentGroup = static_cast<wxPdfRadioGroup*>(radioGroup->second);
  }
  else
  {
//    currentGroup = new wxPdfRadioGroup(GetNewObjId(), group);
    currentGroup = new wxPdfRadioGroup(0, group);
    (*m_radioGroups)[group] = currentGroup;
#if 0
// TODO: integrate radio button groups into PDF document
    int n = (*m_formFields).size()+1;
    (*m_formFields)[n] = currentGroup;
#endif
  }

  wxPdfRadioButton* field = new wxPdfRadioButton(GetNewObjId(), currentGroup->GetCount()+1);
  field->SetName(name);
  field->SetRectangle(x, y, width, width);
// TODO: integrate radio button groups into PDF document
  AddFormField(field /*, false*/);
  currentGroup->Add(field);

  // Font ZapfDingBats is required to display radio buttons
  LoadZapfDingBats();
}

void
wxPdfDocument::TextField(const wxString& name, double width, double height,
                         const wxString& value, bool multiline)
{
  TextField(name, m_x, m_y, width, height, value, multiline);
}

void
wxPdfDocument::TextField(const wxString& name, 
                         double x, double y, double width, double height,
                         const wxString& value, bool multiline)
{
  wxPdfTextField* field = new wxPdfTextField(GetNewObjId(), m_currentFont->GetIndex(), m_fontSizePt, value);
  field->SetName(name);
  field->SetValue(value);
  field->SetMultiLine(multiline);
  field->SetRectangle(x, y, width, height);
  AddFormField(field);
}

void
wxPdfDocument::AddFormField(wxPdfAnnotationWidget* field, bool setFormField)
{
  field->SetBorderColour(m_formBorderColour);
  field->SetBackgroundColour(m_formBackgroundColour);
  field->SetTextColour(m_formTextColour);
  field->SetBorderStyle(m_formBorderStyle);
  field->SetBorderWidth(m_formBorderWidth);
  if (setFormField)
  {
    unsigned int n = (unsigned int) (*m_formFields).size()+1;
    (*m_formFields)[n] = field;
  }

  wxArrayPtrVoid* annotationArray = NULL;
  wxPdfFormAnnotsMap::iterator formAnnots = (*m_formAnnotations).find(m_page);
  if (formAnnots != (*m_formAnnotations).end())
  {
    annotationArray = formAnnots->second;
  }
  else
  {
    annotationArray = new wxArrayPtrVoid;
    (*m_formAnnotations)[m_page] = annotationArray;
  }
  annotationArray->Add(field);
}

void
wxPdfDocument::SetFormColours(const wxPdfColour& borderColour,
                              const wxPdfColour& backgroundColour, 
                              const wxPdfColour& textColour)
{
  m_formBorderColour     = borderColour.GetColour(false).BeforeLast(wxT(' '));
  m_formBackgroundColour = backgroundColour.GetColour(false).BeforeLast(wxT(' '));
  m_formTextColour       = textColour.GetColour(false);
}

void
wxPdfDocument::SetFormBorderStyle(wxPdfBorderStyle borderStyle, double borderWidth)
{
  switch (borderStyle)
  {
    case wxPDF_BORDER_DASHED:    m_formBorderStyle = wxString(wxT("D")); break;
    case wxPDF_BORDER_BEVELED:   m_formBorderStyle = wxString(wxT("B")); break;
    case wxPDF_BORDER_INSET:     m_formBorderStyle = wxString(wxT("I")); break;
    case wxPDF_BORDER_UNDERLINE: m_formBorderStyle = wxString(wxT("U")); break;
    case wxPDF_BORDER_SOLID:
    default:                     m_formBorderStyle = wxString(wxT("S")); break;
  }
  m_formBorderWidth = (borderWidth >= 0) ? borderWidth * m_k : 1;
}

void
wxPdfDocument::LoadZapfDingBats()
{
  if (m_zapfdingbats == 0)
  {
    // Save current font
    wxPdfFontDetails* saveFont = m_currentFont;
    wxString saveFamily = m_fontFamily;
    int saveStyle = m_fontStyle;
    double saveSize = m_fontSizePt;

    // Select font ZapfDingBats
    SelectFont(wxT("ZapfDingBats"), wxT(""), 9, false);
    m_zapfdingbats = m_currentFont->GetIndex();

    // Restore current font
    m_currentFont = saveFont;
    m_fontFamily = saveFamily;
    m_fontStyle = saveStyle;
    m_fontSizePt  = saveSize;
    m_fontSize    = saveSize / m_k;
  }
}

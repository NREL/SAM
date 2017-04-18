///////////////////////////////////////////////////////////////////////////////
// Name:        pdfform.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2006-01-18
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfform.h Interface of the wxPdfDocument form field handling

#ifndef _PDF_FORM_H_
#define _PDF_FORM_H_

// wxWidgets headers
#include <wx/arrstr.h>
#include <wx/string.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

/// Object types
enum wxPdfObjectType
{
  wxPDF_OBJECT_INDIRECT,
  wxPDF_OBJECT_RADIOGROUP,

  wxPDF_OBJECT_ANNOTATION,
  wxPDF_OBJECT_TEXTANNOTATION,

  wxPDF_OBJECT_WIDGET,
  wxPDF_OBJECT_WIDGET_CHECKBOX,
  wxPDF_OBJECT_WIDGET_COMBOBOX,
  wxPDF_OBJECT_WIDGET_PUSHBUTTON,
  wxPDF_OBJECT_WIDGET_RADIOBUTTON,
  wxPDF_OBJECT_WIDGET_TEXTFIELD
};

/// Base class for indirect PDF objects (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfIndirectObject
{
public:
  /// Constructor
  /**
  * \param objectId the object number
  * \param generationId the generation number
  */
  wxPdfIndirectObject(int objectId, int generationId);

  /// Destructor
  virtual ~wxPdfIndirectObject();

  /// Get type of this object
  /**
  * \return the type of the object
  */
  wxPdfObjectType GetType() { return m_type; }

  /// Set type of object
  /**
  * \param type the type of the object
  */
  void SetType(wxPdfObjectType type) { m_type = type; }

  /// Get identifier of this object
  /**
  * \return the object number
  */
  int GetObjectId() { return m_objectId; }

  /// Get generation identifier of this object
  /**
  * \return the generation number
  */
  int GetGenerationId() { return m_generationId; }

private:
  wxPdfObjectType m_type;          ///< type of object
  int             m_objectId;      ///< object identifier
  int             m_generationId;  ///< generation identifier
};

/// Base class for annotation objects (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfAnnotationObject : public wxPdfIndirectObject
{
public:
  /// Constructor
  /**
  * \param objectId the object number
  * \param generationId the generation number
  */
  wxPdfAnnotationObject(int objectId, int generationId);
  
  /// Destructor
  ~wxPdfAnnotationObject();

  /// Set the associated rectangular area
  /**
  * \param x offset in x direction 
  * \param y offset in y direction
  * \param width the width of the annotation 
  * \param height the height of the annotation
  */
  void SetRectangle(double x, double y, double width, double height);

  /// Get the X offset
  /**
  * \return the x offset
  */
  double GetX() { return m_x; }

  /// Get the Y offset
  /**
  * \return the y offset
  */
  double GetY() { return m_y; }

  /// Get the width
  /**
  * \return the width
  */
  double GetWidth() { return m_w; }

  /// Get the height
  /**
  * \return the height
  */
  double GetHeight() { return m_h; }

private:
  double m_x;   ///< X offset of associated area
  double m_y;   ///< Y offset of associated area
  double m_w;   ///< Width of associated area
  double m_h;   ///< Height of associated area
};

/// Base class for PDF form fields (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfAnnotationWidget : public wxPdfAnnotationObject
{
public:
  /// Constructor
  /**
  * \param objectId the object number
  * \param generationId the generation number
  */
  wxPdfAnnotationWidget(int objectId, int generationId = 0);
  
  /// Destructor
  virtual ~wxPdfAnnotationWidget();

  /// Set form field name
  void SetName(const wxString& name) { m_name = name; }
  
  /// Get form field name
  wxString GetName() { return m_name; }

  /// Set border colour representation of form field
  void SetBorderColour(const wxString& borderColour) { m_borderColour = borderColour; }
  
  /// Get border colour representation of form field
  wxString GetBorderColour() { return m_borderColour; }

  /// Set background colour representation of form field
  void SetBackgroundColour(const wxString& backgroundColour) { m_backgroundColour = backgroundColour; }
  
  /// Get background colour representation of form field
  wxString GetBackgroundColour() { return m_backgroundColour; }

  /// Set text colour representation of form field
  void SetTextColour(const wxString& textColour) { m_textColour = textColour; }
  
  /// Get text colour representation of form field
  wxString GetTextColour() { return m_textColour; }

  /// Set border style of form field
  void SetBorderStyle(const wxString& borderStyle) { m_borderStyle = borderStyle; }
  
  /// Get border style of form field
  wxString GetBorderStyle() { return m_borderStyle; }

  /// Set border width of form field
  void SetBorderWidth(double borderWidth) { m_borderWidth = borderWidth; }

  /// Get border width of form field
  double GetBorderWidth() { return m_borderWidth; }

private:
  wxString m_name;              ///< name of form field
  wxString m_borderColour;      ///< border colour representation
  wxString m_backgroundColour;  ///< background colour representation
  wxString m_textColour;        ///< text colour representation
  double   m_borderWidth;       ///< border width in points
  wxString m_borderStyle;       ///< border style
};

/// Class representing check box form fields (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfCheckBox : public wxPdfAnnotationWidget
{
public:
  /// Constructor
  wxPdfCheckBox(int objectId, int generationId = 0);

  /// Destructor
  virtual ~wxPdfCheckBox();

  /// Set status of form field (checked/unchecked)
  void SetValue(bool checked) { m_checked = checked; }
  
  /// Get status of form field (checked/unchecked)
  bool GetValue() { return m_checked; }

private:
  bool m_checked;  ///< status of check box
};

/// Class representing combo box form field (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfComboBox : public wxPdfAnnotationWidget
{
public:
  /// Constructor
  wxPdfComboBox(int objectId, 
                int fontindex, double fontsize, 
                int generationId = 0);

  /// Destructor
  virtual ~wxPdfComboBox();

  /// Set value list of combo box
  void SetValue(const wxArrayString& values) { m_values = values; }
  
  /// Get value list of combo box
  wxArrayString GetValue() { return m_values; }

  /// Get object index of font used for displaying the value list
  int GetFontIndex() { return m_fontindex; }
  
  /// Get size of font used for displaying the value list
  double GetFontSize() { return m_fontsize; }

private:
  int           m_fontindex;  ///< index of combo box font
  double        m_fontsize;   ///< size of combo box font
  wxArrayString m_values;     ///< array of combo box values
};

/// Class representing push button form field (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfPushButton : public wxPdfAnnotationWidget
{
public:
  /// Constructor
  wxPdfPushButton(int objectId,
                  int fontindex, double fontsize, 
                  int generationId = 0);

  /// Destructor
  virtual ~wxPdfPushButton();

  /// Set caption string of button
  void SetCaption(const wxString& caption) { m_caption = caption; }
  
  /// Get caption string of button
  wxString GetCaption() { return m_caption; }
  
  /// Set JavaScript action  associated with the button
  void SetAction(const wxString& action) { m_action = action; }
  
  /// Get JavaScript action  associated with the button
  wxString GetAction() { return m_action; }

  /// Get object index of font used for displaying the value list
  int GetFontIndex() { return m_fontindex; }
  
  /// Get size of font used for displaying the value list
  double GetFontSize() { return m_fontsize; }

private:
  int      m_fontindex;  ///< index of combo box font
  double   m_fontsize;   ///< size of combo box font
  wxString m_caption;    ///< caption string
  wxString m_action;     ///< JavaScript action
};

// Forward declaration of radio button group
class WXDLLIMPEXP_FWD_PDFDOC wxPdfRadioGroup;

/// Class representing radio button form field (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfRadioButton : public wxPdfAnnotationWidget
{
public:
  /// Constructor
  wxPdfRadioButton(int objectId, int index, int generationId = 0);
  
  /// Destructor
  virtual ~wxPdfRadioButton();

  /// Set parent (radio button group) of this button
  void SetParent(wxPdfRadioGroup* parent) { m_parent = parent; }
  
  /// Get parent (radio button group) of this button
  wxPdfRadioGroup* GetParent() { return m_parent; }
  
  /// Get index of this button within the radio button group
  int GetIndex() { return m_index; }

private:
  wxPdfRadioGroup* m_parent;  ///< pointer to parent group
  int              m_index;   ///< index within button group
};

/// Class representing radio button groups (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfRadioGroup : public wxPdfIndirectObject
{
public:
  /// Constructor
  wxPdfRadioGroup(int objectId, const wxString& groupName, int generationId = 0);
  
  /// Destructor
  virtual ~wxPdfRadioGroup();

  /// Add a button to the group
  void Add(wxPdfRadioButton* radio);

  /// Get name of the button group
  wxString GetName() { return m_groupName; }

  /// Get the number of buttons in the group
  unsigned int GetCount() { return (unsigned int) m_radios.GetCount(); }
  
  /// Get the array of buttons in the group
  wxArrayPtrVoid GetKids() { return m_radios; }

private:
  wxString       m_groupName;  ///< name of the radio button group
  wxArrayPtrVoid m_radios;     //< array of all radio buttons in this group
};

/// Class representing text form fields (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfTextField : public wxPdfAnnotationWidget
{
public:
  /// Constructor
  wxPdfTextField(int objectId,
                 int fontindex, double fontsize, 
                 const wxString& value, int generation_id = 0);

  /// Destructor
  virtual ~wxPdfTextField();

  /// Set default value of the text field
  void SetValue(const wxString& value) { m_value = value; }

  /// Get default value of the text field
  wxString GetValue() { return m_value; }

  /// Set multi line property of the text field
  void SetMultiLine(bool multiline) { m_multiline = multiline; }

  /// Get multi line property of the text field
  bool GetMultiLine() { return m_multiline; }

  /// Get object index of font used for displaying the value list
  int GetFontIndex() { return m_fontindex; }
  
  /// Get size of font used for displaying the value list
  double GetFontSize() { return m_fontsize; }

private:
  int      m_fontindex;  ///< index of text field font
  double   m_fontsize;   ///< size of text field font
  wxString m_value;      ///< default value of text field
  bool     m_multiline;  ///< multi line property of text field
};

#endif

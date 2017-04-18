///////////////////////////////////////////////////////////////////////////////
// Name:        pdflayer.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-07-01
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdflayer.h Definition of layer classes

#ifndef _PDF_LAYER_H_
#define _PDF_LAYER_H_

// wxWidgets headers
#include <wx/dynarray.h>
#include <wx/string.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

class WXDLLIMPEXP_FWD_PDFDOC wxPdfDictionary;

class WXDLLIMPEXP_FWD_PDFDOC wxPdfLayer;
WX_DEFINE_USER_EXPORTED_ARRAY_PTR(wxPdfLayer*, wxPdfArrayLayer, class WXDLLIMPEXP_PDFDOC);

/// Optional content group types
enum wxPdfOcgType
{
  wxPDF_OCG_TYPE_UNKNOWN,
  wxPDF_OCG_TYPE_LAYER,
  wxPDF_OCG_TYPE_TITLE,
  wxPDF_OCG_TYPE_MEMBERSHIP
};

/// OCG Intent options
const int wxPDF_OCG_INTENT_DEFAULT = 0;
const int wxPDF_OCG_INTENT_VIEW    = 1;
const int wxPDF_OCG_INTENT_DESIGN  = 2;

/// OCG visibility policy
enum wxPdfOcgPolicy
{
  wxPDF_OCG_POLICY_ALLON,
  wxPDF_OCG_POLICY_ANYON,
  wxPDF_OCG_POLICY_ANYOFF,
  wxPDF_OCG_POLICY_ALLOFF
};

/// Base class for optional content. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfOcg
{
public:
  /// Constructor
  wxPdfOcg();

  /// Destructor
  virtual ~wxPdfOcg();

  /// Set OCG index
  void SetIndex(int index) { m_index = index; }

  /// Get OCG index
  int  GetIndex() const { return m_index; }

  /// Set object index
  void SetObjIndex(int n) { m_n = n; }

  /// Get object index
  int  GetObjIndex() const { return m_n; }

  /// Check whether OCG is initialized
  bool IsOk() const;

  /// Get 
  wxPdfOcgType GetType() const { return m_type; }

protected:
  wxPdfOcgType   m_type;      ///< Type of OCG

private:
  int            m_index;     ///< Index number of this OCG
  int            m_n;         ///< OCG object index
};

/// Class for a layer with optional content.
class WXDLLIMPEXP_PDFDOC wxPdfLayer : public wxPdfOcg
{
public:
  /// Constructor
  /**
  * Creates a new layer.
  * \param name the name of the layer
  */    
  wxPdfLayer(const wxString& name);

  /// Destructor
  virtual ~wxPdfLayer();

  /// Get the name of the layer
  /**
  * \return the layer name
  */
  wxString GetName() const { return m_name; }

  /// Get the layer title
  /**
  * \return the layer title
  */
  wxString GetTitle() const { return m_title; }

  /// Set Optional Content Group (OCG) Intent
  /**
  * \param[in] intent Combination of the defined wxPDF_OCG_INTENT_???? values to set
  */
  void SetIntent(const unsigned int intent) { m_intent |= intent; };

  /// Get Optional Content Group (OCG) Intent
  /**
  * \return the current combination of wxPDF_OCG_INTENT_* values
  */
  int GetIntent() const { return m_intent; };

  /// Clear Optional Content Group (OCG) Intent
  /**
  * \param[in] intent Combination of the defined wxPDF_OCG_INTENT_???? values to clear
  */
  void ClearIntent(const unsigned int intent) { m_intent &= ~intent; };

  /// Set the initial visibility of the layer
  /**
  * Sets the initial visibility of the layer.
  * \param on the initial visibility of the layer
  */
  void SetOn(bool on) { m_on = on; }

  /// Get the initial visibility of the layer
  /**
  * Gets the initial visibility of the layer.
  * \return the initial visibility of the layer
  */
  bool IsOn() const  { return m_on; }

  /// Set the visibility of the layer on the layer panel
  /**
  * Sets the visibility of the layer in Acrobat's layer panel. If <CODE>false</CODE>
  * the layer cannot be directly manipulated by the user. Note that any children layers will
  * also be absent from the panel.
  * \param onPanel the visibility of the layer in Acrobat's layer panel
  */
  void SetOnPanel(bool onPanel) { m_onPanel = onPanel; }

  /// Get the layer visibility on the layer panel
  /**
  * Gets the layer visibility in Acrobat's layer panel
  * \return the layer visibility in Acrobat's layer panel
  */
  bool IsOnPanel() const { return m_onPanel; }

  /// Add child layer
  /**
  * Adds a child layer. Nested layers can only have one parent.
  * \param child the child layer
  */    
  bool AddChild(wxPdfLayer* child);

  /// Get parent layer
  /**
  * Gets the parent layer.
  * \return the parent layer or <CODE>null</CODE> if the layer has no parent
  */    
  wxPdfLayer* GetParent() const { return m_parent; }

  bool HasChildren() const { return !m_children.IsEmpty(); }
  
  /// Get the list of child layers
  /**
  * Gets the child layers.
  * \return the child layers or an empty list if the layer has no children
  */    
  wxPdfArrayLayer GetChildren() const { return m_children; }

  /// Set the creator's info
  /**
  * Used by the creating application to store application-specific
  * data associated with this optional content group.
  * \param creator a text string specifying the application that created the group
  * \param subtype a string defining the type of content controlled by the group. Suggested
  * values include but are not limited to <B>Artwork</B>, for graphic-design or publishing
  * applications, and <B>Technical</B>, for technical designs such as building plans or
  * schematics
  */    
  void SetCreatorInfo(const wxString& creator, const wxString& subtype);

  /// Set the language
  /**
  * Specifies the language of the content controlled by this
  * optional content group
  * \param lang a language string which specifies a language and possibly a locale
  * (for example, <B>es-MX</B> represents Mexican Spanish)
  * \param preferred used by viewer applications when there is a partial match but no exact
  * match between the system language and the language strings in all usage dictionaries
  */    
  void SetLanguage(const wxString& lang, bool preferred);

  /// Set the export state
  /**
  * Specifies the recommended state for content in this
  * group when the document (or part of it) is saved by a viewer application to a format
  * that does not support optional content (for example, an earlier version of
  * PDF or a raster image format).
  * \param exportState the export state
  */    
  void SetExport(bool exportState);

  /// Set the range of zoom magnification
  /**
  * Specifies a range of magnifications at which the content
  * in this optional content group is best viewed.
  * \param min the minimum recommended magnification factors at which the group
  * should be ON. A negative value will set the default to 0
  * \param max the maximum recommended magnification factor at which the group
  * should be ON. A negative value will set the largest possible magnification supported by the
  * viewer application
  */    
  void SetZoom(double min, double max);

  /// Set the print state
  /**
  * Specifies that the content in this group is intended for
  * use in printing
  * \param subtype a name specifying the kind of content controlled by the group;
  * for example, <B>Trapping</B>, <B>PrintersMarks</B> and <B>Watermark</B>
  * \param printState indicates that the group should be
  * set to that state when the document is printed from a viewer application
  */    
  void SetPrint(const wxString& subtype, bool printState);

  /// Set the view state
  /**
  * Indicates that the group should be set to that state when the
  * document is opened in a viewer application.
  * \param viewState the view state
  */    
  void SetView(bool viewState);

  /// Get the usage dictionary
  /**
  * \return a reference to the usage dictionary
  */
  wxPdfDictionary* GetUsage() const { return m_usage; }

  /// Create a title layer
  /**
  * Creates a title layer. A title layer is not really a layer but a collection of layers
  * under the same title heading.
  * \param title the title text
  * \return the title layer
  */    
  static wxPdfLayer* CreateTitle(const wxString& title);

protected:
  /// Copy constructor
  wxPdfLayer(const wxPdfLayer& encoding);

  /// Assignment operator
  wxPdfLayer& operator=(const wxPdfLayer& encoding);

  /// Set the parent of the layer
  bool SetParent(wxPdfLayer* parent);

  /// Allocate the usage dictonary
  wxPdfDictionary* AllocateUsage();

private:
  wxString         m_name;     ///< Name of the layer
  wxString         m_title;    ///< Title of a layer group
  int              m_intent;   ///< Intended use
  bool             m_on;       ///< Flag for ON/OFF state
  bool             m_onPanel;  ///< Flag for ON/OFF state on layer panel
  wxPdfLayer*      m_parent;   ///< parent layer
  wxPdfArrayLayer  m_children; ///< list of child layers
  wxPdfDictionary* m_usage;    ///< usage dictionary
};

/// Class for a layer with optional content.
class WXDLLIMPEXP_PDFDOC wxPdfLayerMembership : public wxPdfOcg
{
public:
  /// Constructor
  wxPdfLayerMembership();

  /// Destructor
  virtual ~wxPdfLayerMembership();

  /// Add layer
  /**
  * Adds a new member to the layer.
  * \param layer the new member to the layer
  */    
  bool AddMember(wxPdfLayer* layer);

  /// Get a list of the layers
  wxPdfArrayLayer GetMembers() const;

  /// Set visibility policy
  /**
  * Sets the visibility policy for content belonging to this
  * membership dictionary. Possible values are 
  * wxPDF_OCGPOLICY_ALLON, wxPDF_OCGPOLICY_ANYON, wxPDF_OCGPOLICY_ANYOFF and wxPDF_OCGPOLICY_ALLOFF.
  * The default value is wxPDF_OCGPOLICY_ANYON.
  * \param policy the visibility policy
  */    
  void SetVisibilityPolicy(wxPdfOcgPolicy policy);

  /// Get the visibility policy
  wxPdfOcgPolicy GetVisibilityPolicy() const;

protected:
  /// Copy constructor
  wxPdfLayerMembership(const wxPdfLayerMembership& layer);

  /// Assignment operator
  wxPdfLayerMembership& operator=(const wxPdfLayerMembership& layer);

private:
  wxPdfArrayLayer m_layers; ///< Array of layers which are members
  wxPdfOcgPolicy  m_policy; ///< Visibility policy
};

/// Class for a group of layers with optional content.
class WXDLLIMPEXP_PDFDOC wxPdfLayerGroup
{
public:
  /// Constructor
  wxPdfLayerGroup();

  /// Destructor
  virtual ~wxPdfLayerGroup();

  /// Copy constructor
  wxPdfLayerGroup(const wxPdfLayerGroup& layer);

  /// Assignment operator
  wxPdfLayerGroup& operator=(const wxPdfLayerGroup& layer);

  /// Add layer
  /**
  * Adds a new member to the layer radio group.
  * \param layer the new member to the layer radio group
  */    
  bool Add(wxPdfLayer* layer);

  /// Get list of group members
  wxPdfArrayLayer GetGroup() const;

private:
  wxPdfArrayLayer m_layers; ///< Array of layers which are members of the radio group
};

#endif

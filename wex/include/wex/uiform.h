#ifndef __uiform_h
#define __uiform_h

#include <vector>

#include <wx/wx.h>
#include <wx/clrpicker.h>

#define wxUI_USE_OVERLAY 1
#include <wx/overlay.h>

class wxUIProperty;

class wxUIPropertyUpdateInterface
{
public:
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p ) = 0;
};

class wxUIProperty
{
public:
	explicit wxUIProperty();
	explicit wxUIProperty( const wxUIProperty &copy );
	explicit wxUIProperty( double d );
	explicit wxUIProperty( int i );
	explicit wxUIProperty( int i, const wxArrayString &named_options );
	explicit wxUIProperty( int i, const wxString &commasep_options );
	explicit wxUIProperty( bool b );
	explicit wxUIProperty( const char *str );
	explicit wxUIProperty( const wxString &s );
	explicit wxUIProperty( const wxColour &c );
	explicit wxUIProperty( const wxImage &img );
	explicit wxUIProperty( const wxArrayString &strlist );

	enum { INVALID, DOUBLE, BOOLEAN, INTEGER, COLOUR, STRING, STRINGLIST, IMAGE };

	int GetType();

	void Set( double d );
	void Set( bool b );
	void Set( int i );
	void Set( const wxColour &c );
	void Set( const wxString &s );
	void Set( const wxArrayString &list);
	void Set( const wxImage &img );
	
	void SetNamedOptions( const wxArrayString &opts, int selection = -1 );
	wxArrayString GetNamedOptions();

	int GetInteger();
	bool GetBoolean();
	double GetDouble();
	wxColour GetColour();
	wxString GetString();
	wxArrayString GetStringList();
	wxImage GetImage();

	wxString AsString();

	bool IsValid() { return m_type != INVALID; }

	void Write( wxOutputStream & );
	bool Read( wxInputStream & );
	
	void AddUpdateInterface( const wxString &name, wxUIPropertyUpdateInterface *pui );
	void RemoveUpdateInterface( wxUIPropertyUpdateInterface *pui );
	void ClearUpdateInterfaces();

private:
	void Init();
	wxUint8 m_type;
	wxUIProperty *m_pReference;

	double m_doubleVal;
	bool m_boolVal;
	int m_intVal;
	wxColour m_colour;
	wxString m_string;
	wxImage m_image;
	wxArrayString m_strList;
	wxArrayString m_namedOptions;
	
	void ValueChanged();
	struct puidata { wxUIPropertyUpdateInterface *pui; wxString id; };
	std::vector<puidata> m_updateInterfaceList;
};

class wxUIObject : public wxUIPropertyUpdateInterface
{
public:
	wxUIObject( );
	virtual ~wxUIObject();

	virtual wxString GetTypeName() = 0;
	virtual wxUIObject *Duplicate() = 0;
	virtual bool IsNativeObject() = 0;

	virtual bool Copy( wxUIObject *rhs );
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom );
	virtual bool IsWithin( int xx, int yy );
	virtual bool DrawDottedOutline() { return false; }
	
	/* methods for handling native controls */
	virtual wxWindow *CreateNative( wxWindow * ) { return 0; }
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p );
	virtual void OnNativeEvent( );

	void DestroyNative();
	wxWindow *GetNative() { return m_nativeObject; }
	template<typename c> c*GetNative() { return dynamic_cast<c*>(m_nativeObject); }

	void SetName( const wxString &name );
	wxString GetName();
	void SetGeometry( const wxRect &r );
	wxRect GetGeometry();
	wxPoint GetPosition();
	wxSize GetSize();
	virtual void Show( bool b );
	bool IsVisible() { return m_visible; }

	wxUIProperty &Property( const wxString &name );
	bool HasProperty( const wxString &name );
	wxArrayString Properties();
	int GetTabOrder();
		
	virtual void Write( wxOutputStream & );
	virtual bool Read( wxInputStream & );
	

protected:
	void AddProperty( const wxString &name, wxUIProperty *prop );
	
private:
	void DeleteProperties();
	bool m_visible;
	struct propdata { wxString name, lowered; wxUIProperty *prop; };
	std::vector<propdata> m_properties;

protected:
	wxWindow *AssignNative( wxWindow *win );

private:
	wxWindow *m_nativeObject;
	
};


class wxUIPropertyEditor : public wxPanel
{
public:
	wxUIPropertyEditor( wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize );
	virtual ~wxUIPropertyEditor();
	
	void SetObject( wxUIObject *obj );
	wxUIObject *GetObject() { return m_curObject; }
	void UpdatePropertyValues();
	
private:
	void OnChange( wxCommandEvent & );
	void OnColourPicker( wxColourPickerEvent & );
	void OnButton( wxCommandEvent & );
	
	struct pgpinfo {
		wxString name;
		int type;
		wxWindow *label;
		wxWindow *editor;
	};
	
	void ValueToPropGrid( pgpinfo &p );
	void PropGridToValue( pgpinfo &p );
	
	pgpinfo *Find( wxObject *editor );

	std::vector<pgpinfo> m_curProps;
	wxUIObject *m_curObject;
	wxString m_lastChangedProperty;
	
	DECLARE_EVENT_TABLE();
};

class wxUIObjectTypeProvider
{
public:
	static void RegisterBuiltinTypes();
	static void Register( wxUIObject *obj );
	static std::vector<wxUIObject*> GetTypes();
	static wxUIObject *Create( const wxString &type );
};

class wxUIFormData
{
public:
	explicit wxUIFormData();
	explicit wxUIFormData( const wxUIFormData &rhs );
	virtual ~wxUIFormData();
	
	wxUIFormData *Duplicate() const;
	void Copy( const wxUIFormData &rhs );

	// build/destroy native interface as needed
	void Attach( wxWindow *form );
	void Detach();
	wxWindow *GetWindow() { return m_formWindow; }
	
	// load/save form definition
	virtual void Write( wxOutputStream & );
	virtual bool Read( wxInputStream & );

	// methods to create/edit UI objects
	wxUIObject *Create( const wxString &type, const wxRect &geom, const wxString &name = wxEmptyString );
	wxUIObject *Create( const wxString &type );
	void Add( wxUIObject * );
	void Delete( wxUIObject * );
	void DeleteAll();
	wxUIObject *Find( const wxString &name );
	std::vector<wxUIObject*> GetObjects();
	wxUIObject **GetObjects( size_t *n );
	void Raise( wxUIObject * );

	// form properties
	void SetName( const wxString &name );
	wxString GetName();
	void SetSize( int width, int height );
	wxSize GetSize();

	// virtual function to provide
	// descendant classes ways to provide info about 
	// labels, units, and other properties to be rendered
	// on the actual form
	virtual bool GetMetaData( const wxString &name,
		wxString *label, wxString *units, wxColour *colour );

protected:
	wxString m_name;
	int m_width;
	int m_height;
	std::vector< wxUIObject* > m_objects;

	wxWindow *m_formWindow;
};


class wxUIObjectCopyBuffer
{
public:
	wxUIObjectCopyBuffer();
	~wxUIObjectCopyBuffer();

	void Clear();
	void Assign( std::vector<wxUIObject*> &objlist);
	std::vector<wxUIObject*> Get();
	int Count();

private:
	std::vector<wxUIObject*> m_copyList;
};

class wxUIFormEvent : public wxCommandEvent
{
public:
	wxUIFormEvent( wxUIObject *uiobj, wxEventType commandType = wxEVT_NULL, int id = 0)
		: wxCommandEvent(commandType, id), m_uiObject(uiobj) { }
	wxUIObject *GetUIObject() { return m_uiObject; }
	void SetUIObject(wxUIObject *o) { m_uiObject = o; }
private:
	wxUIObject *m_uiObject;
};

DECLARE_EVENT_TYPE( wxEVT_UIFORM_SELECT, -1 )

typedef void (wxEvtHandler::*wxUIFormEventFunction)(wxUIFormEvent&);

#define EVT_UIFORM_GENERIC(id, type, fn) \
    DECLARE_EVENT_TABLE_ENTRY( type, id, -1, \
    (wxObjectEventFunction) (wxEventFunction) (wxCommandEventFunction) \
    wxStaticCastEvent( wxUIFormEventFunction, & fn ), (wxObject *) NULL ),

#define EVT_UIFORM_SELECT( id, fn ) EVT_UIFORM_GENERIC( id, wxEVT_UIFORM_SELECT, fn )

class wxUIFormEditor : public wxWindow
{
public:
	wxUIFormEditor( wxWindow *parent, int id = wxID_ANY, 
		const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize );

	void SetFormData( wxUIFormData *form );
	wxUIFormData *GetFormData() { return m_form; }

	wxUIObject *CreateObject( const wxString &type );

	void SetCopyBuffer( wxUIObjectCopyBuffer *cpbuf);
	void SetPropertyEditor( wxUIPropertyEditor *pe );
	void EnableTabOrderMode(bool b);

	void ClearSelections();
	std::vector<wxUIObject*> GetSelections();
	bool IsSelected( wxUIObject * );
	bool IsSelected( const wxString &name );

	void Snap( int *x, int *y, int spacing = -1 );
	int Snap( int p, int spacing = -1 );

	void SetViewMode( bool b );
	
	void GetScale( double *x, double *y );
	wxSize ScaleSize( const wxSize &s );
	wxRect ScaleRect( const wxRect &r );

private:	

	void DrawMultiSelBox();
	void DrawMoveResizeOutlines();
	int IsOverResizeBox(int x, int y, wxUIObject *obj);
	void SetResizeCursor(int pos = -1);

	double m_scaleX, m_scaleY;
	bool m_moveMode;
	bool m_moveModeErase;
	bool m_multiSelMode;
	bool m_multiSelModeErase;
	bool m_resizeMode;
	bool m_resizeModeErase;
	int m_resizeBox;
	int m_origX, m_origY;
	int m_diffX, m_diffY;
	int m_diffW, m_diffH;
	int m_popupX, m_popupY;

#ifdef wxUI_USE_OVERLAY
	wxOverlay m_overlay;
#endif
	
	void OnMouseMove(wxMouseEvent &evt);
	void OnLeftUp(wxMouseEvent &evt);
	void OnDoubleClick(wxMouseEvent &evt);
	void OnLeftDown(wxMouseEvent &evt);
	void OnRightDown(wxMouseEvent &evt);
	void OnSize(wxSizeEvent &evt);
	void OnPaint(wxPaintEvent &evt);
	void OnPopup(wxCommandEvent &evt);
	void OnCreateCtrl(wxCommandEvent &evt);
	
	bool m_tabOrderMode;
	int m_tabOrderCounter;
	int m_snapSpacing;

	bool m_modified;
	wxUIObjectCopyBuffer *m_copyBuffer;
	wxUIPropertyEditor *m_propEditor;
	std::vector<wxUIObject*> m_selected;
	wxUIFormData *m_form;

	bool m_viewMode;

	bool m_enableScaling;

	DECLARE_EVENT_TABLE()
};


class wxUIFormDesigner : public wxScrolledWindow
{
public:
	wxUIFormDesigner( wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize );

	void SetFormData( wxUIFormData *form );
	wxUIFormData *GetFormData();

	void SetFormSize( int width, int height );
	wxSize GetFormSize();

	void SetPropertyEditor( wxUIPropertyEditor *pe ) { m_editor->SetPropertyEditor(pe); }
	void SetCopyBuffer( wxUIObjectCopyBuffer *cb ) { m_editor->SetCopyBuffer(cb); }

	wxUIFormEditor *GetEditor() { return m_editor; }

	void UpdateScrollbars();
private:
	void OnPaint(wxPaintEvent &evt);
	void OnResize(wxSizeEvent &evt);
	void OnMouseDown(wxMouseEvent &evt);
	void OnMouseUp(wxMouseEvent &evt);
	void OnMouseMove(wxMouseEvent &evt);

	bool m_mouseDown;
	int m_diffX, m_diffY;

	wxUIFormData *m_formData;
	wxUIFormEditor *m_editor;

	DECLARE_EVENT_TABLE()
};

#endif

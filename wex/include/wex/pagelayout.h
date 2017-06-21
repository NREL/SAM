#ifndef __pagelayout_h
#define __pagelayout_h

#include <vector>

#include <wx/wx.h>

class wxPdfDocument;

// forward
class wxPageOutputDevice
{
public:
	virtual ~wxPageOutputDevice() {  }

	enum { SERIF, SANSERIF, FIXED };
	enum { SOLID, DOTTED };

	// note: page output device operates natively in inches.

	virtual void Clip( float x, float y, float width, float height ) = 0;
	virtual void Unclip() = 0;
	virtual void Color( const wxColour &c ) = 0;
	virtual void LineStyle( float thick = 0.013f, int sty = SOLID ) = 0;
	virtual void Line( float x1, float y1, float x2, float y2 ) = 0;
	virtual void Rect( float x, float y, float width, float height, bool fill = false, float radius = 0.0f  ) = 0;
	virtual void Circle( float x, float y, float radius, bool fill = false ) = 0;
	virtual void Arc( float x, float y, float width, float height, float angle1, float angle2, bool fill = false ) = 0;
	virtual void Font( int face, int points, bool bold, bool italic ) = 0;
	virtual void Text( float x, float y, const wxString &text, float angle = 0.0f ) = 0;
	virtual void Measure( const wxString &text, float *width, float *height ) = 0;
	virtual void Image( const wxImage &img, float top, float left, float width = 0.0f, float height = 0.0f ) = 0;
};


class wxPageScaleInterface {
public:
	wxPageScaleInterface() { }
	virtual ~wxPageScaleInterface() { }
	virtual float GetPPI() = 0;
	virtual void PageToScreen( float x, float y, int *px, int *py ) = 0;
	virtual void ScreenToPage( int px, int py, float *x, float *y ) = 0;
};

class wxScreenOutputDevice : public wxPageOutputDevice
{
private:
	wxPageScaleInterface *m_lc;
	wxDC &m_dc;
	wxPen m_pen;
	wxBrush m_brush;
	wxFont m_font;
public:
	wxScreenOutputDevice( wxPageScaleInterface *lc, wxDC &dc );
	virtual void Clip( float x, float y, float width, float height );
	virtual void Unclip();
	virtual void Color( const wxColour &c );
	virtual void LineStyle( float thick, int sty );
	virtual void Line( float x1, float y1, float x2, float y2 );
	virtual void Rect( float x, float y, float width, float height, bool fill, float radius );
	virtual void Circle( float x, float y, float radius, bool fill );
	virtual void Arc( float x, float y, float width, float height, float angle1, float angle2, bool fill );
	virtual void Font( int face, int points, bool bold, bool italic );
	virtual void Text( float x, float y, const wxString &text, float angle );
	virtual void Measure( const wxString &text, float *width, float *height );
	virtual void Image( const wxImage &img, float top, float left, float width, float height );
	float DevicePPI();
	int ScalePointSizeToScreen( int requested );
};


class wxPdfOutputDevice : public wxPageOutputDevice
{
private:
static int m_imageIndex;
	wxPdfDocument &m_pdf;
public:
	wxPdfOutputDevice( wxPdfDocument &pdf );
	virtual void Clip( float x, float y, float width, float height );
	virtual void Unclip();
	virtual void Color( const wxColour &c );
	virtual void LineStyle( float thick, int style );
	virtual void Line( float x1, float y1, float x2, float y2 );
	virtual void Rect( float x, float y, float width, float height, bool fill = false, float radius = 0.0f  ) ;
	virtual void Circle( float x, float y, float radius, bool fill = false );
	virtual void Arc( float x, float y, float width, float height, float angle1, float angle2, bool fill = false );
	virtual void Font( int face, int points, bool bold, bool italic );
	virtual void Text( float x, float y, const wxString &text, float angle );
	virtual void Measure( const wxString &text, float *width, float *height );
	virtual void Image( const wxImage &img, float top, float left, float width = 0.0f, float height = 0.0f );
};



class wxPageLayout;
class wxPageLayoutCtrl;
class wxPageObject;

class wxPagePdfRenderer
{
public:
	// Valid escape sequence for header/footer text
	// @PAGENUM@, @PAGECOUNT@, @DATETIME@
	void AddPage( wxPageLayout *page,
		const wxString &header = wxEmptyString,
		const wxString &footer = wxEmptyString );

	bool Render( const wxString &pdf_file );

private:
	struct page_data
	{
		wxPageLayout *page;
		wxString header;
		wxString footer;
	};

	std::vector<page_data> m_pageList;
};


class wxPageObject
{
	friend class wxPageLayout;
	friend class wxPageLayoutCtrl;
public:
	wxPageObject( )
		: m_x(1), m_y(1), m_width(3), m_height(2) {  }
	virtual ~wxPageObject() {  }

	virtual wxString TypeName() = 0;
	virtual wxPageObject *Duplicate() = 0;
	virtual bool Copy( wxPageObject *obj ) = 0;
	virtual wxString Description() = 0;
	virtual bool EditObject( wxPageLayoutCtrl *layout_window ) = 0; /* should return true if object was modified, false otherwise */
	virtual void Render( wxPageOutputDevice & ) = 0;
	virtual bool ReadData( wxInputStream &is ) = 0;
	virtual bool WriteData( wxOutputStream &os ) = 0;


	bool Inside( float x, float y );
	void SetGeometry( float x=-1.0f, float y=-1.0f, float width=-1.0f, float height=-1.0f );
	void GetGeometry( float *x, float *y, float *width, float *height );
protected:
	float m_x, m_y, m_width, m_height;
};

class wxPageLayout
{
	friend class wxPageLayoutCtrl;
public:
	wxPageLayout( int orient = wxPORTRAIT, wxPaperSize paper = wxPAPER_LETTER );
	virtual ~wxPageLayout();

	wxPageLayout *Duplicate();

	void Add( wxPageObject *obj );
	void Delete( wxPageObject *obj );
	void Raise( wxPageObject *obj );
	void Clear();
	wxPageObject **GetObjects( int *count );	
	wxPageObject *Under( float x, float y );

	void SetOrientation( int orient );
	int GetOrientation() { return m_orientation; }
	void SetPaperType( wxPaperSize paper ); // wxPAPER_LETTER, wxPAPER_LEGAL, etc...
	wxPaperSize GetPaperType() { return m_paperType; }
	void SetMargins( float top, float bottom, float left, float right );
	void GetMargins( float *top, float *bottom, float *left, float *right );

	void GetDimensions( float *horiz, float *vert );

	bool Read( wxInputStream &is );
	bool Write( wxOutputStream &os );

	void Render( wxPageOutputDevice &dv );

private:

	int m_orientation;
	wxPaperSize m_paperType;
	float m_marginTop, m_marginBottom, m_marginLeft, m_marginRight;
	std::vector<wxPageObject*> m_objectList;

	float m_paperXDim, m_paperYDim;
	wxPageLayoutCtrl *m_viewCtrl;
	
	struct {
		int x, y, width, height;
	} layout_cache;
};

class wxPageObjectTypes
{
public:
	static void Register( wxPageObject *dummy );
	static wxPageObject *Create( const wxString &type );
	static wxArrayString AllTypes();
	static wxString DescriptionOf( const wxString &type );
};

class wxPageLayoutEvent : public wxCommandEvent
{
public:
	wxPageLayoutEvent( wxEventType cmdType = wxEVT_NULL, int id=0)
		: wxCommandEvent( cmdType, id )
	{
		m_pageObject = 0;
		m_pageLayout = 0;
	}

	wxPageLayoutEvent( wxEventType cmdType, wxPageLayoutCtrl *sender, wxPageObject *obj = 0);

	wxPageObject *GetPageObject() { return m_pageObject; }
	wxPageLayout *GetPage() { return m_pageLayout; }


	void Set( wxPageObject *obj, wxPageLayout *page )
	{
		m_pageObject = obj;
		m_pageLayout = page;
	}

private:
	wxPageObject *m_pageObject;
	wxPageLayout *m_pageLayout;
};

DECLARE_EVENT_TYPE( wxEVT_PAGELAYOUT_SELECT, -1 )
DECLARE_EVENT_TYPE( wxEVT_PAGELAYOUT_MODIFY, -1 )
DECLARE_EVENT_TYPE( wxEVT_PAGELAYOUT_CREATE, -1 )

typedef void (wxEvtHandler::*wxPageLayoutEventFunction)( wxPageLayoutEvent & );

#define EVT_PAGELAYOUT_GENERIC( id, type, fn ) \
    DECLARE_EVENT_TABLE_ENTRY( type, id, -1, \
    (wxObjectEventFunction) (wxEventFunction) (wxCommandEventFunction) \
    wxStaticCastEvent( wxPageLayoutEventFunction, & fn ), (wxObject *) NULL ),

#define EVT_PAGELAYOUT_SELECT( id, fn ) EVT_PAGELAYOUT_GENERIC( id, wxEVT_PAGELAYOUT_SELECT, fn )
#define EVT_PAGELAYOUT_MODIFY( id, fn ) EVT_PAGELAYOUT_GENERIC( id, wxEVT_PAGELAYOUT_MODIFY, fn )
#define EVT_PAGELAYOUT_CREATE( id, fn ) EVT_PAGELAYOUT_GENERIC( id, wxEVT_PAGELAYOUT_CREATE, fn )


class wxPageLayoutCtrl : 
		public wxScrolledWindow, 
		public wxPageScaleInterface
{
public:
	wxPageLayoutCtrl( wxWindow *parent, int id, const wxPoint &pos=wxDefaultPosition, const wxSize &size = wxDefaultSize);
	virtual ~wxPageLayoutCtrl();

	void SetPage( wxPageLayout *page );
	wxPageLayout *GetPage();
	wxPageObject **GetSelections(int *count);

	void Invalidate();
	void Invalidate( wxPageObject * );

	void SetPPI( float ppi );
	float GetScreenPPI() { return m_screenPPI; }
	virtual float GetPPI() { return m_ppi; }
	virtual void PageToScreen( float x, float y, int *px, int *py );
	virtual void ScreenToPage( int px, int py, float *x, float *y );

	void Zoom( float percent = -1.0f );
	void FitHorizontal();
	void FitVertical();
	void AlignEdges( int edge = wxTOP );

	void ShowGrid( bool b ) { m_showGrid = b; Invalidate(); }
	bool ShowGrid() { return m_showGrid; }
	void ShowOutlines( bool b ) { m_showOutlines = b; Invalidate(); }
	bool ShowOutlines() { return m_showOutlines; }
	void SnapCoordinates( bool b ) { m_snapCoordinates = b; }
	bool SnapCoordinates() { return m_snapCoordinates; }
	void SnapSpacing( float spacing ) { if (spacing>0) m_snapSpacing = spacing; }
	float SnapSpacing() { return m_snapSpacing; }
	void GridSpacing( float spacing ) { if (spacing>0) m_gridSpacing = spacing; Invalidate(); }
	float GridSpacing() { return m_gridSpacing; }

	void CreatePopupMenu();
private:
	wxPageLayout *m_page;

	int m_xMargin, m_yMargin;
	float m_ppi;
	float m_screenPPI;

	wxMenu *m_popupMenu;
	int m_popupX, m_popupY;

	bool m_snapCoordinates;
	bool m_showGrid, m_showOutlines;
	bool m_moveMode;
	bool m_moveModeErase;
	bool m_multiSelMode;
	bool m_multiSelModeErase;
	int m_origX, m_origY, m_diffX, m_diffY, m_diffW, m_diffH;
	int m_resizeBox;
	bool m_resizeMode;
	bool m_resizeModeErase;
	float m_gridSpacing;
	float m_snapSpacing;

	std::vector< wxPageObject* > m_selectedItems;

	class paper_menu_item {
	public:
		paper_menu_item() { menuId = 0; paperSize = wxPAPER_LETTER; }
		paper_menu_item( const wxString &text, wxPaperSize size )
		{
			caption = text;
			paperSize = size;
			menuId = 0;
		}
		wxString caption;
		wxPaperSize paperSize;
		int menuId;
	};
	std::vector< paper_menu_item > m_paperMenuInfo;
		
	wxColour m_selectColour;
	wxCursor m_standardCursor;
	wxCursor m_moveResizeCursor;
	wxCursor m_nwseCursor;
	wxCursor m_nsCursor;
	wxCursor m_weCursor;
	wxCursor m_neswCursor;

	void DrawBackground( wxDC &dc );
	void DrawPageOutline( wxDC &dc );

	void OnResize( wxSizeEvent &evt );
	void OnPaint( wxPaintEvent &evt );
	void OnErase( wxEraseEvent &evt );
	void OnLeftDown( wxMouseEvent &evt );
	void OnLeftDouble( wxMouseEvent &evt );
	void OnLeftUp( wxMouseEvent &evt );
	void OnRightDown( wxMouseEvent &evt );
	void OnMouseMove( wxMouseEvent &evt );
	void OnLeave( wxMouseEvent &evt );
	void OnMouseWheel( wxMouseEvent &evt );
	void OnPopup( wxCommandEvent &evt );

	void MouseToPage( const wxPoint &pt, float *x, float *y );
	void DrawMultiSelBox();
	void DrawMoveResizeOutlines();
	void SetResizeCursor(int pos = -1);
	int IsOverResizeBox(int x, int y, wxPageObject *obj);

	void Snap( float *x, float *y );
	float Snap( float );

	DECLARE_EVENT_TABLE();
};

#endif

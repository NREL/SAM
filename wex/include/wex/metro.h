#ifndef __wexmetro_h
#define __wexmetro_h

#include <vector>

#include <wx/gdicmn.h>
#include <wx/font.h>
#include <wx/window.h>
#include <wx/panel.h>
#include <wx/control.h>
#include <wx/scrolwin.h>
#include <wx/popupwin.h>
#include <wx/bookctrl.h>


class wxMetroThemeProvider
{
public:
	virtual ~wxMetroThemeProvider();
	virtual wxFont Font( int style, int size );
	virtual wxColour Colour( int id );
	virtual wxBitmap Bitmap( int id, bool light );
};


enum {
	// fonts
	wxMT_NORMAL,
	wxMT_LIGHT, 
	wxMT_SEMIBOLD,
	wxMT_SEMILIGHT,

	// colors
	wxMT_FOREGROUND,
	wxMT_BACKGROUND,
	wxMT_HOVER,
	wxMT_DIMHOVER,
	wxMT_LIGHTHOVER,
	wxMT_ACCENT,
	wxMT_TEXT,
	wxMT_ACTIVE,
	wxMT_SELECT,
	wxMT_HIGHLIGHT,

	// bitmaps
	wxMT_LEFTARROW,
	wxMT_RIGHTARROW,
	wxMT_DOWNARROW,
	wxMT_UPARROW
};

class wxMetroTheme
{
public:
	static void SetTheme( wxMetroThemeProvider *theme );
	static wxMetroThemeProvider &GetTheme();

	static wxFont Font( int style = wxMT_NORMAL, int size = -1 );
	static wxColour Colour( int id );
	static wxBitmap Bitmap( int id, bool light = true );
};


// widget styles for

#define wxMB_RIGHTARROW 0x01
#define wxMB_LEFTARROW 0x02
#define wxMB_DOWNARROW 0x04
#define wxMB_UPARROW 0x08
#define wxMB_ALIGNLEFT 0x10
#define wxMB_SMALLFONT 0x20

class wxMetroButton : public wxWindow
{
public:
	wxMetroButton(wxWindow *parent, int id, const wxString &label, const wxBitmap &bitmap = wxNullBitmap,
			const wxPoint &pos=wxDefaultPosition, const wxSize &sz=wxDefaultSize,
			long style = 0);
	
	wxSize DoGetBestSize() const;
	void SetLabel( const wxString &l ) { m_label = l; InvalidateBestSize();}
	wxString GetLabel() const { return m_label; }
	void SetBitmap( const wxBitmap &b ) { m_bitmap = b; InvalidateBestSize(); }
	wxBitmap GetBitmap() const { return m_bitmap; }
	void SetStyle( long sty ) { m_style = sty; InvalidateBestSize(); }
	long GetStyle() const { return m_style; }

private:
	wxString m_label;
	wxBitmap m_bitmap;
	int m_state;
	bool m_pressed;
	long m_style;
	int m_space;

	void OnPaint(wxPaintEvent &evt);
	void OnResize(wxSizeEvent &evt);
	void OnLeftDown(wxMouseEvent &evt);
	void OnLeftUp(wxMouseEvent &evt);
	void OnEnter(wxMouseEvent &evt);
	void OnLeave(wxMouseEvent &evt);
	void OnMotion(wxMouseEvent &evt);

	DECLARE_EVENT_TABLE()
};


class wxSimplebook;


// styles for wxMetroTabList, wxMetroNotebook
#define wxMT_LIGHTTHEME 0x01
#define wxMT_MENUBUTTONS 0x02

// issues wxEVT_LISTBOX when a new item is selected
// issues wxEVT_BUTTON when a menu button is clicked
class wxMetroTabList : public wxWindow
{
public:
	wxMetroTabList( wxWindow *parent, int id = wxID_ANY,
		const wxPoint &pos = wxDefaultPosition,
		const wxSize &size = wxDefaultSize,
		long style = 0 );

	void Append( const wxString &label, bool button = false, bool shown=true );
	void Insert( const wxString &label, size_t pos, bool button = false, bool shown=true );
	void Remove( const wxString &label );
	void RemoveAt( size_t n );
	int Find( const wxString &label );
	void Clear();
	size_t Count();
	wxString GetLabel( size_t idx );
	wxArrayString GetLabels();
	void SetLabel( size_t idx, const wxString &text );
	void SetSelection( size_t idx );
	size_t GetSelection();
	wxString GetStringSelection();
	void ReorderLeft( size_t idx );
	void ReorderRight( size_t idx );
	void HideItem(size_t idx);
	void ShowItem(size_t idx);
	
	wxPoint GetPopupMenuPosition( int index );
	
	wxSize DoGetBestSize() const;

protected:
	struct item
	{
		item( const wxString &l, bool bb, bool shw) : label(l), x_start(0), width(0), shown(shw), button(bb), visible(true) { }
		item(const item &x) : label(x.label), x_start(x.x_start), width(x.width), shown(x.shown), button(x.button), visible(true) { }
		wxString label;
		int x_start;
		int width;
		bool shown;
		bool button;
		bool visible;
	};

	std::vector<item> m_items;
	int m_dotdotWidth;
	bool m_dotdotHover;
	int m_selection;
	int m_pressIdx;
	int m_hoverIdx;
	long m_style;
	bool m_buttonHover;

	int m_space, m_xPadding, m_yPadding;

	bool IsOverButton( int mouse_x, size_t i );

	void OnMenu( wxCommandEvent & );
	void OnSize( wxSizeEvent & );
	void OnPaint( wxPaintEvent & );
	void OnLeftDown( wxMouseEvent & );
	void OnLeftUp( wxMouseEvent & );
	void OnLeave( wxMouseEvent & );
	void OnMouseMove( wxMouseEvent & );

	void SwitchPage( size_t i );

	DECLARE_EVENT_TABLE();
};

#if 0
// sends EVT_NOTEBOOK_PAGE_CHANGED event
class wxMetroNotebook : public wxBookCtrlBase
{
public:
	wxMetroNotebook(wxWindow *parent, int id=-1, 
		const wxPoint &pos=wxDefaultPosition, const wxSize &sz=wxDefaultSize, long style = 0 );

    virtual bool SetPageText(size_t n, const wxString& strText);
    virtual wxString GetPageText(size_t n) const;
    virtual int GetPageImage(size_t n) const;
    virtual bool SetPageImage(size_t n, int imageId);
    virtual bool InsertPage(size_t n,
                            wxWindow *page,
                            const wxString& text,
                            bool bSelect = false,
                            int imageId = NO_IMAGE);
    virtual int SetSelection(size_t n);
    virtual int ChangeSelection(size_t n);
	virtual bool DeleteAllPages();
	virtual void UpdateSelectedPage(size_t newsel);
	virtual void MakeChangedEvent(wxBookCtrlEvent &evt);
	virtual wxBookCtrlEvent* wxMetroNotebook::CreatePageChangingEvent() const;

protected:
	void OnTabListChanged( wxCommandEvent & );
    virtual wxWindow *DoRemovePage(size_t page);
	wxMetroTabList *GetTabs() const { return (wxMetroTabList*)m_bookctrl; }

	DECLARE_EVENT_TABLE();
};

#endif

class wxMetroNotebook : public wxWindow
{
public:
	wxMetroNotebook(wxWindow *parent, int id=-1, 
		const wxPoint &pos=wxDefaultPosition, const wxSize &sz=wxDefaultSize, long style = 0 );

	void AddPage(wxWindow *win, const wxString &text, bool active=false, bool button=false);
	void AddScrolledPage(wxWindow *win, const wxString &text, bool active=false, bool button=false);
	size_t GetPageCount() const;
	wxWindow *RemovePage( size_t index );
	void DeletePage( size_t index );
	int GetPageIndex(wxWindow *win);
	wxWindow *GetPage( size_t index );
	void HidePage( size_t index );
	void ShowPage( size_t index );
	
	int GetSelection() const;
	void SetSelection(size_t id);
	void SetText(size_t id, const wxString &text);
	wxString GetText( size_t id ) const;
	wxString GetSelectionText() const;
	
	wxPoint GetPopupMenuPosition( int index );

private:
	wxMetroTabList *m_list;	

	struct page_info
	{
		wxWindow *win;
		wxString text;
		wxWindow *scroll_win;
		bool button;
		bool visible;
	};

	std::vector<page_info> m_pages;
	int m_sel;

	void OnSize(wxSizeEvent &evt);
	void UpdateTabList();
	void OnTabList( wxCommandEvent & );
	void SwitchPage( size_t i );
	void UpdateLayout();

	DECLARE_EVENT_TABLE()
};


class wxMetroListBox : public wxScrolledWindow
{
public:
	wxMetroListBox( wxWindow *parent, int id, 
		const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize );
	virtual ~wxMetroListBox();

	void Add( const wxString &label );
	void Add( const wxArrayString &list );
	void Delete( size_t idx );
	void Clear();
	int Find( const wxString &label );
	int Count();
	void Set( size_t idx, const wxString &label );
	wxString Get( size_t idx );
	int GetSelection();
	wxString GetSelectionString();
	void SetSelection( int idx );
	bool SetSelectionString( const wxString &s );
	wxString GetValue();

	void Invalidate();
private:
	struct _item
	{
		wxString name;
		wxRect geom;
	};

	std::vector<_item> m_items;
	int m_hoverIdx;
	int m_selectedIdx;
	int m_space;

	void OnPaint( wxPaintEvent &evt );
	void OnErase( wxEraseEvent &evt );
	void OnResize( wxSizeEvent &evt );
	void OnLeftDown( wxMouseEvent &evt );
	void OnMouseMove( wxMouseEvent &evt );
	void OnLeave( wxMouseEvent &evt );
	void OnDClick( wxMouseEvent &evt );

	DECLARE_EVENT_TABLE();
};


class wxMetroPopupMenu
{
public:
	wxMetroPopupMenu( long theme = 0  /* can be wxMT_LIGHTTHEME */ );
	void SetFont( const wxFont &f );
	void Append( int id, const wxString &label );
	void AppendCheckItem( int id, const wxString &label, bool checked = false );
	void AppendSeparator();

	void Popup( wxWindow *parent, const wxPoint &pos = wxDefaultPosition, int origin = wxTOP|wxLEFT );

private:
	long m_theme;
	struct item {
		int id;
		wxString label;
		bool is_checkItem;
		bool checked;
	};
	std::vector<item> m_items;
	wxFont m_font;
};


#endif


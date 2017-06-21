#include <wx/settings.h>
#include <wx/fontenum.h>
#include <wx/dcbuffer.h>
#include <wx/simplebook.h>
#include <wx/scrolwin.h>
#include <wx/menu.h>
#include <wx/sizer.h>
#include <wx/event.h>
#include <wx/log.h>

#include <algorithm>
#include "wex/metro.h"
#include "wex/utils.h"

#include "wex/icons/up_arrow_13.cpng"
#include "wex/icons/down_arrow_13.cpng"
#include "wex/icons/left_arrow_13.cpng"
#include "wex/icons/right_arrow_13.cpng"

#include "wex/icons/up_arrow_gray_13.cpng"
#include "wex/icons/down_arrow_gray_13.cpng"
#include "wex/icons/left_arrow_gray_13.cpng"
#include "wex/icons/right_arrow_gray_13.cpng"

static wxMetroThemeProvider g_basicTheme;
static wxMetroThemeProvider *g_otherTheme = 0;

void wxMetroTheme::SetTheme( wxMetroThemeProvider *theme )
{
	if ( g_otherTheme == theme ) return;
	if ( g_otherTheme != 0 ) delete g_otherTheme;
	g_otherTheme = theme;
}

wxMetroThemeProvider &wxMetroTheme::GetTheme()
{
	if ( g_otherTheme != 0 ) return *g_otherTheme;
	else return g_basicTheme;
}

wxFont wxMetroTheme::Font( int style, int size )
{
	return GetTheme().Font( style, size );
}

wxColour wxMetroTheme::Colour( int id )
{
	return GetTheme().Colour( id );
}

wxBitmap wxMetroTheme::Bitmap( int id, bool light )
{
	return GetTheme().Bitmap( id, light );
}



wxMetroThemeProvider::~wxMetroThemeProvider()
{
	 // nothing here
}

wxFont wxMetroThemeProvider::Font( int style, int size )
{
	wxFont font( wxSystemSettings::GetFont( wxSYS_DEFAULT_GUI_FONT ) );

	if ( size > 1 )
		font.SetPointSize( size );

#ifdef __WXMSW__
	wxString face = "Segoe UI";
	if ( style == wxMT_LIGHT ) face = "Segoe UI Light";
	else if ( style == wxMT_SEMIBOLD ) face = "Segoe UI Semibold";
	else if ( style == wxMT_SEMILIGHT ) face = "Segoe UI Semilight";

	if ( wxFontEnumerator::IsValidFacename( face ) )
		font.SetFaceName( face );
#else
	if ( style == wxMT_SEMIBOLD )
		font.SetWeight( wxFONTWEIGHT_BOLD );
#endif

	return font;
}

wxColour wxMetroThemeProvider::Colour( int id )
{
	switch( id )
	{
	case wxMT_FOREGROUND: return wxColour( 0, 114, 198 );
	case wxMT_BACKGROUND:  return *wxWHITE;
	case wxMT_HOVER: return wxColour( 0, 88, 153 );
	case wxMT_DIMHOVER: return wxColour( 0, 107, 186 );
	case wxMT_LIGHTHOVER: return wxColour( 231, 232, 238 );
	case wxMT_ACCENT: return wxColour( 255, 143, 50 );
	case wxMT_TEXT: return wxColour( 135, 135, 135 ); 
	case wxMT_ACTIVE: return wxColour( 0, 114, 198 );
	case wxMT_SELECT:  return wxColour(193,210,238);
	case wxMT_HIGHLIGHT: return wxColour(224,232,246);
	default: return wxNullColour;
	}
}

wxBitmap wxMetroThemeProvider::Bitmap( int id, bool light )
{
static wxBitmap left_arrow;
	if ( left_arrow.IsNull() ) left_arrow = wxBITMAP_PNG_FROM_DATA( left_arrow_13 );
	
static wxBitmap down_arrow;
	if ( down_arrow.IsNull() ) down_arrow = wxBITMAP_PNG_FROM_DATA( down_arrow_13 );
	
static wxBitmap right_arrow;
	if ( right_arrow.IsNull() ) right_arrow = wxBITMAP_PNG_FROM_DATA( right_arrow_13 );
	
static wxBitmap up_arrow;
	if ( up_arrow.IsNull() ) up_arrow = wxBITMAP_PNG_FROM_DATA( up_arrow_13 );
	
static wxBitmap left_arrow_dark;
	if ( left_arrow_dark.IsNull() ) left_arrow_dark = wxBITMAP_PNG_FROM_DATA( left_arrow_gray_13 );
	
static wxBitmap down_arrow_dark;
	if ( down_arrow_dark.IsNull() ) down_arrow_dark = wxBITMAP_PNG_FROM_DATA( down_arrow_gray_13 );
	
static wxBitmap right_arrow_dark;
	if ( right_arrow_dark.IsNull() ) right_arrow_dark = wxBITMAP_PNG_FROM_DATA( right_arrow_gray_13 );
	
static wxBitmap up_arrow_dark;
	if ( up_arrow_dark.IsNull() ) up_arrow_dark = wxBITMAP_PNG_FROM_DATA( up_arrow_gray_13 );

	switch ( id )
	{
	case wxMT_LEFTARROW: return light ? left_arrow : left_arrow_dark;
	case wxMT_RIGHTARROW: return light ? right_arrow : right_arrow_dark;
	case wxMT_UPARROW: return light ? up_arrow : up_arrow_dark;
	case wxMT_DOWNARROW: return light ? down_arrow : down_arrow_dark;
	default: return wxNullBitmap;
	}
}




BEGIN_EVENT_TABLE(wxMetroButton, wxWindow)
	EVT_PAINT(wxMetroButton::OnPaint)
	EVT_SIZE(wxMetroButton::OnResize)
	EVT_LEFT_DOWN(wxMetroButton::OnLeftDown)
	EVT_LEFT_DCLICK( wxMetroButton::OnLeftDown )
	EVT_LEFT_UP(wxMetroButton::OnLeftUp)
	EVT_ENTER_WINDOW(wxMetroButton::OnEnter)
	EVT_LEAVE_WINDOW(wxMetroButton::OnLeave)
	EVT_MOTION(wxMetroButton::OnMotion)
END_EVENT_TABLE()


wxMetroButton::wxMetroButton(wxWindow *parent, int id, const wxString &label, const wxBitmap &bitmap,
	const wxPoint &pos, const wxSize &sz, long style)
	: wxWindow(parent, id, pos, sz), m_label( label ), m_bitmap( bitmap ), m_style( style )
{
	m_space = (int)( 6 * wxGetScreenHDScale() );
	SetBackgroundStyle(wxBG_STYLE_CUSTOM);
	m_state = 0;  // state: 0=normal, 1=hover, 2=click
	m_pressed = false;

	if ( style & wxMB_SMALLFONT )
		SetFont( *wxNORMAL_FONT );
	else
		SetFont( wxMetroTheme::Font( wxMT_NORMAL, 11) );
}

wxSize wxMetroButton::DoGetBestSize() const
{
	wxClientDC dc( const_cast<wxMetroButton*>( this ) );
	dc.SetFont( GetFont() );
	int tw, th;
	dc.GetTextExtent( m_label, &tw, &th );
	
	wxBitmap icon(wxNullBitmap);

	if ( m_style & wxMB_RIGHTARROW )
		icon = wxMetroTheme::Bitmap( wxMT_RIGHTARROW );
	else if ( m_style & wxMB_DOWNARROW )
		icon = wxMetroTheme::Bitmap( wxMT_DOWNARROW );
	else if ( m_style & wxMB_UPARROW )
		icon = wxMetroTheme::Bitmap( wxMT_UPARROW );
	else if ( m_style & wxMB_LEFTARROW )
		icon = wxMetroTheme::Bitmap( wxMT_LEFTARROW );

	if ( !icon.IsNull() )
	{
		tw += icon.GetWidth() + ( tw > 0 ? m_space : 0 );
		if ( icon.GetHeight() > th )
			th = icon.GetHeight();
	}

	if ( !m_bitmap.IsNull() )
	{
		tw += m_bitmap.GetWidth() + ( tw > 0 ? m_space : 0 );
		if ( m_bitmap.GetHeight() > th )
			th = m_bitmap.GetHeight();
	}

	int xspace = 0;
	if ( !m_label.IsEmpty() ) xspace = 20;
	else if ( !m_bitmap.IsNull() ) xspace = 10;
	else if ( !icon.IsNull() ) xspace = 5;

	return wxSize( tw + xspace, th + 10 );
}

void wxMetroButton::OnPaint(wxPaintEvent &)
{
	wxAutoBufferedPaintDC dc(this);
	int cwidth, cheight;
	GetClientSize(&cwidth, &cheight);

	dc.SetBackground( m_state > 0 
		? wxMetroTheme::Colour( wxMT_HOVER ) 
		: wxMetroTheme::Colour( wxMT_FOREGROUND ) );
	dc.Clear();

	int tw, th;
	dc.GetTextExtent( m_label, &tw, &th );

	wxBitmap icon(wxNullBitmap);
	int xoffset_icon = 0;	
	int yoffset_icon = 0;
	int icon_width = 0;
	int icon_height = 0;

	if ( m_style & wxMB_RIGHTARROW )
		icon = wxMetroTheme::Bitmap( wxMT_RIGHTARROW );	
	else if ( m_style & wxMB_DOWNARROW )
	{
		icon = wxMetroTheme::Bitmap( wxMT_DOWNARROW );
		yoffset_icon++;
	}
	else if ( m_style & wxMB_UPARROW )
		icon = wxMetroTheme::Bitmap( wxMT_UPARROW );
	else if ( m_style & wxMB_LEFTARROW )
		icon = wxMetroTheme::Bitmap( wxMT_LEFTARROW );
	
	int xoffset = 0;
	if ( !icon.IsNull() )
	{
		int space = ( tw > 0 ? m_space : 0 );
		tw += icon.GetWidth() + space;
		xoffset -= icon.GetWidth() + space;
		icon_width = icon.GetWidth();
		icon_height = icon.GetHeight();
	}

	int bit_width = 0;
	int bit_height = 0;
	int bit_space = 0;
	int yoffset_bit = 0;
	if ( !m_bitmap.IsNull() )
	{
		bit_width = m_bitmap.GetWidth();
		bit_height = m_bitmap.GetHeight();
		tw += bit_width;
		xoffset -= bit_width;
		if ( !icon.IsNull() || !m_label.IsEmpty() )
		{
			tw += m_space;
			xoffset -= m_space;
		}
		bit_space = m_space/2;
	}

	dc.SetFont( GetFont() );
	dc.SetTextForeground( m_state == 1 
		? wxMetroTheme::Colour( wxMT_HIGHLIGHT ) 
		: wxMetroTheme::Colour( wxMT_BACKGROUND ) );
	
	int yoffset = 0;
	if ( !m_label.IsEmpty() && m_state == 2 )
		yoffset++;
	else if ( m_state == 2 && m_style & wxMB_LEFTARROW )
		xoffset_icon--;
	else if ( m_state ==2 && m_style & wxMB_RIGHTARROW )
		xoffset_icon++;
	else if ( m_state == 2 && m_style & wxMB_DOWNARROW )
		yoffset_icon++;
	else if ( m_state == 2 && !m_bitmap.IsNull() && m_label.IsEmpty() && icon.IsNull() )
		yoffset_bit++;

	int bit_x = cwidth/2 - tw/2;
	int text_x = bit_x + bit_width + bit_space;
	int icon_x = cwidth/2 + tw/2 - icon.GetWidth();

	if ( m_style & wxMB_ALIGNLEFT )
	{
		bit_x = 10;
		text_x = bit_x + bit_width + bit_space;
		icon_x = 10 + tw - icon.GetWidth();
	}

	if ( !m_bitmap.IsNull() )
		dc.DrawBitmap( m_bitmap, bit_x, cheight/2 - bit_height/2 + yoffset_bit );

	dc.DrawText( m_label, text_x, cheight/2 - th/2+yoffset );
	
	if ( !icon.IsNull() )
		dc.DrawBitmap( icon, icon_x+xoffset_icon, cheight/2 - icon_height/2 + yoffset_icon );
}

void wxMetroButton::OnResize(wxSizeEvent &)
{
	Refresh();
}

void wxMetroButton::OnLeftDown(wxMouseEvent &)
{
	SetFocus();
	CaptureMouse();
	m_pressed = true;
	m_state = 2;
	Refresh();
}

void wxMetroButton::OnLeftUp(wxMouseEvent &evt)
{
	if (HasCapture())
	{
		m_pressed = false;
		ReleaseMouse();
		m_state = 0;
		Refresh();

		wxSize sz = GetClientSize();
		int x = evt.GetX();
		int y = evt.GetY();
		if (x>=0&&y>=0 && x<=sz.GetWidth() && y<=sz.GetHeight())
		{
			wxCommandEvent press(wxEVT_COMMAND_BUTTON_CLICKED, this->GetId() );
			press.SetEventObject(this);
			GetEventHandler()->ProcessEvent(press);
		}
	}

}

void wxMetroButton::OnEnter(wxMouseEvent &)
{
	m_state = m_pressed?2:1;
	Refresh();
}

void wxMetroButton::OnLeave(wxMouseEvent &)
{
	m_state = 0;
	Refresh();
}

void wxMetroButton::OnMotion( wxMouseEvent &)
{
	if ( m_state != 1 )
	{
		m_state = 1;
		Refresh();
	}
}




enum { ID_TAB0 = wxID_HIGHEST+142 };

BEGIN_EVENT_TABLE(wxMetroTabList, wxWindow)
	EVT_LEFT_DCLICK( wxMetroTabList::OnLeftDown)
	EVT_LEFT_DOWN( wxMetroTabList::OnLeftDown )
	EVT_LEFT_UP( wxMetroTabList::OnLeftUp)
	EVT_MOTION(wxMetroTabList::OnMouseMove)
	EVT_PAINT( wxMetroTabList::OnPaint )
	EVT_SIZE( wxMetroTabList::OnSize )
	EVT_LEAVE_WINDOW( wxMetroTabList::OnLeave )
	EVT_MENU_RANGE( ID_TAB0, ID_TAB0+100, wxMetroTabList::OnMenu )
END_EVENT_TABLE()

wxMetroTabList::wxMetroTabList( wxWindow *parent, int id,
	const wxPoint &pos, const wxSize &size, long style )
	: wxWindow( parent, id, pos, size )
{
	double sf = wxGetScreenHDScale();
	m_space = (int)( 2.0 * sf );
	m_xPadding = (int)( 10.0 * sf );
	m_yPadding = (int)( 12.0 * sf );


	SetBackgroundStyle( wxBG_STYLE_PAINT );
	m_dotdotWidth = 0;
	m_dotdotHover = false;
	m_pressIdx = -1;
	m_hoverIdx = -1;
	m_buttonHover = false;
	m_style = style;
	m_selection = 0;

	SetFont( wxMetroTheme::Font( wxMT_LIGHT, 16 ) );
}

void wxMetroTabList::Append( const wxString &label, bool button, bool shown )
{
	if ( m_style & wxMT_MENUBUTTONS ) button = true;
	m_items.push_back( item(label, button, shown) );
}

void wxMetroTabList::Insert( const wxString &label, size_t pos, bool button, bool shown )
{
	if ( m_style & wxMT_MENUBUTTONS ) button = true;
	m_items.insert( m_items.begin()+pos, item(label, button, shown) );
}

void wxMetroTabList::Remove( const wxString &label )
{
	int idx = Find( label );
	if ( idx >= 0 )
		m_items.erase( m_items.begin()+idx );
}

void wxMetroTabList::RemoveAt( size_t n )
{
	if ( n >= 0 && n < m_items.size() )
		m_items.erase( m_items.begin()+n );
}

int wxMetroTabList::Find( const wxString &label )
{
	for ( size_t i=0;i<m_items.size();i++ )
		if ( m_items[i].label == label ) 
			return i;

	return -1;
}


void wxMetroTabList::ReorderLeft( size_t idx )
{
	if ( idx > 0 && idx < m_items.size() )
	{
		item x( m_items[idx-1] );
		m_items[idx-1] = m_items[idx];
		m_items[idx] = x;

		if ( m_selection == idx ) m_selection--;

		m_hoverIdx = -1;
		Refresh();
	}
}

void wxMetroTabList::ReorderRight( size_t idx )
{
	if ( idx >= 0 && idx < m_items.size()-1)
	{
		item x( m_items[idx+1] );
		m_items[idx+1] = m_items[idx];
		m_items[idx] = x;

		if ( m_selection == idx ) m_selection++;

		m_hoverIdx = -1;
		Refresh();
	}
}

wxPoint wxMetroTabList::GetPopupMenuPosition( int index )
{
	if ( index >= 0 && index < m_items.size() )	return ClientToScreen( wxPoint( m_items[index].x_start, GetClientSize().y ) );
	else return wxDefaultPosition;
}

wxString wxMetroTabList::GetLabel( size_t idx )
{
	if ( idx < m_items.size() ) return m_items[idx].label;
	else return wxEmptyString;
}

wxArrayString wxMetroTabList::GetLabels()
{
	wxArrayString list;
	for( size_t i=0;i<m_items.size();i++ )
		list.Add( m_items[i].label );
	return list;
}

void wxMetroTabList::SetLabel( size_t idx, const wxString &text )
{
	if ( idx < m_items.size() )
	{
		m_items[idx].label = text;
		Refresh();
	}
}

size_t wxMetroTabList::Count()
{
	return m_items.size();
}

void wxMetroTabList::Clear()
{
	m_items.clear();
}

void wxMetroTabList::SetSelection( size_t i )
{
	m_selection = i;
	if ( m_selection >= m_items.size() )
		m_selection = m_items.size()-1;
}

void wxMetroTabList::HideItem(size_t idx)
{
	if (m_items[idx].shown) { m_items[idx].shown = false; }
}

void wxMetroTabList::ShowItem(size_t idx)
{
	if (!m_items[idx].shown) { m_items[idx].shown = true; }
}

size_t wxMetroTabList::GetSelection()
{
	return m_selection;
}

wxString wxMetroTabList::GetStringSelection()
{
	if( m_selection >= 0 && m_selection < m_items.size() )
		return m_items[m_selection].label;
	else
		return wxEmptyString;
}

wxSize wxMetroTabList::DoGetBestSize() const
{
	wxClientDC dc( const_cast<wxMetroTabList*>( this ) );
	dc.SetFont( GetFont() );

	int width = 0;

	int button_width = wxMetroTheme::Bitmap( wxMT_DOWNARROW ).GetWidth() + m_space + m_space;

	for ( size_t i=0;i<m_items.size(); i++ )
	{
		int tw, th;
		dc.GetTextExtent( m_items[i].label, &tw, &th );
		width += tw + m_space + m_xPadding;
		if ( m_items[i].button ) width += button_width;
	}

	int height = dc.GetCharHeight() + m_yPadding;

	return wxSize( width, height );
}

void wxMetroTabList::OnSize(wxSizeEvent &)
{
	Refresh();
}

void wxMetroTabList::OnPaint(wxPaintEvent &)
{
	wxAutoBufferedPaintDC dc(this);

	int cwidth, cheight;
	GetClientSize(&cwidth, &cheight);
	
	bool light = ( m_style & wxMT_LIGHTTHEME );

	dc.SetBackground( light ? *wxWHITE : wxMetroTheme::Colour( wxMT_FOREGROUND ) );
	dc.Clear();

	int x = m_space;

	m_dotdotWidth = 0;
	
	wxBitmap button_icon( wxMetroTheme::Bitmap( wxMT_DOWNARROW, !light ) );
	int button_width = button_icon.GetWidth() + m_space + m_space;
	int button_height = button_icon.GetHeight();
	
	dc.SetFont( GetFont() );
	int CharHeight = dc.GetCharHeight();
	for (size_t i=0;i<m_items.size();i++)
	{
		int txtw, txth;
		if (m_items[i].shown)
		{
			dc.GetTextExtent( m_items[i].label, &txtw, &txth );
			m_items[i].x_start = x;
			m_items[i].width = txtw+m_xPadding+m_xPadding;
			if ( m_items[i].button ) m_items[i].width += button_width;
			x += m_items[i].width + m_space;

			if ( i > 0 && x > cwidth - 25 ) // 25 approximates width of '...'	
				m_dotdotWidth = 1;

			m_items[i].visible = ( m_dotdotWidth == 0 );
		}
		else
		{
			if (m_items[i].visible) { m_items[i].visible = false; }
		}
	}

	// compute size of '...'
	int dotdot_height = 0;
	if ( m_dotdotWidth > 0 )
	{
		wxFont font( wxMetroTheme::Font( wxMT_NORMAL, 18) );
		font.SetWeight( wxFONTWEIGHT_BOLD );
		dc.SetFont( font );
		dc.GetTextExtent( "...", &m_dotdotWidth, &dotdot_height );
		m_dotdotWidth += m_space + m_space;
		dc.SetFont( GetFont() ); // restore font
	}

	// if the selected item is not shown, shift it into place and hide any others that it might cover up
	if ( m_dotdotWidth > 0 
		&& m_selection >= 0 
		&& m_selection < (int) m_items.size()
		&& !m_items[m_selection].visible )
	{
		int shifted_x = cwidth - m_dotdotWidth - m_items[m_selection].width;

		for ( int i = (int)m_items.size()-1; i >= 0; i-- )
		{
			if ( m_items[i].x_start + m_items[i].width >= shifted_x )
				m_items[i].visible = false;
			else
				break;
		}

		m_items[m_selection].visible = true;
		m_items[m_selection].x_start = shifted_x;
	}
	

	// now draw all the items that have been determined to be visible
	for ( size_t i=0; i<m_items.size(); i++ )
	{
		if ( !m_items[i].visible || !m_items[i].shown ) continue;

		if ( !light )
		{
			wxColour col( i==m_hoverIdx||i==m_selection 
				? wxMetroTheme::Colour( wxMT_HOVER ) 
				: wxMetroTheme::Colour( wxMT_FOREGROUND ) );
			dc.SetPen( wxPen(col, 1) );
			dc.SetBrush( wxBrush(col) );
			dc.DrawRectangle( m_items[i].x_start, 0, m_items[i].width, cheight );
			
			if ( m_items[i].button
				&& m_hoverIdx == i
				&&  m_buttonHover  )
			{
				dc.SetPen( wxPen( wxMetroTheme::Colour( light ? wxMT_HIGHLIGHT : wxMT_DIMHOVER ), 1 ) );
				dc.SetBrush( wxBrush( wxMetroTheme::Colour( light ? wxMT_HIGHLIGHT : wxMT_DIMHOVER ) ) );
				dc.DrawRectangle( m_items[i].x_start + m_items[i].width - button_width - m_xPadding + m_space,
					0, button_width + m_xPadding - m_space, cheight );
			}
		}


		wxColour text( *wxWHITE );
		if ( light )
		{
			text = ( i == (int)m_selection 
				? wxMetroTheme::Colour( wxMT_FOREGROUND ) 
				: ( i==m_hoverIdx 
					? wxMetroTheme::Colour( wxMT_SELECT ) 
					: wxMetroTheme::Colour( wxMT_TEXT ) ) );
		}

		dc.SetTextForeground( text );			
		dc.DrawText( m_items[i].label, m_items[i].x_start + m_xPadding, cheight/2-CharHeight/2-1 );

		if ( m_items[i].button )
		{	
			dc.DrawBitmap( button_icon, 
				m_items[i].x_start + m_items[i].width - m_space - button_width, 
				cheight/2-button_height/2+1 );
		}

	}

	if ( m_dotdotWidth > 0 )
	{
		if ( !light && m_dotdotHover )
		{
			dc.SetPen( wxPen(wxMetroTheme::Colour( wxMT_HOVER ), 1) );
			dc.SetBrush( wxBrush(wxMetroTheme::Colour( wxMT_HOVER ) ) );
			dc.DrawRectangle( cwidth - m_dotdotWidth, 0, m_dotdotWidth, cheight );
		}

		wxFont font( wxMetroTheme::Font( wxMT_NORMAL, 18) );
		font.SetWeight( wxFONTWEIGHT_BOLD );
		dc.SetFont( font );
		dc.SetTextForeground( light 
			? (m_dotdotHover ?  wxMetroTheme::Colour( wxMT_SELECT ) : wxMetroTheme::Colour( wxMT_TEXT )) 
			: *wxWHITE );

		dc.DrawText( "...", cwidth - m_dotdotWidth + m_space, cheight/2 - dotdot_height/2-1 );
	}

	if ( light )
	{
		dc.SetPen( wxPen( wxColour(209,209,209) ));
		dc.DrawLine( 0, cheight-1, cwidth, cheight-1);
	}
}
	
void wxMetroTabList::OnLeftDown(wxMouseEvent &evt)
{
	int cwidth, cheight;
	GetClientSize(&cwidth, &cheight);

	int mouse_x = evt.GetX();
	for (size_t i=0;i<m_items.size();i++)
	{
		if ( m_items[i].visible
			&& mouse_x >= m_items[i].x_start 
			&& mouse_x < m_items[i].x_start + m_items[i].width)
		{
			if ( m_items[i].button && IsOverButton( mouse_x, i ) )
			{
				SwitchPage( i ); // first switch pages if we're going to fire the context menu up

				wxCommandEvent evt( wxEVT_BUTTON, GetId() );
				evt.SetEventObject( this );
				evt.SetInt( i );
				evt.SetString( m_items[i].label );
				ProcessEvent( evt );
				return;
			}

			m_pressIdx = i;
			if (this->HasCapture())
				this->ReleaseMouse();

			this->CaptureMouse();
			return;
		}
	}

	if ( m_selection >= 0 && m_selection < m_items.size()
		&& m_dotdotWidth > 0 && mouse_x > cwidth - m_dotdotWidth )
	{
		wxMetroPopupMenu menu( m_style&wxMT_LIGHTTHEME ? wxMT_LIGHTTHEME : 0 );			
		for ( size_t i=0;i< m_items.size();i++)
			if ( m_items[i].shown && !m_items[i].visible )
				menu.Append( ID_TAB0+i, m_items[i].label /*, i == (int)m_selection*/ );
		
		//wxPoint pos( cwidth-m_dotdotWidth, cheight );
		wxPoint pos( cwidth, cheight );
		pos = ClientToScreen(pos);		
		menu.Popup( this, pos, wxTOP|wxRIGHT );
	}
}

void wxMetroTabList::OnMouseMove(wxMouseEvent &evt)
{
	int cwidth, cheight;
	GetClientSize(&cwidth, &cheight);
	m_hoverIdx = -1;
	m_buttonHover = false;
	int mouse_x = evt.GetX();

	bool was_hovering = m_dotdotHover;
	m_dotdotHover = ( m_dotdotWidth > 0 && mouse_x > cwidth - m_dotdotWidth );

	for (size_t i=0;i<m_items.size();i++)
	{
		if ( m_items[i].visible 
			&& mouse_x >= m_items[i].x_start 
			&& mouse_x < m_items[i].x_start + m_items[i].width)
		{
			if ( m_items[i].button && IsOverButton( mouse_x, i ) )
				m_buttonHover = true;

			if (m_hoverIdx != (int)i)
				Refresh();
			m_hoverIdx = i;
			return;
		}
	}

	if ( m_dotdotHover != was_hovering )
		Refresh();
}

bool wxMetroTabList::IsOverButton( int mouse_x, size_t i )
{
	if ( m_items[i].button )
	{
		int button_width = wxMetroTheme::Bitmap( wxMT_DOWNARROW ).GetWidth() + m_space + m_space;
		if ( mouse_x > m_items[i].x_start + m_items[i].width - button_width - m_xPadding + m_space )
			return true;
	}
	
	return false;
}

void wxMetroTabList::OnLeftUp(wxMouseEvent &evt)
{
	if (m_pressIdx >= 0 && this->HasCapture())
		this->ReleaseMouse();

	int mouse_x = evt.GetX();
	for (size_t i=0;i<m_items.size();i++)
	{
		if ( m_items[i].visible 
			&& mouse_x >= m_items[i].x_start 
			&& mouse_x < m_items[i].x_start + m_items[i].width)
		{
			if ( IsOverButton( mouse_x, i ) )
				return;

			SwitchPage( i );
			return;
		}
	}
}

void wxMetroTabList::OnMenu( wxCommandEvent &evt )
{
	SwitchPage( evt.GetId() - ID_TAB0 );
}

void wxMetroTabList::OnLeave(wxMouseEvent &)
{
	m_pressIdx = -1;
	m_hoverIdx = -1;
	m_dotdotHover = false;
	Refresh();
}

void wxMetroTabList::SwitchPage( size_t i )
{
	m_selection = i;
	wxCommandEvent evt( wxEVT_LISTBOX, GetId() );
	evt.SetEventObject( this );
	evt.SetInt( i );
	evt.SetString( m_items[i].label );
	ProcessEvent( evt );
	Refresh();

}

#if 0
BEGIN_EVENT_TABLE( wxMetroNotebook, wxBookCtrlBase )
	EVT_LISTBOX( wxID_ANY, wxMetroNotebook::OnTabListChanged )
END_EVENT_TABLE()

wxMetroNotebook::wxMetroNotebook(wxWindow *parent, int id, const wxPoint &pos, const wxSize &sz, long style)
	: wxBookCtrlBase( parent, id, pos, sz, wxBORDER_NONE|wxBK_TOP )
{
	//SetInternalBorder( 0 );
	//SetControlMargin( 0 );

	m_bookctrl = new wxMetroTabList( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, style );
	
	/*
    wxSizer* mainSizer = new wxBoxSizer(IsVertical() ? wxVERTICAL : wxHORIZONTAL);
	m_controlSizer = new wxBoxSizer(IsVertical() ? wxHORIZONTAL : wxVERTICAL);
    m_controlSizer->Add(m_bookctrl, 1, (IsVertical() ? wxALIGN_CENTRE_VERTICAL : wxALIGN_CENTRE) |wxGROW, 0);
    mainSizer->Add(m_controlSizer, 0, (IsVertical() ? (int) wxGROW : (int) wxALIGN_CENTRE_VERTICAL)|wxALL, m_controlMargin);
    SetSizer(mainSizer);*/
#ifdef __WXMSW__
    // On XP with themes enabled the GetViewRect used in GetControllerSize() to
    // determine the space needed for the list view will incorrectly return
    // (0,0,0,0) the first time.  So send a pending event so OnSize will be
    // called again after the window is ready to go.  Technically we don't
    // need to do this on non-XP windows, but if things are already sized
    // correctly then nothing changes and so there is no harm.
    wxSizeEvent evt;
    GetEventHandler()->AddPendingEvent(evt);
#endif

}

bool wxMetroNotebook::SetPageText(size_t n, const wxString& strText)
{
	GetTabs()->SetLabel( n, strText );
	return true;
}

wxString wxMetroNotebook::GetPageText(size_t n) const 
{
	return GetTabs()->GetLabel( n );
}

int wxMetroNotebook::GetPageImage(size_t n) const
{
	return wxNOT_FOUND;
}

bool wxMetroNotebook::SetPageImage(size_t n, int imageId)
{
	return false;
}

bool wxMetroNotebook::InsertPage(size_t n,
                        wxWindow *page,
                        const wxString& text,
                        bool bSelect,
                        int imageId )
{    
	if ( !wxBookCtrlBase::InsertPage(n, page, text, bSelect, imageId) )
        return false;

    GetTabs()->Insert(text, n);

    // if the inserted page is before the selected one, we must update the
    // index of the selected page
    if ( int(n) <= m_selection )
    {
        // one extra page added
        m_selection++;
        GetTabs()->SetSelection(m_selection);
    }

	if ( bSelect ) ChangeSelection( n ); // don't fire an event on insertion

	GetTabs()->Refresh();

    return true;
}

int wxMetroNotebook::SetSelection(size_t n)
{
	return DoSetSelection(n, SetSelection_SendEvent);
}

int wxMetroNotebook::ChangeSelection(size_t n)
{
	return DoSetSelection(n);
}

wxWindow *wxMetroNotebook::DoRemovePage(size_t page)
{
    wxWindow *win = wxBookCtrlBase::DoRemovePage(page);

    if ( win )
    {
        GetTabs()->RemoveAt(page);
		GetTabs()->Refresh();
        DoSetSelectionAfterRemoval(page);
    }

    return win;
}

bool wxMetroNotebook::DeleteAllPages()
{
    GetTabs()->Clear();
	GetTabs()->Refresh();
    return wxBookCtrlBase::DeleteAllPages();
}


void wxMetroNotebook::UpdateSelectedPage(size_t newsel)
{
    m_selection = static_cast<int>(newsel);
    GetTabs()->SetSelection(m_selection);
}

wxBookCtrlEvent* wxMetroNotebook::CreatePageChangingEvent() const
{
    return new wxBookCtrlEvent(wxEVT_BOOKCTRL_PAGE_CHANGING, m_windowId);
}

void wxMetroNotebook::MakeChangedEvent(wxBookCtrlEvent &evt)
{
    evt.SetEventType(wxEVT_BOOKCTRL_PAGE_CHANGED);
}

void wxMetroNotebook::OnTabListChanged( wxCommandEvent &evt )
{
    if ( evt.GetEventObject() != m_bookctrl )
    {
        evt.Skip();
        return;
    }

    const int selNew = evt.GetSelection();

    if ( selNew == m_selection )
    {
        // this event can only come from our own Select(m_selection) below
        // which we call when the page change is vetoed, so we should simply
        // ignore it
        return;
    }

    SetSelection(selNew);

    // change wasn't allowed, return to previous state
    if (m_selection != selNew)
        GetTabs()->SetSelection(m_selection);
}
#endif

/************* wxMetroNotebook ************** */

BEGIN_EVENT_TABLE(wxMetroNotebook, wxWindow)
	EVT_SIZE( wxMetroNotebook::OnSize )
	EVT_LISTBOX( wxID_ANY, wxMetroNotebook::OnTabList )
END_EVENT_TABLE()

wxMetroNotebook::wxMetroNotebook(wxWindow *parent, int id, const wxPoint &pos, const wxSize &sz, long style)
	: wxWindow( parent, id, pos, sz )
{
	m_sel = -1;
	SetBackgroundColour( wxMetroTheme::Colour( wxMT_BACKGROUND ) );
	SetForegroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );
	m_list = new wxMetroTabList(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, style );
}

void wxMetroNotebook::AddPage(wxWindow *win, const wxString &text, bool active, bool button)
{
	win->Show( false );
	win->Reparent( this );

	page_info x;
	x.win = win;
	x.text = text;
	x.scroll_win = 0;
	x.button = button;
	x.visible = true;

	m_pages.push_back( x );

	UpdateTabList();
	UpdateLayout();

	if (active || m_pages.size() == 1)
		SetSelection( m_pages.size()-1 );
}

void wxMetroNotebook::HidePage(size_t index)
{
	if ( index >= m_pages.size() ) return;
	m_list->HideItem(index);
	if (m_pages[index].visible) { m_pages[index].visible = false; }
}

void wxMetroNotebook::ShowPage(size_t index)
{
	if ( index >= m_pages.size() ) return;
	m_list->ShowItem(index);
	if (!m_pages[index].visible) { m_pages[index].visible = true; }
}

void wxMetroNotebook::AddScrolledPage(wxWindow *win, const wxString &text, bool active, bool button)
{
	wxScrolledWindow *scrollwin = new wxScrolledWindow( this );
	scrollwin->Show( false );
	win->Reparent( scrollwin );

	int cw, ch;
	win->GetClientSize(&cw,&ch);

	scrollwin->SetScrollbars( 1, 1, cw, ch, 0, 0 );
	scrollwin->SetScrollRate(25,25);
	
	win->Move(0,0);
	
	page_info x;
	x.win = win;
	x.text = text;
	x.scroll_win = scrollwin;
	x.button = button;
	x.visible = true;

	m_pages.push_back( x );

	UpdateTabList();
	UpdateLayout();

	if (active || m_pages.size() == 1)
		SetSelection( m_pages.size()-1 );
}

int wxMetroNotebook::GetPageIndex(wxWindow *win)
{
	if ( win == NULL ) return -1;
	int ndx = -1;
	for (size_t i=0; i<m_pages.size(); i++)
	{
		if ( m_pages[i].win == win )
			ndx = i;
	}
	return ndx;
}


wxWindow *wxMetroNotebook::RemovePage( size_t ndx )
{
	if ( ndx < 0 || ndx >= (int)m_pages.size() )
		return 0;
	
	wxWindow *win = GetPage( ndx );

	int cursel = m_list->GetSelection();
	if ( cursel >= ndx )
	{
		cursel--;
		if ( cursel < 0 )
			cursel = 0;
	}

	if ( m_pages[ndx].scroll_win != 0
		&& win->GetParent() == m_pages[ndx].scroll_win )
	{
		win->SetParent( this );
		m_pages[ndx].scroll_win->Destroy();
		m_pages[ndx].scroll_win = 0;
	}

	m_pages.erase( m_pages.begin() + ndx );
		
	UpdateTabList();

	if ( m_list->GetSelection() != cursel )
		m_list->SetSelection(cursel);

	UpdateLayout();
	return win;
}

void wxMetroNotebook::DeletePage( size_t ndx )
{
	wxWindow *win = RemovePage( ndx );
	if ( win != 0 ) win->Destroy();

}

wxWindow *wxMetroNotebook::GetPage( size_t index )
{
	if ( index < m_pages.size() ) return m_pages[index].win;
	else return 0;
}


int wxMetroNotebook::GetSelection() const 
{
	return m_sel;
}

void wxMetroNotebook::SetText(size_t id, const wxString &text)
{
	if ((id < (int)m_pages.size()) && (id >= 0))
	{
		m_pages[id].text = text;
		UpdateTabList();
	}
}

wxString wxMetroNotebook::GetText( size_t id ) const
{
	if ( id < (int) m_pages.size() && id >= 0 )
		return m_pages[id].text;
	else
		return wxEmptyString;
}

wxString wxMetroNotebook::GetSelectionText() const
{
	return GetText( GetSelection() );
}

wxPoint wxMetroNotebook::GetPopupMenuPosition( int index )
{
	return m_list->GetPopupMenuPosition(index);
}

void wxMetroNotebook::SetSelection(size_t id)
{
	if ( id >= m_pages.size() ) return;


	if ( m_sel == id ) return;

	if ( m_sel >= 0 )
	{
		if ( m_pages[m_sel].scroll_win )
			m_pages[m_sel].scroll_win->Show( false );
		else
			m_pages[m_sel].win->Show( false );
	}

	m_sel = id;
	m_list->SetSelection(id);
	m_list->Refresh();
	
	if ( m_sel >= 0 )
	{
		if ( m_pages[m_sel].scroll_win )
			m_pages[m_sel].scroll_win->Show( true );
		else
			m_pages[m_sel].win->Show( true );
	}

}

size_t wxMetroNotebook::GetPageCount() const
{
	return m_pages.size();
}

void wxMetroNotebook::OnSize( wxSizeEvent &evt )
{
	UpdateLayout();
}

void wxMetroNotebook::UpdateLayout()
{
	int W, H;
	GetClientSize(&W,&H);
	int T = m_list->GetBestSize().y;
	m_list->SetSize(0,0,W,T);
	
	wxRect pgsz( 0, T, W, H-T );	
	for (size_t i=0;i<m_pages.size();i++)
	{
		if ( m_pages[i].scroll_win != 0 )
		{
			m_pages[i].scroll_win->SetSize( pgsz );
			if (wxScrolledWindow *parent = dynamic_cast<wxScrolledWindow*>( GetPage(i) ))
			{
				int cw, ch;
				m_pages[i].scroll_win->GetClientSize(&cw,&ch);
				int vw, vh;
				parent->GetVirtualSize(&vw,&vh);

				if (vw != cw || vh != ch)
				{
					parent->SetScrollbars(1,1, cw, ch);
					parent->SetScrollRate(25,25);
				}

				int x,y;
				m_pages[i].scroll_win->GetPosition(&x,&y);
				if (vw > cw && vh > ch 
					&& (x != 0 || y != 0))
					m_pages[i].scroll_win->Move(0,0);
		
			}
		}
		else
			m_pages[i].win->SetSize( pgsz );
	}
}

void wxMetroNotebook::UpdateTabList()
{
	size_t sel = m_list->GetSelection();
	m_list->Clear();
	for (size_t i=0;i<m_pages.size();i++)
		m_list->Append( m_pages[i].text, m_pages[i].button, m_pages[i].visible );

	m_list->SetSelection( sel );
}

void wxMetroNotebook::OnTabList( wxCommandEvent &evt )
{
	if( evt.GetEventObject() == m_list )
		SwitchPage( evt.GetInt() );
	else
		evt.Skip();
}

void wxMetroNotebook::SwitchPage( size_t i )
{	
	if ( i == GetSelection() ) return;
		
	wxNotebookEvent evt(wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGING, GetId() );
	evt.SetEventObject( this );
	evt.SetOldSelection( GetSelection() );
	evt.SetSelection(i);
	evt.Allow();
	ProcessEvent(evt);

	if ( !evt.IsAllowed() )
	{
		m_list->SetSelection( GetSelection() );
		return; // abort the selection if the changing event was vetoed.
	}

	SetSelection(i);

	// fire EVT_NOTEBOOK_PAGE_CHANGED
	wxNotebookEvent evt2(wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGED, GetId() );
	evt2.SetEventObject( this );
	evt2.SetSelection(i);
	ProcessEvent(evt2);
}


#define SCRL_RATE 25

BEGIN_EVENT_TABLE(wxMetroListBox, wxScrolledWindow)
	EVT_SIZE( wxMetroListBox::OnResize )	
	EVT_LEFT_DOWN( wxMetroListBox::OnLeftDown )
	EVT_LEFT_DCLICK( wxMetroListBox::OnDClick )
	EVT_PAINT( wxMetroListBox::OnPaint )
	EVT_MOTION( wxMetroListBox::OnMouseMove )
	EVT_LEAVE_WINDOW( wxMetroListBox::OnLeave )
	EVT_ERASE_BACKGROUND( wxMetroListBox::OnErase )
END_EVENT_TABLE()

wxMetroListBox::wxMetroListBox(wxWindow *parent, int id, const wxPoint &pos, const wxSize &size)
	: wxScrolledWindow(parent,id, pos, size, wxBORDER_NONE)
{
	m_space = (int)( 10.0 * wxGetScreenHDScale() );
	SetBackgroundStyle(wxBG_STYLE_CUSTOM);
	SetBackgroundColour( *wxWHITE );
	SetFont( wxMetroTheme::Font( wxMT_LIGHT, 15 ) );	
	m_selectedIdx = -1;
	m_hoverIdx = -1;
	
}

wxMetroListBox::~wxMetroListBox()
{
	/* nothing to do */
}


void wxMetroListBox::Add(const wxString &item)
{
	_item x;
	x.name = item;
	m_items.push_back( x );
}

void wxMetroListBox::Add( const wxArrayString &list )
{
	for( size_t i=0;i<list.size();i++ )
		Add( list[i] );
}

void wxMetroListBox::Delete( size_t idx )
{
	if ( idx < m_items.size() )
		m_items.erase( m_items.begin() + idx );
}

int wxMetroListBox::Find(const wxString &item)
{
	for (size_t i=0;i<m_items.size();i++)
		if (m_items[i].name == item)
			return i;
	return -1;
}

wxString wxMetroListBox::Get(size_t idx)
{
	if (idx >= 0 && idx < m_items.size())
		return m_items[idx].name;
	else
		return wxEmptyString;
}

wxString wxMetroListBox::GetValue()
{
	wxString retval;
	if(m_selectedIdx >= 0)
		retval = Get(m_selectedIdx);
	else
		retval = wxEmptyString;

	return retval;
}

void wxMetroListBox::Clear()
{
	m_items.clear();
	Invalidate();
}

int wxMetroListBox::Count()
{
	return m_items.size();
}

void wxMetroListBox::SetSelection(int idx)
{
	m_selectedIdx = idx;
	Refresh();
}

bool wxMetroListBox::SetSelectionString( const wxString &s )
{
	m_selectedIdx = Find( s );
	Refresh();
	return m_selectedIdx >= 0;
}

wxString wxMetroListBox::GetSelectionString()
{
	return GetValue();
}

int wxMetroListBox::GetSelection()
{
	return m_selectedIdx;
}

void wxMetroListBox::Invalidate()
{
	int hpos, vpos;
	GetViewStart( &hpos, &vpos );
	hpos *= SCRL_RATE;
	vpos *= SCRL_RATE;

	wxSize sz = GetClientSize();
	int width,height;
	width=height=0;

	wxClientDC dc( this );
	dc.SetFont( GetFont() );


	int y = 0;
	for (int i=0;i<m_items.size();i++)
	{
		
		int height = dc.GetCharHeight() + m_space;
		m_items[i].geom.x = 0;
		m_items[i].geom.y = y;
		m_items[i].geom.width = sz.GetWidth()+1;
		m_items[i].geom.height = height;
		y += height;
	}

	SetScrollbars(1,1,sz.GetWidth(),y, hpos, vpos);
	SetScrollRate( SCRL_RATE, SCRL_RATE );
	Refresh();
}

void wxMetroListBox::OnResize(wxSizeEvent &evt)
{
	Invalidate();
}



void wxMetroListBox::OnPaint(wxPaintEvent &evt)
{
	wxAutoBufferedPaintDC dc(this);
	DoPrepareDC( dc );
	
	wxColour bg = GetBackgroundColour();
	dc.SetBrush(wxBrush(bg));
	dc.SetPen(wxPen(bg,1));
	wxRect windowRect( wxPoint(0,0), GetClientSize() );
	CalcUnscrolledPosition(windowRect.x, windowRect.y,
		&windowRect.x, &windowRect.y);
	dc.DrawRectangle(windowRect);
	dc.SetFont( GetFont() );
	dc.SetTextForeground( *wxBLACK );
	int height = dc.GetCharHeight();
	for (int i=0;i<m_items.size();i++)	
	{
		wxColour bcol = (m_selectedIdx == i) ? wxColour(50,50,50) :
			( (m_hoverIdx == i) ? wxColour(231,231,231) : GetBackgroundColour() );
		dc.SetPen( wxPen( bcol ) );
		dc.SetBrush( wxBrush( bcol ) );
		dc.DrawRectangle( m_items[i].geom );
		dc.SetTextForeground( (m_selectedIdx==i) ? *wxWHITE : *wxBLACK );
		dc.DrawText( m_items[i].name, m_space/2, m_items[i].geom.y + m_items[i].geom.height/2 - height/2 );
	}			
}

void wxMetroListBox::OnErase( wxEraseEvent & )
{
	/* nothing to do */
}

void wxMetroListBox::OnLeftDown(wxMouseEvent &evt)
{
	int vsx, vsy;
	GetViewStart( &vsx, &vsy );
	vsx *= SCRL_RATE;
	vsy *= SCRL_RATE;

	SetFocus();

	for (int i=0;i<m_items.size();i++)
	{
		if (evt.GetY()+vsy > m_items[i].geom.y 
			&& evt.GetY()+vsy < m_items[i].geom.y+m_items[i].geom.height )
		{
			
			m_selectedIdx = i;
			Refresh();
				
			wxCommandEvent selevt(wxEVT_COMMAND_LISTBOX_SELECTED, this->GetId() );
			selevt.SetEventObject(this);
			selevt.SetInt(i);
			selevt.SetString(GetValue());
			GetEventHandler()->ProcessEvent(selevt);
			return;
		}
	}

	m_selectedIdx = -1;
	Refresh();
}

void wxMetroListBox::OnDClick( wxMouseEvent &evt )
{
	wxCommandEvent selevt(wxEVT_COMMAND_LISTBOX_DOUBLECLICKED, this->GetId() );
	selevt.SetEventObject(this);
	selevt.SetInt( m_selectedIdx );
	selevt.SetString(GetValue());
	GetEventHandler()->ProcessEvent(selevt);
}

void wxMetroListBox::OnMouseMove(wxMouseEvent &evt)
{	
	int vsx, vsy;
	GetViewStart( &vsx, &vsy );
	vsx *= SCRL_RATE;
	vsy *= SCRL_RATE;

	for (int i=0;i<m_items.size();i++)
	{
		if (evt.GetY()+vsy > m_items[i].geom.y 
			&& evt.GetY()+vsy < m_items[i].geom.y+m_items[i].geom.height )
		{
			
			if ( m_hoverIdx != i )
			{
				m_hoverIdx = i;
				Refresh();
			}

			return;
		}
	}

	if ( m_hoverIdx != -1 )
	{
		m_hoverIdx = -1;
		Refresh();
	}
}

void wxMetroListBox::OnLeave(wxMouseEvent &evt)
{
	m_hoverIdx = -1;
	Refresh();
}


class wxMetroPopupMenuWindow : public wxPopupWindow
{
	int m_border, m_space, m_checkSpace, m_checkHeight, m_checkWidth;

	struct item {
		int id;
		wxString label;
		bool is_checkItem;
		bool checked;
		int ymin, ymax;
	};
	int m_hover;
	std::vector<item> m_items;
	long m_theme; 
	bool m_hasCheckItems;
public:
	wxMetroPopupMenuWindow( wxWindow *parent, long theme = 0)
		: wxPopupWindow( parent, wxBORDER_NONE|wxWANTS_CHARS ),
		m_theme( theme )
	{
		double sf = wxGetScreenHDScale();

		m_border = (int)(1.0*sf);
		m_space = (int)(5.0*sf);
		m_checkSpace = (int)(14.0*sf);
		m_checkHeight = (int)(14.0*sf);
		m_checkWidth = (int)(10.0*sf);
		
		m_hover = -1;
		SetBackgroundStyle(wxBG_STYLE_CUSTOM);
	}
	

	void Append( int id, const wxString &label, bool isCheck, bool checked )
	{
		item x;
		x.id = id;
		x.label = label;
	#ifdef __WXOSX__
		x.label.Replace( wxT("\tCtrl-"), wxT("\t\x2318") );
	#endif
		x.ymin = x.ymax = 0;
		x.is_checkItem = isCheck;
		x.checked = checked;
		m_items.push_back( x );

		if ( isCheck ) m_hasCheckItems = true;

		InvalidateBestSize();
	}

	void AppendSeparator()
	{
		Append( -1, wxEmptyString, false, false );
	}

	virtual wxSize DoGetBestSize() const
	{
		wxClientDC dc( const_cast<wxMetroPopupMenuWindow*>(this) );
		dc.SetFont( GetFont() );
		int ch = dc.GetCharHeight();
		int wl = 0, wr = 0;
		int h = m_border;
		for( size_t i=0;i<m_items.size();i++ )
		{
			if ( m_items[i].label.IsEmpty() )
				h += m_border;
			else
			{
				int tpos = m_items[i].label.Find( '\t' );
				if ( tpos != wxNOT_FOUND )
				{
					wxString l = m_items[i].label.Left(tpos);
					wxString r = m_items[i].label.Mid(tpos+1);

					wxSize szl = dc.GetTextExtent( l );
					if ( wl < szl.x ) wl = szl.x;

					wxSize szr = dc.GetTextExtent( r );
					if ( wr < szr.x ) wr = szr.x;
				}
				else
				{
					wxSize sz = dc.GetTextExtent( m_items[i].label );
					if ( wl < sz.x ) wl = sz.x;
				}

				h += m_space + ch;
			}
		}
	
		wl += 4*m_space;
		if ( m_hasCheckItems ) wl += m_checkSpace;

		if ( wr > 0 ) wr += 3*m_space;

		return wxSize( wl + wr, h+m_border );
	}
	


	void OnPaint( wxPaintEvent & )
	{
		wxAutoBufferedPaintDC dc( this );
		dc.SetFont( GetFont() );
		int ch = dc.GetCharHeight();
		wxSize sz = GetClientSize();

		wxColour acc = m_theme&wxMT_LIGHTTHEME ? *wxWHITE : wxMetroTheme::Colour( wxMT_HOVER );
		wxColour text = m_theme&wxMT_LIGHTTHEME ? wxMetroTheme::Colour(wxMT_HOVER) : *wxWHITE;
		wxColour acchi = m_theme&wxMT_LIGHTTHEME ? text : *wxWHITE;
		wxColour texthi = m_theme&wxMT_LIGHTTHEME ? *wxWHITE : acc;

		dc.SetBrush(  wxBrush(acc) );
		dc.SetPen( wxPen( wxMetroTheme::Colour( wxMT_HOVER ), 1 ) );
		dc.DrawRectangle( wxRect(0,0,sz.x,sz.y) );

		int uh = ch + m_space;
		int x0 = m_hasCheckItems ? m_checkSpace : 0;
		
		dc.SetPen( wxPen( text, 1) );

		size_t yy = m_border;
		for ( size_t i=0;i<m_items.size();i++ )
		{
			m_items[i].ymin = yy;

			if ( !m_items[i].label.IsEmpty() )
			{
				if ( m_hover == (int)i )
				{
					dc.SetTextForeground( texthi );
					dc.SetBrush( wxBrush( acchi ) );
					dc.DrawRectangle( 0, yy, sz.x, ch + m_space );
				}
				else
					dc.SetTextForeground( text );

				int texty = yy + uh/2 - ch/2;

				int tpos = m_items[i].label.Find( '\t' );
				if ( tpos != wxNOT_FOUND )
				{
					wxString l = m_items[i].label.Left(tpos);
					wxString r = m_items[i].label.Mid(tpos+1);

					dc.DrawText( l, x0+m_border+m_space, texty );
					dc.SetTextForeground( *wxLIGHT_GREY );
					dc.DrawText( r, sz.x - m_border - m_space - dc.GetTextExtent(r).x, texty );
				}
				else
				{
					dc.DrawText( m_items[i].label, x0+m_border+m_space, texty );			
				}

				if ( m_items[i].is_checkItem )
				{
					wxColour chkcol( m_hover == (int)i ? acc : acchi );
					if( m_items[i].checked ) dc.SetBrush( wxBrush( chkcol ) );
					else dc.SetBrush( *wxTRANSPARENT_BRUSH );

					dc.SetPen( wxPen( chkcol, 1 ) );
					dc.DrawRectangle( 4, yy + uh/2 - m_checkHeight/2, m_checkWidth, m_checkHeight );
				}

				yy += uh;
			}
			else
			{
				dc.DrawLine( 0, yy, sz.x, yy );
				yy += m_border;
			}

			m_items[i].ymax = yy;
		}

	}


	void OnErase( wxEraseEvent & )
	{
		/* nothing to do */
	}
	

	int Current( const wxPoint &mouse )
	{
		if ( mouse.x < 0 || mouse.x > GetClientSize().x ) return -1;

		for( size_t i=0;i<m_items.size();i++ )
			if ( mouse.y >= m_items[i].ymin
				&& mouse.y < m_items[i].ymax )
				return (int) i;

		return -1;
	}

	void OnMouse( wxMouseEvent &evt )
	{
		wxEventType et = evt.GetEventType();
		bool inside = HitTest( evt.GetPosition() ) == wxHT_WINDOW_INSIDE;

		if ( ( et == wxEVT_MOTION && inside )
			|| et == wxEVT_LEAVE_WINDOW )
		{
			
			int hh = Current( evt.GetPosition() );
			if ( hh != m_hover )
			{
				m_hover = hh;
				Refresh();
			}

			return;
		}

		if ( evt.GetEventType() == wxEVT_LEFT_UP && inside )
		{
			int sel = Current( evt.GetPosition() );
			if ( sel >= 0 )
			{
				wxCommandEvent ee( wxEVT_MENU );
				ee.SetId( m_items[sel].id );
				AddPendingEvent( ee );
				Dismiss();
				return;
			}
		}
		

		if ( !inside && (et == wxEVT_LEFT_DCLICK
			|| et == wxEVT_LEFT_DOWN
			|| et == wxEVT_RIGHT_DCLICK
			|| et == wxEVT_RIGHT_DOWN) )
			Dismiss();
	}
	

	void Popup( const wxPoint &rpos, int origin )
	{
		wxSize size = GetBestSize();
		SetClientSize( size );
		wxPoint pos( rpos == wxDefaultPosition ? wxGetMousePosition() : rpos );
		if ( origin & wxRIGHT ) pos.x -= size.x;
		if ( origin & wxBOTTOM ) pos.y -= size.y;
		Position( pos, wxSize(0,0) );
		Show( true );
		SetFocus();
		if ( !HasCapture() )
			CaptureMouse();
	}

	virtual bool Destroy()
	{	
		wxCHECK_MSG( !wxPendingDelete.Member(this), false,
					 wxS("Shouldn't destroy the popup twice.") );

		wxPendingDelete.Append(this);
		return true;
	}
	
	void Dismiss()
	{
		if ( HasCapture() )
			ReleaseMouse();

		Hide();
		Destroy();
	}

	void OnLoseFocus( wxFocusEvent & )
	{
		Dismiss();
	}

	void OnCaptureLost( wxMouseCaptureLostEvent & )
	{
		Dismiss();
	}

	void OnChar( wxKeyEvent & )
	{
		Dismiss();
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( wxMetroPopupMenuWindow, wxPopupWindow )
	EVT_PAINT( wxMetroPopupMenuWindow::OnPaint )
	EVT_ERASE_BACKGROUND( wxMetroPopupMenuWindow::OnErase )
	EVT_MOUSE_EVENTS( wxMetroPopupMenuWindow::OnMouse )
	EVT_KILL_FOCUS( wxMetroPopupMenuWindow::OnLoseFocus )
	EVT_MOUSE_CAPTURE_LOST( wxMetroPopupMenuWindow::OnCaptureLost )
	EVT_CHAR( wxMetroPopupMenuWindow::OnChar )
END_EVENT_TABLE()

wxMetroPopupMenu::wxMetroPopupMenu( long theme ) : m_theme(theme)
{
	m_font = wxMetroTheme::Font( wxMT_NORMAL, 13 );
} 

void wxMetroPopupMenu::SetFont( const wxFont &f )
{
	m_font = f;
}

void wxMetroPopupMenu::Append( int id, const wxString &label )
{
	item x;
	x.id = id;
	x.label = label;
	x.is_checkItem = false;
	x.checked = false;
	m_items.push_back( x );
}

void wxMetroPopupMenu::AppendCheckItem( int id, const wxString &label, bool checked )
{
	item x;
	x.id = id;
	x.label = label;
	x.is_checkItem = true;
	x.checked = checked;
	m_items.push_back( x );
}

void wxMetroPopupMenu::AppendSeparator()
{
	Append( -1, wxEmptyString );
}

void wxMetroPopupMenu::Popup( wxWindow *parent, const wxPoint &pos, int origin )
{
	// menu window will Destroy() itself after it is dismissed
	wxMetroPopupMenuWindow *menu = new wxMetroPopupMenuWindow( parent, m_theme );
	menu->SetFont( m_font );
	for( size_t i=0;i<m_items.size();i++ )
		menu->Append( m_items[i].id, m_items[i].label, m_items[i].is_checkItem, m_items[i].checked );
	menu->Popup( pos, origin );
}

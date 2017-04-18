#include <wx/dcbuffer.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/stattext.h>
#include <wx/sizer.h>

#include <wex/snaplay.h>
#include <wex/metro.h>
#include <wex/utils.h>

#include <algorithm>

class wxSnapLayout::OverlayWindow : public wxFrame
{
	wxStaticText *m_label;
public:
	OverlayWindow( wxWindow *parent, const wxPoint &pos, const wxSize &size )
		: wxFrame( parent, wxID_ANY, wxEmptyString,  pos, size, 
			wxBORDER_NONE | wxFRAME_FLOAT_ON_PARENT | wxFRAME_NO_TASKBAR )
	{
		SetBackgroundColour( *wxLIGHT_GREY );
		SetTransparent( 200 );

		m_label = new wxStaticText( this, wxID_ANY, wxEmptyString );
		m_label->SetFont( wxMetroTheme::Font( wxMT_NORMAL, 16 ) );
		m_label->SetForegroundColour( *wxWHITE );
		
		wxBoxSizer *sizer = new wxBoxSizer( wxHORIZONTAL );
		sizer->AddStretchSpacer();
		sizer->Add( m_label, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
		sizer->AddStretchSpacer();
		SetSizer( sizer );
	}

	void SetLabel( const wxString &s )
	{
		if ( m_label )	m_label->SetLabel( s );
	}
};


BEGIN_EVENT_TABLE( wxSnapLayout, wxScrolledWindow )
	EVT_SIZE( wxSnapLayout::OnSize )
	EVT_ERASE_BACKGROUND( wxSnapLayout::OnErase )
	EVT_PAINT( wxSnapLayout::OnPaint )
	EVT_LEFT_DOWN( wxSnapLayout::OnLeftDown )
	EVT_LEFT_UP( wxSnapLayout::OnLeftUp )
	EVT_MOTION( wxSnapLayout::OnMotion )
	EVT_MOUSE_CAPTURE_LOST( wxSnapLayout::OnCaptureLost )
	EVT_LEAVE_WINDOW( wxSnapLayout::OnLeaveWindow )
END_EVENT_TABLE()

wxSnapLayout::wxSnapLayout( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size )
	: wxScrolledWindow( parent, id, pos, size, wxHSCROLL|wxVSCROLL|wxCLIP_CHILDREN )
{
	SetBackgroundStyle( wxBG_STYLE_PAINT );
	m_active = 0;
	m_handle = -1;
	m_curtarget = 0;
	m_transp = 0;
	m_sizeHover = -1;
	m_moveHover = -1;
	m_showSizing = false;
	m_space = (int)(15.0 * wxGetScreenHDScale());
	m_scrollRate = 1;
}

wxSnapLayout::~wxSnapLayout()
{
	for( size_t i=0;i<m_list.size();i++ )
		delete m_list[i];
}
	
void wxSnapLayout::Add( wxWindow *win, int width, int height )
{
	if ( Find( win ) >= 0 ) return;

	if ( win->GetParent() != this )
		win->Reparent( this );

	layout_box *l = new layout_box;
	l->win = win;
	l->req.x = width;
	l->req.y = height;
	l->rect = wxScaleRect( 0, 0, 500, 300 );
	l->highlight = false;
	m_list.push_back( l );

	AutoLayout();
}

void wxSnapLayout::ClearHighlights()
{
	for( size_t i=0;i<m_list.size();i++ )
		m_list[i]->highlight = false;

	Refresh();
}

void wxSnapLayout::Highlight( wxWindow *w )
{
	int i = Find( w );
	if ( i >= 0 )
	{
		m_list[i]->highlight = true;
		Refresh();
	}
}

void wxSnapLayout::Delete( wxWindow *w )
{
	int i = Find( w );
	if ( i >= 0 )
	{
		w->Destroy();
		delete m_list[i];
		m_list.erase( m_list.begin()+i );
		AutoLayout();
	}
}

void wxSnapLayout::SetBackgroundText( const wxString &text )
{
	m_backgroundText = text;
	Refresh();
}

int wxSnapLayout::Find( wxWindow *w )
{
	for( size_t i=0;i<m_list.size();i++ )
		if ( m_list[i]->win == w )
			return i;

	return -1;
}

size_t wxSnapLayout::Count()
{
	return m_list.size();
}

wxWindow *wxSnapLayout::Get( size_t i )
{
	if ( i < m_list.size() ) return m_list[i]->win;
	else return 0;
}

std::vector<wxWindow*> wxSnapLayout::Windows()
{
	std::vector<wxWindow*> wl;
	for( size_t i=0;i<m_list.size();i++ )
		wl.push_back( m_list[i]->win );
	return wl;
}

void wxSnapLayout::DeleteAll()
{
	for( size_t i=0;i<m_list.size();i++ )
	{
		m_list[i]->win->Destroy();
		delete m_list[i];
	}
	m_list.clear();
	Refresh();
}
void wxSnapLayout::ScrollTo( wxWindow *w )
{
	int i = Find( w );
	if ( i >= 0 )
		Scroll( m_list[i]->rect.x, m_list[i]->rect.y );
}

bool wxSnapLayout::CanPlace( size_t idx, int width, int height, int xplace, int yplace, int client_width )
{
	wxRect proposed( xplace, yplace, width-1, height-1 );
	for( size_t i=0;i<idx;i++ )
		if ( m_list[i]->active.Intersects( proposed ) )
			return false;

	
	return ( proposed.x >= 0 && proposed.x + proposed.width < client_width );
}

void wxSnapLayout::Place( size_t icur, int cwidth )
{	
	layout_box &l = *m_list[icur];
	wxSize sz( l.req.x <= 0 || l.req.y <= 0 ? l.win->GetBestSize() : l.req );
	l.active.width = sz.x + m_space + m_space;
	l.active.height = sz.y + m_space + m_space;
	
	int xx = 0, yy = 0;
	if ( icur > 0 )
	{
		wxPoint best(0,99999);
		bool placed = false;
		for( size_t i=0;i<icur;i++ )
		{
			// can place below or to the right of the i'th widget?
			wxPoint try1( m_list[i]->active.x + m_list[i]->active.width, m_list[i]->active.y );
			if( CanPlace( icur, l.active.width, l.active.height, try1.x, try1.y, cwidth ) )
			{
				if ( !placed || try1.y < best.y )
				{
					best = try1;
					placed = true;
				}
			}
			
			/*
			wxPoint try2( m_list[i]->active.x, m_list[i]->active.y+m_list[i]->active.height );
			if( CanPlace( icur, l.active.width, l.active.height, try2.x, try2.y, cwidth ) )
			{
				if ( !placed || try2.y < best.y )
				{
					best = try2;
					placed = true;
				}
			}
			*/
			/*
			wxPoint try3( m_list[i]->active.x+m_list[i]->active.width-l.active.width, m_list[i]->active.y+m_list[i]->active.height );
			if ( CanPlace( icur, l.active.width, l.active.height, try3.x, try3.y, cwidth ) )
			{
				if ( !placed || try3.y < best.y )
				{
					best = try3;
					placed = true;
				}
			}
			*/
		}


		// if we still haven't found a place, start a new row
		// at the current y maximum
		if ( !placed )
		{
			xx = 0;
			yy = 0;
			for( size_t i=0;i<icur;i++ )
				if ( m_list[i]->active.y+m_list[i]->active.height > yy )
					yy = m_list[i]->active.y+m_list[i]->active.height;

		}
		else
		{
			xx = best.x;
			yy = best.y;
		}
		
	}

	l.active.x = xx;
	l.active.y = yy;
}

void wxSnapLayout::AutoLayout2()
{
	// clear all the drop targets
	m_targets.clear();

	// get current scroll position, window size
	int vsx, vsy;
	GetViewStart( &vsx, &vsy );
	vsx *= m_scrollRate;
	vsy *= m_scrollRate;
	wxSize client( GetClientSize() );

	// place all the widgets
	for( size_t i=0;i<m_list.size();i++ )
		Place( i, client.x );

	// setup widget rects and move all of them into place
	for( size_t i=0;i<m_list.size();i++ )
	{
		layout_box &l = *m_list[i];

		l.rect.x = l.active.x + m_space;
		l.rect.y = l.active.y + m_space;
		l.rect.width = l.active.width - m_space - m_space;
		l.rect.height = l.active.height - m_space - m_space;

		l.move_box[0] = wxRect( l.rect.x, l.active.y, l.rect.width, m_space-1 );
		l.move_box[1] = wxRect( l.active.x, l.rect.y, m_space-1, l.rect.height );
		l.move_box[2] = wxRect( l.active.x+l.active.width-m_space+1, l.rect.y, m_space-1, l.rect.height );
		l.move_box[3] = wxRect( l.rect.x, l.active.y+l.active.height-m_space+1, l.rect.width, m_space-1 );

		l.size_nw = wxRect( l.active.x+1, l.active.y+1, m_space-2, m_space-2 );
		l.size_sw = wxRect( l.active.x+1, l.active.y+l.active.height-m_space+1, m_space-2, m_space-2 );
		l.size_ne = wxRect( l.active.x+l.active.width-m_space+1, l.active.y+1, m_space-2, m_space-2 );
		l.size_se = wxRect( l.active.x+l.active.width-m_space+1, l.active.y+l.active.height-m_space+1, m_space-2, m_space-2 );

		l.win->SetSize( l.rect.x-vsx,
			l.rect.y-vsy,
			l.rect.width,
			l.rect.height );
				
		drop_target dt;
		dt.index = i;
		dt.target.x = l.active.x - m_space;
		dt.target.y = l.active.y;
		dt.target.width = m_space+m_space;
		dt.target.height = l.active.height;
		m_targets.push_back( dt );

	}


	if ( m_list.size() > 0 )
	{
		size_t last = m_list.size()-1;
		drop_target dt;
		dt.index = m_list.size();
		dt.target.x = m_list[last]->active.x+m_list[last]->active.width-m_space;
		dt.target.y = m_list[last]->active.y;
		dt.target.width = m_space+m_space;
		dt.target.height = m_list[last]->active.height;
		m_targets.push_back( dt );
	}

	// find the max extents and setup the scrollbars
	size_t ymax = 0;
	for( size_t i=0;i<m_list.size();i++ )
		if ( m_list[i]->active.y + m_list[i]->active.height > ymax )
			ymax = m_list[i]->active.y + m_list[i]->active.height;
		
	SetScrollbars( 1, 1, client.x, ymax, vsx/m_scrollRate, vsy/m_scrollRate );
	SetScrollRate( m_scrollRate, m_scrollRate );
}

void wxSnapLayout::AutoLayout()
{
	AutoLayout2();
	return;

	m_targets.clear();

	if ( m_list.size() == 0 ) return;

	wxPoint vs( GetViewStart() );
	wxSize client( GetClientSize() );

	int x = 0;
	int y = 0;

	int row_min_height = 10000;
	int row_max_height = 0;
	
	size_t nn = m_list.size();
	
	int irow = 0;
	int icol = 0;
	int idx = 0;
	while( idx < m_list.size() )
	{
		layout_box &l = *m_list[idx];
		wxSize sz( l.req.x <= 0 || l.req.y <= 0 ? l.win->GetBestSize() : l.req );

		// determine if this widget goes in this row

		l.active.width = sz.x + m_space + m_space;
		l.active.height = sz.y + m_space + m_space;

		bool row_shift = false;

		if ( x + l.active.width < client.x || idx == 0 )
		{
			// widget fits in this row
			l.active.x = x;
			l.active.y = y;
			x += l.active.width;
		}
		else
		{
			irow++;
			icol = 0;
			row_shift = true;

			// try to find a place in the next row

			// shift down to the next row
			y += row_max_height + m_space;
			x = 0;

			l.active.x = x;
			l.active.y = y;

			// reset row height trackers
			row_max_height = 0;
			row_min_height = 10000;
		}

		if ( l.active.height < row_min_height ) row_min_height = l.active.height;
		if ( l.active.height > row_max_height ) row_max_height = l.active.height;
		
		l.rect.x = l.active.x+m_space;
		l.rect.y = l.active.y+m_space;
		l.rect.width = sz.x;
		l.rect.height = sz.y;
		
		l.win->Move( CalcScrolledPosition( wxPoint( l.rect.x, l.rect.y ) ) );
		l.win->SetClientSize( l.rect.width, l.rect.height );

		drop_target dt;
		dt.index = idx;
		dt.target.x = l.active.x - m_space;
		dt.target.y = l.active.y;
		dt.target.width = m_space+m_space;
		dt.target.height = l.active.height;
		m_targets.push_back( dt );


		icol++;

		if ( row_shift )
			x += l.active.width;
		
		idx++;
	}
	
	
	SetScrollbars( 1, 1, client.x, y+row_max_height, vs.x, vs.y );
	SetScrollRate( 30, 30 );
}

wxSnapLayout::layout_box *wxSnapLayout::CheckActive( const wxPoint &p, int *handle )
{
	*handle = -1;
	for( size_t i=0;i<m_list.size();i++ )
	{
		layout_box &l = *m_list[i];
		if ( l.active.Contains( p ) )
		{
			if ( l.size_ne.Contains( p ) ) *handle = NE;
			else if ( l.size_nw.Contains( p ) ) *handle = NW;
			else if ( l.size_se.Contains( p ) ) *handle = SE;
			else if ( l.size_sw.Contains( p ) ) *handle = SW;
			return m_list[i];
		}
	}

	return 0;
}

void wxSnapLayout::OnLeftDown( wxMouseEvent &evt )
{
	int vsx, vsy;
	GetViewStart(&vsx,&vsy);
	int mx = m_scrollRate*vsx+evt.GetX();
	int my = m_scrollRate*vsy+evt.GetY();

	m_active = CheckActive( wxPoint(mx,my), &m_handle );
	Refresh();
	
	if ( m_active )
	{
		switch( m_handle )
		{
		case SE: case NW: SetCursor( wxCURSOR_SIZENWSE ); break;
		case SW: case NE: SetCursor( wxCURSOR_SIZENESW ); break;
		default: SetCursor( wxCURSOR_DEFAULT );
		}
	}
	else SetCursor( wxCURSOR_DEFAULT );

	m_orig = ClientToScreen( wxPoint(mx,my) );

	if( m_active && m_handle >= 0 )
		ShowTransparency( m_active->rect );

	if ( m_active != 0 && !HasCapture() )
		CaptureMouse();
}

void wxSnapLayout::OnLeaveWindow( wxMouseEvent &evt )
{
	if ( m_sizeHover >= 0 || m_moveHover >= 0 )
	{
		m_sizeHover = -1;
		m_moveHover = -1;
		Refresh();
	}
}

void wxSnapLayout::OnLeftUp( wxMouseEvent &evt )
{
	if ( HasCapture() )
		ReleaseMouse();

	if ( m_active != 0 && m_handle >= 0 )
	{

		m_active->req.x = m_sizerect.width;
		m_active->req.y = m_sizerect.height;

		AutoLayout();
	}
	else if ( m_active != 0 && m_handle < 0 && m_curtarget != 0 )
	{
		std::vector<layout_box*>::iterator itmoving = std::find( m_list.begin(), m_list.end(), m_active );
		int imoving = Find( m_active->win );
		int itarget = m_curtarget->index;
		if ( itarget != imoving
			&& itarget != imoving + 1 
			&& itmoving != m_list.end() )
		{
			if ( itarget > imoving ) itarget--;

			m_list.erase( itmoving );
			if ( itarget < 0 ) itarget = 0;
			m_list.insert( m_list.begin()+itarget, m_active );

			AutoLayout();
			
		}
	}


	m_active = 0;
	m_curtarget = 0;

	SetCursor( wxCURSOR_DEFAULT );
	Refresh();
	HideTransparency();
}

void wxSnapLayout::OnMotion( wxMouseEvent &evt )
{
	int vsx, vsy;
	GetViewStart(&vsx,&vsy);
	int mx = m_scrollRate*vsx+evt.GetX();
	int my = m_scrollRate*vsy+evt.GetY();
	wxPoint mpos(mx,my);

	wxPoint diff = ClientToScreen( mpos ) - m_orig;
	
	if ( m_active && m_handle < 0 )
	{
		drop_target *last = m_curtarget;
		m_curtarget = 0;
		for( size_t i=0;i<m_targets.size();i++ )
			if ( m_targets[i].target.Contains( mpos ) )
				m_curtarget = &m_targets[i];

		wxRect r( m_active->rect );
		r.x += diff.x;
		r.y += diff.y;
		ShowTransparency( r );

		if ( last != m_curtarget )
			Refresh();
	}
	else if ( m_active && m_handle >= 0 )
	{		

		wxRect r( m_active->rect );
		switch( m_handle )
		{
		case NW:
			r.x += diff.x;
			r.y += diff.y;
			r.width -= diff.x;
			r.height -= diff.y;
			break;
		case NE:
			r.y += diff.y;
			r.width += diff.x;
			r.height -= diff.y;
			break;
		case SW:
			r.x += diff.x;
			r.width -= diff.x;
			r.height += diff.y;
			break;
		case SE:
			r.width += diff.x;
			r.height += diff.y;
			break;
		}


		if( r.width < 10 ) r.width = 10;
		if( r.height < 10 ) r.height = 10;

		m_sizerect = r;

		ShowTransparency( r );
	}

	if ( !m_active )
	{
		int oldmovehov = m_moveHover;
		int oldsizehov = m_sizeHover;
		m_moveHover = -1;
		m_sizeHover = -1;
		wxCursor curs(wxCURSOR_DEFAULT);
		for( size_t i=0;i<m_list.size();i++ )
		{
			if ( m_list[i]->size_ne.Contains( mpos ) 
				|| m_list[i]->size_sw.Contains( mpos ) )
			{
				curs = wxCURSOR_SIZENESW;
				m_sizeHover = i;
				break;
			}

			if ( m_list[i]->size_nw.Contains( mpos ) 
				|| m_list[i]->size_se.Contains( mpos ) )
			{
				curs = wxCURSOR_SIZENWSE;
				m_sizeHover = i;
				break;
			}

			for (size_t k=0;k<4;k++ )
			{
				if ( m_list[i]->move_box[k].Contains( mpos ) )
				{
					curs = wxCURSOR_SIZING;
					m_moveHover = i;
					break;
				}
			}

			if( m_moveHover >= 0 )
				break;

		}

		if ( oldsizehov != m_sizeHover 
			|| oldmovehov != m_moveHover )
		{
			SetCursor( curs );
			Refresh();
		}
	}
}

void wxSnapLayout::OnCaptureLost( wxMouseCaptureLostEvent & )
{
	m_active = 0;
	m_handle = -1;
	Refresh();
	HideTransparency();
}

void wxSnapLayout::OnSize( wxSizeEvent & )
{
	AutoLayout();
}

void wxSnapLayout::OnErase( wxEraseEvent & )
{
	/* nothing to do */
}

void wxSnapLayout::OnPaint( wxPaintEvent & )
{
	wxAutoBufferedPaintDC dc( this );
	DoPrepareDC(dc);

	wxSize client(GetClientSize());

	dc.SetBackground( *wxWHITE_BRUSH );
	dc.Clear();

	if ( !m_backgroundText.IsEmpty() )
	{
		dc.SetFont( wxMetroTheme::Font( wxMT_LIGHT, 22 ) );
		wxSize size( dc.GetTextExtent( m_backgroundText ) );
		dc.SetTextForeground( *wxLIGHT_GREY );
		dc.DrawText( m_backgroundText, client.x/2-size.x/2, client.y/2-size.y/2 );
	}


	/*
		
	dc.SetBrush( *wxTRANSPARENT_BRUSH );
	dc.SetPen( *wxBLUE_PEN );

	for( size_t i=0;i<m_list.size();i++)
	{
		dc.DrawRectangle( m_list[i]->active );
		dc.DrawText( wxString::Format( "(%d) [ %d x %d ]", (int)i, 
			m_list[i]->rect.width, m_list[i]->rect.height ), 
			m_list[i]->active.x+1, m_list[i]->active.y+1);
	}
	
	*/
	
	dc.SetPen( *wxTRANSPARENT_PEN );

	dc.SetBrush( wxMetroTheme::Colour( wxMT_SELECT ) );
	for( size_t i=0;i<m_list.size();i++)
		if (m_list[i]->highlight ) dc.DrawRectangle( m_list[i]->active );

	if ( m_active != 0 && m_handle < 0 )
	{
		dc.SetBrush( wxBrush( wxMetroTheme::Colour( wxMT_HIGHLIGHT ), wxSOLID ) );
		for( size_t i=0;i<m_targets.size();i++ )
			dc.DrawRectangle( m_targets[i].target );
	}

	if ( m_curtarget )
	{
		dc.SetBrush( wxBrush( wxMetroTheme::Colour( wxMT_DIMHOVER ) ) );
		dc.DrawRectangle( m_curtarget->target );
	}

	dc.SetBrush( wxBrush( wxColour(230,230,230), wxSOLID ) );

	if ( m_sizeHover >= 0 )
	{
		dc.DrawRectangle( m_list[m_sizeHover]->size_ne );
		dc.DrawRectangle( m_list[m_sizeHover]->size_nw );
		dc.DrawRectangle( m_list[m_sizeHover]->size_se );
		dc.DrawRectangle( m_list[m_sizeHover]->size_sw );
	}

	if ( m_moveHover >= 0 )
	{
		for( size_t i=0;i<4;i++ )
			dc.DrawRectangle( m_list[m_moveHover]->move_box[i] );
	}
}

void wxSnapLayout::ShowTransparency( wxRect r )
{
	int vsx, vsy;
	GetViewStart(&vsx,&vsy);
	vsx *= m_scrollRate;
	vsy *= m_scrollRate;

	wxPoint pos = ClientToScreen( wxPoint(r.x, r.y) );
	pos.x -= vsx;
	pos.y -= vsy;
	wxSize size(r.width, r.height);

	if ( m_transp == 0 )
	{
		m_transp = new OverlayWindow( this, pos, size );		
		
		if ( m_showSizing && m_handle >= 0 )
			m_transp->SetLabel( wxString::Format("%d x %d", size.x, size.y ) );
	}
	else
	{
		m_transp->Move( pos );
		m_transp->SetClientSize( size );

		if ( m_showSizing && m_handle >= 0 )
			m_transp->SetLabel( wxString::Format("%d x %d", size.x, size.y ) );
	}


	m_transp->ShowWithoutActivating();
}

void wxSnapLayout::HideTransparency()
{
	if ( m_transp )
	{
		m_transp->Hide();
		m_transp = 0;
	}
}

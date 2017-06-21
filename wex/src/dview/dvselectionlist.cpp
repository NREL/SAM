#include <wx/dcbuffer.h>

#include "wex/utils.h"

#include "wex/dview/dvselectionlist.h"

#include "wex/icons/cirplus_12.cpng"
#include "wex/icons/cirminus_12.cpng"

enum { ID_popup_first = wxID_HIGHEST+941,
	ID_EXPAND_ALL, ID_EXPAND_SELECTIONS, ID_COLLAPSE_ALL,
	ID_popup_last };

#define SCRL_RATE 10


DEFINE_EVENT_TYPE(wxEVT_DVSELECTIONLIST)
	
BEGIN_EVENT_TABLE( wxDVSelectionListCtrl, wxScrolledWindow )
	EVT_SIZE(              wxDVSelectionListCtrl::OnResize    )
	EVT_ERASE_BACKGROUND(  wxDVSelectionListCtrl::OnErase     )
	EVT_PAINT(             wxDVSelectionListCtrl::OnPaint     )
	EVT_LEFT_DOWN(         wxDVSelectionListCtrl::OnLeftDown  )
	EVT_RIGHT_DOWN(        wxDVSelectionListCtrl::OnRightDown )
	EVT_MOTION(            wxDVSelectionListCtrl::OnMouseMove )
	EVT_LEAVE_WINDOW(      wxDVSelectionListCtrl::OnLeave     )
	EVT_MENU_RANGE( ID_popup_first, ID_popup_last, wxDVSelectionListCtrl::OnPopupMenu )
END_EVENT_TABLE()

wxDVSelectionListCtrl::wxDVSelectionListCtrl( wxWindow* parent, wxWindowID id, 
	int num_cols,
	const wxPoint& pos, const wxSize& size,
	unsigned long style )
	: wxScrolledWindow( parent, id, pos, size, wxCLIP_CHILDREN|wxBORDER_NONE ),
		m_style( style ),
		m_ungroupedLabel( "Others" )
{
	SetBackgroundStyle(::wxBG_STYLE_CUSTOM );
		
	if (num_cols < 1) num_cols = 1;
	if (num_cols >= NMAXCOLS) num_cols = NMAXCOLS-1;

	m_numCols = num_cols;

	m_lastEventRow = 0;
	m_lastEventCol = 0;
	m_lastEventValue = false;

	double xS, yS;
	wxDevicePPIToScale( wxClientDC(this).GetPPI(), &xS, &yS );

	m_itemHeight = (int)(18*yS);
	m_groupHeight = (int)(20*yS);
	m_boxSize = (int)(11*xS);
	m_xOffset = 6;

	m_bestSize = wxSize(150, 500); // updated by Invalidate()
}

wxDVSelectionListCtrl::~wxDVSelectionListCtrl()
{
	FreeRowItems();
}

void wxDVSelectionListCtrl::FreeRowItems()
{
	for ( std::vector<row_item*>::iterator it = m_itemList.begin();
		it != m_itemList.end();
		++it )
		delete *it;

	m_itemList.clear();
	m_groups.clear();
}

void wxDVSelectionListCtrl::Filter( const wxString &filter )
{
	if (filter.IsEmpty())
	{
		for (size_t i=0;i<m_itemList.size();i++)
			m_itemList[i]->shown = true;
	}
	else
	{
		for (size_t i=0;i<m_itemList.size();i++)
		{
			row_item *ri = m_itemList[i];
			if (filter.Len() <= 2 && ri->label.Left( filter.Len() ).Lower() == filter)
				ri->shown = true;
			else if (ri->label.Lower().Find( filter ) >= 0)
				ri->shown = true;
			else
				ri->shown = false;
		}
	}

	Invalidate();
}

int wxDVSelectionListCtrl::AppendNoUpdate( const wxString &name, const wxString &group )
{
	row_item *x = new row_item;
	x->label = name;
	x->group = group;
	for (int i=0;i<NMAXCOLS;i++)
	{
		x->value[i] = false;
		x->enable[i] = true;
	}
	x->shown = true;

	m_itemList.push_back( x );
	x->row_index = m_itemList.size() - 1;

	return m_itemList.size() - 1;
}

int wxDVSelectionListCtrl::Append(const wxString& name, const wxString& group)
{
	int idx = AppendNoUpdate( name, group );
	
	Organize();
	Invalidate();

	return idx;
}

void wxDVSelectionListCtrl::Append( const wxArrayString &names, const wxString &group )
{
	for( size_t i=0;i<names.size();i++ )
		AppendNoUpdate( names[i], group );

	Organize();
	Invalidate();
}

void wxDVSelectionListCtrl::RemoveAt(int row)
{
	if (row < 0 || row >= (int) m_itemList.size()) return;
	delete m_itemList[ row ];
	m_itemList.erase( m_itemList.begin() + (size_t)row );
	
	DeAssignLineColour( row );
	Organize();
	Invalidate();
}

void wxDVSelectionListCtrl::RemoveAll()
{
	DeAssignAll();
	FreeRowItems();
	Organize();
	Invalidate();
}

int wxDVSelectionListCtrl::Length()
{
	return (int)m_itemList.size();
}

void wxDVSelectionListCtrl::ClearColumn(int col)
{
	if ( col < 0 || col >= NMAXCOLS || col >= m_numCols  ) return;

	for (size_t i=0;i<m_itemList.size();i++)
	{
		m_itemList[i]->value[ col ] = false;
		HandleLineColour(i);
	}

	Refresh();
}

int wxDVSelectionListCtrl::SelectRowWithNameInCol(const wxString& name, int col)
{
	if ( col < 0 || col >= NMAXCOLS || col >= m_numCols ) return -1;

	for (size_t i=0;i<m_itemList.size();i++)
	{
		if ( m_itemList[i]->label == name )
		{
			m_itemList[i]->value[ col ] = true;
			
			HandleRadio( i, col );
			HandleLineColour(i);
			Refresh();
			return i;
		}
	}

	return -1;
}

void wxDVSelectionListCtrl::SelectRowInCol(int row, int col, bool value)
{
	if ( col < 0 || col >= NMAXCOLS || col >= m_numCols ) return;
	if ( row < 0 || row >= m_itemList.size() ) return;

	m_itemList[row]->value[ col ] = value;
	
	HandleRadio(row,col);	
	HandleLineColour(row);
	Refresh();
}

void wxDVSelectionListCtrl::Enable(int row, int col, bool enable)
{
	if ( col < 0 || col >= NMAXCOLS || col >= m_numCols ) return;
	if ( row < 0 || row >= m_itemList.size() ) return;

	if (!enable) m_itemList[row]->value[ col ] = false;
	m_itemList[row]->enable[col] = enable;
	Refresh();
}

bool wxDVSelectionListCtrl::IsRowSelected(int row, int start_col)
{
	if ( row < 0 || row >= m_itemList.size() ) return false;

	for (int i=start_col;i<NMAXCOLS;i++)
		if ( m_itemList[row]->value[i] )
			return true;

	return false;
}

bool wxDVSelectionListCtrl::IsSelected(int row, int col)
{
	if ( col < 0 || col >= NMAXCOLS || col >= m_numCols ) return false;
	if ( row < 0 || row >= m_itemList.size() ) return false;

	return m_itemList[row]->value[col];
}

void wxDVSelectionListCtrl::GetLastEventInfo(int* row, int* col, bool* isNowChecked)
{
	if (row) *row = m_lastEventRow;
	if (col) *col = m_lastEventCol;
	if (isNowChecked) *isNowChecked = m_lastEventValue;
}

wxString wxDVSelectionListCtrl::GetRowLabel(int row)
{
	if ( row < 0 || row >= m_itemList.size() ) return wxEmptyString;
	return m_itemList[row]->label;
}

wxString wxDVSelectionListCtrl::GetRowLabelWithGroup( int row )
{
	if ( row < 0 || row >= m_itemList.size() ) return wxEmptyString;
	wxString grp( m_itemList[row]->group );
	if ( grp.IsEmpty() ) return m_itemList[row]->label;
	else return grp + " - " + m_itemList[row]->label;
}

wxString wxDVSelectionListCtrl::GetSelectedNamesInCol(int col)
{
	//Return list of names from col that are selected.
	wxString names;
	for ( int i=0; i<Length(); i++ )
	{
		if (IsSelected(i, col)) //Always col 0.  Top graph.
		{
			names += GetRowLabel(i);
			names += ';'; //Can't be same as used to separate keys/values in settings.
		}
	}
	return names;
}

std::vector<int> wxDVSelectionListCtrl::GetSelectionsInCol( int col )
{
	std::vector<int> list;
	size_t len = m_itemList.size();
	if ( len > 0 )
	{
		list.reserve( len/2 );
		for( int i=0; i<len; i++ )
			if ( IsSelected( i, col ) )
				list.push_back( i );
	}
	return list;
}

int wxDVSelectionListCtrl::GetNumSelected( int col )
{
	int count = 0;
	size_t len = m_itemList.size();
	for( int i=0; i<len; i++ )
		if ( IsSelected( i, col ) )
			count++;
	return count;
}


int wxDVSelectionListCtrl::GetNumberOfSelections()
{
	int count = 0;
	for( int c=0;c<m_numCols;c++ )	
		count += GetNumSelected( c );
	return count;
}

int wxDVSelectionListCtrl::GetUnsortedRowIndex(int SortedIndex)
{
	int Ctr = 0;

	for (size_t i = 0; i < m_groups.size(); i++)
	{
		for (size_t j = 0; j < m_groups[i].items.size(); j++)
		{
			if (Ctr == SortedIndex)
			{
				return m_groups[i].items[j]->row_index;
			}
			else
			{
				Ctr++;
			}
		}
	}

	return 0;
}

void wxDVSelectionListCtrl::ExpandAll()
{
	m_collapsedGroups.clear();
	Invalidate();
	Refresh();
}

void wxDVSelectionListCtrl::ExpandSelections()
{
	m_collapsedGroups.clear();
	for( size_t g=0;g<m_groups.size();g++ )
	{
		bool has_sel = false;
		for( size_t i=0;i<m_groups[g].items.size();i++ )
			if ( IsRowSelected( m_groups[g].items[i]->row_index ) )
				has_sel = true;

		if ( !has_sel )
			m_collapsedGroups.Add( m_groups[g].label );
	}

	Invalidate();
	Refresh();
}

void wxDVSelectionListCtrl::CollapseAll()
{
	m_collapsedGroups.clear();
	for( size_t i=0;i<m_groups.size();i++ )
		m_collapsedGroups.Add( m_groups[i].label );

	Invalidate();
	Refresh();
}

void wxDVSelectionListCtrl::SetUngroupedLabel( const wxString &l )
{
	m_ungroupedLabel = l;
	Refresh();
}

void wxDVSelectionListCtrl::Organize()
{
	m_groups.clear();
	if (m_itemList.size() == 0) return;

	for (size_t i=0;i<m_itemList.size();i++)
		if ( !m_itemList[i]->group.IsEmpty() 
			&& FindGroup( m_itemList[i]->group ) < 0 )
			m_groups.push_back( group( m_itemList[i]->group ) );
		
	for (size_t i=0;i<m_groups.size();i++)
	{
		for (size_t k=0;k<m_itemList.size();k++)
			if ( m_itemList[k]->group == m_groups[i].label )
				m_groups[i].items.push_back( m_itemList[k] );
	}

	group ungrouped( m_ungroupedLabel );
	ungrouped.others = true;
	for (size_t i=0;i<m_itemList.size();i++)
		if ( m_itemList[i]->group.IsEmpty() )
			ungrouped.items.push_back( m_itemList[i] );

	m_groups.push_back( ungrouped );
}

void wxDVSelectionListCtrl::RecalculateBestSize()
{
	wxClientDC dc(this);
	dc.SetFont( GetFont() );

	// calculate desired geometry here
	int width = 0;
	int height = m_itemHeight;
	wxString last_group;
	for (size_t g=0;g<m_groups.size();g++)
	{
		height += m_groupHeight;
				
		wxSize sz = dc.GetTextExtent( m_groups[g].label );
		if (sz.GetWidth() > width)
			width = sz.GetWidth();

		if ( m_collapsedGroups.Index( m_groups[g].label ) == wxNOT_FOUND )
		{
			for( size_t i=0;i<m_groups[g].items.size();i++ )
			{
				row_item *ri = m_groups[g].items[i];

				if ( ! ri->shown )
					continue;

				height += m_itemHeight; // reserve height for each item in the group
				wxSize sz = dc.GetTextExtent( ri->label );
				if (sz.GetWidth() > width)
					width = sz.GetWidth();
			}
		}
	}
	
	width += 4*m_xOffset + m_numCols*(m_boxSize+3);
	m_bestSize.Set( width, height );
}

void wxDVSelectionListCtrl::Invalidate()
{
	RecalculateBestSize();
	ResetScrollbars();
	InvalidateBestSize();
}

void wxDVSelectionListCtrl::ResetScrollbars()
{
	int hpos, vpos;
	GetViewStart( &hpos, &vpos );
	SetScrollbars( SCRL_RATE, SCRL_RATE, m_bestSize.GetWidth()/SCRL_RATE,m_bestSize.GetHeight()/SCRL_RATE,hpos,vpos );	
	InvalidateBestSize();
}

wxSize wxDVSelectionListCtrl::DoGetBestSize() const
{
	return m_bestSize;
}


void wxDVSelectionListCtrl::OnResize(wxSizeEvent &evt)
{
	ResetScrollbars();
}

void wxDVSelectionListCtrl::OnErase(wxEraseEvent &evt)
{
	/* nothing to do */
}

void wxDVSelectionListCtrl::OnPaint(wxPaintEvent &evt)
{
static wxBitmap s_cirMinus, s_cirPlus;
	if ( !s_cirMinus.IsOk() || !s_cirPlus.IsOk() )
	{
		s_cirMinus = wxBITMAP_PNG_FROM_DATA( cirminus_12 );
		s_cirPlus = wxBITMAP_PNG_FROM_DATA( cirplus_12 );
	}

	wxAutoBufferedPaintDC dc(this);
	DoPrepareDC(dc);

	int cwidth = 0, cheight = 0;
	GetClientSize( &cwidth, &cheight );

	
	// paint the background.
	wxColour bg = GetBackgroundColour();
	dc.SetBrush(wxBrush(bg));
	dc.SetPen(wxPen(bg,1));
	wxRect windowRect( wxPoint(0,0), GetClientSize() );
	CalcUnscrolledPosition(windowRect.x, windowRect.y,
		&windowRect.x, &windowRect.y);
	dc.DrawRectangle(windowRect);

	wxFont font_normal( GetFont() );
	wxFont font_bold( font_normal );
	font_bold.SetWeight( wxFONTWEIGHT_BOLD );
	

	int y = 0;
	for( size_t g=0;g<m_groups.size();g++ )
	{
		if ( m_groups[g].items.size() == 0 )
			continue;

		if ( !m_groups[g].others || m_groups.size() > 1 )
		{
			m_groups[g].geom = wxRect( 0, y, windowRect.width, m_groupHeight );
			dc.SetFont( font_bold );
			dc.SetPen( wxPen(bg,1) );
			dc.SetBrush( wxBrush( wxColour(50,50,50), wxSOLID ) );
			dc.DrawRectangle( m_groups[g].geom );
			dc.SetTextForeground( *wxWHITE );
			wxBitmap &bit = (m_collapsedGroups.Index( m_groups[g].label ) >= 0 ) ? s_cirPlus : s_cirMinus;
			dc.DrawBitmap( bit, 3, y+m_groupHeight/2-bit.GetHeight()/2 );		
			dc.DrawText( m_groups[g].label, 3 + bit.GetWidth() + 3, 
				y + m_groupHeight/2-dc.GetCharHeight()/2-1 );
			y += m_groupHeight;
		}			

		if ( m_collapsedGroups.Index( m_groups[g].label ) >= 0 )
			continue;
		
		std::vector<row_item*> &items = m_groups[g].items;
		for (size_t i=0;i<items.size();i++)
		{
			if ( ! items[i]->shown )
				continue;

			int x = m_xOffset;
			int yoff = (m_itemHeight-m_boxSize)/2;
			int radius = m_boxSize / 2;
		
			int Start_Col = 0;
			if ((m_style&wxDVSEL_RADIO_FIRST_COL) || (m_style == wxDVSEL_RADIO_ALL_COL)) { Start_Col = 1; }

			if (!(m_style&wxDVSEL_NO_COLOURS) && IsRowSelected(items[i]->row_index, Start_Col))
			{
				dc.SetPen( wxPen(bg,1) );
				dc.SetBrush( wxBrush( items[i]->color, wxSOLID ) );
				dc.DrawRectangle( m_xOffset-4, 
					y, 
					m_numCols*m_boxSize + (m_numCols-1)*yoff+8, 
					m_itemHeight );
			}

			for ( size_t c=0; c<(size_t)m_numCols; c++ )
			{
				wxColour color =  items[i]->enable[ c ] ? *wxBLACK : *wxLIGHT_GREY;
				items[i]->geom[c] = wxRect( x, y+yoff, m_boxSize, m_boxSize ); // save geometry to speed up mouse clicks
			
				dc.SetBrush( *wxWHITE_BRUSH );
				dc.SetPen( wxPen( color, 1 ) );

				if (((m_style&wxDVSEL_RADIO_FIRST_COL) && c == 0) || (m_style == wxDVSEL_RADIO_ALL_COL))
					dc.DrawCircle(x + radius, y + radius + yoff, radius);
				else 
					dc.DrawRectangle( x, y+yoff, m_boxSize, m_boxSize );
			
				if ( items[i]->value[ c ] )
				{
					dc.SetBrush( *wxBLACK_BRUSH );
					dc.SetPen( *wxBLACK_PEN );
					if (((m_style&wxDVSEL_RADIO_FIRST_COL) && c == 0) || (m_style == wxDVSEL_RADIO_ALL_COL))
						dc.DrawCircle(x + radius, y + radius + yoff, radius - 2);
					else
						dc.DrawRectangle( x+2, y+yoff+2, m_boxSize-4, m_boxSize-4 );
				}

				x += m_boxSize + yoff;
			}

			dc.SetFont( font_normal );
			dc.SetTextForeground( *wxBLACK );
			dc.DrawText( items[i]->label, x+2, y + m_itemHeight/2-dc.GetCharHeight()/2-1 );

			y += m_itemHeight;
		}
	}
}

int wxDVSelectionListCtrl::FindGroup( const wxString &label )
{
	for( size_t g=0;g<m_groups.size();g++ )
		if ( m_groups[g].label == label )
			return (int) g;

	return -1;
}

void wxDVSelectionListCtrl::OnLeftDown(wxMouseEvent &evt)
{
	if ( !HasFocus() )
		SetFocus();

	int vsx, vsy;
	GetViewStart(&vsx,&vsy);
	int mx = vsx*SCRL_RATE + evt.GetX();
	int my = vsy*SCRL_RATE + evt.GetY();

	for( size_t g=0;g<m_groups.size();g++ )
	{
		if ( m_groups[g].geom.Contains( mx, my ) )
		{
			if ( m_collapsedGroups.Index( m_groups[g].label ) >= 0 )
				m_collapsedGroups.Remove( m_groups[g].label );
			else
				m_collapsedGroups.Add( m_groups[g].label );

			Invalidate();
			Refresh();
			return;
		}

		if ( m_collapsedGroups.Index( m_groups[g].label ) >= 0 )
			continue;
		
		std::vector<row_item*> &items = m_groups[g].items;
		for (size_t i=0;i<items.size();i++)
		{
			for (size_t c=0;c<m_numCols;c++)
			{
				if ( items[i]->shown && items[i]->geom[c].Contains( mx, my ) )
				{
					if ( ! items[i]->enable[c] ) return;

					items[i]->value[c] = !items[i]->value[c];
								
					m_lastEventRow = items[i]->row_index;
					m_lastEventCol = c;
					m_lastEventValue = items[i]->value[c];

					HandleRadio( items[i]->row_index, c );
					HandleLineColour( items[i]->row_index );
					Refresh();
				
					wxCommandEvent evt(wxEVT_DVSELECTIONLIST, GetId());
					evt.SetEventObject(this);
					GetEventHandler()->ProcessEvent(evt);
				
					return;
				}
			}
		}
	}
}

void wxDVSelectionListCtrl::OnRightDown( wxMouseEvent &evt )
{
	if ( m_groups.size() > 1 )
	{
		wxMenu menu;
		menu.Append( ID_EXPAND_ALL, "Expand all" );
		menu.Append( ID_EXPAND_SELECTIONS, "Expand selections" );
		menu.AppendSeparator();
		menu.Append( ID_COLLAPSE_ALL, "Collapse all" );
		PopupMenu( &menu );
	}
}

void wxDVSelectionListCtrl::OnPopupMenu( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_EXPAND_ALL: ExpandAll(); break;
	case ID_EXPAND_SELECTIONS: ExpandSelections(); break;
	case ID_COLLAPSE_ALL: CollapseAll(); break;
		break;
	}
}

void wxDVSelectionListCtrl::OnMouseMove(wxMouseEvent &evt)
{
/*
	int vsx, vsy;
	GetViewStart(&vsx,&vsy);
	wxDVSelectionListCtrlItem *cur_item = LocateXY(vsx+evt.GetX(), vsy+evt.GetY());
	if (cur_item != mLastHoverItem)
	{
		if (mLastHoverItem) mLastHoverItem->Hover = false;
		mLastHoverItem = cur_item;
		if (cur_item) cur_item->Hover = true;

		Refresh();
	}
*/
}

void wxDVSelectionListCtrl::OnLeave(wxMouseEvent &evt)
{
/*	if (mLastHoverItem)
	{
		mLastHoverItem->Hover = false;
		mLastHoverItem = NULL;

		Refresh();
	}
*/
}


void wxDVSelectionListCtrl::HandleRadio( int r, int c )
{
	if (((m_style&wxDVSEL_RADIO_FIRST_COL) || (m_style == wxDVSEL_RADIO_ALL_COL)) && c == 0)
	{
		for (size_t k=0;k<m_itemList.size();k++)
		{
			m_itemList[k]->value[0] = ( k == r );			
			HandleLineColour( k );
		}
	}

	if (m_style == wxDVSEL_RADIO_ALL_COL)
	{
		for (size_t k = 0; k < m_itemList.size(); k++)
		{
			m_itemList[k]->value[1] = (k == r);
			HandleLineColour(k);
		}
	}
}

void wxDVSelectionListCtrl::HandleLineColour(int row)
{
	bool row_selected = IsRowSelected( row );
	bool has_colour = IsColourAssigned( row );

	if ( row_selected && !has_colour )
		m_itemList[row]->color = AssignLineColour( row );
	else if ( !row_selected && has_colour )
		DeAssignLineColour( row );
}

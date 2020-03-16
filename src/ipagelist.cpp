/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <algorithm>

#include <wx/wx.h>
#include <wx/dcbuffer.h>

#include <wex/metro.h>
#include <wex/utils.h>

#include "main.h"
#include "ipagelist.h"
#include "casewin.h"

#include "../resource/notes.cpng"

#define SCRL_RATE 25

#ifndef MAX
#define MAX(a,b) ((a)<(b)?(b):(a))
#endif

BEGIN_EVENT_TABLE(InputPageList, wxScrolledWindow)
	EVT_SIZE( InputPageList::OnResize )	
	EVT_LEFT_DOWN( InputPageList::OnLeftDown )
	EVT_PAINT( InputPageList::OnPaint )
	EVT_MOTION( InputPageList::OnMouseMove )
	EVT_LEAVE_WINDOW( InputPageList::OnLeave )
	EVT_ERASE_BACKGROUND( InputPageList::OnErase )
END_EVENT_TABLE()

static wxBitmap g_notesBitmap;


static wxColour HighlightColour(224,224,224);
static wxColour SelectColour(204,204,204);
static wxColour BackColour(243,243,243);

InputPageList::InputPageList(wxWindow *parent, int id, const wxPoint &pos, const wxSize &size)
	: wxScrolledWindow(parent,id, pos, size, wxBORDER_NONE)
{
	SetBackgroundStyle(wxBG_STYLE_CUSTOM);
	SetBackgroundColour( BackColour );
	
	if ( !g_notesBitmap.IsOk() )
		g_notesBitmap = wxBITMAP_PNG_FROM_DATA( notes );

	m_caseWin = 0;
	m_selectedIdx = -1;
	m_hoverIdx = -1;
	
}

InputPageList::~InputPageList()
{
	/* nothing to do */
}


void InputPageList::Add(const wxString &item, bool geom_recalc, const wxString &resource)
{
	if (Find(item) >= 0)
		return;
	
	_item x;
	x.name = item;
	x.resource = resource.IsEmpty()?item:resource;

	wxString res_name = (!resource.IsEmpty()?resource:item);
	res_name.Replace("/","");

	m_items.push_back( x );

	if (geom_recalc)
		Invalidate();
}

void InputPageList::SetCaseWindow(CaseWindow *cw)
{
	m_caseWin = cw;
}

void InputPageList::Add(const wxArrayString &item)
{
	for (int i=0;i<(int)item.size();i++)
		Add(item[i], (i==(int)(item.size()-1)) );
}

int InputPageList::Find(const wxString &item)
{
	for (size_t i=0;i<m_items.size();i++)
		if (m_items[i].name == item)
			return i;
	return -1;
}

wxString InputPageList::GetItem(int idx)
{
	if (idx >= 0 && idx < (int)m_items.size())
		return m_items[idx].name;
	else
		return wxEmptyString;
}

wxString InputPageList::GetValue()
{
	return GetItem(m_selectedIdx);
}

void InputPageList::Remove(int idx)
{
	if (idx >= 0 && idx < (int)m_items.size())
	{
		m_items.erase( m_items.begin() + idx );
		Invalidate();
	}
}

void InputPageList::ClearItems()
{
	m_items.clear();
	Invalidate();
}

int InputPageList::Count()
{
	return m_items.size();
}

int InputPageList::FindItem(const wxString &name)
{
	for (size_t i=0;i<m_items.size();i++)
		if (m_items[i].name == name)
			return i;
	return -1;
}

wxArrayString InputPageList::GetItems()
{
	wxArrayString list;
	for (size_t i=0;i<m_items.size();i++)
		list.Add( m_items[i].name );

	return list;
}

void InputPageList::Select(int idx)
{
	m_selectedIdx = idx;
	Refresh();
}

int InputPageList::GetSelection()
{
	return m_selectedIdx;
}

wxString InputPageList::GetStringSelection()
{
	if ( m_selectedIdx >= 0 && m_selectedIdx < (int)m_items.size() )
		return m_items[m_selectedIdx].name;
	else
		return wxEmptyString;
}


#define MYFONT wxMetroTheme::Font( wxMT_LIGHT, 13 )

void InputPageList::Invalidate()
{
	int hpos, vpos;
	GetViewStart( &hpos, &vpos );
	hpos *= SCRL_RATE;
	vpos *= SCRL_RATE;

	wxSize sz = GetClientSize();
	wxClientDC dc(this);
	dc.SetFont( MYFONT );

	int height = (int)(1.6*dc.GetCharHeight());	
	int y = 0;
	for (size_t i=0;i<m_items.size();i++)
	{
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

void InputPageList::OnResize(wxSizeEvent &)
{
	Invalidate();
}

#define TXTXOFF 10


void InputPageList::OnPaint(wxPaintEvent &)
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

	double scale = wxGetScreenHDScale();
	
	dc.SetTextForeground( *wxBLACK );
	dc.SetFont( MYFONT );

	for (int i=0;i<(int)m_items.size();i++)	
	{
		wxRect r = m_items[i].geom;

		wxColour c( GetBackgroundColour() );
		if(i==m_hoverIdx) c=HighlightColour;
		if(i==m_selectedIdx) c=SelectColour;
		
		dc.SetPen( wxPen(c) );
		dc.SetBrush( wxBrush(c, wxBRUSHSTYLE_SOLID) );
		dc.DrawRectangle( r.x, r.y, r.width, r.height);

		//wxFont font(MYFONT);
		//if ( i==m_selectedIdx ) font.SetWeight( wxFONTWEIGHT_BOLD );
		//dc.SetFont( font );

		wxSize tsz( dc.GetTextExtent( m_items[i].name ) );

		dc.DrawText( m_items[i].name, r.x+(int)(TXTXOFF*scale), r.y+r.height/2-tsz.y/2 );

		dc.SetBackground( wxBrush( GetBackgroundColour() ) );

		if (g_notesBitmap.IsOk() && m_caseWin 
			 && m_caseWin->HasPageNote( m_items[i].resource ) )
		{
//			int tx_w = dc.GetTextExtent( m_items[i].name ).GetWidth();
			dc.DrawBitmap(g_notesBitmap, 
				r.x + r.width - g_notesBitmap.GetWidth() - ((int)TXTXOFF*scale),
				r.y + r.height/2 - g_notesBitmap.GetHeight()/2);
		}
	}
}

void InputPageList::OnErase( wxEraseEvent & )
{
	/* nothing to do */
}

void InputPageList::OnLeftDown(wxMouseEvent &evt)
{
	int vsx, vsy;
	GetViewStart( &vsx, &vsy );
	vsx *= SCRL_RATE;
	vsy *= SCRL_RATE;

	SetFocus();

	for (size_t i=0;i<m_items.size();i++)
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
}

void InputPageList::OnMouseMove(wxMouseEvent &evt)
{	
	int vsx, vsy;
	GetViewStart( &vsx, &vsy );
	vsx *= SCRL_RATE;
	vsy *= SCRL_RATE;

	for (int i=0;i<(int)m_items.size();i++)
	{
		if (evt.GetY()+vsy > m_items[i].geom.y 
			&& evt.GetY()+vsy < m_items[i].geom.y+m_items[i].geom.height
			&& m_hoverIdx != i )
		{
			
			m_hoverIdx = i;
			Refresh();
			return;
		}
	}
}

void InputPageList::OnLeave(wxMouseEvent &)
{
	m_hoverIdx = -1;
	Refresh();
}


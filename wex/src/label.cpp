#include <wx/dcclient.h>
#include <wx/dcbuffer.h>

#include "wex/utils.h"
#include "wex/label.h"

BEGIN_EVENT_TABLE( wxLabel, wxWindow )
	EVT_ERASE_BACKGROUND( wxLabel::OnErase )
	EVT_PAINT( wxLabel::OnPaint )
	EVT_SIZE( wxLabel::OnResize )
END_EVENT_TABLE()

wxLabel::wxLabel( wxWindow *parent, int id, const wxString &caption, const wxPoint &pos, const wxSize &size)
	: wxWindow(parent, id, pos, size )
{
	SetBackgroundStyle( wxBG_STYLE_PAINT );

	InvalidateBestSize();
	m_colour = *wxBLACK;
	m_text = caption;
	m_alignTop = false;
	m_alignRight = false;
	m_relSize = 0;
	m_bold = false;
	m_wordWrap = false;
}

wxSize wxLabel::DoGetBestSize() const
{
	wxClientDC dc(const_cast<wxLabel*>(this));
	wxFont font( GetFont() );
	if ( m_bold ) font.SetWeight( wxFONTWEIGHT_BOLD );
	dc.SetFont( font );
	wxSize size = dc.GetTextExtent( m_text );
	int border = (int)( 4.0 * wxGetScreenHDScale() );
	return wxSize( size.x + border, size.y + border );
}

void wxLabel::AlignTop(bool b)
{
	m_alignTop = b;
}

void wxLabel::AlignRight(bool b)
{
	m_alignRight = b;
}

void wxLabel::SetText(const wxString &txt)
{
	InvalidateBestSize();
	m_text = txt;
	Refresh();
}

void wxLabel::SetColour(const wxColour &c)
{
	m_colour = c;
}

void wxLabel::SetBold(bool b)
{
	m_bold = b;
}

void wxLabel::SetWordWrap(bool b)
{
	m_wordWrap = b;
}

void wxLabel::SetRelativeSize(int sz)
{
	m_relSize = sz;
}

void wxLabel::OnPaint(wxPaintEvent &evt)
{
	wxAutoBufferedPaintDC pdc(this);
	PrepareDC(pdc);
	pdc.SetClippingRegion( this->GetUpdateRegion() );

	pdc.SetBackground( wxBrush( GetBackgroundColour() ) );
	pdc.Clear();
	
	int width, height;
	GetClientSize( &width, &height );

	wxLabelDraw( pdc, wxRect( 0, 0, width, height ), GetFont(), m_text, m_colour, 
		m_alignTop, m_alignRight, m_bold, m_wordWrap, m_relSize );

	pdc.DestroyClippingRegion();
}

void wxLabel::OnErase( wxEraseEvent &evt )
{
	// nothing to do 
}

void wxLabel::OnResize(wxSizeEvent &evt)
{
	Refresh();
}

void wxLabelDraw( wxDC &dc, const wxRect &geom, const wxFont &font, const wxString &text, const wxColour &col, 
				 bool alignTop, bool alignRight, bool isBold, bool wordWrap, int relSize )
{
	dc.SetTextForeground( col );

	wxFont f( font );

	if ( isBold )
		f.SetWeight( wxFONTWEIGHT_BOLD );
	else if (!isBold && f.GetWeight() != wxFONTWEIGHT_NORMAL )
		f.SetWeight( wxFONTWEIGHT_NORMAL );

	if ( relSize != 0)
		f.SetPointSize( f.GetPointSize() + relSize );

	dc.SetFont( f );
	
	wxCoord width, height;
	dc.GetTextExtent( text , &width, &height);
	wxRect tbounds( geom.x+1, geom.y+geom.height/2-height/2-1,
					width, height+3);

	if ( alignTop ) tbounds.y = geom.y + 1;
	if ( alignRight ) tbounds.x = geom.x + geom.width - width - 2;	
	
	if ( wordWrap && !alignRight && alignTop )
		wxDrawWordWrappedText(dc, text, geom.width-4, true, tbounds.x, geom.y+1);
	else
		dc.DrawText( text, tbounds.x, tbounds.y );
}

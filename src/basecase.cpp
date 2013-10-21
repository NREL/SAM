#include <wx/simplebook.h>
#include <wx/panel.h>
#include <wx/clipbrd.h>
#include <wx/dcbuffer.h>

#include <wex/extgrid.h>
#include <wex/metro.h>
#include <wex/dview/dvplotctrl.h>
#include <wex/plot/plplotctrl.h>


#include "basecase.h"

enum { ID_PAGESELECT = wxID_HIGHEST+948 };

BEGIN_EVENT_TABLE( BaseCase, wxSplitterWindow )	
	EVT_LISTBOX( ID_PAGESELECT, BaseCase::OnCommand )
END_EVENT_TABLE()

BaseCase::BaseCase( wxWindow *parent, CaseWindow *cw )
	: wxSplitterWindow( parent, wxID_ANY, 
		wxDefaultPosition, wxDefaultSize, wxSP_NOBORDER | wxSP_LIVE_UPDATE )
{
	wxPanel *left_panel = new wxPanel( this );

	m_metrics = new MetricsTable( left_panel );
	matrix_t<wxString> data( 10, 2 );
	data.at(0,0) = "Metric"; data.at(0,1) = "Value";
	for( size_t i=1;i<10;i++ )
	{
		data.at(i,0) = wxString::Format("Metric %d", (int)i+1);
		data.at(i,1) = wxString::Format("%.2lf", (i+193)*pow(1.22, (double)(i)/3.0) );
	}
	m_metrics->SetData( data );

	m_pageList = new wxMetroListBox( left_panel, ID_PAGESELECT );
	//m_pageList->SetFont( wxMetroTheme::Font( wxMT_LIGHT, 10 ) );
	m_pageList->Add( "Standard Graphs" );
	m_pageList->Add( "Data Tables" );
	m_pageList->Add( "Cash Flow" );
	m_pageList->Add( "Hourly Time Series" );
	m_pageList->Add( "Average Day Profiles" );
	m_pageList->Add( "Hourly Heat Map" );
	m_pageList->Add( "Histograms" );
	m_pageList->Add( "Duration Curves" );
	m_pageList->Add( "Hourly Scatter Plots" );
	m_pageList->SetSelection( 0 );

	wxBoxSizer *left_sizer = new wxBoxSizer( wxVERTICAL );
	left_sizer->Add( m_metrics, 0, wxALL|wxEXPAND, 0 );
	left_sizer->Add( m_pageList, 1, wxALL|wxEXPAND, 0 );
	left_panel->SetSizer( left_sizer );


	m_pageFlipper = new wxSimplebook( this, wxID_ANY, 
		wxDefaultPosition, wxDefaultSize, wxBORDER_NONE );

	
	m_plot = new wxPLPlotCtrl( m_pageFlipper, wxID_ANY );
	m_pageFlipper->AddPage( m_plot, "Graphs", true );

	m_cashFlowGrid = new wxExtGridCtrl( m_pageFlipper, wxID_ANY );
	m_cashFlowGrid->CreateGrid( 10, 30 );
	m_pageFlipper->AddPage( m_cashFlowGrid, "Cash Flow" );

	m_dview = new wxDVPlotCtrl( m_pageFlipper );
	m_pageFlipper->AddPage( m_dview, "Time Series" );

	
	m_pageFlipper->AddPage( new wxPanel( m_pageFlipper ), "Data tables" );

	SplitVertically( left_panel, m_pageFlipper, 200 );

}

BaseCase::~BaseCase()
{
	/* nothing to do */
}

void BaseCase::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_PAGESELECT:
		if ( m_pageList->GetSelection() < m_pageFlipper->GetPageCount() )
			m_pageFlipper->SetSelection( m_pageList->GetSelection() );
		break;
	default:
		break;
	}
}






enum { ID_METRICS_COPY_TSV = wxID_HIGHEST+258,
ID_METRICS_COPY_CSV};

BEGIN_EVENT_TABLE( MetricsTable, wxWindow)
	EVT_SIZE( MetricsTable::OnResize)
	EVT_PAINT( MetricsTable::OnPaint)
	EVT_RIGHT_DOWN(MetricsTable::OnRightClick)
	EVT_MENU( ID_METRICS_COPY_TSV, MetricsTable::OnContextMenu )
	EVT_MENU( ID_METRICS_COPY_CSV, MetricsTable::OnContextMenu )
END_EVENT_TABLE()

MetricsTable::MetricsTable(wxWindow *parent)
	: wxWindow(parent,-1,wxDefaultPosition, wxDefaultSize, wxCLIP_CHILDREN)
{
	mRowHeight = 0;
	mBestHeight = 0;
	SetBackgroundStyle(wxBG_STYLE_CUSTOM);
	mFillColour = wxColour(50,50,50);
	mTableColour = *wxWHITE;

}

void MetricsTable::OnContextMenu(wxCommandEvent &evt)
{
	wxString sep = evt.GetId() == ID_METRICS_COPY_CSV ? "," : "\t";

	wxString tdat;
	for (int r=0;r<mTable.nrows();r++)
		for (int c=0;c<mTable.ncols();c++)
			tdat += mTable.at(r,c) + (c==mTable.ncols()-1 ? "\n" : sep);

	if (wxTheClipboard->Open())
	{
	// This data objects are held by the clipboard, 
	// so do not delete them in the app.
		wxTheClipboard->SetData( new wxTextDataObject(tdat) );
		wxTheClipboard->Close();
	}
}

void MetricsTable::OnRightClick(wxMouseEvent &evt)
{
	wxMenu popup;
	popup.Append( ID_METRICS_COPY_TSV, "Copy table");
	popup.Append( ID_METRICS_COPY_CSV, "Copy as CSV");
	PopupMenu( &popup, evt.GetPosition() );
}

void MetricsTable::SetData(const matrix_t<wxString> &data)
{
	mTable = data;
	Refresh();
}

wxSize MetricsTable::DoGetBestSize() const
{
	int height = 0;
	wxClientDC dc( const_cast<MetricsTable*>(this) );
	wxFont f = *wxNORMAL_FONT;
	f.SetWeight( wxFONTWEIGHT_BOLD );
	dc.SetFont( f );
	height = (dc.GetCharHeight()+2) * (mTable.nrows()) + 10;
	return wxSize(200,height); // mBestHeight;
}

#define XBORDER 3

void MetricsTable::OnPaint(wxPaintEvent &evt)
{
	wxAutoBufferedPaintDC dc(this);
	int cwidth, cheight;
	GetClientSize(&cwidth, &cheight);
	
	dc.SetPen( wxPen( mFillColour, 1) );
	dc.SetBrush( wxBrush( mFillColour, wxSOLID ) );
	dc.DrawRectangle(0,0,cwidth,cheight);

	wxFont f = *wxNORMAL_FONT;
	f.SetWeight( wxFONTWEIGHT_BOLD );
	dc.SetFont( f );

	int ch = dc.GetCharHeight();
	int nrows = mTable.nrows();
	int ncols = mTable.ncols();

	int r,c;
	matrix_t<wxSize> cellsz;

	dc.SetTextForeground( *wxWHITE );

	cellsz.resize( nrows, ncols );
	for (r=0;r<nrows;r++)
		for (c=0;c<ncols;c++)
			cellsz.at(r,c) = dc.GetTextExtent( mTable.at(r,c) );

	int rowheight=0;
	std::vector<int> colxsz( ncols );
	for (c=0;c<ncols;c++)
	{
		colxsz[c] = 0;
		for (r=0;r<nrows;r++)
		{
			if (cellsz.at(r,c).x > colxsz[c])
				colxsz[c] = cellsz.at(r,c).x;

			if (cellsz.at(r,c).y > rowheight)
				rowheight = cellsz.at(r,c).y;
		}

	}
	
	rowheight += 2;

	if (mTable.nrows() > 0)
	{
		for (int i=0;i<mTable.ncols();i++)
		{
			if (mTable.at(0,i).IsEmpty())
				continue;

			int xpos = XBORDER;
			for (int k=0;k<i && k<colxsz.size();k++)
				xpos += colxsz[k];

			dc.DrawText(mTable.at(0,i), xpos, 2);
		}
	}
	
	dc.SetPen( wxPen(mTableColour, 1) );
	dc.SetBrush( wxBrush( mTableColour, wxSOLID ) );
	dc.DrawRectangle(XBORDER,ch+5,cwidth-XBORDER-XBORDER,cheight-ch-8);
	dc.SetClippingRegion(wxRect(XBORDER,ch+5,cwidth-XBORDER-XBORDER,cheight-ch-8));

	// draw table;

	dc.SetFont( *wxNORMAL_FONT );
	dc.SetTextForeground(*wxBLACK);

	int y=ch+6;

	std::vector<int> colstart;
	if (ncols > 0)
	{
		colstart.resize(ncols);
		colstart[0] = 0;
		for (c=1;c<ncols;c++)
			colstart[c] = colstart[c-1] + colxsz[c-1];
	}

	for (r=1;r<nrows;r++)
	{
		for (c=0;c<ncols;c++)
			dc.DrawText(mTable.at(r,c), XBORDER+1+colstart[c], y+1);

		y += rowheight;
	}

	dc.SetPen( wxPen(*::wxLIGHT_GREY, 1) );
	for (r=1;r<nrows;r++)
		dc.DrawLine(XBORDER,r*rowheight+ch+6, cwidth-XBORDER, r*rowheight+ch+6);

	for (c=1;c<ncols;c++)
		dc.DrawLine( colstart[c], ch+5, colstart[c], (nrows-1)*rowheight+ch+6);

	dc.DestroyClippingRegion();

	mBestHeight = ch+12+rowheight*(nrows-1);
	mRowHeight = rowheight;
}

void MetricsTable::OnResize(wxSizeEvent &evt)
{
	Refresh();
}

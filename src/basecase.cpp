#include <wx/simplebook.h>
#include <wx/panel.h>
#include <wx/clipbrd.h>
#include <wx/dcbuffer.h>

#include <wex/extgrid.h>
#include <wex/metro.h>
#include <wex/dview/dvplotctrl.h>
#include <wex/plot/plplotctrl.h>
#include <wex/plot/plaxis.h>
#include <wex/plot/pllineplot.h>


#include "basecase.h"

enum { ID_PAGESELECT = wxID_HIGHEST+948 };

static void setup_plot( wxPLPlotCtrl *plot, int i )
{	
	plot->SetTitle( wxT("Demo Plot: using \\theta(x)=sin(x)^2, x_0=1\n\\zeta(x)=3\\dot sin^2(x)") );

		
	wxPLLabelAxis *mx = new wxPLLabelAxis( -1, 12, "Months of the year (\\Chi\\Psi)" );
	mx->ShowLabel( false );

	mx->Add( 0,  "Jan" );
	mx->Add( 1,  "Feb" );
	mx->Add( 2,  "March\nMarzo" );
	mx->Add( 3,  "Apr" );
	mx->Add( 4,  "May" );
	mx->Add( 5,  "June\nJunio" );
	mx->Add( 6,  "July" );
	mx->Add( 7,  "August" );
	mx->Add( 8,  "September" );
	mx->Add( 9,  "October" );
	mx->Add( 10, "November" );
	mx->Add( 11, "December\nDeciembre" );

	plot->SetXAxis2( mx );
	/*
	plot->SetYAxis1( new wxPLLinearAxis(-140, 150, wxT("y1 label\ntakes up 2 lines")) );
	plot->SetYAxis1( new wxPLLinearAxis(-11, -3, wxT("\\theta(x)")), wxPLPlotCtrl::PLOT_BOTTOM );
		
	*/
	//plot->SetScaleTextSize( true );

	plot->ShowGrid( true, true );
		
	plot->SetXAxis1( new wxPLLogAxis( 0.01, 100, "\\nu  (m^3/kg)" ) );	

	std::vector< wxRealPoint > sine_data;
	std::vector< wxRealPoint > cosine_data;
	std::vector< wxRealPoint > tangent_data;
	for (double x = -6; x < 12; x+= 0.01)
	{
		sine_data.push_back( wxRealPoint( x, (i+1)*3*sin( x )*sin( x ) ) );
		cosine_data.push_back( wxRealPoint( x/2, 2*cos( x/2 )*x ) );
		tangent_data.push_back( wxRealPoint( x, x*tan( x ) ) );
	}


	plot->AddPlot( new wxPLLinePlot( sine_data, "3\\dot sin^2(x)", "forest green", wxPLLinePlot::DOTTED ), 
		wxPLPlotCtrl::X_BOTTOM, 
		wxPLPlotCtrl::Y_LEFT, 
		wxPLPlotCtrl::PLOT_TOP);


	plot->GetXAxis1()->SetLabel( "Bottom X Axis has a great sequence of \\nu  values!" );
		

	plot->AddPlot( new wxPLLinePlot( cosine_data, "cos(\\Omega_\\alpha  )", *wxRED, wxPLLinePlot::DASHED ), 
		wxPLPlotCtrl::X_BOTTOM, 
		wxPLPlotCtrl::Y_LEFT,
		wxPLPlotCtrl::PLOT_TOP);
		
	if ( i > 2 )
	{
		wxPLLinePlot *lltan = new wxPLLinePlot( tangent_data, "\\beta\\dot tan(\\beta)", *wxBLUE, wxPLLinePlot::SOLID, 1, false );
		lltan->SetAntiAliasing( true );
		plot->AddPlot( lltan, 
			wxPLPlotCtrl::X_BOTTOM, 
			wxPLPlotCtrl::Y_LEFT,
			wxPLPlotCtrl::PLOT_BOTTOM);

		std::vector< wxRealPoint > pow_data;
		for (double i=0.01;i<20;i+=0.1)
			pow_data.push_back( wxRealPoint(i, pow(i,3)-0.02*pow(i,6) ) );
		

		plot->AddPlot( new wxPLLinePlot( pow_data, "i^3 -0.02\\dot i^6", *wxBLACK, wxPLLinePlot::SOLID ),
			wxPLPlotCtrl::X_BOTTOM,
			wxPLPlotCtrl::Y_LEFT,
			wxPLPlotCtrl::PLOT_TOP );
	}



	plot->GetYAxis1()->SetLabel( "Pressure (kPa)" );
	plot->GetYAxis1()->SetColour( *wxRED );
	plot->GetYAxis1()->SetWorld( -20, 20 );

}

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

	wxScrolledWindow *scrolwin = new wxScrolledWindow( m_pageFlipper, wxID_ANY );
	scrolwin->SetBackgroundColour( *wxWHITE );
	int y = 10;
	for( size_t i=0;i<5;i++ )
	{
		wxPLPlotCtrl *pl = new wxPLPlotCtrl( scrolwin, wxID_ANY, wxPoint(10, y), wxSize(400,300) );
		y+=310;
		setup_plot( pl, i );
		m_plot.push_back( pl );
	}

	scrolwin->SetScrollbars( 1, 1, 400, y );
	m_pageFlipper->AddPage( scrolwin, "Graphs", true );

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

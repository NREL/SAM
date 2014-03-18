#include <wx/wx.h>

#include <wex/plot/plaxis.h>
#include <wex/plot/plbarplot.h>
#include <wex/plot/pllineplot.h>
#include <wex/plot/plscatterplot.h>

#include <wex/radiochoice.h>
#include <wex/utils.h>
#include <wex/exttext.h>
#include <wex/metro.h>

#include "graph.h"
#include "variables.h"


Graph::Graph()
{
	Type = BAR;
	ShowXValues = ShowYValues = ShowLegend = true;
	LegendPos = wxPLPlotCtrl::RIGHT;
	Size=0;
	CoarseGrid = FineGrid = true;	
	YMin=YMax= std::numeric_limits<double>::quiet_NaN();
	FontScale = 1;
}

void Graph::Copy(Graph *gr)
{
	Type = gr->Type;
	Y = gr->Y;
	
	XLabel = gr->XLabel;
	YLabel = gr->YLabel;
	Title = gr->Title;
	ShowXValues = gr->ShowXValues;
	ShowYValues = gr->ShowYValues;
	ShowLegend = gr->ShowLegend;
	LegendPos = gr->LegendPos;
	Size = gr->Size;
	CoarseGrid = gr->CoarseGrid;
	FineGrid = gr->FineGrid;
	YMin = gr->YMin;
	YMax = gr->YMax;
	Notes = gr->Notes;
	FontScale = gr->FontScale;
}


bool Graph::SameAs(Graph *gr)
{
	return ( this->Title == gr->Title && this->Y == gr->Y );
}

bool Graph::Write( wxOutputStream &os )
{
	wxDataOutputStream ds(os);
	ds.Write16( 0xfd ); // identifier
	ds.Write8( 1 ); // version

	ds.Write32( Type );
	ds.WriteString( Title );
	ds.Write32( Y.Count() );
	for (int i=0;i<(int)Y.Count();i++)
		ds.WriteString( Y[i] );

	ds.WriteString( XLabel );
	ds.WriteString( YLabel );
	ds.Write8( ShowXValues ? 1 : 0 );
	ds.Write8( ShowYValues ? 1 : 0  );
	ds.Write8( ShowLegend ? 1 : 0  );
	ds.Write8( LegendPos );
	ds.Write8( Size );
	ds.Write8( CoarseGrid ? 1 : 0  );
	ds.Write8( FineGrid ? 1 : 0  );
	ds.WriteDouble( YMin );
	ds.WriteDouble( YMax );
	ds.WriteString( Notes );
	ds.WriteDouble( FontScale );

	ds.Write16( 0xfd ); // identifier
	return true;
}


bool Graph::Read( wxInputStream &is )
{
	size_t i, count;
	wxDataInputStream ds(is);

	unsigned short identifier = ds.Read16();
	/*unsigned char ver = */ ds.Read8(); // read the version number, not currently used...

	Type = ds.Read32();
	Title = ds.ReadString();

	Y.Clear();
	count = ds.Read32();
	for (i=0;i<count;i++)
		Y.Add( ds.ReadString() );

	XLabel = ds.ReadString();
	YLabel = ds.ReadString();
	
	ShowXValues = ds.Read8() ? true : false;
	ShowYValues = ds.Read8() ? true : false;
	ShowLegend = ds.Read8() ? true : false;
	LegendPos = ds.Read8();

	Size = ds.Read8();
	CoarseGrid = ds.Read8() ? true : false;
	FineGrid = ds.Read8() ? true : false;
	
	YMin = ds.ReadDouble();
	YMax = ds.ReadDouble();
	
	Notes = ds.ReadString();
	FontScale = ds.ReadDouble();

	return identifier == ds.Read16();
}


GraphCtrl::GraphCtrl( wxWindow *parent, int id )
	: wxPLPlotCtrl( parent, id )
{
	/* nothing to do */
}


void GraphCtrl::Display( DataProvider *data, Graph &gi )
{
static const char *s_monthNames[12] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
										"Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

static std::vector<wxColour> s_colours;
	if ( s_colours.size() == 0 )
	{
		s_colours.push_back( wxColour(111,164,196) );
		s_colours.push_back( wxColour("GREY") );
		s_colours.push_back( wxColour(181,211,227) );
		s_colours.push_back( *wxLIGHT_GREY );
		s_colours.push_back( wxColour("PALE GREEN") );
		s_colours.push_back( wxColour("GOLDENROD") );
		s_colours.push_back( wxColour("MEDIUM VIOLET RED") );
		s_colours.push_back( wxColour("MEDIUM SEA GREEN") );
		s_colours.push_back( wxColour("DARK SLATE GREY") );
		s_colours.push_back( wxColour("WHEAT") );
		s_colours.push_back( wxColour("FIREBRICK") );
		s_colours.push_back( wxColour("dark orchid") );
		s_colours.push_back( wxColour("dim grey") );
		s_colours.push_back( wxColour("brown") );
	}

	m_d = data;
	m_g.Copy( &gi );

	DeleteAllPlots();
	
	if ( !m_d )
	{
		Refresh();
		return;
	}
	
	// setup visual properties of graph
	wxFont font( wxMetroTheme::Font(wxMT_LIGHT, 12) );
	if ( m_g.FontScale != 0.0 )
	{
		int points = (int)(((double)font.GetPointSize())*m_g.FontScale);
		if ( points < 4 ) points = 4;
		if ( points > 32 ) points = 32;
		font.SetPointSize( points );		
	}
	
	SetFont( font );

	ShowGrid( m_g.CoarseGrid, m_g.FineGrid );
	SetTitle( m_g.Title );
	ShowLegend( m_g.ShowLegend );
	SetLegendLocation( (wxPLPlotCtrl::LegendPos)m_g.LegendPos );


	// setup data
	std::vector<VarValue*> yvars;
	wxArrayString ynames;
	int ndata = -1;

	for( size_t i=0;i<m_g.Y.size();i++ )
	{
		if ( VarValue *vv = m_d->GetValue( m_g.Y[i] ) )
		{
			int count = 0;
			if ( vv->Type() == VV_NUMBER )
				count = 1;
			else if ( vv->Type() == VV_ARRAY )
				count = vv->Length();

			if ( i == 0 ) ndata = count;
			else if ( ndata != count ) ndata = -1;

			if ( count > 0 )
			{
				yvars.push_back( vv );
				ynames.push_back( m_g.Y[i] );
			}
		}
	}

	if ( ndata < 0 )
	{
		SetTitle( "All variables must have the same number of data values." );
		Refresh();
		return;
	}

	std::vector< std::vector<wxRealPoint> > plotdata( yvars.size() );

	int cidx = 0; // colour index
	wxPLBarPlot *last_bar = 0;
	std::vector<wxPLBarPlot*> bar_group;

	for( size_t i=0;i<yvars.size();i++ )
	{
		if ( yvars[i]->Type() == VV_ARRAY )
		{
			size_t n = 0;
			float *p = yvars[i]->Array( &n );

			plotdata[i].reserve( ndata );
			for( size_t k=0;k<n;k++ )
				plotdata[i].push_back( wxRealPoint( k, p[k] ) );
		}
		else
			plotdata[i].push_back( wxRealPoint( i, yvars[i]->Value() ) ); 

		wxPLPlottable *plot = 0;
		if ( m_g.Type == Graph::LINE )
			plot = new wxPLLinePlot( plotdata[i], m_d->GetLabel( ynames[i] ), s_colours[cidx], 
				wxPLLinePlot::SOLID, m_g.Size+2 );
		else if ( m_g.Type == Graph::BAR || m_g.Type == Graph::STACKED )
		{
			wxPLBarPlot *bar = new wxPLBarPlot(  plotdata[i], m_d->GetLabel(ynames[i]), s_colours[cidx] );
			if ( m_g.Size != 0 )
				bar->SetThickness( m_g.Size, false );

			if ( m_g.Type == Graph::STACKED )
				bar->SetStackedOn( last_bar );
			else
				bar_group.push_back( bar );

			last_bar = bar;
			plot = bar;
		}
		else if ( m_g.Type == Graph::SCATTER )
		{
			plot = new wxPLScatterPlot( plotdata[i], m_d->GetLabel( ynames[i] ), s_colours[cidx], m_g.Size+2 );
			if ( plotdata[i].size() < 100 )
				plot->SetAntiAliasing( true );
		}


		if ( ++cidx >= s_colours.size() ) cidx = 0; // incr and wrap around colour index
		
		if ( plot != 0 )
			AddPlot( plot, wxPLPlotCtrl::X_BOTTOM, wxPLPlotCtrl::Y_LEFT, wxPLPlotCtrl::PLOT_TOP, false );
	}

	
	// group the bars together if they're not stacked and not single values
	if ( ndata > 1 && m_g.Type == Graph::BAR )
		for( size_t i=0;i<bar_group.size();i++ )
			bar_group[i]->SetGroup( bar_group );

	// create the axes
	if ( ndata == 1 )
	{
		// single value axis
		wxPLLabelAxis *x1 = new wxPLLabelAxis( -1, yvars.size() + 1, m_g.XLabel );
		for( size_t i=0;i<ynames.size();i++)
			x1->Add( i+1, m_d->GetLabel( ynames[i] ) );
		SetXAxis1( x1 );
	}
	else if ( ndata == 12 )
	{
		// month axis
		wxPLLabelAxis *x1 = new wxPLLabelAxis( -1, 12, m_g.XLabel );
		for( size_t i=0;i<12;i++ )
			x1->Add( i, s_monthNames[i] );
		SetXAxis1( x1 );
	}
	else
	{
		// linear axis
		SetXAxis1( new wxPLLinearAxis( -1, ndata+1, m_g.XLabel ) );
	}


	// setup y axis

	if ( GetPlotCount() > 0 )
	{
		double ymin, ymax;
		GetPlot(0)->GetMinMax( 0, 0, &ymin, &ymax );
		for( size_t i=1;i<GetPlotCount();i++ )
			GetPlot(i)->ExtendMinMax( 0, 0, &ymin, &ymax );

		if ( m_g.Type == Graph::STACKED || m_g.Type == Graph::BAR )
		{ // forcibly include the zero line for bar plots
			if ( ymin > 0 ) ymin = 0;
			if ( ymax < 0 ) ymax = 0;
		}
		
		double yadj = (ymax-ymin)*0.05;
		
		if (ymin != 0) ymin -= yadj;
		if (ymax != 0) ymax += yadj;

		SetYAxis1( new wxPLLinearAxis( ymin, ymax, m_g.YLabel ) );
	}

	
	Invalidate();
	Refresh();
}



enum { ID_Y = wxID_HIGHEST+495, ID_TYPE, ID_TITLE, ID_XLABEL, ID_YLABEL, ID_LEGEND, 
	ID_SCALE, ID_SIZE, ID_COARSE, ID_FINE };

BEGIN_EVENT_TABLE( GraphViewer, wxPanel )
	EVT_CHECKLISTBOX( ID_Y, GraphViewer::OnEdit )
	EVT_TEXT_ENTER( ID_TITLE, GraphViewer::OnEdit )
	EVT_TEXT_ENTER( ID_XLABEL, GraphViewer::OnEdit )
	EVT_TEXT_ENTER( ID_YLABEL, GraphViewer::OnEdit )
	EVT_RADIOBUTTON( ID_TYPE, GraphViewer::OnEdit )
	EVT_COMMAND_SCROLL( ID_SCALE, GraphViewer::OnSlider )
	EVT_COMMAND_SCROLL( ID_SIZE, GraphViewer::OnSlider )
	EVT_CHECKBOX( ID_COARSE, GraphViewer::OnEdit )
	EVT_CHECKBOX( ID_FINE, GraphViewer::OnEdit )
END_EVENT_TABLE()


GraphViewer::GraphViewer( wxWindow *parent )
	: wxPanel( parent )
{
	m_case = 0;
	m_data = 0;

	m_Y = new wxCheckListBox( this, ID_Y );
	m_type = new wxRadioChoice( this, ID_TYPE );
	m_type->SetHorizontal( true );
	m_type->Add( "Bar" );
	m_type->Add( "Stacked" );
	m_type->Add( "Line" );
	m_type->Add( "Scatter" );
	m_type->SetSelection( 0 );

	m_title = new wxExtTextCtrl( this, ID_TITLE );
	m_xlabel = new wxExtTextCtrl( this, ID_XLABEL );
	m_ylabel = new wxExtTextCtrl( this, ID_YLABEL );
	
	m_scale = new wxSlider( this, ID_SCALE, 10, 0, 20 );
	m_size = new wxSlider( this, ID_SIZE, 0, 0, 30 );

	m_coarse = new wxCheckBox( this, ID_COARSE, "Coarse grid" );
	m_coarse->SetValue( true );
	m_fine = new wxCheckBox( this, ID_FINE, "Fine grid" );
	m_fine->SetValue( true );

	m_scroll = new wxScrolledWindow( this, wxID_ANY );
	
	wxBoxSizer *sizer_left = new wxBoxSizer( wxVERTICAL );
	sizer_left->Add( m_type, 0, wxALL|wxEXPAND, 4 );
	sizer_left->Add( m_title, 0, wxALL|wxEXPAND, 4 );
	sizer_left->Add( m_xlabel, 0, wxALL|wxEXPAND, 4 );
	sizer_left->Add( m_ylabel, 0, wxALL|wxEXPAND, 4 );
	sizer_left->Add( m_size, 0, wxALL|wxEXPAND, 4 );
	sizer_left->Add( m_scale, 0, wxALL|wxEXPAND, 4 );
	sizer_left->Add( m_coarse, 0, wxALL|wxEXPAND, 4 );
	sizer_left->Add( m_fine, 0, wxALL|wxEXPAND, 4 );
	sizer_left->Add( m_Y, 1, wxALL|wxEXPAND, 4 );
	

	wxBoxSizer *sizer_main = new wxBoxSizer( wxHORIZONTAL );
	sizer_main->Add( sizer_left, 0, wxALL|wxEXPAND,  0 );
	sizer_main->Add( m_scroll, 1, wxALL|wxEXPAND, 0 );
	SetSizer( sizer_main );

	m_graph = new GraphCtrl( m_scroll, wxID_ANY );
	m_graph->SetClientSize( 700, 500 );
}

GraphViewer::~GraphViewer()
{
}

void GraphViewer::Setup( Case *c, DataProvider *dp )
{
	m_case = c;
	m_data = dp;

	Clear();

	if ( !m_case || !m_data ) return;

	m_names = dp->GetVariables();
	m_labels.Clear();
	for( size_t i=0;i<m_names.size();i++ )
		m_labels.Add( dp->GetLabel( m_names[i] ) );

	wxSortByLabels( m_names, m_labels );
	
	m_Y->Clear();
	m_Y->Append( m_labels );
}

void GraphViewer::Clear()
{
	for( size_t i=0;i<m_graphs.size();i++ )
		m_graphs[i]->Destroy();

	m_graphs.clear();

	m_xlabel->Clear();
	m_ylabel->Clear();
	m_title->Clear();
}

void GraphViewer::UpdateCurrentGraph()
{
	Graph g;
	for( size_t i=0;i<m_names.size();i++)
		if ( m_Y->IsChecked( i ) )
			g.Y.Add( m_names[i] );

	g.Type = m_type->GetSelection();
	g.Title = m_title->GetValue();
	g.XLabel = m_xlabel->GetValue();
	g.YLabel = m_ylabel->GetValue();
	g.FontScale = ((double)m_scale->GetValue())/10.0;
	g.Size = m_size->GetValue();
	g.CoarseGrid = m_coarse->GetValue();
	g.FineGrid = m_fine->GetValue();

	m_graph->Display( m_data, g );

}


void GraphViewer::OnEdit( wxCommandEvent & )
{
	UpdateCurrentGraph();
}

void GraphViewer::OnSlider( wxScrollEvent & )
{
	UpdateCurrentGraph();
}
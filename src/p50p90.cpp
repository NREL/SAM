#include <algorithm>

#include <wx/dir.h>
#include <wx/textctrl.h>
#include <wx/filename.h>
#include <wx/hyperlink.h>

#include <wex/snaplay.h>
#include <wex/extgrid.h>
#include <wex/numeric.h>
#include <wex/plot/plplotctrl.h>
#include <wex/plot/plbarplot.h>
#include <wex/plot/pllineplot.h>

#include <wex/plot/plplotctrl.h>
#include <wex/plot/plbarplot.h>

#include "simulation.h"
#include "p50p90.h"
#include "case.h"
#include "main.h"
#include "graph.h"
#include "results.h"

enum { ID_SELECT_FOLDER = wxID_HIGHEST+494,
	ID_SIMULATE };

BEGIN_EVENT_TABLE( P50P90Form, wxPanel )
	EVT_BUTTON( ID_SELECT_FOLDER, P50P90Form::OnSelectFolder )
	EVT_BUTTON( ID_SIMULATE, P50P90Form::OnSimulate )
END_EVENT_TABLE()	


P50P90Form::P50P90Form( wxWindow *parent, Case *cc )
	: wxPanel( parent ), m_case(cc)
{
	SetBackgroundColour( wxMetroTheme::Colour(wxMT_FOREGROUND) );

	wxBoxSizer *sizer_top = new wxBoxSizer( wxHORIZONTAL );
	sizer_top->Add( new wxMetroButton( this, ID_SIMULATE, "Run P50/P90 simulations", wxNullBitmap,wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 0 );
	sizer_top->AddSpacer( 150 );

	//sizer_top->Add( new wxHyperlinkCtrl( this, wxID_ANY, "Download historical weather data", SamApp::WebApi("historical_nsrdb") ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	wxStaticText *label = new wxStaticText( this, wxID_ANY, "Select weather file folder:" );
	label->SetForegroundColour( *wxWHITE );
	sizer_top->Add( label , 0, wxLEFT|wxRIGHT|wxALIGN_CENTER_VERTICAL, 0 );
	sizer_top->Add( m_folder = new wxTextCtrl( this, wxID_ANY ), 1, wxLEFT|wxRIGHT|wxALIGN_CENTER_VERTICAL, 3 );
	sizer_top->Add( new wxMetroButton( this, ID_SELECT_FOLDER, "..." ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 0 );

	label = new wxStaticText( this, wxID_ANY, "Custom Px:" );
	label->SetForegroundColour( *wxWHITE );

	m_puser = new wxNumericCtrl( this, wxID_ANY, 70.5, wxNUMERIC_REAL );		
	sizer_top->AddSpacer( 20 );
	sizer_top->Add( label , 0, wxLEFT|wxRIGHT|wxALIGN_CENTER_VERTICAL, 0 );
	sizer_top->Add( m_puser, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );

	m_layout = new wxSnapLayout( this, wxID_ANY );

	m_grid = new wxExtGridCtrl( m_layout, wxID_ANY );
	m_grid->CreateGrid( 1, 1 );
	m_grid->SetRowLabelAlignment(wxALIGN_RIGHT,wxALIGN_CENTRE);
	m_grid->SetDefaultCellAlignment(wxALIGN_RIGHT,wxALIGN_CENTRE);
	m_grid->DisableCellEditControl();
	m_grid->DisableDragCell();
	m_grid->DisableDragRowSize();
	m_grid->DisableDragColMove();
	m_grid->DisableDragGridSize();
	m_grid->SetCellValue(0,0,"No data.");
	m_grid->EnableEditing(false);
	m_grid->EnableCopyPaste(true);
	m_grid->EnablePasteEvent(false);

	m_layout->Add( m_grid );
		
	wxBoxSizer *sizer_main = new wxBoxSizer( wxVERTICAL );
	sizer_main->Add( sizer_top, 0, wxALL|wxEXPAND, 0 );
	sizer_main->Add( m_layout, 1, wxALL|wxEXPAND, 0 );
	SetSizer( sizer_main );
}

void P50P90Form::OnSimulate( wxCommandEvent & )
{
	
	std::vector<unsigned short> years;
	wxArrayString folder_files; 
	wxArrayString list;
	wxDir::GetAllFiles( m_folder->GetValue(), &list );

	if ( m_folder->GetValue().IsEmpty()
		|| !wxDirExists( m_folder->GetValue() ) 
		|| list.size() < 10 )
	{
		wxMessageBox("Please select a folder with at least 10 weather data files.\n\nSee Help for details and a link to NSRDB historical data.", "P50/P90 Simulations", wxOK, this );
		return;
	}

	int nthread = wxThread::GetCPUCount();

	SimulationDialog tpd( "Scanning...", nthread );
		
	for (int i=0;i<list.Count();i++)
	{
		tpd.Update( 0, (float)i/ (float)list.size() * 100.0f, wxString::Format("%d of %d", (int)(i+1), (int)list.size()  ) );
		wxYield();	

		wxString file = wxFileNameFromPath(list[i]);
		wxString ext = wxFileName(file).GetExt().Lower();
		if (ext != "tm2" && ext != "tm3" && ext != "csv" && ext != "smw" && ext != "srw" )
			continue; 

		long yrval = -1;
		int pos2 = file.find_last_of("."); //need to find the period that separates the file extension, not any other periods that may be present in the file name
		int pos1 = pos2 - 5;
		if (pos1 != wxNOT_FOUND && pos2 != wxNOT_FOUND
			&& pos2 > pos1
			&& file.Mid( pos1+1, pos2-pos1-1 ).ToLong(&yrval)
			&& yrval > 1900 )
		{
			years.push_back( (unsigned short)yrval );
			folder_files.Add( file );
		}
	}
	
	if (years.size() < 10)
	{
		wxMessageBox("It is not possible to empirically determine the P90 value with less than 10 years of weather data.");
		return;
	}

	// sort years and files together
	int count = (int)years.size();
	for (int i=0;i<count-1;i++)
	{
		int smallest = i;

		for (int j=i+1;j<count;j++)
			if ( years[j] < years[smallest] )
				smallest = j;

		// swap
		unsigned short yr = years[i];
		years[i] = years[smallest];
		years[smallest] = yr;

		wxString buf = folder_files[i];
		folder_files[i] = folder_files[smallest];
		folder_files[smallest] = buf;

	}


	// all single value outputs
	wxArrayString output_vars, output_labels, output_units;
	Simulation::ListAllOutputs( m_case->GetConfiguration(), 
		&output_vars, &output_labels, &output_units, NULL, true );

	
	tpd.NewStage( "Preparing simulations...", 1 );
	
	std::vector<Simulation*> sims;
	for (size_t n=0; n<years.size(); n++)
	{
		wxString weatherFile = m_folder->GetValue() + "/" + folder_files[n];

		Simulation *sim = new Simulation( m_case, wxString::Format("Year %d", (int)years[n]) );
		sims.push_back( sim );

		sim->Override( "use_specific_weather_file", VarValue(true) );
		sim->Override( "user_specified_weather_file", VarValue(weatherFile) );
		sim->Override("use_specific_wf_wind", VarValue(true));
		sim->Override("user_specified_wf_wind", VarValue(weatherFile));

		if ( !sim->Prepare() )
			wxMessageBox( wxString::Format("internal error preparing simulation %d for P50 / P90", (int)(n+1)) );

		tpd.Update( 0, (float)n / (float)years.size() * 100.0f, wxString::Format("%d of %d", (int)(n+1), (int)years.size()  ) );
		
		if ( tpd.Canceled() )
		{	
			// abort right away, delete sims, and return
			for( size_t i=0;i<sims.size();i++ )
				delete sims[i];

			return;
		}
	}


	tpd.NewStage( "Calculating..." );
	size_t nyearsok = Simulation::DispatchThreads( tpd, sims, nthread );
	
	tpd.NewStage( "Collecting outputs...", 1 );
	// all single value output data for each run
	matrix_t<double> output_data;
	output_data.resize_fill(years.size(), output_vars.Count(), 0.0);
	for( size_t n=0;n<sims.size();n++ )
	{
		for( size_t i=0;i<output_vars.size();i++ )
			if ( VarValue *vv = sims[n]->GetOutput( output_vars[i] ) )
				output_data.at( n, i ) = (double)vv->Value();
			
		tpd.Update( 0, (float)n / (float)sims.size() * 100.0f );
	}
	
	matrix_t<double> output_stats;

	double Puser = m_puser->Value();

	enum{ P10E, P50E, P90E, PUSR, MIN, MAX, STDDEV, P10N, P50N, P90N, NSTATS };
	output_stats.resize_fill( output_vars.size(), NSTATS, 0.0 );
	
	//*** Compute P50, P90, etc ***//
	if ( nyearsok == years.size() )
	{
		tpd.NewStage( "Processing results...", 1 );

		std::vector< std::vector<wxRealPoint> > cdfdata;
		std::vector<size_t> save_list;
		for ( size_t varIndex = 0; varIndex < output_vars.size(); varIndex++)
		{
			//Calculate P50, P90, etc.
			wxString varname = output_vars[varIndex];
			wxString varlabel = output_labels[varIndex];

			if (varlabel.IsEmpty())
				continue;

			double firstval = output_data(0,varIndex);
			std::vector<double> data;
			data.reserve(years.size());
			bool include = false;
			for (size_t nn=0; nn<years.size(); nn++)
			{
				data.push_back( output_data.at(nn, varIndex) );
				if ( firstval != output_data(nn,varIndex) )
					include = true;
			}

			tpd.Update( 0, (float)varIndex / (float)output_vars.size() * 100.0f );
			wxYield();

			if ( include )
			{
				save_list.push_back( varIndex );
			
				// mean, std dev, variance
				double sum = 0;
				for (size_t i=0; i<data.size(); i++)
					sum += data[i];
				double mean = sum / data.size();
				double variance = 0;
				for (size_t i=0; i<data.size(); i++)
					variance += pow(data[i] - mean, 2);
				variance /= data.size() - 1; //Use Bessel's Correction on the variance.
				double stdDev = sqrt(variance);
			
				double P90n = mean - (1.282 * stdDev);
				double P10n = mean + 1.282 * stdDev;

				output_stats.at( varIndex, STDDEV ) = stdDev;
				output_stats.at( varIndex, P90N ) = P90n;
				output_stats.at( varIndex, P50N ) = mean;
				output_stats.at( varIndex, P10N ) = P10n;

				//Now, let's interpolate P90.
				//This is to make a CDF but we can also use it to linearly interpolate P90.
				std::sort( data.begin(), data.end() );

				output_stats.at( varIndex, MIN ) = data[0];
				output_stats.at( varIndex, MAX ) = data[data.size()-1];
			
				std::vector<wxRealPoint> cdf1;
				// set percentiles array
				std::vector<double> darr;
				for (size_t i=0; i<data.size(); i++)
				{
					darr.push_back(100.0 * double(i+1) / double(data.size()));
					cdf1.push_back( wxRealPoint( data[i], darr[i] ) );
				}
				cdfdata.push_back( cdf1 );
				
				//Do linear interpolation.
				int below10index = floor(0.1 * data.size()) - 1;
				double slope = (data[below10index + 1] - data[below10index]) / (darr[below10index + 1] - darr[below10index]);
				double interpolatedP90 = data[below10index] + (10 - darr[below10index]) * slope;

				int below50index = floor(0.5 * data.size()) - 1;
				slope = (data[below50index + 1] - data[below50index]) / (darr[below50index + 1] - darr[below50index]);
				double interpolatedP50 = data[below50index] + (50 - darr[below50index]) * slope;

				int below90index = floor(0.9 * data.size()) - 1;
				slope = (data[below90index + 1] - data[below90index]) / (darr[below90index + 1] - darr[below90index]);
				double interpolatedP10 = data[below90index] + (90 - darr[below90index]) * slope;

				double interpolatedPuser = std::numeric_limits<double>::quiet_NaN();
				if ( Puser > 0 && Puser < 100 )
				{
					int idx = floor( 0.01*Puser * data.size()) - 1;
					slope = (data[idx + 1] - data[idx]) / (darr[idx + 1] - darr[idx]);
					interpolatedPuser = data[idx] + (Puser - darr[idx]) * slope;
				}
							
				output_stats.at( varIndex, P90E ) = interpolatedP90;
				output_stats.at( varIndex, P50E ) = interpolatedP50;
				output_stats.at( varIndex, P10E ) = interpolatedP10;
				output_stats.at( varIndex, PUSR ) = interpolatedPuser;
			}
		}


		// update results
		m_grid->Freeze();
		m_grid->ResizeGrid( save_list.size(), output_stats.ncols() );
		int row = 0;
		for( size_t i=0;i<save_list.size();i++ )
		{
			int index = save_list[i];
			wxString L( output_labels[index] );
			if ( !output_units[index].IsEmpty() )
				L += " (" + output_units[index] + ")";

			m_grid->SetRowLabelValue( i, L );
		}
		
		m_grid->SetColLabelValue( 0, "P10" );
		m_grid->SetColLabelValue( 1, "P50" );
		m_grid->SetColLabelValue( 2, "P90" );
		m_grid->SetColLabelValue( 3, wxString::Format("P%lg", Puser) );
		m_grid->SetColLabelValue( 4, "Min" );
		m_grid->SetColLabelValue( 5, "Max" );
		m_grid->SetColLabelValue( 6, "StdDev" );
		m_grid->SetColLabelValue( 7, "P10-norm" );
		m_grid->SetColLabelValue( 8, "P50-norm" );
		m_grid->SetColLabelValue( 9, "P90-norm" );
		
		for( size_t r=0;r<save_list.size();r++ )
			for( size_t c=0;c<output_stats.ncols();c++ )
				m_grid->SetCellValue( r, c, wxString::Format("%lg", output_stats( save_list[r],c)) );
		
		m_grid->SetRowLabelSize(wxGRID_AUTOSIZE);
		m_grid->SetColLabelSize(wxGRID_AUTOSIZE);
		m_grid->GetParent()->Layout();
		m_grid->Layout();
		m_grid->Thaw();
		
		// delete all the plots
		size_t i=0;
		while( i<m_layout->Count() )
		{
			if( wxPLPlotCtrl *plt = dynamic_cast<wxPLPlotCtrl*>( m_layout->Get(i) ) )
				m_layout->Delete( plt );
			else
				i++;
		}

		double emax = 1;
		std::vector<wxRealPoint> energy;
		for( size_t n=0;n<years.size();n++ )
		{
			if ( VarValue *vv = sims[n]->GetOutput("annual_energy") )
			{
				energy.push_back( wxRealPoint( years[n], vv->Value() ) );
				if ( vv->Value() > emax ) emax = vv->Value();
			}
		}

		if ( energy.size() > 1 )
		{
			wxPLPlotCtrl *plot = new wxPLPlotCtrl( m_layout, wxID_ANY, wxDefaultPosition, wxScaleSize(450,200) );
			plot->AddPlot( new wxPLBarPlot( energy, 0, wxEmptyString ) );
			wxPLLabelAxis *x1 = new wxPLLabelAxis( years[0]-1, years[years.size()-1]+1, "Year" );
			for( size_t i=0;i<years.size();i++ )
				x1->Add( years[i], wxString::Format("%d", (int)years[i]) );
			plot->SetXAxis1( x1 );
			plot->Y1().SetLabel( "Annual Energy (kWh)" );
			plot->Y1().SetWorldMax( 1.1*emax );
			plot->ShowLegend( false );
			plot->Invalidate();
			m_layout->Add(plot);
		}

		std::vector<wxColour> &colours = Graph::Colours();

		wxArrayString units;
		std::vector<wxPLPlotCtrl*> cdfplots;
		for( size_t i=0;i<cdfdata.size();i++ )
		{
			int index = save_list[i]; // cdfdata and save_list are same size
			if ( output_units[index].IsEmpty() )
				continue;

			wxPLPlotCtrl *plot = NULL;
			for( size_t j=0;j<units.size();j++ )
				if ( output_units[index] == units[j] )
					plot = cdfplots[j];

			if ( !plot )
			{
				plot = new wxPLPlotCtrl( m_layout, wxID_ANY, wxDefaultPosition, wxScaleSize(450,200) );
				cdfplots.push_back(plot);
				units.Add( output_units[index] );
				m_layout->Add( plot );
			}

			wxColour C( colours[ plot->GetPlotCount() % colours.size() ] );
			plot->AddPlot( new wxPLLinePlot( cdfdata[i], output_labels[index], C, wxPLLinePlot::SOLID, 2.0 ) );

			plot->X1().SetLabel( output_units[index] );
			plot->Y1().SetLabel( "Percent" );			
		}

	}
	else
	{
		tpd.Log(wxString::Format("Not all simulations completed successfully. (%d of %d OK)", (int)nyearsok, (int)years.size()));
	}

	for( size_t i=0;i<sims.size();i++ )
		delete sims[i];


	tpd.Finalize();
	
	m_layout->AutoLayout();
}

void P50P90Form::OnSelectFolder( wxCommandEvent & )
{
	wxString dir = wxDirSelector("Choose weather file folder", m_folder->GetValue());
	if ( !dir.IsEmpty() )
		m_folder->ChangeValue( dir );
}
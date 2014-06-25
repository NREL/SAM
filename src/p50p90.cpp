#include <algorithm>

#include <wx/dir.h>
#include <wx/textctrl.h>
#include <wx/filename.h>

#include <wex/snaplay.h>
#include <wex/extgrid.h>

#include <wex/plot/plplotctrl.h>
#include <wex/plot/plbarplot.h>

#include "simulation.h"
#include "p50p90.h"
#include "case.h"
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
	wxBoxSizer *sizer_top = new wxBoxSizer( wxHORIZONTAL );
	sizer_top->Add( new wxStaticText( this, wxID_ANY, "Select weather file folder:" ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	sizer_top->Add( m_folder = new wxTextCtrl( this, wxID_ANY ), 1, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
	sizer_top->Add( new wxButton( this, ID_SELECT_FOLDER, "...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	sizer_top->Add( new wxButton( this, ID_SIMULATE, "Simulate P50/P90", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );

	m_output = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY );
	m_output->SetInitialSize( wxSize(600, 300) );

	m_grid = new wxExtGridCtrl( this, wxID_ANY );
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
		
	m_layout = new wxSnapLayout( this, wxID_ANY );

	wxBoxSizer *sizer_bot = new wxBoxSizer( wxHORIZONTAL );
	sizer_bot->Add( m_grid, 1, wxALL|wxEXPAND, 0 );
	sizer_bot->Add( m_layout, 1, wxALL|wxEXPAND, 0 );

	wxBoxSizer *sizer_main = new wxBoxSizer( wxVERTICAL );
	sizer_main->Add( sizer_top, 0, wxALL|wxEXPAND, 5 );
	sizer_main->Add( m_output, 0, wxALL|wxEXPAND, 4 );
	sizer_main->Add( sizer_bot, 1, wxALL|wxEXPAND, 4 );
	SetSizer( sizer_main );
}

void P50P90Form::OnSimulate( wxCommandEvent & )
{
	m_output->Clear();
	std::vector<unsigned short> years;
	wxArrayString folder_files; 
	wxArrayString list;
	
	wxDir::GetAllFiles( m_folder->GetValue(), &list );
	for (int i=0;i<list.Count();i++)
	{
		wxString file = wxFileNameFromPath(list[i]);
		wxString ext = wxFileName(file).GetExt().Lower();
		if (ext != "tm2" && ext != "tm3" && ext != "csv" && ext != "smw" && ext != "smw" )
			continue; 

		long yrval = -1;
		int pos1 = file.Find('_');
		int pos2 = file.Find('.');
		if (pos1 != wxNOT_FOUND && pos2 != wxNOT_FOUND
			&& pos2 > pos1
			&& file.Mid( pos1+1, pos2-pos1-1 ).ToLong(&yrval)
			&& yrval > 1900 )
		{
			years.push_back( (unsigned short)yrval );
			folder_files.Add( file );
		}
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

	
	if (years.size() < 13) {
		wxMessageBox( "Insufficient number of years of weather data found: at least 13 required for P50/P90 analysis.");
		return;
	}


	// all single value outputs
	wxArrayString output_vars, output_labels, output_units;
	Simulation::ListAllOutputs( m_case->GetConfiguration(), 
		&output_vars, &output_labels, &output_units, true );

	// all single value output data for each run

	matrix_t<double> output_data;
	output_data.resize_fill(years.size(), output_vars.Count(), 0.0);

	std::vector<Simulation*> sims;
	size_t nyears = 0;
	for (size_t n=0; n<years.size(); n++)
	{

		wxString weatherFile = m_folder->GetValue() + "/" + folder_files[n];

		Simulation *sim = new Simulation( m_case, wxString::Format("year %d", (int)years[n]) );
		sim->Override( "use_specific_weather_file", VarValue(true) );
		sim->Override( "user_specified_weather_file", VarValue(weatherFile) );

		m_output->AppendText( wxString::Format("Simulating %d (%d of %d)...\n", (int)years[n], n+1, (int)years.size() ) );
		if ( sim->Invoke( true ) )
		{
			nyears++;
			// save all single value outputs
			for( size_t i=0;i<output_vars.size();i++ )
				if ( VarValue *vv = sim->GetOutput( output_vars[i] ) )
					output_data.at( n, i ) = (double)vv->Value();
		}
		else
			m_output->AppendText( wxJoin(sim->GetErrors(),'\n') );

		sims.push_back( sim );

		wxSafeYield( 0, true );
	}


	matrix_t<double> output_stats;

	enum{ P50E, P90E, MIN, MAX, STDDEV, P50N, P90N, NSTATS };
	output_stats.resize_fill( output_vars.size(), NSTATS, 0.0 );
	
	//*** Compute P50, P90, etc ***//
	if ( nyears == years.size() )
	{
		m_output->AppendText("Processing results.\n");
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

				output_stats.at( varIndex, STDDEV ) = stdDev;
				output_stats.at( varIndex, P90N ) = P90n;
				output_stats.at( varIndex, P50N ) = mean;

				//Now, let's interpolate P90.
				//This is to make a CDF but we can also use it to linearly interpolate P90.
				std::sort( data.begin(), data.end() );

				output_stats.at( varIndex, MIN ) = data[0];
				output_stats.at( varIndex, MAX ) = data[data.size()-1];
			
				// set percentiles array
				std::vector<double> darr;
				for (size_t i=0; i<data.size(); i++)
					darr.push_back(100.0 * double(i+1) / double(data.size()));

				//Do linear interpolation.
				int below10index = floor(0.1 * data.size()) - 1;
				double slope = (data[below10index + 1] - data[below10index]) / (darr[below10index + 1] - darr[below10index]);
				double interpolatedP90 = data[below10index] + (10 - darr[below10index]) * slope;

				int below50index = floor(0.5 * data.size()) - 1;
				slope = (data[below50index + 1] - data[below50index]) / (darr[below50index + 1] - darr[below50index]);
				double interpolatedP50 = data[below50index] + (50 - darr[below50index]) * slope;
				
			
				output_stats.at( varIndex, P90E ) = interpolatedP90;
				output_stats.at( varIndex, P50E ) = interpolatedP50;
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
		
		m_grid->SetColLabelValue( 0, "P50" );
		m_grid->SetColLabelValue( 1, "P90" );
		m_grid->SetColLabelValue( 2, "Min" );
		m_grid->SetColLabelValue( 3, "Max" );
		m_grid->SetColLabelValue( 4, "StdDev" );
		m_grid->SetColLabelValue( 5, "P50-norm" );
		m_grid->SetColLabelValue( 6, "P90-norm" );
		
		for( size_t r=0;r<save_list.size();r++ )
			for( size_t c=0;c<output_stats.ncols();c++ )
				m_grid->SetCellValue( r, c, wxString::Format("%lg", output_stats( save_list[r],c)) );
		
		m_grid->SetRowLabelSize(wxGRID_AUTOSIZE);
		m_grid->SetColLabelSize(wxGRID_AUTOSIZE);
		m_grid->GetParent()->Layout();
		m_grid->Layout();
		m_grid->Thaw();

		wxWindowList &wl = m_layout->GetChildren();
		for( wxWindowList::iterator it = wl.begin(); it!=wl.end();++it )
			(*it)->Destroy();

		std::vector<wxRealPoint> energy;
		for( size_t n=0;n<years.size();n++ )
			if ( VarValue *vv = sims[n]->GetOutput("annual_energy") )
				energy.push_back( wxRealPoint( years[n], vv->Value() ) );

		wxPLPlotCtrl *plot = new wxPLPlotCtrl( m_layout, wxID_ANY, wxDefaultPosition, wxSize(600,400) );
		plot->AddPlot( new wxPLBarPlot( energy, "Annual Energy" ) );
		plot->GetXAxis1()->SetWorldMin( plot->GetXAxis1()->GetWorldMin()-1 );
		plot->GetXAxis1()->SetWorldMax( plot->GetXAxis1()->GetWorldMax()+1 );
		plot->GetYAxis1()->SetWorldMin( 0 );
		plot->SetLegendLocation( wxPLPlotCtrl::BOTTOM );
		plot->Invalidate();

		m_layout->Add( plot );
	}
	else
	{
		wxMessageBox("not all simulations succeeded.  statistics not calculated.");
	}

	/*
	wxFrame *frame = new wxFrame( this, wxID_ANY, wxString::Format("Simulation results for year %d", years[0] ), wxDefaultPosition, wxSize(700, 400 ) );
	ResultsViewer *viwer = new ResultsViewer( frame, wxID_ANY );
	viwer->Setup( sims[0] );
	frame->Show();
	*/

	for( size_t i=0;i<sims.size();i++ )
		delete sims[i];
}

void P50P90Form::OnSelectFolder( wxCommandEvent & )
{
	wxDirDialog dlg( this, "Choose weather file folder", m_folder->GetValue(),
		wxDD_DEFAULT_STYLE | wxDD_DIR_MUST_EXIST );

	if ( dlg.ShowModal() == wxID_OK )
		m_folder->ChangeValue( dlg.GetPath() );
}
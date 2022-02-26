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

#include <wx/dir.h>
#include <wx/textctrl.h>
#include <wx/filename.h>
#include <wx/hyperlink.h>
#include <wx/clipbrd.h>
#include <wx/busyinfo.h>


#include <wex/snaplay.h>
#include <wex/extgrid.h>
#include <wex/numeric.h>
#include <wex/plot/plplotctrl.h>
#include <wex/plot/plbarplot.h>
#include <wex/plot/pllineplot.h>

#include <wex/plot/plplotctrl.h>
#include <wex/plot/plbarplot.h>

#include "simulation.h"
#include "pvuncertainty.h"
#include "case.h"
#include "main.h"
#include "graph.h"
#include "results.h"

enum { ID_SELECT_FOLDER = wxID_HIGHEST+494,
	ID_SIMULATE, ID_COPYTABLE };

BEGIN_EVENT_TABLE( PVUncertaintyForm, wxPanel )
	EVT_BUTTON( ID_SELECT_FOLDER, PVUncertaintyForm::OnSelectFolder )
	EVT_BUTTON(ID_SIMULATE, PVUncertaintyForm::OnSimulate)
	EVT_BUTTON(ID_COPYTABLE, PVUncertaintyForm::OnCopyTable)
END_EVENT_TABLE()


PVUncertaintyForm::PVUncertaintyForm( wxWindow *parent, Case *cc )
	: wxPanel( parent ), m_case(cc)
{
	SetBackgroundColour( wxMetroTheme::Colour(wxMT_FOREGROUND) );

	wxBoxSizer *sizer_top = new wxBoxSizer( wxHORIZONTAL );
	sizer_top->Add( new wxMetroButton( this, ID_SIMULATE, "Run PV uncertainty simulations", wxNullBitmap,wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 0 );
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

	sizer_top->Add(new wxMetroButton(this, ID_COPYTABLE, "Copy table to clipboard", wxNullBitmap, wxDefaultPosition, wxDefaultSize), 0, wxALL | wxALIGN_CENTER_VERTICAL, 0);

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

void PVUncertaintyForm::OnSimulate( wxCommandEvent & )
{
	
	std::vector<unsigned short> years;
	wxArrayString folder_files; 
	wxArrayString list;
	wxDir::GetAllFiles( m_folder->GetValue(), &list );

	if ( m_folder->GetValue().IsEmpty()
		|| !wxDirExists( m_folder->GetValue() ) 
		|| list.size() < 10 )
	{
		wxMessageBox(wxString::Format("Please choose a folder!\nYou either did not choose a folder, or the folder you chose has less than 10 weather files.",list.size()), "P50/P90 Simulations", wxOK, this );
		return;
	}

	int nthread = wxThread::GetCPUCount();

	SimulationDialog tpd( "Scanning...", nthread );
		
	for (int i=0;i<(int)list.Count();i++)
	{
		tpd.Update( 0, (float)i/ (float)list.size() * 100.0f, wxString::Format("%d of %d", (int)(i+1), (int)list.size()  ) );
		wxYield();	

		wxString file = wxFileNameFromPath(list[i]);
		wxString ext = wxFileName(file).GetExt().Lower();
        if (ext != "tm2" && ext != "epw" && ext != "csv" && ext != "smw" && ext != "srw")
        {
            wxMessageBox(wxString::Format("Invalid file!\nP50/P90 simulations do not work with the %s file extension. Only csv, srw, smw, epw, and tm2 file extensions are supported. Please remove any files with invalid extensions from the weather file folder.",ext), "P50/P90 Simulations", wxOK, this);
            return;
        }

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
		wxMessageBox(wxString::Format("Insufficient number of files!\nThe folder you chose has less than 10 files with correctly formatted file names. Please be sure that all file names in the folder include the year preceeded by an underscore like \"filename_2008.csv\". Folder contains %d files with valid file names.", years.size() ), "P50/P90 Simulations", wxOK, this);
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
		&output_vars, &output_labels, &output_units, NULL, NULL, true );

	
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
			wxMessageBox( wxString::Format("Internal error preparing simulation %d for P50/P90.", (int)(n+1)) );

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
	bool Puser_flag_1 = false;
	bool Puser_flag_2 = false; //flags for error reporting for the P-XX P-user input
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
				if (Puser > 0 && Puser < 100)
				{
					//there is a minimum/maximum empirical P-value that can be calculated based on the number of weather files because of the interpolation that happens.
					//for example, if the user has 20 weather files and asks for a P97, the first weather file in the series represents the P95, and there is nothing lower to interpolate to.
					//rather than extrapolate, report an error to the user. jmf 1/3/17
					int n = data.size(); //n is the number of weather files
					double limit = 100.0 / n; //this is the upper limit of the P-value that can be calculated; i.e. if there are 20 weather files, the p-limit is P-5. Need 100.0 to force computer to do division in double precision.
					if (Puser < limit || Puser >(100 - limit))
						Puser_flag_1 = true;
					else
					{
						//you'll notice above that the below90index corresponds to the interpolated P10, so the reversal step needs to be factored in for the P-user here but was missing. Fixed 1/3/17 jmf.
						int idx = floor(0.01*(100 - Puser) * data.size()) - 1;
						slope = (data[idx + 1] - data[idx]) / (darr[idx + 1] - darr[idx]);
						interpolatedPuser = data[idx] + ((100 - Puser) - darr[idx]) * slope;
					}
				}
				else
					Puser_flag_2 = true;
							
				output_stats.at( varIndex, P90E ) = interpolatedP90;
				output_stats.at( varIndex, P50E ) = interpolatedP50;
				output_stats.at( varIndex, P10E ) = interpolatedP10;
				output_stats.at( varIndex, PUSR ) = interpolatedPuser;
			}
		}

		//error messages must be reported outside of the loop or you'll get a bunch of them
		if (Puser_flag_1)
		{
			int n = years.size(); //n is the number of weather files
			double limit = 100.0 / n; //this is the upper limit of the P-value that can be calculated; i.e. if there are 20 weather files, the p-limit is P-5. Need 100.0 to force computer to do division in double precision.
			wxMessageBox(wxString::Format("Custom P-values must be between %.1f and %.1f because you have %d weather files. Please increase the number of weather files or change the custom P-value.", limit, 100 - limit, n));
		}
		if (Puser_flag_2)
			wxMessageBox("Custom P-values must be greater than 0 and less than 100.");


		// update results
		m_grid->Freeze();
		m_grid->ResizeGrid( save_list.size(), output_stats.ncols() );
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

void PVUncertaintyForm::OnSelectFolder(wxCommandEvent&)
{
	wxString dir = wxDirSelector("Choose weather file folder", m_folder->GetValue());
	if (!dir.IsEmpty())
		m_folder->ChangeValue(dir);
}

void PVUncertaintyForm::OnCopyTable(wxCommandEvent&)
{
	wxBusyInfo busy("Processing data table... please wait");
	wxString dat = "";
	GetTextData(dat, '\t');

	// strip commas per request from Paul 5/23/12 meeting
	dat.Replace(",", "");

	if (wxTheClipboard->Open())
	{
		wxTheClipboard->Clear();
		wxTheClipboard->SetData(new wxTextDataObject(dat));
		wxTheClipboard->Close();
	}
}

void PVUncertaintyForm::GetTextData(wxString& dat, char sep, bool withHeader)
{
	dat = wxEmptyString;
	if (!m_grid)
		return;

	wxGridTableBase* m_grid_data = m_grid->GetTable();
//	size_t approxbytes = (m_grid_data->GetNumberRows() +1) * 15 * (m_grid_data->GetNumberCols()+1);
//	dat.Alloc(approxbytes);

	int c;
	wxString label = "";
	if (sep == ',')
		dat += '"' + label + '"';
	else
		dat += label;
	dat += sep;
	// column header
	if (withHeader) {
		for (c = 0; c < m_grid_data->GetNumberCols(); c++)
		{
			label = m_grid_data->GetColLabelValue(c);
			label.Replace('\n', " | ");

			if (sep == ',')
				dat += '"' + label + '"';
			else
				dat += label;

			if (c < m_grid_data->GetNumberCols() - 1)
				dat += sep;
			else
				dat += '\n';
		}
	}
	// data
	for (int r = 0; r < m_grid_data->GetNumberRows(); r++)
	{
		auto rowlabel =  m_grid_data->GetRowLabelValue(r);
		if (sep == ',')
			dat += '"' + rowlabel + '"';
		else
			dat += rowlabel;
		dat += sep;

		for (c = 0; c < m_grid_data->GetNumberCols(); c++)
		{
			dat += m_grid_data->GetValue(r, c);

			if (c < m_grid_data->GetNumberCols() - 1)
				dat += sep;
			else
				dat += '\n';
		}
	}
}


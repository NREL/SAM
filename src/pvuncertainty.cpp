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
#include <wx/stattext.h>
#include <wx/filename.h>
#include <wx/hyperlink.h>
#include <wx/clipbrd.h>
#include <wx/busyinfo.h>
#include <wx/richtooltip.h>
#include <wx/tokenzr.h>


#include <wex/snaplay.h>
#include <wex/extgrid.h>
#include <wex/numeric.h>
#include <wex/plot/plplotctrl.h>
#include <wex/plot/plbarplot.h>
#include <wex/plot/pllineplot.h>
#include <wex/plot/plhistplot.h>
#include <wex/dview/dvpncdfctrl.h>

#include "simulation.h"
#include "stochastic.h"
#include "pvuncertainty.h"
#include "case.h"
#include "main.h"
#include "graph.h"
#include "results.h"
#include "widgets.h"

enum { ID_SELECT_FOLDER = wxID_HIGHEST+494,
	ID_SIMULATE, ID_COPYTABLE, ID_SETPVALUE };

BEGIN_EVENT_TABLE( PVUncertaintyForm, wxPanel )
	EVT_BUTTON( ID_SELECT_FOLDER, PVUncertaintyForm::OnSelectFolder )
	EVT_BUTTON(ID_SIMULATE, PVUncertaintyForm::OnSimulate)
	EVT_BUTTON(ID_SETPVALUE, PVUncertaintyForm::OnSetPValue)
	EVT_BUTTON(ID_COPYTABLE, PVUncertaintyForm::OnCopyTable)
END_EVENT_TABLE()



PVUncertaintyForm::PVUncertaintyForm( wxWindow *parent, Case *cc )
	: wxPanel( parent ), m_case(cc)
{
	SetBackgroundColour( *wxWHITE );

	wxBoxSizer *sizer_top = new wxBoxSizer( wxHORIZONTAL );
	sizer_top->Add( new wxMetroButton( this, ID_SIMULATE, "Run PV uncertainty simulations", wxNullBitmap,wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW ), 0, wxALL, 0 );
	sizer_top->AddSpacer( 150 );

//	sizer_top->Add(new wxMetroButton(this, ID_COPYTABLE, "Copy table to clipboard", wxNullBitmap, wxDefaultPosition, wxDefaultSize), 0, wxALL | wxALIGN_CENTER_VERTICAL, 0);

    wxStaticBoxSizer *sizer_inputs = new wxStaticBoxSizer( wxVERTICAL, this, "Sources of Uncertainty" );

    // add sources of uncertainty and information to show with tool tip
    std::vector< std::tuple<std::string, std::string, std::string > > sourceinfo;
	sourceinfo.push_back(std::make_tuple("Source #1", "Uncertainty source with an initial normal distribution with a 10% uncertainty with a 1% standard deviation", "Source #1:1:10:1:0:0"));
	sourceinfo.push_back(std::make_tuple("Source #2", "Uncertainty source with an initial normal distribution with a 5% uncertainty with a 2% standard deviation", "Source #2:1:5:2:0:0"));
	sourceinfo.push_back(std::make_tuple("Source #3", "Uncertainty source with an initial normal distribution with a 2% uncertainty with a 0.5% standard deviation", "Source #3:1:2:0.5:0:0"));

	m_sd = StochasticData(); // defaults to 100 samples and 0 seed

	for (size_t i = 0; i < sourceinfo.size(); i++) {
		m_uncertaintySources.push_back(new UncertaintySource(this, std::get<0>(sourceinfo[i]), std::get<1>(sourceinfo[i]), std::get<2>(sourceinfo[i])));
		sizer_inputs->Add(m_uncertaintySources[i], 1, wxALL, 5);
		m_sd.InputDistributions.push_back(m_uncertaintySources[i]->m_infoDistDialog); // TODO clean this up
	}

		
    wxStaticBoxSizer *sizer_interannual = new wxStaticBoxSizer( wxVERTICAL, this, "Interannual Variability" );

    wxBoxSizer *sizer_weather_file = new wxBoxSizer(wxHORIZONTAL);
    wxStaticText *label = new wxStaticText( this, wxID_ANY, "Select weather file folder:" );
    sizer_weather_file->Add( label , 0, wxLEFT|wxRIGHT|wxALIGN_CENTER_VERTICAL, 0 );
	sizer_weather_file->Add(m_folder = new wxTextCtrl(this, wxID_ANY), 0, wxEXPAND | wxALL, 3);
	sizer_weather_file->Add( new wxButton( this, ID_SELECT_FOLDER, "..." ), 0, wxLEFT|wxALIGN_CENTER_VERTICAL, 0 );
	sizer_weather_file->SetSizeHints(m_folder);
    sizer_interannual->Add(sizer_weather_file, 0, wxEXPAND | wxALL, 2);
    sizer_interannual->Add( new wxHyperlinkCtrl( this, wxID_ANY, "Download files from NSRDB for my location", SamApp::WebApi("historical_nsrdb") ), 0, wxALL, 0 );
     
    wxStaticBoxSizer *sizer_changePvalue = new wxStaticBoxSizer( wxHORIZONTAL, this, "Update P value" );
    label = new wxStaticText( this, wxID_ANY, "Custom Px:" );
    m_puser = new wxNumericCtrl( this, wxID_ANY, 90, wxNUMERIC_REAL );
    sizer_changePvalue->AddSpacer( 20 );
    sizer_changePvalue->Add( label , 0, wxLEFT|wxRIGHT|wxALIGN_CENTER_VERTICAL, 0 );
    sizer_changePvalue->Add( m_puser, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
    sizer_changePvalue->Add( new wxButton( this, ID_SETPVALUE, "Change P-value" ), 0, wxALL, 0 );


	m_layout = new wxSnapLayout(this, wxID_ANY);

    wxBoxSizer *sizer_main = new wxBoxSizer( wxVERTICAL );
	sizer_main->Add( sizer_top, 0, wxALL|wxEXPAND, 5 );
    sizer_main->Add( sizer_inputs, 0, wxALL|wxEXPAND, 5 );
    sizer_main->Add( sizer_interannual, 0, wxALL|wxEXPAND, 5 );
	sizer_main->Add(sizer_changePvalue, 0, wxALL | wxEXPAND, 5);
	sizer_main->Add(m_layout, 0, wxALL | wxEXPAND, 5);
	SetSizer( sizer_main );
}

PVUncertaintyForm::~PVUncertaintyForm()
{
	/*
    for (size_t i = 0; m_tsDataSets.size(); i++)
        if (m_tsDataSets[i]) delete m_tsDataSets[i];
	*/
}

void PVUncertaintyForm::OnSetPValue(wxCommandEvent&)
{
	double pValue;
	if (!m_puser->GetValue().ToDouble(&pValue)) pValue = 90;
	SetPValue(pValue);
}

void PVUncertaintyForm::SetPValue(double pValue)
{
	m_pnCdfAll->SetPValue(pValue);
	m_pnCdfIV->SetPValue(pValue);
	m_pnCdfUS->SetPValue(pValue);
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


	// all single value outputs - initially use annual energy only - possibly add more later
	wxArrayString output_vars, output_labels, output_units;
	wxArrayString output_vars_all, output_labels_all, output_units_all;
	Simulation::ListAllOutputs( m_case->GetConfiguration(), &output_vars_all, &output_labels_all, &output_units_all, NULL, NULL, true );
	for (size_t i = 0; i < output_vars_all.size(); i++) {
		if (output_vars_all[i] == "annual_energy") {
			output_vars.push_back(output_vars_all[i]);
			output_labels.push_back(output_labels_all[i]);
			output_units.push_back(output_units_all[i]);
		}
	}

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

	// generate samples
	// update to new distributions
	for (size_t i = 0; i < m_uncertaintySources.size(); i++) {
		m_sd.InputDistributions[i] = m_uncertaintySources[i]->m_infoDistDialog;
	}
    
    // change sample size from default of 100 to 10000 must change in header m_pUS
    m_sd.N = 10000;
	wxArrayString errors;
	ComputeLHSInputVectors(m_sd, output_stats, &errors);

	if (output_stats.nrows() != 10000) {
		// throw ?
		return;
	}
	// set uncertainty sources combined factor
	size_t sizeUS = output_stats.nrows();
	for (size_t i = 0; i < sizeUS; i++) {
		double combined_factor = 1;
		for (size_t j = 0; j < output_stats.ncols(); j++) {
			// compute combined factors = product( 1 - sample/100.0) for each uncertainty source sample value
			combined_factor *= (1.0 - (output_stats(i, j) / 100.0));
		}
		m_pUS[i] = combined_factor;
	}
	// set interannual variability m_pIV
	size_t sizeIV = years.size();
	for (size_t n = 0; n < sizeIV; n++){
		if (VarValue* vv = sims[n]->GetOutput("annual_energy"))
			m_pIV[n] =  vv->Value();
	}
	// set overall uncertainty m_pAll
	size_t sizeAll = sizeUS * sizeIV;
	for (size_t i = 0; i < sizeUS; i++) {
		for (size_t n = 0; n < sizeIV; n++) {
			m_pAll[i + n * sizeUS] = m_pUS[i] * m_pIV[n];
		}
	}


	/*
    m_uncertaintySourcesFactor.clear();
    m_uncertaintySourcesFactor.reserve(output_stats.nrows());
    for (size_t i = 0; i < output_stats.nrows(); i++) {
        double combined_factor = 1;
        for (size_t j = 0; j < output_stats.ncols(); j++)    {
            // compute combined factors = product( 1 - sample/100.0) for each uncertainty source sample value
            combined_factor *= (1.0 - (output_stats(i,j)/100.0));
        }
        m_uncertaintySourcesFactor.push_back(combined_factor);
    }
	*/
    if (nyearsok == years.size())
    {
    
    /*
	// delete all the grids
	size_t i = 0;
	while (i < m_layout->Count())
	{
		if (wxExtGridCtrl* grid = dynamic_cast<wxExtGridCtrl*>(m_layout->Get(i)))
			m_layout->Delete(grid);
		else
			i++;
	}



	wxExtGridCtrl* grid = new wxExtGridCtrl(m_layout, wxID_ANY);
	grid->EnableCopyPaste(true);
	grid->CreateGrid(output_stats.nrows(), output_stats.ncols());
	grid->Freeze();
	// for string value variables - show string values (e.g. lists - array type, weather files,...)
    for (size_t i = 0; i < output_stats.nrows(); i++) {
        double combined_factor = 1;
        for (size_t j = 0; j < output_stats.ncols(); j++)    {
            // compute combined factors = product( 1 - sample/100.0) for each uncertainty source sample value
            combined_factor *= (1.0 - (output_stats(i,j)/100.0));
			grid->SetCellValue(i, j, wxString::Format("%lg", output_stats(i, j)));
        }
        uncertainty_sources_combined_factor.push_back(combined_factor);
	}
        

	wxArrayString collabels;
	for (size_t i = 0; i < m_sd.InputDistributions.Count(); i++)
	{
		wxString label = GetVarNameFromInputDistribution(m_sd.InputDistributions[i]);
		collabels.Add(label);
	}
	for (size_t i = 0; i < output_stats.ncols(); i++)
		grid->SetColLabelValue(i, collabels[i]);
	grid->AutoSize();
	grid->Thaw();

	m_layout->Add(grid);
     
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
			plot->SetTitle("Interannual Variablity");
			plot->Invalidate();
			m_layout->Add(plot);
		}

        if ( m_uncertaintySourcesFactor.size() > 0 )
        {
            wxPLPlotCtrl *plot = new wxPLPlotCtrl( m_layout, wxID_ANY, wxDefaultPosition, wxScaleSize(450,200) );
            auto hist =new wxPLHistogramPlot();
            std::vector<wxRealPoint> data;
            data.reserve(m_uncertaintySourcesFactor.size());
            // this is inefficient
            for (size_t i=0; i<m_uncertaintySourcesFactor.size(); i++)
                data.push_back(wxRealPoint(i,m_uncertaintySourcesFactor[i]));
            hist->SetData(data);
            hist->SetNumberOfBins(hist->GetFreedmanDiaconisBinsFor(data.size()));

            plot->AddPlot( hist );
            plot->ShowLegend( false );
            plot->SetTitle("Uncertainty Sources");
            plot->Invalidate();
            m_layout->Add(plot);
        }
*/
        // delete all the plots
        size_t i=0;
        while( i<m_layout->Count() )
        {
            if( wxDVPnCdfCtrl *plt = dynamic_cast<wxDVPnCdfCtrl*>( m_layout->Get(i) ) )
                m_layout->Delete( plt );
            else
                i++;
        }
// delete existing timeseries data (also in destructor) 
/*
		for (size_t i = 0; m_tsDataSets.size(); i++)
            if (m_tsDataSets[i]) delete m_tsDataSets[i];
        m_tsDataSets.clear();
 */       
 //       if ( m_uncertaintySourcesFactor.size() > 0 )
 //       {



           // double p[m_uncertaintySourcesFactor.size()];
            // this is inefficient
            //for (size_t i=0; i<m_uncertaintySourcesFactor.size(); i++)
            //    m_p[i] = m_uncertaintySourcesFactor[i];
            //m_tsDataSets.push_back(new TimeSeriesData(m_pUS, m_uncertaintySourcesFactor.size(), 1, 0, "Uncertainty Sources", "fraction"));
            //pnCdf->AddDataSet(m_tsDataSets.back(), true);
		m_pnCdfAll = new wxDVPnCdfCtrl(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL, "panel", false, false, false, false);
		m_pnCdfAll->AddDataSet(new TimeSeriesData(m_pAll, sizeAll, 1, 0, "Overall uncertainty", "Energy (kWh)"), true);
		m_pnCdfAll->SelectDataSetAtIndex(0);
		m_layout->Add(m_pnCdfAll);
		m_pnCdfIV = new wxDVPnCdfCtrl(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL, "panel", false, false, false, false);
		m_pnCdfIV->AddDataSet(new TimeSeriesData(m_pIV, sizeIV, 1, 0, "Interannual variablility", "Energy (kWh)"), true);
		m_pnCdfIV->SelectDataSetAtIndex(0);
		m_layout->Add(m_pnCdfIV);
		m_pnCdfUS = new wxDVPnCdfCtrl(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL, "panel", false, false, false, false);
		m_pnCdfUS->AddDataSet(new TimeSeriesData(m_pUS, sizeUS, 1, 0, "Uncertainty Sources", "fraction"), true);
		m_pnCdfUS->SelectDataSetAtIndex(0);
        m_layout->Add(m_pnCdfUS);
//        }

		// for each weather file multiply the samples for each factor to give a single value of annual energy adjusted by the factors (100 samples for each weather file = product of all uncertainty sources)


		// rank order the resulting (number weather files * 100) samples and compute desired P value


		std::vector<wxColour> &colours = Graph::Colours();
		/*
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
		*/
	}
	else
	{
		tpd.Log(wxString::Format("Not all simulations completed successfully. (%d of %d OK)", (int)nyearsok, (int)years.size()));
	}

	for( size_t i=0;i<sims.size();i++ )
		delete sims[i];


	tpd.Finalize();
	
	m_layout->InvalidateBestSize();
	m_layout->AutoLayout();
	Layout();
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
//	GetTextData(dat, '\t');

	// strip commas per request from Paul 5/23/12 meeting
	dat.Replace(",", "");

	if (wxTheClipboard->Open())
	{
		wxTheClipboard->Clear();
		wxTheClipboard->SetData(new wxTextDataObject(dat));
		wxTheClipboard->Close();
	}
}

/*
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
*/
enum {
  ID_btnEditUncertaintySourceDist = wxID_HIGHEST+414,
  ID_ttMouseDown
};

BEGIN_EVENT_TABLE( UncertaintySource, wxPanel )
    EVT_BUTTON( ID_btnEditUncertaintySourceDist, UncertaintySource::OnEdit)
    EVT_TOOLTIPCTRL(ID_ttMouseDown, UncertaintySource::OnToolTip)
END_EVENT_TABLE()

UncertaintySource::UncertaintySource(wxWindow *parent, std::string& source_label, std::string& source_info, std::string& initial_value): wxPanel( parent ), m_label(source_label), m_info(source_info), m_infoDistDialog(initial_value)
{
//	m_infoDistDialog = "1:10:1:0:0"; // factor with a normal distribution with mean of 10% and std dev 1%
	
	wxBoxSizer *sizer_inputs = new wxBoxSizer( wxHORIZONTAL );
    
    m_source =  new wxStaticText(this, wxID_ANY, wxString(source_label) );
    m_source->SetSizeHints(80, 24);
    sizer_inputs->Add(m_source,  wxALL|wxALIGN_BOTTOM);
    
    m_tt = new AFToolTipCtrl(this);
    m_tt->SetSizeHints(24, 24);  // to appear using sizers
    m_tt->SetId(ID_ttMouseDown); // to connect event
    sizer_inputs->Add(m_tt, 0,wxALL|wxALIGN_TOP,5);
    
    m_distInfo = new wxTextCtrl(this, wxID_ANY, "Distribution Information", wxDefaultPosition, wxDefaultSize, wxTE_READONLY | wxTE_DONTWRAP | wxBORDER_NONE);
    m_distInfo->SetSizeHints(90, 24);
    sizer_inputs->Add(m_distInfo, wxEXPAND | wxALL|wxALIGN_BOTTOM);
    
    sizer_inputs->Add( new wxButton(this, ID_btnEditUncertaintySourceDist, "Edit...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxALIGN_TOP);


	InputDistDialog dlg(this, "Edit " + m_source->GetLabel() + " Distribution");
	wxArrayString parts;
	parts = wxSplit(m_infoDistDialog, ':');
	dlg.Setup(parts[0], parts[2], wxAtoi(parts[1]), wxAtof(parts[2]), wxAtof(parts[3]), wxAtof(parts[4]), wxAtof(parts[5]));
	PopulateDistInfoText(wxAtoi(parts[1]),dlg);

    SetSizer(sizer_inputs);
}

void UncertaintySource::OnEdit(wxCommandEvent &evt)
{
    InputDistDialog dlg(this, "Edit " + m_source->GetLabel() + " Distribution");
	wxArrayString parts;
	parts = wxSplit(m_infoDistDialog, ':');
	dlg.Setup(parts[0], parts[2], wxAtoi(parts[1]), wxAtof(parts[2]), wxAtof(parts[3]), wxAtof(parts[4]), wxAtof(parts[5]));
    if (dlg.ShowModal()==wxID_OK)
    {
        m_infoDistDialog = m_source->GetLabel() + ":"
        + wxString::Format("%d", dlg.cboDistribution->GetSelection()) + ":"
        + wxString::Format("%lg", dlg.nums[0]->Value()) + ":"
        + wxString::Format("%lg", dlg.nums[1]->Value()) + ":"
        + wxString::Format("%lg", dlg.nums[2]->Value()) + ":"
        + wxString::Format("%lg", dlg.nums[3]->Value());
       
        auto i = dlg.cboDistribution->GetSelection();
		PopulateDistInfoText(i, dlg);
    }
}

void UncertaintySource::PopulateDistInfoText(int i, InputDistDialog& dlg)
{
	wxArrayString distinfo(wxStringTokenize(lhs_dist_names[i], ","));

	if (distinfo.size() > 0) {
		wxString dist_info = distinfo[0];
		for (size_t j = 1; j<distinfo.size(); j++)
			dist_info += ", " + distinfo[j] + "=" + wxString::Format("%lg", dlg.nums[j - 1]->Value());
		m_distInfo->SetValue(dist_info);
	}

}

void UncertaintySource::OnToolTip(wxCommandEvent &evt)
{
    wxRichToolTip tip(m_label, m_info);
    tip.ShowFor(m_tt);
}


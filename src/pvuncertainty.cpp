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
#include "nsrdb.h"

// for DBL_MAX definition
#ifndef WIN32
#include <float.h>
#endif


enum { ID_SELECT_FOLDER = wxID_HIGHEST+494,
	ID_SIMULATE, ID_COPYTABLE, ID_SETPVALUE, ID_NSRDBDOWNLOAD };

BEGIN_EVENT_TABLE( PVUncertaintyForm, wxPanel )
	EVT_BUTTON( ID_SELECT_FOLDER, PVUncertaintyForm::OnSelectFolder )
	EVT_BUTTON(ID_SIMULATE, PVUncertaintyForm::OnSimulate)
	EVT_BUTTON(ID_SETPVALUE, PVUncertaintyForm::OnSetPValue)
	EVT_BUTTON(ID_COPYTABLE, PVUncertaintyForm::OnCopyTable)
	EVT_HYPERLINK(ID_NSRDBDOWNLOAD, PVUncertaintyForm::OnNSRDBDownload)
END_EVENT_TABLE()



PVUncertaintyForm::PVUncertaintyForm( wxWindow *parent, Case *cc )
	: wxPanel( parent ), m_case(cc), m_data(m_case->PVUncertainty())
{
	SetBackgroundColour( *wxWHITE );

	wxBoxSizer *sizer_top = new wxBoxSizer( wxHORIZONTAL );
	sizer_top->Add( new wxMetroButton( this, ID_SIMULATE, "Run PV uncertainty simulations", wxNullBitmap,wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW ), 0, wxALL, 0 );
	sizer_top->AddSpacer( 150 );

//	sizer_top->Add(new wxMetroButton(this, ID_COPYTABLE, "Copy table to clipboard", wxNullBitmap, wxDefaultPosition, wxDefaultSize), 0, wxALL | wxALIGN_CENTER_VERTICAL, 0);

    wxStaticBoxSizer *sizer_inputs = new wxStaticBoxSizer( wxVERTICAL, this, "Sources of Uncertainty" );
	/*Distributions
	char const *lhs_dist_names[LHS_NUMDISTS] = {
	0	"Uniform,Min,Max",
	1	"Normal,Mean (mu),Std. Dev. (sigma)",
	2	"Lognormal,Mean,ErrorF",
	3	"Lognormal-N,Mean,Std. Dev.",
	4	"Triangular,A,B,C",
	5	"Gamma,Alpha,Beta",
	6	"Poisson,Lambda",
	7	"Binomial,P,N",
	8	"Exponential,Lambda",
	9	"Weibull,Alpha or k (shape parameter),Beta or lambda (scale parameter)",
	10	"UserCDF,N"
};
*/
    // add sources of uncertainty and information to show with tool tip
    std::vector< std::tuple<std::string, std::string, std::string > > sourceinfo;
	sourceinfo.push_back(std::make_tuple("Translation from GHI to GPOA", "Details for Translation from GHI to GPOA", "Translation from GHI to GPOA:1:11.5:2.5:0:0"));
	sourceinfo.push_back(std::make_tuple("Horizontal shading", "Details for horizontal shading", "Horizontal shading:4:-1:0:0:0"));
	sourceinfo.push_back(std::make_tuple("Row shading", "Details for row shading", "Row shading:4:-5:-1:0:0"));
	sourceinfo.push_back(std::make_tuple("STC power (single module rating)", "Details for STC power (single module rating)", "STC power (single module rating):1:0:2:0:0"));
	sourceinfo.push_back(std::make_tuple("Inverter availability", "Details for Inverter availability", "Inverter availability:4:-5.7:-2.7:0:0"));
	sourceinfo.push_back(std::make_tuple("Spectral response", "Details for Spectral response", "Spectral response:1:-1:0.5:0:0"));
	sourceinfo.push_back(std::make_tuple("Cell temperature", "Details for Cell temperature", "Cell temperature:1:-2.4:1:0:0"));
	sourceinfo.push_back(std::make_tuple("Mismatch loss", "Details for Mismatch loss", "Mismatch loss:4:-1.8:-0.8:0:0"));
	sourceinfo.push_back(std::make_tuple("DC cabling", "Details for DC cabling", "DC cabling:4:-2.5:-1.5:-1:0"));
	sourceinfo.push_back(std::make_tuple("Transformer", "Details for Transformer", "Transformer:4:-2:-1:-0.5:0"));
	sourceinfo.push_back(std::make_tuple("Soiling", "Details for Soiling", "Soiling:4:-1.5:-0.5:0:0"));

	m_sd_defaults = StochasticData(); // defaults to 100 samples and 0 seed

	for (size_t i = 0; i < sourceinfo.size(); i++) {
		m_uncertaintySources.push_back(new UncertaintySource(this, std::get<0>(sourceinfo[i]), std::get<1>(sourceinfo[i]), std::get<2>(sourceinfo[i])));
		sizer_inputs->Add(m_uncertaintySources[i], 1, wxALL, 5);
		m_sd_defaults.InputDistributions.push_back(std::get<2>(sourceinfo[i]));
	}

		
    wxStaticBoxSizer *sizer_interannual = new wxStaticBoxSizer( wxVERTICAL, this, "Interannual Variability" );

    wxBoxSizer *sizer_weather_file = new wxBoxSizer(wxHORIZONTAL);
    wxStaticText *label = new wxStaticText( this, wxID_ANY, "Select weather file folder:" );
    sizer_weather_file->Add( label , 0, wxLEFT|wxRIGHT|wxALIGN_CENTER_VERTICAL, 0 );
	sizer_weather_file->Add(m_folder = new wxTextCtrl(this, wxID_ANY, "", wxDefaultPosition, wxSize(1050,24)), 0, wxEXPAND | wxALL, 3);
	sizer_weather_file->Add( new wxButton( this, ID_SELECT_FOLDER, "..." ), 0, wxLEFT|wxALIGN_CENTER_VERTICAL, 0 );
	sizer_weather_file->SetSizeHints(m_folder);
    sizer_interannual->Add(sizer_weather_file, 0, wxEXPAND | wxALL, 2);
    sizer_interannual->Add( new wxHyperlinkCtrl( this, ID_NSRDBDOWNLOAD, "Download files from NSRDB for my location", SamApp::WebApi("historical_nsrdb") ), 0, wxALL, 0 );
     
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
	m_validRuns = false;
	UpdateFromSimInfo();
}

PVUncertaintyForm::~PVUncertaintyForm()
{
	/*
    for (size_t i = 0; m_tsDataSets.size(); i++)
        if (m_tsDataSets[i]) delete m_tsDataSets[i];
	*/
}

void PVUncertaintyForm::UpdateFromSimInfo()
{
	// defaults in constructor
	m_folder->SetValue(m_data.WeatherFileFolder);
	m_puser->SetValue(m_data.pValue);


	if (m_data.UncertaintySources.InputDistributions.Count() < 1)
		m_data.UncertaintySources = m_sd_defaults;

	// update uncertainty sources
	for (size_t i = 0; i < m_data.UncertaintySources.InputDistributions.Count() && i < m_uncertaintySources.size(); i++)
		m_uncertaintySources[i]->SetInfoDistDialog(m_data.UncertaintySources.InputDistributions[i]);
}

void PVUncertaintyForm::OnSetPValue(wxCommandEvent&)
{
	double pValue;
	if (!m_puser->GetValue().ToDouble(&pValue)) pValue = 90;
	SetPValue(pValue);
}

void PVUncertaintyForm::SetPValue(double pValue)
{
	m_data.pValue = pValue;
	if (m_validRuns) {
		m_pnCdfAll->SetPValue(pValue);
		m_pnCdfIV->SetPValue(pValue);
		m_pnCdfUS->SetPValue(pValue);

		double pValueX = m_pnCdfIV->GetPValueX();
		m_barIV->SetPBarValue(pValue, pValueX);
	}
}

void PVUncertaintyForm::OnSimulate( wxCommandEvent & )
{
	
	std::vector<unsigned short> years;
	wxArrayString folder_files; 
	wxArrayString list;
	wxDir::GetAllFiles( m_folder->GetValue(), &list );


	// start of weather file simulations

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
			wxMessageBox( wxString::Format("Internal error preparing simulation %d for PV Uncertainty.", (int)(n+1)) );

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
	
	// end of weather file simulations


	matrix_t<double> output_stats;

	// generate samples
	// update to new distributions
	for (size_t i = 0; i < m_uncertaintySources.size(); i++) {
		m_data.UncertaintySources.InputDistributions[i] = m_uncertaintySources[i]->GetInfoDistDialog();
	}
    
    // change sample size from default of 100 to 1000 must change in header m_pUS
	m_data.UncertaintySources.N = 1000;
	wxArrayString errors;

	if (!ComputeLHSInputVectors(m_data.UncertaintySources, output_stats, &errors))
	{
		wxShowTextMessageDialog("An error occured while computing the samples using LHS:\n\n" + wxJoin(errors, '\n'));
		return;
	}


	if (output_stats.nrows() != 1000) {
		// throw ?
		return;
	}

	/*
	// show samples for debugging
	wxDialog* dlg = new wxDialog(this, wxID_ANY, "Stochastic Input Vectors", wxDefaultPosition, wxScaleSize(400, 600), wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);
	wxExtGridCtrl* grid = new wxExtGridCtrl(dlg, wxID_ANY);
	grid->EnableCopyPaste(true);
	grid->CreateGrid(output_stats.nrows(), output_stats.ncols());
	grid->Freeze();
	// for string value variables - show string values (e.g. lists - array type, weather files,...)
	for (size_t j = 0; j < output_stats.ncols(); j++)
	{
		wxString var = m_data.UncertaintySources.InputDistributions[j];
		wxArrayString parts = wxStringTokenize(var, ":");
		if (parts.Count() < 2) continue;
		int dist_type = wxAtoi(parts[1]);
		if ((parts.Count() < 6) && (dist_type != LHS_USERCDF)) continue;
		if (dist_type == LHS_USERCDF)
		{
			wxString item = GetVarNameFromInputDistribution(parts[0]);
			wxArrayString values;
				VarInfo* vi = m_case->GetConfiguration()->Variables.Lookup(item);
				if (!vi) continue;
				values = vi->IndexLabels;
			if (values.Count() > 0)
			{
				for (size_t i = 0; i < output_stats.nrows(); i++)
				{
					int ndx = (int)output_stats(i, j);
					if ((ndx >= 0) && (ndx < (int)values.Count()))
						grid->SetCellValue(i, j, values[ndx]);
				}
			}
		}
		else
		{
			for (size_t i = 0; i < output_stats.nrows(); i++)
				grid->SetCellValue(i, j, wxString::Format("%lg", output_stats(i, j)));
		}
	}


	for (size_t i = 0; i < output_stats.ncols(); i++) {
		wxString var = m_data.UncertaintySources.InputDistributions[i];
		wxArrayString parts = wxStringTokenize(var, ":");
		grid->SetColLabelValue(i, parts[0]);
	}
	grid->AutoSize();
	grid->Thaw();

	dlg->Show();
	*/


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

    if (nyearsok == years.size())
    {
    
 
        // delete all the pdf/cdf plots
        size_t i=0;
        while( i<m_layout->Count() )
        {
            if( wxDVPnCdfCtrl *plt = dynamic_cast<wxDVPnCdfCtrl*>( m_layout->Get(i) ) )
                m_layout->Delete( plt );
            else
                i++;
        }
		// delete all the plots
		i = 0;
		while (i < m_layout->Count())
		{
			if (wxDVBarPValueCtrl* plt = dynamic_cast<wxDVBarPValueCtrl*>(m_layout->Get(i)))
				m_layout->Delete(plt);
			else
				i++;
		}
//		if (m_pnCdfIV) m_pnCdfIV->Destroy();

		double emax = 1;
		for (size_t n = 0; n < years.size(); n++)
		{
			if (VarValue* vv = sims[n]->GetOutput("annual_energy"))
			{
				m_ivenergy.push_back(wxRealPoint(years[n], vv->Value()));
				if (vv->Value() > emax) emax = vv->Value();
			}
		}


		m_pnCdfAll = new wxDVPnCdfCtrl(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL, "panel", false, false, false, false);
		m_pnCdfAll->AddDataSet(new TimeSeriesData(m_pAll, sizeAll, 1, 0, "Overall uncertainty", "Energy (kWh)"), true);
		m_pnCdfAll->SelectDataSetAtIndex(0);
		m_layout->Add(m_pnCdfAll);
		m_pnCdfIV = new wxDVPnCdfCtrl(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL, "panel", false, false, false, false);
		m_pnCdfIV->AddDataSet(new TimeSeriesData(m_pIV, sizeIV, 1, 0, "Interannual variablility", "Energy (kWh)"), true);
		m_pnCdfIV->SelectDataSetAtIndex(0);
		m_pnCdfIV->Hide();
//		m_layout->Add(m_pnCdfIV);
		m_barIV = new wxDVBarPValueCtrl(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL, "panel");
		m_barIV->SetBarValues(m_ivenergy);
		m_layout->Add(m_barIV);
		m_pnCdfUS = new wxDVPnCdfCtrl(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL, "panel", false, false, false, false);
		m_pnCdfUS->AddDataSet(new TimeSeriesData(m_pUS, sizeUS, 1, 0, "Uncertainty Sources", "fraction"), true);
		m_pnCdfUS->SelectDataSetAtIndex(0);
        m_layout->Add(m_pnCdfUS);

	}
	else
	{
		tpd.Log(wxString::Format("Not all simulations completed successfully. (%d of %d OK)", (int)nyearsok, (int)years.size()));
	}

	for( size_t i=0;i<sims.size();i++ )
		delete sims[i];

	double pv;
	if (m_puser->GetValue().ToDouble(&pv))
		SetPValue(pv);
	else {
		m_puser->SetValue(50);
		SetPValue(50);
	}

	tpd.Finalize();
	
	m_layout->InvalidateBestSize();
	m_layout->AutoLayout();
	Layout();

	m_validRuns = true;
}

void PVUncertaintyForm::OnSelectFolder(wxCommandEvent&)
{
	wxString dir = wxDirSelector("Choose weather file folder", m_folder->GetValue());
	if (!dir.IsEmpty())
		m_folder->ChangeValue(dir);
}

void PVUncertaintyForm::OnNSRDBDownload(wxHyperlinkEvent& evt)
{
		// fcall_nsrdbquery
	NSRDBDialog dlgNSRDB(SamApp::Window(), "NSRDB Download");
	dlgNSRDB.CenterOnParent();
	int code = dlgNSRDB.ShowModal(); //shows the dialog and makes it so you can't interact with other parts until window is closed

	if (code == wxID_OK)
		m_folder->ChangeValue(dlgNSRDB.GetWeatherFolder());
	evt.Skip(false); // skip opening browser
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

PVUncertaintyData::PVUncertaintyData()
{
	pValue = 90;
}

void PVUncertaintyData::Copy(PVUncertaintyData& pvd)
{
	UncertaintySources = pvd.UncertaintySources;
	WeatherFileFolder = pvd.WeatherFileFolder;
	pValue = pvd.pValue;
}

void PVUncertaintyData::Write(wxOutputStream& _o)
{
	wxDataOutputStream out(_o);
	out.Write8(0x9f);
	out.Write8(1);

	UncertaintySources.Write(_o);

	out.WriteString(WeatherFileFolder);
	out.WriteDouble(pValue);

	out.Write8(0x9f);
}

bool PVUncertaintyData::Read(wxInputStream& _i)
{
	wxDataInputStream in(_i);
	wxUint8 code = in.Read8();
	in.Read8(); // ver

	UncertaintySources.Read(_i);

	WeatherFileFolder = in.ReadString();
	pValue = in.ReadDouble();

	return in.Read8() == code;
}



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
    m_source->SetSizeHints(250, 24);
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

void UncertaintySource::SetInfoDistDialog(wxString& _infoDistDialog)
{
	m_infoDistDialog = _infoDistDialog;
	InputDistDialog dlg(this, "Edit " + m_source->GetLabel() + " Distribution");
	wxArrayString parts;
	parts = wxSplit(m_infoDistDialog, ':');
	dlg.Setup(parts[0], parts[2], wxAtoi(parts[1]), wxAtof(parts[2]), wxAtof(parts[3]), wxAtof(parts[4]), wxAtof(parts[5]));
	PopulateDistInfoText(wxAtoi(parts[1]), dlg);
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

wxDVBarPValueCtrl::wxDVBarPValueCtrl(wxWindow* parent, wxWindowID id, const wxPoint& pos, const wxSize& size, long style , const wxString& name)
: wxPanel(parent, id, pos, size, style, name) {
	m_plotSurface = new wxPLPlotCtrl(this, wxID_ANY);
	m_plotSurface->SetBackgroundColour(*wxWHITE);

	m_pValueResultLabel = new wxStaticText(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT);
	m_pValueResultTextBox = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT | wxTE_READONLY);
	m_pValueResultTextBox->SetForegroundColour(UIColorCalculatedFore);
	m_pValueResultTextBox->SetBackgroundColour(UIColorCalculatedBack);
	m_pValueResultUnits = new wxStaticText(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT);

	wxBoxSizer* sizer = new wxBoxSizer(wxVERTICAL);

	wxBoxSizer* options2Sizer = new wxBoxSizer(wxHORIZONTAL);

	options2Sizer->AddStretchSpacer();

	options2Sizer->Add(m_pValueResultLabel, 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
	options2Sizer->Add(m_pValueResultTextBox, 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
	options2Sizer->Add(m_pValueResultUnits, 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);

	options2Sizer->AddStretchSpacer();

	wxBoxSizer* leftSizer = new wxBoxSizer(wxVERTICAL);
//	leftSizer->Add(options1Sizer, 0, wxEXPAND, 0);
	leftSizer->Add(m_plotSurface, 1, wxEXPAND | wxALL, 10);
	leftSizer->Add(options2Sizer, 0, wxEXPAND, 0);

	wxBoxSizer* mainSizer = new wxBoxSizer(wxHORIZONTAL);
	mainSizer->Add(leftSizer, 1, wxALL | wxEXPAND, 0);
	mainSizer->Add(sizer, 0, wxALL | wxEXPAND, 0);
	SetSizer(mainSizer);

	m_plotSurface->ShowLegend(false);
}

void wxDVBarPValueCtrl::RebuildPlotSurface() {
	m_plotSurface->DeleteAllPlots();
	wxColour clr = wxColour("dark olive green");
//	clr = wxColour(clr.Red(), clr.Green(), clr.Blue(), 128);
	clr = wxColour(clr.Red(), clr.Green(), clr.Blue(), 64);
	m_plotSurface->AddPlot(new wxPLBarPlot(m_values, 0, wxEmptyString, clr));

	if (m_pvaluebar.size()>0)
		m_plotSurface->AddPlot(new wxPLBarPlot(m_pvaluebar, 0, wxEmptyString, *wxBLUE));

	// specific to this data
	wxPLLabelAxis* x1 = new wxPLLabelAxis(m_years[0] - 1, m_years[m_years.size() - 1] + 1, "Year");
	for (size_t i = 0; i < m_years.size(); i++)
		x1->Add(m_years[i], wxString::Format("%d", (int)m_years[i]));
	m_plotSurface->SetXAxis1(x1);

	m_plotSurface->Y1().SetLabel("Energy (kWh)");
	m_plotSurface->Y1().SetWorldMax(1.1 * m_ymax);
	m_plotSurface->ShowLegend(false);
	m_plotSurface->SetTitle("Interannual Variablity");

	// bar size shrinks
	wxPLBarPlot* bp = (wxPLBarPlot * )m_plotSurface->GetPlot(0);
	bp->SetThickness(20.0);

	InvalidatePlot();
}

void wxDVBarPValueCtrl::SetBarValues(const std::vector<wxRealPoint>& values) {
	m_values = values;
	m_ymax = -DBL_MAX;
	m_years.clear();
	for (auto &val : m_values) {
		if (val.y > m_ymax) m_ymax = val.y;
		m_years.push_back((unsigned short)val.x);
	}
	RebuildPlotSurface();
}
void wxDVBarPValueCtrl::SetPBarValue(const double& pValue, const double& pValueX) {
	m_pValueResultLabel->SetLabel(wxString::Format("P%lg", pValue));
	m_pValueResultTextBox->SetValue(wxString::Format("%lg", pValueX));
	m_pValueResultUnits->SetLabel("Energy (kWh)");

	m_pvaluebar.clear();
	// assume that SetBarValues already called and set
	for (auto& val : m_values) {
		if (fabs(val.y - pValueX) < 1.e-3)
			m_pvaluebar.push_back(val);
	}
	RebuildPlotSurface();
}

void wxDVBarPValueCtrl::InvalidatePlot() {
	m_plotSurface->Invalidate();
	m_plotSurface->Refresh();
	Layout(); 
}

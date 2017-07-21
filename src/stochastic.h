/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef __stochastic_h
#define __stochastic_h

#include <vector>

#include <wx/panel.h>
#include <wx/dialog.h>
#include <wx/sstream.h>

#include <wx/string.h>
#include <wx/arrstr.h>

enum {
	LHS_UNIFORM,
	LHS_NORMAL,
	LHS_LOGNORMAL,
	LHS_LOGNORMAL_N,
	LHS_TRIANGULAR,
	LHS_GAMMA,
	LHS_POISSON,
	LHS_BINOMIAL,
	LHS_EXPONENTIAL,
	LHS_WEIBULL,
	LHS_USERCDF,
LHS_NUMDISTS
};

extern char *lhs_dist_names[LHS_NUMDISTS];

class LHS
{
public:
	LHS();

	void Reset();
	bool Exec();
	wxString ErrorMessage();

	void SeedVal(int sv = -1);
	void Points(int n);
	void Correlate(const wxString &name1, const wxString &name2, double corr);
	void Distribution(int type, const wxString &name, const std::vector<double> &params);
	
	bool Retrieve(const wxString &name, std::vector<double> &values);
	wxArrayString ListAll();
	void Remove(const wxString &name);
	void RemoveCorrelation(const wxString &name1, const wxString &name2);

private:
	struct DistInfo
	{
		int type;
		wxString name;
		std::vector<double> params;
		std::vector<double> values;
	};

	struct CorrInfo
	{
		wxString name1;
		wxString name2;
		double corr;
	};

	int Find(const wxString &name);

	std::vector<DistInfo> m_dist;
	std::vector<CorrInfo> m_corr;
	wxString m_errmsg;
	int m_npoints;
	int m_seedval;
};

class Stepwise
{
public:
	Stepwise();

	void Reset();
	bool Exec();
	wxString ErrorMessage();

	// set simulation inputs and results
	void SetInputVector(const wxString &name, const std::vector<double> &data);
	void SetOutputVector(const std::vector<double> &data);

	bool GetStatistics(const wxString &name, 
		double *R2, double *R2inc, double *SRC);

private:
	struct datavec
	{
		wxString name;
		std::vector<double> vec;
		bool calculated;
		double R2;
		double R2inc;
		double SRC;
	};

	std::vector<datavec> m_inputs;
	std::vector<double> m_output_vec;
	wxString m_err;
	
};

class StochasticData
{
public:
	StochasticData();

	void Copy( StochasticData &stat );
	void Write( wxOutputStream & );
	bool Read( wxInputStream & );

	int Seed;
	int N;

	wxArrayString Outputs;
	wxArrayString InputDistributions;
	wxArrayString Correlations;
};

class StochasticPanel : public wxPanel
{
public:
	StochasticPanel(wxWindow *parent, Case *cc);

	void UpdateFromSimInfo();	

	void OnSeedChange(wxCommandEvent &evt);
	void OnNChange(wxCommandEvent &evt);

	void OnAddInput(wxCommandEvent &evt);
	void OnEditInput(wxCommandEvent &evt);
	void OnRemoveInput(wxCommandEvent &evt);

	void OnAddOutput( wxCommandEvent & );
	void OnRemoveOutput( wxCommandEvent & );
	
	void OnAddCorr(wxCommandEvent &evt);
	void OnEditCorr(wxCommandEvent &evt);
	void OnRemoveCorr(wxCommandEvent &evt);

	void OnComputeSamples(wxCommandEvent &evt);

	void OnSimulate( wxCommandEvent & );

	void Simulate();

	void OnSelectFolder(wxCommandEvent &);
	void OnCheckWeather(wxCommandEvent &);
	void OnComboWeather(wxCommandEvent &);
	void OnShowWeatherCDF(wxCommandEvent &);

	wxString GetLabelFromVarName(const wxString &var_name);
	void UpdateWeatherFileList();
	void UpdateWeatherFileControls();
	void UpdateWeatherFileCDF();
	int GetWeatherFileDistributionIndex();
	void UpdateWeatherFileInputDistribution();
	void UpdateWeatherFileSums();
	void UpdateWeatherFileSort();
	bool GetWeatherFileForSum(const double sum, wxString *wf);
	int GetInputDistributionIndex(int idx);

private:
	Case *m_case;
	StochasticData &m_sd;

	// weather file folder control - persisted with stochastic_weather_folder=folder name as string.
	wxTextCtrl *m_folder;
	wxCheckBox *m_chk_weather_files;
	wxComboBox *m_cbo_weather_files;
	wxArrayString m_weather_files;
	wxString m_weather_folder_varname;
	wxString m_weather_folder_displayname;
	std::vector<size_t> m_weather_file_sorted_index;
	std::vector<double> m_weather_file_sums;

	wxListBox *m_corrList;
	wxListBox *m_inputList;
	wxListBox *m_outputList;
	wxNumericCtrl *m_N;
	wxNumericCtrl *m_seed;
	wxCheckBox *m_useThreads;
	
	wxExtGridCtrl *m_dataGrid;
	wxExtGridCtrl *m_statGrid;

	struct stepresult {
		stepresult() { deltar2 = beta = 0.0; }
		double deltar2;
		double beta;
	};
	
	matrix_t<stepresult> m_regressions;
	DECLARE_EVENT_TABLE()
};

wxString GetVarNameFromInputDistribution(const wxString &input_distribution);

bool ComputeLHSInputVectors( StochasticData &sd, matrix_t<double> &table, wxArrayString *errors);

#endif
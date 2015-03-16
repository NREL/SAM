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
	LHS_TRIANGULAR,
	LHS_GAMMA,
	LHS_POISSON,
	LHS_BINOMIAL,
	LHS_EXPONENTIAL,
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
	wxString GetLabelFromVarName(const wxString &var_name);
	void UpdateWeatherFileList();
	void UpdateWeatherFileControls();
	void UpdateWeatherFileCDF();
	int GetWeatherFileDistributionIndex();
	void UpdateWeatherFileInputDistribution();
	void UpdateWeatherFileSums();
	void UpdateWeatherFileSort();

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
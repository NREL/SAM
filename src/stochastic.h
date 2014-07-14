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

	wxArrayString OutputMetrics;
	wxArrayString InputDistributions;
	wxArrayString Correlations;
};

class StochasticPanel : public wxPanel
{
public:
	StochasticPanel(wxWindow *parent, Case *cc);

	void UpdateFromSimInfo();
	StochasticData *GetSimInfo();
	
	void OnSeedChange(wxCommandEvent &evt);
	void OnNChange(wxCommandEvent &evt);

	void OnAddMetric(wxCommandEvent &evt);
	void OnRemoveMetric(wxCommandEvent &evt);

	void OnAddInput(wxCommandEvent &evt);
	void OnEditInput(wxCommandEvent &evt);
	void OnRemoveInput(wxCommandEvent &evt);
	void OnSelectInput(wxCommandEvent &evt);
	
	void OnAddCorr(wxCommandEvent &evt);
	void OnEditCorr(wxCommandEvent &evt);
	void OnRemoveCorr(wxCommandEvent &evt);
	void OnSelectCorr(wxCommandEvent &evt);

	void OnComputeSamples(wxCommandEvent &evt);

private:
	Case *mCase;
	StochasticData *mSSimInf;

	wxListBox *lstOutputMetrics;
	wxButton *btnAddMetric;
	wxButton *btnRemoveMetric;
	wxStaticBox *GroupBox1;
	wxButton *btnRemoveInput;
	wxButton *btnAddInput;
	wxNumericCtrl *numSeed;
	wxButton *btnAddCorr;
	wxButton *btnEditCorr;
	wxButton *btnRemoveCorr;
	wxListBox *lstCorrelations;
	wxListBox *lstInputs;
	wxNumericCtrl *numN;
	wxButton *btnComputeSamples;
	wxCheckBox *chkEnable;
	wxButton *btnRemove;
	wxButton *btnEditInput;

	DECLARE_EVENT_TABLE()
};


bool ComputeLHSInputVectors( StochasticData *ssim, matrix_t<double> &table, wxArrayString *errors);

#endif
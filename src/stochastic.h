#ifndef __stochastic_h
#define __stochastic_h

#include <vector>

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

	void Configure(const wxString &exe, const wxString &workdir);
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
	wxString m_lhsexe;
	wxString m_workdir;
	int m_npoints;
	int m_seedval;
};


#endif
#include <wx/filefn.h>
#include <wx/stopwatch.h>
#include <wx/tokenzr.h>

#include "stochastic.h"

char *lhs_dist_names[LHS_NUMDISTS] = {
	"Uniform,Min,Max",
	"Normal,Mean (mu),Std. Dev. (sigma)",
	"Lognormal,Mean,ErrorF",
	"Triangular,A,B,C",
	"Gamma,Alpha,Beta",
	"Poisson,Lambda",
	"Binomial,P,N",
	"Exponential,Lambda",
	"UserCDF,N,[val,cdf]*"
};

LHS::LHS()
{
	m_npoints = 500;
	m_seedval = -1;
}

void LHS::Configure(const wxString &exe, const wxString &workdir)
{
	m_lhsexe = exe;
	m_workdir = workdir;
}

void LHS::Reset()
{
	m_dist.clear();
	m_corr.clear();
	m_npoints = 500;
	m_errmsg.Empty();
}

void LHS::SeedVal(int sv)
{
	m_seedval =sv;
}

bool LHS::Exec()
{
	if (!wxDirExists(m_workdir))
	{
		m_errmsg = "Working directory does not exist.";
		return false;
	}

	if (!wxFileExists(m_lhsexe))
	{
		m_errmsg = "Sandia LHS executable does not exist.";
		return false;
	}


	// write lhsinputs.lhi file
	wxString inputfile = m_workdir + "/SAMLHS.LHI";
	FILE *fp = fopen(inputfile.c_str(), "w");
	if (!fp)
	{
		m_errmsg = "Could not write to LHS input file " + inputfile;
		return false;
	}

	int sv = wxGetLocalTime();
	if (m_seedval > 0)
		sv = m_seedval;

	fprintf(fp, "LHSTITL SAM LHS RUN\n");
	fprintf(fp, "LHSOBS %d\n", m_npoints);
	fprintf(fp, "LHSSEED %d\n", sv);
	fprintf(fp, "LHSRPTS CORR DATA\n");
	fprintf(fp, "LHSSCOL\n");
	fprintf(fp, "LHSOUT samlhs.lsp\n");
	fprintf(fp, "LHSPOST samlhs.msp\n");
	fprintf(fp, "LHSMSG samlhs.lmo\n");
	fprintf(fp, "DATASET:\n");
	for (size_t i=0;i<m_dist.size();i++)
	{
		int ncdfpairs;
		int nminparams = wxStringTokenize(lhs_dist_names[ m_dist[i].type ], ",").Count()-1;
		if ( (int)m_dist[i].params.size() < nminparams)
		{
			m_errmsg.Printf("Dist '%s' requires minimum %d params, only %d specified.", 
				(const char*)m_dist[i].name.c_str(), nminparams, (int)m_dist[i].params.size());
			fclose(fp);
			return false;
		}

		switch(m_dist[i].type)
		{
		case LHS_UNIFORM:
			fprintf(fp, "%s UNIFORM %lg %lg\n", (const char*)m_dist[i].name.c_str(), 
				m_dist[i].params[0], 
				m_dist[i].params[1]);
			break;
		case LHS_NORMAL:
			fprintf(fp, "%s NORMAL %lg %lg\n", (const char*)m_dist[i].name.c_str(), 
				m_dist[i].params[0], 
				m_dist[i].params[1]);
			break;
		case LHS_LOGNORMAL:
			fprintf(fp, "%s LOGNORMAL %lg %lg\n", (const char*)m_dist[i].name.c_str(), 
				m_dist[i].params[0], 
				m_dist[i].params[1]);
			break;
		case LHS_TRIANGULAR:
			fprintf(fp, "%s %lg TRIANGULAR %lg %lg %lg\n", (const char*)m_dist[i].name.c_str(), m_dist[i].params[1], 
				m_dist[i].params[0], 
				m_dist[i].params[1], 
				m_dist[i].params[2]);
			break;
		case LHS_GAMMA:
			fprintf(fp, "%s GAMMA %lg %lg\n", (const char*)m_dist[i].name.c_str(), 
				m_dist[i].params[0], 
				m_dist[i].params[1]);
			break;
		case LHS_POISSON:
			fprintf(fp, "%s POISSON %lg\n", (const char*)m_dist[i].name.c_str(), 
				m_dist[i].params[0]);
			break;
		case LHS_BINOMIAL:
			fprintf(fp, "%s BINOMIAL %lg %lg\n", (const char*)m_dist[i].name.c_str(), 
				m_dist[i].params[0], 
				m_dist[i].params[1]);
			break;
		case LHS_EXPONENTIAL:
			fprintf(fp, "%s EXPONENTIAL %lg\n", (const char*)m_dist[i].name.c_str(), 
				m_dist[i].params[0]);
			break;
		case LHS_USERCDF:
			ncdfpairs = (int) m_dist[i].params[0];
			fprintf(fp, "%s DISCRETE CUMULATIVE %d #\n", (const char*)m_dist[i].name.c_str(), ncdfpairs);
			for (int j=0;j<ncdfpairs;j++)
			{
				if (2+2*j >= (int)m_dist[i].params.size())
				{
					m_errmsg.Printf("user defined CDF error: too few [value,cdf] pairs in list: %d pairs should exist.", ncdfpairs);
					fclose(fp);
					return false;
				}

				fprintf(fp, "  %lg %lg", m_dist[i].params[ 1+2*j ], m_dist[i].params[ 2+2*j ] );
				if (j==ncdfpairs-1) fprintf(fp, "\n");
				else fprintf(fp, " #\n");
			}
			break;
		}
	}

	for (size_t i=0;i<m_corr.size();i++)
	{
		if (Find(m_corr[i].name1)>=0 && Find(m_corr[i].name2)>=0)
			fprintf(fp, "CORRELATE %s %s %lg\n", (const char*)m_corr[i].name1.c_str(), (const char*)m_corr[i].name2.c_str(), m_corr[i].corr);
	}

	fclose(fp);

	// now run using the callback provided or 'system' function

	// delete any output or error that may exist
	wxRemoveFile( m_workdir + "/SAMLHS.LSP" );
	wxRemoveFile( m_workdir + "/LHS.ERR" );

	// run the executable synchronously
	wxString curdir = wxGetCwd();
	wxSetWorkingDirectory( m_workdir );
	wxString execstr =  wxString('"' + m_lhsexe + "\" SAMLHS.LHI"); 
	bool exe_ok = ( 0 == system(execstr.c_str()) );
	wxSetWorkingDirectory(curdir);
	exe_ok = true;
	
	if (wxFileExists(m_workdir + "/LHS.ERR"))
	{
		m_errmsg = "LHS error.  There could be a problem with the input setup.";
		FILE *ferr = fopen( wxString(m_workdir + "/LHS.ERR").c_str(), "r");
		if (ferr)
		{
			char buf[256];
			m_errmsg += "\n\n";
			wxString line;
			while ( !feof(ferr) )
			{
				fgets( buf, 255, ferr );
				m_errmsg += wxString(buf) + "\n";
			}
			fclose(ferr);
		}
		return false;
	}

	if (!exe_ok)
	{
		m_errmsg = "Failed to run LHS executable";
		return false;
	}

	// read the lsp output file
	wxString outputfile = m_workdir + "/SAMLHS.LSP";
	fp = fopen( outputfile.c_str(), "r");
	if (!fp)
	{
		m_errmsg = "Could not read output file " + outputfile;
		return false;
	}

	for (size_t i=0;i<m_dist.size();i++)
	{
		m_dist[i].values.clear();
		m_dist[i].values.reserve( m_npoints );
	}

	int nline = 0;
	char cbuf[1024];
	int n_runs = 0;
	bool found_data = false;
	while ( !feof(fp) )
	{
		fgets(cbuf, 1023, fp);
		wxString buf( cbuf );
		nline++;

		if (buf.Trim() == "@SAMPLEDATA")
		{
			found_data = true;
			continue;
		}

		if (found_data)
		{
			n_runs++;
			int n = atoi(buf.c_str());
			if (n != n_runs)
			{
				m_errmsg = "output file formatting error (run count) at line " + wxString::Format("%d",nline);
				fclose(fp);
				return false;
			}
			
			fgets(cbuf, 1023, fp);
			wxString buf( cbuf );
			nline++;
			n = atoi(buf.c_str());
			if (n != (int) m_dist.size())
			{
				m_errmsg = "output file formatting error (ndist count) at line " + wxString::Format("%d",nline);
				fclose(fp);
				return false;
			}

			for (size_t i=0;i<m_dist.size();i++)
			{
				fgets(cbuf, 1023, fp);
				wxString buf( cbuf );
				nline++;
				m_dist[i].values.push_back( wxAtof( buf ) );
			}

		}
	}


	return true;
}

wxString LHS::ErrorMessage()
{
	return m_errmsg;
}


void LHS::Points(int n)
{
	if (n > 0 && n < 50000)
		m_npoints = n;
}

void LHS::Correlate(const wxString &name1, const wxString &name2, double corr)
{
	if (corr > -1 && corr < 1)
	{
		CorrInfo x;
		x.name1 = name1;
		x.name2 = name2;
		x.corr = corr;
		m_corr.push_back(x);
	}
}

void LHS::Distribution(int type, const wxString &name, const std::vector<double> &params)
{
	int idx = Find(name);
	if (idx >= 0)
	{
		m_dist[idx].type = type;
		m_dist[idx].name = name;
		m_dist[idx].params = params;
	}
	else
	{
		DistInfo x;
		x.type = type;
		x.name = name;
		x.params = params;
		m_dist.push_back( x );
	}
}

bool LHS::Retrieve(const wxString &name, std::vector<double> &values)
{
	int idx = Find(name);
	if (idx < 0)
		return false;

	values = m_dist[idx].values;
	return true;

}

wxArrayString LHS::ListAll()
{
	wxArrayString list;
	for (size_t i=0;i<m_dist.size();i++)
		list.Add(m_dist[i].name);
	return list;
}

void LHS::Remove(const wxString &name)
{
	int idx = Find(name);
	if (idx < 0) return;

	m_dist.erase( m_dist.begin() + idx );
}

void LHS::RemoveCorrelation(const wxString &name1, const wxString &name2)
{
	for (size_t i=0;i<m_corr.size();i++)
	{
		if (m_corr[i].name1 == name1 && m_corr[i].name2 == name2)
		{
			m_corr.erase( m_corr.begin() + i );
			return;
		}
	}
}

int LHS::Find(const wxString &name)
{
	for (size_t i=0;i<m_dist.size();i++)
		if (m_dist[i].name == name)
			return i;
	return -1;
}

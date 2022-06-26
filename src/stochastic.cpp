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

#include <wx/filefn.h>
#include <wx/stopwatch.h>
#include <wx/tokenzr.h>
#include <wx/utils.h>
#include <wx/filename.h>
#include <wx/progdlg.h>
#include <wx/dir.h>

#include <wex/metro.h>
#include <wex/utils.h>

#include "main.h"
#include "casewin.h"
#include "stochastic.h"
#include "variablegrid.h"

char const *lhs_dist_names[LHS_NUMDISTS] = {
    "Uniform,Min,Max",
    "Normal,Mean (mu),Std. Dev. (sigma)",
    "Lognormal,Mean,ErrorF",
    "Lognormal-N,Mean,Std. Dev.",
    "Triangular,A,B,C",
    "Gamma,Alpha,Beta",
    "Poisson,Lambda",
    "Binomial,P,N",
    "Exponential,Lambda",
    "Weibull,Alpha or k (shape parameter),Beta or lambda (scale parameter)",
    "UserCDF,N"
};



LHS::LHS()
{
	m_npoints = 500;
	m_seedval = 0;
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
	wxString workdir(wxFileName::GetTempDir());
	
	wxString lhsexe( SamApp::GetRuntimePath() + "/bin/" + wxString(LHSBINARY) );

	if (!wxFileExists(lhsexe))
	{
		m_errmsg = "Sandia LHS executable does not exist: " + lhsexe;
		return false;
	}

	// write lhsinputs.lhi file
	wxString inputfile = workdir + "/SAMLHS.LHI";
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
		case LHS_LOGNORMAL_N:
			fprintf(fp, "%s LOGNORMAL-N %lg %lg\n", (const char*)m_dist[i].name.c_str(),
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
		case LHS_WEIBULL:
			fprintf(fp, "%s WEIBULL %lg %lg\n", (const char*)m_dist[i].name.c_str(),
				m_dist[i].params[0],
				m_dist[i].params[1]);
			break;
		case LHS_USERCDF:
			ncdfpairs = (int) m_dist[i].params[0];
			fprintf(fp, "%s DISCRETE CUMULATIVE %d #\n", (const char*)m_dist[i].name.c_str(), ncdfpairs);
			// update for uniform discrete distributions initially
			if (ncdfpairs <= 0)
			{
				m_errmsg.Printf("user defined CDF error: too few [value,cdf] pairs in list: %d pairs should exist.", ncdfpairs);
				fclose(fp);
				return false;
			}
			/*
			for (int j = 0; j<ncdfpairs; j++)
			{
				double cdf = (j + 1);
				cdf /= (double)ncdfpairs;
				if (cdf > 1.0) cdf = 1.0;
				fprintf(fp, "  %d %lg", j, cdf);
				if (j == ncdfpairs - 1) fprintf(fp, "\n");
				else fprintf(fp, " #\n");
			}
			*/
			
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
	if( wxFileExists( workdir + "/SAMLHS.LSP" ) )
		wxRemoveFile( workdir + "/SAMLHS.LSP" );

	if( wxFileExists( workdir + "/LHS.ERR" ) )
		wxRemoveFile( workdir + "/LHS.ERR" );

	// run the executable synchronously
	wxString curdir = wxGetCwd();
	wxSetWorkingDirectory( workdir );
	wxString execstr =  wxString('"' + lhsexe + "\" SAMLHS.LHI"); 
	bool exe_ok = ( 0 == wxExecute( execstr, wxEXEC_SYNC|wxEXEC_HIDE_CONSOLE ) );
	wxSetWorkingDirectory(curdir);
	exe_ok = true;
	
	if (wxFileExists(workdir + "/LHS.ERR"))
	{
		m_errmsg = "LHS error.  There could be a problem with the input setup.";
		FILE *ferr = fopen( wxString(workdir + "/LHS.ERR").c_str(), "r");
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
	wxString outputfile = workdir + "/SAMLHS.LSP";
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
			if ( n_runs == m_npoints )
				break;

			n_runs++;
			int n = atoi(buf.c_str());
			if (n != n_runs)
			{
				m_errmsg = wxString::Format("output file formatting error (run count %d!=%d) at line %d: ",n, n_runs, nline) + buf;
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

	fclose( fp );


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

#ifdef __WXMSW__
#define STWBINARY "stepwise.exe"
#else
#define STWBINARY "stepwise.bin"
#endif

Stepwise::Stepwise()
{
	/* nothing to do */
}


void Stepwise::Reset()
{
	m_inputs.clear();
	m_output_vec.clear();
	m_err.Empty();
}

bool Stepwise::Exec( )
{
	wxString workdir( wxFileName::GetTempDir() );
	
	wxString exe( SamApp::GetRuntimePath() + "/bin/" + STWBINARY );
	if (!wxFileExists(exe))
	{
		m_err = "STEPWISE executable does not exist: " + exe;
		return false;
	}

	// check inputs and outputs
	int datalen = -1;
	for (size_t i=0;i<m_inputs.size();i++)
	{
		if (datalen < 0) datalen = m_inputs[i].vec.size();

		if ((int)m_inputs[i].vec.size() != datalen)
		{
			m_err = "Inconsistent input data vector lengths.";
			return false;
		}
	}

	if ((int)m_output_vec.size() != datalen)
	{
		m_err = "Inconsistent output data vector length.";
		return false;
	}

	// write input vector file
	wxString input_data = workdir + "/input_data.txt";
	FILE *fp = fopen(input_data.c_str(), "w");
	if (!fp)
	{
		m_err = "Could not open input_data.txt for writing.";
		return false;
	}

	// write headers
	for (size_t i=0;i<m_inputs.size();i++)
		fprintf(fp, "%s%c", (const char*)m_inputs[i].name.c_str(), i<m_inputs.size()-1 ? '\t' : '\n');

	// write data columns
	for (int i=0;i<datalen;i++)
		for (size_t j=0;j<m_inputs.size();j++)
			fprintf(fp, "%lg%c", m_inputs[j].vec[i], j<m_inputs.size()-1 ? '\t' : '\n');

	fclose(fp);

	// write output vector file
	wxString output_data = workdir + "/output.txt";
	fp = fopen(output_data.c_str(), "w");
	if (!fp)
	{
		m_err = "Could not open output.txt for writing.";
		return false;
	}

	for(int i=0;i<datalen;i++)
		fprintf(fp, "%lg\n", m_output_vec[i]);

	fclose(fp);

	// write control file
	wxString control_file = workdir + "/stepin.txt";
	fp = fopen(control_file.c_str(), "w");
	if (!fp)
	{
		m_err = "Could not open stepin.txt for writing.";
		return false;
	}

	fprintf(fp, "stp_test_usr.inp               ! user file name\n");
	fprintf(fp, "stp_test_ind.dat               ! independent (input) data file name\n");
	fprintf(fp, "stp_test_dep.dat               ! dependent (output) data file name\n");
	fprintf(fp, "stp_test_out.out               ! result file name\n");
	fprintf(fp, "1                              ! TITLE -  1: include title ; 0 : do not include title\n");
	fprintf(fp, "First_Analysis                 ! title if included: up to 30 characters\n");
	fprintf(fp, "%d                             ! number of input parameters\n", (int)m_inputs.size());
	fprintf(fp, "1                              ! number of timesteps (not implemented yet)\n");
	fprintf(fp, "1                              ! LABEL - 0: no label, 1: label following, 2: input label in input file\n");
	fprintf(fp, "Y                              ! output label (for option 1 in label) \n");
	fprintf(fp, "0                              ! BACKWARD regression ; 0= do not include ; 1 = include\n");
	fprintf(fp, "1                              ! STEPWISE regression ; 0= do not include ; 1 = include\n");
	fprintf(fp, "0.05                           ! SIGIN  for STEPWISE regression (option 1 in Stepwise)\n");
	fprintf(fp, "0.05                           ! SIGOUT for STEPWISE regression (option 1 in Stepwise)\n");
	fprintf(fp, "0                              ! Forced variables - 1: include - 0: do not include\n");
	fprintf(fp, "0                              ! Dropped variables - 1: include - 0: do not include\n");
	fprintf(fp, "0                              ! PRESS - 1: include - 0: do not include\n");
	fprintf(fp, "1                              ! RANK - 1: include - 0: do not include\n");
	fprintf(fp, "0                              ! WEIGHT - 1: include - 0: do not include\n");
	fclose(fp);

	/*
-------- EXAMPLE INPUT FILE FROM C.Sallaberry August 2010 for STEPWISE 2.21a WIPP -----------

stp_test.inp                   ! user file name
stp_test_z_ind.dat             ! independent (input) data file name
stp_test_z_dep.dat             ! dependent (output) data file name
stp_test_z_out.out             ! result file name
1                              ! TITLE -  1: include title ; 0 : do not include title
Stepwise_Test_#1               ! title if included: up to 30 characters
18                             ! number of input parameters
1                              ! number of timesteps (not implemented yet)
1                              ! LABEL - 0: no label, 1: label following, 2: input label in input file
Y                              ! output label (for option 1 in label) 
0                              ! BACKWARD regression ; 0= do not include ; 1 = include
1                              ! STEPWISE regression ; 0= do not include ; 1 = include
0.1                            ! SIGIN  for STEPWISE regression (option 1 in Stepwise)
0.1                            ! SIGOUT for STEPWISE regression (option 1 in Stepwise)
1                              ! Forced variables - 1: include - 0: do not include
1                              ! number of forced variables
6                              ! Forced variable #
1                              ! Dropped variables - 1: include - 0: do not include
1                              ! Number of dropped variables
16                             ! Dropped variables #
1                              ! PRESS - 1: include - 0: do not include
1                              ! RANK - 1: include - 0: do not include
0                              ! WEIGHT - 1: include - 0: do not include

*/

	// all files written, now change folders and run STEPWISE

	// delete any output file that may exist
	if ( wxFileExists( workdir + "/result.txt" ) )	wxRemoveFile( workdir + "/result.txt" );
	if ( wxFileExists( workdir + "/stp_test_usr.inp" ) )	wxRemoveFile( workdir + "/stp_test_usr.inp" );
	if ( wxFileExists( workdir + "/stp_test_ind.dat" ) )	wxRemoveFile( workdir + "/stp_test_ind.dat" );
	if ( wxFileExists( workdir + "/stp_test_dep.dat" ) )	wxRemoveFile( workdir + "/stp_test_dep.dat" );
	if ( wxFileExists( workdir + "/stp_test_out.out" ) )	wxRemoveFile( workdir + "/stp_test_out.out" );


	wxString curdir = wxGetCwd();
	wxSetWorkingDirectory( workdir );
	wxExecute( '"' + exe + '"', wxEXEC_SYNC|wxEXEC_HIDE_CONSOLE );
	wxSetWorkingDirectory(curdir);


	wxString results_file = workdir + "/result.txt";
	fp = fopen(results_file.c_str(), "r");
	if (!fp)
	{
		m_err = "Could not open result.txt file for reading.";
		return false;
	}

	char cbuf[2048];
	
	fgets(cbuf,2047, fp); // header line
	fgets(cbuf,2047, fp); // delimiter line ==========

	int nlines=0;
	while ( !feof( fp ) )
	{
		if (nlines++ > (int)m_inputs.size())
			break;

		fgets( cbuf, 2047, fp );

		wxArrayString parts = wxStringTokenize( cbuf, " \t:", wxTOKEN_STRTOK);
		if (parts.Count() != 4)
			continue;

		for (size_t i=0;i<m_inputs.size();i++)
		{
			if (m_inputs[i].name.Lower() == parts[0].Lower())
			{
				m_inputs[i].R2 = atof( parts[1].c_str() );
				m_inputs[i].R2inc = atof( parts[2].c_str() );
				m_inputs[i].SRC = atof( parts[3].c_str() );
				m_inputs[i].calculated = true;
			}
		}
	}

	fclose(fp);

	return true;
}

wxString Stepwise::ErrorMessage()
{
	return m_err;
}

// set simulation inputs and results
void Stepwise::SetInputVector(const wxString &name, const std::vector<double> &data)
{
	if (name.IsEmpty()) return;

	bool found = false;
	for (size_t i=0;i<m_inputs.size();i++)
	{
		if (m_inputs[i].name == name)
		{
			m_inputs[i].vec = data;
			found = true;
		}
	}


	if (!found)
	{
		m_inputs.push_back( datavec() );
		datavec &x = m_inputs[m_inputs.size()-1];
		x.name = name;
		x.vec = data;
		x.calculated = false;
		x.R2 = 0;
		x.SRC = 0;
	}
}

void Stepwise::SetOutputVector(const std::vector<double> &data)
{
	m_output_vec = data;
}

bool Stepwise::GetStatistics(const wxString &name, double *R2, double *R2inc, double *SRC)
{
	for (size_t i=0;i<m_inputs.size();i++)
	{
		if (m_inputs[i].name == name && m_inputs[i].calculated )
		{
			if (R2) *R2 = m_inputs[i].R2;
			if (R2inc) *R2inc = m_inputs[i].R2inc;
			if (SRC) *SRC = m_inputs[i].SRC;
			return true;
		}
	}

	return false;
}

StochasticData::StochasticData()
{
	Seed = 0;
	N = 100;
}

void StochasticData::Copy( StochasticData &stat )
{
	Seed = stat.Seed;
	N = stat.N;
	Outputs = stat.Outputs;
	InputDistributions = stat.InputDistributions;
	Correlations = stat.Correlations;
}

void StochasticData::Write( wxOutputStream &_o )
{
	wxDataOutputStream out(_o);
	out.Write8( 0x8f );
	out.Write8( 1 );

	out.Write32( N );
	out.Write32( Seed );

	out.WriteString( wxJoin( Outputs, '|' ) );
	out.WriteString( wxJoin( InputDistributions, '|' ) );
	out.WriteString( wxJoin( Correlations, '|' ) );

	out.Write8( 0x8f );
}

bool StochasticData::Read( wxInputStream &_i )
{
	wxDataInputStream in(_i);
	wxUint8 code = in.Read8();
	in.Read8(); // ver

	N = in.Read32();
	Seed = in.Read32();
	Outputs = wxStringTokenize( in.ReadString(), "|" );
	InputDistributions = wxStringTokenize( in.ReadString(), "|" );
	Correlations = wxStringTokenize( in.ReadString(), "|" );

	return in.Read8() == code;
}



enum { ID_cboDistribution = wxID_HIGHEST+394, ID_cdfnum };

BEGIN_EVENT_TABLE( InputDistDialog, wxDialog )
    EVT_CHOICE( ID_cboDistribution, InputDistDialog::OnDistChange )
    EVT_NUMERIC(ID_cdfnum, InputDistDialog::OnCdfNumChange )
END_EVENT_TABLE()



InputDistDialog::InputDistDialog(wxWindow *parent, const wxString &title)
    : wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxScaleSize(750,350), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
{
    cboDistribution = new wxChoice(this, ID_cboDistribution, wxDefaultPosition, wxScaleSize(375,28));
	cboDistribution->SetMinSize(wxScaleSize(375, 28));
	cboDistribution->SetMaxSize(wxScaleSize(375, 28));
    for (int i = 0; i<LHS_NUMDISTS ; i++)
            cboDistribution->Append(wxString(::lhs_dist_names[i]).BeforeFirst(','));


    lblVarName = new wxStaticText(this, wxID_ANY, "VarName");
    lblVarValue = new wxStaticText(this, wxID_ANY, "VarValue");

    grid = new wxFlexGridSizer(2);
    grid->Add(new wxStaticText(this, wxID_ANY, "Variable name:"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
    grid->Add( lblVarName, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
    grid->Add( new wxStaticText( this, wxID_ANY, "Variable value:" ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
    grid->Add( lblVarValue, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );

    for( size_t i=0;i<4;i++ )
    {
        lbls[i] = new wxStaticText( this, wxID_ANY, "-----" );
        nums[i] = new wxNumericCtrl( this, wxID_ANY );
        grid->Add( lbls[i], 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
        grid->Add( nums[i], 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );

    }

    cdf_grid = new wxExtGridCtrl(this, wxID_ANY);
    cdf_grid->CreateGrid(2, 2);
	cdf_grid->SetColLabelValue(0, "Value");
	cdf_grid->SetColLabelValue(1, "CDF");
    cdf_grid->EnableEditing(true);
    cdf_grid->EnableCopyPaste(true);
	// user CDF (DISCRETE CONTINUOUS) must have at least two values monotonically increasing and last CDF value 1
	cdf_grid->SetCellValue(0, 0, "0");
	cdf_grid->SetCellValue(0, 1, "0.1");
	cdf_grid->SetCellValue(0, 0, "1");
	cdf_grid->SetCellValue(0, 1, "1");

    cdf_numlabel = new wxStaticText(this, wxID_ANY, "Number of observables:");
    cdf_num = new wxNumericCtrl( this, ID_cdfnum, 1);
    
    wxBoxSizer *cdf_numsizer = new wxBoxSizer(wxHORIZONTAL);
    cdf_numsizer->Add(cdf_numlabel, 0, wxALL | wxEXPAND, 2);
    cdf_numsizer->Add(cdf_num, 0, wxALL | wxEXPAND, 2);
    
	// add png images for distributions
	pngDistribution = new wxStaticBitmap(this, wxID_ANY, wxNullBitmap, wxDefaultPosition, wxSize(325, 235), 0);
	pngDistribution->SetMinSize(wxSize(325, 235));

	wxBoxSizer* sizerR = new wxBoxSizer(wxVERTICAL);
	sizerR->Add(pngDistribution, 0, wxALL | wxEXPAND, 5);

	wxBoxSizer* sizerL = new wxBoxSizer(wxVERTICAL);
	sizerL->Add(cboDistribution, 0, wxALL | wxEXPAND, 5);
	sizerL->Add(grid, 1, wxALL | wxEXPAND, 0);
    sizerL->Add(cdf_numsizer);
    sizerL->Add(cdf_grid, 1, wxALL | wxEXPAND, 0);


	wxBoxSizer* sizerH = new wxBoxSizer(wxHORIZONTAL);
	sizerH->Add(sizerL, 1, wxALL | wxEXPAND, 5);
	sizerH->Add(sizerR, 0, wxALL | wxEXPAND, 5);

	wxBoxSizer* sizer = new wxBoxSizer(wxVERTICAL);
	sizer->Add(sizerH, 1, wxALL | wxEXPAND, 5);
    sizer->Add(CreateButtonSizer(wxOK | wxCANCEL), 0, wxALL | wxEXPAND, 10);
    SetSizer( sizer );

	cboDistribution->Select(LHS_NORMAL);


    lbls[2]->Hide(); nums[2]->Hide();
    lbls[3]->Hide(); nums[3]->Hide();
}




void InputDistDialog::Setup(const wxString &name, const wxString &value,
    int DistType, double p0, double p1, double p2, double p3)
{
    lblVarName->SetLabel(name);
    lblVarValue->SetLabel(value);
    m_disttype = DistType;
    cboDistribution->SetSelection(DistType);
    nums[0]->SetValue(p0);
    nums[1]->SetValue(p1);
    nums[2]->SetValue(p2);
    nums[3]->SetValue(p3);
    UpdateLabels();
}

//	void Setup(const wxString &name, const wxString &value,
//		int DistType, wxArrayString listValues, wxArrayString cdf_values)
void InputDistDialog::Setup(	int DistType, wxArrayString listValues, wxArrayString cdf_values)
{
    //lblVarName->SetLabel(name);
    //lblVarValue->SetLabel(value);
    m_disttype = DistType;
    cboDistribution->SetSelection(DistType);
    cdf_grid->ClearGrid();
    int num_rows = listValues.Count();
    cdf_num->SetValue(num_rows);
    if ((num_rows == 0) || ((int)cdf_values.Count() != num_rows))
    {
        wxMessageBox("Error setting up user CDF.", "Stochastic Simulation Message");
        return;
    }
    cdf_grid->Freeze();
    cdf_grid->ResizeGrid(num_rows, 2);
//    cdf_grid->HideRowLabels();
    cdf_grid->SetColLabelValue(0, "Value");
    cdf_grid->SetColLabelValue(1, "CDF");
    for (int i = 0; i < num_rows; i++)
    {
        cdf_grid->SetCellValue(i, 0, listValues[i]);
		double val;
		bool readonly;
		if (listValues[i].ToDouble(&val))
			cdf_grid->SetReadOnly(i, 0, false);
		else
			cdf_grid->SetReadOnly(i, 0, true);
		cdf_grid->SetCellValue(i, 1, cdf_values[i]);
    }
    cdf_grid->AutoSize();
    cdf_grid->Thaw();
    UpdateLabels();
}

void InputDistDialog::UpdateLabels()
{
    int cur_selection = cboDistribution->GetSelection();
    wxArrayString parts = wxStringTokenize(::lhs_dist_names[cur_selection], ",");


	wxString img_filename = SamApp::GetRuntimePath() + "png/" + parts[0].Lower() + ".png";
	if (wxFileExists(img_filename)) {
		wxImage img(img_filename, wxBITMAP_TYPE_PNG);
		//img.Rescale(320, 228);
		wxBitmap bmp(img);
		pngDistribution->SetBitmap(bmp);
		pngDistribution->Show(true);
	}
	else
		pngDistribution->Show(false);

    m_disttype = cur_selection;
    
    int i;
    if (m_disttype == LHS_USERCDF)
    {
        cdf_numlabel->Show(true);
        cdf_num->Show(true);
        cdf_grid->Show(true);
        grid->Show(false);
        cboDistribution->SetSelection(LHS_USERCDF);
    }
    else
    {
        /*
        if (cur_selection == LHS_USERCDF)
        {
            cur_selection = LHS_NORMAL;
            cboDistribution->SetSelection(LHS_NORMAL);
            parts = wxStringTokenize(::lhs_dist_names[cur_selection], ",");
        }
        */
        cdf_numlabel->Show(false);
        cdf_num->Show(false);
        cdf_grid->Show(false);
        grid->Show(true);
        for (i = 0; i<4; i++)
        {
            lbls[i]->Hide();
            nums[i]->Hide();
        }

        for (i = 1; i < (int)parts.Count(); i++)
        {
            lbls[i - 1]->SetLabel(parts[i] + ":");
            lbls[i - 1]->Show();
            nums[i - 1]->Show();
        }
    }
    Layout();
    Refresh();
}

void InputDistDialog::OnDistChange(wxCommandEvent &)
{
    UpdateLabels();
}

void InputDistDialog::OnCdfNumChange(wxCommandEvent &)
{
    int nrow = cdf_num->Value();
    if (nrow > 0)
        cdf_grid->ResizeGrid(nrow, 2);
}



#include "case.h"
#include "casewin.h"
#include "simulation.h"

enum {
  ID_lstOutputMetrics = wxID_HIGHEST+414,
  ID_btnRemoveInput,
  ID_btnAddInput,
  ID_btnAddOutput,
  ID_btnRemoveOutput,
  ID_m_seed,
  ID_btnAddCorr,
  ID_btnEditCorr,
  ID_btnRemoveCorr,
  ID_m_corrList,
  ID_m_inputList,
  ID_m_N,
  ID_btnComputeSamples,
  ID_btnEditInput,
  ID_Simulate,
  ID_Select_Folder,
  ID_Check_Weather,
  ID_Combo_Weather,
  ID_Show_Weather_CDF,
  ID_GRID,
  ID_SHOW_ALL_INPUTS
};

BEGIN_EVENT_TABLE( StochasticPanel, wxPanel )
	EVT_NUMERIC( ID_m_N, StochasticPanel::OnNChange)
	EVT_NUMERIC( ID_m_seed, StochasticPanel::OnSeedChange)
	
	EVT_BUTTON( ID_btnAddInput, StochasticPanel::OnAddInput)
	EVT_BUTTON( ID_btnRemoveInput, StochasticPanel::OnRemoveInput)
	EVT_BUTTON( ID_btnEditInput, StochasticPanel::OnEditInput)
	EVT_LISTBOX_DCLICK( ID_m_inputList, StochasticPanel::OnEditInput)
	
	EVT_BUTTON( ID_btnAddOutput, StochasticPanel::OnAddOutput)
	EVT_BUTTON( ID_btnRemoveOutput, StochasticPanel::OnRemoveOutput)

	EVT_BUTTON( ID_btnAddCorr, StochasticPanel::OnAddCorr)
	EVT_BUTTON( ID_btnRemoveCorr, StochasticPanel::OnRemoveCorr)
	EVT_BUTTON( ID_btnEditCorr, StochasticPanel::OnEditCorr)
	EVT_LISTBOX_DCLICK( ID_m_corrList, StochasticPanel::OnEditCorr)
	EVT_BUTTON( ID_btnComputeSamples, StochasticPanel::OnComputeSamples)

	EVT_BUTTON( ID_Simulate, StochasticPanel::OnSimulate )

	EVT_BUTTON(ID_Select_Folder, StochasticPanel::OnSelectFolder)
	EVT_CHECKBOX(ID_Check_Weather, StochasticPanel::OnCheckWeather)
	EVT_COMBOBOX(ID_Combo_Weather, StochasticPanel::OnComboWeather)
	EVT_BUTTON(ID_Show_Weather_CDF, StochasticPanel::OnShowWeatherCDF)

	EVT_GRID_CMD_LABEL_RIGHT_CLICK(ID_GRID, StochasticPanel::OnGridColLabelRightClick)
	EVT_MENU(ID_SHOW_ALL_INPUTS, StochasticPanel::OnMenuItem)

	END_EVENT_TABLE()

StochasticPanel::StochasticPanel(wxWindow *parent, Case *cc)
	 : wxPanel( parent ), m_case( cc ), m_sd( m_case->Stochastic() )
{
	wxBoxSizer *sizer_main = new wxBoxSizer( wxVERTICAL );

	wxPanel *top_panel = new wxPanel( this );
	top_panel->SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );
	
	wxSize sz;
	m_N = new wxNumericCtrl(top_panel, ID_m_N, 100, wxNUMERIC_INTEGER);
	sz = m_N->GetBestSize();
	m_N->SetInitialSize( wxSize( sz.x/2, sz.y ) );

	m_seed = new wxNumericCtrl(top_panel, ID_m_seed, -1, wxNUMERIC_INTEGER);
	sz = m_seed->GetBestSize();
	m_seed->SetInitialSize( wxSize( sz.x/2,sz.y ) );

	wxBoxSizer *top_sizer = new wxBoxSizer( wxHORIZONTAL );	
	top_sizer->Add( new wxMetroButton(top_panel, ID_Simulate, "Run simulations", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW), 0, wxALL|wxEXPAND, 0 );
	top_sizer->Add( m_useThreads = new wxCheckBox( top_panel, wxID_ANY, "Use threads"), 0, wxLEFT|wxRIGHT|wxEXPAND, 3);
	m_useThreads->SetValue( true );
	m_useThreads->Hide();
	top_sizer->AddStretchSpacer();
	wxStaticText *lbl;
	top_sizer->Add( lbl = new wxStaticText(top_panel, wxID_ANY, "Number of samples:"), 0, wxLEFT|wxRIGHT|wxALIGN_CENTER_VERTICAL, 3 );
	lbl->SetForegroundColour( *wxWHITE );
	top_sizer->Add( m_N, 0, wxLEFT|wxRIGHT|wxALIGN_CENTER_VERTICAL, 3 );
	top_sizer->Add( lbl = new wxStaticText(top_panel, wxID_ANY, "Seed value (0 for random):"), 0, wxLEFT|wxRIGHT|wxALIGN_CENTER_VERTICAL, 3 );
	lbl->SetForegroundColour( *wxWHITE );
	top_sizer->Add( m_seed, 0, wxLEFT|wxRIGHT|wxALIGN_CENTER_VERTICAL, 3 );
	top_sizer->Add( new wxMetroButton(top_panel, ID_btnComputeSamples, "Compute samples"), 0, wxALL, 0 );

	top_panel->SetSizer( top_sizer );

	sizer_main->Add( top_panel, 0, wxALL|wxEXPAND, 0 );


	wxStaticBoxSizer *szbox = new wxStaticBoxSizer(wxHORIZONTAL, this, "Configure");

	wxBoxSizer *sizer_inputs = new wxBoxSizer( wxHORIZONTAL );	
	sizer_inputs->Add( new wxStaticText( szbox->GetStaticBox(), wxID_ANY, "Input variables:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	sizer_inputs->Add( new wxButton(szbox->GetStaticBox(), ID_btnAddInput, "Add...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sizer_inputs->Add( new wxButton(szbox->GetStaticBox(), ID_btnEditInput, "Edit...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sizer_inputs->Add( new wxButton(szbox->GetStaticBox(), ID_btnRemoveInput, "Remove", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	
	wxBoxSizer *sizer_inputs_v = new wxBoxSizer( wxVERTICAL );
	sizer_inputs_v->Add( sizer_inputs, 0, wxALL|wxEXPAND, 3 );
	m_inputList = new wxListBox(szbox->GetStaticBox(), ID_m_inputList);
	m_inputList->SetInitialSize( wxScaleSize( 300, 100 ) );
	sizer_inputs_v->Add( m_inputList, 0, wxALL|wxEXPAND, 5 );


	wxBoxSizer *sizer_corr = new wxBoxSizer( wxHORIZONTAL );
	sizer_corr->Add( new wxStaticText(szbox->GetStaticBox(), wxID_ANY, "Correlations:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	sizer_corr->Add( new wxButton(szbox->GetStaticBox(), ID_btnAddCorr, "Add...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sizer_corr->Add( new wxButton(szbox->GetStaticBox(), ID_btnEditCorr, "Edit...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sizer_corr->Add( new wxButton(szbox->GetStaticBox(), ID_btnRemoveCorr, "Remove", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );	
	// weather file option
	wxBoxSizer *sizer_wf = new wxBoxSizer(wxHORIZONTAL);
	m_chk_weather_files = new wxCheckBox(this, ID_Check_Weather, "Include weather file normal distribution based on DNI or DHI");
	sizer_wf->Add(m_chk_weather_files, 0, wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL, 0);

	wxArrayString weather_file_columns;
	weather_file_columns.Add("DNI");
	weather_file_columns.Add("GHI");
	wxString InitialValue = "DNI";
	m_cbo_weather_files = new wxComboBox(this, ID_Combo_Weather, InitialValue, wxDefaultPosition, wxDefaultSize, weather_file_columns, wxCB_READONLY);
	sizer_wf->Add(m_cbo_weather_files, 0, wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL, 10);

	wxStaticText *label = new wxStaticText(this, wxID_ANY, "Weather file folder:");
	sizer_wf->Add(label, 0,  wxRIGHT | wxALIGN_CENTER_VERTICAL, 2);
	sizer_wf->Add(m_folder = new wxTextCtrl(this, wxID_ANY), 2, wxALL | wxALIGN_CENTER_VERTICAL, 3);
	m_folder->SetEditable(false);
	sizer_wf->Add(new wxButton(this, ID_Select_Folder, "Choose folder...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxRIGHT | wxALIGN_CENTER_VERTICAL, 0);
	sizer_wf->Add(new wxButton(this, ID_Show_Weather_CDF, "Show CDF...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxRIGHT | wxALIGN_CENTER_VERTICAL, 0);


	wxBoxSizer *sizer_corr_v = new wxBoxSizer( wxVERTICAL );
	sizer_corr_v->Add( sizer_corr, 0, wxALL|wxEXPAND, 3 );
	m_corrList = new wxListBox(szbox->GetStaticBox(), ID_m_corrList);
	m_corrList->SetInitialSize(wxScaleSize(300, 100));
//	m_corrList->SetInitialSize(wxScaleSize(300, 50));
	sizer_corr_v->Add(m_corrList, 0, wxALL | wxEXPAND, 5);
//	sizer_corr_v->Add(sizer_wf, 0, wxALL | wxEXPAND, 3);

	wxBoxSizer *sizer_out = new wxBoxSizer( wxHORIZONTAL );
	sizer_out->Add( new wxStaticText(szbox->GetStaticBox(), wxID_ANY, "Outputs:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	sizer_out->Add( new wxButton(szbox->GetStaticBox(), ID_btnAddOutput, "Add...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sizer_out->Add( new wxButton(szbox->GetStaticBox(), ID_btnRemoveOutput, "Remove", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	
	wxBoxSizer *sizer_out_v = new wxBoxSizer( wxVERTICAL );
	sizer_out_v->Add( sizer_out, 0, wxALL|wxEXPAND, 3 );
	m_outputList = new wxListBox( szbox->GetStaticBox(), wxID_ANY );
	m_outputList->SetInitialSize( wxScaleSize( 200, 100 ) );	
	sizer_out_v->Add( m_outputList, 0, wxALL|wxEXPAND, 5 );

	szbox->Add( sizer_inputs_v, 0, wxALL|wxEXPAND, 5 );
	szbox->Add( sizer_corr_v, 0, wxALL|wxEXPAND, 5 );
	szbox->Add( sizer_out_v, 0, wxALL|wxEXPAND, 5 );
	
	sizer_main->Add( szbox, 0, wxALL, 5 );
	
	sizer_main->Add(sizer_wf, 0, wxALL | wxEXPAND, 3);


	m_dataGrid = new wxExtGridCtrl( this, ID_GRID );
	m_dataGrid->CreateGrid( 1, 1 );
	m_dataGrid->SetCellValue(0, 0, "No results.");

	m_statGrid = new wxExtGridCtrl( this, wxID_ANY );
	m_statGrid->CreateGrid( 1, 1 );
	m_statGrid->SetCellValue(0, 0, "No results.");

	wxBoxSizer *sizer_grids = new wxBoxSizer( wxHORIZONTAL );
	sizer_grids->Add( m_dataGrid, 1, wxALL|wxEXPAND, 10 );
	sizer_grids->Add( m_statGrid, 1, wxALL|wxEXPAND, 10 );

	sizer_main->Add( sizer_grids, 1, wxALL|wxEXPAND, 0 );

	SetSizer( sizer_main );


	// do not change unless persistence is changed.
	m_weather_folder_varname = "stochastic_weather_folder";
	m_weather_folder_displayname = wxString::Format("Weather Files (%s)", m_cbo_weather_files->GetValue()) ;

	m_regenerate_samples = true;

	UpdateFromSimInfo();
	UpdateWeatherFileControls();

}

StochasticPanel::~StochasticPanel()
{
	if (m_sims.size() > 0)
		for (size_t i = 0; i < m_sims.size(); i++)
			delete m_sims[i];
	m_sims.clear();
}

void StochasticPanel::OnGridColLabelRightClick(wxGridEvent &evt)
{
	m_selected_grid_col = evt.GetCol();
	m_selected_grid_row = evt.GetRow();
	if ((m_selected_grid_col < 0) && (m_selected_grid_row > -1)) // row header - skip upper left corner -1,-1
	{
		//	row menu
		wxPoint point = evt.GetPosition();
		int x, y;
		m_dataGrid->GetPosition(&x, &y);
		point.y += y;
		wxMenu *menu = new wxMenu;
		menu->Append(ID_SHOW_ALL_INPUTS, _T("Show inputs"));
		PopupMenu(menu, point);
	}
}


void StochasticPanel::OnMenuItem(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
	case ID_SHOW_ALL_INPUTS:
		if (m_dataGrid->GetNumberRows() > m_selected_grid_row && (int) m_sims.size() > m_selected_grid_row)
		{
				new VariableGridFrame(this, &SamApp::Project(), m_case, m_sims[m_selected_grid_row]->GetInputVarTable(), wxString::Format("Inputs for stochastic run %d", m_selected_grid_row + 1));
		}
		break;
	}
}


void StochasticPanel::UpdateWeatherFileList()
{
	wxString fld = m_folder->GetValue();

	if (!wxDirExists(fld))
	{
		wxMessageBox("Please choose a weather file folder.","Stochastic Simulation Message");
		return;
	}

	m_weather_files.Clear();
	wxArrayString val_list;
	wxDir::GetAllFiles(m_folder->GetValue(), &val_list);
    for (size_t j = 0; j < val_list.Count(); j++) {
        wxString ext = wxFileName(val_list[j]).GetExt().Lower();
        if (ext != "tm2" && ext != "epw" && ext != "csv" && ext != "smw" && ext != "srw")
            continue; // consistent with PVUncertainty
        m_weather_files.Add(wxFileNameFromPath(val_list[j]));
    }
}


int StochasticPanel::GetWeatherFileDistributionIndex()
{
	int ndx = -1;
	for (int j = 0; j < (int)m_sd.InputDistributions.Count(); j++)
	{
		if (GetVarNameFromInputDistribution(m_sd.InputDistributions[j]) == m_weather_folder_varname)
		{
			ndx = j;
			break;
		}
	}
	return ndx;
}

void StochasticPanel::UpdateWeatherFileControls()
{
	// find weather file distribution
	// format varname=folder=combo index
	int ndx = GetWeatherFileDistributionIndex();
	if (ndx >= 0)
	{
		wxArrayString parts = wxStringTokenize(m_sd.InputDistributions[ndx], ":");
		wxArrayString control_values = wxStringTokenize(parts[0], "=");
		if (control_values.Count() != 3)
		{
			m_sd.InputDistributions.RemoveAt(ndx);
			return;
		}
		// update folder list
		control_values[1].Replace(";", ":");
		m_folder->SetValue(control_values[1]);
		// update combo box
		m_cbo_weather_files->SetSelection(wxAtoi(control_values[2]));
		// update check box
		m_chk_weather_files->SetValue(true);
	}
	else
	{
		// update check box
		m_chk_weather_files->SetValue(false);
		// update folder list
		m_folder->SetValue("");
		// update combo box
		m_cbo_weather_files->SetSelection(0);
	}
}


void StochasticPanel::UpdateWeatherFileInputDistribution()
{
	// find weather file distribution
	// format varname=folder=combo index
	int ndx = GetWeatherFileDistributionIndex();
	wxString fld = m_folder->GetValue();

	if (!wxDirExists(fld))
	{
		wxMessageBox("Please choose a weather file folder.","Stochastic Simulation Message");
		return;
	}

	fld.Replace(":", ";");
	bool checked = m_chk_weather_files->GetValue();
	wxString input_distribution = m_weather_folder_varname + "=" + fld + "="
		+ wxString::Format("%d", m_cbo_weather_files->GetSelection());

	if (ndx >= 0)
	{
		if (checked)
		{
			wxArrayString parts = wxStringTokenize(m_sd.InputDistributions[ndx], ":");
			wxArrayString control_values = wxStringTokenize(parts[0], "=");
			if (control_values.Count() != 3)
			{
				m_sd.InputDistributions.RemoveAt(ndx);
				return;
			}
			parts[0] = input_distribution;
			m_sd.InputDistributions[ndx] = parts[0];
			for (size_t i = 1; i < parts.Count(); i++)
				m_sd.InputDistributions[ndx] += ":" + parts[i];
		}
		else 
		{ // not enabled so remove for later vector processing.
			m_sd.InputDistributions.RemoveAt(ndx);
		}
	}
	else
	{
		if (checked)
			m_sd.InputDistributions.Add(input_distribution);
	}
}

void StochasticPanel::UpdateWeatherFileSort()
{
	if (m_weather_files.Count() != m_weather_file_sums.size())
		return;

	size_t count = m_weather_files.Count();
	if (count < 1)
		return;

	for (size_t i = 0; i < count; i++)
	{
		size_t smallest = i;
		
		for (size_t j = i + 1; j < count; j++)
		{
			if (m_weather_file_sums[j] < m_weather_file_sums[smallest])
				smallest = j;
		}
		
		double d_temp = m_weather_file_sums[i];
		m_weather_file_sums[i] = m_weather_file_sums[smallest];
		m_weather_file_sums[smallest] = d_temp;

		wxString s_temp = m_weather_files[i];
		m_weather_files[i] = m_weather_files[smallest];
		m_weather_files[smallest] = s_temp;
	}
	/*
	wxString sums;
	for (size_t i = 0; i < m_weather_files.Count(); i++)
		sums += m_weather_files[i] + ", " 
		+ wxString::Format("=%lg\n", m_weather_file_sums[i]);
	wxMessageBox("Sorted\n" + sums);
	*/
}

bool StochasticPanel::GetWeatherFileForSum(const double sum, wxString *wf)
{
	bool found = false;
	if ((m_weather_files.Count() == m_weather_file_sums.size()) && (m_weather_files.Count() > 0))
	{
		// find nearest sum and return full weather file path for simulation
		double mindist = 1e99;
		int	minidx = -1;
		for (size_t i = 0; i < m_weather_file_sums.size(); i++)
		{
			double d = fabs(m_weather_file_sums[i] - sum);
			if (d < mindist)
			{
				mindist = d;
				minidx = i;
			}
		}
		// can put "closeness" criteria here
		found = (minidx > -1);
		*wf = m_weather_files[minidx];
	}
	return found;
}




void StochasticPanel::UpdateWeatherFileSums()
{
	UpdateWeatherFileList();
	if (m_weather_files.Count() < 1) return;
	
	wxString output_value = wxEmptyString;
	wxString selection = m_cbo_weather_files->GetValue().Lower();
	if (selection == "ghi")
		output_value = "annual_global";
	else if (selection == "dni")
		output_value = "annual_beam";
	else
		return;

	m_weather_file_sums.clear();
	
	ssc_data_t pdata = ssc_data_create();

	for (size_t i = 0; i < m_weather_files.Count(); i++)
	{
		wxString wf = m_folder->GetValue() + "/" + m_weather_files[i];
		ssc_data_set_string(pdata, "file_name", (const char*)wf.c_str());
		ssc_data_set_number(pdata, "header_only", 0);

		if (ssc_module_exec_simple_nothread("wfreader", pdata))
		{
			wxMessageBox("Error scanning '" + wf + "'", "Stochastic Simulation Message");
			continue;
		}

		ssc_number_t p;
		if (!ssc_data_get_number(pdata, output_value.c_str(),&p))
		{
			wxMessageBox("Error retrieving annual " + selection + " for '" + wf + "'", "Stochastic Simulation Message");
			continue;
		}
		m_weather_file_sums.push_back(p);
	}
	ssc_data_free(pdata);


	if (m_weather_file_sums.size() != m_weather_files.Count())
	{
		m_weather_file_sums.clear();
		wxMessageBox("Error with annual " + selection, "Stochastic Simulation Message");
	}
	/*
	else
	{
		wxString sums;
		for (size_t i = 0; i < m_weather_files.Count(); i++)
			sums += m_weather_files[i] + ", " + selection 
			+ wxString::Format("=%lg\n", m_weather_file_sums[i]);
		wxMessageBox("Success\n" + sums);
	}
	*/
}

void StochasticPanel::UpdateWeatherFileCDF()
{
	// sort weather file list based on combo box selection
	UpdateWeatherFileSums();
	UpdateWeatherFileSort();
	// create CDF values as sum value
	/* for example (from SolarPACES paper)
	w DISCRETE CUMULATIVE 30 #
	2.17768e+006 0.0333333 #
	2.18536e+006 0.0666667 #
	2.27818e+006 0.1 #
	2.33181e+006 0.133333 #
	2.37158e+006 0.166667 #
	2.45144e+006 0.2 #
	2.52132e+006 0.233333 #
	2.52851e+006 0.266667 #
	2.5429e+006 0.3 #
	2.5451e+006 0.333333 #
	2.55828e+006 0.366667 #
	2.57009e+006 0.4 #
	2.58152e+006 0.433333 #
	2.58213e+006 0.466667 #
	2.62372e+006 0.5 #
	2.62435e+006 0.533333 #
	2.63079e+006 0.566667 #
	2.64441e+006 0.6 #
	2.64566e+006 0.633333 #
	2.65612e+006 0.666667 #
	2.66647e+006 0.7 #
	2.67841e+006 0.733333 #
	2.69885e+006 0.766667 #
	2.70346e+006 0.8 #
	2.71064e+006 0.833333 #
	2.71747e+006 0.866667 #
	2.72253e+006 0.9 #
	2.74622e+006 0.933333 #
	2.76209e+006 0.966667 #
	2.79734e+006 1
	*/
	int ndx = GetWeatherFileDistributionIndex();
	if (ndx < 0)
	{
		wxMessageBox("Error retrieving weather file input distribution. Please enable weather file distribution before showing CDF.","Stochastic Simulation Message");
		return;
	}
	wxArrayString parts = wxStringTokenize(m_sd.InputDistributions[ndx], ":");
	if (parts.Count() < 1)
	{
		wxMessageBox("Error with weather file intput distribution count.", "Stochastic Simulation Message");
		return;
	}
	wxString input_distribution = parts[0];
	// set distribution type and number of value,cdf pairs
	int dist_type = LHS_USERCDF;
	int N = (int)m_weather_file_sums.size();
	input_distribution += wxString::Format(":%d:%d", dist_type, N);
	// set value,cdf pairs
	// parts[0]=varname
	// parts[1]=distribution type
	// parts[2]=number of element for user cdf N
	// parts[3], parts[4] are index=cdfvalue pairs
	// up to parts[3+2N-2], parts[3+2N-1]
	// repopulate weather file list and check count
	// check distribution and if count does not match - repopulate uniformly
	//	if ((N>0) && (parts.Count() != (3 + 2 * N)))
	//	{
	for (int j = 0; j<N; j++)
	{
		double cdf = (j + 1);
		cdf /= (double)N;
		if (cdf > 1.0) cdf = 1.0;
		input_distribution += wxString::Format(":%lg:%lg", m_weather_file_sums[j], cdf);
	}
	m_sd.InputDistributions[ndx] = input_distribution;
	//	}
}

void StochasticPanel::OnShowWeatherCDF(wxCommandEvent &)
{
	UpdateWeatherFileCDF();
    int ndx = GetWeatherFileDistributionIndex();
    if (ndx >= 0)
	{
        wxArrayString parts = wxStringTokenize(m_sd.InputDistributions[ndx], ":");
        size_t wf_count = 0;
        if (parts.Count() > 3)
        {
            wxDialog* dlg = new wxDialog(this, wxID_ANY, "Weather File CDF", wxDefaultPosition, wxScaleSize(800, 600), wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);
            wxExtGridCtrl* grid = new wxExtGridCtrl(dlg, wxID_ANY);
            grid->EnableCopyPaste(true);
            grid->CreateGrid(parts.Count()/2-1, 3);
            grid->Freeze();
            for (size_t j = 3; j < parts.Count(); j = j + 2)
            {
                wxString wf;
                if (GetWeatherFileForSum(wxAtof(parts[j]), &wf))
                {
                    grid->SetCellValue(wf_count, 0, wf);
                    grid->SetCellValue(wf_count, 1, parts[j]);
                    grid->SetCellValue(wf_count, 2, parts[j + 1]);
                    if ( !wf.empty() )
                        wf_count++;
                }
            }
            grid->SetColLabelValue(0, "Weather File");
            wxString selection = m_cbo_weather_files->GetValue().Lower();
            if (selection == "ghi")
                grid->SetColLabelValue(1, "GHI");
            else
                grid->SetColLabelValue(1, "DNI");
            grid->SetColLabelValue(2, "CDF");
            grid->AutoSize();
            grid->Thaw();
            dlg->Show();
        }
        else
            wxMessageBox("No CDF to show.", "Stochastic Simulation Message");
    }
}

void StochasticPanel::OnSelectFolder(wxCommandEvent &)
{
	wxString dir = wxDirSelector("Choose weather file folder", m_folder->GetValue());
	if (!dir.IsEmpty())
	{
		m_folder->ChangeValue(dir);
		UpdateWeatherFileInputDistribution();
	}
}

void StochasticPanel::OnCheckWeather(wxCommandEvent &)
{
	// update input distribution 
	// find weather file input and if none then add
	UpdateWeatherFileInputDistribution();
}

void StochasticPanel::OnComboWeather(wxCommandEvent &)
{
	// update input distribution
	UpdateWeatherFileInputDistribution();
}


wxString StochasticPanel::GetLabelFromVarName(const wxString &var_name)
{
	wxString label;
    if (var_name == m_weather_folder_varname)
    {
        //label = m_weather_folder_displayname;
        label = wxString::Format("Weather Files (%s)", m_cbo_weather_files->GetValue());
    }
    else
		label = m_case->GetConfiguration()->Variables.Label(var_name);
	return label;
}



void StochasticPanel::UpdateFromSimInfo()
{
	m_N->SetValue(m_sd.N);
	m_seed->SetValue(m_sd.Seed);

	int i;

	m_outputList->Freeze();
	m_outputList->Clear();
	wxArrayString vars, labels;
	Simulation::ListAllOutputs(m_case->GetConfiguration(), &vars, &labels, NULL, NULL, NULL, true);

	for (size_t i = 0; i < m_sd.Outputs.Count(); i++)
	{
		int idx = vars.Index(m_sd.Outputs[i]);
		if (idx >= 0)
			m_outputList->Append(labels[idx]);
		else
			m_outputList->Append("<Error - remove this>");
	}

	m_outputList->Thaw();

	m_inputList->Freeze();
	m_inputList->Clear();
	for (i = 0; i < (int)m_sd.InputDistributions.Count(); i++)
	{
		wxArrayString parts = wxStringTokenize(m_sd.InputDistributions[i], ":");
		if (parts.Count() < 2)
			continue;

		wxString item = GetVarNameFromInputDistribution(parts[0]);
		int disttype = atoi(parts[1].c_str());
		if (disttype < 0) disttype = 0;
		if (disttype >= LHS_NUMDISTS) disttype = LHS_NUMDISTS - 1;

		if (item == m_weather_folder_varname)
		{
			continue;
			/*
			wxArrayString wff = wxStringTokenize(parts[0], "=");
			if (wff.Count() == 2)
			{
				wxString path = wff[1];
				path.Replace(";", ":");
				if (m_folder->GetValue().IsEmpty())
					m_folder->SetValue(path);
				if (path.Lower() != m_folder->GetValue().Lower())
				{
					wxString fld = m_folder->GetValue();
					fld.Replace(":", ";");
					wff[1] = fld;
					parts[0] = wff[0] + "=" + wff[1];
					UpdateWeatherFileList();
				}
				if (m_weather_files.Count() == 0)
					UpdateWeatherFileList();
				item = m_weather_folder_displayname;
				//check weather file distribution for changes in number of files
				// parts[0]=varname
				// parts[1]=distribution type
				// parts[2]=number of element for user cdf N
				// parts[3], parts[4] are index=cdfvalue pairs
				// up to parts[3+2N-2], parts[3+2N-1]
				// repopulate weather file list and check count
				// check distribution and if count does not match - repopulate uniformly
				int N = (int)m_weather_files.Count();
				if ((N>0)&&(parts.Count() != (3 + 2 * N)))
				{
					parts[2] = wxString::Format("%d",N);
					//update to uniform distributed cdf function over weahter file list
					wxString input_distribution = parts[0] +
						wxString::Format(":%d:%d", LHS_USERCDF, N);
					for (int j = 0; j<N; j++)
					{
						double cdf = (j + 1);
						cdf /= (double)N;
						if (cdf > 1.0) cdf = 1.0;
						input_distribution += wxString::Format(":%d:%lg", j, cdf);
					}
					m_sd.InputDistributions[i]=input_distribution;
				}
			}
			*/
		}
		else
		{

			item = m_case->GetConfiguration()->Variables.Label(item);
		}

		//		if (parts.Count() == 6)
		if ((parts.Count() >= 6) || (disttype == LHS_USERCDF))
		{

			wxArrayString distparts = wxStringTokenize(::lhs_dist_names[disttype], ",");
			if (distparts.Count() > 1)
			{
				item += " ( " + distparts[0] + " [";
				for (size_t j = 1; j < distparts.Count(); j++)
				{
					item += parts[1 + j];
					if (j < distparts.Count() - 1) item += ",";
				}
				item += "] )";
			}
		}

		m_inputList->Append(item);
	}

	m_inputList->Thaw();

	m_corrList->Freeze();
	m_corrList->Clear();
	for (i = 0; i < (int)m_sd.Correlations.Count(); i++)
	{
		wxArrayString parts = wxStringTokenize(m_sd.Correlations[i], ":");
		if (parts.Count() < 3) continue;


		wxString l1 = GetLabelFromVarName(parts[0]);
		wxString l2 = GetLabelFromVarName(parts[1]);

		if (l1.IsEmpty() || l2.IsEmpty()) continue;

		m_corrList->Append(l1 + ", " + l2 + ", " + parts[2]);
	}

	m_corrList->Thaw();

}


void StochasticPanel::OnNChange(wxCommandEvent &)
{
	m_sd.N = m_N->AsInteger();
}

void StochasticPanel::OnSeedChange(wxCommandEvent &)
{
	m_sd.Seed = m_seed->AsInteger();
}

void StochasticPanel::OnAddInput(wxCommandEvent &)
{
	wxArrayString varlist;
	int i;
	for (i=0;i<(int)m_sd.InputDistributions.Count();i++)
		varlist.Add(GetVarNameFromInputDistribution( m_sd.InputDistributions[i]));
	
	wxArrayString names, labels;
	ConfigInfo *ci = m_case->GetConfiguration();
	VarInfoLookup &vil = ci->Variables;

//	names.Add(m_weather_folder_varname);
//	labels.Add("User weather file folder/" + m_weather_folder_displayname);

	for (VarInfoLookup::iterator it = vil.begin(); it != vil.end(); ++it)
	{
		wxString name = it->first;
		VarInfo &vi = *(it->second);
		VarValue *vv = m_case->Values().Get(name);
		bool single_value = false;
		if (vv && vv->Type() == VV_ARRAY && vv->Length() == 1)
			single_value = true;
		// update to select only "Parametric" variables and numbers or array with single value
		if ( vi.Flags & VF_PARAMETRIC && ((vi.Type == VV_NUMBER ) || single_value))
		{
			wxString label = vi.Label;
			if (label.IsEmpty())
				label = "{ " + name + " }";
			if (!vi.Units.IsEmpty())
				label += " (" + vi.Units + ")";
			if (!vi.Group.IsEmpty())
				label = vi.Group + "/" + label;

			labels.Add(label);
			names.Add(name);
		}
	}

	wxSortByLabels(names, labels);
	SelectVariableDialog dlg(this, "Choose Inputs");
	dlg.SetItems(names, labels);
	dlg.SetCheckedNames( varlist );
	if (dlg.ShowModal() == wxID_OK)
	{
		varlist = dlg.GetCheckedNames();

		i = 0;
		// remove any input variables in StochasticData that are no longer in list
		while (i < (int)m_sd.InputDistributions.Count())
		{
			wxString var_name = GetVarNameFromInputDistribution(m_sd.InputDistributions[i]);
			if ((varlist.Index(var_name) < 0) && (var_name != m_weather_folder_varname))
					m_sd.InputDistributions.RemoveAt(i);// remove, do not increment i
			else
				i++;
		}

		// add any inputs not already in StatSimList
		for (i = 0; i < (int)varlist.Count(); i++)
		{
			bool found = false;
			for (int j = 0; j < (int)m_sd.InputDistributions.Count(); j++)
				if (GetVarNameFromInputDistribution(m_sd.InputDistributions[j]) == varlist[i])
					found = true;

			if (!found)
			{
				wxString var_name = varlist[i];
				VarValue *vv=NULL;
				wxArrayString val_list;
				
				if (var_name == m_weather_folder_varname)
				{
					continue;
					/*
					wxString path = m_folder->GetValue();
					path.Replace(":", ";"); // to store in Input distribution collection without issue with ":" delimiter
					var_name += "=" + path;
					UpdateWeatherFileList();
					val_list = m_weather_files;
					*/
				}
				else
				{
					vv = m_case->Values().Get(var_name);
					if (!vv)
						continue;
					VarInfo *vi = m_case->GetConfiguration()->Variables.Lookup(var_name);
					if (!vi)
						continue;
					val_list = vi->IndexLabels;
				}
				if (val_list.Count() > 0) // list value
				{ // default to user cdf with uniform values
					int dist_type = LHS_USERCDF;
					int ncdfpairs = val_list.Count();
					wxString input_distribution = var_name + wxString::Format(":%d:%d", dist_type, ncdfpairs);
					for (int j = 0; j<ncdfpairs; j++)
					{
						double cdf = (j + 1);
						cdf /= (double)ncdfpairs;
						if (cdf > 1.0) cdf = 1.0;
						input_distribution += wxString::Format(":%d:%lg", j, cdf);
					}
					m_sd.InputDistributions.Add(input_distribution);
				}
				else
				{ // default to normal distribution (for numeric values)
					m_sd.InputDistributions.Add(
						var_name + wxString::Format(":1:%lg:%lg:0:0",
						(double)vv->Value(), (double)0.15*vv->Value()));
				}
			}
		}

		UpdateFromSimInfo();
		m_regenerate_samples = true;

	}

}

int StochasticPanel::GetInputDistributionIndex(int idx)
{
	// update input editing and removal index if weather file distribution enabled.
	int ndx = idx;
	bool wf_input_found = false;
	for (int i = 0; ((i < (int)m_sd.InputDistributions.Count()) && (i < (idx+1))); i++)
	{
		wxString var_name = GetVarNameFromInputDistribution(m_sd.InputDistributions[i]);
		if (var_name == m_weather_folder_varname)
		{
			wf_input_found = true;
			break;
		}
	}
	if (wf_input_found) ndx++;
	return std::min(ndx, (int)m_sd.InputDistributions.Count());
}

void StochasticPanel::OnEditInput(wxCommandEvent &)
{
	int idx = m_inputList->GetSelection();
	if ( idx < 0 ) return;

	//wxString var = m_sd.InputDistributions[idx];
	idx = GetInputDistributionIndex(idx);
	wxString var_name = GetVarNameFromInputDistribution(m_sd.InputDistributions[idx]);

	wxArrayString parts = wxStringTokenize(m_sd.InputDistributions[idx], ":");
	if (parts.Count() < 2) return;
	int dist_type = wxAtoi(parts[1]);
	if ((parts.Count() < 6) && (dist_type != LHS_USERCDF)) return;

	ConfigInfo *ci = m_case->GetConfiguration();


	wxArrayString val_list;
	wxString label;
	wxString value;
/*	if (var_name == m_weather_folder_varname)
	{
		wxString path = m_folder->GetValue();
		wxDir::GetAllFiles(path, &val_list);
		m_weather_files.Clear();
		for (int j = 0; j < val_list.Count(); j++)
			m_weather_files.Add(wxFileNameFromPath(val_list[j]));
		path.Replace(":", ";"); // to store in Input distribution collection without issue with ":" delimiter
		var_name += "=" + path;
		val_list = m_weather_files;
		label = m_weather_folder_displayname;
	}
	else
	{
	*/
		VarValue *vptr = m_case->Values().Get(var_name);
		if (!vptr) return;
		value = wxString::Format("%g", vptr->Value());
		VarInfo *vi = ci->Variables.Lookup(var_name);
		if (!vi)
			return;
		val_list = vi->IndexLabels;
		label = ci->Variables.Label(var_name);
//	}

	InputDistDialog dlg(this, "Edit " + label + " Distribution");

	if (dist_type == LHS_USERCDF) {
		if (val_list.Count() < 1) // list value
		{
			for (size_t j = 3; j < parts.Count(); j += 2)
				val_list.Add(parts[j]);
		}
		int dist_type = wxAtoi(parts[1]);
		wxArrayString cdf_values;
		for (size_t j = 4; j < parts.Count(); j += 2) 
			cdf_values.Add(parts[j]);
		dlg.Setup(dist_type, val_list, cdf_values);
	}
	else
	{
		dlg.Setup(ci->Variables.Label(var_name), value,
			wxAtoi(parts[1]), wxAtof(parts[2]), wxAtof(parts[3]), wxAtof(parts[4]), wxAtof(parts[5]));
	}

	if (dlg.ShowModal()==wxID_OK)
	{
		int dist_type = dlg.cboDistribution->GetSelection();
		if (dist_type == LHS_USERCDF) // list value
		{
			int num_values = dlg.cdf_grid->GetNumberRows();
			wxString input_dist = var_name + wxString::Format(":%d:%d", dist_type, num_values);
			for (int j = 0; j < num_values; j++)
				input_dist += ":" + dlg.cdf_grid->GetCellValue(j, 0) + ":" + dlg.cdf_grid->GetCellValue(j, 1);
			m_sd.InputDistributions[idx] = input_dist;
		}
		else
		{
			m_sd.InputDistributions[idx] = var_name + ":"
				+ wxString::Format("%d", dist_type) + ":"
				+ wxString::Format("%lg", dlg.nums[0]->Value()) + ":"
				+ wxString::Format("%lg", dlg.nums[1]->Value()) + ":"
				+ wxString::Format("%lg", dlg.nums[2]->Value()) + ":"
				+ wxString::Format("%lg", dlg.nums[3]->Value());
		}

		UpdateFromSimInfo();
		m_regenerate_samples = true;

	}
}

void StochasticPanel::OnRemoveInput(wxCommandEvent &)
{
	int idx = m_inputList->GetSelection();
	if (idx < 0)
	{
		wxMessageBox("No input variable selected.", "Stochastic Simulation Message");
		return;
	}
	else
	{
		idx = GetInputDistributionIndex(idx);
		m_sd.InputDistributions.RemoveAt(idx);
	}

	UpdateFromSimInfo();
	m_regenerate_samples = true;


	if (m_inputList->GetCount() > 0)
		m_inputList->Select(0);
//	m_inputList->Select(idx - 1 >= 0 ? idx - 1 : idx);
}

void StochasticPanel::OnAddOutput(wxCommandEvent &)
{	
	wxArrayString names, labels, units, groups;
	Simulation::ListAllOutputs( m_case->GetConfiguration(), 
		&names, &labels, &units, &groups, NULL, true );

	for( size_t i=0;i<labels.size();i++ )
	{
		if ( !units[i].IsEmpty() )
			labels[i] += " (" + units[i] + ")";

		if ( !groups[i].IsEmpty() )
			labels[i] = groups[i] + "/" + labels[i];
	}

	wxSortByLabels(names, labels);
	SelectVariableDialog dlg(this, "Choose Output Metrics");
	dlg.SetItems(names, labels);
	dlg.SetCheckedNames( m_sd.Outputs );
	if (dlg.ShowModal() == wxID_OK)
	{
		m_sd.Outputs = dlg.GetCheckedNames();			
		UpdateFromSimInfo();
	}
}

void StochasticPanel::OnRemoveOutput(wxCommandEvent &)
{
	int idx = m_outputList->GetSelection();
	if (idx < 0)
		wxMessageBox("No output metric selected.", "Stochastic Simulation Message");
	else
		m_sd.Outputs.RemoveAt(idx);

	UpdateFromSimInfo();

	if (m_outputList->GetCount() > 0)
		m_outputList->Select(idx-1>=0?idx-1:idx);
}

void StochasticPanel::OnAddCorr(wxCommandEvent &)
{
	wxArrayString names;
	wxArrayString labels;

	for (size_t i=0;i<m_sd.InputDistributions.Count();i++)
	{
		wxString var_name = GetVarNameFromInputDistribution(m_sd.InputDistributions[i]);
		if (var_name == m_weather_folder_varname) continue;
		names.Add( var_name );

/*		if (var_name == m_weather_folder_varname)
		{
			labels.Add("User weather file folder/" + m_weather_folder_displayname);
		}
		else
		{*/
			VarInfo *v = m_case->GetConfiguration()->Variables.Lookup(var_name);
			if (!v)
			{
				labels.Add("<<Label Lookup Error>>");
				continue;
			}
			if (!v->Group.IsEmpty())
				labels.Add(v->Group + "/" + v->Label);
			else
				labels.Add(v->Label);
//		}
	}

	wxArrayString list;


	wxSortByLabels(names, labels);
	SelectVariableDialog dlg(this, "Choose Correlation Variables");
	dlg.SetItems(names, labels);
	if (dlg.ShowModal() == wxID_OK)
	{
		list = dlg.GetCheckedNames();

		if (list.Count() != 2)
		{
			wxMessageBox("You must choose exactly 2 input variables to correlate.", "Stochastic Simulation Message");
			return;
		}

		wxString scorr = wxGetTextFromUser("Enter correlation value (-1.0 < corr < 1.0):", "Edit", "0.0");
		if (!scorr.IsEmpty())
		{
			double corr = atof(scorr.c_str());
			if (corr <= -1) corr = -0.999;
			if (corr >= 1) corr = 0.999;
			m_sd.Correlations.Add( list[0] + ":" + list[1] + ":" + wxString::Format("%lg", corr ) );
			UpdateFromSimInfo();
			m_regenerate_samples = true;

		}
	}

}

void StochasticPanel::OnEditCorr(wxCommandEvent &)
{
	int idx = m_corrList->GetSelection();
	if (idx < 0) return;

	wxArrayString parts = wxStringTokenize(m_sd.Correlations[idx], ":");
	if (parts.Count() < 3) return;

	wxString result = wxGetTextFromUser("Edit correlation value (-1.0 < corr < 1.0):", "Edit", parts[2]);
	if (!result.IsEmpty())
	{
		double corr = atof( result.c_str() );
		if (corr <= -1) corr = -0.999;
		if (corr >= 1) corr = 0.999;
		wxString var_name0 = GetVarNameFromInputDistribution(parts[0]);
		wxString var_name1 = GetVarNameFromInputDistribution(parts[1]);
		m_sd.Correlations[idx] = var_name0 + ":" + var_name1 + ":" + wxString::Format("%lg", corr);
		UpdateFromSimInfo();
		m_regenerate_samples = true;

	}
}

void StochasticPanel::OnRemoveCorr(wxCommandEvent &)
{
	int idx = m_corrList->GetSelection();
	if (idx < 0)
		wxMessageBox("No correlation selected.", "Stochastic Simulation Message");
	else
		m_sd.Correlations.RemoveAt(idx);

	UpdateFromSimInfo();
	m_regenerate_samples = true;


	if (m_corrList->GetCount() > 0)
		m_corrList->Select(idx-1>=0?idx-1:idx);
}

void StochasticPanel::ComputeSamples()
{
    if (m_chk_weather_files->GetValue())
        UpdateWeatherFileCDF();

    wxArrayString errors;
    //	matrix_t<double> table;

    if (!ComputeLHSInputVectors(m_sd, m_input_data, &errors))
    {
        wxShowTextMessageDialog("An error occured while computing the samples using LHS:\n\n" + wxJoin(errors, '\n'));
        return;
    }

    wxArrayString collabels;
    for (size_t i = 0; i < m_sd.InputDistributions.Count(); i++)
    {
        wxString item = GetVarNameFromInputDistribution(m_sd.InputDistributions[i]);
        wxString label = GetLabelFromVarName(item);
        collabels.Add(label);
    }

    wxDialog* dlg = new wxDialog(this, wxID_ANY, "Stochastic Input Vectors", wxDefaultPosition, wxScaleSize(400, 600), wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);
    wxExtGridCtrl* grid = new wxExtGridCtrl(dlg, wxID_ANY);
    grid->EnableCopyPaste(true);
    grid->CreateGrid(m_input_data.nrows(), m_input_data.ncols());
    grid->Freeze();
    // for string value variables - show string values (e.g. lists - array type, weather files,...)
    for (size_t j = 0; j < m_input_data.ncols(); j++)
    {
        wxString var = m_sd.InputDistributions[j];
        wxArrayString parts = wxStringTokenize(var, ":");
        if (parts.Count() < 2) continue;
        int dist_type = wxAtoi(parts[1]);
        if ((parts.Count() < 6) && (dist_type != LHS_USERCDF)) continue;
        if (dist_type == LHS_USERCDF)
        {
            wxString item = GetVarNameFromInputDistribution(parts[0]);
            wxArrayString values;
            if (item == m_weather_folder_varname)
            {
                //values = m_weather_files;
                for (size_t i = 0; i < m_input_data.nrows(); i++)
                {
                    wxString wf;
                    if (GetWeatherFileForSum(m_input_data(i, j), &wf))
                        grid->SetCellValue(i, j, wf 
                            + wxString::Format(" (%lg)", m_input_data(i, j)));
                }
            }
			else {
				VarInfo* vi = m_case->GetConfiguration()->Variables.Lookup(item);
				if (!vi) continue;
				values = vi->IndexLabels;
				if (values.Count() > 0)
				{
					for (size_t i = 0; i < m_input_data.nrows(); i++)
					{
						int ndx = (int)m_input_data(i, j);
						if ((ndx >= 0) && (ndx < (int)values.Count()))
							grid->SetCellValue(i, j, values[ndx]);
					}
				}
				else {
					for (size_t i = 0; i < m_input_data.nrows(); i++)
						grid->SetCellValue(i, j, wxString::Format("%lg", m_input_data(i, j)));
				}
			}
        }
        else
        {
            for (size_t i = 0; i < m_input_data.nrows(); i++)
                grid->SetCellValue(i, j, wxString::Format("%lg", m_input_data(i, j)));
        }
    }


    for (size_t i = 0; i < m_input_data.ncols(); i++)
        grid->SetColLabelValue(i, collabels[i]);
    grid->AutoSize();
    grid->Thaw();

    m_regenerate_samples = false;
    dlg->Show();

}

void StochasticPanel::OnComputeSamples(wxCommandEvent &)
{
    ComputeSamples();
}

void StochasticPanel::OnSimulate( wxCommandEvent & )
{
    // do not show or regenerate computed samples if the user generated them immediately before simulating
    if ( m_regenerate_samples )
        ComputeSamples();
	Simulate();
}

void StochasticPanel::Simulate()
{
	wxBusyCursor _busy;

	wxArrayString errors;
//	matrix_t<double> input_data;

	if (m_chk_weather_files->GetValue())
		UpdateWeatherFileCDF();

	if (m_regenerate_samples)
	{
		if (!ComputeLHSInputVectors(m_sd, m_input_data, &errors))
		{
			wxShowTextMessageDialog("An error occured while computing the samples using LHS:\n\n" + wxJoin(errors, '\n'));
			return;
		}
	}

	if (m_sd.Outputs.size() == 0)
	{
		wxMessageBox("Please choose one or more output variables.", "Stochastic Simulation Message");
		return;
	}

	wxArrayString output_vars( m_sd.Outputs ), output_labels, output_units, ov, ol, ou;
	Simulation::ListAllOutputs( m_case->GetConfiguration(), &ov, &ol, &ou, NULL, NULL, true );
	for( size_t i=0;i<output_vars.size();i++ )
	{
		int idx = ov.Index( output_vars[i] );
		if ( idx >= 0 )
		{
			output_labels.Add( ol[idx] );
			output_units.Add( ou[idx] );
		}
		else
		{
			output_labels.Add( "{" + output_vars[i] + "}" );
			output_units.Add( wxEmptyString );
		}	
	}



	// all single value output data for each run
	matrix_t<double> output_data;
	output_data.resize_fill(m_sd.N, output_vars.size(), 0.0);

	wxStopWatch sw;

	int nthread = wxThread::GetCPUCount();

	SimulationDialog tpd( "Preparing simulations...", nthread );

	for (size_t i = 0; i < m_sims.size(); i++)
		delete m_sims[i];
	
	m_sims.clear();

	int count_sims = 1;

	for (int i = 0; i < m_sd.N; i++)
	{
		Simulation *s = new Simulation(m_case, wxString::Format("Stochastic #%d", (int)(i + 1)));
		m_sims.push_back(s);

		for (size_t j = 0; j < m_sd.InputDistributions.size(); j++)
		{
			wxString iname(GetVarNameFromInputDistribution(m_sd.InputDistributions[j]));

			if (iname == m_weather_folder_varname)
			{
				wxString weather_file;
				if (!GetWeatherFileForSum(m_input_data(i, j), &weather_file))
					continue;
				weather_file = m_folder->GetValue() + "/" + weather_file;
				s->Override("use_specific_weather_file", VarValue(true));
				s->Override("user_specified_weather_file", VarValue(weather_file));
				s->Override("use_specific_wf_wind", VarValue(true));
				s->Override("user_specified_wf_wind", VarValue(weather_file));
			}
			else if (m_case->Values().Get(iname)->Length() == 1)
			{
				double val[1];
				val[0] = (double)m_input_data(i, j);
				s->Override(iname, VarValue(val,1));
			}
			else
				s->Override(iname, VarValue((double)m_input_data(i, j)));
		}

		if (!s->Prepare())
			wxMessageBox(wxString::Format("Internal error preparing simulation %d for stochastic simulation.", (int)(i + 1)), "Stochastic Simulation Message");

		tpd.Update(0, (float)i / (float)m_sd.N * 100.0f, wxString::Format("%d of %d", (int)(i + 1), (int)m_sd.N));

		if (tpd.Canceled())
		{
			return;
		}
		count_sims++;
	}

	sw.Time();
	sw.Start();
	

	if ( nthread > (int)m_sims.size() ) nthread = m_sims.size();
	tpd.NewStage("Calculating...", nthread);
	
	size_t nok = 0;
	if ( m_useThreads->GetValue() )
	{
		nok = Simulation::DispatchThreads( tpd, m_sims, nthread );
	}
	else
	{
		for( size_t i=0;i<m_sims.size();i++ )
		{	
			if( m_sims[i]->Invoke( true, false ) )
				nok++;
			
			tpd.Update( 0, (float)i / (float)m_sd.N * 100.0f );
		}
	}

	sw.Time();
	sw.Start();

	for( int i=0;i<m_sd.N;i++ )
	{
		if ( m_sims[i]->Ok() )
		{
			for( size_t j=0;j<output_vars.size();j++ )
				if ( VarValue *vv = m_sims[i]->GetOutput( output_vars[j] ) )
					output_data(i,j) = vv->Value();
		}
		else
		{
		//	outlog->AppendText( wxJoin( sims[i]->GetErrors(), '\n') );
		}

//		delete sims[i];
	}
//	sims.clear();

	sw.Time();
	
	//tpd->Log( wxString::Format("Prep %d, Sim %d, Outputs %d (ms)", time_prep, time_sim, time_outputs ) );

	// compute stepwise regression
	
	tpd.NewStage( "Regressing outputs...", 1 );	
	Stepwise stw;
	std::vector<double> data;
	data.resize( m_sd.N );
	for( size_t i=0;i<m_sd.InputDistributions.size();i++ )
	{
		for( int j=0;j<m_sd.N;j++ )
			data[j] = m_input_data(j,i);
		stw.SetInputVector( wxString("input_")+((char)('a'+i)), data );
	}

	m_regressions.clear();
	m_regressions.resize_fill( output_vars.size(), m_sd.InputDistributions.size(), stepresult() );

	for( size_t i=0;i<output_vars.size();i++ )
	{
		for( int j=0;j<m_sd.N;j++ )
			data[j] = output_data(j,i);

		stw.SetOutputVector( data );
		
		tpd.Update( 0, (float)i / (float)output_vars.size() * 100.0f );

		if ( stw.Exec() )
		{
			for( size_t j=0;j<m_sd.InputDistributions.size();j++ )
				stw.GetStatistics( wxString("input_")+((char)('a'+j)), NULL, 
					&m_regressions(i,j).deltar2, 
					&m_regressions(i,j).beta );
		}
		else
			tpd.Log( "Error running stepwise regression for '" + output_labels[i] + "': " + stw.ErrorMessage());
	}

	// update results
	m_dataGrid->Freeze();
	m_dataGrid->ResizeGrid( m_sd.N, output_vars.size() );
	for( size_t j=0;j<output_vars.size();j++ )
	{
		wxString L( output_labels[j] );
		if ( !output_units[j].IsEmpty() )
			L += "\n(" + output_units[j] + ")";

		m_dataGrid->SetColLabelValue( j, L );

		for( int i=0;i<m_sd.N;i++ )
			m_dataGrid->SetCellValue( i, j, wxString::Format("%lg", output_data(i,j) ) );
	}
	
	m_dataGrid->Thaw();
	m_dataGrid->SetRowLabelSize( wxGRID_AUTOSIZE );
	m_dataGrid->SetColLabelSize( wxGRID_AUTOSIZE );
	m_dataGrid->AutoSize();
	m_dataGrid->Layout();


	m_statGrid->Freeze();
	m_statGrid->ResizeGrid( 2*m_sd.InputDistributions.size(), output_vars.size() );

	for( size_t i=0;i<output_vars.size();i++ )
	{
		wxString L( output_labels[i] );
		if ( !output_units[i].IsEmpty() )
			L += "\n(" + output_units[i] + ")";
		m_statGrid->SetColLabelValue( i, L );
	}

	for( size_t i=0;i<m_sd.InputDistributions.size();i++ )
	{
		wxString var( GetVarNameFromInputDistribution(m_sd.InputDistributions[i]));
		wxString L, u;
		if (var == m_weather_folder_varname)
		{
			//L = m_weather_folder_displayname;
            L = wxString::Format("Weather Files (%s)", m_cbo_weather_files->GetValue());

			u = "";
		}
		else
		{
			L= m_case->GetConfiguration()->Variables.Label(var);
			u = m_case->GetConfiguration()->Variables.Units(var);
		}
		if ( !u.IsEmpty() )
			L += " (" + u + ")";

		m_statGrid->SetRowLabelValue( i, "Delta R^2: " + L);
		m_statGrid->SetRowLabelValue( m_sd.InputDistributions.size()+i, "Beta: " + L );
	}

	for( size_t i=0;i<m_regressions.ncols(); i++ )
	{
		for( size_t j=0;j<output_vars.size(); j++ )
		{
			m_statGrid->SetCellValue( i, j,
				wxString::Format("%lg", m_regressions(j,i).deltar2) );

			m_statGrid->SetCellValue( m_sd.InputDistributions.size()+i, j,
				wxString::Format("%lg", m_regressions(j,i).beta) );
		}
	}
	
	m_statGrid->Thaw();
	m_statGrid->SetRowLabelAlignment( wxLEFT, wxCENTER );
	m_statGrid->SetRowLabelSize( wxGRID_AUTOSIZE );
	m_statGrid->SetColLabelSize( wxGRID_AUTOSIZE );
	m_statGrid->AutoSize();
	m_statGrid->Layout();

	Layout();
	wxYield();
	
	if ( (int)nok != m_sd.N )
		tpd.Log("Not all simulations completed successfully.");

	tpd.Finalize();
}


wxString GetVarNameFromInputDistribution(const wxString &input_distribution)
{
	// return varname for use in lists adding inputs, etc
	wxString var_name = input_distribution.BeforeFirst(':');
	var_name = var_name.BeforeFirst('='); // weather files
	return var_name;
}


bool ComputeLHSInputVectors( StochasticData &sd, matrix_t<double> &table, wxArrayString *errors)
{
	int i, n, j;

	if (sd.N < 1)
	{
		if (errors) errors->Add("Number of runs requested must be greater than 1.");
		return false;
	}

	table.resize_fill(sd.N, sd.InputDistributions.Count(), 0.0);

	// compute the input vectors with Sandia LHS
	LHS lhs;
	for (i=0;i<(int)sd.InputDistributions.Count();i++)
	{
		wxArrayString distinfo = wxStringTokenize(sd.InputDistributions[i],":");
		if (distinfo.Count() < 2) continue;
		int dist_type = wxAtoi(distinfo[1]);
		if ((distinfo.Count() < 6) && (dist_type != LHS_USERCDF)) continue;
		std::vector<double> params;
		if (dist_type == LHS_USERCDF)
		{
			if (distinfo.Count() < 3) continue;
			int N = wxAtoi(distinfo[2]);
			if ((int)distinfo.Count() != (3 + 2 * N)) continue;
			int list_item_num = 0;
			for (size_t j = 2; j < distinfo.Count(); j++) {
				double val;
				if (distinfo[j].ToDouble(&val))
					params.push_back(val);
				else {
					params.push_back(list_item_num); // index value of list items
					list_item_num++;
				}
			}
		}
		else
		{
			params.push_back(wxAtof(distinfo[2]));
			params.push_back(wxAtof(distinfo[3]));
			params.push_back(wxAtof(distinfo[4]));
			params.push_back(wxAtof(distinfo[5]));
		}
		lhs.Distribution(dist_type, wxString((char)('a' + i)), params);
	}

	for (i=0;i<(int)sd.Correlations.Count();i++)
	{
		wxArrayString corrinfo = wxStringTokenize(sd.Correlations[i],":");
		if (corrinfo.Count() < 3) continue;
		int name_idx1 = -1;
		int name_idx2 = -1;

		for (j=0;j<(int)sd.InputDistributions.Count();j++)
		{
			wxString curname = GetVarNameFromInputDistribution(sd.InputDistributions[j]);
			if (curname == corrinfo[0]) name_idx1 = j;
			if (curname == corrinfo[1]) name_idx2 = j;
		}

		if (name_idx1 < 0 || name_idx2 < 0) continue;

		lhs.Correlate( wxString((char)('a'+name_idx1)), wxString((char)('a'+name_idx2)), wxAtof(corrinfo[2]) );
	}

	lhs.Points( sd.N );
	lhs.SeedVal( sd.Seed );

	if (!lhs.Exec())
	{
		//wxMessageBox("Could not run Sandia Latin Hypercube Sampling (LHS) program.\n\n" + lhs.ErrorMessage());
		if(errors) errors->Add(lhs.ErrorMessage());
		return false;
	}

	// retrieve all input vectors;
	for (i=0;i<(int)sd.InputDistributions.Count();i++)
	{
		std::vector<double> values;
		lhs.Retrieve( wxString((char)('a'+i)), values);
		if (values.size() != (size_t)sd.N)
		{
			if(errors) errors->Add(wxString::Format("Incorrect number of LHS values (%d) retrieved for input vector %d\n", (int)values.size(), i));
			return false;
		}

		// copy to input vector matrix
		for (n=0;n<sd.N;n++)
			table.at(n,i) = values[n];
	}

	return true;
}

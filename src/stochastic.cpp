#include <wx/filefn.h>
#include <wx/stopwatch.h>
#include <wx/tokenzr.h>
#include <wx/utils.h>
#include <wx/filename.h>

#include <wex/utils.h>

#include "main.h"

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

#ifdef __WXMSW__
#define LHSBINARY "lhs.exe"
#else
#define LHSBINARY "lhs.bin"
#endif

bool LHS::Exec()
{
	wxString workdir( wxFileName::GetTempDir() );
	
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
	int ninputs = 0;
	for (size_t i=0;i<m_inputs.size();i++)
	{
		if (datalen < 0) datalen = m_inputs[i].vec.size();

		if (m_inputs[i].vec.size() != datalen)
		{
			m_err = "Inconsistent input data vector lengths.";
			return false;
		}
	}

	if (m_output_vec.size() != datalen)
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
	for (size_t i=0;i<datalen;i++)
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

	for(size_t i=0;i<datalen;i++)
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
	wxRemoveFile( workdir + "/result.txt" );
	wxRemoveFile( workdir + "/stp_test_usr.inp" );
	wxRemoveFile( workdir + "/stp_test_ind.dat" );
	wxRemoveFile( workdir + "/stp_test_dep.dat" );
	wxRemoveFile( workdir + "/stp_test_out.out" );


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
		if (nlines++ > m_inputs.size())
			break;

		fgets( cbuf, 2047, fp );

		wxArrayString parts = wxStringTokenize( cbuf, " \t:", wxTOKEN_STRTOK);
		if (parts.Count() != 4)
			continue;

		bool assigned = false;
		for (size_t i=0;i<m_inputs.size();i++)
		{
			if (m_inputs[i].name.Lower() == parts[0].Lower())
			{
				m_inputs[i].R2 = atof( parts[1].c_str() );
				m_inputs[i].R2inc = atof( parts[2].c_str() );
				m_inputs[i].SRC = atof( parts[3].c_str() );
				m_inputs[i].calculated = true;
				assigned = true;
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
	for (int i=0;i<m_inputs.size();i++)
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
	wxUint8 ver = in.Read8();

	N = in.Read32();
	Seed = in.Read32();
	Outputs = wxStringTokenize( in.ReadString(), "|" );
	InputDistributions = wxStringTokenize( in.ReadString(), "|" );
	Correlations = wxStringTokenize( in.ReadString(), "|" );

	return in.Read8() == code;
}



enum { ID_cboDistribution = wxID_HIGHEST+394 };

class InputDistDialog : public wxDialog
{
public:
	wxChoice *cboDistribution;
	wxStaticText *lblVarName;
	wxStaticText *lblVarValue;
	wxStaticText *lbls[4];
	wxNumericCtrl *nums[4];

	InputDistDialog(wxWindow *parent, const wxString &title)
		: wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxSize(450,350), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
	{
		cboDistribution = new wxChoice(this, ID_cboDistribution);
				
		for (int i=0;i<LHS_NUMDISTS && i < LHS_USERCDF;i++)
			cboDistribution->Append( wxString(::lhs_dist_names[i]).BeforeFirst(',') );

		cboDistribution->Select(LHS_NORMAL);

		lblVarName = new wxStaticText(this, wxID_ANY, "VarName");
		lblVarValue = new wxStaticText(this, wxID_ANY, "VarValue");

		wxFlexGridSizer *grid = new wxFlexGridSizer(2);
		grid->Add( new wxStaticText( this, wxID_ANY, "Variable name:" ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
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

		wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
		sizer->Add( cboDistribution, 0, wxALL|wxEXPAND, 5 );
		sizer->Add( grid, 1, wxALL|wxEXPAND, 0 );
		sizer->Add( CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10 );
		SetSizer( sizer );

		
		lbls[2]->Hide(); nums[2]->Hide();
		lbls[3]->Hide(); nums[3]->Hide();
	}
	
	void Setup(const wxString &name, const wxString &value,
			int DistType, double p0, double p1, double p2, double p3)
	{
		lblVarName->SetLabel(name);
		lblVarValue->SetLabel(value);
		cboDistribution->SetSelection(DistType);
		nums[0]->SetValue(p0);
		nums[1]->SetValue(p1);
		nums[2]->SetValue(p2);
		nums[3]->SetValue(p3);
		UpdateLabels();
	}

	void UpdateLabels()
	{
		wxArrayString parts = wxStringTokenize(::lhs_dist_names[ cboDistribution->GetSelection() ], ",");
	
		int i;
		for (i=0;i<4;i++)
		{
			lbls[i]->Hide();
			nums[i]->Hide();
		}

		for (i=1;i<(int)parts.Count();i++)
		{
			lbls[i-1]->SetLabel( parts[i] + ":");
			lbls[i-1]->Show();
			nums[i-1]->Show();
		}

		Layout();
		Refresh();
	}

	void OnDistChange(wxCommandEvent &evt)
	{
		UpdateLabels();
	}

	DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE( InputDistDialog, wxDialog )
	EVT_CHOICE( ID_cboDistribution, InputDistDialog::OnDistChange )
END_EVENT_TABLE()



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
  ID_Simulate
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
END_EVENT_TABLE()

StochasticPanel::StochasticPanel(wxWindow *parent, Case *cc)
	 : wxPanel( parent ), m_case( cc ), m_sd( m_case->Stochastic() )
{
	wxBoxSizer *sizer_main = new wxBoxSizer( wxVERTICAL );

	wxStaticBoxSizer *szbox = new wxStaticBoxSizer(wxVERTICAL, this, "Configure Stochastic Simulation");

	wxBoxSizer *sizer_inputs = new wxBoxSizer( wxHORIZONTAL );	
	sizer_inputs->Add( new wxStaticText( szbox->GetStaticBox(), wxID_ANY, "Input distributions:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	sizer_inputs->Add( new wxButton(szbox->GetStaticBox(), ID_btnAddInput, "Add...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sizer_inputs->Add( new wxButton(szbox->GetStaticBox(), ID_btnEditInput, "Edit...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sizer_inputs->Add( new wxButton(szbox->GetStaticBox(), ID_btnRemoveInput, "Remove", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	
	wxBoxSizer *sizer_inputs_v = new wxBoxSizer( wxVERTICAL );
	sizer_inputs_v->Add( sizer_inputs, 0, wxALL|wxEXPAND, 3 );
	m_inputList = new wxListBox(szbox->GetStaticBox(), ID_m_inputList);
	m_inputList->SetInitialSize( wxSize( 325, 100 ) );
	sizer_inputs_v->Add( m_inputList, 0, wxALL|wxEXPAND, 5 );


	wxBoxSizer *sizer_corr = new wxBoxSizer( wxHORIZONTAL );
	sizer_corr->Add( new wxStaticText(szbox->GetStaticBox(), wxID_ANY, "Select correlations:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	sizer_corr->Add( new wxButton(szbox->GetStaticBox(), ID_btnAddCorr, "Add...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sizer_corr->Add( new wxButton(szbox->GetStaticBox(), ID_btnEditCorr, "Edit...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sizer_corr->Add( new wxButton(szbox->GetStaticBox(), ID_btnRemoveCorr, "Remove", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );	
	
	wxBoxSizer *sizer_corr_v = new wxBoxSizer( wxVERTICAL );
	sizer_corr_v->Add( sizer_corr, 0, wxALL|wxEXPAND, 3 );
	m_corrList = new wxListBox(szbox->GetStaticBox(), ID_m_corrList);
	m_corrList->SetInitialSize( wxSize( 325, 100 ) );	
	sizer_corr_v->Add( m_corrList, 0, wxALL|wxEXPAND, 5 );

	wxBoxSizer *sizer_horiz = new wxBoxSizer( wxHORIZONTAL );
	sizer_horiz->Add( sizer_inputs_v, 0, wxALL|wxEXPAND, 5 );
	sizer_horiz->Add( sizer_corr_v, 0, wxALL|wxEXPAND, 5 );
	szbox->Add( sizer_horiz );

	m_N = new wxNumericCtrl(szbox->GetStaticBox(), ID_m_N, 100, wxNumericCtrl::INTEGER);
	m_seed = new wxNumericCtrl(szbox->GetStaticBox(), ID_m_seed, -1, wxNumericCtrl::INTEGER);
	
	wxBoxSizer *sizer_ctrls = new wxBoxSizer( wxHORIZONTAL );
	sizer_ctrls->Add( new wxStaticText(szbox->GetStaticBox(), wxID_ANY, "Number of samples:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	sizer_ctrls->Add( m_N );
	sizer_ctrls->Add( new wxStaticText(szbox->GetStaticBox(), wxID_ANY, "Seed value (0 for random):"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	sizer_ctrls->Add( m_seed );

	szbox->Add( sizer_ctrls, 0, wxALL|wxEXPAND, 5 );

	sizer_main->Add( szbox, 0, wxALL, 5 );
	
	m_outputList = new wxListBox( this, wxID_ANY );
	sizer_main->Add( m_outputList );

	
	
	wxBoxSizer *sizer_cmd = new wxBoxSizer( wxHORIZONTAL );
	sizer_cmd->Add( new wxButton(this, ID_btnAddOutput, "Add output..."), 0, wxALL|wxEXPAND, 5 );
	sizer_cmd->Add( new wxButton(this, ID_btnRemoveOutput, "Remove output"), 0, wxALL|wxEXPAND, 5 );
	sizer_cmd->Add( new wxButton(this, ID_btnComputeSamples, "Compute samples..."), 0, wxALL|wxEXPAND, 5 );
	sizer_cmd->Add( new wxButton(this, ID_Simulate, "Run simulation"), 0, wxALL|wxEXPAND, 5 );

	sizer_main->Add( sizer_cmd );
	
	m_grid = new wxExtGridCtrl( this, wxID_ANY );
	m_grid->CreateGrid( 1, 1 );
	m_grid->SetCellValue(0, 0, "No results.");

	sizer_main->Add( m_grid, 1, wxALL|wxEXPAND, 0 );

	SetSizer( sizer_main );
	

	UpdateFromSimInfo();	
}

void StochasticPanel::UpdateFromSimInfo()
{	
	m_N->SetValue( m_sd.N );
	m_seed->SetValue( m_sd.Seed );

	int i;

	m_outputList->Freeze();
	m_outputList->Clear();
	wxArrayString vars, labels;
	Simulation::ListAllOutputs( m_case->GetConfiguration(), &vars, &labels, NULL, true );

	for (int i=0;i<m_sd.Outputs.Count();i++)
	{
		int idx = vars.Index( m_sd.Outputs[i] );
		if (idx >= 0)
			m_outputList->Append( labels[idx] ); 
		else
			m_outputList->Append("<Error - remove this>");
	}

	m_outputList->Thaw();

	m_inputList->Freeze();
	m_inputList->Clear();
	for (i=0;i<(int)m_sd.InputDistributions.Count();i++)
	{
		wxArrayString parts = wxStringTokenize(m_sd.InputDistributions[i], ":");
		if ( parts.Count() == 0 )
			continue;

		wxString item = parts[0];
		item = m_case->GetConfiguration()->Variables.Label( item );
		
		if (parts.Count() == 6)
		{
			int disttype  = atoi(parts[1].c_str());
			if (disttype < 0) disttype = 0;
			if (disttype >= LHS_NUMDISTS) disttype = LHS_NUMDISTS-1;

			wxArrayString distparts = wxStringTokenize(::lhs_dist_names[disttype], ",");
			if (distparts.Count() > 1)
			{
				item += " ( " + distparts[0] + " [";
				for (int j=1;j<distparts.Count();j++)
				{
					item += parts[1+j];
					if (j < distparts.Count()-1) item += ",";
				}
				item += "] )";
			}
		}

		m_inputList->Append( item );
	}

	m_inputList->Thaw();

	m_corrList->Freeze();
	m_corrList->Clear();
	for (i=0;i<(int)m_sd.Correlations.Count();i++)
	{
		wxArrayString parts = wxStringTokenize( m_sd.Correlations[i], ":" );
		if (parts.Count() < 3) continue;
		
		wxString l1 = m_case->GetConfiguration()->Variables.Label( parts[0] );
		wxString l2 = m_case->GetConfiguration()->Variables.Label( parts[1] );
		
		if ( l1.IsEmpty() || l2.IsEmpty() ) continue;

		m_corrList->Append( l1 + ", " + l2 + ", " + parts[2] );
	}

	m_corrList->Thaw();
}


void StochasticPanel::OnNChange(wxCommandEvent &evt)
{
	m_sd.N = m_N->AsInteger();
}

void StochasticPanel::OnSeedChange(wxCommandEvent &evt)
{
	m_sd.Seed = m_seed->AsInteger();
}

void StochasticPanel::OnAddInput(wxCommandEvent &evt)
{
	wxArrayString varlist;
	int i;
	for (i=0;i<(int)m_sd.InputDistributions.Count();i++)
		varlist.Add( m_sd.InputDistributions[i].BeforeFirst(':') );
	
	wxArrayString names, labels;
	ConfigInfo *ci = m_case->GetConfiguration();
	VarInfoLookup &vil = ci->Variables;

	for (VarInfoLookup::iterator it = vil.begin(); it != vil.end(); ++it)
	{
		wxString name = it->first;
		VarInfo &vi = *(it->second);

		// skip calculated and indicator values
		//if (vi.Flags & VF_CALCULATED || vi.Flags & VF_INDICATOR) continue;
		// update to select only "Parametric" variables
		if ( vi.Flags & VF_PARAMETRIC && vi.Type == VV_NUMBER )
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
	SelectVariableDialog dlg(this, "Select Numeric Inputs");
	dlg.SetItems(names, labels);
	dlg.SetCheckedNames( varlist );
	if (dlg.ShowModal() == wxID_OK)
	{
		varlist = dlg.GetCheckedNames();

		i=0;
		// remove any input variables in StochasticData that are no longer in list
		while (i<(int)m_sd.InputDistributions.Count())
		{
			if ( varlist.Index( m_sd.InputDistributions[i].BeforeFirst(':') ) < 0 )
				m_sd.InputDistributions.RemoveAt(i);// remove, do not increment i
			else
				i++;
		}

		// add any inputs not already in StatSimList
		for (i=0;i<(int)varlist.Count();i++)
		{
			bool found = false;
			for (int j=0;j<(int)m_sd.InputDistributions.Count();j++)
				if ( m_sd.InputDistributions[j].BeforeFirst(':') == varlist[i] )
					found = true;

			if (!found)
			{
				VarValue *vv = m_case->Values().Get( varlist[i] );
				if (!vv)
					continue;

				// default to normal distribution
				m_sd.InputDistributions.Add( 
					varlist[i] + wxString::Format(":1:%lg:%lg:0:0", 
					(double)vv->Value(), (double)0.15*vv->Value() ));
			}
		}

		UpdateFromSimInfo();
	}

}

void StochasticPanel::OnEditInput(wxCommandEvent &evt)
{
	int idx = m_inputList->GetSelection();
	if ( idx < 0 ) return;

	wxString var = m_sd.InputDistributions[idx];
	wxArrayString parts = wxStringTokenize(var, ":");
	if (parts.Count() < 6) return;

	ConfigInfo *ci = m_case->GetConfiguration();
	VarValue *vptr = m_case->Values().Get(parts[0]);
	if (!vptr) return;

	InputDistDialog dlg(this, "Edit " + ci->Variables.Label( parts[0] ) + " Distribution");

	dlg.Setup( ci->Variables.Label( parts[0] ), wxString::Format("%g", vptr->Value()),
		wxAtoi(parts[1]), wxAtof(parts[2]), wxAtof(parts[3]), wxAtof(parts[4]), wxAtof(parts[5]) );

	if (dlg.ShowModal()==wxID_OK)
	{
		m_sd.InputDistributions[idx] = parts[0] + ":"
			+ wxString::Format("%d",  dlg.cboDistribution->GetSelection() ) + ":"
			+ wxString::Format("%lg", dlg.nums[0]->Value() ) + ":"
			+ wxString::Format("%lg", dlg.nums[1]->Value() ) + ":"
			+ wxString::Format("%lg", dlg.nums[2]->Value() ) + ":"
			+ wxString::Format("%lg", dlg.nums[3]->Value() );

		UpdateFromSimInfo();
	}
}

void StochasticPanel::OnRemoveInput(wxCommandEvent &evt)
{
	int idx = m_inputList->GetSelection();
	if (idx < 0)
		wxMessageBox("No input variable selected.");
	else
		m_sd.InputDistributions.RemoveAt(idx);

	UpdateFromSimInfo();

	if (m_inputList->GetCount() > 0)
		m_inputList->Select(idx-1>=0?idx-1:idx);
}

void StochasticPanel::OnAddOutput(wxCommandEvent &evt)
{
	wxArrayString list = m_sd.Outputs;
	
	wxDialog dlg( this, wxID_ANY, "Choose output metrics", wxDefaultPosition, wxSize(300,450), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER );
	wxCheckListBox *ckl = new wxCheckListBox( &dlg, wxID_ANY );
	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	sizer->Add( ckl, 1, wxALL|wxEXPAND, 5 );
	sizer->Add( dlg.CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL, 10 );
	dlg.SetSizer(sizer);

	wxArrayString vars, labels;
	Simulation::ListAllOutputs( m_case->GetConfiguration(), &vars, &labels, NULL, true );

	ckl->Freeze();
	ckl->Clear();
	ckl->Append( labels );

	for (int i=0;i<(int)vars.Count();i++)
		ckl->Check(i, (m_sd.Outputs.Index( vars[i] ) >= 0) );

	ckl->Thaw();

	int result = dlg.ShowModal();
	if (result == wxID_OK)
	{
		m_sd.Outputs.Clear();
		for( size_t i=0;i<ckl->GetCount();i++ )
			if ( ckl->IsChecked( i ) )
				m_sd.Outputs.Add( vars[i] );
			
		UpdateFromSimInfo();
	}
}

void StochasticPanel::OnRemoveOutput(wxCommandEvent &evt)
{
	int idx = m_outputList->GetSelection();
	if (idx < 0)
		wxMessageBox("No output metric selected.");
	else
		m_sd.Outputs.RemoveAt(idx);

	UpdateFromSimInfo();

	if (m_outputList->GetCount() > 0)
		m_outputList->Select(idx-1>=0?idx-1:idx);
}

void StochasticPanel::OnAddCorr(wxCommandEvent &evt)
{
	wxArrayString names;
	wxArrayString labels;

	for (int i=0;i<m_sd.InputDistributions.Count();i++)
	{
		names.Add( m_sd.InputDistributions[i].BeforeFirst(':'));

		VarInfo *v = m_case->GetConfiguration()->Variables.Lookup( names[i] );
		if (!v)
		{
			labels.Add("<<Label Lookup Error>>");
			continue;
		}
		if ( !v->Group.IsEmpty() )
			labels.Add( v->Group + "/" + v->Label );
		else
			labels.Add( v->Label );
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
			wxMessageBox("You must pick exactly 2 input variables to correlate.");
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
		}
	}

}

void StochasticPanel::OnEditCorr(wxCommandEvent &evt)
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
		m_sd.Correlations[idx] = parts[0] + ":" + parts[1] + ":" + wxString::Format("%lg", corr );
		UpdateFromSimInfo();
	}
}

void StochasticPanel::OnRemoveCorr(wxCommandEvent &evt)
{
	int idx = m_corrList->GetSelection();
	if (idx < 0)
		wxMessageBox("No correlation selected.");
	else
		m_sd.Correlations.RemoveAt(idx);

	UpdateFromSimInfo();

	if (m_corrList->GetCount() > 0)
		m_corrList->Select(idx-1>=0?idx-1:idx);
}


void StochasticPanel::OnComputeSamples(wxCommandEvent &evt)
{
	wxArrayString errors;
	matrix_t<double> table;
	if (!ComputeLHSInputVectors( m_sd, table, &errors))
	{
		wxShowTextMessageDialog("An error occured while computing the samples using LHS:\n\n" + wxJoin(errors,'\n'));
		return;
	}

	wxArrayString collabels;
	for (int i=0;i<m_sd.InputDistributions.Count();i++)
		collabels.Add( m_case->GetConfiguration()->Variables.Label( m_sd.InputDistributions[i].BeforeFirst(':') ) );

	
	wxDialog *dlg = new wxDialog( this, wxID_ANY, "Data Vectors", wxDefaultPosition, wxSize(400,600), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER);
	wxExtGridCtrl *grid = new wxExtGridCtrl( dlg, wxID_ANY );
	grid->EnableCopyPaste( true );
	grid->CreateGrid( table.nrows(), table.ncols() );
	grid->Freeze();
	for( size_t i=0;i<table.nrows();i++ )
		for( size_t j=0;j<table.ncols();j++ )
			grid->SetCellValue( i, j, wxString::Format("%lg", table(i,j)) );

	for( size_t i=0;i<table.ncols();i++ )
		grid->SetColLabelValue( i, collabels[i] );

	grid->Thaw();

	dlg->Show();
}

void StochasticPanel::OnSimulate( wxCommandEvent & )
{
	Simulate();
}

void StochasticPanel::Simulate()
{
	wxArrayString errors;
	matrix_t<double> input_data;
	if ( !ComputeLHSInputVectors( m_sd, input_data, &errors ) )
	{
		wxShowTextMessageDialog("An error occured while computing the samples using LHS:\n\n" + wxJoin(errors,'\n'));
		return;
	}

	if ( m_sd.Outputs.size() == 0 )
	{
		wxMessageBox("please select one or more output variables to analyze first.");
		return;
	}

	wxArrayString output_vars( m_sd.Outputs ), output_labels, output_units, ov, ol, ou;
	Simulation::ListAllOutputs( m_case->GetConfiguration(), &ov, &ol, &ou, true );
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
	output_data.resize_fill( m_sd.N, output_vars.size(), 0.0);

	wxDialog *dlg = new wxDialog( this, wxID_ANY, "Stochastic simulation", wxDefaultPosition, wxSize(500,300));
	wxTextCtrl *outlog = new wxTextCtrl( dlg, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE );
	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( outlog, 1, wxALL|wxEXPAND, 0 );
	sizer->Add( dlg->CreateButtonSizer( wxOK ), 0, wxALL|wxEXPAND, 5 );
	dlg->SetSizer( sizer );
	dlg->CenterOnParent();
	dlg->Show();
	wxSafeYield( 0, true );


	Simulation sim( m_case, "stochastic");
	size_t nok = 0;
	for( size_t i=0;i<m_sd.N;i++ )
	{
		sim.Clear();

		for( size_t j=0;j<m_sd.InputDistributions.size();j++ )
		{
			wxString iname( m_sd.InputDistributions[j].BeforeFirst(':') );
			sim.Override( iname, VarValue( (float)input_data(i,j) ) );
		}
		
		outlog->AppendText( wxString::Format( "Simulating %d of %d...\n", (int)(i+1), (int)m_sd.N )); 
		if ( sim.Invoke(true) )
		{
			nok++;
			for( size_t j=0;j<output_vars.size();j++ )
				if ( VarValue *vv = sim.GetOutput( output_vars[j] ) )
					output_data(i,j) = vv->Value();
		}
		else
		{
			outlog->AppendText( wxJoin( sim.GetErrors(), '\n') );
		}

		wxSafeYield( 0, true );
	}

	// compute stepwise regression
	Stepwise stw;
	std::vector<double> data;
	data.resize( m_sd.N );
	for( size_t i=0;i<m_sd.InputDistributions.size();i++ )
	{
		for( size_t j=0;j<m_sd.N;j++ )
			data[j] = input_data(j,i);
		stw.SetInputVector( wxString("input_")+((char)('a'+i)), data );
	}

	for( size_t i=0;i<output_vars.size();i++ )
	{
		for( size_t j=0;j<m_sd.N;j++ )
			data[j] = output_data(j,i);

		stw.SetOutputVector( data );
		if ( stw.Exec() )
		{
			double beta, dr2;

			for( size_t j=0;j<m_sd.InputDistributions.size();j++ )
			{
				double dr2, beta;
				if (stw.GetStatistics( wxString("input_")+((char)('a'+j)), NULL, &dr2, &beta))
					outlog->AppendText( "Regressed: " + output_labels[i] + " wrt " + m_sd.InputDistributions[j].BeforeFirst(':') 
						+ wxString::Format("  beta=%lg  deltar2=%lg\n", beta, dr2) );
				else
					outlog->AppendText( "No statistics for " + output_labels[i] + "\n" );
			}
		}
		else
			outlog->AppendText( "Error running stepwise regression: " + output_labels[i] + "\n");
	}
	

	// update results
	m_grid->Freeze();
	m_grid->ResizeGrid( m_sd.N, output_vars.size() );
	int row = 0;
	for( size_t j=0;j<output_vars.size();j++ )
	{
		wxString L( output_labels[j] );
		if ( !output_units[j].IsEmpty() )
			L += " (" + output_units[j] + ")";

		m_grid->SetColLabelValue( j, L );

		for( size_t i=0;i<m_sd.N;i++ )
			m_grid->SetCellValue( i, j, wxString::Format("%lg", output_data(i,j) ) );
	}

	m_grid->SetRowLabelSize(wxGRID_AUTOSIZE);
	m_grid->SetColLabelSize(wxGRID_AUTOSIZE);
	m_grid->GetParent()->Layout();
	m_grid->Layout();
	m_grid->Thaw();
	
	//if ( nok == m_sd.N )
		//dlg->Destroy();
	//else
		//wxMessageBox("Not all simulations succeeded");
}

bool ComputeLHSInputVectors( StochasticData &sd, matrix_t<double> &table, wxArrayString *errors)
{
	int i, n, j, r, c;

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
		if (distinfo.Count() < 6) continue;
		std::vector<double> params;
		params.push_back( wxAtof(distinfo[2]) );
		params.push_back( wxAtof(distinfo[3]) );
		params.push_back( wxAtof(distinfo[4]) );
		params.push_back( wxAtof(distinfo[5]) );
		lhs.Distribution( wxAtoi(distinfo[1]), wxString((char)('a' + i)), params);
	}

	for (i=0;i<(int)sd.Correlations.Count();i++)
	{
		wxArrayString corrinfo = wxStringTokenize(sd.Correlations[i],":");
		if (corrinfo.Count() < 3) continue;
		int name_idx1 = -1;
		int name_idx2 = -1;

		for (j=0;j<(int)sd.InputDistributions.Count();j++)
		{
			wxString curname = sd.InputDistributions[j].BeforeFirst(':');
			if (curname == corrinfo[0]) name_idx1 = j;
			if (curname == corrinfo[1]) name_idx2 = j;
		}

		if (name_idx1 < 0 || name_idx2 < 0) continue;

		lhs.Correlate( wxString((char)('a'+name_idx1)), wxString((char)('a'+name_idx2)), atof(corrinfo[2]) );
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
		if (values.size() != sd.N)
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

#ifndef __codegen_h
#define __codegen_h

#include <wx/string.h>
#include <wx/stream.h>
#include <wx/dialog.h>

#include <wex/tpdlg.h>

#include <lk/env.h>

#include <ssc/sscapi.h>


class Case;
class CaseWindow;
class VarTable;

struct CodeGenData {
	CodeGenData() :
		scale(1.0), mode('g'), thousep(false), deci(2)
	{}
	wxString var;
	wxString label;
	double scale;
	char mode;
	bool thousep;
	int deci;
	wxString pre, post;
};

class CodeGen_Base
{
public:
	CodeGen_Base( Case *cc, const wxString &folder );

	void Clear();
	Case *GetCase() { return m_case; }
	wxString GetErrors();

	// language specfic header and supporting functions
	virtual bool Header(FILE *fp)=0;

	virtual bool CreateSSCModule(FILE *fp, wxString &name) = 0;
	virtual bool RunSSCModule(FILE *fp, wxString &name) = 0;
	virtual bool FreeSSCModule(FILE *fp) = 0;

	virtual bool Input(FILE *fp, ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold) = 0;
	virtual bool Output(FILE *fp, ssc_data_t p_data) = 0;

	virtual bool Footer(FILE *fp) = 0;


	// same across languages
	bool GenerateCode(FILE *fp, const int &array_matrix_threshold);
	bool Prepare();
	bool Ok();
	void AddData(CodeGenData md);
	static bool ShowCodeGenDialog(CaseWindow *cw);

protected:
	Case *m_case;
	wxString m_folder;
	wxArrayString m_errors;
	VarTable m_inputs;
	std::vector<CodeGenData> m_data;
};

class CodeGen_lk : virtual public CodeGen_Base
{
public:
	CodeGen_lk(Case *cc, const wxString &folder);
	bool Header(FILE *fp);
	bool CreateSSCModule(FILE *fp, wxString &name);
	bool RunSSCModule(FILE *fp, wxString &name);
	bool FreeSSCModule(FILE *fp);
	bool Input(FILE *fp, ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold);
	bool Output(FILE *fp, ssc_data_t p_data);
	bool Footer(FILE *fp);
};

class CodeGen_c : virtual public CodeGen_Base
{
public:
	CodeGen_c(Case *cc, const wxString &folder);
	bool Header(FILE *fp);
	bool CreateSSCModule(FILE *fp, wxString &name);
	bool RunSSCModule(FILE *fp, wxString &name);
	bool FreeSSCModule(FILE *fp);
	bool Input(FILE *fp, ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold);
	bool Output(FILE *fp, ssc_data_t p_data);
	bool Footer(FILE *fp);
};

#endif

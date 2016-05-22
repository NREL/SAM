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
	CodeGen_Base( Case *cc, const wxString &fullpath );
	~CodeGen_Base();
	void Clear();
	Case *GetCase() { return m_case; }
	wxString GetErrors();

	// language specfic header and supporting functions
	virtual bool Header()=0;

	virtual bool CreateSSCModule(wxString &name) = 0;
	virtual bool RunSSCModule(wxString &name) = 0;
	virtual bool FreeSSCModule() = 0;

	virtual bool Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold) = 0;
	virtual bool Output(ssc_data_t p_data) = 0;

	virtual bool Footer() = 0;

	// same across languages
	bool GenerateCode(const int &array_matrix_threshold);
	bool Prepare();
	bool Ok();
	void AddData(CodeGenData md);
	static bool ShowCodeGenDialog(CaseWindow *cw);

protected:
	Case *m_case;
	FILE *m_fp;
	wxString m_folder;
	wxString m_name;
	wxArrayString m_errors;
	VarTable m_inputs;
	std::vector<CodeGenData> m_data;
};

class CodeGen_lk : virtual public CodeGen_Base
{
public:
	CodeGen_lk(Case *cc, const wxString &folder);
	bool Header();
	bool CreateSSCModule(wxString &name);
	bool RunSSCModule(wxString &name);
	bool FreeSSCModule();
	bool Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold);
	bool Output(ssc_data_t p_data);
	bool Footer();
};

class CodeGen_c : virtual public CodeGen_Base
{
public:
	CodeGen_c(Case *cc, const wxString &folder);
	bool Header();
	bool CreateSSCModule(wxString &name);
	bool RunSSCModule(wxString &name);
	bool FreeSSCModule();
	bool Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold);
	bool Output(ssc_data_t p_data);
	bool Footer();
};

class CodeGen_csharp : virtual public CodeGen_Base
{
public:
	CodeGen_csharp(Case *cc, const wxString &folder);
	bool Header();
	bool CreateSSCModule(wxString &name);
	bool RunSSCModule(wxString &name);
	bool FreeSSCModule();
	bool Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold);
	bool Output(ssc_data_t p_data);
	bool Footer();
};

class CodeGen_matlab : virtual public CodeGen_Base
{
public:
	CodeGen_matlab(Case *cc, const wxString &folder);
	bool Header();
	bool CreateSSCModule(wxString &name);
	bool RunSSCModule(wxString &name);
	bool FreeSSCModule();
	bool Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold);
	bool Output(ssc_data_t p_data);
	bool Footer();
};

class CodeGen_python : virtual public CodeGen_Base
{
public:
	CodeGen_python(Case *cc, const wxString &folder);
	bool Header();
	bool CreateSSCModule(wxString &name);
	bool RunSSCModule(wxString &name);
	bool FreeSSCModule();
	bool Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold);
	bool Output(ssc_data_t p_data);
	bool Footer();
};

#endif

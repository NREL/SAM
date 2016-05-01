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

	
	// language specfic header and supporting functions
	virtual bool Header(FILE *fp)=0;
	virtual bool CreateSSCData(FILE *fp, wxString &name) = 0;
	virtual bool FreeSSCData(FILE *fp, wxString &name) = 0;
	virtual bool CreateSSCModule(FILE *fp, wxString &name) = 0;
	virtual bool RunSSCModule(FILE *fp, wxString &name) = 0;
	virtual bool FreeSSCModule(FILE *fp, wxString &name) = 0;
//	virtual bool SetSSCVariable(FILE *fp)=0;
//	virtual bool GetSSCVariable(FILE *fp)=0;
	virtual bool Input(FILE *fp, ssc_data_t p_data, const char *name, const wxString &folder)= 0;
	// language specific output specification (e.g. printf)
	virtual bool Output(FILE *fp)=0;
	
	// try to make same across languages
	bool GenerateCode(FILE *fp);
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
	bool CreateSSCData(FILE *fp, wxString &name);
	bool FreeSSCData(FILE *fp, wxString &name);
	bool CreateSSCModule(FILE *fp, wxString &name);
	bool RunSSCModule(FILE *fp, wxString &name);
	bool FreeSSCModule(FILE *fp, wxString &name);
//	bool SetSSCVariable(FILE *fp);
//	bool GetSSCVariable(FILE *fp);
	bool Input(FILE *fp, ssc_data_t p_data, const char *name, const wxString &folder);
	// language specific output specification (e.g. printf)
	bool Output(FILE *fp);

};
#endif

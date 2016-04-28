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
	CodeGen_Base( Case *cc, const wxString &name );

	void Clear();
	Case *GetCase() { return m_case; }

	/*
	// language specfic header and supporting functions
	virtual bool Header(FILE *fp)=0;
	virtual bool CreateSSCData(FILE *fp, wxString &name)=0;
	virtual bool FreeSSCData(FILE *fp, wxString &name)=0;
	virtual bool CreateSSCModule(FILE *fp, wxString &name)=0;
	virtual bool RunSSCModule(FILE *fp, wxString &name)=0;
	virtual bool FreeSSCModule(FILE *fp, wxString &name)=0;
	virtual bool SetSSCVariable(FILE *fp, wxString &name)=0;
	virtual bool GetSSCVariable(FILE *fp, wxString &name)=0;
	// language specific output specification (e.g. printf)
	virtual bool Output(FILE *fp, wxString &name)=0;
	*/
	// try to make same across languages
	bool GenerateCode(FILE *fp);
	bool Prepare();
	bool Ok();
	void AddData(CodeGenData md);
	static bool ShowCodeGenDialog(CaseWindow *cw);

protected:
	Case *m_case;
	wxString m_name;
	wxArrayString m_errors;
	VarTable m_inputs;
	std::vector<CodeGenData> m_data;
};


#endif

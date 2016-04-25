#ifndef __codegen_h
#define __codegen_h

#include <wx/string.h>
#include <wx/stream.h>
#include <wx/dialog.h>

#include <wex/tpdlg.h>

#include <lk/env.h>

#include <ssc/sscapi.h>

#include "variables.h"

class Case;
class ConfigInfo;
class wxThreadProgressDialog;
class CodeGen_BaseDialog;


class CodeGen_Base
{
public:
	CodeGen_Base( Case *cc, const wxString &name );

	void Clear();
	Case *GetCase() { return m_case; }

	// language specfic header and supporting functions
	virtual bool Header(wxOutputStream &os)=0;
	virtual bool CreateSSCData(wxOutputStream &os, wxString &name)=0;
	virtual bool FreeSSCData(wxOutputStream &os, wxString &name)=0;
	virtual bool CreateSSCModule(wxOutputStream &os, wxString &name)=0;
	virtual bool RunSSCModule(wxOutputStream &os, wxString &name)=0;
	virtual bool FreeSSCModule(wxOutputStream &os, wxString &name)=0;
	virtual bool SetSSCVariable(wxOutputStream &os, wxString &name)=0;
	virtual bool GetSSCVariable(wxOutputStream &os, wxString &name)=0;
	// language specific output specification (e.g. printf)
	virtual bool Output(wxOutputStream &os, wxString &name)=0;

	// try to make same across languages
	bool GenerateCode(wxOutputStream &os);

	bool Ok();

protected:
	Case *m_case;
	wxString m_name;
	wxArrayString m_errors;
};

// for file and language prompting
class CodeGen_BaseDialog
{
public:
	CodeGen_BaseDialog( const wxString &message=wxEmptyString );
	~CodeGen_BaseDialog();

private:
	wxFrame *m_transp;
};
#endif

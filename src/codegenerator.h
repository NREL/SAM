/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

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
	virtual bool SupportingFiles() = 0;
	virtual bool Header()=0;
	virtual bool CreateSSCModule(wxString &name) = 0;
	virtual bool RunSSCModule(wxString &name) = 0;
	virtual bool FreeSSCModule() = 0;

	virtual bool Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold) = 0;
	virtual bool Output(ssc_data_t p_data) = 0;

	virtual bool Footer() = 0;

	// same across languages
	bool PlatformFiles();
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
	bool SupportingFiles();
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
	bool SupportingFiles();
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
	bool SupportingFiles();
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
	bool SupportingFiles();
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
	bool SupportingFiles();
	bool FreeSSCModule();
	bool Footer();
};

class CodeGen_python2 : public virtual CodeGen_python
{
public:
	CodeGen_python2(Case *cc, const wxString &folder);
	bool Header();
	bool Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold);
	bool Output(ssc_data_t p_data);
	bool CreateSSCModule(wxString &name);
	bool RunSSCModule(wxString &name);
};

class CodeGen_python3 : public virtual CodeGen_python
{
public:
	CodeGen_python3(Case *cc, const wxString &folder);
	bool Header();
	bool Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold);
	bool Output(ssc_data_t p_data);
	bool CreateSSCModule(wxString &name);
	bool RunSSCModule(wxString &name);
};


class CodeGen_java : virtual public CodeGen_Base
{
public:
	CodeGen_java(Case *cc, const wxString &folder);
	bool SupportingFiles();
	bool Header();
	bool CreateSSCModule(wxString &name);
	bool RunSSCModule(wxString &name);
	bool FreeSSCModule();
	bool Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold);
	bool Output(ssc_data_t p_data);
	bool Footer();
};

class CodeGen_php : public virtual CodeGen_Base
{
public:
	CodeGen_php(Case *cc, const wxString &folder);
	bool Header();
	bool CreateSSCModule(wxString &name);
	bool RunSSCModule(wxString &name);
	bool FreeSSCModule();
	bool Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold);
	bool Output(ssc_data_t p_data);
	bool Footer();
};

class CodeGen_php5 : public virtual CodeGen_php
{
public:
	CodeGen_php5(Case *cc, const wxString &folder);
	bool SupportingFiles();
};

class CodeGen_php7 : public virtual CodeGen_php
{
public:
	CodeGen_php7(Case *cc, const wxString &folder);
	bool SupportingFiles();
};

class CodeGen_vba : public virtual CodeGen_Base
{
public:
	CodeGen_vba(Case *cc, const wxString &folder);
	bool SupportingFiles();
	bool Header();
	bool CreateSSCModule(wxString &name);
	bool RunSSCModule(wxString &name);
	bool FreeSSCModule();
	bool Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold);
	bool Output(ssc_data_t p_data);
	bool Footer();
};


class CodeGen_ios : virtual public CodeGen_Base
{
public:
	CodeGen_ios(Case *cc, const wxString &folder);
	bool SupportingFiles();
	bool Header();
	bool CreateSSCModule(wxString &name);
	bool RunSSCModule(wxString &name);
	bool FreeSSCModule();
	bool Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold);
	bool Output(ssc_data_t p_data);
	bool Footer();
};


class CodeGen_android : virtual public CodeGen_Base
{
public:
	CodeGen_android(Case *cc, const wxString &folder);
	bool SupportingFiles();
	bool Header();
	bool CreateSSCModule(wxString &name);
	bool RunSSCModule(wxString &name);
	bool FreeSSCModule();
	bool Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold);
	bool Output(ssc_data_t p_data);
	bool Footer();
};

class CodeGen_json : virtual public CodeGen_Base
{
public:
	CodeGen_json(Case *cc, const wxString &folder);
	bool SupportingFiles();
	bool Header();
	bool CreateSSCModule(wxString &name);
	bool RunSSCModule(wxString &name);
	bool FreeSSCModule();
	bool Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold);
	bool Output(ssc_data_t p_data);
	bool Footer();
private:
	int m_num_cm;
	int m_num_metrics;
};


#endif

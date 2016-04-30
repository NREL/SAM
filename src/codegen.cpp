#include <algorithm>

#include <wx/datstrm.h>
#include <wx/gauge.h>
#include <wx/progdlg.h>
#include <wx/thread.h>
#include <wx/statline.h>
#include <wx/stattext.h>
#include <wx/file.h>
#include <wx/ffile.h>

#include <wx/filedlg.h>
#include <wx/filefn.h>

#include <wex/metro.h>
#include <wex/utils.h>

#include <lk/absyn.h>
#include <lk/stdlib.h>
#include <lk/eval.h>

#include <ssc/sscapi.h>

#include "variables.h"
#include "codegen.h"
#include "codegencallback.h"
#include "main.h"
#include "equations.h"
#include "case.h"
#include "casewin.h"
#include "results.h"
#include "invoke.h"


CodeGenCallbackContext::CodeGenCallbackContext(CodeGen_Base *c, const wxString &desc)
	: CaseCallbackContext(c->GetCase(), desc), m_cgb(c)
{
}

CodeGen_Base *CodeGenCallbackContext::GetCodeGen_Base()
{
	return m_cgb;
}


void CodeGenCallbackContext::SetupLibraries(lk::env_t *env)
{
	env->register_funcs(invoke_codegencallback_funcs(), this);
}




enum {
	ID_btn_select_file,
	ID_btn_generate_file,
	ID_btn_open_file,
	ID_txt_code_file,
	ID_choice_language
};





// for file and language prompting
class CodeGen_Dialog : public wxDialog
{
private:
	Case *m_case;
	ConfigInfo *m_ci;
	CaseWindow *m_caseWin;
	wxExtTextCtrl *txt_code_file;
	wxChoice *choice_language;

public:
	CodeGen_Dialog(wxWindow *parent, int id)
		: wxDialog(parent, id, "Code Generator", wxDefaultPosition, wxScaleSize(600, 350),
		wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
	{

		txt_code_file = new wxExtTextCtrl(this, ID_txt_code_file, "<output file>");
 
		wxArrayString data_languages;
		// ids or just index values from here
		data_languages.Add("lk");
		data_languages.Add("c");
		choice_language = new wxChoice(this, ID_choice_language, wxDefaultPosition, wxDefaultSize, data_languages);
		choice_language->SetSelection(0);

		wxBoxSizer *sz1 = new wxBoxSizer(wxHORIZONTAL);
		sz1->Add(new wxStaticText(this, wxID_ANY, "Generated file:"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 4);
		sz1->Add(txt_code_file, 1, wxALL | wxALIGN_CENTER_VERTICAL, 4);
		sz1->Add(new wxButton(this, ID_btn_select_file, "...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxALIGN_CENTER_VERTICAL, 4);

		wxBoxSizer *sz2 = new wxBoxSizer(wxHORIZONTAL);
		sz2->Add(new wxStaticText(this, wxID_ANY, "Desired language:"), 0, wxALL | wxEXPAND, 4);
		sz2->Add(choice_language, 0, wxALL | wxEXPAND, 4);
		sz2->Add(new wxButton(this, ID_btn_generate_file, "Generate File", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 1);
		sz2->Add(new wxButton(this, ID_btn_open_file, "Open file", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 1);

	

		wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
		sizer->Add(sz1, 0, wxALL | wxEXPAND, 5);
		sizer->Add(sz2, 1, wxALL | wxEXPAND, 5);
		sizer->Add(CreateButtonSizer(wxHELP | wxOK | wxCANCEL), 0, wxALL | wxEXPAND, 10);
		SetSizerAndFit(sizer);
	}

	~CodeGen_Dialog()
	{
	}

	void Set(CaseWindow *cwin)
	{
		m_caseWin = cwin;
		m_case = cwin->GetCase();
		m_ci = m_case->GetConfiguration();

	}


	void OnCodeFile(wxCommandEvent &evt)
	{
		if (!m_case) return;

		wxFileDialog dlg(this, "Specify an output file", "", "", "Code Files (*.*)|*.*");
		if (dlg.ShowModal() == wxID_OK)
		{
			wxString fn = dlg.GetPath();
			if (wxPathOnly(fn) == wxPathOnly(SamApp::Window()->GetProjectFileName()))
				fn = dlg.GetFilename();

			txt_code_file->SetValue(fn);
		}
	}

	void OnGenerateFile(wxCommandEvent &)
	{
		// create appropriate language class with case passed to constructor
		// run GenerateCode function
		int code = choice_language->GetSelection();
		// TODO test for valid filename
		wxString fn = txt_code_file->GetValue();
		if (FILE *fp = fopen(fn.c_str(), "w"))
		{
			if (code== 0) // lk
			{
				// testing - will use derived class for each language
				CodeGen_lk *cg = new CodeGen_lk(m_case, "test ");
				cg->GenerateCode(fp);
			}
			fclose(fp);
		}
	}

	void OnOpenFile(wxCommandEvent &)
	{
		// create appropriate language class with case passed to constructor
		// run GenerateCode function
		int code = choice_language->GetSelection();
		// TODO test for valid filename
		wxString fn = txt_code_file->GetValue();
		wxLaunchDefaultApplication(fn);
	}



	void OnHelp(wxCommandEvent &)
	{
		SamApp::ShowHelp("code_generation");
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(CodeGen_Dialog, wxDialog)
EVT_BUTTON(ID_btn_select_file, CodeGen_Dialog::OnCodeFile)
EVT_BUTTON(ID_btn_generate_file, CodeGen_Dialog::OnGenerateFile)
EVT_BUTTON(ID_btn_open_file, CodeGen_Dialog::OnOpenFile)
EVT_BUTTON(wxID_HELP, CodeGen_Dialog::OnHelp)
END_EVENT_TABLE()


/*

// prototype for each language
static void dump_variable(FILE *fp, ssc_data_t p_data, const char *name)
{
	ssc_number_t value;
	ssc_number_t *p;
	int len, nr, nc;
	wxString str_value;
	double dbl_value;
	int type = ::ssc_data_query(p_data, name);
	switch (type)
	{
	case SSC_STRING:
		str_value = wxString::FromUTF8(::ssc_data_get_string(p_data, name));
		str_value.Replace("\\", "/");
		fprintf(fp, "var( '%s', '%s' );\n", name, (const char*)str_value.c_str());
		break;
	case SSC_NUMBER:
		::ssc_data_get_number(p_data, name, &value);
		dbl_value = (double)value;
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(fp, "var( '%s', %lg );\n", name, dbl_value);
		break;
	case SSC_ARRAY:
		p = ::ssc_data_get_array(p_data, name, &len);
		fprintf(fp, "var( '%s', [", name);
		for (int i = 0; i<(len - 1); i++)
		{
			dbl_value = (double)p[i];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(fp, " %lg,", dbl_value);
		}
		dbl_value = (double)p[len - 1];
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(fp, " %lg ] );\n", dbl_value);
		break;
	case SSC_MATRIX:
		p = ::ssc_data_get_matrix(p_data, name, &nr, &nc);
		len = nr*nc;
		fprintf(fp, "var( '%s', \n[ [", name);
		for (int k = 0; k<(len - 1); k++)
		{
			dbl_value = (double)p[k];
			if (dbl_value > 1e38) dbl_value = 1e38;
			if ((k + 1) % nc == 0)
				fprintf(fp, " %lg ], \n[", dbl_value);
			else
				fprintf(fp, " %lg,", dbl_value);
		}
		dbl_value = (double)p[len - 1];
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(fp, " %lg ] ] );\n", dbl_value);
	}
}

*/






static bool VarValueToSSC( VarValue *vv, ssc_data_t pdata, const wxString &sscname )
{
	switch( vv->Type() )
	{
	case VV_NUMBER:
		ssc_data_set_number( pdata, sscname.c_str(), (ssc_number_t)vv->Value() );
		break;
	case VV_ARRAY:
	{
		size_t n;
		float *p = vv->Array( &n );
		if ( sizeof(ssc_number_t) == sizeof( float ) )
			ssc_data_set_array( pdata, sscname.c_str(), p, n );
		else
		{
			ssc_number_t *pp = new ssc_number_t[n];
			for( size_t i=0;i<n;i++ )
				pp[i] = p[i];

			ssc_data_set_array( pdata, sscname.c_str(), pp, n );

			delete [] pp;
		}
	}
		break;
	case VV_MATRIX:
	{
		matrix_t<float> &fl = vv->Matrix();
		if ( sizeof(ssc_number_t) == sizeof(float) )
		{
			ssc_data_set_matrix( pdata, sscname.c_str(), fl.data(), fl.nrows(), fl.ncols() );
		}
		else
		{
			ssc_number_t *pp = new ssc_number_t[ fl.nrows() * fl.ncols() ];
			size_t n = 0;
			for( size_t r = 0; r < fl.nrows(); r++ )
				for( size_t c=0;c<fl.ncols();c++)
					pp[n++] = (ssc_number_t)fl(r,c);

			ssc_data_set_matrix( pdata, sscname.c_str(), pp, fl.nrows(), fl.ncols() );
			delete [] pp;
		}
	}
		break;
	case VV_STRING:
		ssc_data_set_string( pdata, sscname.c_str(), vv->String().c_str() );
		break;
	case VV_TABLE:
	{
		ssc_data_t tab = ssc_data_create();
		VarTable &vt = vv->Table();
		for( VarTable::iterator it = vt.begin();
			it != vt.end();
			++it )
		{
			VarValueToSSC( it->second, tab, it->first );
		}

		ssc_data_set_table( pdata, sscname.c_str(), tab );

		ssc_data_free( tab ); // ssc_data_set_table above makes a deep copy, so free this here
	}
		break;


	case VV_INVALID:
	default:
		return false;
	}

	return true;
}


static void write_array_string(wxDataOutputStream &out, wxArrayString &list)
{
	out.Write32(list.size());
	for (size_t i = 0; i<list.size(); i++)
		out.WriteString(list[i]);
}

static void read_array_string(wxDataInputStream &in, wxArrayString &list)
{
	list.Clear();
	size_t n = in.Read32();
	for (size_t i = 0; i<n; i++)
		list.Add(in.ReadString());
}

// end of prototype from simulation.cpp



CodeGen_Base::CodeGen_Base( Case *cc, const wxString &name )
	: m_case( cc ), m_name( name )
{
}

bool CodeGen_Base::Prepare()
{
	m_inputs.clear();
	m_inputs = m_case->Values();
	/* may be used in the future
	// transfer all the values except for ones that have been 'overriden'
	for (VarTableBase::const_iterator it = m_case->Values().begin();
		it != m_case->Values().end();
		++it)
		if (0 == m_inputs.Get(it->first))
			m_inputs.Set(it->first, *(it->second));
*/
	// recalculate all the equations
	CaseEvaluator eval(m_case, m_inputs, m_case->Equations());
	int n = eval.CalculateAll();

	if (n < 0)
	{
		wxArrayString &errs = eval.GetErrors();
		for (size_t i = 0; i<errs.size(); i++)
			m_errors.Add(errs[i]);

		return false;
	}
	return true;
}


bool CodeGen_Base::GenerateCode(FILE *fp)
{
	ConfigInfo *cfg = m_case->GetConfiguration();


	if (!cfg)
	{
		m_errors.Add("no valid configuration for this case");
		return false;
	}
	if (!Prepare())
	{
		m_errors.Add("preparation failed for this case");
		return false;
	}

	// get list of compute modules from case configuration
	wxArrayString simlist = cfg->Simulations;

	if (simlist.size() == 0)
	{
		m_errors.Add("No simulation compute modules defined for this configuration.");
		return false;
	}

	// go through and translate all SAM UI variables to SSC variables
	ssc_data_t p_data = ssc_data_create();

	for (size_t kk = 0; kk < simlist.size(); kk++)
	{
		ssc_module_t p_mod = ssc_module_create(simlist[kk].c_str());
		if (!p_mod)
		{
			m_errors.Add("could not create ssc module: " + simlist[kk]);
			continue;
		}

		int pidx = 0;
		while (const ssc_info_t p_inf = ssc_module_var_info(p_mod, pidx++))
		{
			int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
			int data_type = ssc_info_data_type(p_inf); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX		
			wxString name(ssc_info_name(p_inf)); // assumed to be non-null
			wxString reqd(ssc_info_required(p_inf));

			if (var_type == SSC_INPUT || var_type == SSC_INOUT)
			{
				// handle ssc variable names
				// that are explicit field accesses"shading:mxh"
				wxString field;
				int pos = name.Find(':');
				if (pos != wxNOT_FOUND)
				{
					field = name.Mid(pos + 1);
					name = name.Left(pos);
				}

				int existing_type = ssc_data_query(p_data, ssc_info_name(p_inf));
				if (existing_type != data_type)
				{
					if (VarValue *vv = m_case->Values().Get(name))
					{
						if (!field.IsEmpty())
						{
							if (vv->Type() != VV_TABLE)
								m_errors.Add("SSC variable has table:field specification, but '" + name + "' is not a table in SAM");

							bool do_copy_var = false;
							if (reqd.Left(1) == "?")
							{
								// if the SSC variable is optional, check for the 'en_<field>' element in the table
								if (VarValue *en_flag = vv->Table().Get("en_" + field))
									if (en_flag->Boolean())
										do_copy_var = true;
							}
							else do_copy_var = true;

							if (do_copy_var)
							{
								if (VarValue *vv_field = vv->Table().Get(field))
								{
									if (!VarValueToSSC(vv_field, p_data, name + ":" + field))
										m_errors.Add("Error translating table:field variable from SAM UI to SSC for '" + name + "':" + field);
								}
							}

						}

						if (!VarValueToSSC(vv, p_data, name))
							m_errors.Add("Error translating data from SAM UI to SSC for " + name);

					}
					else if (reqd == "*")
						m_errors.Add("SSC requires input '" + name + "', but was not found in the SAM UI or from previous simulations");
				}
			}
		}

		const char *name = ssc_data_first(p_data);
		while (name)
		{
			//dump_variable(fp, p_data, name);
			Input(fp, p_data, name, m_name);
			name = ssc_data_next(p_data);
		}
		RunSSCModule(fp, simlist[kk]);
//		fprintf(fp, "run('%s');\n", (const char*)simlist[kk].c_str());

	}

	// outputs
	m_data.clear();
	CodeGenCallbackContext cc(this, "Metrics callback: " + cfg->Technology + ", " + cfg->Financing);

	// first try to invoke a T/F specific callback if one exists
	if (lk::node_t *metricscb = SamApp::GlobalCallbacks().Lookup("metrics", cfg->Technology + "|" + cfg->Financing))
		cc.Invoke(metricscb, SamApp::GlobalCallbacks().GetEnv());

	// if no metrics were defined, run it T & F one at a time
	if (m_data.size() == 0)
	{
		if (lk::node_t *metricscb = SamApp::GlobalCallbacks().Lookup("metrics", cfg->Technology))
			cc.Invoke(metricscb, SamApp::GlobalCallbacks().GetEnv());

		if (lk::node_t *metricscb = SamApp::GlobalCallbacks().Lookup("metrics", cfg->Financing))
			cc.Invoke(metricscb, SamApp::GlobalCallbacks().GetEnv());
	}

	/*
	for (size_t ii = 0; ii < m_data.size(); ii++)
		fprintf(fp, "outln('%s ' + var('%s'));\n", (const char*)m_data[ii].label.c_str(), (const char*)m_data[ii].var.c_str());
*/
	if (!Output(fp))
	{
		m_errors.Add("Output failed");
	}
	/*
	// write language specific header
	Header(fp);

	// create ssc data container 
	wxString data_name = "data";
	CreateSSCData(fp, data_name);

	// list of outputs from metrics - go through and call output for each
	wxString out_name = "";
	Output(fp, out_name);
	// clean up
	FreeSSCData(fp, data_name);
	*/
	return true;
}

void CodeGen_Base::AddData(CodeGenData md) 
{ 
	m_data.push_back(md); 
}


bool CodeGen_Base::Ok()
{
	return m_errors.size() == 0;
}

bool CodeGen_Base::ShowCodeGenDialog(CaseWindow *cw)
{
	CodeGen_Dialog dialog(SamApp::Window(), wxID_ANY);
	dialog.CenterOnParent();
	dialog.Set(cw);
	if (wxID_OK == dialog.ShowModal())
	{
		return true;
	}
	else
		return false;
}



CodeGen_lk::CodeGen_lk(Case *cc, const wxString &name) : CodeGen_Base(cc, name)
{

}


bool CodeGen_lk::Output(FILE *fp)
{
	for (size_t ii = 0; ii < m_data.size(); ii++)
		fprintf(fp, "outln('%s ' + var('%s'));\n", (const char*)m_data[ii].label.c_str(), (const char*)m_data[ii].var.c_str());
	return true;
}

bool CodeGen_lk::Input(FILE *fp, ssc_data_t p_data, const char *name, wxString Folder)
{
	ssc_number_t value;
	ssc_number_t *p;
	int len, nr, nc;
	wxString str_value;
	double dbl_value;
	int type = ::ssc_data_query(p_data, name);
	switch (type)
	{
	case SSC_STRING:
		str_value = wxString::FromUTF8(::ssc_data_get_string(p_data, name));
		str_value.Replace("\\", "/");
		fprintf(fp, "var( '%s', '%s' );\n", name, (const char*)str_value.c_str());
		break;
	case SSC_NUMBER:
		::ssc_data_get_number(p_data, name, &value);
		dbl_value = (double)value;
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(fp, "var( '%s', %lg );\n", name, dbl_value);
		break;
	case SSC_ARRAY:
		p = ::ssc_data_get_array(p_data, name, &len);
		fprintf(fp, "var( '%s', [", name);
		for (int i = 0; i<(len - 1); i++)
		{
			dbl_value = (double)p[i];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(fp, " %lg,", dbl_value);
		}
		dbl_value = (double)p[len - 1];
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(fp, " %lg ] );\n", dbl_value);
		break;
	case SSC_MATRIX:
		p = ::ssc_data_get_matrix(p_data, name, &nr, &nc);
		len = nr*nc;
		fprintf(fp, "var( '%s', \n[ [", name);
		for (int k = 0; k<(len - 1); k++)
		{
			dbl_value = (double)p[k];
			if (dbl_value > 1e38) dbl_value = 1e38;
			if ((k + 1) % nc == 0)
				fprintf(fp, " %lg ], \n[", dbl_value);
			else
				fprintf(fp, " %lg,", dbl_value);
		}
		dbl_value = (double)p[len - 1];
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(fp, " %lg ] ] );\n", dbl_value);
	}
	return true;
}


bool CodeGen_lk::RunSSCModule(FILE *fp, wxString& name)
{
	if (name.IsNull() || name.Length() < 1)
		return false;
	else
		fprintf(fp, "run('%s');\n", (const char*)name.c_str());
	return true;
}


bool CodeGen_lk::Header(FILE *fp)
{
	return true;
}

bool CodeGen_lk::CreateSSCData(FILE *fp, wxString &name)
{
	return true;
}

bool CodeGen_lk::FreeSSCData(FILE *fp, wxString &name)
{
	return true;
}

bool CodeGen_lk::CreateSSCModule(FILE *fp, wxString &name)
{
	return true;
}


bool CodeGen_lk::FreeSSCModule(FILE *fp, wxString &name)
{
	return true;
}




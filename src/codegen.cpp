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
	ID_btn_select_folder,
	ID_btn_generate,
	ID_btn_open_folder,
	ID_txt_code_folder,
	ID_choice_language,
	ID_choice_array_matrix_threshold
};





// for file and language prompting
class CodeGen_Dialog : public wxDialog
{
private:
	Case *m_case;
	ConfigInfo *m_ci;
	CaseWindow *m_caseWin;
	wxExtTextCtrl *txt_code_folder;
	wxChoice *choice_language;
	wxChoice *choice_array_matrix_threshold;
	wxString m_foldername;

public:
	CodeGen_Dialog(wxWindow *parent, int id)
		: wxDialog(parent, id, "Code Generator", wxDefaultPosition, wxScaleSize(600, 350),
		wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
	{

		txt_code_folder = new wxExtTextCtrl(this, ID_txt_code_folder, "");
 
		wxArrayString data_languages;
		// ids or just index values from here
		data_languages.Add("lk");
		data_languages.Add("c");
		choice_language = new wxChoice(this, ID_choice_language, wxDefaultPosition, wxDefaultSize, data_languages);
		choice_language->SetSelection(0);

		wxArrayString data_threshold;
		// ids or just index values from here
		data_threshold.Add("no separate files");
		data_threshold.Add("all in separate files");
		data_threshold.Add(">288 elements (diurnal)");
		data_threshold.Add(">20 elements (typical analysis period)");
		choice_array_matrix_threshold = new wxChoice(this, ID_choice_language, wxDefaultPosition, wxDefaultSize, data_threshold);
		choice_array_matrix_threshold->SetSelection(2); // default 288

		wxBoxSizer *sz1 = new wxBoxSizer(wxHORIZONTAL);
		sz1->Add(new wxStaticText(this, wxID_ANY, "Output folder:"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 4);
		sz1->Add(txt_code_folder, 1, wxALL | wxALIGN_CENTER_VERTICAL, 4);
		sz1->Add(new wxButton(this, ID_btn_select_folder, "...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxALIGN_CENTER_VERTICAL, 4);

		wxBoxSizer *sz2 = new wxBoxSizer(wxHORIZONTAL);
		sz2->Add(new wxStaticText(this, wxID_ANY, "Output code language:"), 0, wxALL | wxEXPAND, 4);
		sz2->Add(choice_language, 0, wxALL | wxEXPAND, 4);

		wxBoxSizer *sz3 = new wxBoxSizer(wxHORIZONTAL);
		sz3->Add(new wxStaticText(this, wxID_ANY, "Separate files for arrays and matrices:"), 0, wxALL | wxEXPAND, 4);
		sz3->Add(choice_array_matrix_threshold, 0, wxALL | wxEXPAND, 4);


		wxBoxSizer *sz4 = new wxBoxSizer(wxHORIZONTAL);
		sz4->Add(new wxButton(this, wxHELP, "Help", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 1);
		sz4->AddStretchSpacer();
		sz4->Add(new wxButton(this, ID_btn_generate, "Generate code", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 1);
		sz4->Add(new wxButton(this, ID_btn_open_folder, "Open folder", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 1);
		sz4->Add(new wxButton(this, wxCANCEL, "Close", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 1);

	

		wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
		sizer->Add(sz1, 0, wxALL | wxEXPAND, 5);
		sizer->Add(sz2, 1, wxALL | wxEXPAND, 5);
		sizer->Add(sz3, 1, wxALL | wxEXPAND, 5);
		sizer->Add(sz4, 0, wxALL | wxEXPAND, 5);
		//sizer->Add(CreateButtonSizer(wxHELP | wxOK | wxCANCEL), 0, wxALL | wxEXPAND, 10);
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

	int GetThreshold()
	{
		int threshold = 288; // default
		switch (choice_array_matrix_threshold->GetSelection())
		{
		case 0: // none (greater than 10,000 always written out)
			threshold = 10000;
			break;
		case 1: // all written to separate files
			threshold = 0;
			break;
		case 2: // diurnal
			threshold = 288;
			break;
		case 3: // analysis period
			threshold = 20;
			break;
		default:
			threshold = 288;
		}
		return threshold;
	}

	void OnCodeFolder(wxCommandEvent &evt)
	{
		if (!m_case) return;

		wxDirDialog dlg(this, "Select an output folder", ::wxGetHomeDir(), wxDD_DEFAULT_STYLE | wxDD_DIR_MUST_EXIST);
		if (dlg.ShowModal() == wxID_OK)
		{
			wxString fn = dlg.GetPath();
			txt_code_folder->SetValue(fn);
		}
	}

	void OnGenerate(wxCommandEvent &)
	{
		// create appropriate language class with case passed to constructor
		// run GenerateCode function
		int code = choice_language->GetSelection();
		// TODO test for valid folder
		wxString folder = txt_code_folder->GetValue();
		folder.Replace("\\", "/");
		if (!wxDirExists(folder))
		{
			wxMessageBox(wxString::Format("Error: the path '%s' does not exist", (const char*)folder.c_str()), "Path Error", wxICON_ERROR);
			return;
		}
		wxString m_foldername = folder;
		int threshold = GetThreshold();
		if (code == 0) // lk
		{
			m_foldername = folder + "/" + SamApp::Project().GetCaseName(m_case) + ".lk";
			if (FILE *fp = fopen(m_foldername.c_str(), "w"))
			{
				CodeGen_lk *cg = new CodeGen_lk(m_case, folder);
				cg->GenerateCode(fp, threshold);
				fclose(fp);
				if (!cg->Ok())	wxMessageBox(cg->GetErrors(), "Generate Errors", wxICON_ERROR);
			}
		}
		else if (code == 1) // c
		{
			m_foldername = folder + "/" + SamApp::Project().GetCaseName(m_case) + ".c";
			if (FILE *fp = fopen(m_foldername.c_str(), "w"))
			{
				CodeGen_c *cg = new CodeGen_c(m_case, folder);
				cg->GenerateCode(fp, threshold);
				fclose(fp);
				if (!cg->Ok())	wxMessageBox(cg->GetErrors(), "Generate Errors", wxICON_ERROR);
			}
		}
	}

	void OnOpenFolder(wxCommandEvent &)
	{
		// run GenerateCode function
		int code = choice_language->GetSelection();
		// 
		wxString fn = txt_code_folder->GetValue();
		wxLaunchDefaultApplication(fn);
	}

	void OnHelp(wxCommandEvent &)
	{
		SamApp::ShowHelp("code_generation");
	}

	void OnCancel(wxCommandEvent &)
	{
		Close();
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(CodeGen_Dialog, wxDialog)
EVT_BUTTON(ID_btn_select_folder, CodeGen_Dialog::OnCodeFolder)
EVT_BUTTON(ID_btn_generate, CodeGen_Dialog::OnGenerate)
EVT_BUTTON(ID_btn_open_folder, CodeGen_Dialog::OnOpenFolder)
EVT_BUTTON(wxHELP, CodeGen_Dialog::OnHelp)
EVT_BUTTON(wxCANCEL, CodeGen_Dialog::OnCancel)
END_EVENT_TABLE()



static bool VarValueToSSC(VarValue *vv, ssc_data_t pdata, const wxString &sscname)
{
	switch (vv->Type())
	{
	case VV_NUMBER:
		ssc_data_set_number(pdata, sscname.c_str(), (ssc_number_t)vv->Value());
		break;
	case VV_ARRAY:
	{
		size_t n;
		float *p = vv->Array(&n);
		if (sizeof(ssc_number_t) == sizeof(float))
			ssc_data_set_array(pdata, sscname.c_str(), p, n);
		else
		{
			ssc_number_t *pp = new ssc_number_t[n];
			for (size_t i = 0; i<n; i++)
				pp[i] = p[i];

			ssc_data_set_array(pdata, sscname.c_str(), pp, n);

			delete[] pp;
		}
	}
	break;
	case VV_MATRIX:
	{
		matrix_t<float> &fl = vv->Matrix();
		if (sizeof(ssc_number_t) == sizeof(float))
		{
			ssc_data_set_matrix(pdata, sscname.c_str(), fl.data(), fl.nrows(), fl.ncols());
		}
		else
		{
			ssc_number_t *pp = new ssc_number_t[fl.nrows() * fl.ncols()];
			size_t n = 0;
			for (size_t r = 0; r < fl.nrows(); r++)
				for (size_t c = 0; c<fl.ncols(); c++)
					pp[n++] = (ssc_number_t)fl(r, c);

			ssc_data_set_matrix(pdata, sscname.c_str(), pp, fl.nrows(), fl.ncols());
			delete[] pp;
		}
	}
	break;
	case VV_STRING:
		ssc_data_set_string(pdata, sscname.c_str(), vv->String().c_str());
		break;
	case VV_TABLE:
	{
		ssc_data_t tab = ssc_data_create();
		VarTable &vt = vv->Table();
		for (VarTable::iterator it = vt.begin();
			it != vt.end();
			++it)
		{
			VarValueToSSC(it->second, tab, it->first);
		}

		ssc_data_set_table(pdata, sscname.c_str(), tab);

		ssc_data_free(tab); // ssc_data_set_table above makes a deep copy, so free this here
	}
	break;


	case VV_INVALID:
	default:
		return false;
	}

	return true;
}



static bool TypeToSSC( int Type, ssc_data_t pdata, const wxString &sscname )
{
//	switch( Type )
//	{
//	case VV_NUMBER:
		ssc_data_set_number( pdata, sscname.c_str(), (ssc_number_t)0 );
/*		break;
	case VV_ARRAY:
	{
		size_t n=2;
		float p[2] = { 0.0, 0.0 };
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
		matrix_t<float> fl(2,2,0.0);
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
		ssc_data_set_string( pdata, sscname.c_str(), "name" );
		break;

	case VV_INVALID:
	default:
		return false;
	}
*/
	return true;
}



CodeGen_Base::CodeGen_Base( Case *cc, const wxString &folder )
	: m_case( cc ), m_folder( folder )
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


bool CodeGen_Base::GenerateCode(FILE *fp, const int &array_matrix_threshold)
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

	// go through and get outputs for determining types
	ssc_data_t p_data_output = ssc_data_create();

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
			else if (var_type == SSC_OUTPUT)
			{
				wxString field;
				int pos = name.Find(':');
				if (pos != wxNOT_FOUND)
				{
					field = name.Mid(pos + 1);
					name = name.Left(pos);
				}

				if (!TypeToSSC(data_type, p_data_output, name))
					m_errors.Add("Error for output " + name);

			}
		}
		/* avoid duplication of inputs 
		const char *name = ssc_data_first(p_data);
		while (name)
		{
			Input(fp, p_data, name, m_folder, array_matrix_threshold);
			name = ssc_data_next(p_data);
		}
		RunSSCModule(fp, simlist[kk]);
		*/
	}

	// write language specific header
	if (!Header(fp))
		m_errors.Add("Header failed");



	const char *name = ssc_data_first(p_data);
	while (name)
	{
		if (!Input(fp, p_data, name, m_folder, array_matrix_threshold))
			m_errors.Add(wxString::Format("Input %s write failed",name));
		name = ssc_data_next(p_data);
	}

	// run compute modules in sequence (INOUT variables will be updated
	for (size_t kk = 0; kk < simlist.size(); kk++)
	{
		CreateSSCModule(fp, simlist[kk]);
		RunSSCModule(fp, simlist[kk]);
		FreeSSCModule(fp);
	}
	// outputs - metrics for case
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

	if (!Output(fp, p_data_output))
		m_errors.Add("Output failed");

	if (!Footer(fp))
		m_errors.Add("Footer failed");

	return (m_errors.Count() == 0);
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

wxString CodeGen_Base::GetErrors()
{
	wxString ret = "";
	for (size_t i = 0; i < m_errors.Count(); i++)
		ret += m_errors[i] + "\n";
	return ret;
}

// lk code generation class

CodeGen_lk::CodeGen_lk(Case *cc, const wxString &folder) : CodeGen_Base(cc, folder)
{
}


bool CodeGen_lk::Output(FILE *fp, ssc_data_t p_data)
{
	for (size_t ii = 0; ii < m_data.size(); ii++)
		fprintf(fp, "outln('%s ' + var('%s'));\n", (const char*)m_data[ii].label.c_str(), (const char*)m_data[ii].var.c_str());
	return true;
}

bool CodeGen_lk::Input(FILE *fp, ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold)
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
		if (len > array_matrix_threshold)
		{ // separate csv file (var_name.csv in folder) for each variable
			wxCSVData csv;
			wxString fn = folder + "/" + wxString(name) + ".csv";
			// write out as single column data for compatibility with csvread in SDKTool
			for (int i = 0; i < len; i++)
			{
				dbl_value = (double)p[i];
				if (dbl_value > 1e38) dbl_value = 1e38;
//				str_value = wxString::Format("%lg", dbl_value);
				csv.Set(i, 0, wxString::Format("%lg", dbl_value));
			}
			csv.WriteFile(fn);
//			fprintf(fp, "var( '%s', csvread('%s'));", name, (const char*)fn.c_str());
			fprintf(fp, "var( '%s', real_array(read_text_file('%s')));\n", name, (const char*)fn.c_str());
		}
		else
		{
			fprintf(fp, "var( '%s', [", name);
			for (int i = 0; i < (len - 1); i++)
			{
				dbl_value = (double)p[i];
				if (dbl_value > 1e38) dbl_value = 1e38;
				fprintf(fp, " %lg,", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(fp, " %lg ] );\n", dbl_value);
		}
		break;
	case SSC_MATRIX:
		p = ::ssc_data_get_matrix(p_data, name, &nr, &nc);
		len = nr*nc;
		if (len > array_matrix_threshold)
		{ // separate csv file (var_name.csv in folder) for each variable
			wxCSVData csv;
			wxString fn = folder + "/" + wxString(name) + ".csv";
			for (int r = 0; r < nr; r++)
			{
				for (int c = 0; c < nc; c++)
				{
					dbl_value = (double)p[r*nc+c];
					if (dbl_value > 1e38) dbl_value = 1e38;
					csv.Set(r, c, wxString::Format("%lg", dbl_value));
				}
			}
			csv.WriteFile(fn);
			fprintf(fp, "var( '%s', csvread('%s'));\n", name, (const char*)fn.c_str());
		}
		else
		{
			fprintf(fp, "var( '%s', \n[ [", name);
			for (int k = 0; k < (len - 1); k++)
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
		// TODO tables in future
	}
	return true;
}


bool CodeGen_lk::RunSSCModule(FILE *fp, wxString &name)
{
	if (name.IsNull() || name.Length() < 1)
		return false;
	else
		fprintf(fp, "run('%s');\n", (const char*)name.c_str());
	return true;
}


bool CodeGen_lk::Header(FILE *fp)
{
	fprintf(fp, "clear();\n");
	return true;
}

bool CodeGen_lk::CreateSSCModule(FILE *fp, wxString &name)
{
	return true;
}

bool CodeGen_lk::FreeSSCModule(FILE *fp)
{
	return true;
}

bool CodeGen_lk::Footer(FILE *fp)
{
	return true;
}


// c code generation class

CodeGen_c::CodeGen_c(Case *cc, const wxString &folder) : CodeGen_Base(cc, folder)
{
}


bool CodeGen_c::Output(FILE *fp, ssc_data_t p_data)
{
//		fprintf(fp, "outln('%s ' + var('%s'));\n", (const char*)m_data[ii].label.c_str(), (const char*)m_data[ii].var.c_str());
	ssc_number_t value;
	ssc_number_t *p;
	int len, nr, nc;
	wxString str_value;
	double dbl_value;
	for (size_t ii = 0; ii < m_data.size(); ii++)
	{
		const char *name = (const char*)m_data[ii].var.c_str();
		int type = ::ssc_data_query(p_data, name); 
		switch (type)
		{
		case SSC_STRING:
			fprintf(fp, "	const char *%s = ssc_data_get_string( data, \"%s\" );\n", name, name);
			fprintf(fp, "	printf(\"\%s = \%s\"), %s, %s);\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_NUMBER:
			fprintf(fp, "	ssc_number_t %s;\n", name);
			fprintf(fp, "	ssc_data_get_number(data, \"%s\", &%s);\n", name, name);
			fprintf(fp, "	printf(\"%%s = %%lg\\n\", \"%s\", (double)%s);\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_ARRAY:
			p = ::ssc_data_get_array(p_data, name, &len);
			fprintf(fp, "ssc_number_t p_%s[%d] ={", name, len);
			fprintf(fp, "ssc_data_get_array( data, \"%s\", p_%s, %d );\n", name, name, len);
			break;
		case SSC_MATRIX:
			// TODO tables in future
			break;
		}
	}
	return true;
}

bool CodeGen_c::Input(FILE *fp, ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold)
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
		fprintf(fp, "	ssc_data_set_string( data, \"%s\", \"%s\" );\n", name, (const char*)str_value.c_str());
		break;
	case SSC_NUMBER:
		::ssc_data_get_number(p_data, name, &value);
		dbl_value = (double)value;
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(fp, "	ssc_data_set_number( data, \"%s\", %lg );\n", name, dbl_value);
		break;
	case SSC_ARRAY:
		p = ::ssc_data_get_array(p_data, name, &len);
		if (len > array_matrix_threshold)
		{ // separate csv file (var_name.csv in folder) for each variable
			wxCSVData csv;
			wxString fn = folder + "/" + wxString(name) + ".csv";
			// write out as single column data for compatibility with csvread in SDKTool
			for (int i = 0; i < len; i++)
			{
				dbl_value = (double)p[i];
				if (dbl_value > 1e38) dbl_value = 1e38;
				//				str_value = wxString::Format("%lg", dbl_value);
				csv.Set(i, 0, wxString::Format("%lg", dbl_value));
			}
			csv.WriteFile(fn);
			//			fprintf(fp, "var( '%s', csvread('%s'));", name, (const char*)fn.c_str());
			fprintf(fp, "var( '%s', real_array(read_text_file('%s')));\n", name, (const char*)fn.c_str());
		}
		else
		{
			fprintf(fp, "	ssc_number_t p_%s[%d] ={", name, len);
			for (int i = 0; i < (len-1); i++)
			{
				dbl_value = (double)p[i];
				if (dbl_value > 1e38) dbl_value = 1e38;
				fprintf(fp, " %lg,", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(fp, " %lg };\n", dbl_value);
			fprintf(fp, "	ssc_data_set_array( data, \"%s\", p_%s, %d );\n", name, name, len);
		}
		break;
	case SSC_MATRIX:
		p = ::ssc_data_get_matrix(p_data, name, &nr, &nc);
		len = nr*nc;
		if (len > array_matrix_threshold)
		{ // separate csv file (var_name.csv in folder) for each variable
			wxCSVData csv;
			wxString fn = folder + "/" + wxString(name) + ".csv";
			for (int r = 0; r < nr; r++)
			{
				for (int c = 0; c < nc; c++)
				{
					dbl_value = (double)p[r*nc + c];
					if (dbl_value > 1e38) dbl_value = 1e38;
					csv.Set(r, c, wxString::Format("%lg", dbl_value));
				}
			}
			csv.WriteFile(fn);
			fprintf(fp, "var( '%s', csvread('%s'));\n", name, (const char*)fn.c_str());
		}
		else
		{
			fprintf(fp, "	ssc_number_t p_%s[%d] ={", name, len);
			for (int k = 0; k < (len - 1); k++)
			{
				dbl_value = (double)p[k];
				if (dbl_value > 1e38) dbl_value = 1e38;
				fprintf(fp, " %lg,", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(fp, " %lg };\n", dbl_value);
			fprintf(fp, "	ssc_data_set_matrix( data, \"%s\", p_%s, %d, %d );\n", name, name, nr, nc);
		}
		// TODO tables in future
	}
	return true;
}


bool CodeGen_c::RunSSCModule(FILE *fp, wxString &name)
{
	fprintf(fp, "	if (ssc_module_exec(module, data) == 0)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		printf(\"error during simulation.\"); \n");
	fprintf(fp, "		ssc_module_free(module); \n");
	fprintf(fp, "		ssc_data_free(data); \n");
	fprintf(fp, "		return -1; \n");
	fprintf(fp, "	}\n");
	return true;
}


bool CodeGen_c::Header(FILE *fp)
{
	// top of file and supporting functions
	fprintf(fp, "#include <stdio.h>\n");
	fprintf(fp, "#include \"sscapi.h\"\n");
	fprintf(fp, "\n");

	// handle message
	fprintf(fp, "ssc_bool_t my_handler(ssc_module_t p_mod, ssc_handler_t p_handler, int action,\n");
	fprintf(fp, "	float f0, float f1, const char *s0, const char *s1, void *user_data)\n");
	fprintf(fp, "{\n");
	fprintf(fp, "	if (action == SSC_LOG)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		// print log message to console\n");
	fprintf(fp, "		switch ((int)f0)\n");
	fprintf(fp, "		{\n");
	fprintf(fp, "		case SSC_NOTICE: printf(\"Notice: %s\", s0); break;\n");
	fprintf(fp, "		case SSC_WARNING: printf(\"Warning: %s\", s0); break;\n");
	fprintf(fp, "		case SSC_ERROR: printf(\"Error: %s\", s0); break;\n");
	fprintf(fp, "		}\n");
	fprintf(fp, "		return 1;\n");
	fprintf(fp, "	}\n");
	fprintf(fp, "	else if (action == SSC_UPDATE)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		// print status update to console\n");
	fprintf(fp, "		printf(\"(%.2f %%) % s\", f0, s0);\n");
	fprintf(fp, "		return 1; // return 0 to abort simulation as needed.\n");
	fprintf(fp, "	}\n");
	fprintf(fp, "	else\n");
	fprintf(fp, "		return 0;\n");
	fprintf(fp, "}\n");
	fprintf(fp, "\n");

	// handle csv files
	fprintf(fp, "\n");

	fprintf(fp, "int main(int argc, char *argv[])\n");
	fprintf(fp, "{\n");

	// create global data container
	fprintf(fp, "	ssc_data_t data = ssc_data_create();\n");
	fprintf(fp, "	if (data == NULL)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		printf(\"error: out of memory.\");\n");
	fprintf(fp, "		return -1;\n");
	fprintf(fp, "	}\n");

	return true;
}

bool CodeGen_c::CreateSSCModule(FILE *fp, wxString &name)
{
	if (name.IsNull() || name.Length() < 1)
		return false;
	else
	{
		fprintf(fp, "	ssc_module_t module = ssc_module_create(\"%s\"); \n", (const char*)name.c_str());
		fprintf(fp, "	if (NULL == module)\n");
		fprintf(fp, "	{\n");
		fprintf(fp, "		printf(\"error: could not create '%s' module.\"); \n", (const char*)name.c_str());
		fprintf(fp, "		ssc_data_free(data); \n");
		fprintf(fp, "		return -1; \n");
		fprintf(fp, "	}\n");
	}
	return true;
}

bool CodeGen_c::FreeSSCModule(FILE *fp)
{
	fprintf(fp, "	ssc_module_free(module);\n");
	return true;
}

bool CodeGen_c::Footer(FILE *fp)
{
	fprintf(fp, "	ssc_data_free(data);\n");
	fprintf(fp, "	return 0;\n");
	fprintf(fp, "}\n");
	return true;
}



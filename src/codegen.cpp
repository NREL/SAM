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


/*
// for file and language prompting
enum {
	ID_btn_open_code,
	ID_btn_open_folder
};

class CodeOpen_Dialog : public wxDialog
{
private:
	Case *m_case;
	wxTextCtrl *m_txtctrl;
	wxFileName m_filename;

public:
	CodeOpen_Dialog(wxWindow *parent, int id, wxFileName &fn, wxString &message)
		: wxDialog(parent, id, "Open Code", wxDefaultPosition, wxScaleSize(600, 350),
		wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER), m_filename(fn)
	{
		m_txtctrl = new wxTextCtrl(this, -1, message,
			wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE | wxTE_READONLY);
		//txtctrl->SetFont( wxFont(10, wxMODERN, wxNORMAL, wxNORMAL) );

		wxBoxSizer *sz4 = new wxBoxSizer(wxHORIZONTAL);
		sz4->Add(new wxButton(this, wxHELP, "Help", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 1);
		sz4->AddStretchSpacer();
		sz4->Add(new wxButton(this, ID_btn_open_folder, "Open folder", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 1);
		sz4->Add(new wxButton(this, ID_btn_open_code, "Open code", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 1);
		sz4->Add(new wxButton(this, wxCANCEL, "Close", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 1);



		wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
		sizer->Add(m_txtctrl, 1, wxALL | wxEXPAND, 10);
		sizer->Add(sz4, 0, wxALL | wxEXPAND, 5);
		SetSizerAndFit(sizer);
	}

	~CodeOpen_Dialog()
	{
	}


	void OnOpenFolder(wxCommandEvent &)
	{
		wxLaunchDefaultApplication(m_filename.GetPath());
	}

	void OnOpenCode(wxCommandEvent &)
	{
		// default associated program
		wxLaunchDefaultApplication(m_filename.GetFullPath());
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

BEGIN_EVENT_TABLE(CodeOpen_Dialog, wxDialog)
EVT_BUTTON(ID_btn_open_folder, CodeOpen_Dialog::OnOpenFolder)
EVT_BUTTON(ID_btn_open_code, CodeOpen_Dialog::OnOpenCode)
EVT_BUTTON(wxHELP, CodeOpen_Dialog::OnHelp)
EVT_BUTTON(wxCANCEL, CodeOpen_Dialog::OnCancel)
END_EVENT_TABLE()

*/


// for file and language prompting

enum {
	ID_btn_select_folder,
	ID_btn_generate,
	ID_txt_code_folder,
	ID_choice_language,
	ID_check_csvfiles,
//	ID_choice_array_matrix_threshold
};

class CodeGen_Dialog : public wxDialog
{
private:
	Case *m_case;
	ConfigInfo *m_ci;
	CaseWindow *m_caseWin;
	wxExtTextCtrl *txt_code_folder;
	wxChoice *choice_language;
//	wxChoice *choice_array_matrix_threshold;
//	wxCheckBox *chk_csvfiles;
	wxString m_foldername;
//	wxFileName m_wxfilename;


public:
	CodeGen_Dialog(wxWindow *parent, int id, CaseWindow *cwin)
		: wxDialog(parent, id, "Code Generator", wxDefaultPosition, wxScaleSize(600, 350),
		wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
	{
		m_caseWin = cwin;

		if (!m_caseWin) return;

		m_case = cwin->GetCase();
		m_ci = m_case->GetConfiguration();


		// initialize property
//		m_foldername = m_case->GetProperty("CodeGeneratorFolder");
		m_foldername = SamApp::Settings().Read("CodeGeneratorFolder");
		if (m_foldername.IsEmpty()) m_foldername = ::wxGetHomeDir();

		txt_code_folder = new wxExtTextCtrl(this, ID_txt_code_folder, m_foldername);
 
		wxArrayString data_languages;
		// ids or just index values from here
		data_languages.Add("lk");
		data_languages.Add("c");
		data_languages.Add("c#");
		choice_language = new wxChoice(this, ID_choice_language, wxDefaultPosition, wxDefaultSize, data_languages);
	
//		int lang = 0;
//		wxString str_lang = m_case->GetProperty("CodeGeneratorLanguage");
//		wxString str_lang = SamApp::Settings().Read("CodeGeneratorLanguage");
//		if (str_lang.IsNumber())
//			lang = wxAtoi(str_lang);

		int lang = (int)SamApp::Settings().ReadLong("CodeGeneratorLanguage",0);
		if (lang < 0) lang = 0;
		if (lang > (data_languages.Count() - 1)) lang = data_languages.Count() - 1;
		choice_language->SetSelection(lang);

		/*
		wxArrayString data_threshold;
		// ids or just index values from here
		data_threshold.Add("no separate files");
		data_threshold.Add("all in separate files");
		data_threshold.Add(">288 elements (diurnal)");
		data_threshold.Add(">20 elements (typical analysis period)");
		choice_array_matrix_threshold = new wxChoice(this, ID_choice_language, wxDefaultPosition, wxDefaultSize, data_threshold);
		choice_array_matrix_threshold->SetSelection(2); // default 288
		
		chk_csvfiles = new wxCheckBox(this, ID_check_csvfiles, "csv files for large arrays and matrices");
		chk_csvfiles->SetValue((m_case->GetProperty("CodeGeneratorCSVFiles") != "NO"));
		*/

		wxBoxSizer *sz1 = new wxBoxSizer(wxHORIZONTAL);
		sz1->Add(new wxStaticText(this, wxID_ANY, "Specify output folder:"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 4);
		sz1->Add(txt_code_folder, 1, wxALL | wxALIGN_CENTER_VERTICAL, 4);
		sz1->Add(new wxButton(this, ID_btn_select_folder, "...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxALIGN_CENTER_VERTICAL, 4);

		wxBoxSizer *sz2 = new wxBoxSizer(wxHORIZONTAL);
		sz2->Add(new wxStaticText(this, wxID_ANY, "Select code language:"), 0, wxALL | wxEXPAND, 4);
		sz2->Add(choice_language, 0, wxALL | wxEXPAND, 4);

//		wxBoxSizer *sz3 = new wxBoxSizer(wxHORIZONTAL);
//		sz3->Add(new wxStaticText(this, wxID_ANY, "Separate files for arrays and matrices:"), 0, wxALL | wxEXPAND, 4);
//		sz3->Add(choice_array_matrix_threshold, 0, wxALL | wxEXPAND, 4);
//		sz3->Add(chk_csvfiles, 0, wxALL | wxEXPAND, 4);


		wxBoxSizer *sz4 = new wxBoxSizer(wxHORIZONTAL);
		sz4->Add(new wxButton(this, wxHELP, "Help", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 1);
		sz4->AddStretchSpacer();
		sz4->Add(new wxButton(this, ID_btn_generate, "Generate code", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 1);
		sz4->Add(new wxButton(this, wxCANCEL, "Close", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 1);

	
		wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
		sizer->Add(sz1, 0, wxALL | wxEXPAND, 5);
		sizer->Add(sz2, 1, wxALL | wxEXPAND, 5);
//		sizer->Add(sz3, 1, wxALL | wxEXPAND, 5);
		sizer->Add(sz4, 0, wxALL | wxEXPAND, 5);
		SetSizerAndFit(sizer);
	}

	~CodeGen_Dialog()
	{
	}


	int GetThreshold()
	{
		int threshold = 288; // everything with more than 288 elements written to csv file.
		/*
		int threshold = 10000; // > 10,000 always written out
		if (chk_csvfiles->IsChecked())
			threshold = 288;

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
		*/
		return threshold;
	}

	void OnCodeFolder(wxCommandEvent &evt)
	{
		if (!m_case) return;

		wxDirDialog dlg(this, "Select an output folder", m_foldername, wxDD_DEFAULT_STYLE | wxDD_DIR_MUST_EXIST);
		if (dlg.ShowModal() == wxID_OK)
		{
			m_foldername = dlg.GetPath();
			m_foldername.Replace("\\", "/");
			txt_code_folder->SetValue(m_foldername);
		}
	}

	void OnGenerate(wxCommandEvent &)
	{
		// create appropriate language class with case passed to constructor
		// run GenerateCode function
		int code = choice_language->GetSelection();
		m_foldername = txt_code_folder->GetValue();
		m_foldername.Replace("\\", "/");
		txt_code_folder->SetValue(m_foldername);
		if (!wxDirExists(m_foldername))
		{
			wxMessageBox(wxString::Format("Error: the path '%s' does not exist", (const char*)m_foldername.c_str()), "Path Error", wxICON_ERROR);
			return;
		}
		int threshold = GetThreshold();

		// from wxWiki to convert wxString to char*
		wxString fn = SamApp::Project().GetCaseName(m_case);
		// replace spaces for SDK user friendly name
		fn.Replace(" ", "_");
		char cfn[100];
		strcpy(cfn, (const char*)fn.mb_str(wxConvUTF8));
		fn = m_foldername + "/" + wxString::FromAscii(cfn);

		if (code == 0) // lk
		{
			fn += ".lk";
			if (FILE *fp = fopen(fn.c_str(), "w"))
			{
				CodeGen_lk *cg = new CodeGen_lk(m_case, m_foldername);
				cg->GenerateCode(fp, threshold);
				fclose(fp);
				if (!cg->Ok())	
					wxMessageBox(cg->GetErrors(), "Generate Errors", wxICON_ERROR);
				else
					ShowOpenDialog();
			}
		}
		else if (code == 1) // c
		{
			fn += ".c";
			if (FILE *fp = fopen(fn.c_str(), "w"))
			{
				CodeGen_c *cg = new CodeGen_c(m_case, m_foldername);
				cg->GenerateCode(fp, threshold);
				fclose(fp);
				if (!cg->Ok())
					wxMessageBox(cg->GetErrors(), "Generate Errors", wxICON_ERROR);
				else
					ShowOpenDialog();
			}
		}
		else if (code == 2) // c#
		{
			fn += ".cs";
			if (FILE *fp = fopen(fn.c_str(), "w"))
			{
				CodeGen_csharp *cg = new CodeGen_csharp(m_case, m_foldername);
				cg->GenerateCode(fp, threshold);
				fclose(fp);
				if (!cg->Ok())
					wxMessageBox(cg->GetErrors(), "Generate Errors", wxICON_ERROR);
				else
					ShowOpenDialog();
			}
		}
	}

	void ShowOpenDialog()
	{
//		wxString message = "Code generation successful!\n\nClick 'OK' to open folder containing all files generated.\n\nClick 'Cancel' to return to the code generator dialog.";
//		m_case->SetProperty("CodeGeneratorFolder", m_foldername);
		SamApp::Settings().Write("CodeGeneratorFolder", m_foldername);
//		wxString csvfile = "YES";
//		if (!chk_csvfiles->GetValue()) csvfile = "NO";
//		m_case->SetProperty("CodeGeneratorCSVFiles", csvfile);
//		m_case->SetProperty("CodeGeneratorLanguage", wxString::Format("%d", choice_language->GetSelection()));
		SamApp::Settings().Write("CodeGeneratorLanguage", choice_language->GetSelection());
		Close();
		wxString message = "Open folder containing all files generated?";
		if (wxYES == wxMessageBox(message, "Code Generator Success", wxYES | wxNO))
		{
			wxLaunchDefaultApplication(m_foldername);
		}
	}

	/*
	void OnOpenFolder(wxCommandEvent &)
	{
		// run GenerateCode function
		int code = choice_language->GetSelection();
		// 
		wxString fn = txt_code_folder->GetValue();
		wxLaunchDefaultApplication(fn);
	}
	*/
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
//EVT_BUTTON(ID_btn_open_folder, CodeGen_Dialog::OnOpenFolder)
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



static bool SSCTypeToSSC( int ssc_type, ssc_data_t pdata, const wxString &sscname )
{
	switch (ssc_type)
	{
	case SSC_NUMBER:
	{
		ssc_number_t x = 0.0;
		ssc_data_set_number(pdata, sscname.c_str(), x);
	}
		break;
	case SSC_ARRAY:
	{
		size_t n=2;
		ssc_number_t p[2] = { 0.0, 0.0 };
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
	case SSC_MATRIX:
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
	case SSC_STRING:
		ssc_data_set_string( pdata, sscname.c_str(), "name" );
		break;

	default:
		return false;
	}

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
			int ssc_data_type = ssc_info_data_type(p_inf); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX		
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
				if (existing_type != ssc_data_type)
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
//					else if (reqd == "*")
//						m_errors.Add("SSC requires input '" + name + "', but was not found in the SAM UI or from previous simulations");
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

				int existing_type = ssc_data_query(p_data, ssc_info_name(p_inf));
				if (existing_type != ssc_data_type)
				{
					if (!SSCTypeToSSC(ssc_data_type, p_data_output, name))
						m_errors.Add("Error for output " + name);
				}
			}
		}
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
	CodeGen_Dialog dialog(SamApp::Window(), wxID_ANY, cw);
	dialog.CenterOnParent();
	if (wxID_OK == dialog.ShowModal())
		return true;
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
			fprintf(fp, "	set_array( data, \"%s\", \"%s\", %d);\n", name, (const char*)fn.c_str(), len);
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
			fprintf(fp, "	set_matrix( data, \"%s\", \"%s\", %d, %d);\n", name, (const char*)fn.c_str(), nr, nc);
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
	fprintf(fp, "#include <string.h>\n");
	fprintf(fp, "#include <stdlib.h>\n");
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
	// arrays
	fprintf(fp, "bool set_array(ssc_data_t p_data, const char *name, const char* fn, int len)\n");
	fprintf(fp, "{\n");
	fprintf(fp, "	char buffer[1024];\n");
	fprintf(fp, "	char *record, *line;\n");
	fprintf(fp, "	int i = 0;\n");
	fprintf(fp, "	ssc_number_t *ary;\n");
	fprintf(fp, "	FILE *fp = fopen(fn, \"r\");\n");
	fprintf(fp, "	if (fp == NULL)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		printf(\"file opening failed \");\n");
	fprintf(fp, "		return false;\n");
	fprintf(fp, "	}\n");
	fprintf(fp, "	ary = (ssc_number_t *)malloc(len * sizeof(ssc_number_t));\n");
	fprintf(fp, "	if (fp == NULL)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		printf(\"file opening failed \");\n");
	fprintf(fp, "		return false;\n");
	fprintf(fp, "	}\n");
	fprintf(fp, "	while ((line = fgets(buffer, sizeof(buffer), fp)) != NULL)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		record = strtok(line, \",\");\n");
	fprintf(fp, "		while ((record != NULL) && (i < len))\n");
	fprintf(fp, "		{\n");
	fprintf(fp, "			ary[i] = atof(record);\n");
	fprintf(fp, "			record = strtok(NULL, \",\");\n");
	fprintf(fp, "			i++;\n");
	fprintf(fp, "		}\n");
	fprintf(fp, "	}\n");
	fprintf(fp, "	fclose(fp);\n");
	fprintf(fp, "	ssc_data_set_array(p_data, name, ary, len);\n");
	fprintf(fp, "	free(ary);\n");
	fprintf(fp, "	return true;\n");
	fprintf(fp, "}\n");
	fprintf(fp, "\n");

	// matrices
	fprintf(fp, "bool set_matrix(ssc_data_t p_data, const char *name, const char* fn, int nr, int nc)\n");
	fprintf(fp, "{\n");
	fprintf(fp, "	char buffer[1024];\n");
	fprintf(fp, "	char *record, *line;\n");
	fprintf(fp, "	ssc_number_t *ary;\n");
	fprintf(fp, "	int i = 0, len = nr*nc;\n");
	fprintf(fp, "	FILE *fp = fopen(fn, \"r\");\n");
	fprintf(fp, "	if (fp == NULL)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		printf(\"file opening failed \");\n");
	fprintf(fp, "		return false;\n");
	fprintf(fp, "	}\n");
	fprintf(fp, "	ary = (ssc_number_t *)malloc(len * sizeof(ssc_number_t));\n");
	fprintf(fp, "	if (fp == NULL)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		printf(\"file opening failed \");\n");
	fprintf(fp, "		return false;\n");
	fprintf(fp, "	}\n");
	fprintf(fp, "	while ((line = fgets(buffer, sizeof(buffer), fp)) != NULL)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		record = strtok(line, \",\");\n");
	fprintf(fp, "		while ((record != NULL) && (i < len))\n");
	fprintf(fp, "		{\n");
	fprintf(fp, "			ary[i] = atof(record);\n");
	fprintf(fp, "			record = strtok(NULL, \",\");\n");
	fprintf(fp, "			i++;\n");
	fprintf(fp, "		}\n");
	fprintf(fp, "	}\n");
	fprintf(fp, "	fclose(fp);\n");
	fprintf(fp, "	ssc_data_set_matrix(p_data, name, ary, nr, nc);\n");
	fprintf(fp, "	free(ary);\n");
	fprintf(fp, "	return true;\n");
	fprintf(fp, "}\n");


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
	fprintf(fp, "	ssc_module_t module;\n");
	fprintf(fp, "\n");

	return true;
}

bool CodeGen_c::CreateSSCModule(FILE *fp, wxString &name)
{
	if (name.IsNull() || name.Length() < 1)
		return false;
	else
	{
		fprintf(fp, "	module = ssc_module_create(\"%s\"); \n", (const char*)name.c_str());
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


// c# code generation class

CodeGen_csharp::CodeGen_csharp(Case *cc, const wxString &folder) : CodeGen_Base(cc, folder)
{
}


bool CodeGen_csharp::Output(FILE *fp, ssc_data_t p_data)
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
			fprintf(fp, "		String %s = data.GetString( \"%s\" );\n", name, name);
			fprintf(fp, "		Console.WriteLine(\"{0} = {1}\"), %s, %s);\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_NUMBER:
			fprintf(fp, "		float %s = data.GetNumber(\"%s\");\n", name, name);
			fprintf(fp, "		Console.WriteLine(\"{0} = {1}\", \"%s\", %s);\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_ARRAY:
			p = ::ssc_data_get_array(p_data, name, &len);
			fprintf(fp, "		ssc_number_t p_%s[%d] ={", name, len);
			fprintf(fp, "		csvfile...( data, \"%s\", p_%s, %d );\n", name, name, len);
			break;
		case SSC_MATRIX:
			// TODO tables in future
			break;
		}
	}
	return true;
}

bool CodeGen_csharp::Input(FILE *fp, ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold)
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
		fprintf(fp, "		data.SetString( \"%s\", \"%s\" );\n", name, (const char*)str_value.c_str());
		break;
	case SSC_NUMBER:
		::ssc_data_get_number(p_data, name, &value);
		dbl_value = (double)value;
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(fp, "		data.SetNumber( \"%s\", %lgf );\n", name, dbl_value);
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
			fprintf(fp, "		data.SetArray( \"%s\", \"%s\", %d);\n", name, (const char*)fn.c_str(), len);
		}
		else
		{
			fprintf(fp, "		float[] p_%s ={", name, len);
			for (int i = 0; i < (len - 1); i++)
			{
				dbl_value = (double)p[i];
				if (dbl_value > 1e38) dbl_value = 1e38;
				fprintf(fp, " %lgf,", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(fp, " %lgf };\n", dbl_value);
			fprintf(fp, "		data.SetArray( \"%s\", p_%s);\n", name, name);
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
			fprintf(fp, "		data.SetMatrix( \"%s\", \"%s\", %d, %d);\n", name, (const char*)fn.c_str(), nr, nc);
		}
		else
		{
			fprintf(fp, "		float[,] p_%s ={ {", name, len);
			for (int k = 0; k < (len - 1); k++)
			{
				dbl_value = (double)p[k];
				if (dbl_value > 1e38) dbl_value = 1e38;
				if ((k > 0) && (k%nc == 0))
					fprintf(fp, " { %lgf,", dbl_value);
				else if (k%nc == (nc - 1))
					fprintf(fp, " %lgf },", dbl_value);
				else 
					fprintf(fp, " %lgf, ", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(fp, " %lgf } };\n", dbl_value);
			fprintf(fp, "		data.SetMatrix( \"%s\", p_%s );\n", name, name);
		}
		// TODO tables in future
	}
	return true;
}


bool CodeGen_csharp::RunSSCModule(FILE *fp, wxString &name)
{
	fprintf(fp, "		if (!module.Exec(data))\n");
	fprintf(fp, "		{\n");
	fprintf(fp, "			int idx = 0;\n");
	fprintf(fp, "			String msg;\n");
	fprintf(fp, "			int type;\n");
	fprintf(fp, "			float time;\n");
	fprintf(fp, "			while (module.Log(idx, out msg, out type, out time))\n");
	fprintf(fp, "			{\n");
	fprintf(fp, "				String stype = \"NOTICE\";\n");
	fprintf(fp, "				if (type == SSC.API.WARNING) stype = \"WARNING\";\n");
	fprintf(fp, "				else if (type == SSC.API.ERROR) stype = \"ERROR\";\n");
	fprintf(fp, "				Console.WriteLine(\"[\" + stype + \" at time : \" + time + \"]: \" + msg \);\n");
	fprintf(fp, "				idx++;\n");
	fprintf(fp, "			}\n");
	fprintf(fp, "			return;\n");
	fprintf(fp, "		}\n");
	return true;
}


bool CodeGen_csharp::Header(FILE *fp)
{
	// top of file and supporting functions
	fprintf(fp, "using System;\n");
	fprintf(fp, "using System.IO;\n");
	fprintf(fp, "using System.Collections.Generic;\n");
	fprintf(fp, "using System.Linq;\n");
	fprintf(fp, "using System.Text;\n");
	fprintf(fp, "using System.Threading.Tasks;\n");
	fprintf(fp, "using System.Runtime.InteropServices;\n");
	fprintf(fp, "namespace SSC\n");
	fprintf(fp, "{\n");
	fprintf(fp, "    class sscapi\n");
	fprintf(fp, "    {\n");
	fprintf(fp, "        static sscapi()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_version\")]\n");
	fprintf(fp, "        public static extern int ssc_version32();\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_version\")]\n");
	fprintf(fp, "        public static extern int ssc_version64();\n");
	fprintf(fp, "        public static int ssc_version()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_version64() : ssc_version32();\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_build_info\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_build_info32();\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_build_info\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_build_info64();\n");
	fprintf(fp, "        public static IntPtr ssc_build_info()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_build_info64() : ssc_build_info32();\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_create\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_data_create32();\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_create\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_data_create64();\n");
	fprintf(fp, "        public static IntPtr ssc_data_create()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_data_create64() : ssc_data_create32();\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_free\")]\n");
	fprintf(fp, "        public static extern void ssc_data_free32(HandleRef cxtData);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_free\")]\n");
	fprintf(fp, "        public static extern void ssc_data_free64(HandleRef cxtData);\n");
	fprintf(fp, "        public static void ssc_data_free(HandleRef cxtData)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (System.IntPtr.Size == 8) \n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_free64(cxtData);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_free32(cxtData);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_clear\")]\n");
	fprintf(fp, "        public static extern void ssc_data_clear32(HandleRef cxtData);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_clear\")]\n");
	fprintf(fp, "        public static extern void ssc_data_clear64(HandleRef cxtData);\n");
	fprintf(fp, "        public static void ssc_data_clear(HandleRef cxtData)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_clear64(cxtData);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_clear32(cxtData);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_unassign\")]\n");
	fprintf(fp, "        public static extern void ssc_data_unassign32(HandleRef cxtData, string variableName);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_unassign\")]\n");
	fprintf(fp, "        public static extern void ssc_data_unassign64(HandleRef cxtData, string variableName);\n");
	fprintf(fp, "        public static void ssc_data_unassign(HandleRef cxtData, string variableName)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_unassign64(cxtData, variableName);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_unassign32(cxtData, variableName);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_query\")]\n");
	fprintf(fp, "        public static extern int ssc_data_query32(HandleRef cxtData, string variableName);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_query\")]\n");
	fprintf(fp, "        public static extern int ssc_data_query64(HandleRef cxtData, string variableName);\n");
	fprintf(fp, "        public static int ssc_data_query(HandleRef cxtData, string variableName)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_data_query64(cxtData, variableName) : ssc_data_query32(cxtData, variableName);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_first\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_data_first32(HandleRef cxtData);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_first\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_data_first64(HandleRef cxtData);\n");
	fprintf(fp, "        public static IntPtr ssc_data_first(HandleRef cxtData)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_data_first64(cxtData) : ssc_data_first32(cxtData);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_next\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_data_next32(HandleRef cxtData);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_next\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_data_next64(HandleRef cxtData);\n");
	fprintf(fp, "        public static IntPtr ssc_data_next(HandleRef cxtData)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_data_next64(cxtData) : ssc_data_next32(cxtData);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_string\")]\n");
	fprintf(fp, "        public static extern void ssc_data_set_string32(HandleRef cxtData, string name, string value);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_string\")]\n");
	fprintf(fp, "        public static extern void ssc_data_set_string64(HandleRef cxtData, string name, string value);\n");
	fprintf(fp, "        public static void ssc_data_set_string(HandleRef cxtData, string name, string value)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_set_string64(cxtData, name, value);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_set_string32(cxtData, name, value);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_number\")]\n");
	fprintf(fp, "        public static extern void ssc_data_set_number32(HandleRef cxtData, string name, float value);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_number\")]\n");
	fprintf(fp, "        public static extern void ssc_data_set_number64(HandleRef cxtData, string name, float value);\n");
	fprintf(fp, "        public static void ssc_data_set_number(HandleRef cxtData, string name, float value)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_set_number64(cxtData, name, value);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_set_number32(cxtData, name, value);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_array\")]\n");
	fprintf(fp, "        public static extern void ssc_data_set_array32(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[] array, int length);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_array\")]\n");
	fprintf(fp, "        public static extern void ssc_data_set_array64(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[] array, int length);\n");
	fprintf(fp, "        public static void ssc_data_set_array(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[] array, int length)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_set_array64(cxtData, name, array, length);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_set_array32(cxtData, name, array, length);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_matrix\")]\n");
	fprintf(fp, "        public static extern void ssc_data_set_matrix32(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[,] matrix, int nRows, int nCols);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_matrix\")]\n");
	fprintf(fp, "        public static extern void ssc_data_set_matrix64(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[,] matrix, int nRows, int nCols);\n");
	fprintf(fp, "        public static void ssc_data_set_matrix(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[,] matrix, int nRows, int nCols)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_set_matrix64(cxtData, name, matrix, nRows, nCols);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_set_matrix32(cxtData, name, matrix, nRows, nCols);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_table\")]\n");
	fprintf(fp, "        public static extern void ssc_data_set_table32(HandleRef cxtData, string name, HandleRef cxtTable);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_table\")]\n");
	fprintf(fp, "        public static extern void ssc_data_set_table64(HandleRef cxtData, string name, HandleRef cxtTable);\n");
	fprintf(fp, "        public static void ssc_data_set_table(HandleRef cxtData, string name, HandleRef cxtTable)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_set_table64(cxtData, name, cxtTable);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_data_set_table32(cxtData, name, cxtTable);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_string\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_data_get_string32(HandleRef cxtData, string name);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_string\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_data_get_string64(HandleRef cxtData, string name);\n");
	fprintf(fp, "        public static IntPtr ssc_data_get_string(HandleRef cxtData, string name)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_data_get_string64(cxtData, name) : ssc_data_get_string32(cxtData, name);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_number\")]\n");
	fprintf(fp, "        public static extern int ssc_data_get_number32(HandleRef cxtData, string name, out float number);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_number\")]\n");
	fprintf(fp, "        public static extern int ssc_data_get_number64(HandleRef cxtData, string name, out float number);\n");
	fprintf(fp, "        public static int ssc_data_get_number(HandleRef cxtData, string name, out float number)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_data_get_number64(cxtData, name, out number) : ssc_data_get_number32(cxtData, name, out number);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_array\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_data_get_array32(HandleRef cxtData, string name, out int len);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_array\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_data_get_array64(HandleRef cxtData, string name, out int len);\n");
	fprintf(fp, "        public static IntPtr ssc_data_get_array(HandleRef cxtData, string name, out int len)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_data_get_array64(cxtData, name, out len) : ssc_data_get_array32(cxtData, name, out len);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_matrix\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_data_get_matrix32(HandleRef cxtData, string name, out int nRows, out int nCols);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_matrix\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_data_get_matrix64(HandleRef cxtData, string name, out int nRows, out int nCols);\n");
	fprintf(fp, "        public static IntPtr ssc_data_get_matrix(HandleRef cxtData, string name, out int nRows, out int nCols)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_data_get_matrix64(cxtData, name, out nRows, out nCols) : ssc_data_get_matrix32(cxtData, name, out nRows, out nCols);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_table\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_data_get_table32(HandleRef cxtData, string name);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_table\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_data_get_table64(HandleRef cxtData, string name);\n");
	fprintf(fp, "        public static IntPtr ssc_data_get_table(HandleRef cxtData, string name)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_data_get_table64(cxtData, name) : ssc_data_get_table32(cxtData, name);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_entry\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_module_entry32(int moduleIndex);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_entry\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_module_entry64(int moduleIndex);\n");
	fprintf(fp, "        public static IntPtr ssc_module_entry(int moduleIndex)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_module_entry64(moduleIndex) : ssc_module_entry32(moduleIndex);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_entry_name\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_entry_name32(HandleRef cxtEntry);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_entry_name\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_entry_name64(HandleRef cxtEntry);\n");
	fprintf(fp, "        public static IntPtr ssc_entry_name(HandleRef cxtEntry)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_entry_name64(cxtEntry) : ssc_entry_name32(cxtEntry);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_entry_description\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_entry_description32(HandleRef cxtEntry);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_entry_description\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_entry_description64(HandleRef cxtEntry);\n");
	fprintf(fp, "        public static IntPtr ssc_entry_description(HandleRef cxtEntry)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_entry_description64(cxtEntry) : ssc_entry_description32(cxtEntry);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_entry_version\")]\n");
	fprintf(fp, "        public static extern int ssc_entry_version32(HandleRef cxtEntry);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_entry_version\")]\n");
	fprintf(fp, "        public static extern int ssc_entry_version64(HandleRef cxtEntry);\n");
	fprintf(fp, "        public static int ssc_entry_version(HandleRef cxtEntry)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_entry_version64(cxtEntry) : ssc_entry_version32(cxtEntry);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_create\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_module_create32(string moduleName);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_create\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_module_create64(string moduleName);\n");
	fprintf(fp, "        public static IntPtr ssc_module_create(string moduleName)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_module_create64(moduleName) : ssc_module_create32(moduleName);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_free\")]\n");
	fprintf(fp, "        public static extern void ssc_module_free32(HandleRef cxtModule);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_free\")]\n");
	fprintf(fp, "        public static extern void ssc_module_free64(HandleRef cxtModule);\n");
	fprintf(fp, "        public static void ssc_module_free(HandleRef cxtModule)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_module_free64(cxtModule);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                ssc_module_free32(cxtModule);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_var_info\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_module_var_info32(HandleRef cxtModule, int index);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_var_info\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_module_var_info64(HandleRef cxtModule, int index);\n");
	fprintf(fp, "        public static IntPtr ssc_module_var_info(HandleRef cxtModule, int index)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_module_var_info64(cxtModule, index) : ssc_module_var_info32(cxtModule, index);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_var_type\")]\n");
	fprintf(fp, "        public static extern int ssc_info_var_type32(HandleRef cxtInfo);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_var_type\")]\n");
	fprintf(fp, "        public static extern int ssc_info_var_type64(HandleRef cxtInfo);\n");
	fprintf(fp, "        public static int ssc_info_var_type(HandleRef cxtInfo)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_info_var_type64(cxtInfo) : ssc_info_var_type32(cxtInfo);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_data_type\")]\n");
	fprintf(fp, "        public static extern int ssc_info_data_type32(HandleRef cxtInfo);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_data_type\")]\n");
	fprintf(fp, "        public static extern int ssc_info_data_type64(HandleRef cxtInfo);\n");
	fprintf(fp, "        public static int ssc_info_data_type(HandleRef cxtInfo)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_info_data_type64(cxtInfo) : ssc_info_data_type32(cxtInfo);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_name\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_name32(HandleRef cxtInfo);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_name\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_name64(HandleRef cxtInfo);\n");
	fprintf(fp, "        public static IntPtr ssc_info_name(HandleRef cxtInfo)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_info_name64(cxtInfo) : ssc_info_name32(cxtInfo);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_label\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_label32(HandleRef cxtInfo);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_label\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_label64(HandleRef cxtInfo);\n");
	fprintf(fp, "        public static IntPtr ssc_info_label(HandleRef cxtInfo)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_info_label64(cxtInfo) : ssc_info_label32(cxtInfo);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_units\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_units32(HandleRef cxtInfo);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_units\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_units64(HandleRef cxtInfo);\n");
	fprintf(fp, "        public static IntPtr ssc_info_units(HandleRef cxtInfo)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_info_units64(cxtInfo) : ssc_info_units32(cxtInfo);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_meta\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_meta32(HandleRef cxtInfo);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_meta\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_meta64(HandleRef cxtInfo);\n");
	fprintf(fp, "        public static IntPtr ssc_info_meta(HandleRef cxtInfo)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_info_meta64(cxtInfo) : ssc_info_meta32(cxtInfo);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_group\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_group32(HandleRef cxtInfo);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_group\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_group64(HandleRef cxtInfo);\n");
	fprintf(fp, "        public static IntPtr ssc_info_group(HandleRef cxtInfo)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_info_group64(cxtInfo) : ssc_info_group32(cxtInfo);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_required\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_required32(HandleRef cxtInfo);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_required\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_required64(HandleRef cxtInfo);\n");
	fprintf(fp, "        public static IntPtr ssc_info_required(HandleRef cxtInfo)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_info_required64(cxtInfo) : ssc_info_required32(cxtInfo);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_constraints\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_constraints32(HandleRef cxtInfo);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_constraints\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_constraints64(HandleRef cxtInfo);\n");
	fprintf(fp, "        public static IntPtr ssc_info_constraints(HandleRef cxtInfo)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_info_constraints64(cxtInfo) : ssc_info_constraints32(cxtInfo);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_uihint\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_uihint32(HandleRef cxtInfo);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_uihint\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_info_uihint64(HandleRef cxtInfo);\n");
	fprintf(fp, "        public static IntPtr ssc_info_uihint(HandleRef cxtInfo)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_info_uihint64(cxtInfo) : ssc_info_units32(cxtInfo);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec_simple\")]\n");
	fprintf(fp, "        public static extern int ssc_module_exec_simple32(string moduleName, HandleRef cxtData);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec_simple\")]\n");
	fprintf(fp, "        public static extern int ssc_module_exec_simple64(string moduleName, HandleRef cxtData);\n");
	fprintf(fp, "        public static int ssc_module_exec_simple(string moduleName, HandleRef cxtData)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_module_exec_simple64(moduleName, cxtData) : ssc_module_exec_simple32(moduleName, cxtData);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec_simple_nothread\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_module_exec_simple_nothread32(string moduleName, HandleRef cxtData);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec_simple_nothread\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_module_exec_simple_nothread64(string moduleName, HandleRef cxtData);\n");
	fprintf(fp, "        public static IntPtr ssc_module_exec_simple_nothread(string moduleName, HandleRef cxtData)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_module_exec_simple_nothread64(moduleName, cxtData) : ssc_module_exec_simple_nothread32(moduleName, cxtData);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "        \n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec\")]\n");
	fprintf(fp, "        public static extern int ssc_module_exec32(HandleRef cxtModule, HandleRef cxtData);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec\")]\n");
	fprintf(fp, "        public static extern int ssc_module_exec64(HandleRef cxtModule, HandleRef cxtData);\n");
	fprintf(fp, "        public static int ssc_module_exec(HandleRef cxtModule, HandleRef cxtData)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_module_exec64(cxtModule, cxtData) : ssc_module_exec32(cxtModule, cxtData);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec_with_handler\")]\n");
	fprintf(fp, "        public static extern int ssc_module_exec_with_handler32(HandleRef cxtModule, HandleRef cxtData, HandleRef cxtHandler, HandleRef cxtUserData);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec_with_handler\")]\n");
	fprintf(fp, "        public static extern int ssc_module_exec_with_handler64(HandleRef cxtModule, HandleRef cxtData, HandleRef cxtHandler, HandleRef cxtUserData);\n");
	fprintf(fp, "        public static int ssc_module_exec_with_handler(HandleRef cxtModule, HandleRef cxtData, HandleRef cxtHandler, HandleRef cxtUserData)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_module_exec_with_handler64(cxtModule, cxtData, cxtHandler, cxtUserData) : ssc_module_exec_with_handler32(cxtModule, cxtData, cxtHandler, cxtUserData);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_log\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_module_log32(HandleRef cxtModule, int index, out int messageType, out float time);\n");
	fprintf(fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_log\")]\n");
	fprintf(fp, "        public static extern IntPtr ssc_module_log64(HandleRef cxtModule, int index, out int messageType, out float time);\n");
	fprintf(fp, "        public static IntPtr ssc_module_log(HandleRef cxtModule, int index, out int messageType, out float time)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (System.IntPtr.Size == 8) ? ssc_module_log64(cxtModule, index, out messageType, out time) : ssc_module_log32(cxtModule, index, out messageType, out time);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "    }\n");
	fprintf(fp, "\n");
	fprintf(fp, "\n");
	fprintf(fp, "    public class Data\n");
	fprintf(fp, "    {\n");
	fprintf(fp, "        private HandleRef m_data;\n");
	fprintf(fp, "        private bool m_owned;\n");
	fprintf(fp, "\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public Data()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            m_data = new HandleRef(this, sscapi.ssc_data_create());\n");
	fprintf(fp, "            m_owned = true;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public Data( IntPtr dataRefNotOwned )\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            m_data = new HandleRef(this, dataRefNotOwned);\n");
	fprintf(fp, "            m_owned = false;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        ~Data()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (m_owned && m_data.Handle != IntPtr.Zero)\n");
	fprintf(fp, "                sscapi.ssc_data_free(m_data);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public void Clear()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            sscapi.ssc_data_clear(m_data);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public String First()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            IntPtr p = sscapi.ssc_data_first(m_data);\n");
	fprintf(fp, "            if (p != IntPtr.Zero)\n");
	fprintf(fp, "                return Marshal.PtrToStringAnsi(p);\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "                return null;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public String Next()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            IntPtr p = sscapi.ssc_data_next(m_data);\n");
	fprintf(fp, "            if (p != IntPtr.Zero)\n");
	fprintf(fp, "                return Marshal.PtrToStringAnsi(p);\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "                return null;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public int Query(String name)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return sscapi.ssc_data_query(m_data, name);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public void SetNumber(String name, float value)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            sscapi.ssc_data_set_number(m_data, name, value);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public float GetNumber(String name)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            float val = float.NaN;\n");
	fprintf(fp, "            sscapi.ssc_data_get_number(m_data, name, out val);\n");
	fprintf(fp, "            return val;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public void SetString(String name, String value)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            sscapi.ssc_data_set_string(m_data, name, value);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public String GetString(String name)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            IntPtr p = sscapi.ssc_data_get_string(m_data, name);\n");
	fprintf(fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public void SetArray(String name, float[] data)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            sscapi.ssc_data_set_array(m_data, name, data, data.Length);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public void SetArray(String name, String fn, int len)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            StreamReader sr = new StreamReader(fn);\n");
	fprintf(fp, "            int Row = 0;\n");
	fprintf(fp, "            float[] data = new float[len];\n");
	fprintf(fp, "            while (!sr.EndOfStream && Row < len)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "				string[] Line = sr.ReadLine().Split(',');\n");
	fprintf(fp, "				data[Row] = float.Parse(Line[0]);\n");
	fprintf(fp, "				Row++;\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            sscapi.ssc_data_set_array(m_data, name, data, len);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public float[] GetArray(String name)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            int len;\n");
	fprintf(fp, "            IntPtr res = sscapi.ssc_data_get_array(m_data, name, out len);\n");
	fprintf(fp, "            float[] arr = null;\n");
	fprintf(fp, "            if (len > 0)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                arr = new float[len];\n");
	fprintf(fp, "                Marshal.Copy(res, arr, 0, len);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            return arr;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public void SetMatrix(String name, float[,] mat)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            int nRows = mat.GetLength(0);\n");
	fprintf(fp, "            int nCols = mat.GetLength(1);\n");
	fprintf(fp, "            sscapi.ssc_data_set_matrix(m_data, name, mat, nRows, nCols);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public void SetMatrix(String name, String fn, int nr, int nc)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            StreamReader sr = new StreamReader(fn);\n");
	fprintf(fp, "            int Row = 0;\n");
	fprintf(fp, "            float[,] mat = new float[nr, nc];\n");
	fprintf(fp, "            while (!sr.EndOfStream && Row < nr)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "				string[] Line = sr.ReadLine().Split(',');\n");
	fprintf(fp, "				for (int ic = 0; ic < Line.Length && ic < nc; ic++)\n");
	fprintf(fp, "					mat[Row, ic] = float.Parse(Line[ic]);\n");
	fprintf(fp, "				Row++;\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            sscapi.ssc_data_set_matrix(m_data, name, mat, nr, nc);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public float[,] GetMatrix(String name)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            int nRows, nCols;\n");
	fprintf(fp, "            IntPtr res = sscapi.ssc_data_get_matrix(m_data, name, out nRows, out nCols);\n");
	fprintf(fp, "            if (nRows * nCols > 0)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                float[] sscMat = new float[nRows * nCols];\n");
	fprintf(fp, "                Marshal.Copy(res, sscMat, 0, nRows * nCols);\n");
	fprintf(fp, "                float[,] mat = new float[nRows, nCols];\n");
	fprintf(fp, "                for (int i = 0; i < nRows; i++)\n");
	fprintf(fp, "                {\n");
	fprintf(fp, "                    for (int j = 0; j < nCols; j++)\n");
	fprintf(fp, "                    {\n");
	fprintf(fp, "                        mat[i, j] = sscMat[i * nCols + j];\n");
	fprintf(fp, "                    }\n");
	fprintf(fp, "                }\n");
	fprintf(fp, "                return mat;\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "                return null;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public void SetTable(String name, Data table)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            sscapi.ssc_data_set_table(m_data, name, table.GetDataHandle());\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public Data GetTable(String name)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            IntPtr p = sscapi.ssc_data_get_table(m_data, name);\n");
	fprintf(fp, "            if (IntPtr.Zero == p)\n");
	fprintf(fp, "                return null;\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "                return new Data( p );\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public HandleRef GetDataHandle()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return m_data;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "    }\n");
	fprintf(fp, "\n");
	fprintf(fp, "\n");
	fprintf(fp, "    public class Module\n");
	fprintf(fp, "    {\n");
	fprintf(fp, "        private HandleRef m_mod;\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public Module(String name)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            m_mod = new HandleRef(this, sscapi.ssc_module_create(name) );\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        ~Module()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (m_mod.Handle != IntPtr.Zero)\n");
	fprintf(fp, "                sscapi.ssc_module_free(m_mod);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public bool IsOk()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return m_mod.Handle != IntPtr.Zero;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public HandleRef GetModuleHandle()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return m_mod;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public bool Exec( Data data )\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return (sscapi.ssc_module_exec(m_mod, data.GetDataHandle()) != 0);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public bool Log(int idx, out String msg, out int type, out float time)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            msg = \"\";\n");
	fprintf(fp, "            IntPtr p = sscapi.ssc_module_log(m_mod, idx, out type, out time);\n");
	fprintf(fp, "            if (IntPtr.Zero != p)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                msg = Marshal.PtrToStringAnsi(p);\n");
	fprintf(fp, "                return true;\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "                return false;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "    }\n");
	fprintf(fp, "\n");
	fprintf(fp, "\n");
	fprintf(fp, "    public class Entry\n");
	fprintf(fp, "    {\n");
	fprintf(fp, "        private HandleRef m_entry;\n");
	fprintf(fp, "        private int m_idx;\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public Entry()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            m_idx = 0;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public void Reset()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            m_idx = 0;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public bool Get()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            IntPtr p = sscapi.ssc_module_entry(m_idx);\n");
	fprintf(fp, "            if (p == IntPtr.Zero)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                Reset();\n");
	fprintf(fp, "                return false;\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "\n");
	fprintf(fp, "            m_entry = new HandleRef(this, p);\n");
	fprintf(fp, "            m_idx++;\n");
	fprintf(fp, "            return true;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public String Name()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (m_entry.Handle != IntPtr.Zero)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                IntPtr p = sscapi.ssc_entry_name(m_entry);\n");
	fprintf(fp, "                return Marshal.PtrToStringAnsi(p);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            else return null;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public String Description()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (m_entry.Handle != IntPtr.Zero)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                IntPtr p = sscapi.ssc_entry_description(m_entry);\n");
	fprintf(fp, "                return Marshal.PtrToStringAnsi(p);\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "                return null;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public int Version()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (m_entry.Handle != IntPtr.Zero)\n");
	fprintf(fp, "                return sscapi.ssc_entry_version(m_entry);\n");
	fprintf(fp, "            else\n");
	fprintf(fp, "                return -1;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "    }\n");
	fprintf(fp, "\n");
	fprintf(fp, "    public class Info\n");
	fprintf(fp, "    {\n");
	fprintf(fp, "        private HandleRef m_inf;\n");
	fprintf(fp, "        private Module m_mod;\n");
	fprintf(fp, "        private int m_idx;\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public Info(Module m)\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            m_mod = m;\n");
	fprintf(fp, "            m_idx = 0;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public void Reset()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            m_idx = 0;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public bool Get()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            IntPtr p = sscapi.ssc_module_var_info(m_mod.GetModuleHandle(), m_idx);\n");
	fprintf(fp, "            if (p == IntPtr.Zero)\n");
	fprintf(fp, "            {\n");
	fprintf(fp, "                Reset();\n");
	fprintf(fp, "                return false;\n");
	fprintf(fp, "            }\n");
	fprintf(fp, "\n");
	fprintf(fp, "            m_inf = new HandleRef(this, p);\n");
	fprintf(fp, "            m_idx++;\n");
	fprintf(fp, "            return true;\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public String Name()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (m_inf.Handle == IntPtr.Zero) return null;\n");
	fprintf(fp, "            IntPtr p = sscapi.ssc_info_name(m_inf);\n");
	fprintf(fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public int VarType()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (m_inf.Handle == IntPtr.Zero) return -1;\n");
	fprintf(fp, "            return sscapi.ssc_info_var_type(m_inf);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public int DataType()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (m_inf.Handle == IntPtr.Zero) return -1;\n");
	fprintf(fp, "            return sscapi.ssc_info_data_type(m_inf);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public string Label()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (m_inf.Handle == IntPtr.Zero) return null;\n");
	fprintf(fp, "            IntPtr p = sscapi.ssc_info_label(m_inf);\n");
	fprintf(fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public string Units()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (m_inf.Handle == IntPtr.Zero) return null;\n");
	fprintf(fp, "            IntPtr p = sscapi.ssc_info_units(m_inf);\n");
	fprintf(fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public string Meta()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (m_inf.Handle == IntPtr.Zero) return null;\n");
	fprintf(fp, "            IntPtr p = sscapi.ssc_info_meta(m_inf);\n");
	fprintf(fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public string Group()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (m_inf.Handle == IntPtr.Zero) return null;\n");
	fprintf(fp, "            IntPtr p = sscapi.ssc_info_group(m_inf);\n");
	fprintf(fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public string Required()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (m_inf.Handle == IntPtr.Zero) return null;\n");
	fprintf(fp, "            IntPtr p = sscapi.ssc_info_required(m_inf);\n");
	fprintf(fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        public string Constraints()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            if (m_inf.Handle == IntPtr.Zero) return null;\n");
	fprintf(fp, "            IntPtr p = sscapi.ssc_info_constraints(m_inf);\n");
	fprintf(fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "    }\n");
	fprintf(fp, "\n");
	fprintf(fp, "    public class API\n");
	fprintf(fp, "    {\n");
	fprintf(fp, "        // constants for return value of Info.VarType() (see sscapi.h)\n");
	fprintf(fp, "        public const int INPUT = 1;\n");
	fprintf(fp, "        public const int OUTPUT = 2;\n");
	fprintf(fp, "        public const int INOUT = 3;\n");
	fprintf(fp, "\n");
	fprintf(fp, "\n");
	fprintf(fp, "        // constants for out integer type in Module.Log() method (see sscapi.h)\n");
	fprintf(fp, "        public const int NOTICE = 1;\n");
	fprintf(fp, "        public const int WARNING = 2;\n");
	fprintf(fp, "        public const int ERROR = 3;\n");
	fprintf(fp, "\n");
	fprintf(fp, "\n");
	fprintf(fp, "        // constants for return value of Data.Query() and Info.DataType() (see sscapi.h)\n");
	fprintf(fp, "        public const int INVALID = 0;\n");
	fprintf(fp, "        public const int STRING = 1;\n");
	fprintf(fp, "        public const int NUMBER = 2;\n");
	fprintf(fp, "        public const int ARRAY = 3;\n");
	fprintf(fp, "        public const int MATRIX = 4;\n");
	fprintf(fp, "        public const int TABLE = 5;\n");
	fprintf(fp, "\n");
	fprintf(fp, "        static public int Version()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            return sscapi.ssc_version();\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "\n");
	fprintf(fp, "        static public String BuildInfo()\n");
	fprintf(fp, "        {\n");
	fprintf(fp, "            IntPtr buildInfo = sscapi.ssc_build_info();\n");
	fprintf(fp, "            return Marshal.PtrToStringAnsi(buildInfo);\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "    }\n");
	fprintf(fp, "}\n");
	fprintf(fp, "\n");

	// csv reader


	fprintf(fp, "class SAM_Code\n");
	fprintf(fp, "{\n");
	fprintf(fp, "	static void Main() \n");
	fprintf(fp, "	{\n");

	// create global data container
	fprintf(fp, "		SSC.Data data = new SSC.Data();\n");
	fprintf(fp, "		if (data == null)\n");
	fprintf(fp, "		{\n");
	fprintf(fp, "			Console.WriteLine(\"error: out of memory.\");\n");
	fprintf(fp, "			return;\n");
	fprintf(fp, "		}\n");
	fprintf(fp, "		SSC.Module module;\n");
	fprintf(fp, "\n");

	return true;
}

bool CodeGen_csharp::CreateSSCModule(FILE *fp, wxString &name)
{
	if (name.IsNull() || name.Length() < 1)
		return false;
	else
	{
		fprintf(fp, "		module = new SSC.Module(\"%s\"); \n", (const char*)name.c_str());
		fprintf(fp, "		if (null == module)\n");
		fprintf(fp, "		{\n");
		fprintf(fp, "			Console.WriteLine(\"error: could not create '%s' module.\"); \n", (const char*)name.c_str());
		fprintf(fp, "			return; \n");
		fprintf(fp, "		}\n");
	}
	return true;
}

bool CodeGen_csharp::FreeSSCModule(FILE *fp)
{
// csharp cleans own garbage in SSC.api
	return true;
}

bool CodeGen_csharp::Footer(FILE *fp)
{
	fprintf(fp, "	}\n");
	fprintf(fp, "}\n");
	return true;
}




// MATLAB code generation class

CodeGen_matlab::CodeGen_matlab(Case *cc, const wxString &folder) : CodeGen_Base(cc, folder)
{
}


bool CodeGen_matlab::Output(FILE *fp, ssc_data_t p_data)
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

bool CodeGen_matlab::Input(FILE *fp, ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold)
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
			fprintf(fp, "	set_array( data, \"%s\", \"%s\", %d);\n", name, (const char*)fn.c_str(), len);
		}
		else
		{
			fprintf(fp, "	ssc_number_t p_%s[%d] ={", name, len);
			for (int i = 0; i < (len - 1); i++)
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
			fprintf(fp, "	set_matrix( data, \"%s\", \"%s\", %d, %d);\n", name, (const char*)fn.c_str(), nr, nc);
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


bool CodeGen_matlab::RunSSCModule(FILE *fp, wxString &name)
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


bool CodeGen_matlab::Header(FILE *fp)
{
	// top of file and supporting functions
	fprintf(fp, "#include <stdio.h>\n");
	fprintf(fp, "#include <string.h>\n");
	fprintf(fp, "#include <stdlib.h>\n");
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
	// arrays
	fprintf(fp, "bool set_array(ssc_data_t p_data, const char *name, const char* fn, int len)\n");
	fprintf(fp, "{\n");
	fprintf(fp, "	char buffer[1024];\n");
	fprintf(fp, "	char *record, *line;\n");
	fprintf(fp, "	int i = 0;\n");
	fprintf(fp, "	ssc_number_t *ary;\n");
	fprintf(fp, "	FILE *fp = fopen(fn, \"r\");\n");
	fprintf(fp, "	if (fp == NULL)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		printf(\"file opening failed \");\n");
	fprintf(fp, "		return false;\n");
	fprintf(fp, "	}\n");
	fprintf(fp, "	ary = (ssc_number_t *)malloc(len * sizeof(ssc_number_t));\n");
	fprintf(fp, "	if (fp == NULL)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		printf(\"file opening failed \");\n");
	fprintf(fp, "		return false;\n");
	fprintf(fp, "	}\n");
	fprintf(fp, "	while ((line = fgets(buffer, sizeof(buffer), fp)) != NULL)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		record = strtok(line, \",\");\n");
	fprintf(fp, "		while ((record != NULL) && (i < len))\n");
	fprintf(fp, "		{\n");
	fprintf(fp, "			ary[i] = atof(record);\n");
	fprintf(fp, "			record = strtok(NULL, \",\");\n");
	fprintf(fp, "			i++;\n");
	fprintf(fp, "		}\n");
	fprintf(fp, "	}\n");
	fprintf(fp, "	fclose(fp);\n");
	fprintf(fp, "	ssc_data_set_array(p_data, name, ary, len);\n");
	fprintf(fp, "	free(ary);\n");
	fprintf(fp, "	return true;\n");
	fprintf(fp, "}\n");
	fprintf(fp, "\n");

	// matrices
	fprintf(fp, "bool set_matrix(ssc_data_t p_data, const char *name, const char* fn, int nr, int nc)\n");
	fprintf(fp, "{\n");
	fprintf(fp, "	char buffer[1024];\n");
	fprintf(fp, "	char *record, *line;\n");
	fprintf(fp, "	ssc_number_t *ary;\n");
	fprintf(fp, "	int i = 0, len = nr*nc;\n");
	fprintf(fp, "	FILE *fp = fopen(fn, \"r\");\n");
	fprintf(fp, "	if (fp == NULL)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		printf(\"file opening failed \");\n");
	fprintf(fp, "		return false;\n");
	fprintf(fp, "	}\n");
	fprintf(fp, "	ary = (ssc_number_t *)malloc(len * sizeof(ssc_number_t));\n");
	fprintf(fp, "	if (fp == NULL)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		printf(\"file opening failed \");\n");
	fprintf(fp, "		return false;\n");
	fprintf(fp, "	}\n");
	fprintf(fp, "	while ((line = fgets(buffer, sizeof(buffer), fp)) != NULL)\n");
	fprintf(fp, "	{\n");
	fprintf(fp, "		record = strtok(line, \",\");\n");
	fprintf(fp, "		while ((record != NULL) && (i < len))\n");
	fprintf(fp, "		{\n");
	fprintf(fp, "			ary[i] = atof(record);\n");
	fprintf(fp, "			record = strtok(NULL, \",\");\n");
	fprintf(fp, "			i++;\n");
	fprintf(fp, "		}\n");
	fprintf(fp, "	}\n");
	fprintf(fp, "	fclose(fp);\n");
	fprintf(fp, "	ssc_data_set_matrix(p_data, name, ary, nr, nc);\n");
	fprintf(fp, "	free(ary);\n");
	fprintf(fp, "	return true;\n");
	fprintf(fp, "}\n");


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
	fprintf(fp, "	ssc_module_t module;\n");
	fprintf(fp, "\n");

	return true;
}

bool CodeGen_matlab::CreateSSCModule(FILE *fp, wxString &name)
{
	if (name.IsNull() || name.Length() < 1)
		return false;
	else
	{
		fprintf(fp, "	module = ssc_module_create(\"%s\"); \n", (const char*)name.c_str());
		fprintf(fp, "	if (NULL == module)\n");
		fprintf(fp, "	{\n");
		fprintf(fp, "		printf(\"error: could not create '%s' module.\"); \n", (const char*)name.c_str());
		fprintf(fp, "		ssc_data_free(data); \n");
		fprintf(fp, "		return -1; \n");
		fprintf(fp, "	}\n");
	}
	return true;
}

bool CodeGen_matlab::FreeSSCModule(FILE *fp)
{
	fprintf(fp, "	ssc_module_free(module);\n");
	return true;
}

bool CodeGen_matlab::Footer(FILE *fp)
{
	fprintf(fp, "	ssc_data_free(data);\n");
	fprintf(fp, "	return 0;\n");
	fprintf(fp, "}\n");
	return true;
}






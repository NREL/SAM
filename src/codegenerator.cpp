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
#include <wx/filename.h>

#include <wex/metro.h>
#include <wex/utils.h>

#include <lk/absyn.h>
#include <lk/stdlib.h>
#include <lk/eval.h>

#include <ssc/sscapi.h>

#include "variables.h"
#include "codegenerator.h"
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



CodeGen_Base::CodeGen_Base( Case *cc, const wxString &fullpath )
	: m_case( cc )
{
	m_fp = fopen(fullpath.c_str(), "w");
	wxFileName::SplitPath(fullpath, &m_folder, &m_name, NULL );
}

CodeGen_Base::~CodeGen_Base()
{
	if (m_fp) fclose(m_fp);
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


bool CodeGen_Base::GenerateCode(const int &array_matrix_threshold)
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
	if (!Header())
		m_errors.Add("Header failed");



	const char *name = ssc_data_first(p_data);
	while (name)
	{
		if (!Input(p_data, name, m_folder, array_matrix_threshold))
			m_errors.Add(wxString::Format("Input %s write failed",name));
		name = ssc_data_next(p_data);
	}

	// run compute modules in sequence (INOUT variables will be updated
	for (size_t kk = 0; kk < simlist.size(); kk++)
	{
		CreateSSCModule(simlist[kk]);
		RunSSCModule(simlist[kk]);
		FreeSSCModule();
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

	if (!Output(p_data_output))
		m_errors.Add("Output failed");

	if (!Footer())
		m_errors.Add("Footer failed");

	if (m_fp) fclose(m_fp);
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
	if (!cw || !(cw->GetCase())) return false;

	wxArrayString code_languages;
	// ids or just index values from here
	code_languages.Add("lk");
	code_languages.Add("c");
	code_languages.Add("c#");
	code_languages.Add("matlab");
	code_languages.Add("python");

	// initialize properties
	wxString foldername = SamApp::Settings().Read("CodeGeneratorFolder");
	if (foldername.IsEmpty()) foldername = ::wxGetHomeDir();

	int lang = (int)SamApp::Settings().ReadLong("CodeGeneratorLanguage", 0);
	if (lang < 0) lang = 0;
	if (lang >(code_languages.Count() - 1)) lang = code_languages.Count() - 1;

	// get language
	wxSingleChoiceDialog *scd_language = new wxSingleChoiceDialog(SamApp::Window(), "Select code language:", "Code language", code_languages);
	scd_language->SetSelection(lang);
	if (scd_language->ShowModal() == wxID_OK)
	{
		lang = scd_language->GetSelection();
		SamApp::Settings().Write("CodeGeneratorLanguage", lang);
	}
	else // user cancelled.
		return false;


	// get folder
	wxDirDialog dlg(SamApp::Window(), "Select an output folder", foldername, wxDD_DEFAULT_STYLE | wxDD_DIR_MUST_EXIST);
	if (dlg.ShowModal() == wxID_OK)
	{
		foldername = dlg.GetPath();
		foldername.Replace("\\", "/");
		SamApp::Settings().Write("CodeGeneratorFolder", foldername);
	}
	else // user cancelled.
		return false;

	// generate code
	int threshold = 288; // all arrays and matrices with more than 288 elements get written to csv file

	// from wxWiki to convert wxString to char*
	Case *c = cw->GetCase();
	wxString fn = SamApp::Project().GetCaseName(c);
	// replace spaces for SDK user friendly name
	fn.Replace(" ", "_");
	fn.Replace("(", "_"); // matlab
	fn.Replace(")", "_"); // matlab
	char cfn[100];
	strcpy(cfn, (const char*)fn.mb_str(wxConvUTF8));
	fn = foldername + "/" + wxString::FromAscii(cfn);

	CodeGen_Base *cg;
	wxString err_msg = "";
	if (lang == 0) // lk
	{
		fn += ".lk";
		cg = new CodeGen_lk(c, fn);
	}
	else if (lang == 1) // c
	{
		fn += ".c";
		cg = new CodeGen_c(c, fn);
	}
	else if (lang == 2) // c#
	{
		fn += ".cs";
		cg = new CodeGen_csharp(c, fn);
	}
	else if (lang == 3) // matlab
	{
		fn += ".m";
		cg = new CodeGen_matlab(c, fn);
	}
	else if (lang == 4) // python
	{
		fn += ".py";
		cg = new CodeGen_python(c, fn);
	}

	if (cg)
	{
		cg->GenerateCode(threshold);
		if (!cg->Ok())
			wxMessageBox(cg->GetErrors(), "Code Generator Errors", wxICON_ERROR);
		else
			wxLaunchDefaultApplication(foldername);
		return cg->Ok();
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


bool CodeGen_lk::Output(ssc_data_t p_data)
{
	for (size_t ii = 0; ii < m_data.size(); ii++)
		fprintf(m_fp, "outln('%s ' + var('%s'));\n", (const char*)m_data[ii].label.c_str(), (const char*)m_data[ii].var.c_str());
	return true;
}

bool CodeGen_lk::Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold)
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
		fprintf(m_fp, "var( '%s', '%s' );\n", name, (const char*)str_value.c_str());
		break;
	case SSC_NUMBER:
		::ssc_data_get_number(p_data, name, &value);
		dbl_value = (double)value;
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(m_fp, "var( '%s', %.17g );\n", name, dbl_value);
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
//				str_value = wxString::Format("%.17g", dbl_value);
				csv.Set(i, 0, wxString::Format("%.17g", dbl_value));
			}
			csv.WriteFile(fn);
//			fprintf(m_fp, "var( '%s', csvread('%s'));", name, (const char*)fn.c_str());
			fprintf(m_fp, "var( '%s', real_array(read_text_file('%s')));\n", name, (const char*)fn.c_str());
		}
		else
		{
			fprintf(m_fp, "var( '%s', [", name);
			for (int i = 0; i < (len - 1); i++)
			{
				dbl_value = (double)p[i];
				if (dbl_value > 1e38) dbl_value = 1e38;
				fprintf(m_fp, " %.17g,", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(m_fp, " %.17g ] );\n", dbl_value);
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
					csv.Set(r, c, wxString::Format("%.17g", dbl_value));
				}
			}
			csv.WriteFile(fn);
			fprintf(m_fp, "var( '%s', csvread('%s'));\n", name, (const char*)fn.c_str());
		}
		else
		{
			fprintf(m_fp, "var( '%s', \n[ [", name);
			for (int k = 0; k < (len - 1); k++)
			{
				dbl_value = (double)p[k];
				if (dbl_value > 1e38) dbl_value = 1e38;
				if ((k + 1) % nc == 0)
					fprintf(m_fp, " %.17g ], \n[", dbl_value);
				else
					fprintf(m_fp, " %.17g,", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(m_fp, " %.17g ] ] );\n", dbl_value);
		}
		// TODO tables in future
	}
	return true;
}


bool CodeGen_lk::RunSSCModule(wxString &name)
{
	if (name.IsNull() || name.Length() < 1)
		return false;
	else
		fprintf(m_fp, "run('%s');\n", (const char*)name.c_str());
	return true;
}


bool CodeGen_lk::Header()
{
	fprintf(m_fp, "clear();\n");
	return true;
}

bool CodeGen_lk::CreateSSCModule(wxString &name)
{
	return true;
}

bool CodeGen_lk::FreeSSCModule()
{
	return true;
}

bool CodeGen_lk::Footer()
{
	return true;
}


// c code generation class

CodeGen_c::CodeGen_c(Case *cc, const wxString &folder) : CodeGen_Base(cc, folder)
{
}


bool CodeGen_c::Output(ssc_data_t p_data)
{
	wxString str_value;
	for (size_t ii = 0; ii < m_data.size(); ii++)
	{
		const char *name = (const char*)m_data[ii].var.c_str();
		int type = ::ssc_data_query(p_data, name); 
		switch (type)
		{
		case SSC_STRING:
			fprintf(m_fp, "	const char *%s = ssc_data_get_string( data, \"%s\" );\n", name, name);
			fprintf(m_fp, "	printf(\"%%s = %%s\"), %s, %s);\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_NUMBER:
			fprintf(m_fp, "	ssc_number_t %s;\n", name);
			fprintf(m_fp, "	ssc_data_get_number(data, \"%s\", &%s);\n", name, name);
			fprintf(m_fp, "	printf(\"%%s = %%.17g\\n\", \"%s\", (double)%s);\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_ARRAY:
			// TODO finish and test
			//p = ::ssc_data_get_array(p_data, name, &len);
			//fprintf(m_fp, "	ssc_number_t p_%s[%d] ={", name, len);
			//fprintf(m_fp, "	ssc_data_get_array( data, \"%s\", p_%s, %d );\n", name, name, len);
			break;
		case SSC_MATRIX:
			// TODO tables in future
			break;
		}
	}
	return true;
}

bool CodeGen_c::Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold)
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
		fprintf(m_fp, "	ssc_data_set_string( data, \"%s\", \"%s\" );\n", name, (const char*)str_value.c_str());
		break;
	case SSC_NUMBER:
		::ssc_data_get_number(p_data, name, &value);
		dbl_value = (double)value;
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(m_fp, "	ssc_data_set_number( data, \"%s\", %.17g );\n", name, dbl_value);
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
				//				str_value = wxString::Format("%.17g", dbl_value);
				csv.Set(i, 0, wxString::Format("%.17g", dbl_value));
			}
			csv.WriteFile(fn);
			fprintf(m_fp, "	set_array( data, \"%s\", \"%s\", %d);\n", name, (const char*)fn.c_str(), len);
		}
		else
		{
			fprintf(m_fp, "	ssc_number_t p_%s[%d] ={", name, len);
			for (int i = 0; i < (len-1); i++)
			{
				dbl_value = (double)p[i];
				if (dbl_value > 1e38) dbl_value = 1e38;
				fprintf(m_fp, " %.17g,", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(m_fp, " %.17g };\n", dbl_value);
			fprintf(m_fp, "	ssc_data_set_array( data, \"%s\", p_%s, %d );\n", name, name, len);
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
					csv.Set(r, c, wxString::Format("%.17g", dbl_value));
				}
			}
			csv.WriteFile(fn);
			fprintf(m_fp, "	set_matrix( data, \"%s\", \"%s\", %d, %d);\n", name, (const char*)fn.c_str(), nr, nc);
		}
		else
		{
			fprintf(m_fp, "	ssc_number_t p_%s[%d] ={", name, len);
			for (int k = 0; k < (len - 1); k++)
			{
				dbl_value = (double)p[k];
				if (dbl_value > 1e38) dbl_value = 1e38;
				fprintf(m_fp, " %.17g,", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(m_fp, " %.17g };\n", dbl_value);
			fprintf(m_fp, "	ssc_data_set_matrix( data, \"%s\", p_%s, %d, %d );\n", name, name, nr, nc);
		}
		// TODO tables in future
	}
	return true;
}


bool CodeGen_c::RunSSCModule(wxString &name)
{
	fprintf(m_fp, "	if (ssc_module_exec(module, data) == 0)\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		printf(\"error during simulation.\"); \n");
	fprintf(m_fp, "		ssc_module_free(module); \n");
	fprintf(m_fp, "		ssc_data_free(data); \n");
	fprintf(m_fp, "		return -1; \n");
	fprintf(m_fp, "	}\n");
	return true;
}


bool CodeGen_c::Header()
{
	// top of file and supporting functions
	fprintf(m_fp, "#include <stdio.h>\n");
	fprintf(m_fp, "#include <string.h>\n");
	fprintf(m_fp, "#include <stdlib.h>\n");
	fprintf(m_fp, "#include \"sscapi.h\"\n");
	fprintf(m_fp, "\n");

	// handle message
	fprintf(m_fp, "ssc_bool_t my_handler(ssc_module_t p_mod, ssc_handler_t p_handler, int action,\n");
	fprintf(m_fp, "	float f0, float f1, const char *s0, const char *s1, void *user_data)\n");
	fprintf(m_fp, "{\n");
	fprintf(m_fp, "	if (action == SSC_LOG)\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		// print log message to console\n");
	fprintf(m_fp, "		switch ((int)f0)\n");
	fprintf(m_fp, "		{\n");
	fprintf(m_fp, "		case SSC_NOTICE: printf(\"Notice: %s\", s0); break;\n");
	fprintf(m_fp, "		case SSC_WARNING: printf(\"Warning: %s\", s0); break;\n");
	fprintf(m_fp, "		case SSC_ERROR: printf(\"Error: %s\", s0); break;\n");
	fprintf(m_fp, "		}\n");
	fprintf(m_fp, "		return 1;\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	else if (action == SSC_UPDATE)\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		// print status update to console\n");
	fprintf(m_fp, "		printf(\"(%.2f %%) % s\", f0, s0);\n");
	fprintf(m_fp, "		return 1; // return 0 to abort simulation as needed.\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	else\n");
	fprintf(m_fp, "		return 0;\n");
	fprintf(m_fp, "}\n");
	fprintf(m_fp, "\n");

	// handle csv files
	// arrays
	fprintf(m_fp, "bool set_array(ssc_data_t p_data, const char *name, const char* fn, int len)\n");
	fprintf(m_fp, "{\n");
	fprintf(m_fp, "	char buffer[1024];\n");
	fprintf(m_fp, "	char *record, *line;\n");
	fprintf(m_fp, "	int i = 0;\n");
	fprintf(m_fp, "	ssc_number_t *ary;\n");
	fprintf(m_fp, "	FILE *fp = fopen(fn, \"r\");\n");
	fprintf(m_fp, "	if (m_fp == NULL)\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		printf(\"file opening failed \");\n");
	fprintf(m_fp, "		return false;\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	ary = (ssc_number_t *)malloc(len * sizeof(ssc_number_t));\n");
	fprintf(m_fp, "	if (m_fp == NULL)\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		printf(\"file opening failed \");\n");
	fprintf(m_fp, "		return false;\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	while ((line = fgets(buffer, sizeof(buffer), fp)) != NULL)\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		record = strtok(line, \",\");\n");
	fprintf(m_fp, "		while ((record != NULL) && (i < len))\n");
	fprintf(m_fp, "		{\n");
	fprintf(m_fp, "			ary[i] = atof(record);\n");
	fprintf(m_fp, "			record = strtok(NULL, \",\");\n");
	fprintf(m_fp, "			i++;\n");
	fprintf(m_fp, "		}\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	fclose();\n");
	fprintf(m_fp, "	ssc_data_set_array(p_data, name, ary, len);\n");
	fprintf(m_fp, "	free(ary);\n");
	fprintf(m_fp, "	return true;\n");
	fprintf(m_fp, "}\n");
	fprintf(m_fp, "\n");

	// matrices
	fprintf(m_fp, "bool set_matrix(ssc_data_t p_data, const char *name, const char* fn, int nr, int nc)\n");
	fprintf(m_fp, "{\n");
	fprintf(m_fp, "	char buffer[1024];\n");
	fprintf(m_fp, "	char *record, *line;\n");
	fprintf(m_fp, "	ssc_number_t *ary;\n");
	fprintf(m_fp, "	int i = 0, len = nr*nc;\n");
	fprintf(m_fp, "	FILE *fp = fopen(fn, \"r\");\n");
	fprintf(m_fp, "	if (m_fp == NULL)\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		printf(\"file opening failed \");\n");
	fprintf(m_fp, "		return false;\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	ary = (ssc_number_t *)malloc(len * sizeof(ssc_number_t));\n");
	fprintf(m_fp, "	if (m_fp == NULL)\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		printf(\"file opening failed \");\n");
	fprintf(m_fp, "		return false;\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	while ((line = fgets(buffer, sizeof(buffer), fp)) != NULL)\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		record = strtok(line, \",\");\n");
	fprintf(m_fp, "		while ((record != NULL) && (i < len))\n");
	fprintf(m_fp, "		{\n");
	fprintf(m_fp, "			ary[i] = atof(record);\n");
	fprintf(m_fp, "			record = strtok(NULL, \",\");\n");
	fprintf(m_fp, "			i++;\n");
	fprintf(m_fp, "		}\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	fclose();\n");
	fprintf(m_fp, "	ssc_data_set_matrix(p_data, name, ary, nr, nc);\n");
	fprintf(m_fp, "	free(ary);\n");
	fprintf(m_fp, "	return true;\n");
	fprintf(m_fp, "}\n");


	fprintf(m_fp, "\n");

	fprintf(m_fp, "int main(int argc, char *argv[])\n");
	fprintf(m_fp, "{\n");

	// create global data container
	fprintf(m_fp, "	ssc_data_t data = ssc_data_create();\n");
	fprintf(m_fp, "	if (data == NULL)\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		printf(\"error: out of memory.\");\n");
	fprintf(m_fp, "		return -1;\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	ssc_module_t module;\n");
	fprintf(m_fp, "\n");

	return true;
}

bool CodeGen_c::CreateSSCModule(wxString &name)
{
	if (name.IsNull() || name.Length() < 1)
		return false;
	else
	{
		fprintf(m_fp, "	module = ssc_module_create(\"%s\"); \n", (const char*)name.c_str());
		fprintf(m_fp, "	if (NULL == module)\n");
		fprintf(m_fp, "	{\n");
		fprintf(m_fp, "		printf(\"error: could not create '%s' module.\"); \n", (const char*)name.c_str());
		fprintf(m_fp, "		ssc_data_free(data); \n");
		fprintf(m_fp, "		return -1; \n");
		fprintf(m_fp, "	}\n");
	}
	return true;
}

bool CodeGen_c::FreeSSCModule()
{
	fprintf(m_fp, "	ssc_module_free(module);\n");
	return true;
}

bool CodeGen_c::Footer()
{
	fprintf(m_fp, "	ssc_data_free(data);\n");
	fprintf(m_fp, "	return 0;\n");
	fprintf(m_fp, "}\n");
	return true;
}


// c# code generation class

CodeGen_csharp::CodeGen_csharp(Case *cc, const wxString &folder) : CodeGen_Base(cc, folder)
{
}


bool CodeGen_csharp::Output(ssc_data_t p_data)
{
	wxString str_value;
	for (size_t ii = 0; ii < m_data.size(); ii++)
	{
		const char *name = (const char*)m_data[ii].var.c_str();
		int type = ::ssc_data_query(p_data, name);
		switch (type)
		{
		case SSC_STRING:
			fprintf(m_fp, "		String %s = data.GetString( \"%s\" );\n", name, name);
			fprintf(m_fp, "		Console.WriteLine(\"{0} = {1}\"), %s, %s);\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_NUMBER:
			fprintf(m_fp, "		float %s = data.GetNumber(\"%s\");\n", name, name);
			fprintf(m_fp, "		Console.WriteLine(\"{0} = {1}\", \"%s\", %s);\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_ARRAY: // TODO
			//p = ::ssc_data_get_array(p_data, name, &len);
			//fprintf(m_fp, "		ssc_number_t p_%s[%d] ={", name, len);
			//fprintf(m_fp, "		csvfile...( data, \"%s\", p_%s, %d );\n", name, name, len);
			break;
		case SSC_MATRIX:
			// TODO tables in future
			break;
		}
	}
	return true;
}

bool CodeGen_csharp::Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold)
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
		fprintf(m_fp, "		data.SetString( \"%s\", \"%s\" );\n", name, (const char*)str_value.c_str());
		break;
	case SSC_NUMBER:
		::ssc_data_get_number(p_data, name, &value);
		dbl_value = (double)value;
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(m_fp, "		data.SetNumber( \"%s\", %.17gf );\n", name, dbl_value);
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
				//				str_value = wxString::Format("%.17g", dbl_value);
				csv.Set(i, 0, wxString::Format("%.17g", dbl_value));
			}
			csv.WriteFile(fn);
			fprintf(m_fp, "		data.SetArray( \"%s\", \"%s\", %d);\n", name, (const char*)fn.c_str(), len);
		}
		else
		{
			fprintf(m_fp, "		float[] p_%s ={", name, len);
			for (int i = 0; i < (len - 1); i++)
			{
				dbl_value = (double)p[i];
				if (dbl_value > 1e38) dbl_value = 1e38;
				fprintf(m_fp, " %.17gf,", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(m_fp, " %.17gf };\n", dbl_value);
			fprintf(m_fp, "		data.SetArray( \"%s\", p_%s);\n", name, name);
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
					csv.Set(r, c, wxString::Format("%.17g", dbl_value));
				}
			}
			csv.WriteFile(fn);
			fprintf(m_fp, "		data.SetMatrix( \"%s\", \"%s\", %d, %d);\n", name, (const char*)fn.c_str(), nr, nc);
		}
		else
		{
			fprintf(m_fp, "		float[,] p_%s ={ {", name, len);
			for (int k = 0; k < (len - 1); k++)
			{
				dbl_value = (double)p[k];
				if (dbl_value > 1e38) dbl_value = 1e38;
				if ((k > 0) && (k%nc == 0))
					fprintf(m_fp, " { %.17gf,", dbl_value);
				else if (k%nc == (nc - 1))
					fprintf(m_fp, " %.17gf },", dbl_value);
				else 
					fprintf(m_fp, " %.17gf, ", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(m_fp, " %.17gf } };\n", dbl_value);
			fprintf(m_fp, "		data.SetMatrix( \"%s\", p_%s );\n", name, name);
		}
		// TODO tables in future
	}
	return true;
}


bool CodeGen_csharp::RunSSCModule(wxString &name)
{
	fprintf(m_fp, "		if (!module.Exec(data))\n");
	fprintf(m_fp, "		{\n");
	fprintf(m_fp, "			int idx = 0;\n");
	fprintf(m_fp, "			String msg;\n");
	fprintf(m_fp, "			int type;\n");
	fprintf(m_fp, "			float time;\n");
	fprintf(m_fp, "			while (module.Log(idx, out msg, out type, out time))\n");
	fprintf(m_fp, "			{\n");
	fprintf(m_fp, "				String stype = \"NOTICE\";\n");
	fprintf(m_fp, "				if (type == SSC.API.WARNING) stype = \"WARNING\";\n");
	fprintf(m_fp, "				else if (type == SSC.API.ERROR) stype = \"ERROR\";\n");
	fprintf(m_fp, "				Console.WriteLine(\"[\" + stype + \" at time : \" + time + \"]: \" + msg );\n");
	fprintf(m_fp, "				idx++;\n");
	fprintf(m_fp, "			}\n");
	fprintf(m_fp, "			return;\n");
	fprintf(m_fp, "		}\n");
	return true;
}


bool CodeGen_csharp::Header()
{
	// top of file and supporting functions
	fprintf(m_fp, "using System;\n");
	fprintf(m_fp, "using System.IO;\n");
	fprintf(m_fp, "using System.Collections.Generic;\n");
	fprintf(m_fp, "using System.Linq;\n");
	fprintf(m_fp, "using System.Text;\n");
	fprintf(m_fp, "using System.Threading.Tasks;\n");
	fprintf(m_fp, "using System.Runtime.InteropServices;\n");
	fprintf(m_fp, "namespace SSC\n");
	fprintf(m_fp, "{\n");
	fprintf(m_fp, "    class sscapi\n");
	fprintf(m_fp, "    {\n");
	fprintf(m_fp, "        static sscapi()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_version\")]\n");
	fprintf(m_fp, "        public static extern int ssc_version32();\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_version\")]\n");
	fprintf(m_fp, "        public static extern int ssc_version64();\n");
	fprintf(m_fp, "        public static int ssc_version()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_version64() : ssc_version32();\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_build_info\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_build_info32();\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_build_info\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_build_info64();\n");
	fprintf(m_fp, "        public static IntPtr ssc_build_info()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_build_info64() : ssc_build_info32();\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_create\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_data_create32();\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_create\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_data_create64();\n");
	fprintf(m_fp, "        public static IntPtr ssc_data_create()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_data_create64() : ssc_data_create32();\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_free\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_free32(HandleRef cxtData);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_free\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_free64(HandleRef cxtData);\n");
	fprintf(m_fp, "        public static void ssc_data_free(HandleRef cxtData)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (System.IntPtr.Size == 8) \n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_free64(cxtData);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_free32(cxtData);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_clear\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_clear32(HandleRef cxtData);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_clear\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_clear64(HandleRef cxtData);\n");
	fprintf(m_fp, "        public static void ssc_data_clear(HandleRef cxtData)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_clear64(cxtData);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_clear32(cxtData);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_unassign\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_unassign32(HandleRef cxtData, string variableName);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_unassign\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_unassign64(HandleRef cxtData, string variableName);\n");
	fprintf(m_fp, "        public static void ssc_data_unassign(HandleRef cxtData, string variableName)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_unassign64(cxtData, variableName);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_unassign32(cxtData, variableName);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_query\")]\n");
	fprintf(m_fp, "        public static extern int ssc_data_query32(HandleRef cxtData, string variableName);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_query\")]\n");
	fprintf(m_fp, "        public static extern int ssc_data_query64(HandleRef cxtData, string variableName);\n");
	fprintf(m_fp, "        public static int ssc_data_query(HandleRef cxtData, string variableName)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_data_query64(cxtData, variableName) : ssc_data_query32(cxtData, variableName);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_first\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_data_first32(HandleRef cxtData);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_first\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_data_first64(HandleRef cxtData);\n");
	fprintf(m_fp, "        public static IntPtr ssc_data_first(HandleRef cxtData)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_data_first64(cxtData) : ssc_data_first32(cxtData);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_next\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_data_next32(HandleRef cxtData);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_next\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_data_next64(HandleRef cxtData);\n");
	fprintf(m_fp, "        public static IntPtr ssc_data_next(HandleRef cxtData)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_data_next64(cxtData) : ssc_data_next32(cxtData);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_string\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_set_string32(HandleRef cxtData, string name, string value);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_string\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_set_string64(HandleRef cxtData, string name, string value);\n");
	fprintf(m_fp, "        public static void ssc_data_set_string(HandleRef cxtData, string name, string value)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_set_string64(cxtData, name, value);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_set_string32(cxtData, name, value);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_number\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_set_number32(HandleRef cxtData, string name, float value);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_number\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_set_number64(HandleRef cxtData, string name, float value);\n");
	fprintf(m_fp, "        public static void ssc_data_set_number(HandleRef cxtData, string name, float value)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_set_number64(cxtData, name, value);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_set_number32(cxtData, name, value);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_array\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_set_array32(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[] array, int length);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_array\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_set_array64(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[] array, int length);\n");
	fprintf(m_fp, "        public static void ssc_data_set_array(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[] array, int length)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_set_array64(cxtData, name, array, length);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_set_array32(cxtData, name, array, length);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_matrix\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_set_matrix32(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[,] matrix, int nRows, int nCols);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_matrix\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_set_matrix64(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[,] matrix, int nRows, int nCols);\n");
	fprintf(m_fp, "        public static void ssc_data_set_matrix(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[,] matrix, int nRows, int nCols)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_set_matrix64(cxtData, name, matrix, nRows, nCols);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_set_matrix32(cxtData, name, matrix, nRows, nCols);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_table\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_set_table32(HandleRef cxtData, string name, HandleRef cxtTable);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_set_table\")]\n");
	fprintf(m_fp, "        public static extern void ssc_data_set_table64(HandleRef cxtData, string name, HandleRef cxtTable);\n");
	fprintf(m_fp, "        public static void ssc_data_set_table(HandleRef cxtData, string name, HandleRef cxtTable)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_set_table64(cxtData, name, cxtTable);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_data_set_table32(cxtData, name, cxtTable);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_string\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_data_get_string32(HandleRef cxtData, string name);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_string\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_data_get_string64(HandleRef cxtData, string name);\n");
	fprintf(m_fp, "        public static IntPtr ssc_data_get_string(HandleRef cxtData, string name)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_data_get_string64(cxtData, name) : ssc_data_get_string32(cxtData, name);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_number\")]\n");
	fprintf(m_fp, "        public static extern int ssc_data_get_number32(HandleRef cxtData, string name, out float number);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_number\")]\n");
	fprintf(m_fp, "        public static extern int ssc_data_get_number64(HandleRef cxtData, string name, out float number);\n");
	fprintf(m_fp, "        public static int ssc_data_get_number(HandleRef cxtData, string name, out float number)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_data_get_number64(cxtData, name, out number) : ssc_data_get_number32(cxtData, name, out number);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_array\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_data_get_array32(HandleRef cxtData, string name, out int len);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_array\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_data_get_array64(HandleRef cxtData, string name, out int len);\n");
	fprintf(m_fp, "        public static IntPtr ssc_data_get_array(HandleRef cxtData, string name, out int len)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_data_get_array64(cxtData, name, out len) : ssc_data_get_array32(cxtData, name, out len);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_matrix\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_data_get_matrix32(HandleRef cxtData, string name, out int nRows, out int nCols);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_matrix\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_data_get_matrix64(HandleRef cxtData, string name, out int nRows, out int nCols);\n");
	fprintf(m_fp, "        public static IntPtr ssc_data_get_matrix(HandleRef cxtData, string name, out int nRows, out int nCols)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_data_get_matrix64(cxtData, name, out nRows, out nCols) : ssc_data_get_matrix32(cxtData, name, out nRows, out nCols);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_table\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_data_get_table32(HandleRef cxtData, string name);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_data_get_table\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_data_get_table64(HandleRef cxtData, string name);\n");
	fprintf(m_fp, "        public static IntPtr ssc_data_get_table(HandleRef cxtData, string name)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_data_get_table64(cxtData, name) : ssc_data_get_table32(cxtData, name);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_entry\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_module_entry32(int moduleIndex);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_entry\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_module_entry64(int moduleIndex);\n");
	fprintf(m_fp, "        public static IntPtr ssc_module_entry(int moduleIndex)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_module_entry64(moduleIndex) : ssc_module_entry32(moduleIndex);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_entry_name\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_entry_name32(HandleRef cxtEntry);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_entry_name\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_entry_name64(HandleRef cxtEntry);\n");
	fprintf(m_fp, "        public static IntPtr ssc_entry_name(HandleRef cxtEntry)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_entry_name64(cxtEntry) : ssc_entry_name32(cxtEntry);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_entry_description\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_entry_description32(HandleRef cxtEntry);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_entry_description\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_entry_description64(HandleRef cxtEntry);\n");
	fprintf(m_fp, "        public static IntPtr ssc_entry_description(HandleRef cxtEntry)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_entry_description64(cxtEntry) : ssc_entry_description32(cxtEntry);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_entry_version\")]\n");
	fprintf(m_fp, "        public static extern int ssc_entry_version32(HandleRef cxtEntry);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_entry_version\")]\n");
	fprintf(m_fp, "        public static extern int ssc_entry_version64(HandleRef cxtEntry);\n");
	fprintf(m_fp, "        public static int ssc_entry_version(HandleRef cxtEntry)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_entry_version64(cxtEntry) : ssc_entry_version32(cxtEntry);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_create\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_module_create32(string moduleName);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_create\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_module_create64(string moduleName);\n");
	fprintf(m_fp, "        public static IntPtr ssc_module_create(string moduleName)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_module_create64(moduleName) : ssc_module_create32(moduleName);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_free\")]\n");
	fprintf(m_fp, "        public static extern void ssc_module_free32(HandleRef cxtModule);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_free\")]\n");
	fprintf(m_fp, "        public static extern void ssc_module_free64(HandleRef cxtModule);\n");
	fprintf(m_fp, "        public static void ssc_module_free(HandleRef cxtModule)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (System.IntPtr.Size == 8)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_module_free64(cxtModule);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                ssc_module_free32(cxtModule);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_var_info\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_module_var_info32(HandleRef cxtModule, int index);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_var_info\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_module_var_info64(HandleRef cxtModule, int index);\n");
	fprintf(m_fp, "        public static IntPtr ssc_module_var_info(HandleRef cxtModule, int index)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_module_var_info64(cxtModule, index) : ssc_module_var_info32(cxtModule, index);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_var_type\")]\n");
	fprintf(m_fp, "        public static extern int ssc_info_var_type32(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_var_type\")]\n");
	fprintf(m_fp, "        public static extern int ssc_info_var_type64(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        public static int ssc_info_var_type(HandleRef cxtInfo)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_info_var_type64(cxtInfo) : ssc_info_var_type32(cxtInfo);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_data_type\")]\n");
	fprintf(m_fp, "        public static extern int ssc_info_data_type32(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_data_type\")]\n");
	fprintf(m_fp, "        public static extern int ssc_info_data_type64(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        public static int ssc_info_data_type(HandleRef cxtInfo)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_info_data_type64(cxtInfo) : ssc_info_data_type32(cxtInfo);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_name\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_name32(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_name\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_name64(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        public static IntPtr ssc_info_name(HandleRef cxtInfo)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_info_name64(cxtInfo) : ssc_info_name32(cxtInfo);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_label\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_label32(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_label\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_label64(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        public static IntPtr ssc_info_label(HandleRef cxtInfo)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_info_label64(cxtInfo) : ssc_info_label32(cxtInfo);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_units\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_units32(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_units\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_units64(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        public static IntPtr ssc_info_units(HandleRef cxtInfo)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_info_units64(cxtInfo) : ssc_info_units32(cxtInfo);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_meta\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_meta32(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_meta\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_meta64(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        public static IntPtr ssc_info_meta(HandleRef cxtInfo)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_info_meta64(cxtInfo) : ssc_info_meta32(cxtInfo);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_group\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_group32(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_group\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_group64(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        public static IntPtr ssc_info_group(HandleRef cxtInfo)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_info_group64(cxtInfo) : ssc_info_group32(cxtInfo);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_required\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_required32(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_required\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_required64(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        public static IntPtr ssc_info_required(HandleRef cxtInfo)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_info_required64(cxtInfo) : ssc_info_required32(cxtInfo);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_constraints\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_constraints32(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_constraints\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_constraints64(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        public static IntPtr ssc_info_constraints(HandleRef cxtInfo)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_info_constraints64(cxtInfo) : ssc_info_constraints32(cxtInfo);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_uihint\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_uihint32(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_info_uihint\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_info_uihint64(HandleRef cxtInfo);\n");
	fprintf(m_fp, "        public static IntPtr ssc_info_uihint(HandleRef cxtInfo)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_info_uihint64(cxtInfo) : ssc_info_units32(cxtInfo);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec_simple\")]\n");
	fprintf(m_fp, "        public static extern int ssc_module_exec_simple32(string moduleName, HandleRef cxtData);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec_simple\")]\n");
	fprintf(m_fp, "        public static extern int ssc_module_exec_simple64(string moduleName, HandleRef cxtData);\n");
	fprintf(m_fp, "        public static int ssc_module_exec_simple(string moduleName, HandleRef cxtData)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_module_exec_simple64(moduleName, cxtData) : ssc_module_exec_simple32(moduleName, cxtData);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec_simple_nothread\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_module_exec_simple_nothread32(string moduleName, HandleRef cxtData);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec_simple_nothread\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_module_exec_simple_nothread64(string moduleName, HandleRef cxtData);\n");
	fprintf(m_fp, "        public static IntPtr ssc_module_exec_simple_nothread(string moduleName, HandleRef cxtData)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_module_exec_simple_nothread64(moduleName, cxtData) : ssc_module_exec_simple_nothread32(moduleName, cxtData);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "        \n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec\")]\n");
	fprintf(m_fp, "        public static extern int ssc_module_exec32(HandleRef cxtModule, HandleRef cxtData);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec\")]\n");
	fprintf(m_fp, "        public static extern int ssc_module_exec64(HandleRef cxtModule, HandleRef cxtData);\n");
	fprintf(m_fp, "        public static int ssc_module_exec(HandleRef cxtModule, HandleRef cxtData)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_module_exec64(cxtModule, cxtData) : ssc_module_exec32(cxtModule, cxtData);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec_with_handler\")]\n");
	fprintf(m_fp, "        public static extern int ssc_module_exec_with_handler32(HandleRef cxtModule, HandleRef cxtData, HandleRef cxtHandler, HandleRef cxtUserData);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_exec_with_handler\")]\n");
	fprintf(m_fp, "        public static extern int ssc_module_exec_with_handler64(HandleRef cxtModule, HandleRef cxtData, HandleRef cxtHandler, HandleRef cxtUserData);\n");
	fprintf(m_fp, "        public static int ssc_module_exec_with_handler(HandleRef cxtModule, HandleRef cxtData, HandleRef cxtHandler, HandleRef cxtUserData)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_module_exec_with_handler64(cxtModule, cxtData, cxtHandler, cxtUserData) : ssc_module_exec_with_handler32(cxtModule, cxtData, cxtHandler, cxtUserData);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        [DllImport(\"ssc32.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_log\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_module_log32(HandleRef cxtModule, int index, out int messageType, out float time);\n");
	fprintf(m_fp, "        [DllImport(\"ssc64.dll\", CallingConvention = CallingConvention.Cdecl, EntryPoint = \"ssc_module_log\")]\n");
	fprintf(m_fp, "        public static extern IntPtr ssc_module_log64(HandleRef cxtModule, int index, out int messageType, out float time);\n");
	fprintf(m_fp, "        public static IntPtr ssc_module_log(HandleRef cxtModule, int index, out int messageType, out float time)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (System.IntPtr.Size == 8) ? ssc_module_log64(cxtModule, index, out messageType, out time) : ssc_module_log32(cxtModule, index, out messageType, out time);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "    }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "    public class Data\n");
	fprintf(m_fp, "    {\n");
	fprintf(m_fp, "        private HandleRef m_data;\n");
	fprintf(m_fp, "        private bool m_owned;\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public Data()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            m_data = new HandleRef(this, sscapi.ssc_data_create());\n");
	fprintf(m_fp, "            m_owned = true;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public Data( IntPtr dataRefNotOwned )\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            m_data = new HandleRef(this, dataRefNotOwned);\n");
	fprintf(m_fp, "            m_owned = false;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        ~Data()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (m_owned && m_data.Handle != IntPtr.Zero)\n");
	fprintf(m_fp, "                sscapi.ssc_data_free(m_data);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public void Clear()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            sscapi.ssc_data_clear(m_data);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public String First()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            IntPtr p = sscapi.ssc_data_first(m_data);\n");
	fprintf(m_fp, "            if (p != IntPtr.Zero)\n");
	fprintf(m_fp, "                return Marshal.PtrToStringAnsi(p);\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "                return null;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public String Next()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            IntPtr p = sscapi.ssc_data_next(m_data);\n");
	fprintf(m_fp, "            if (p != IntPtr.Zero)\n");
	fprintf(m_fp, "                return Marshal.PtrToStringAnsi(p);\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "                return null;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public int Query(String name)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return sscapi.ssc_data_query(m_data, name);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public void SetNumber(String name, float value)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            sscapi.ssc_data_set_number(m_data, name, value);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public float GetNumber(String name)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            float val = float.NaN;\n");
	fprintf(m_fp, "            sscapi.ssc_data_get_number(m_data, name, out val);\n");
	fprintf(m_fp, "            return val;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public void SetString(String name, String value)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            sscapi.ssc_data_set_string(m_data, name, value);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public String GetString(String name)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            IntPtr p = sscapi.ssc_data_get_string(m_data, name);\n");
	fprintf(m_fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public void SetArray(String name, float[] data)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            sscapi.ssc_data_set_array(m_data, name, data, data.Length);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public void SetArray(String name, String fn, int len)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            StreamReader sr = new StreamReader(fn);\n");
	fprintf(m_fp, "            int Row = 0;\n");
	fprintf(m_fp, "            float[] data = new float[len];\n");
	fprintf(m_fp, "            while (!sr.EndOfStream && Row < len)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "				string[] Line = sr.ReadLine().Split(',');\n");
	fprintf(m_fp, "				data[Row] = float.Parse(Line[0]);\n");
	fprintf(m_fp, "				Row++;\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            sscapi.ssc_data_set_array(m_data, name, data, len);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public float[] GetArray(String name)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            int len;\n");
	fprintf(m_fp, "            IntPtr res = sscapi.ssc_data_get_array(m_data, name, out len);\n");
	fprintf(m_fp, "            float[] arr = null;\n");
	fprintf(m_fp, "            if (len > 0)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                arr = new float[len];\n");
	fprintf(m_fp, "                Marshal.Copy(res, arr, 0, len);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            return arr;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public void SetMatrix(String name, float[,] mat)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            int nRows = mat.GetLength(0);\n");
	fprintf(m_fp, "            int nCols = mat.GetLength(1);\n");
	fprintf(m_fp, "            sscapi.ssc_data_set_matrix(m_data, name, mat, nRows, nCols);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public void SetMatrix(String name, String fn, int nr, int nc)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            StreamReader sr = new StreamReader(fn);\n");
	fprintf(m_fp, "            int Row = 0;\n");
	fprintf(m_fp, "            float[,] mat = new float[nr, nc];\n");
	fprintf(m_fp, "            while (!sr.EndOfStream && Row < nr)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "				string[] Line = sr.ReadLine().Split(',');\n");
	fprintf(m_fp, "				for (int ic = 0; ic < Line.Length && ic < nc; ic++)\n");
	fprintf(m_fp, "					mat[Row, ic] = float.Parse(Line[ic]);\n");
	fprintf(m_fp, "				Row++;\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            sscapi.ssc_data_set_matrix(m_data, name, mat, nr, nc);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public float[,] GetMatrix(String name)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            int nRows, nCols;\n");
	fprintf(m_fp, "            IntPtr res = sscapi.ssc_data_get_matrix(m_data, name, out nRows, out nCols);\n");
	fprintf(m_fp, "            if (nRows * nCols > 0)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                float[] sscMat = new float[nRows * nCols];\n");
	fprintf(m_fp, "                Marshal.Copy(res, sscMat, 0, nRows * nCols);\n");
	fprintf(m_fp, "                float[,] mat = new float[nRows, nCols];\n");
	fprintf(m_fp, "                for (int i = 0; i < nRows; i++)\n");
	fprintf(m_fp, "                {\n");
	fprintf(m_fp, "                    for (int j = 0; j < nCols; j++)\n");
	fprintf(m_fp, "                    {\n");
	fprintf(m_fp, "                        mat[i, j] = sscMat[i * nCols + j];\n");
	fprintf(m_fp, "                    }\n");
	fprintf(m_fp, "                }\n");
	fprintf(m_fp, "                return mat;\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "                return null;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public void SetTable(String name, Data table)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            sscapi.ssc_data_set_table(m_data, name, table.GetDataHandle());\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public Data GetTable(String name)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            IntPtr p = sscapi.ssc_data_get_table(m_data, name);\n");
	fprintf(m_fp, "            if (IntPtr.Zero == p)\n");
	fprintf(m_fp, "                return null;\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "                return new Data( p );\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public HandleRef GetDataHandle()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return m_data;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "    }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "    public class Module\n");
	fprintf(m_fp, "    {\n");
	fprintf(m_fp, "        private HandleRef m_mod;\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public Module(String name)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            m_mod = new HandleRef(this, sscapi.ssc_module_create(name) );\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        ~Module()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (m_mod.Handle != IntPtr.Zero)\n");
	fprintf(m_fp, "                sscapi.ssc_module_free(m_mod);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public bool IsOk()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return m_mod.Handle != IntPtr.Zero;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public HandleRef GetModuleHandle()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return m_mod;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public bool Exec( Data data )\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return (sscapi.ssc_module_exec(m_mod, data.GetDataHandle()) != 0);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public bool Log(int idx, out String msg, out int type, out float time)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            msg = \"\";\n");
	fprintf(m_fp, "            IntPtr p = sscapi.ssc_module_log(m_mod, idx, out type, out time);\n");
	fprintf(m_fp, "            if (IntPtr.Zero != p)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                msg = Marshal.PtrToStringAnsi(p);\n");
	fprintf(m_fp, "                return true;\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "                return false;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "    }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "    public class Entry\n");
	fprintf(m_fp, "    {\n");
	fprintf(m_fp, "        private HandleRef m_entry;\n");
	fprintf(m_fp, "        private int m_idx;\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public Entry()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            m_idx = 0;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public void Reset()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            m_idx = 0;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public bool Get()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            IntPtr p = sscapi.ssc_module_entry(m_idx);\n");
	fprintf(m_fp, "            if (p == IntPtr.Zero)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                Reset();\n");
	fprintf(m_fp, "                return false;\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "            m_entry = new HandleRef(this, p);\n");
	fprintf(m_fp, "            m_idx++;\n");
	fprintf(m_fp, "            return true;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public String Name()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (m_entry.Handle != IntPtr.Zero)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                IntPtr p = sscapi.ssc_entry_name(m_entry);\n");
	fprintf(m_fp, "                return Marshal.PtrToStringAnsi(p);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            else return null;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public String Description()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (m_entry.Handle != IntPtr.Zero)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                IntPtr p = sscapi.ssc_entry_description(m_entry);\n");
	fprintf(m_fp, "                return Marshal.PtrToStringAnsi(p);\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "                return null;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public int Version()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (m_entry.Handle != IntPtr.Zero)\n");
	fprintf(m_fp, "                return sscapi.ssc_entry_version(m_entry);\n");
	fprintf(m_fp, "            else\n");
	fprintf(m_fp, "                return -1;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "    }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "    public class Info\n");
	fprintf(m_fp, "    {\n");
	fprintf(m_fp, "        private HandleRef m_inf;\n");
	fprintf(m_fp, "        private Module m_mod;\n");
	fprintf(m_fp, "        private int m_idx;\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public Info(Module m)\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            m_mod = m;\n");
	fprintf(m_fp, "            m_idx = 0;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public void Reset()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            m_idx = 0;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public bool Get()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            IntPtr p = sscapi.ssc_module_var_info(m_mod.GetModuleHandle(), m_idx);\n");
	fprintf(m_fp, "            if (p == IntPtr.Zero)\n");
	fprintf(m_fp, "            {\n");
	fprintf(m_fp, "                Reset();\n");
	fprintf(m_fp, "                return false;\n");
	fprintf(m_fp, "            }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "            m_inf = new HandleRef(this, p);\n");
	fprintf(m_fp, "            m_idx++;\n");
	fprintf(m_fp, "            return true;\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public String Name()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (m_inf.Handle == IntPtr.Zero) return null;\n");
	fprintf(m_fp, "            IntPtr p = sscapi.ssc_info_name(m_inf);\n");
	fprintf(m_fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public int VarType()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (m_inf.Handle == IntPtr.Zero) return -1;\n");
	fprintf(m_fp, "            return sscapi.ssc_info_var_type(m_inf);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public int DataType()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (m_inf.Handle == IntPtr.Zero) return -1;\n");
	fprintf(m_fp, "            return sscapi.ssc_info_data_type(m_inf);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public string Label()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (m_inf.Handle == IntPtr.Zero) return null;\n");
	fprintf(m_fp, "            IntPtr p = sscapi.ssc_info_label(m_inf);\n");
	fprintf(m_fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public string Units()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (m_inf.Handle == IntPtr.Zero) return null;\n");
	fprintf(m_fp, "            IntPtr p = sscapi.ssc_info_units(m_inf);\n");
	fprintf(m_fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public string Meta()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (m_inf.Handle == IntPtr.Zero) return null;\n");
	fprintf(m_fp, "            IntPtr p = sscapi.ssc_info_meta(m_inf);\n");
	fprintf(m_fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public string Group()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (m_inf.Handle == IntPtr.Zero) return null;\n");
	fprintf(m_fp, "            IntPtr p = sscapi.ssc_info_group(m_inf);\n");
	fprintf(m_fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public string Required()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (m_inf.Handle == IntPtr.Zero) return null;\n");
	fprintf(m_fp, "            IntPtr p = sscapi.ssc_info_required(m_inf);\n");
	fprintf(m_fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        public string Constraints()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            if (m_inf.Handle == IntPtr.Zero) return null;\n");
	fprintf(m_fp, "            IntPtr p = sscapi.ssc_info_constraints(m_inf);\n");
	fprintf(m_fp, "            return Marshal.PtrToStringAnsi(p);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "    }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "    public class API\n");
	fprintf(m_fp, "    {\n");
	fprintf(m_fp, "        // constants for return value of Info.VarType() (see sscapi.h)\n");
	fprintf(m_fp, "        public const int INPUT = 1;\n");
	fprintf(m_fp, "        public const int OUTPUT = 2;\n");
	fprintf(m_fp, "        public const int INOUT = 3;\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        // constants for out integer type in Module.Log() method (see sscapi.h)\n");
	fprintf(m_fp, "        public const int NOTICE = 1;\n");
	fprintf(m_fp, "        public const int WARNING = 2;\n");
	fprintf(m_fp, "        public const int ERROR = 3;\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        // constants for return value of Data.Query() and Info.DataType() (see sscapi.h)\n");
	fprintf(m_fp, "        public const int INVALID = 0;\n");
	fprintf(m_fp, "        public const int STRING = 1;\n");
	fprintf(m_fp, "        public const int NUMBER = 2;\n");
	fprintf(m_fp, "        public const int ARRAY = 3;\n");
	fprintf(m_fp, "        public const int MATRIX = 4;\n");
	fprintf(m_fp, "        public const int TABLE = 5;\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        static public int Version()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            return sscapi.ssc_version();\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "\n");
	fprintf(m_fp, "        static public String BuildInfo()\n");
	fprintf(m_fp, "        {\n");
	fprintf(m_fp, "            IntPtr buildInfo = sscapi.ssc_build_info();\n");
	fprintf(m_fp, "            return Marshal.PtrToStringAnsi(buildInfo);\n");
	fprintf(m_fp, "        }\n");
	fprintf(m_fp, "    }\n");
	fprintf(m_fp, "}\n");
	fprintf(m_fp, "\n");

	// csv reader


	fprintf(m_fp, "class SAM_Code\n");
	fprintf(m_fp, "{\n");
	fprintf(m_fp, "	static void Main() \n");
	fprintf(m_fp, "	{\n");

	// create global data container
	fprintf(m_fp, "		SSC.Data data = new SSC.Data();\n");
	fprintf(m_fp, "		if (data == null)\n");
	fprintf(m_fp, "		{\n");
	fprintf(m_fp, "			Console.WriteLine(\"error: out of memory.\");\n");
	fprintf(m_fp, "			return;\n");
	fprintf(m_fp, "		}\n");
	fprintf(m_fp, "		SSC.Module module;\n");
	fprintf(m_fp, "\n");

	return true;
}

bool CodeGen_csharp::CreateSSCModule(wxString &name)
{
	if (name.IsNull() || name.Length() < 1)
		return false;
	else
	{
		fprintf(m_fp, "		module = new SSC.Module(\"%s\"); \n", (const char*)name.c_str());
		fprintf(m_fp, "		if (null == module)\n");
		fprintf(m_fp, "		{\n");
		fprintf(m_fp, "			Console.WriteLine(\"error: could not create '%s' module.\"); \n", (const char*)name.c_str());
		fprintf(m_fp, "			return; \n");
		fprintf(m_fp, "		}\n");
	}
	return true;
}

bool CodeGen_csharp::FreeSSCModule()
{
// csharp cleans own garbage in SSC.api
	return true;
}

bool CodeGen_csharp::Footer()
{
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "}\n");
	return true;
}




// MATLAB code generation class

CodeGen_matlab::CodeGen_matlab(Case *cc, const wxString &folder) : CodeGen_Base(cc, folder)
{
}


bool CodeGen_matlab::Output(ssc_data_t p_data)
{
	//		fprintf(m_fp, "outln('%s ' + var('%s'));\n", (const char*)m_data[ii].label.c_str(), (const char*)m_data[ii].var.c_str());
	wxString str_value;
	for (size_t ii = 0; ii < m_data.size(); ii++)
	{
		const char *name = (const char*)m_data[ii].var.c_str();
		int type = ::ssc_data_query(p_data, name);
		switch (type)
		{
		case SSC_STRING:
			fprintf(m_fp, "	%s = ssccall('data_get_string', data, '%s' );\n", name, name);
			fprintf(m_fp, "	disp(sprintf('%%s = %%s), '%s', %s));\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_NUMBER:
			fprintf(m_fp, "	%s = ssccall('data_get_number', data, '%s' );\n", name, name);
			fprintf(m_fp, "	disp(sprintf('%%s = %%g', '%s', %s));\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_ARRAY:
			fprintf(m_fp, "	%s = ssccall('data_get_array', data, '%s')", name, name);
			fprintf(m_fp, "	[nrows ncols] = size(%s);\n", name);
			fprintf(m_fp, "	for i = 1: nrows\n");
			fprintf(m_fp, "		for j = 1: ncols\n");
			fprintf(m_fp, "			disp(sprintf('\treturned array element: (%%d) = %%g', i, %s(i,j)));\n", name);
			fprintf(m_fp, "		end\n");
			fprintf(m_fp, "	end\n");
			break;
		case SSC_MATRIX:
			fprintf(m_fp, "	%s = ssccall('data_get_matrix', data, '%s')", name, name);
			fprintf(m_fp, "	[nrows ncols] = size(%s);\n", name);
			fprintf(m_fp, "	for i = 1: nrows\n");
			fprintf(m_fp, "		for j = 1: ncols\n");
			fprintf(m_fp, "			disp(sprintf('\treturned matrix element: (%%d,%%d) = %%g', i,j, %s(i,j)));\n", name);
			fprintf(m_fp, "		end\n");
			fprintf(m_fp, "	end\n");
			break;
		}
	}
	return true;
}

bool CodeGen_matlab::Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold)
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
		fprintf(m_fp, "	ssccall('data_set_string', data, '%s', '%s');\n", name, (const char*)str_value.c_str());
		break;
	case SSC_NUMBER:
		::ssc_data_get_number(p_data, name, &value);
		dbl_value = (double)value;
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(m_fp, "	ssccall('data_set_number', data, '%s', %.17g);\n", name, dbl_value);
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
				//				str_value = wxString::Format("%.17g", dbl_value);
				csv.Set(i, 0, wxString::Format("%.17g", dbl_value));
			}
			csv.WriteFile(fn);
			fprintf(m_fp, "	%s = csvread( '%s');\n", name, (const char*)fn.c_str());
		}
		else
		{
			fprintf(m_fp, "	%s =[", name);
			for (int i = 0; i < (len - 1); i++)
			{
				dbl_value = (double)p[i];
				if (dbl_value > 1e38) dbl_value = 1e38;
				fprintf(m_fp, " %.17g;", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(m_fp, " %.17g ];\n", dbl_value);
		}
		fprintf(m_fp, "	ssccall( 'data_set_array', data, '%s', %s );\n", name, name);
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
					csv.Set(r, c, wxString::Format("%.17g", dbl_value));
				}
			}
			fprintf(m_fp, "	%s = csvread( '%s');\n", name, (const char*)fn.c_str());
		}
		else
		{
			fprintf(m_fp, "	%s =[", name);
			for (int k = 0; k < (len - 1); k++)
			{
				dbl_value = (double)p[k];
				if (dbl_value > 1e38) dbl_value = 1e38;
				if (k%nc == (nc - 1))
					fprintf(m_fp, " %.17g ;", dbl_value);
				else 
					fprintf(m_fp, " %.17g  ", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(m_fp, " %.17g ];\n", dbl_value);
		}
		fprintf(m_fp, "	ssccall( 'data_set_matrix', data, '%s', %s );\n", name, name);
		// TODO tables in future
	}
	return true;
}


bool CodeGen_matlab::RunSSCModule(wxString &name)
{
	fprintf(m_fp, "	ok = ssccall('module_exec', module, data);\n");
	fprintf(m_fp, "	if ~ok,\n");
	fprintf(m_fp, "		disp('%s errors:'); \n", (const char*)name.c_str());
	fprintf(m_fp, "		ii=0;\n");
	fprintf(m_fp, "		while 1,\n");
	fprintf(m_fp, "			err = ssccall('module_log', module, ii);\n");
	fprintf(m_fp, "			if strcmp(err,''),\n");
	fprintf(m_fp, "			      break;\n");
	fprintf(m_fp, "			end\n");
	fprintf(m_fp, "			disp( err );\n");
	fprintf(m_fp, "			ii=ii+1;\n");
	fprintf(m_fp, "		end\n");
	fprintf(m_fp, "		return \n");
	fprintf(m_fp, "	end\n");
	return true;
}


bool CodeGen_matlab::Header()
{
	// top of file and supporting functions
	fprintf(m_fp, "function %s\n", (const char*)m_name.c_str());
	fprintf(m_fp, "function [result] = ssccall(action, arg0, arg1, arg2 )\n");
	fprintf(m_fp, "    [pathstr, fn, fext] = fileparts(mfilename('fullpath'));\n");
	fprintf(m_fp, "        ssclibpath = './';\n");
	fprintf(m_fp, "        ssclib = 'ssc';\n");
	fprintf(m_fp, "    if ~libisloaded(ssclib)\n");
	fprintf(m_fp, "        oldFolder = cd(pathstr);\n");
	fprintf(m_fp, "        loadlibrary(strcat(ssclibpath,ssclib),strcat(ssclibpath,'sscapi.h'));\n");
	fprintf(m_fp, "        cd(oldFolder);\n");
	fprintf(m_fp, "    end\n");
	fprintf(m_fp, "    if strcmp(action,'load')\n");
	fprintf(m_fp, "        if ~libisloaded(ssclib)\n");
	fprintf(m_fp, "            oldFolder = cd(pathstr);\n");
	fprintf(m_fp, "            loadlibrary(strcat(ssclibpath,ssclib),strcat(ssclibpath,'../sscapi.h'));\n");
	fprintf(m_fp, "            cd(oldFolder);\n");
	fprintf(m_fp, "        end\n");
	fprintf(m_fp, "    elseif strcmp(action,'unload')\n");
	fprintf(m_fp, "        if libisloaded(ssclib)\n");
	fprintf(m_fp, "            unloadlibrary(ssclib)    \n");
	fprintf(m_fp, "        end\n");
	fprintf(m_fp, "    elseif strcmp(action,'version')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_version');\n");
	fprintf(m_fp, "    elseif strcmp(action,'build_info')\n");
	fprintf(m_fp, "        result = calllib(ssclib, 'ssc_build_info');\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_create')\n");
	fprintf(m_fp, "        result = calllib(ssclib, 'ssc_data_create');\n");
	fprintf(m_fp, "        if ( isnullpointer(result) )\n");
	fprintf(m_fp, "            result = 0;\n");
	fprintf(m_fp, "        end\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_free')\n");
	fprintf(m_fp, "        result = calllib(ssclib, 'ssc_data_free', arg0);\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_unassign')\n");
	fprintf(m_fp, "        result = calllib(ssclib, 'ssc_data_unassign', arg0, arg1);\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_query')\n");
	fprintf(m_fp, "        result = calllib(ssclib, 'ssc_data_query', arg0, arg1 );\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_first')\n");
	fprintf(m_fp, "        result = calllib(ssclib, 'ssc_data_first', arg0 );\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_next')\n");
	fprintf(m_fp, "        result = calllib(ssclib, 'ssc_data_next', arg0 );\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_set_string')\n");
	fprintf(m_fp, "        result = calllib(ssclib, 'ssc_data_set_string', arg0, arg1, arg2 );\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_set_number')\n");
	fprintf(m_fp, "        result = calllib(ssclib, 'ssc_data_set_number', arg0, arg1, single(arg2) );\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_set_array')\n");
	fprintf(m_fp, "        len = length(arg2);\n");
	fprintf(m_fp, "        arr = libpointer( 'singlePtr', arg2 );\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_data_set_array',arg0,arg1,arr,len);\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_set_matrix')\n");
	fprintf(m_fp, "        [nr nc] = size(arg2);\n");
	fprintf(m_fp, "        mat = zeros(nr*nc, 1);\n");
	fprintf(m_fp, "        ii = 1;\n");
	fprintf(m_fp, "        for r=1:nr,\n");
	fprintf(m_fp, "            for c=1:nc,\n");
	fprintf(m_fp, "                mat(ii) = arg2(r,c);\n");
	fprintf(m_fp, "                ii=ii+1;\n");
	fprintf(m_fp, "            end\n");
	fprintf(m_fp, "        end\n");
	fprintf(m_fp, "        arr = libpointer( 'singlePtr', mat );\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_data_set_matrix',arg0,arg1,arr,nr,nc);\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_set_table')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_data_set_table',arg0,arg1,arg2);\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_get_string')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_data_get_string',arg0,arg1);\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_get_number')\n");
	fprintf(m_fp, "         p = libpointer('singlePtr',0);\n");
	fprintf(m_fp, "         calllib(ssclib,'ssc_data_get_number', arg0,arg1,p);\n");
	fprintf(m_fp, "         result = get(p,'Value');\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_get_array')\n");
	fprintf(m_fp, "        p_count = libpointer('int32Ptr',0);   \n");
	fprintf(m_fp, "        [xobj] = calllib(ssclib,'ssc_data_get_array',arg0,arg1,p_count);\n");
	fprintf(m_fp, "        setdatatype(xobj,'int32Ptr',p_count.Value,1);\n");
	fprintf(m_fp, "        len = p_count.Value;\n");
	fprintf(m_fp, "        result = zeros( len, 1 );\n");
	fprintf(m_fp, "        for i=1:len,\n");
	fprintf(m_fp, "            pidx = xobj+(i-1);\n");
	fprintf(m_fp, "            setdatatype(pidx,'singlePtr',1,1);\n");
	fprintf(m_fp, "            result(i) = pidx.Value;\n");
	fprintf(m_fp, "        end\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_get_matrix')\n");
	fprintf(m_fp, "        p_rows = libpointer('int32Ptr',0);\n");
	fprintf(m_fp, "        p_cols = libpointer('int32Ptr',0);\n");
	fprintf(m_fp, "        [xobj] = calllib(ssclib,'ssc_data_get_matrix',arg0,arg1,p_rows,p_cols);\n");
	fprintf(m_fp, "        setdatatype(xobj,'int32Ptr',p_rows.Value*p_cols.Value,1);\n");
	fprintf(m_fp, "        nrows = p_rows.Value;\n");
	fprintf(m_fp, "        ncols = p_cols.Value;\n");
	fprintf(m_fp, "        if ( nrows*ncols > 0 )\n");
	fprintf(m_fp, "            result = zeros( nrows, ncols );\n");
	fprintf(m_fp, "            ii=1;\n");
	fprintf(m_fp, "            for r=1:nrows,\n");
	fprintf(m_fp, "                for c=1:ncols,\n");
	fprintf(m_fp, "                    pidx = xobj+(ii-1);\n");
	fprintf(m_fp, "                    setdatatype(pidx,'singlePtr',1,1);\n");
	fprintf(m_fp, "                    result(r,c) = pidx.Value;\n");
	fprintf(m_fp, "                    ii=ii+1;\n");
	fprintf(m_fp, "                end\n");
	fprintf(m_fp, "            end\n");
	fprintf(m_fp, "        end\n");
	fprintf(m_fp, "    elseif strcmp(action,'data_get_table')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_data_get_table',arg0,arg1);\n");
	fprintf(m_fp, "    elseif strcmp(action,'module_entry')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_module_entry',arg0);\n");
	fprintf(m_fp, "        if isnullpointer( result ),\n");
	fprintf(m_fp, "            result = 0;\n");
	fprintf(m_fp, "        end\n");
	fprintf(m_fp, "    elseif strcmp(action,'entry_name')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_entry_name',arg0);\n");
	fprintf(m_fp, "    elseif strcmp(action,'entry_description')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_entry_description',arg0);\n");
	fprintf(m_fp, "    elseif strcmp(action,'entry_version')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_entry_version',arg0);\n");
	fprintf(m_fp, "    elseif strcmp(action,'module_var_info')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_module_var_info',arg0,arg1);\n");
	fprintf(m_fp, "        if isnullpointer( result ),\n");
	fprintf(m_fp, "            result = 0;\n");
	fprintf(m_fp, "        end\n");
	fprintf(m_fp, "    elseif strcmp(action,'info_var_type')\n");
	fprintf(m_fp, "        ty = calllib(ssclib,'ssc_info_var_type',arg0);\n");
	fprintf(m_fp, "        if (ty == 1)\n");
	fprintf(m_fp, "            result = 'input';\n");
	fprintf(m_fp, "        elseif ( ty==2 )\n");
	fprintf(m_fp, "            result = 'output';\n");
	fprintf(m_fp, "        else\n");
	fprintf(m_fp, "            result = 'inout';\n");
	fprintf(m_fp, "        end\n");
	fprintf(m_fp, "    elseif strcmp(action,'info_data_type')\n");
	fprintf(m_fp, "        dt = calllib(ssclib,'ssc_info_data_type',arg0);\n");
	fprintf(m_fp, "        if (dt == 1)\n");
	fprintf(m_fp, "            result = 'string';\n");
	fprintf(m_fp, "        elseif (dt == 2)\n");
	fprintf(m_fp, "            result = 'number';\n");
	fprintf(m_fp, "        elseif (dt == 3)\n");
	fprintf(m_fp, "            result = 'array';\n");
	fprintf(m_fp, "        elseif (dt == 4)\n");
	fprintf(m_fp, "            result = 'matrix';\n");
	fprintf(m_fp, "        elseif (dt == 5)\n");
	fprintf(m_fp, "            result = 'table';\n");
	fprintf(m_fp, "        else\n");
	fprintf(m_fp, "            result = 'invalid';\n");
	fprintf(m_fp, "        end\n");
	fprintf(m_fp, "    elseif strcmp(action,'info_name')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_info_name',arg0);\n");
	fprintf(m_fp, "    elseif strcmp(action,'info_label')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_info_label',arg0);\n");
	fprintf(m_fp, "    elseif strcmp(action,'info_units')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_info_units',arg0);\n");
	fprintf(m_fp, "    elseif strcmp(action,'info_meta')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_info_meta',arg0);\n");
	fprintf(m_fp, "    elseif strcmp(action,'info_group')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_info_group',arg0);\n");
	fprintf(m_fp, "    elseif strcmp(action,'info_required')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_info_required',arg0);\n");
	fprintf(m_fp, "    elseif strcmp(action,'info_constraints')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_info_constraints',arg0);\n");
	fprintf(m_fp, "    elseif strcmp(action,'info_uihint')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_info_uihint',arg0);\n");
	fprintf(m_fp, "    elseif strcmp(action,'exec_simple')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_module_exec_simple',arg0,arg1);\n");
	fprintf(m_fp, "    elseif strcmp(action,'exec_simple_nothread')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_module_exec_simple_nothread',arg0,arg1);\n");
	fprintf(m_fp, "    elseif strcmp(action,'module_create')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_module_create',arg0);\n");
	fprintf(m_fp, "        if ( isnullpointer(result) )\n");
	fprintf(m_fp, "            result = 0;\n");
	fprintf(m_fp, "        end\n");
	fprintf(m_fp, "    elseif strcmp(action,'module_free')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_module_free',arg0);\n");
	fprintf(m_fp, "    elseif strcmp(action,'module_exec')\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_module_exec',arg0,arg1);\n");
	fprintf(m_fp, "    elseif strcmp(action,'module_log')\n");
	fprintf(m_fp, "        p_type = libpointer('int32Ptr',1);\n");
	fprintf(m_fp, "        p_time = libpointer('singlePtr',1);\n");
	fprintf(m_fp, "        result = calllib(ssclib,'ssc_module_log', arg0, arg1, p_type, p_time);\n");
	fprintf(m_fp, "    elseif strcmp(action,'module_log_detailed')\n");
	fprintf(m_fp, "        p_type = libpointer('int32Ptr',1);\n");
	fprintf(m_fp, "        p_time = libpointer('singlePtr',1);\n");
	fprintf(m_fp, "        text = calllib(ssclib,'ssc_module_log', arg0, arg1, p_type, p_time);\n");
	fprintf(m_fp, "        typetext = 'notice';\n");
	fprintf(m_fp, "        if (p_type.Value == 2)\n");
	fprintf(m_fp, "            typetext = 'warning';\n");
	fprintf(m_fp, "        elseif (p_type.Value == 3)\n");
	fprintf(m_fp, "            typetext = 'error';\n");
	fprintf(m_fp, "        end\n");
	fprintf(m_fp, "        if ( strcmp(text,'') )\n");
	fprintf(m_fp, "            result = 0;\n");
	fprintf(m_fp, "        else\n");
	fprintf(m_fp, "            result = {text , typetext , p_time.Value};\n");
	fprintf(m_fp, "        end\n");
	fprintf(m_fp, "    else\n");
	fprintf(m_fp, "        disp( sprintf('ssccall: invalid action %s', action) );        \n");
	fprintf(m_fp, "        result = 0;\n");
	fprintf(m_fp, "    end\n");
	fprintf(m_fp, "end\n");
	fprintf(m_fp, "function bb = isnullpointer(p)\n");
	fprintf(m_fp, "    bb = false;\n");
	fprintf(m_fp, "    try\n");
	fprintf(m_fp, "        setdatatype(p, 'voidPtr', 1, 1);\n");
	fprintf(m_fp, "        deref = get(p);\n");
	fprintf(m_fp, "    catch\n");
	fprintf(m_fp, "        e = lasterror();\n");
	fprintf(m_fp, "        if strcmp(e.identifier, 'MATLAB:libpointer:ValueNotDefined')\n");
	fprintf(m_fp, "            bb = true;\n");
	fprintf(m_fp, "        end\n");
	fprintf(m_fp, "    end\n");
	fprintf(m_fp, "end\n");
	fprintf(m_fp, "clear\n");
	fprintf(m_fp, "ssccall('load');\n");
	fprintf(m_fp, "data = ssccall('data_create');\n");
	return true;
}

bool CodeGen_matlab::CreateSSCModule(wxString &name)
{
	if (name.IsNull() || name.Length() < 1)
		return false;
	else
	{
		fprintf(m_fp, "	module = ssccall('module_create', '%s'); \n", (const char*)name.c_str());
	}
	return true;
}

bool CodeGen_matlab::FreeSSCModule()
{
	fprintf(m_fp, "	ssccall('module_free', module);\n");
	return true;
}

bool CodeGen_matlab::Footer()
{
	fprintf(m_fp, "	ssccall('data_free', data);\n");
	fprintf(m_fp, "	ssccall('unload');\n");
	fprintf(m_fp, "end");
	return true;
}




// Python 2.7 code generation class

CodeGen_python::CodeGen_python(Case *cc, const wxString &folder) : CodeGen_Base(cc, folder)
{
}


bool CodeGen_python::Output(ssc_data_t p_data)
{
	//		fprintf(m_fp, "outln('%s ' + var('%s'));\n", (const char*)m_data[ii].label.c_str(), (const char*)m_data[ii].var.c_str());
	wxString str_value;
	for (size_t ii = 0; ii < m_data.size(); ii++)
	{
		const char *name = (const char*)m_data[ii].var.c_str();
		int type = ::ssc_data_query(p_data, name);
		switch (type)
		{
		case SSC_STRING:
			fprintf(m_fp, "	%s = ssc.data_set_string( data, '%s' );\n", name, name);
			fprintf(m_fp, " print '%s = ', %s\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_NUMBER:
			fprintf(m_fp, "	%s = ssc.data_get_number(data, '%s');\n", name, name);
			fprintf(m_fp, "	print '%s = ', %s\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_ARRAY:
			fprintf(m_fp, "	%s = ssc.data_get_array(data, '%s')", name, name);
			fprintf(m_fp, "	for i in range(len(%s)):\n", name);
			fprintf(m_fp, "		print '\tarray element: ' , i ,' = ' , %s[i]\n", name);
			break;
		case SSC_MATRIX:
			fprintf(m_fp, "	%s = ssc.data_get_matrix(data, '%s')", name, name);
			fprintf(m_fp, "	nrows = len(%s);\n", name);
			fprintf(m_fp, "	ncols = len(%s[0])\n", name);
			fprintf(m_fp, "	for i in range(nrows):\n");
			fprintf(m_fp, "		for j in range(ncols):\n");
			fprintf(m_fp, "			print '\treturned matrix element : (' , i , ', ' , j , ') = ' , %s[i][j]\n", name);
			break;
		}
	}
	return true;
}

bool CodeGen_python::Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold)
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
		fprintf(m_fp, "	ssc.data_set_string( data, '%s', '%s' );\n", name, (const char*)str_value.c_str());
		break;
	case SSC_NUMBER:
		::ssc_data_get_number(p_data, name, &value);
		dbl_value = (double)value;
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(m_fp, "	ssc.data_set_number( data, '%s', %.17g )\n", name, dbl_value);
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
				csv.Set(i, 0, wxString::Format("%.17g", dbl_value));
			}
			csv.WriteFile(fn);
			fprintf(m_fp, "	ssc.data_set_array_from_csv( data, '%s', '%s');\n", name, (const char*)fn.c_str());
		}
		else
		{
			fprintf(m_fp, "	%s =[", name);
			for (int i = 0; i < (len - 1); i++)
			{
				dbl_value = (double)p[i];
				if (dbl_value > 1e38) dbl_value = 1e38;
				fprintf(m_fp, " %.17g,", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(m_fp, " %.17g ];\n", dbl_value);
			fprintf(m_fp, "	ssc.data_set_array( data, '%s',  %s);\n", name, name);
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
					csv.Set(r, c, wxString::Format("%.17g", dbl_value));
				}
			}
			csv.WriteFile(fn);
			fprintf(m_fp, "	ssc.data_set_matrix_from_csv( data, '%s', '%s');\n", name, (const char*)fn.c_str());
		}
		else
		{
			fprintf(m_fp, "	%s = [[", name);
			for (int k = 0; k < (len - 1); k++)
			{
				dbl_value = (double)p[k];
				if (dbl_value > 1e38) dbl_value = 1e38;
				if (k%nc == (nc - 1))
					fprintf(m_fp, " %.17g ], [", dbl_value);
				else
					fprintf(m_fp, " %.17g,  ", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(m_fp, " %.17g ]];\n", dbl_value);
			fprintf(m_fp, "	ssc.data_set_matrix( data,  '%s', %s );\n", name, name);
		}
		break;
		// TODO tables in future
	}
	return true;
}


bool CodeGen_python::RunSSCModule(wxString &name)
{
	fprintf(m_fp, "	ssc.module_exec_set_print( 0 );\n");
	fprintf(m_fp, "	if ssc.module_exec(module, data) == 0:\n");
	fprintf(m_fp, "		print '%s simulation error'\n", (const char*)name.c_str());
	fprintf(m_fp, "		idx = 1\n");
	fprintf(m_fp, "		msg = ssc.module_log(module, 0)\n");
	fprintf(m_fp, "		while (msg != None):\n");
	fprintf(m_fp, "			print '\t: ' + msg\n");
	fprintf(m_fp, "			msg = ssc.module_log(module, idx)\n");
	fprintf(m_fp, "			idx = idx + 1\n");
	fprintf(m_fp, "		sys.exit( \"Simulation Error\" );\n");
	return true;
}


bool CodeGen_python::Header()
{
	// top of file and supporting functions
	fprintf(m_fp, "import string, sys, struct, os\n");
	fprintf(m_fp, "from ctypes import *\n");
	fprintf(m_fp, "c_number = c_float # must be c_double or c_float depending on how defined in sscapi.h\n");
	fprintf(m_fp, "class PySSC:\n");
	fprintf(m_fp, "	def __init__(self):\n");
	fprintf(m_fp, "		if sys.platform == 'win32' or sys.platform == 'cygwin':\n");
	fprintf(m_fp, "			if 8*struct.calcsize(\"P\") == 64:\n");
	fprintf(m_fp, "				self.pdll = CDLL(\"ssc.dll\") \n");
	fprintf(m_fp, "			else:\n");
	fprintf(m_fp, "				self.pdll = CDLL(\"ssc.dll\") \n");
	fprintf(m_fp, "		elif sys.platform == 'darwin':\n");
	fprintf(m_fp, "			self.pdll = CDLL(\"ssc.dylib\") \n");
	fprintf(m_fp, "		elif sys.platform == 'linux2':\n");
	fprintf(m_fp, "			self.pdll = CDLL('./ssc.so')   # instead of relative path, require user to have on LD_LIBRARY_PATH\n");
	fprintf(m_fp, "		else:\n");
	fprintf(m_fp, "			print \"Platform not supported \", sys.platform\n");
	fprintf(m_fp, "	INVALID=0\n");
	fprintf(m_fp, "	STRING=1\n");
	fprintf(m_fp, "	NUMBER=2\n");
	fprintf(m_fp, "	ARRAY=3\n");
	fprintf(m_fp, "	MATRIX=4\n");
	fprintf(m_fp, "	INPUT=1\n");
	fprintf(m_fp, "	OUTPUT=2\n");
	fprintf(m_fp, "	INOUT=3\n");
	fprintf(m_fp, "	def version(self):\n");
	fprintf(m_fp, "		self.pdll.ssc_version.restype = c_int\n");
	fprintf(m_fp, "		return self.pdll.ssc_version()\n");
	fprintf(m_fp, "	def data_create(self):\n");
	fprintf(m_fp, "		self.pdll.ssc_data_create.restype = c_void_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_data_create()\n");
	fprintf(m_fp, "	def data_free(self, p_data):\n");
	fprintf(m_fp, "		self.pdll.ssc_data_free( c_void_p(p_data) )\n");
	fprintf(m_fp, "	def data_clear(self, p_data):\n");
	fprintf(m_fp, "		self.pdll.ssc_data_clear( c_void_p(p_data) )\n");
	fprintf(m_fp, "	def data_unassign(self, p_data, name):\n");
	fprintf(m_fp, "		self.pdll.ssc_data_unassign( c_void_p(p_data), c_char_p(name) )\n");
	fprintf(m_fp, "	def data_query(self, p_data, name):\n");
	fprintf(m_fp, "		self.pdll.ssc_data_query.restype = c_int\n");
	fprintf(m_fp, "		return self.pdll.ssc_data_query( c_void_p(p_data), c_char_p(name) )\n");
	fprintf(m_fp, "	def data_first(self, p_data):\n");
	fprintf(m_fp, "		self.pdll.ssc_data_first.restype = c_char_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_data_first( c_void_p(p_data) )\n");
	fprintf(m_fp, "	def data_next(self, p_data):\n");
	fprintf(m_fp, "		self.pdll.ssc_data_next.restype = c_char_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_data_next( c_void_p(p_data) )\n");
	fprintf(m_fp, "	def data_set_string(self, p_data, name, value):\n");
	fprintf(m_fp, "		self.pdll.ssc_data_set_string( c_void_p(p_data), c_char_p(name), c_char_p(value) )\n");
	fprintf(m_fp, "	def data_set_number(self, p_data, name, value):\n");
	fprintf(m_fp, "		self.pdll.ssc_data_set_number( c_void_p(p_data), c_char_p(name), c_number(value) )\n");
	fprintf(m_fp, "	def data_set_array(self,p_data,name,parr):\n");
	fprintf(m_fp, "		count = len(parr)\n");
	fprintf(m_fp, "		arr = (c_number*count)()\n");
	fprintf(m_fp, "		arr[:] = parr # set all at once instead of looping\n");
	fprintf(m_fp, "		return self.pdll.ssc_data_set_array( c_void_p(p_data), c_char_p(name),pointer(arr), c_int(count))\n");
	fprintf(m_fp, "	def data_set_array_from_csv(self, p_data, name, fn) :\n");
	fprintf(m_fp, "		f = open(fn, 'rb'); \n");
	fprintf(m_fp, "		data = []; \n");
	fprintf(m_fp, "		for line in f : \n");
	fprintf(m_fp, "			data.extend([n for n in map(float, line.split(','))])\n");
	fprintf(m_fp, "		f.close(); \n");
	fprintf(m_fp, "		return self.data_set_array(p_data, name, data); \n");
	fprintf(m_fp, "	def data_set_matrix(self,p_data,name,mat):\n");
	fprintf(m_fp, "		nrows = len(mat)\n");
	fprintf(m_fp, "		ncols = len(mat[0])\n");
	fprintf(m_fp, "		size = nrows*ncols\n");
	fprintf(m_fp, "		arr = (c_number*size)()\n");
	fprintf(m_fp, "		idx=0\n");
	fprintf(m_fp, "		for r in range(nrows):\n");
	fprintf(m_fp, "			for c in range(ncols):\n");
	fprintf(m_fp, "				arr[idx] = c_number(mat[r][c])\n");
	fprintf(m_fp, "				idx=idx+1\n");
	fprintf(m_fp, "		return self.pdll.ssc_data_set_matrix( c_void_p(p_data), c_char_p(name),pointer(arr), c_int(nrows), c_int(ncols))\n");
	fprintf(m_fp, "	def data_set_matrix_from_csv(self, p_data, name, fn) :\n");
	fprintf(m_fp, "		f = open(fn, 'rb'); \n");
	fprintf(m_fp, "		data = []; \n");
	fprintf(m_fp, "		for line in f : \n");
	fprintf(m_fp, "			lst = ([n for n in map(float, line.split(','))])\n");
	fprintf(m_fp, "			data.append(lst);\n");
	fprintf(m_fp, "		f.close(); \n");
	fprintf(m_fp, "		return self.data_set_matrix(p_data, name, data); \n");
	fprintf(m_fp, "	def data_set_table(self,p_data,name,tab):\n");
	fprintf(m_fp, "		return self.pdll.ssc_data_set_table( c_void_p(p_data), c_char_p(name), c_void_p(tab) );\n");
	fprintf(m_fp, "	def data_get_string(self, p_data, name):\n");
	fprintf(m_fp, "		self.pdll.ssc_data_get_string.restype = c_char_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_data_get_string( c_void_p(p_data), c_char_p(name) )\n");
	fprintf(m_fp, "	def data_get_number(self, p_data, name):\n");
	fprintf(m_fp, "		val = c_number(0)\n");
	fprintf(m_fp, "		self.pdll.ssc_data_get_number( c_void_p(p_data), c_char_p(name), byref(val) )\n");
	fprintf(m_fp, "		return val.value\n");
	fprintf(m_fp, "	def data_get_array(self,p_data,name):\n");
	fprintf(m_fp, "		count = c_int()\n");
	fprintf(m_fp, "		self.pdll.ssc_data_get_array.restype = POINTER(c_number)\n");
	fprintf(m_fp, "		parr = self.pdll.ssc_data_get_array( c_void_p(p_data), c_char_p(name), byref(count))\n");
	fprintf(m_fp, "		arr = parr[0:count.value] # extract all at once			\n");
	fprintf(m_fp, "		return arr\n");
	fprintf(m_fp, "	def data_get_matrix(self,p_data,name):\n");
	fprintf(m_fp, "		nrows = c_int()\n");
	fprintf(m_fp, "		ncols = c_int()\n");
	fprintf(m_fp, "		self.pdll.ssc_data_get_matrix.restype = POINTER(c_number)\n");
	fprintf(m_fp, "		parr = self.pdll.ssc_data_get_matrix( c_void_p(p_data), c_char_p(name), byref(nrows), byref(ncols) )\n");
	fprintf(m_fp, "		idx = 0\n");
	fprintf(m_fp, "		mat = []\n");
	fprintf(m_fp, "		for r in range(nrows.value):\n");
	fprintf(m_fp, "			row = []\n");
	fprintf(m_fp, "			for c in range(ncols.value):\n");
	fprintf(m_fp, "				row.append( float(parr[idx]) )\n");
	fprintf(m_fp, "				idx = idx + 1\n");
	fprintf(m_fp, "			mat.append(row)\n");
	fprintf(m_fp, "		return mat\n");
	fprintf(m_fp, "	# don't call data_free() on the result, it's an internal\n");
	fprintf(m_fp, "	# pointer inside SSC\n");
	fprintf(m_fp, "	def data_get_table(self,p_data,name): \n");
	fprintf(m_fp, "		return self.pdll.ssc_data_get_table( c_void_p(p_data), name );\n");
	fprintf(m_fp, "	def module_entry(self,index):\n");
	fprintf(m_fp, "		self.pdll.ssc_module_entry.restype = c_void_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_module_entry( c_int(index) )\n");
	fprintf(m_fp, "	def entry_name(self,p_entry):\n");
	fprintf(m_fp, "		self.pdll.ssc_entry_name.restype = c_char_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_entry_name( c_void_p(p_entry) )\n");
	fprintf(m_fp, "	def entry_description(self,p_entry):\n");
	fprintf(m_fp, "		self.pdll.ssc_entry_description.restype = c_char_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_entry_description( c_void_p(p_entry) )\n");
	fprintf(m_fp, "	def entry_version(self,p_entry):\n");
	fprintf(m_fp, "		self.pdll.ssc_entry_version.restype = c_int\n");
	fprintf(m_fp, "		return self.pdll.ssc_entry_version( c_void_p(p_entry) )\n");
	fprintf(m_fp, "	def module_create(self,name):\n");
	fprintf(m_fp, "		self.pdll.ssc_module_create.restype = c_void_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_module_create( c_char_p(name) )\n");
	fprintf(m_fp, "	def module_free(self,p_mod):\n");
	fprintf(m_fp, "		self.pdll.ssc_module_free( c_void_p(p_mod) )\n");
	fprintf(m_fp, "	def module_var_info(self,p_mod,index):\n");
	fprintf(m_fp, "		self.pdll.ssc_module_var_info.restype = c_void_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_module_var_info( c_void_p(p_mod), c_int(index) )\n");
	fprintf(m_fp, "	def info_var_type( self, p_inf ):\n");
	fprintf(m_fp, "		return self.pdll.ssc_info_var_type( c_void_p(p_inf) )\n");
	fprintf(m_fp, "	def info_data_type( self, p_inf ):\n");
	fprintf(m_fp, "		return self.pdll.ssc_info_data_type( c_void_p(p_inf) )\n");
	fprintf(m_fp, "	def info_name( self, p_inf ):\n");
	fprintf(m_fp, "		self.pdll.ssc_info_name.restype = c_char_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_info_name( c_void_p(p_inf) )\n");
	fprintf(m_fp, "	def info_label( self, p_inf ):\n");
	fprintf(m_fp, "		self.pdll.ssc_info_label.restype = c_char_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_info_label( c_void_p(p_inf) )\n");
	fprintf(m_fp, "	def info_units( self, p_inf ):\n");
	fprintf(m_fp, "		self.pdll.ssc_info_units.restype = c_char_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_info_units( c_void_p(p_inf) )\n");
	fprintf(m_fp, "	def info_meta( self, p_inf ):\n");
	fprintf(m_fp, "		self.pdll.ssc_info_meta.restype = c_char_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_info_meta( c_void_p(p_inf) )\n");
	fprintf(m_fp, "	def info_group( self, p_inf ):\n");
	fprintf(m_fp, "		self.pdll.ssc_info_group.restype = c_char_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_info_group( c_void_p(p_inf) )\n");
	fprintf(m_fp, "	def info_uihint( self, p_inf ):\n");
	fprintf(m_fp, "		self.pdll.ssc_info_uihint.restype = c_char_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_info_uihint( c_void_p(p_inf) )\n");
	fprintf(m_fp, "	def module_exec( self, p_mod, p_data ):\n");
	fprintf(m_fp, "		self.pdll.ssc_module_exec.restype = c_int\n");
	fprintf(m_fp, "		return self.pdll.ssc_module_exec( c_void_p(p_mod), c_void_p(p_data) )\n");
	fprintf(m_fp, "		ssc_module_exec_simple_nothread\n");
	fprintf(m_fp, "	def module_exec_simple_no_thread( self, modname, data ):\n");
	fprintf(m_fp, "		self.pdll.ssc_module_exec_simple_nothread.restype = c_char_p;\n");
	fprintf(m_fp, "		return self.pdll.ssc_module_exec_simple_nothread( c_char_p(modname), c_void_p(data) );\n");
	fprintf(m_fp, "	def module_log( self, p_mod, index ):\n");
	fprintf(m_fp, "		log_type = c_int()\n");
	fprintf(m_fp, "		time = c_float()\n");
	fprintf(m_fp, "		self.pdll.ssc_module_log.restype = c_char_p\n");
	fprintf(m_fp, "		return self.pdll.ssc_module_log( c_void_p(p_mod), c_int(index), byref(log_type), byref(time) )\n");
	fprintf(m_fp, "	def module_exec_set_print( self, prn ):\n");
	fprintf(m_fp, "		return self.pdll.ssc_module_exec_set_print( c_int(prn) );\n");
	fprintf(m_fp, "if __name__ == \"__main__\":\n");
	fprintf(m_fp, "	ssc = PySSC()\n");
	fprintf(m_fp, "	data = ssc.data_create()\n");

	return true;
}

bool CodeGen_python::CreateSSCModule(wxString &name)
{
	if (name.IsNull() || name.Length() < 1)
		return false;
	else
	{
		fprintf(m_fp, "	module = ssc.module_create(\"%s\")	\n", (const char*)name.c_str());
	}
	return true;
}

bool CodeGen_python::FreeSSCModule()
{
	fprintf(m_fp, "	ssc.module_free(module)\n");
	return true;
}

bool CodeGen_python::Footer()
{
	fprintf(m_fp, "	ssc.data_free(data);");
	return true;
}





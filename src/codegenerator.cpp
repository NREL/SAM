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

bool CodeGen_Base::PlatformFiles()
{
	// copy appropriate dll
#if defined(__WXMSW__)
	wxString f1 = SamApp::GetAppPath() + "/ssc.dll";
	wxString f2 = m_folder + "/ssc.dll";
#elif defined(__WXOSX__)
	wxString f1 = SamApp::GetAppPath() + "/../Frameworks/ssc.dylib";
	wxString f2 = m_folder + "/ssc.dylib";
#elif defined(__WXGTK__)
	wxString f1 = SamApp::GetAppPath() + "/ssc.so";
	wxString f2 = m_folder + "/ssc.so";
#else
	return false;
#endif
	wxCopyFile(f1, f2);
	// sscapi.h
	wxString fn = m_folder + "/sscapi.h";
	FILE* f = fopen(fn.c_str(), "w");
	if (!f) return false;
	fprintf(f, "#ifndef __ssc_api_h\n");
	fprintf(f, "#define __ssc_api_h\n");
	fprintf(f, "#if defined(__WINDOWS__)&&defined(__DLL__)\n");
	fprintf(f, "#define SSCEXPORT __declspec(dllexport)\n");
	fprintf(f, "#else\n");
	fprintf(f, "#define SSCEXPORT\n");
	fprintf(f, "#endif\n");
	fprintf(f, "#ifndef __SSCLINKAGECPP__\n");
	fprintf(f, "#ifdef __cplusplus\n");
	fprintf(f, "extern \"C\" {\n");
	fprintf(f, "#endif\n");
	fprintf(f, "#endif\n");
	fprintf(f, "SSCEXPORT int ssc_version();\n");
	fprintf(f, "SSCEXPORT const char *ssc_build_info();\n");
	fprintf(f, "typedef void* ssc_data_t;\n");
	fprintf(f, "typedef float ssc_number_t;\n");
	fprintf(f, "typedef int ssc_bool_t;\n");
	fprintf(f, "#define SSC_INVALID 0\n");
	fprintf(f, "#define SSC_STRING 1\n");
	fprintf(f, "#define SSC_NUMBER 2\n");
	fprintf(f, "#define SSC_ARRAY 3\n");
	fprintf(f, "#define SSC_MATRIX 4\n");
	fprintf(f, "#define SSC_TABLE 5\n");
	fprintf(f, "SSCEXPORT ssc_data_t ssc_data_create();\n");
	fprintf(f, "SSCEXPORT void ssc_data_free( ssc_data_t p_data );\n");
	fprintf(f, "SSCEXPORT void ssc_data_clear( ssc_data_t p_data );\n");
	fprintf(f, "SSCEXPORT void ssc_data_unassign( ssc_data_t p_data, const char *name );\n");
	fprintf(f, "SSCEXPORT int ssc_data_query( ssc_data_t p_data, const char *name );\n");
	fprintf(f, "SSCEXPORT const char *ssc_data_first( ssc_data_t p_data );\n");
	fprintf(f, "SSCEXPORT const char *ssc_data_next( ssc_data_t p_data );\n");
	fprintf(f, "SSCEXPORT void ssc_data_set_string( ssc_data_t p_data, const char *name, const char *value );\n");
	fprintf(f, "SSCEXPORT void ssc_data_set_number( ssc_data_t p_data, const char *name, ssc_number_t value );\n");
	fprintf(f, "SSCEXPORT void ssc_data_set_array( ssc_data_t p_data, const char *name, ssc_number_t *pvalues, int length );\n");
	fprintf(f, "SSCEXPORT void ssc_data_set_matrix( ssc_data_t p_data, const char *name, ssc_number_t *pvalues, int nrows, int ncols );\n");
	fprintf(f, "SSCEXPORT void ssc_data_set_table( ssc_data_t p_data, const char *name, ssc_data_t table );\n");
	fprintf(f, "SSCEXPORT const char *ssc_data_get_string( ssc_data_t p_data, const char *name );\n");
	fprintf(f, "SSCEXPORT ssc_bool_t ssc_data_get_number( ssc_data_t p_data, const char *name, ssc_number_t *value );\n");
	fprintf(f, "SSCEXPORT ssc_number_t *ssc_data_get_array( ssc_data_t p_data, const char *name, int *length );\n");
	fprintf(f, "SSCEXPORT ssc_number_t *ssc_data_get_matrix( ssc_data_t p_data, const char *name, int *nrows, int *ncols );\n");
	fprintf(f, "SSCEXPORT ssc_data_t ssc_data_get_table( ssc_data_t p_data, const char *name );\n");
	fprintf(f, "typedef void* ssc_entry_t;\n");
	fprintf(f, "SSCEXPORT ssc_entry_t ssc_module_entry( int index );\n");
	fprintf(f, "SSCEXPORT const char *ssc_entry_name( ssc_entry_t p_entry );\n");
	fprintf(f, "SSCEXPORT const char *ssc_entry_description( ssc_entry_t p_entry );\n");
	fprintf(f, "SSCEXPORT int ssc_entry_version( ssc_entry_t p_entry );\n");
	fprintf(f, "typedef void* ssc_module_t;\n");
	fprintf(f, "typedef void* ssc_info_t;\n");
	fprintf(f, "SSCEXPORT ssc_module_t ssc_module_create( const char *name );\n");
	fprintf(f, "SSCEXPORT void ssc_module_free( ssc_module_t p_mod );\n");
	fprintf(f, "#define SSC_INPUT 1\n");
	fprintf(f, "#define SSC_OUTPUT 2\n");
	fprintf(f, "#define SSC_INOUT 3\n");
	fprintf(f, "SSCEXPORT const ssc_info_t ssc_module_var_info( ssc_module_t p_mod, int index );\n");
	fprintf(f, "SSCEXPORT int ssc_info_var_type( ssc_info_t p_inf );\n");
	fprintf(f, "SSCEXPORT int ssc_info_data_type( ssc_info_t p_inf );\n");
	fprintf(f, "SSCEXPORT const char *ssc_info_name( ssc_info_t p_inf );\n");
	fprintf(f, "SSCEXPORT const char *ssc_info_label( ssc_info_t p_inf );\n");
	fprintf(f, "SSCEXPORT const char *ssc_info_units( ssc_info_t p_inf );\n");
	fprintf(f, "SSCEXPORT const char *ssc_info_meta( ssc_info_t p_inf );\n");
	fprintf(f, "SSCEXPORT const char *ssc_info_group( ssc_info_t p_inf );\n");
	fprintf(f, "SSCEXPORT const char *ssc_info_required( ssc_info_t p_inf );\n");
	fprintf(f, "SSCEXPORT const char *ssc_info_constraints( ssc_info_t p_inf );\n");
	fprintf(f, "SSCEXPORT const char *ssc_info_uihint( ssc_info_t p_inf );\n");
	fprintf(f, "SSCEXPORT void ssc_module_exec_set_print( int print );\n");
	fprintf(f, "SSCEXPORT ssc_bool_t ssc_module_exec_simple( const char *name, ssc_data_t p_data );\n");
	fprintf(f, "SSCEXPORT const char *ssc_module_exec_simple_nothread( const char *name, ssc_data_t p_data );\n");
	fprintf(f, "#define SSC_LOG 0\n");
	fprintf(f, "#define SSC_UPDATE 1\n");
	fprintf(f, "SSCEXPORT ssc_bool_t ssc_module_exec( ssc_module_t p_mod, ssc_data_t p_data );\n");
	fprintf(f, "typedef void* ssc_handler_t;\n");
	fprintf(f, "SSCEXPORT ssc_bool_t ssc_module_exec_with_handler(\n");
	fprintf(f, "	ssc_module_t p_mod,\n");
	fprintf(f, "	ssc_data_t p_data,\n");
	fprintf(f, "	ssc_bool_t (*pf_handler)( ssc_module_t, ssc_handler_t, int action, float f0, float f1, const char *s0, const char *s1, void *user_data ),\n");
	fprintf(f, "	void *pf_user_data );\n");
	fprintf(f, "#define SSC_NOTICE 1\n");
	fprintf(f, "#define SSC_WARNING 2\n");
	fprintf(f, "#define SSC_ERROR 3\n");
	fprintf(f, "SSCEXPORT const char *ssc_module_log( ssc_module_t p_mod, int index, int *item_type, float *time );\n");
	fprintf(f, "SSCEXPORT void __ssc_segfault();\n");
	fprintf(f, "#ifndef __SSCLINKAGECPP__\n");
	fprintf(f, "#ifdef __cplusplus\n");
	fprintf(f, "}\n");
	fprintf(f, "#endif\n");
	fprintf(f, "\n");
	fprintf(f, "#endif // __SSCLINKAGECPP__\n");
	fprintf(f, "\n");
	fprintf(f, "#endif\n");
	fclose(f);
	return true;
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
	// Platform specific files
	if (!PlatformFiles())
		m_errors.Add("PlatformFiles failed");

	// language specific additional files
	if (!SupportingFiles())
		m_errors.Add("SupportingFiles failed");

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
	code_languages.Add("matlab");
	code_languages.Add("python");
	code_languages.Add("java");
	code_languages.Add("php5");
	code_languages.Add("php7");
#ifdef __WXMSW__
	code_languages.Add("c#");
#endif

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
	//int threshold = 0; // testing writing out all array and matrix values to external files
	//	int threshold = 100000; // testing writing out all values with no external files
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
	else if (lang == 2) // matlab
	{
		fn += ".m";
		cg = new CodeGen_matlab(c, fn);
	}
	else if (lang == 3) // python
	{
		fn += ".py";
		cg = new CodeGen_python(c, fn);
	}
	else if (lang == 4) // java
	{
		fn += ".java";
		cg = new CodeGen_java(c, fn);
	}
	else if (lang == 5) // php
	{
		fn += ".php";
		cg = new CodeGen_php5(c, fn);
	}
	else if (lang == 6) // php
	{
		fn += ".php";
		cg = new CodeGen_php7(c, fn);
	}
#ifdef __WXMSW__
	else if (lang == 7) // c#
	{
		fn += ".cs";
		cg = new CodeGen_csharp(c, fn);
	}
#endif
	else
		return false;

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

bool CodeGen_lk::SupportingFiles()
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
	fprintf(m_fp, "		case SSC_NOTICE: printf(\"Notice: %%s\", s0); break;\n");
	fprintf(m_fp, "		case SSC_WARNING: printf(\"Warning: %%s\", s0); break;\n");
	fprintf(m_fp, "		case SSC_ERROR: printf(\"Error: %%s\", s0); break;\n");
	fprintf(m_fp, "		}\n");
	fprintf(m_fp, "		return 1;\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	else if (action == SSC_UPDATE)\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		// print status update to console\n");
	fprintf(m_fp, "		printf(\"(%%.2f %%) %%s\", f0, s0);\n");
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
	fprintf(m_fp, "	if (fp == NULL)\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		printf(\"file opening failed \");\n");
	fprintf(m_fp, "		return false;\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	ary = (ssc_number_t *)malloc(len * sizeof(ssc_number_t));\n");
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
	fprintf(m_fp, "	fclose(fp);\n");
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
	fprintf(m_fp, "	if (fp == NULL)\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		printf(\"file opening failed \");\n");
	fprintf(m_fp, "		return false;\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	ary = (ssc_number_t *)malloc(len * sizeof(ssc_number_t));\n");
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
	fprintf(m_fp, "	fclose(fp);\n");
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

bool CodeGen_c::SupportingFiles()
{
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

bool CodeGen_csharp::SupportingFiles()
{
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

bool CodeGen_matlab::SupportingFiles()
{
	return true;
}

bool CodeGen_matlab::Footer()
{
	fprintf(m_fp, "	ssccall('data_free', data);\n");
	fprintf(m_fp, "	ssccall('unload');\n");
	fprintf(m_fp, "end");
	return true;
}




// Python 2.7.8 and 3.4.2 code generation class

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
			fprintf(m_fp, "	%s = ssc.data_set_string( data, b'%s' );\n", name, name);
			fprintf(m_fp, " print ('%s = ', %s)\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_NUMBER:
			fprintf(m_fp, "	%s = ssc.data_get_number(data, b'%s');\n", name, name);
			fprintf(m_fp, "	print ('%s = ', %s)\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_ARRAY:
			fprintf(m_fp, "	%s = ssc.data_get_array(data, b'%s')", name, name);
			fprintf(m_fp, "	for i in range(len(%s)):\n", name);
			fprintf(m_fp, "		print ('\tarray element: ' , i ,' = ' , %s[i])\n", name);
			break;
		case SSC_MATRIX:
			fprintf(m_fp, "	%s = ssc.data_get_matrix(data, b'%s')", name, name);
			fprintf(m_fp, "	nrows = len(%s);\n", name);
			fprintf(m_fp, "	ncols = len(%s[0])\n", name);
			fprintf(m_fp, "	for i in range(nrows):\n");
			fprintf(m_fp, "		for j in range(ncols):\n");
			fprintf(m_fp, "			print ('\treturned matrix element : (' , i , ', ' , j , ') = ' , %s[i][j])\n", name);
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
		fprintf(m_fp, "	ssc.data_set_string( data, b'%s', b'%s' );\n", name, (const char*)str_value.c_str());
		break;
	case SSC_NUMBER:
		::ssc_data_get_number(p_data, name, &value);
		dbl_value = (double)value;
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(m_fp, "	ssc.data_set_number( data, b'%s', %.17g )\n", name, dbl_value);
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
			fprintf(m_fp, "	ssc.data_set_array_from_csv( data, b'%s', b'%s');\n", name, (const char*)fn.c_str());
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
			fprintf(m_fp, "	ssc.data_set_array( data, b'%s',  %s);\n", name, name);
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
			fprintf(m_fp, "	ssc.data_set_matrix_from_csv( data, b'%s', b'%s');\n", name, (const char*)fn.c_str());
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
			fprintf(m_fp, "	ssc.data_set_matrix( data, b'%s', %s );\n", name, name);
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
	fprintf(m_fp, "		print ('%s simulation error')\n", (const char*)name.c_str());
	fprintf(m_fp, "		idx = 1\n");
	fprintf(m_fp, "		msg = ssc.module_log(module, 0)\n");
	fprintf(m_fp, "		while (msg != None):\n");
	fprintf(m_fp, "			print ('\t: ' + msg)\n");
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
	fprintf(m_fp, "			print ('Platform not supported ', sys.platform)\n");
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
	fprintf(m_fp, "			data.extend([n for n in map(float, line.split(b','))])\n");
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
	fprintf(m_fp, "			lst = ([n for n in map(float, line.split(b','))])\n");
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
		fprintf(m_fp, "	module = ssc.module_create(b'%s')	\n", (const char*)name.c_str());
	}
	return true;
}

bool CodeGen_python::FreeSSCModule()
{
	fprintf(m_fp, "	ssc.module_free(module)\n");
	return true;
}

bool CodeGen_python::SupportingFiles()
{
	return true;
}

bool CodeGen_python::Footer()
{
	fprintf(m_fp, "	ssc.data_free(data);");
	return true;
}


// java code generation class

CodeGen_java::CodeGen_java(Case *cc, const wxString &folder) : CodeGen_Base(cc, folder)
{
}


bool CodeGen_java::Output(ssc_data_t p_data)
{
	wxString str_value;
	for (size_t ii = 0; ii < m_data.size(); ii++)
	{
		const char *name = (const char*)m_data[ii].var.c_str();
		int type = ::ssc_data_query(p_data, name);
		switch (type)
		{
		case SSC_STRING:// TODO
			fprintf(m_fp, "	const char *%s = ssc_data_get_string( data, \"%s\" );\n", name, name);
			fprintf(m_fp, "	printf(\"%%s = %%s\"), %s, %s);\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_NUMBER:
			fprintf(m_fp, "		float[] %s = {0.0f};\n", name);
			fprintf(m_fp, "		api.ssc_data_get_number(data, \"%s\", %s);\n", name, name);
			fprintf(m_fp, "		System.out.println(\"%s = \" + %s[0]);\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_ARRAY: // TODO
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

bool CodeGen_java::Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold)
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
		fprintf(m_fp, "		api.ssc_data_set_string( data, \"%s\", \"%s\" );\n", name, (const char*)str_value.c_str());
		break;
	case SSC_NUMBER:
		::ssc_data_get_number(p_data, name, &value);
		dbl_value = (double)value;
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(m_fp, "		api.ssc_data_set_number( data, \"%s\", %.17gf );\n", name, dbl_value);
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
			fprintf(m_fp, "		set_array( data, \"%s\", \"%s\", %d);\n", name, (const char*)fn.c_str(), len);
		}
		else
		{
			fprintf(m_fp, "		float[] p_%s = {", name);
			for (int i = 0; i < (len - 1); i++)
			{
				dbl_value = (double)p[i];
				if (dbl_value > 1e38) dbl_value = 1e38;
				fprintf(m_fp, " %.17gf,", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(m_fp, " %.17gf };\n", dbl_value);
			fprintf(m_fp, "		api.ssc_data_set_array( data, \"%s\", p_%s, %d );\n", name, name, len);
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
			fprintf(m_fp, "		set_matrix( data, \"%s\", \"%s\", %d, %d);\n", name, (const char*)fn.c_str(), nr, nc);
		}
		else
		{
			fprintf(m_fp, "		float[] p_%s ={ ", name);
			for (int k = 0; k < (len - 1); k++)
			{
				dbl_value = (double)p[k];
				if (dbl_value > 1e38) dbl_value = 1e38;
				fprintf(m_fp, " %.17gf, ", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(m_fp, " %.17gf };\n", dbl_value);
			fprintf(m_fp, "		api.ssc_data_set_matrix( data, \"%s\", p_%s, %d, %d );\n", name, name, nr, nc);
		}
		// TODO tables in future
	}
	return true;
}


bool CodeGen_java::RunSSCModule(wxString &name)
{// TODO
	fprintf(m_fp, "		if (api.ssc_module_exec(mod,data)==0)\n");
	fprintf(m_fp, "		{\n");
	fprintf(m_fp, "			System.out.println(\"error during simulation.\"); \n");
	fprintf(m_fp, "			api.ssc_module_free(mod); \n");
	fprintf(m_fp, "			api.ssc_data_free(data);  \n");
	fprintf(m_fp, "			return; \n");
	fprintf(m_fp, "		}\n");
	return true;
}


bool CodeGen_java::Header()
{
	// top of file and supporting functions
	fprintf(m_fp, "import java.io.BufferedReader;\n");
	fprintf(m_fp, "import java.io.FileNotFoundException;\n");
	fprintf(m_fp, "import java.io.FileReader;\n");
	fprintf(m_fp, "import java.io.IOException;\n");
	fprintf(m_fp, "import java.lang.reflect.Field;\n");
	fprintf(m_fp, "import java.util.Arrays;\n");

	fprintf(m_fp, "public class %s\n", (const char*)m_name.c_str());
	fprintf(m_fp, "{\n");
	fprintf(m_fp, "	public static SSCAPIJNI api;\n");
	
	// handle csv files
	// arrays
	fprintf(m_fp, "	public static void set_array(long cxt, String name, String csvFile, int len)\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		BufferedReader br = null;\n");
	fprintf(m_fp, "		String line = \"\";\n");
	fprintf(m_fp, "		float ary[] = new float[len];\n");
	fprintf(m_fp, "		int i=0;\n");
	fprintf(m_fp, "		try {\n");
	fprintf(m_fp, "			br = new BufferedReader(new FileReader(csvFile));\n");
	fprintf(m_fp, "			while (((line = br.readLine()) != null) && (i<len)) {\n");
	fprintf(m_fp, "				ary[i]=Float.parseFloat(line);\n");
	fprintf(m_fp, "				i++;\n");
	fprintf(m_fp, "			}\n");
	fprintf(m_fp, "		} catch (FileNotFoundException e) {\n");
	fprintf(m_fp, "			e.printStackTrace();\n");
	fprintf(m_fp, "		} catch (IOException e) {\n");
	fprintf(m_fp, "			e.printStackTrace();\n");
	fprintf(m_fp, "		} finally {\n");
	fprintf(m_fp, "			if (br != null) {\n");
	fprintf(m_fp, "				try {");
	fprintf(m_fp, "					br.close();");
	fprintf(m_fp, "				br.close();");
	fprintf(m_fp, "				} catch (IOException e) {");
	fprintf(m_fp, "					e.printStackTrace();\n");
	fprintf(m_fp, "				}\n");
	fprintf(m_fp, "			}\n");
	fprintf(m_fp, "		}\n");
	fprintf(m_fp, "		api.ssc_data_set_array( cxt, name, ary, len);\n");
	fprintf(m_fp, "		return;\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "\n");

	// matrices
	fprintf(m_fp, "	public static void set_matrix(long cxt, String name, String csvFile, int nr, int nc)\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		BufferedReader br = null;\n");
	fprintf(m_fp, "		String line = \"\";\n");
	fprintf(m_fp, "		String values[];\n");
	fprintf(m_fp, "		String cvsSplitBy = \",\";\n");
	fprintf(m_fp, "		int len=nr*nc;\n");
	fprintf(m_fp, "		float ary[] = new float[len];\n");
	fprintf(m_fp, "		int i=0;\n");
	fprintf(m_fp, "		try {\n");
	fprintf(m_fp, "			br = new BufferedReader(new FileReader(csvFile));\n");
	fprintf(m_fp, "			while (((line = br.readLine()) != null) && (i<len)) {\n");
	fprintf(m_fp, "				values = line.split(cvsSplitBy);\n");
	fprintf(m_fp, "				for (int ic=0;ic<values.length;ic++){\n");
	fprintf(m_fp, "					ary[i]=Float.parseFloat(values[ic]);\n");
	fprintf(m_fp, "					i++;\n");
	fprintf(m_fp, "				}\n");
	fprintf(m_fp, "			}\n");
	fprintf(m_fp, "		} catch (FileNotFoundException e) {\n");
	fprintf(m_fp, "			e.printStackTrace();\n");
	fprintf(m_fp, "		} catch (IOException e) {\n");
	fprintf(m_fp, "			e.printStackTrace();\n");
	fprintf(m_fp, "		} finally {\n");
	fprintf(m_fp, "			if (br != null) {\n");
	fprintf(m_fp, "				try {");
	fprintf(m_fp, "					br.close();");
	fprintf(m_fp, "				br.close();");
	fprintf(m_fp, "				} catch (IOException e) {");
	fprintf(m_fp, "					e.printStackTrace();\n");
	fprintf(m_fp, "				}\n");
	fprintf(m_fp, "			}\n");
	fprintf(m_fp, "		}\n");
	fprintf(m_fp, "		api.ssc_data_set_matrix( cxt, name, ary, nr, nc);\n");
	fprintf(m_fp, "		return;\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "\n");



	fprintf(m_fp, "	public static void main(String[] args) throws Exception\n");
	fprintf(m_fp, "	{\n");

	// create global data container
	fprintf(m_fp, "		System.loadLibrary(\"SSCAPIJNI\");\n");
	fprintf(m_fp, "		api = new SSCAPIJNI();\n");
	fprintf(m_fp, "		long data=api.ssc_data_create();\n");
	fprintf(m_fp, "		long mod;\n");
	fprintf(m_fp, "\n");

	return true;
}

bool CodeGen_java::CreateSSCModule(wxString &name)
{
	if (name.IsNull() || name.Length() < 1)
		return false;
	else
	{
		fprintf(m_fp, "		mod = api.ssc_module_create(\"%s\");\n", (const char*)name.c_str());
	}
	return true;
}

bool CodeGen_java::FreeSSCModule()
{
	fprintf(m_fp, "		api.ssc_module_free(mod);\n");
	return true;
}

bool CodeGen_java::SupportingFiles()
{
	// add JNI file - java class 
	wxString fn = m_folder + "/SSCAPIJNI.java";
	FILE *f = fopen(fn.c_str(), "w");
	if (!f) return false;
	fprintf(f, "public class SSCAPIJNI {\n");
	fprintf(f, "  public final static native int ssc_version();\n");
	fprintf(f, "  public final static native String ssc_build_info();\n");
	fprintf(f, "  public final static native int SSC_INVALID_get();\n");
	fprintf(f, "  public final static native int SSC_STRING_get();\n");
	fprintf(f, "  public final static native int SSC_NUMBER_get();\n");
	fprintf(f, "  public final static native int SSC_ARRAY_get();\n");
	fprintf(f, "  public final static native int SSC_MATRIX_get();\n");
	fprintf(f, "  public final static native int SSC_TABLE_get();\n");
	fprintf(f, "  public final static native long ssc_data_create();\n");
	fprintf(f, "  public final static native void ssc_data_free(long jarg1);\n");
	fprintf(f, "  public final static native void ssc_data_clear(long jarg1);\n");
	fprintf(f, "  public final static native void ssc_data_unassign(long jarg1, String jarg2);\n");
	fprintf(f, "  public final static native int ssc_data_query(long jarg1, String jarg2);\n");
	fprintf(f, "  public final static native String ssc_data_first(long jarg1);\n");
	fprintf(f, "  public final static native String ssc_data_next(long jarg1);\n");
	fprintf(f, "  public final static native void ssc_data_set_string(long jarg1, String jarg2, String jarg3);\n");
	fprintf(f, "  public final static native void ssc_data_set_number(long jarg1, String jarg2, float jarg3);\n");
	fprintf(f, "  public final static native void ssc_data_set_array(long cxt, String name, float[] value, int len);\n");
	fprintf(f, "  public final static native void ssc_data_set_matrix(long cxt, String name, float[] value, int nrow, int ncol);\n");
	fprintf(f, "  public final static native void ssc_data_set_table(long jarg1, String jarg2, long jarg3);\n");
	fprintf(f, "  public final static native String ssc_data_get_string(long jarg1, String jarg2);\n");
	fprintf(f, "  public final static native int ssc_data_get_number(long cxt, String name, float[] value);\n");
	fprintf(f, "  public final static native float[] ssc_data_get_array(long cxt, String name);\n");
	fprintf(f, "  public final static native float[] ssc_data_get_matrix(long cxt, String name, int[] len);\n");
	fprintf(f, "  public final static native long ssc_data_get_table(long cxt, String name);\n");
	fprintf(f, "  public final static native long ssc_module_entry(int jarg1);\n");
	fprintf(f, "  public final static native String ssc_entry_name(long jarg1);\n");
	fprintf(f, "  public final static native String ssc_entry_description(long jarg1);\n");
	fprintf(f, "  public final static native int ssc_entry_version(long jarg1);\n");
	fprintf(f, "  public final static native long ssc_module_create(String jarg1);\n");
	fprintf(f, "  public final static native void ssc_module_free(long jarg1);\n");
	fprintf(f, "  public final static native int SSC_INPUT_get();\n");
	fprintf(f, "  public final static native int SSC_OUTPUT_get();\n");
	fprintf(f, "  public final static native int SSC_INOUT_get();\n");
	fprintf(f, "  public final static native long ssc_module_var_info(long jarg1, int jarg2);\n");
	fprintf(f, "  public final static native int ssc_info_var_type(long jarg1);\n");
	fprintf(f, "  public final static native int ssc_info_data_type(long jarg1);\n");
	fprintf(f, "  public final static native String ssc_info_name(long jarg1);\n");
	fprintf(f, "  public final static native String ssc_info_label(long jarg1);\n");
	fprintf(f, "  public final static native String ssc_info_units(long jarg1);\n");
	fprintf(f, "  public final static native String ssc_info_meta(long jarg1);\n");
	fprintf(f, "  public final static native String ssc_info_group(long jarg1);\n");
	fprintf(f, "  public final static native String ssc_info_required(long jarg1);\n");
	fprintf(f, "  public final static native String ssc_info_constraints(long jarg1);\n");
	fprintf(f, "  public final static native String ssc_info_uihint(long jarg1);\n");
	fprintf(f, "  public final static native int ssc_module_exec_simple(String jarg1, long jarg2);\n");
	fprintf(f, "  public final static native String ssc_module_exec_simple_nothread(String jarg1, long jarg2);\n");
	fprintf(f, "  public final static native int SSC_LOG_get();\n");
	fprintf(f, "  public final static native int SSC_UPDATE_get();\n");
	fprintf(f, "  public final static native int SSC_EXECUTE_get();\n");
	fprintf(f, "  public final static native int ssc_module_exec(long cxt_module, long cxt_data);\n");
	fprintf(f, "  public final static native int ssc_module_exec_with_handler(long jarg1, long jarg2, long jarg3, long jarg4);\n");
	fprintf(f, "  public final static native void ssc_module_extproc_output(long jarg1, String jarg2);\n");
	fprintf(f, "  public final static native int SSC_NOTICE_get();\n");
	fprintf(f, "  public final static native int SSC_WARNING_get();\n");
	fprintf(f, "  public final static native int SSC_ERROR_get();\n");
	fprintf(f, "  public final static native String ssc_module_log(long cxt, int index, int[] type, float[] time);\n");
	fprintf(f, "  public final static native void __ssc_segfault();\n");
	fprintf(f, "}\n");
	fclose(f);
	// add JNI file - c SWIG wrapper to handle java exceptions.
	fn = m_folder + "/sscapijni.c";
	f = fopen(fn.c_str(), "w");
	if (!f) return false;
	fprintf(f, "#define SWIGJAVA\n");
	fprintf(f, "#ifndef SWIGINLINE\n");
	fprintf(f, "# if defined(__cplusplus) || (defined(__GNUC__) && !defined(__STRICT_ANSI__))\n");
	fprintf(f, "#   define SWIGINLINE inline\n");
	fprintf(f, "# else\n");
	fprintf(f, "#   define SWIGINLINE\n");
	fprintf(f, "# endif\n");
	fprintf(f, "#endif\n");
	fprintf(f, "#ifndef SWIGUNUSED\n");
	fprintf(f, "# if defined(__GNUC__)\n");
	fprintf(f, "#   if !(defined(__cplusplus)) || (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4))\n");
	fprintf(f, "#     define SWIGUNUSED __attribute__ ((__unused__))\n");
	fprintf(f, "#   else\n");
	fprintf(f, "#     define SWIGUNUSED\n");
	fprintf(f, "#   endif\n");
	fprintf(f, "# elif defined(__ICC)\n");
	fprintf(f, "#   define SWIGUNUSED __attribute__ ((__unused__))\n");
	fprintf(f, "# else\n");
	fprintf(f, "#   define SWIGUNUSED\n");
	fprintf(f, "# endif\n");
	fprintf(f, "#endif\n");
	fprintf(f, "#ifndef SWIG_MSC_UNSUPPRESS_4505\n");
	fprintf(f, "# if defined(_MSC_VER)\n");
	fprintf(f, "#   pragma warning(disable : 4505) /* unreferenced local function has been removed */\n");
	fprintf(f, "# endif\n");
	fprintf(f, "#endif\n");
	fprintf(f, "#ifndef SWIGUNUSEDPARM\n");
	fprintf(f, "# ifdef __cplusplus\n");
	fprintf(f, "#   define SWIGUNUSEDPARM(p)\n");
	fprintf(f, "# else\n");
	fprintf(f, "#   define SWIGUNUSEDPARM(p) p SWIGUNUSED\n");
	fprintf(f, "# endif\n");
	fprintf(f, "#endif\n");
	fprintf(f, "#ifndef SWIGINTERN\n");
	fprintf(f, "# define SWIGINTERN static SWIGUNUSED\n");
	fprintf(f, "#endif\n");
	fprintf(f, "#ifndef SWIGINTERNINLINE\n");
	fprintf(f, "# define SWIGINTERNINLINE SWIGINTERN SWIGINLINE\n");
	fprintf(f, "#endif\n");
	fprintf(f, "#if (__GNUC__ >= 4) || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)\n");
	fprintf(f, "#  ifndef GCC_HASCLASSVISIBILITY\n");
	fprintf(f, "#    define GCC_HASCLASSVISIBILITY\n");
	fprintf(f, "#  endif\n");
	fprintf(f, "#endif\n");
	fprintf(f, "#ifndef SWIGEXPORT\n");
	fprintf(f, "# if defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)\n");
	fprintf(f, "#   if defined(STATIC_LINKED)\n");
	fprintf(f, "#     define SWIGEXPORT\n");
	fprintf(f, "#   else\n");
	fprintf(f, "#     define SWIGEXPORT __declspec(dllexport)\n");
	fprintf(f, "#   endif\n");
	fprintf(f, "# else\n");
	fprintf(f, "#   if defined(__GNUC__) && defined(GCC_HASCLASSVISIBILITY)\n");
	fprintf(f, "#     define SWIGEXPORT __attribute__ ((visibility(\"default\")))\n");
	fprintf(f, "#   else\n");
	fprintf(f, "#     define SWIGEXPORT\n");
	fprintf(f, "#   endif\n");
	fprintf(f, "# endif\n");
	fprintf(f, "#endif\n");
	fprintf(f, "#ifndef SWIGSTDCALL\n");
	fprintf(f, "# if defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)\n");
	fprintf(f, "#   define SWIGSTDCALL __stdcall\n");
	fprintf(f, "# else\n");
	fprintf(f, "#   define SWIGSTDCALL\n");
	fprintf(f, "# endif\n");
	fprintf(f, "#endif\n");
	fprintf(f, "#if !defined(SWIG_NO_CRT_SECURE_NO_DEPRECATE) && defined(_MSC_VER) && !defined(_CRT_SECURE_NO_DEPRECATE)\n");
	fprintf(f, "# define _CRT_SECURE_NO_DEPRECATE\n");
	fprintf(f, "#endif\n");
	fprintf(f, "#if !defined(SWIG_NO_SCL_SECURE_NO_DEPRECATE) && defined(_MSC_VER) && !defined(_SCL_SECURE_NO_DEPRECATE)\n");
	fprintf(f, "# define _SCL_SECURE_NO_DEPRECATE\n");
	fprintf(f, "#endif\n");
	fprintf(f, "#if defined(__GNUC__) && !defined(__INTEL_COMPILER)\n");
	fprintf(f, "  typedef long long __int64;\n");
	fprintf(f, "#endif\n");
	fprintf(f, "#if defined(__x86_64)\n");
	fprintf(f, "# ifdef _LP64\n");
	fprintf(f, "#   undef _LP64\n");
	fprintf(f, "# endif\n");
	fprintf(f, "#endif\n");
	fprintf(f, "#include <jni.h>\n");
	fprintf(f, "#include <stdlib.h>\n");
	fprintf(f, "#include <string.h>\n");
	fprintf(f, "typedef enum {\n");
	fprintf(f, "  SWIG_JavaOutOfMemoryError = 1,\n");
	fprintf(f, "  SWIG_JavaIOException,\n");
	fprintf(f, "  SWIG_JavaRuntimeException,\n");
	fprintf(f, "  SWIG_JavaIndexOutOfBoundsException,\n");
	fprintf(f, "  SWIG_JavaArithmeticException,\n");
	fprintf(f, "  SWIG_JavaIllegalArgumentException,\n");
	fprintf(f, "  SWIG_JavaNullPointerException,\n");
	fprintf(f, "  SWIG_JavaDirectorPureVirtual,\n");
	fprintf(f, "  SWIG_JavaUnknownError\n");
	fprintf(f, "} SWIG_JavaExceptionCodes;\n");
	fprintf(f, "typedef struct {\n");
	fprintf(f, "  SWIG_JavaExceptionCodes code;\n");
	fprintf(f, "  const char *java_exception;\n");
	fprintf(f, "} SWIG_JavaExceptions_t;\n");
	fprintf(f, "static void SWIGUNUSED SWIG_JavaThrowException(JNIEnv *jenv, SWIG_JavaExceptionCodes code, const char *msg) {\n");
	fprintf(f, "  jclass excep;\n");
	fprintf(f, "  static const SWIG_JavaExceptions_t java_exceptions[] = {\n");
	fprintf(f, "    { SWIG_JavaOutOfMemoryError, \"java / lang / OutOfMemoryError\" },\n");
	fprintf(f, "    { SWIG_JavaIOException, \"java / io / IOException\" },\n");
	fprintf(f, "    { SWIG_JavaRuntimeException, \"java / lang / RuntimeException\" },\n");
	fprintf(f, "    { SWIG_JavaIndexOutOfBoundsException, \"java / lang / IndexOutOfBoundsException\" },\n");
	fprintf(f, "    { SWIG_JavaArithmeticException, \"java / lang / ArithmeticException\" },\n");
	fprintf(f, "    { SWIG_JavaIllegalArgumentException, \"java / lang / IllegalArgumentException\" },\n");
	fprintf(f, "    { SWIG_JavaNullPointerException, \"java / lang / NullPointerException\" },\n");
	fprintf(f, "    { SWIG_JavaDirectorPureVirtual, \"java / lang / RuntimeException\" },\n");
	fprintf(f, "    { SWIG_JavaUnknownError,  \"java / lang / UnknownError\" },\n");
	fprintf(f, "    { (SWIG_JavaExceptionCodes)0,  \"java / lang / UnknownError\" }\n");
	fprintf(f, "  };\n");
	fprintf(f, "  const SWIG_JavaExceptions_t *except_ptr = java_exceptions;\n");
	fprintf(f, "  while (except_ptr->code != code && except_ptr->code)\n");
	fprintf(f, "    except_ptr++;\n");
	fprintf(f, "  (*jenv)->ExceptionClear(jenv);\n");
	fprintf(f, "  excep = (*jenv)->FindClass(jenv, except_ptr->java_exception);\n");
	fprintf(f, "  if (excep)\n");
	fprintf(f, "    (*jenv)->ThrowNew(jenv, excep, msg);\n");
	fprintf(f, "}\n");
	fprintf(f, "#define SWIG_contract_assert(nullreturn, expr, msg) if (!(expr)) {SWIG_JavaThrowException(jenv, SWIG_JavaIllegalArgumentException, msg); return nullreturn; } else\n");
	fprintf(f, "#include \"sscapi.h\"\n");
	fprintf(f, "#ifdef __cplusplus\n");
	fprintf(f, "extern \"C\" {\n");
	fprintf(f, "#endif\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_ssc_1version(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)ssc_version();\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1build_1info(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (char *)ssc_build_info();\n");
	fprintf(f, "  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_1INVALID_1get(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)(0);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_1STRING_1get(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)(1);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_1NUMBER_1get(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)(2);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_1ARRAY_1get(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)(3);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_1MATRIX_1get(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)(4);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_1TABLE_1get(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)(5);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jlong JNICALL Java_SSCAPIJNI_ssc_1data_1create(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jlong jresult = 0 ;\n");
	fprintf(f, "  ssc_data_t result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (ssc_data_t)ssc_data_create();\n");
	fprintf(f, "  *(ssc_data_t *)&jresult = result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT void JNICALL Java_SSCAPIJNI_ssc_1data_1free(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  ssc_data_t arg1 = (ssc_data_t) 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_data_t *)&jarg1;\n");
	fprintf(f, "  ssc_data_free(arg1);\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT void JNICALL Java_SSCAPIJNI_ssc_1data_1clear(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  ssc_data_t arg1 = (ssc_data_t) 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_data_t *)&jarg1;\n");
	fprintf(f, "  ssc_data_clear(arg1);\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT void JNICALL Java_SSCAPIJNI_ssc_1data_1unassign(JNIEnv *jenv, jclass jcls, jlong jarg1, jstring jarg2) {\n");
	fprintf(f, "  ssc_data_t arg1 = (ssc_data_t) 0 ;\n");
	fprintf(f, "  char *arg2 = (char *) 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_data_t *)&jarg1;\n");
	fprintf(f, "  arg2 = 0;\n");
	fprintf(f, "  if (jarg2) {\n");
	fprintf(f, "    arg2 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg2, 0);\n");
	fprintf(f, "    if (!arg2) return ;\n");
	fprintf(f, "  }\n");
	fprintf(f, "  ssc_data_unassign(arg1,(char const *)arg2);\n");
	fprintf(f, "  if (arg2) (*jenv)->ReleaseStringUTFChars(jenv, jarg2, (const char *)arg2);\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_ssc_1data_1query(JNIEnv *jenv, jclass jcls, jlong jarg1, jstring jarg2) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  ssc_data_t arg1 = (ssc_data_t) 0 ;\n");
	fprintf(f, "  char *arg2 = (char *) 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_data_t *)&jarg1;\n");
	fprintf(f, "  arg2 = 0;\n");
	fprintf(f, "  if (jarg2) {\n");
	fprintf(f, "    arg2 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg2, 0);\n");
	fprintf(f, "    if (!arg2) return 0;\n");
	fprintf(f, "  }\n");
	fprintf(f, "  result = (int)ssc_data_query(arg1,(char const *)arg2);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  if (arg2) (*jenv)->ReleaseStringUTFChars(jenv, jarg2, (const char *)arg2);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1data_1first(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  ssc_data_t arg1 = (ssc_data_t) 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_data_t *)&jarg1;\n");
	fprintf(f, "  result = (char *)ssc_data_first(arg1);\n");
	fprintf(f, "  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1data_1next(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  ssc_data_t arg1 = (ssc_data_t) 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_data_t *)&jarg1;\n");
	fprintf(f, "  result = (char *)ssc_data_next(arg1);\n");
	fprintf(f, "  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT void JNICALL Java_SSCAPIJNI_ssc_1data_1set_1string(JNIEnv *jenv, jclass jcls, jlong jarg1, jstring jarg2, jstring jarg3) {\n");
	fprintf(f, "  ssc_data_t arg1 = (ssc_data_t) 0 ;\n");
	fprintf(f, "  char *arg2 = (char *) 0 ;\n");
	fprintf(f, "  char *arg3 = (char *) 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_data_t *)&jarg1;\n");
	fprintf(f, "  arg2 = 0;\n");
	fprintf(f, "  if (jarg2) {\n");
	fprintf(f, "    arg2 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg2, 0);\n");
	fprintf(f, "    if (!arg2) return ;\n");
	fprintf(f, "  }\n");
	fprintf(f, "  arg3 = 0;\n");
	fprintf(f, "  if (jarg3) {\n");
	fprintf(f, "    arg3 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg3, 0);\n");
	fprintf(f, "    if (!arg3) return ;\n");
	fprintf(f, "  }\n");
	fprintf(f, "  ssc_data_set_string(arg1,(char const *)arg2,(char const *)arg3);\n");
	fprintf(f, "  if (arg2) (*jenv)->ReleaseStringUTFChars(jenv, jarg2, (const char *)arg2);\n");
	fprintf(f, "  if (arg3) (*jenv)->ReleaseStringUTFChars(jenv, jarg3, (const char *)arg3);\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT void JNICALL Java_SSCAPIJNI_ssc_1data_1set_1number(JNIEnv *jenv, jclass jcls, jlong cxt, jstring name, jfloat value)\n");
	fprintf(f, "{\n");
	fprintf(f, "  ssc_data_t ssc_cxt = (ssc_data_t) 0 ;\n");
	fprintf(f, "  char *ssc_name = (char *) 0 ;\n");
	fprintf(f, "  ssc_number_t ssc_value ;\n");
	fprintf(f, "  ssc_cxt = *(ssc_data_t *)&cxt;\n");
	fprintf(f, "  ssc_name = 0;\n");
	fprintf(f, "  if (name)\n");
	fprintf(f, "  {\n");
	fprintf(f, "    ssc_name = (char *)(*jenv)->GetStringUTFChars(jenv, name, 0);\n");
	fprintf(f, "    if (!ssc_name) return ;\n");
	fprintf(f, "  }\n");
	fprintf(f, "  ssc_value = (ssc_number_t)value;\n");
	fprintf(f, "  ssc_data_set_number(ssc_cxt,(char const *)ssc_name,ssc_value);\n");
	fprintf(f, "  if (ssc_name) (*jenv)->ReleaseStringUTFChars(jenv, name, (const char *)ssc_name);\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT void JNICALL Java_SSCAPIJNI_ssc_1data_1set_1array(JNIEnv *jenv, jclass jcls, jlong cxt, jstring name, jfloatArray value, jint len)\n");
	fprintf(f, "{\n");
	fprintf(f, "	ssc_data_t ssc_cxt = (ssc_data_t) 0 ;\n");
	fprintf(f, "	char *ssc_name = (char *) 0 ;\n");
	fprintf(f, "	ssc_number_t ssc_value ;\n");
	fprintf(f, "	ssc_cxt = *(ssc_data_t *)&cxt;\n");
	fprintf(f, "	ssc_name = 0;\n");
	fprintf(f, "	if (name)\n");
	fprintf(f, "	{\n");
	fprintf(f, "      ssc_name = (char *)(*jenv)->GetStringUTFChars(jenv, name, 0);\n");
	fprintf(f, "	  if (!ssc_name) return ;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	int i, count;\n");
	fprintf(f, "    jfloat *j_data_array;\n");
	fprintf(f, "    jint j_count_len;\n");
	fprintf(f, "    j_count_len = (*jenv)->GetArrayLength(jenv, value);\n");
	fprintf(f, "    j_data_array = (*jenv)->GetFloatArrayElements(jenv, value, NULL);\n");
	fprintf(f, "    // set data\n");
	fprintf(f, "    count = j_count_len;\n");
	fprintf(f, "    ssc_number_t ssc_array[count];\n");
	fprintf(f, "    for( i=0; i<count;i++)\n");
	fprintf(f, "        ssc_array[i] = j_data_array[i];\n");
	fprintf(f, "    ssc_data_set_array( ssc_cxt, ssc_name, ssc_array, count);\n");
	fprintf(f, "    (*jenv)->ReleaseStringUTFChars(jenv, name, ssc_name);\n");
	fprintf(f, "    (*jenv)->ReleaseFloatArrayElements(jenv, value, j_data_array, 0);\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT void JNICALL Java_SSCAPIJNI_ssc_1data_1set_1matrix(JNIEnv *jenv, jclass jcls, jlong cxt, jstring name, jfloatArray value, jint nrow, jint ncol)\n");
	fprintf(f, "{\n");
	fprintf(f, "	ssc_data_t ssc_cxt = (ssc_data_t) 0 ;\n");
	fprintf(f, "	char *ssc_name = (char *) 0 ;\n");
	fprintf(f, "	ssc_number_t ssc_value ;\n");
	fprintf(f, "	ssc_cxt = *(ssc_data_t *)&cxt;\n");
	fprintf(f, "	ssc_name = 0;\n");
	fprintf(f, "	if (name)\n");
	fprintf(f, "	{\n");
	fprintf(f, "      ssc_name = (char *)(*jenv)->GetStringUTFChars(jenv, name, 0);\n");
	fprintf(f, "	  if (!ssc_name) return ;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	int i, rows, cols;\n");
	fprintf(f, "    jfloat *j_data_array;\n");
	fprintf(f, "    jint j_count_len;\n");
	fprintf(f, "    j_count_len = (*jenv)->GetArrayLength(jenv, value);\n");
	fprintf(f, "    j_data_array = (*jenv)->GetFloatArrayElements(jenv, value, NULL);\n");
	fprintf(f, "    // set data\n");
	fprintf(f, "    ssc_number_t ssc_array[j_count_len];\n");
	fprintf(f, "    for( i=0; i<j_count_len;i++)\n");
	fprintf(f, "        ssc_array[i] = j_data_array[i];\n");
	fprintf(f, "    rows = nrow;\n");
	fprintf(f, "    cols = ncol;\n");
	fprintf(f, "    ssc_data_set_matrix( ssc_cxt, ssc_name, ssc_array, rows, cols);\n");
	fprintf(f, "    (*jenv)->ReleaseStringUTFChars(jenv, name, ssc_name);\n");
	fprintf(f, "    (*jenv)->ReleaseFloatArrayElements(jenv, value, j_data_array, 0);\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT void JNICALL Java_SSCAPIJNI_ssc_1data_1set_1table(JNIEnv *jenv, jclass jcls, jlong jarg1, jstring jarg2, jlong jarg3) {\n");
	fprintf(f, "  ssc_data_t arg1 = (ssc_data_t) 0 ;\n");
	fprintf(f, "  char *arg2 = (char *) 0 ;\n");
	fprintf(f, "  ssc_data_t arg3 = (ssc_data_t) 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_data_t *)&jarg1;\n");
	fprintf(f, "  arg2 = 0;\n");
	fprintf(f, "  if (jarg2) {\n");
	fprintf(f, "    arg2 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg2, 0);\n");
	fprintf(f, "    if (!arg2) return ;\n");
	fprintf(f, "  }\n");
	fprintf(f, "  arg3 = *(ssc_data_t *)&jarg3;\n");
	fprintf(f, "  ssc_data_set_table(arg1,(char const *)arg2,arg3);\n");
	fprintf(f, "  if (arg2) (*jenv)->ReleaseStringUTFChars(jenv, jarg2, (const char *)arg2);\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1data_1get_1string(JNIEnv *jenv, jclass jcls, jlong jarg1, jstring jarg2) {\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  ssc_data_t arg1 = (ssc_data_t) 0 ;\n");
	fprintf(f, "  char *arg2 = (char *) 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_data_t *)&jarg1;\n");
	fprintf(f, "  arg2 = 0;\n");
	fprintf(f, "  if (jarg2) {\n");
	fprintf(f, "    arg2 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg2, 0);\n");
	fprintf(f, "    if (!arg2) return 0;\n");
	fprintf(f, "  }\n");
	fprintf(f, "  result = (char *)ssc_data_get_string(arg1,(char const *)arg2);\n");
	fprintf(f, "  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "  if (arg2) (*jenv)->ReleaseStringUTFChars(jenv, jarg2, (const char *)arg2);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_ssc_1data_1get_1number(JNIEnv *env, jclass jcls, jlong cxt, jstring name, jfloatArray value)\n");
	fprintf(f, "{\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  ssc_data_t sscCxt = (ssc_data_t) 0 ;\n");
	fprintf(f, "  char *sscName = (char *) 0 ;\n");
	fprintf(f, "  ssc_bool_t result;\n");
	fprintf(f, "  ssc_number_t sscValue;\n");
	fprintf(f, "  jfloat *output;\n");
	fprintf(f, "  jint count;\n");
	fprintf(f, "  count = (*env)->GetArrayLength(env, value);\n");
	fprintf(f, "  output = (*env)->GetFloatArrayElements(env, value, NULL);\n");
	fprintf(f, "  sscCxt = *(ssc_data_t *)&cxt;\n");
	fprintf(f, "  sscName = 0;\n");
	fprintf(f, "  if (name)\n");
	fprintf(f, "  {\n");
	fprintf(f, "    sscName = (char *)(*env)->GetStringUTFChars(env, name, 0);\n");
	fprintf(f, "    if (!sscName)\n");
	fprintf(f, "    {\n");
	fprintf(f, "        return 0;\n");
	fprintf(f, "    }\n");
	fprintf(f, "  }\n");
	fprintf(f, "  result = (ssc_bool_t)ssc_data_get_number(sscCxt,(char const *)sscName,&sscValue);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  if (sscName)\n");
	fprintf(f, "  {\n");
	fprintf(f, "      (*env)->ReleaseStringUTFChars(env, name, (const char *)sscName);\n");
	fprintf(f, "  }\n");
	fprintf(f, "  if (result && (count==1))\n");
	fprintf(f, "  {\n");
	fprintf(f, "      output[0] = sscValue;\n");
	fprintf(f, "      (*env)->SetFloatArrayRegion( env, value, 0, count, output );\n");
	fprintf(f, "  }\n");
	fprintf(f, "  else\n");
	fprintf(f, "  {\n");
	fprintf(f, "      jresult = 0;\n");
	fprintf(f, "  }\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jfloatArray JNICALL Java_SSCAPIJNI_ssc_1data_1get_1array(JNIEnv *jenv, jclass jcls, jlong cxt, jstring name)\n");
	fprintf(f, "{\n");
	fprintf(f, "	ssc_data_t ssc_cxt = (ssc_data_t) 0 ;\n");
	fprintf(f, "	char *ssc_name = (char *) 0 ;\n");
	fprintf(f, "	ssc_cxt = *(ssc_data_t *)&cxt;\n");
	fprintf(f, "	ssc_name = 0;\n");
	fprintf(f, "	if (name)\n");
	fprintf(f, "	{\n");
	fprintf(f, "      ssc_name = (char *)(*jenv)->GetStringUTFChars(jenv, name, 0);\n");
	fprintf(f, "	  if (!ssc_name) return NULL;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	int i, count;\n");
	fprintf(f, "    jfloatArray output;\n");
	fprintf(f, "    jint j_data_len;\n");
	fprintf(f, "    jfloat *j_data_array;\n");
	fprintf(f, "    ssc_number_t *ssc_array = ssc_data_get_array( ssc_cxt, ssc_name, &count);\n");
	fprintf(f, "    (*jenv)->ReleaseStringUTFChars(jenv, name, ssc_name);\n");
	fprintf(f, "    j_data_len = count;\n");
	fprintf(f, "    output = (*jenv)->NewFloatArray( jenv, j_data_len );\n");
	fprintf(f, "    if (output == NULL) return NULL;\n");
	fprintf(f, "    j_data_array = (*jenv)->GetFloatArrayElements(jenv, output, NULL);\n");
	fprintf(f, "    for( i=0; i<count;i++)\n");
	fprintf(f, "            j_data_array[i] = ssc_array[i];\n");
	fprintf(f, "    (*jenv)->SetFloatArrayRegion( jenv, output, 0, j_data_len, j_data_array );\n");
	fprintf(f, "    return output;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jfloatArray JNICALL Java_SSCAPIJNI_ssc_1data_1get_1matrix(JNIEnv *jenv, jclass jcls, jlong cxt, jstring name, jintArray len)\n");
	fprintf(f, "{\n");
	fprintf(f, "	ssc_data_t ssc_cxt = (ssc_data_t) 0 ;\n");
	fprintf(f, "	char *ssc_name = (char *) 0 ;\n");
	fprintf(f, "	ssc_cxt = *(ssc_data_t *)&cxt;\n");
	fprintf(f, "	ssc_name = 0;\n");
	fprintf(f, "	if (name)\n");
	fprintf(f, "	{\n");
	fprintf(f, "      ssc_name = (char *)(*jenv)->GetStringUTFChars(jenv, name, 0);\n");
	fprintf(f, "	  if (!ssc_name) return NULL;\n");
	fprintf(f, "	}\n");
	fprintf(f, "    int i, row_count, col_count;\n");
	fprintf(f, "    jfloatArray output;\n");
	fprintf(f, "    jint j_row_len;\n");
	fprintf(f, "    jint j_col_len;\n");
	fprintf(f, "    jfloat *j_data_array;\n");
	fprintf(f, "    jint j_count_len;\n");
	fprintf(f, "    jsize *j_count_array;\n");
	fprintf(f, "    jint j_data_len;\n");
	fprintf(f, "    ssc_number_t *ssc_array = ssc_data_get_matrix( ssc_cxt, ssc_name, &row_count, &col_count);\n");
	fprintf(f, "    (*jenv)->ReleaseStringUTFChars(jenv, name, ssc_name);\n");
	fprintf(f, "    j_row_len=row_count;\n");
	fprintf(f, "    j_col_len=col_count;\n");
	fprintf(f, "    j_data_len = row_count*col_count;\n");
	fprintf(f, "    j_count_len = (*jenv)->GetArrayLength(jenv, len);\n");
	fprintf(f, "    j_count_array = (*jenv)->GetIntArrayElements(jenv, len, NULL);\n");
	fprintf(f, "    if ( j_count_len > 1)\n");
	fprintf(f, "    {\n");
	fprintf(f, "            j_count_array[0] = j_row_len;\n");
	fprintf(f, "            j_count_array[1] = j_col_len;\n");
	fprintf(f, "    }\n");
	fprintf(f, "    (*jenv)->SetIntArrayRegion( jenv, len, 0, j_count_len, j_count_array );\n");
	fprintf(f, "    output = (*jenv)->NewFloatArray( jenv, j_data_len );\n");
	fprintf(f, "    if (output == NULL) return NULL;\n");
	fprintf(f, "    j_data_array = (*jenv)->GetFloatArrayElements(jenv, output, NULL);\n");
	fprintf(f, "\n");
	fprintf(f, "    for( i=0; i<j_data_len;i++)\n");
	fprintf(f, "            j_data_array[i] = ssc_array[i];\n");
	fprintf(f, "\n");
	fprintf(f, "    (*jenv)->SetFloatArrayRegion( jenv, output, 0, j_data_len, j_data_array );\n");
	fprintf(f, "    return output;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jlong JNICALL Java_SSCAPIJNI_ssc_1data_1get_1table(JNIEnv *jenv, jclass jcls, jlong jarg1, jstring jarg2) {\n");
	fprintf(f, "  jlong jresult = 0 ;\n");
	fprintf(f, "  ssc_data_t arg1 = (ssc_data_t) 0 ;\n");
	fprintf(f, "  char *arg2 = (char *) 0 ;\n");
	fprintf(f, "  ssc_data_t result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_data_t *)&jarg1;\n");
	fprintf(f, "  arg2 = 0;\n");
	fprintf(f, "  if (jarg2) {\n");
	fprintf(f, "    arg2 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg2, 0);\n");
	fprintf(f, "    if (!arg2) return 0;\n");
	fprintf(f, "  }\n");
	fprintf(f, "  result = (ssc_data_t)ssc_data_get_table(arg1,(char const *)arg2);\n");
	fprintf(f, "  *(ssc_data_t *)&jresult = result;\n");
	fprintf(f, "  if (arg2) (*jenv)->ReleaseStringUTFChars(jenv, jarg2, (const char *)arg2);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jlong JNICALL Java_SSCAPIJNI_ssc_1module_1entry(JNIEnv *jenv, jclass jcls, jint jarg1) {\n");
	fprintf(f, "  jlong jresult = 0 ;\n");
	fprintf(f, "  int arg1 ;\n");
	fprintf(f, "  ssc_entry_t result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = (int)jarg1;\n");
	fprintf(f, "  result = (ssc_entry_t)ssc_module_entry(arg1);\n");
	fprintf(f, "  *(ssc_entry_t *)&jresult = result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1entry_1name(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  ssc_entry_t arg1 = (ssc_entry_t) 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_entry_t *)&jarg1;\n");
	fprintf(f, "  result = (char *)ssc_entry_name(arg1);\n");
	fprintf(f, "  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1entry_1description(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  ssc_entry_t arg1 = (ssc_entry_t) 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_entry_t *)&jarg1;\n");
	fprintf(f, "  result = (char *)ssc_entry_description(arg1);\n");
	fprintf(f, "  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_ssc_1entry_1version(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  ssc_entry_t arg1 = (ssc_entry_t) 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_entry_t *)&jarg1;\n");
	fprintf(f, "  result = (int)ssc_entry_version(arg1);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jlong JNICALL Java_SSCAPIJNI_ssc_1module_1create(JNIEnv *jenv, jclass jcls, jstring jarg1) {\n");
	fprintf(f, "  jlong jresult = 0 ;\n");
	fprintf(f, "  char *arg1 = (char *) 0 ;\n");
	fprintf(f, "  ssc_module_t result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = 0;\n");
	fprintf(f, "  if (jarg1) {\n");
	fprintf(f, "    arg1 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg1, 0);\n");
	fprintf(f, "    if (!arg1) return 0;\n");
	fprintf(f, "  }\n");
	fprintf(f, "  result = (ssc_module_t)ssc_module_create((char const *)arg1);\n");
	fprintf(f, "  *(ssc_module_t *)&jresult = result;\n");
	fprintf(f, "  if (arg1) (*jenv)->ReleaseStringUTFChars(jenv, jarg1, (const char *)arg1);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT void JNICALL Java_SSCAPIJNI_ssc_1module_1free(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  ssc_module_t arg1 = (ssc_module_t) 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_module_t *)&jarg1;\n");
	fprintf(f, "  ssc_module_free(arg1);\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_1INPUT_1get(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)(1);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_1OUTPUT_1get(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)(2);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_1INOUT_1get(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)(3);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jlong JNICALL Java_SSCAPIJNI_ssc_1module_1var_1info(JNIEnv *jenv, jclass jcls, jlong jarg1, jint jarg2) {\n");
	fprintf(f, "  jlong jresult = 0 ;\n");
	fprintf(f, "  ssc_module_t arg1 = (ssc_module_t) 0 ;\n");
	fprintf(f, "  int arg2 ;\n");
	fprintf(f, "  ssc_info_t result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_module_t *)&jarg1;\n");
	fprintf(f, "  arg2 = (int)jarg2;\n");
	fprintf(f, "  result = (ssc_info_t)ssc_module_var_info(arg1,arg2);\n");
	fprintf(f, "  *(ssc_info_t *)&jresult = result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_ssc_1info_1var_1type(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  ssc_info_t arg1 = (ssc_info_t) 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_info_t *)&jarg1;\n");
	fprintf(f, "  result = (int)ssc_info_var_type(arg1);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_ssc_1info_1data_1type(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  ssc_info_t arg1 = (ssc_info_t) 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_info_t *)&jarg1;\n");
	fprintf(f, "  result = (int)ssc_info_data_type(arg1);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1info_1name(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  ssc_info_t arg1 = (ssc_info_t) 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_info_t *)&jarg1;\n");
	fprintf(f, "  result = (char *)ssc_info_name(arg1);\n");
	fprintf(f, "  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1info_1label(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  ssc_info_t arg1 = (ssc_info_t) 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_info_t *)&jarg1;\n");
	fprintf(f, "  result = (char *)ssc_info_label(arg1);\n");
	fprintf(f, "  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1info_1units(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  ssc_info_t arg1 = (ssc_info_t) 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_info_t *)&jarg1;\n");
	fprintf(f, "  result = (char *)ssc_info_units(arg1);\n");
	fprintf(f, "  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1info_1meta(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  ssc_info_t arg1 = (ssc_info_t) 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_info_t *)&jarg1;\n");
	fprintf(f, "  result = (char *)ssc_info_meta(arg1);\n");
	fprintf(f, "  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1info_1group(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  ssc_info_t arg1 = (ssc_info_t) 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_info_t *)&jarg1;\n");
	fprintf(f, "  result = (char *)ssc_info_group(arg1);\n");
	fprintf(f, "  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1info_1required(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  ssc_info_t arg1 = (ssc_info_t) 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_info_t *)&jarg1;\n");
	fprintf(f, "  result = (char *)ssc_info_required(arg1);\n");
	fprintf(f, "  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1info_1constraints(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  ssc_info_t arg1 = (ssc_info_t) 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_info_t *)&jarg1;\n");
	fprintf(f, "  result = (char *)ssc_info_constraints(arg1);\n");
	fprintf(f, "  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1info_1uihint(JNIEnv *jenv, jclass jcls, jlong jarg1) {\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  ssc_info_t arg1 = (ssc_info_t) 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_info_t *)&jarg1;\n");
	fprintf(f, "  result = (char *)ssc_info_uihint(arg1);\n");
	fprintf(f, "  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_ssc_1module_1exec_1simple(JNIEnv *jenv, jclass jcls, jstring jarg1, jlong jarg2) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  char *arg1 = (char *) 0 ;\n");
	fprintf(f, "  ssc_data_t arg2 = (ssc_data_t) 0 ;\n");
	fprintf(f, "  ssc_bool_t result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = 0;\n");
	fprintf(f, "  if (jarg1) {\n");
	fprintf(f, "    arg1 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg1, 0);\n");
	fprintf(f, "    if (!arg1) return 0;\n");
	fprintf(f, "  }\n");
	fprintf(f, "  arg2 = *(ssc_data_t *)&jarg2;\n");
	fprintf(f, "  result = (ssc_bool_t)ssc_module_exec_simple((char const *)arg1,arg2);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  if (arg1) (*jenv)->ReleaseStringUTFChars(jenv, jarg1, (const char *)arg1);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1module_1exec_1simple_1nothread(JNIEnv *jenv, jclass jcls, jstring jarg1, jlong jarg2) {\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  char *arg1 = (char *) 0 ;\n");
	fprintf(f, "  ssc_data_t arg2 = (ssc_data_t) 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = 0;\n");
	fprintf(f, "  if (jarg1) {\n");
	fprintf(f, "    arg1 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg1, 0);\n");
	fprintf(f, "    if (!arg1) return 0;\n");
	fprintf(f, "  }\n");
	fprintf(f, "  arg2 = *(ssc_data_t *)&jarg2;\n");
	fprintf(f, "  result = (char *)ssc_module_exec_simple_nothread((char const *)arg1,arg2);\n");
	fprintf(f, "  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "  if (arg1) (*jenv)->ReleaseStringUTFChars(jenv, jarg1, (const char *)arg1);\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_1LOG_1get(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)(0);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_1UPDATE_1get(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)(1);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_1EXECUTE_1get(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)(2);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_ssc_1module_1exec(JNIEnv *jenv, jclass jcls, jlong cxt_module, jlong cxt_data) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  ssc_module_t arg1 = (ssc_module_t) 0 ;\n");
	fprintf(f, "  ssc_data_t arg2 = (ssc_data_t) 0 ;\n");
	fprintf(f, "  ssc_bool_t result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_module_t *)&cxt_module;\n");
	fprintf(f, "  arg2 = *(ssc_data_t *)&cxt_data;\n");
	fprintf(f, "  result = (ssc_bool_t)ssc_module_exec(arg1,arg2);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_ssc_1module_1exec_1with_1handler(JNIEnv *jenv, jclass jcls, jlong jarg1, jlong jarg2, jlong jarg3, jlong jarg4) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  ssc_module_t arg1 = (ssc_module_t) 0 ;\n");
	fprintf(f, "  ssc_data_t arg2 = (ssc_data_t) 0 ;\n");
	fprintf(f, "  ssc_bool_t (*arg3)(ssc_module_t,ssc_handler_t,int,float,float,char const *,char const *,void *) = (ssc_bool_t (*)(ssc_module_t,ssc_handler_t,int,float,float,char const *,char const *,void *)) 0 ;\n");
	fprintf(f, "  void *arg4 = (void *) 0 ;\n");
	fprintf(f, "  ssc_bool_t result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  arg1 = *(ssc_module_t *)&jarg1;\n");
	fprintf(f, "  arg2 = *(ssc_data_t *)&jarg2;\n");
	fprintf(f, "  arg3 = *(ssc_bool_t (**)(ssc_module_t,ssc_handler_t,int,float,float,char const *,char const *,void *))&jarg3;\n");
	fprintf(f, "  arg4 = *(void **)&jarg4;\n");
	fprintf(f, "  result = (ssc_bool_t)ssc_module_exec_with_handler(arg1,arg2,arg3,arg4);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_1NOTICE_1get(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)(1);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_1WARNING_1get(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)(2);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jint JNICALL Java_SSCAPIJNI_1ERROR_1get(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  jint jresult = 0 ;\n");
	fprintf(f, "  int result;\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  result = (int)(3);\n");
	fprintf(f, "  jresult = (jint)result;\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT jstring JNICALL Java_SSCAPIJNI_ssc_1module_1log(JNIEnv *jenv, jclass jcls, jlong cxt, jint index, jintArray type, jfloatArray time)\n");
	fprintf(f, "{\n");
	fprintf(f, "  ssc_data_t ssc_cxt = (ssc_data_t) 0 ;\n");
	fprintf(f, "  ssc_cxt = *(ssc_data_t *)&cxt;\n");
	fprintf(f, "  jstring jresult = 0 ;\n");
	fprintf(f, "  char *result = 0 ;\n");
	fprintf(f, "  jint outTypeCount;\n");
	fprintf(f, "  jint *outType;\n");
	fprintf(f, "  jint outTimeCount;\n");
	fprintf(f, "  jfloat *outTime;\n");
	fprintf(f, "  outTypeCount = (*jenv)->GetArrayLength(jenv, type);\n");
	fprintf(f, "  outType = (*jenv)->GetIntArrayElements(jenv, type, NULL);\n");
	fprintf(f, "  outTimeCount = (*jenv)->GetArrayLength(jenv, time);\n");
	fprintf(f, "  outTime = (*jenv)->GetFloatArrayElements(jenv, time, NULL);\n");
	fprintf(f, "  int ssc_log_type;\n");
	fprintf(f, "  float ssc_time;\n");
	fprintf(f, "  result = (char *)ssc_module_log(ssc_cxt,index,&ssc_log_type,&ssc_time);\n");
	fprintf(f, "  if (result && (outTypeCount==1) && (outTimeCount==1))\n");
	fprintf(f, "  {\n");
	fprintf(f, "      jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);\n");
	fprintf(f, "      outType[0] = ssc_log_type;\n");
	fprintf(f, "      (*jenv)->SetIntArrayRegion( jenv, type, 0, outTypeCount, outType );\n");
	fprintf(f, "      outTime[0] = ssc_time;\n");
	fprintf(f, "      (*jenv)->SetFloatArrayRegion( jenv, time, 0, outTimeCount, outTime );\n");
	fprintf(f, "  }\n");
	fprintf(f, "  else\n");
	fprintf(f, "  {\n");
	fprintf(f, "      jresult = 0;\n");
	fprintf(f, "  }\n");
	fprintf(f, "  return jresult;\n");
	fprintf(f, "}\n");
	fprintf(f, "SWIGEXPORT void JNICALL Java_SSCAPIJNI__1_1ssc_1segfault(JNIEnv *jenv, jclass jcls) {\n");
	fprintf(f, "  (void)jenv;\n");
	fprintf(f, "  (void)jcls;\n");
	fprintf(f, "  __ssc_segfault();\n");
	fprintf(f, "}\n");
	fprintf(f, "#ifdef __cplusplus\n");
	fprintf(f, "}\n");
	fprintf(f, "#endif\n");
	fclose(f);
	// add Makefile
	fn = m_folder + "/Makefile";
	f = fopen(fn.c_str(), "w");
	if (!f) return false;
	fprintf(f, "ifdef SystemRoot\n");
	fprintf(f, "	RM = del /Q\n");
	fprintf(f, "	EXT = dll\n");
	fprintf(f, "	JNILIB = SSCAPIJNI.dll\n");
	fprintf(f, "	ifneq (,$(findstring 64, $(value PROCESSOR_IDENTIFIER)))\n");
	fprintf(f, "		BITS = 64\n");
	fprintf(f, "		CCCOMP = c:/MinGW64/bin/gcc.exe\n");
	fprintf(f, "#Update based on your Java installation location		\n");
	fprintf(f, " 		CIFLAGS = -Wl,--kill-at -I\"C:\\Program Files\\Java\\jdk1.7.0_71\\include\"  -I\"C:\\Program Files\\Java\\jdk1.7.0_71\\include\\win32\"\n");
	fprintf(f, "		JC = C:\\Program Files\\Java\\jdk1.7.0_71\\bin\\javac.exe\n");
	fprintf(f, "		JAR = C:\\Program Files\\Java\\jdk1.7.0_71\\bin\\jar.exe\n");
	fprintf(f, "		JAVA = C:\\Program Files\\Java\\jdk1.7.0_71\\bin\\java.exe\n");
	fprintf(f, "		SSCLIB = ssc.dll\n");
	fprintf(f, "	else\n");
	fprintf(f, "		BITS = 32\n");
	fprintf(f, "		CCCOMP = c:/MinGW/bin/gcc.exe\n");
	fprintf(f, "#Update based on your Java installation location		\n");
	fprintf(f, "		CIFLAGS = -Wl,--kill-at -I\"C:\\Program Files(x86)\\Java\\jdk1.7.0_71\\include\"  -I\"C:\\Program Files(x86)\\Java\\jdk1.7.0_71\\include\\win32\"\n");
	fprintf(f, "		JC = C:\\Program Files (x86)\\Java\\jdk1.7.0_71\\bin\\javac.exe\n");
	fprintf(f, "		JAR = C:\\Program Files (x86)\\Java\\jdk1.7.0_71\\bin\\jar.exe\n");
	fprintf(f, "		JAVA = C:\\Program Files (x86)\\Java\\jdk1.7.0_71\\bin\\java.exe\n");
	fprintf(f, "		SSCLIB = ssc.dll\n");
	fprintf(f, "	endif	\n");
	fprintf(f, "else\n");
	fprintf(f, "    PF = $(shell uname)\n");
	fprintf(f, "    ifneq (,$(findstring Darwin, $(PF)))\n");
	fprintf(f, "        VERS = $(shell sw_vers -productVersion)\n");
	fprintf(f, "#Update based on your Java installation location\n");
	fprintf(f, "        CIFLAGS = -I/Library/Java/JavaVirtualMachines/jdk1.8.0_91.jdk/Contents/Home/include -I/Library/Java/JavaVirtualMachines/jdk1.8.0_91.jdk/Contents/Home/include/darwin\n");
	fprintf(f, "        SSCLIB = ./ssc.dylib\n");
	fprintf(f, "        EXT = dylib\n");
	fprintf(f, "        JNILIB = libSSCAPIJNI.jnilib\n");
	fprintf(f, "        ifneq (,$(findstring 10.8, $(VERS) ))\n");
	fprintf(f, "            CIFLAGS = -I/System/Library/Frameworks/JavaVM.framework/Versions/Current/Headers -I/System/Library/Frameworks/JavaVM.framework/Headers\n");
	fprintf(f, "	    SSCLIB = ssc.dylib\n");
	fprintf(f, "	    EXT = dylib\n");
	fprintf(f, "	    JNILIB = libSSCAPIJNI.jnilib\n");
	fprintf(f, "	else\n");
	fprintf(f, "            ifneq (,$(findstring 10.7, $(VERS) ))\n");
	fprintf(f, "                CIFLAGS = -I/System/Library/Frameworks/JavaVM.framework/Versions/Current/Headers -I/System/Library/Frameworks/JavaVM.framework/Headers\n");
	fprintf(f, "	        SSCLIB = ssc.dylib\n");
	fprintf(f, "	        EXT = dylib\n");
	fprintf(f, "	        JNILIB = libSSCAPIJNI.jnilib\n");
	fprintf(f, "	    else\n");
	fprintf(f, "                ifneq (,$(findstring 10.6, $(VERS)))\n");
	fprintf(f, "	            CIFLAGS = -I/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Headers -I/Developer/SDKs/MacOSX10.6.sdk/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers\n");
	fprintf(f, "                    SSCLIB = ssc.dylib\n");
	fprintf(f, "	            EXT = dylib\n");
	fprintf(f, "	            JNILIB = libSSCAPIJNI.jnilib\n");
	fprintf(f, "	        endif\n");
	fprintf(f, "	    endif\n");
	fprintf(f, "    	endif\n");
	fprintf(f, "    else \n");
	fprintf(f, "        ifneq (,$findstring(Linux, $(PF)))\n");
	fprintf(f, "	    CIFLAGS = -I/usr/java/default/include -I/usr/java/default/include/linux -fPIC \n");
	fprintf(f, "	    SSCLIB = ./ssc.so\n");
	fprintf(f, "	    EXT = so\n");
	fprintf(f, "	    JNILIB = libSSCAPIJNI.so\n");
	fprintf(f, "	    LIBPATH = -Djava.library.path=.\n");
	fprintf(f, "    	endif\n");
	fprintf(f, "    endif\n");
	fprintf(f, "    RM = rm -f\n");
	fprintf(f, "    CFLAGS += -D__64BIT__\n");
	fprintf(f, "    BITS = 64l\n");
	fprintf(f, "    JC = javac\n");
	fprintf(f, "    JAR = jar\n");
	fprintf(f, "    JAVA = java\n");
	fprintf(f, "    CCCOMP = gcc\n");
	fprintf(f, "endif\n");
	fprintf(f, "RM_JAVA = *.class  *.jar \n");
	fprintf(f, "RM_ALL =  $(JNILIB)\n");
	fprintf(f, "PROJ_NAME =  %s\n", (const char*)m_name.c_str());
	fprintf(f, "JFLAGS = -g\n");
	fprintf(f, ".SUFFIXES: .java .class\n");
	fprintf(f, ".java.class:\n");
	fprintf(f, "	$(JC) $(JFLAGS) $*.java\n");
	fprintf(f, "CLASSES = \\\n");
	fprintf(f, "	SSCAPIJNI.java \\\n");
	fprintf(f, "	$(PROJ_NAME).java\n");
	fprintf(f, "run: all\n");
	fprintf(f, "	$(JAVA) $(LIBPATH) -jar $(PROJ_NAME).jar\n");
	fprintf(f, "all: jar\n");
	fprintf(f, "java_all:  java_classes\n");
	fprintf(f, "java_classes: $(CLASSES:.java=.class)\n");
	fprintf(f, "jar: java_classes dll\n");
	fprintf(f, "	$(JAR) cvfm $(PROJ_NAME).jar $(PROJ_NAME).Manifest.txt $(PROJ_NAME).class SSCAPIJNI.class 	\n");
	fprintf(f, "java_clean:\n");
	fprintf(f, "	$(RM) $(RM_JAVA)\n");
	fprintf(f, "# $@ matches the target, $< matches the first dependancy\n");
	fprintf(f, "dll : \n");
	fprintf(f, "	$(CCCOMP) -D_JNI_IMPLEMENTATION_  $(CIFLAGS) -shared sscapijni.c -o $(JNILIB) $(SSCLIB)\n");
	fprintf(f, "clean :\n");
	fprintf(f, "	$(RM) $(RM_ALL) $(RM_JAVA)\n");
	fprintf(f, "help:\n");
	fprintf(f, "	@echo \"Please check the settings for your system.Your system may not be supported.Please contact sam.support@nrel.gov.System: $(PF) $(VERS)\"\n");
	fclose(f);
	// Manifest file to define main class
	fn = m_folder + "/" + m_name + ".Manifest.txt";
	f = fopen(fn.c_str(), "w");
	if (!f) return false;
	fprintf(f, "Main-Class: %s\n", (const char*)m_name.c_str());
	fclose(f);

	return true;
}

bool CodeGen_java::Footer()
{
	fprintf(m_fp, "		api.ssc_data_free(data); \n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "}\n");
	return true;
}



// PHP code generation class

CodeGen_php::CodeGen_php(Case *cc, const wxString &folder) : CodeGen_Base(cc, folder)
{
}


bool CodeGen_php::Output(ssc_data_t p_data)
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
			fprintf(m_fp, "	$%s = sscphp_data_get_string( $dat, \"%s\" );\n", name, name);
			fprintf(m_fp, " echo '%s = '. $%s . PHP_EOL;\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_NUMBER:
			fprintf(m_fp, "	$%s = sscphp_data_get_number( $dat, \"%s\");\n", name, name);
			fprintf(m_fp, "	 echo '%s = '. $%s . PHP_EOL;\n", (const char*)m_data[ii].label.c_str(), name);
			break;
		case SSC_ARRAY:
			fprintf(m_fp, "	$%s = sscphp_data_get_array( $dat, \"%s\");\n", name, name);
			fprintf(m_fp, "	for ($i=0; $i<count($%s);$i++):\n", name);
			fprintf(m_fp, "		echo 'array element: ' . $i .' = ' . $%s[$i] . PHP_EOL;\n", name);
			break;
		case SSC_MATRIX: // update to find columns
			fprintf(m_fp, "	$%s = sscphp_data_get_matrix( $dat, \"%s\");\n", name, name);
			fprintf(m_fp, "	for ($i=0; $i<count($%s);$i++):\n", name);
			fprintf(m_fp, "		echo 'array element: ' . $i .' = ' . $%s[$i] . PHP_EOL;\n", name);
			break;
		}
	}
	return true;
}

bool CodeGen_php::Input(ssc_data_t p_data, const char *name, const wxString &folder, const int &array_matrix_threshold)
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
		fprintf(m_fp, "	sscphp_data_set_string( $dat, \"%s\", \"%s\" );\n", name, (const char*)str_value.c_str());
		break;
	case SSC_NUMBER:
		::ssc_data_get_number(p_data, name, &value);
		dbl_value = (double)value;
		if (dbl_value > 1e38) dbl_value = 1e38;
		fprintf(m_fp, "	sscphp_data_set_number( $dat, \"%s\", %.17g );\n", name, dbl_value);
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
			fprintf(m_fp, "	set_array_from_csv( $dat, \"%s\", \"%s\", %d);\n", name, (const char*)fn.c_str(), len);
		}
		else
		{
			fprintf(m_fp, "	$%s =array(", name);
			for (int i = 0; i < (len - 1); i++)
			{
				dbl_value = (double)p[i];
				if (dbl_value > 1e38) dbl_value = 1e38;
				fprintf(m_fp, " %.17g,", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(m_fp, " %.17g );\n", dbl_value);
			fprintf(m_fp, "	sscphp_data_set_array( $dat, \"%s\", $%s);\n", name, name);
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
			fprintf(m_fp, "	set_matrix_from_csv( $dat, \"%s\", \"%s\", %d, %d);\n", name, (const char*)fn.c_str(), nr, nc);
		}
		else
		{
			fprintf(m_fp, "	$%s = array( array(", name);
			for (int k = 0; k < (len - 1); k++)
			{
				dbl_value = (double)p[k];
				if (dbl_value > 1e38) dbl_value = 1e38;
				if (k%nc == (nc - 1))
					fprintf(m_fp, " %.17g ), array(", dbl_value);
				else
					fprintf(m_fp, " %.17g,  ", dbl_value);
			}
			dbl_value = (double)p[len - 1];
			if (dbl_value > 1e38) dbl_value = 1e38;
			fprintf(m_fp, " %.17g ));\n", dbl_value);
			fprintf(m_fp, "	sscphp_data_set_matrix( $dat, \"%s\", $%s );\n", name, name);
		}
		break;
		// TODO tables in future
	}
	return true;
}


bool CodeGen_php::RunSSCModule(wxString &name)
{
	fprintf(m_fp, "	if ( !sscphp_module_exec( $mod, $dat ) )\n");
	fprintf(m_fp, "	{\n");
	fprintf(m_fp, "		$index = 0;\n");
	fprintf(m_fp, "		while ($err = sscphp_module_log( $mod, $index++ ) )\n");
	fprintf(m_fp, "		{\n");
	fprintf(m_fp, "			echo \"$err\" . PHP_EOL;\n");
	fprintf(m_fp, "		}\n");
	fprintf(m_fp, "		exit;\n");
	fprintf(m_fp, "	}\n");
	return true;
}


bool CodeGen_php::Header()
{
	// top of file and supporting functions
	fprintf(m_fp, "<?php\n");
	fprintf(m_fp, "    function set_array_from_csv( $dat, $name, $filename, $len) {\n");
	fprintf(m_fp, "	$row = 0;\n");
	fprintf(m_fp, "	$ary = array();\n");
	fprintf(m_fp, "	if (($handle = fopen($filename, \"r\")) !== FALSE) {\n");
	fprintf(m_fp, "	    while (($data = fgetcsv($handle, 1000, \",\")) !== FALSE) {\n");
	fprintf(m_fp, "		$num = count($data);\n");
	fprintf(m_fp, "		for ($c=0; $c < $num; $c++) {\n");
	fprintf(m_fp, "		    $ary[$row] = floatval($data[$c]);\n");
	fprintf(m_fp, "		}\n");
	fprintf(m_fp, "		$row++;\n");
	fprintf(m_fp, "	    }\n");
	fprintf(m_fp, "	    fclose($handle);\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	if ($row != $len) {\n");
	fprintf(m_fp, "	    echo \"number of elements $len not matching file length $row\" . PHP_EOL;\n");
	fprintf(m_fp, "	    return;\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	sscphp_data_set_array( $dat, $name, $ary);\n");
	fprintf(m_fp, "    }\n");
	fprintf(m_fp, "    function set_matrix_from_csv( $dat, $name, $filename, $nr, $nc) {\n");
	fprintf(m_fp, "	$i=0;\n");
	fprintf(m_fp, "	$row = 0;\n");
	fprintf(m_fp, "	$ary = array();\n");
	fprintf(m_fp, "	if (($handle = fopen($filename, \"r\")) !== FALSE) {\n");
	fprintf(m_fp, "	    while (($data = fgetcsv($handle, 1000, \",\")) !== FALSE) {\n");
	fprintf(m_fp, "		$col = count($data);\n");
	fprintf(m_fp, "		if ($col != $nc) {\n");
	fprintf(m_fp, "		    echo \"number of elements $col not matching row length $nc for row $row\" . PHP_EOL;\n");
	fprintf(m_fp, "		    return;\n");
	fprintf(m_fp, "		}\n");
	fprintf(m_fp, "		$row_ary = array();\n");
	fprintf(m_fp, "	        for ($c=0; $c < $col; $c++) {\n");
	fprintf(m_fp, "            		$row_ary[$i] = floatval($data[$c]);\n");
	fprintf(m_fp, "            		$i++;\n");
	fprintf(m_fp, "        	}\n");
	fprintf(m_fp, "        	$ary[$row]=$row_ary;\n");
	fprintf(m_fp, "        	$row++;\n");
	fprintf(m_fp, "    	    }\n");
	fprintf(m_fp, "    	fclose($handle);\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	if ($row != $nr) {\n");
	fprintf(m_fp, "	    echo \"number of rows $nr not matching file length $row\" . PHP_EOL;\n");
	fprintf(m_fp, "	    return;\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	if ($i != ($nr * $nc)) {\n");
	fprintf(m_fp, "	    echo \"number of elements $i not matching file length \" . $nr*$nc . PHP_EOL;\n");
	fprintf(m_fp, "	    return;\n");
	fprintf(m_fp, "	}\n");
	fprintf(m_fp, "	sscphp_data_set_matrix( $dat, $name, $ary);\n");
	fprintf(m_fp, " }\n");
	fprintf(m_fp, "	$dat = sscphp_data_create();\n");
	return true;
}

bool CodeGen_php::CreateSSCModule(wxString &name)
{
	if (name.IsNull() || name.Length() < 1)
		return false;
	else
	{
		fprintf(m_fp, "	$mod = sscphp_module_create(\"%s\");\n", (const char*)name.c_str());
	}
	return true;
}

bool CodeGen_php::FreeSSCModule()
{
	fprintf(m_fp, "	sscphp_module_free( $mod );\n");
	return true;
}


bool CodeGen_php::Footer()
{
	fprintf(m_fp, "	sscphp_data_free( $dat );\n");
	fprintf(m_fp, "?>\n");
	return true;
}

CodeGen_php5::CodeGen_php5(Case *cc, const wxString &folder) : CodeGen_php(cc, folder), CodeGen_Base(cc, folder)
{
}

bool CodeGen_php5::SupportingFiles()
{
	// add c wrapper for builing
	wxString fn = m_folder + "/sscphp.c";
	FILE *f = fopen(fn.c_str(), "w");
	if (!f) return false;
	fprintf(f, "#include <stdio.h>\n");
	fprintf(f, "#include <stdlib.h>\n");
	fprintf(f, "#include <string.h>\n");
	fprintf(f, "#include <ctype.h>\n");
	fprintf(f, "#include <php.h>\n");
	fprintf(f, "#include \"sscapi.h\"\n");
	fprintf(f, "#define PHP_SSC_VERSION \"1.0\"\n");
	fprintf(f, "#define PHP_SSC_EXTNAME \"sscphp\"\n");
	fprintf(f, "typedef struct {\n");
	fprintf(f, "	ssc_module_t p_mod;\n");
	fprintf(f, "	ssc_data_t p_dat;\n");
	fprintf(f, "	int is_dat_const_ref;\n");
	fprintf(f, "} spobj;\n");
	fprintf(f, "static int spobj_id;\n");
	fprintf(f, "#define spobj_name \"sscphpobj\"\n");
	fprintf(f, "static void _free_spobj( zend_rsrc_list_entry *rsrc TSRMLS_DC ) {\n");
	fprintf(f, "	spobj *p = (spobj*)rsrc->ptr;\n");
	fprintf(f, "	if ( p->p_mod ) ssc_module_free( p->p_mod );\n");
	fprintf(f, "	if ( p->p_dat && !p->is_dat_const_ref ) ssc_data_free( p->p_dat );\n");
	fprintf(f, "	efree(p);\n");
	fprintf(f, "};\n");
	fprintf(f, "static spobj *create_ref( ssc_module_t *m, ssc_data_t *d ) {\n");
	fprintf(f, "	spobj *p = emalloc(sizeof(spobj));\n");
	fprintf(f, "	p->p_mod = m;\n");
	fprintf(f, "	p->p_dat = d;\n");
	fprintf(f, "	p->is_dat_const_ref = 0;\n");
	fprintf(f, "	return p;\n");
	fprintf(f, "}\n");
	fprintf(f, "\n");
	fprintf(f, "PHP_MINIT_FUNCTION(sscphp) {\n");
	fprintf(f, "	spobj_id = zend_register_list_destructors_ex( _free_spobj, \n");
	fprintf(f, "			NULL, spobj_name, module_number );\n");
	fprintf(f, "	return SUCCESS;\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_MSHUTDOWN_FUNCTION(sscphp) {\n");
	fprintf(f, "	return SUCCESS;\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_MINFO_FUNCTION(sscphp) {\n");
	fprintf(f, "	char buf[16];\n");
	fprintf(f, "	sprintf(buf, \"%%d\", ssc_version());\n");
	fprintf(f, "	php_info_print_table_start();\n");
	fprintf(f, "	php_info_print_table_header(2, \"NREL SAM Simulation Core(SSC) support\", \"enabled\");\n");
	fprintf(f, "	php_info_print_table_row(2, \"Version\", buf );\n");
	fprintf(f, "	php_info_print_table_row(2, \"Build\", ssc_build_info() );\n");
	fprintf(f, "	php_info_print_table_end();\n");
	fprintf(f, "}\n");
	fprintf(f, "\n");
	fprintf(f, "PHP_FUNCTION( sscphp_version ) {\n");
	fprintf(f, "	RETURN_LONG( ssc_version() );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_build_info ) {\n");
	fprintf(f, "	RETURN_STRING( ssc_build_info(), 1 );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_create ) {\n");
	fprintf(f, "	spobj *p = create_ref( 0, ssc_data_create() );\n");
	fprintf(f, "	ZEND_REGISTER_RESOURCE( return_value, p, spobj_id );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_free ) {\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"r\", &res) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if ( !p ) return;\n");
	fprintf(f, "	if ( p->p_dat )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		ssc_data_free( p->p_dat );\n");
	fprintf(f, "		p->p_dat = 0;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	RETURN_TRUE;\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_clear ) {\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"r\", &res) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if ( !p ) return;\n");
	fprintf(f, "	if ( p->p_dat )\n");
	fprintf(f, "		ssc_data_clear( p->p_dat );\n");
	fprintf(f, "	RETURN_TRUE;\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_unassign )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	int name_len;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rs\", &res, &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if ( !p ) return;\n");
	fprintf(f, "	if ( p->p_dat )\n");
	fprintf(f, "		ssc_data_unassign( p->p_dat, name );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_query )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	int name_len;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rs\", &res, &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if ( !p ) return;\n");
	fprintf(f, "	if ( p->p_dat )\n");
	fprintf(f, "		RETURN_LONG( ssc_data_query( p->p_dat, name ) );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_first )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"r\", &res) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if ( !p ) return;\n");
	fprintf(f, "	if ( p->p_dat )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		const char *str = ssc_data_first(p->p_dat);\n");
	fprintf(f, "		if ( str )\n");
	fprintf(f, "			RETURN_STRING( str, 1 );\n");
	fprintf(f, "	}\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_next )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"r\", &res) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if ( !p ) return;\n");
	fprintf(f, "	if ( p->p_dat )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		const char *str = ssc_data_next( p->p_dat );\n");
	fprintf(f, "		if ( str )\n");
	fprintf(f, "			RETURN_STRING( str, 1 );\n");
	fprintf(f, "	}\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_set_string )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	int name_len;\n");
	fprintf(f, "	char *value;\n");
	fprintf(f, "	int value_len;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rss\", &res, &name, &name_len, &value, &value_len ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if (!p ) return;\n");
	fprintf(f, "	if( p->p_dat )\n");
	fprintf(f, "		ssc_data_set_string( p->p_dat, name, value );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_set_number )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	int name_len;\n");
	fprintf(f, "	double value;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rsd\", &res, &name, &name_len, &value ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if (!p ) return;\n");
	fprintf(f, "	if( p->p_dat )\n");
	fprintf(f, "		ssc_data_set_number( p->p_dat, name, (ssc_number_t)value );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_set_array )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	int name_len;\n");
	fprintf(f, "	long i, len;\n");
	fprintf(f, "	zval *arr;\n");
	fprintf(f, "	zval **data;\n");
	fprintf(f, "	HashTable *hash;\n");
	fprintf(f, "	HashPosition pointer;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rsa\", &res, &name, &name_len, &arr ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if (!p || !p->p_dat) return;\n");
	fprintf(f, "	hash = Z_ARRVAL_P(arr);\n");
	fprintf(f, "	len = zend_hash_num_elements( hash );\n");
	fprintf(f, "	if (len < 1 ) return;\n");
	fprintf(f, "	ssc_number_t *vec = (ssc_number_t*) malloc( sizeof(ssc_number_t)*len );\n");
	fprintf(f, "	if ( !vec ) return;\n");
	fprintf(f, "\n");
	fprintf(f, "	i=0;\n");
	fprintf(f, "	for( zend_hash_internal_pointer_reset_ex( hash, &pointer );\n");
	fprintf(f, "			zend_hash_get_current_data_ex(hash, (void**) &data, &pointer ) == SUCCESS;\n");
	fprintf(f, "			zend_hash_move_forward_ex(hash, &pointer ) )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		if ( i >= len ) break;\n");
	fprintf(f, "		vec[i] = 0.0f;\n");
	fprintf(f, "		if ( Z_TYPE_PP(data) == IS_DOUBLE )\n");
	fprintf(f, "			vec[i] = (ssc_number_t)Z_DVAL_PP(data);\n");
	fprintf(f, "		else if ( Z_TYPE_PP(data) == IS_LONG )\n");
	fprintf(f, "			vec[i] = (ssc_number_t)Z_LVAL_PP(data);\n");
	fprintf(f, "		i++;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	ssc_data_set_array( p->p_dat, name, vec, len );\n");
	fprintf(f, "	free( vec );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_set_matrix )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	int name_len;\n");
	fprintf(f, "	long i, j, nr, nc, nlen, index;\n");
	fprintf(f, "	zval *mat;\n");
	fprintf(f, "	zval **data;\n");
	fprintf(f, "	HashTable *hash, *row;\n");
	fprintf(f, "	HashPosition pointer1, pointer2;\n");
	fprintf(f, "	ssc_number_t *vec;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rsa\", &res, &name, &name_len, &mat ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if (!p || !p->p_dat) return;\n");
	fprintf(f, "	hash = Z_ARRVAL_P(mat);\n");
	fprintf(f, "	nr = zend_hash_num_elements( hash );\n");
	fprintf(f, "	if ( nr < 1 ) return;\n");
	fprintf(f, "	zend_hash_internal_pointer_reset_ex( hash, &pointer1 );\n");
	fprintf(f, "  	if ( zend_hash_get_current_data_ex(hash, (void**) &data, &pointer1 ) != SUCCESS \n");
	fprintf(f, "	 || Z_TYPE_PP(data) != IS_ARRAY )\n");
	fprintf(f, "	 	return;\n");
	fprintf(f, "	row = Z_ARRVAL_PP(data);\n");
	fprintf(f, "	nc = zend_hash_num_elements( row );\n");
	fprintf(f, "	if ( nc < 1 ) return;\n");
	fprintf(f, "	nlen = nr * nc;\n");
	fprintf(f, "	vec = (ssc_number_t*)malloc( sizeof(ssc_number_t)*nlen );\n");
	fprintf(f, "	if ( !vec ) return;\n");
	fprintf(f, "	i = 0;\n");
	fprintf(f, "	for( zend_hash_internal_pointer_reset_ex( hash, &pointer1 );\n");
	fprintf(f, "			zend_hash_get_current_data_ex(hash, (void**) &data, &pointer1 ) == SUCCESS;\n");
	fprintf(f, "			zend_hash_move_forward_ex(hash, &pointer1) )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		if ( i >= nr ) break;\n");
	fprintf(f, "		if ( Z_TYPE_PP(data) != IS_ARRAY ) continue;\n");
	fprintf(f, "		row = Z_ARRVAL_PP(data);\n");
	fprintf(f, "		j = 0;\n");
	fprintf(f, "		for( zend_hash_internal_pointer_reset_ex( row, &pointer2 );\n");
	fprintf(f, "			zend_hash_get_current_data_ex(row, (void**)&data, &pointer2 ) == SUCCESS;\n");
	fprintf(f, "			zend_hash_move_forward_ex(row, &pointer2 ) )\n");
	fprintf(f, "		{\n");
	fprintf(f, "			if ( j >= nc ) break;\n");
	fprintf(f, "			index = i*nc+j;\n");
	fprintf(f, "			if ( Z_TYPE_PP(data) == IS_DOUBLE )\n");
	fprintf(f, "				vec[ index ] = (ssc_number_t)Z_DVAL_PP(data);\n");
	fprintf(f, "			else if ( Z_TYPE_PP(data) == IS_LONG )\n");
	fprintf(f, "				vec[ index ] = (ssc_number_t)Z_LVAL_PP(data);\n");
	fprintf(f, "			j++;\n");
	fprintf(f, "		}\n");
	fprintf(f, "		i++;\n");
	fprintf(f, "	}\n");
	fprintf(f, "\n");
	fprintf(f, "	ssc_data_set_matrix( p->p_dat, name, vec, nr, nc );\n");
	fprintf(f, "	free( vec );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_set_table )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	int name_len;\n");
	fprintf(f, "	zval *tab;\n");
	fprintf(f, "	spobj *t;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rsr\", &res, &name, &name_len, &tab ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if (!p || !p->p_dat) return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( t, spobj*, &tab, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if (!t || !t->p_dat) return;\n");
	fprintf(f, "	ssc_data_set_table( p->p_dat, name, t->p_dat );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_get_string )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	int name_len;\n");
	fprintf(f, "	const char *value;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rs\", &res, &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if (!p || !p->p_dat) return;\n");
	fprintf(f, "	value = ssc_data_get_string( p->p_dat, name );\n");
	fprintf(f, "	RETURN_STRING( value ? value : \"\" , 1 )\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_get_number )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	int name_len;\n");
	fprintf(f, "	ssc_number_t value;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rs\", &res, &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if (!p || !p->p_dat) return;\n");
	fprintf(f, "\n");
	fprintf(f, "	if ( ssc_data_get_number( p->p_dat, name, &value ) )\n");
	fprintf(f, "		RETURN_DOUBLE( (double)value );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_get_array )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	int name_len;\n");
	fprintf(f, "	ssc_number_t *arr;\n");
	fprintf(f, "	int i, len;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rs\", &res, &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if (!p || !p->p_dat) return;\n");
	fprintf(f, "	arr = ssc_data_get_array( p->p_dat, name, &len );\n");
	fprintf(f, "	if ( !arr || len < 1 ) return;\n");
	fprintf(f, "	array_init( return_value );\n");
	fprintf(f, "	for( i=0;i<len;i++)\n");
	fprintf(f, "		add_index_double( return_value, i, (double)arr[i] );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_get_matrix )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	int name_len;\n");
	fprintf(f, "	ssc_number_t *mat;\n");
	fprintf(f, "	int i,j, nr, nc;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rs\", &res, &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if ( !p || !p->p_dat ) return;\n");
	fprintf(f, "	mat = ssc_data_get_matrix( p->p_dat, name, &nr, &nc );\n");
	fprintf(f, "	if ( !mat || nr < 1 || nc < 1 ) return;\n");
	fprintf(f, "	array_init( return_value );\n");
	fprintf(f, "	for( i=0;i<nr;i++ )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		zval *row;\n");
	fprintf(f, "		MAKE_STD_ZVAL( row );\n");
	fprintf(f, "		array_init( row );\n");
	fprintf(f, "		for( j=0;j<nc;j++ )\n");
	fprintf(f, "		{\n");
	fprintf(f, "			int index = i*nc+j;\n");
	fprintf(f, "			add_index_double( row, j, (double)mat[index] );\n");
	fprintf(f, "		}\n");
	fprintf(f, "		add_index_zval( return_value, i, row );\n");
	fprintf(f, "	}\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_get_table )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	int name_len;\n");
	fprintf(f, "	ssc_data_t table;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rs\", &res, &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if ( !p || !p->p_dat ) return;\n");
	fprintf(f, "	table = ssc_data_get_table( p->p_dat, name );\n");
	fprintf(f, "	if ( !table ) return;\n");
	fprintf(f, "	\n");
	fprintf(f, "	spobj *t = create_ref( 0, 0);\n");
	fprintf(f, "	t->p_dat = table;\n");
	fprintf(f, "	t->is_dat_const_ref = 1; // ssc_data_get_table returns an internal reference\n");
	fprintf(f, "	ZEND_REGISTER_RESOURCE( return_value, t, spobj_id );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_module_entry )\n");
	fprintf(f, "{\n");
	fprintf(f, "	ssc_entry_t ent;\n");
	fprintf(f, "	long index;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"l\", &index ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	\n");
	fprintf(f, "	if ( ent = ssc_module_entry( index ) )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		array_init( return_value );\n");
	fprintf(f, "		add_assoc_string( return_value, \"name\", (char*)ssc_entry_name( ent ), 1 );\n");
	fprintf(f, "		add_assoc_string( return_value, \"description\", (char*)ssc_entry_description( ent ), 1 );\n");
	fprintf(f, "		add_assoc_long( return_value, \"version\", (long)ssc_entry_version( ent ) );\n");
	fprintf(f, "	}\n");
	fprintf(f, "	else\n");
	fprintf(f, "	{\n");
	fprintf(f, "		RETURN_NULL();\n");
	fprintf(f, "	}\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_module_create )\n");
	fprintf(f, "{\n");
	fprintf(f, "	ssc_module_t mod;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	int name_len;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"s\", &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	\n");
	fprintf(f, "	if ( mod = ssc_module_create( name ) )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		spobj *p = create_ref( mod, 0 );\n");
	fprintf(f, "		ZEND_REGISTER_RESOURCE( return_value, p, spobj_id );\n");
	fprintf(f, "	}\n");
	fprintf(f, "	else\n");
	fprintf(f, "	{\n");
	fprintf(f, "		RETURN_NULL();\n");
	fprintf(f, "	}\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_module_free )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"r\", &res ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if (!p || !p->p_mod ) return;\n");
	fprintf(f, "	ssc_module_free( p->p_mod );\n");
	fprintf(f, "	p->p_mod = 0;\n");
	fprintf(f, "	RETURN_TRUE;\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_module_var_info )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	long index;\n");
	fprintf(f, "	ssc_info_t inf;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rl\", &res, &index ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if ( !p || !p->p_mod ) return;\n");
	fprintf(f, "	if ( inf = ssc_module_var_info( p->p_mod, index ) )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		array_init( return_value );\n");
	fprintf(f, "		add_assoc_long( return_value, \"var_type\", (long)ssc_info_var_type( inf ) );\n");
	fprintf(f, "		add_assoc_long( return_value, \"data_type\", (long)ssc_info_data_type( inf ) );\n");
	fprintf(f, "		add_assoc_string( return_value, \"name\", (char*)ssc_info_name( inf ), 1 );\n");
	fprintf(f, "		add_assoc_string( return_value, \"label\", (char*)ssc_info_label( inf ), 1 );\n");
	fprintf(f, "		add_assoc_string( return_value, \"units\", (char*)ssc_info_units( inf ), 1 );\n");
	fprintf(f, "		add_assoc_string( return_value, \"meta\", (char*)ssc_info_meta( inf ), 1 );\n");
	fprintf(f, "		add_assoc_string( return_value, \"group\", (char*)ssc_info_group( inf ), 1 );\n");
	fprintf(f, "		add_assoc_string( return_value, \"required\", (char*)ssc_info_required( inf ), 1 );\n");
	fprintf(f, "		add_assoc_string( return_value, \"constraints\", (char*)ssc_info_constraints( inf ), 1 );\n");
	fprintf(f, "	}\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_module_exec_set_print )\n");
	fprintf(f, "{\n");
	fprintf(f, "	long print;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"l\", &print ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	\n");
	fprintf(f, "	ssc_module_exec_set_print( print );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_module_exec )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *mm, *dd;\n");
	fprintf(f, "	zval *res1, *res2;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rr\", &res1, &res2 ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( mm, spobj*, &res1, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if ( !mm || !mm->p_mod ) return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( dd, spobj*, &res2, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if ( !dd || !dd->p_dat ) return;\n");
	fprintf(f, "	RETURN_BOOL( ssc_module_exec( mm->p_mod, dd->p_dat ) );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_module_log )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	long index;\n");
	fprintf(f, "	const char *msg;\n");
	fprintf(f, "	int type;\n");
	fprintf(f, "	float time;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rl\", &res, &index ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if ( !p || !p->p_mod ) return;\n");
	fprintf(f, "	\n");
	fprintf(f, "	if ( msg = ssc_module_log( p->p_mod, index, &type, &time ) )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		RETURN_STRING( msg, 1 );\n");
	fprintf(f, "	}\n");
	fprintf(f, "}\n");
	fprintf(f, "		\n");
	fprintf(f, "static zend_function_entry sscphp_functions[] = {\n");
	fprintf(f, "	PHP_FE( sscphp_version, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_build_info, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_create, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_free, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_clear, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_query, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_first, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_next, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_set_string, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_set_number, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_set_array, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_set_matrix, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_set_table, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_get_string, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_get_number, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_get_array, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_get_matrix, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_get_table, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_module_entry, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_module_create, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_module_free, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_module_var_info, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_module_exec_set_print, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_module_exec, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_module_log, NULL )\n");
	fprintf(f, "		{ NULL, NULL, NULL }\n");
	fprintf(f, "};\n");
	fprintf(f, "zend_module_entry sscphp_module_entry = {\n");
	fprintf(f, "	STANDARD_MODULE_HEADER,\n");
	fprintf(f, "	PHP_SSC_EXTNAME,\n");
	fprintf(f, "	sscphp_functions,\n");
	fprintf(f, "	PHP_MINIT(sscphp),\n");
	fprintf(f, "	PHP_MSHUTDOWN(sscphp),\n");
	fprintf(f, "	NULL,\n");
	fprintf(f, "	NULL,\n");
	fprintf(f, "	PHP_MINFO(sscphp),\n");
	fprintf(f, "	PHP_SSC_VERSION,\n");
	fprintf(f, "	STANDARD_MODULE_PROPERTIES\n");
	fprintf(f, "};\n");
	fprintf(f, "// install module\n");
	fprintf(f, "ZEND_GET_MODULE(sscphp)\n");
	fclose(f);
    // add php.ini for module location
    // add Makefile
#if defined(__WXMSW__)  // no windows support yet
    return false;
#elif defined(__WXOSX__)
    fn = m_folder + "/php.ini";
    f = fopen(fn.c_str(), "w");
    if (!f) return false;
    fprintf(f, "extension=%s/sscphp.dylib\n", (const char *)m_folder.c_str());
    fprintf(f, "extension_dir=/\n");
    fclose(f);
    fn = m_folder + "/Makefile";
    f = fopen(fn.c_str(), "w");
    if (!f) return false;
    fprintf(f, "PHPDIR=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/usr/include/php\n");
    fprintf(f, "\n");
    fprintf(f, "sscphp.dylib: sscphp.c\n");
    fprintf(f, "	gcc -mmacosx-version-min=10.9 -g -DWX_PRECOMP -O2  -fno-common -DWX_PRECOMP -O2 -arch x86_64  -Wl,-undefined,dynamic_lookup -fno-common -I$(PHPDIR) -I$(PHPDIR)/TSRM -I$(PHPDIR)/main -I$(PHPDIR)/ext -I$(PHPDIR)/Zend -o sscphp.dylib sscphp.c %s/ssc.dylib\n", (const char *)m_folder.c_str());
    fprintf(f, "\n");
    fprintf(f, "run:\n");
    fprintf(f, "	install_name_tool -change @executable_path/../Frameworks/ssc.dylib %s/ssc.dylib sscphp.dylib\n", (const char *)m_folder.c_str());
    fprintf(f, "	php -c ./php.ini %s.php\n", (const char *)m_name.c_str());
    fprintf(f, "\n");
    fprintf(f, "clean:\n");
    fprintf(f, "	rm sscphp.dylib\n");
    fprintf(f, "\n");
    fprintf(f, "help:\n");
    fprintf(f, "	@echo \"Please check the settings for your system.Your system may not be supported.Please contact sam.support@nrel.gov.System: $(PF) $(VERS)\"\n");
    fclose(f);
    return true;
#elif defined(__WXGTK__)
    fn = m_folder + "/php.ini";
    f = fopen(fn.c_str(), "w");
    if (!f) return false;
    fprintf(f, "extension=%s/sscphp.so\n", (const char *)m_folder.c_str());
    fprintf(f, "extension_dir=/\n");
    fclose(f);
    fn = m_folder + "/Makefile";
    f = fopen(fn.c_str(), "w");
    if (!f) return false;
    fprintf(f, "PHPDIR=/usr/include/php\n");
    fprintf(f, "\n");
    fprintf(f, "sscphp.so: sscphp.c\n");
    fprintf(f, "	gcc -shared -O2 -fPIC -I$(PHPDIR) -I$(PHPDIR)/TSRM -I$(PHPDIR)/main -I$(PHPDIR)/ext -I$(PHPDIR)/Zend -o sscphp.so sscphp.c ./ssc.so\n");
    fprintf(f, "\n");
    fprintf(f, "run:\n");
    fprintf(f, "	php -c ./php.ini %s.php\n", (const char *)m_name.c_str());
    fprintf(f, "\n");
    fprintf(f, "clean:\n");
    fprintf(f, "	rm sscphp.so\n");
    fprintf(f, "\n");
    fprintf(f, "help:\n");
    fprintf(f, "	@echo \"Please check the settings for your system.Your system may not be supported.Please contact sam.support@nrel.gov.System: $(PF) $(VERS)\"\n");
    fclose(f);
    return true;
#else
    return false;
#endif
}

CodeGen_php7::CodeGen_php7(Case *cc, const wxString &folder) : CodeGen_php(cc, folder), CodeGen_Base(cc, folder)
{
}

bool CodeGen_php7::SupportingFiles()
{
	// add c wrapper for builing
	wxString fn = m_folder + "/sscphp.c";
	FILE *f = fopen(fn.c_str(), "w");
	if (!f) return false;
	fprintf(f, "#include <stdio.h>\n");
	fprintf(f, "#include <stdlib.h>\n");
	fprintf(f, "#include <string.h>\n");
	fprintf(f, "#include <ctype.h>\n");
	fprintf(f, "#include <php.h>\n");
	fprintf(f, "#include \"sscapi.h\"\n");
	fprintf(f, "#define PHP_SSC_VERSION \"1.0\"\n");
	fprintf(f, "#define PHP_SSC_EXTNAME \"sscphp\"\n");
	fprintf(f, "typedef struct {\n");
	fprintf(f, "	ssc_module_t p_mod;\n");
	fprintf(f, "	ssc_data_t p_dat;\n");
	fprintf(f, "	int is_dat_const_ref;\n");
	fprintf(f, "} spobj;\n");
	fprintf(f, "static int spobj_id;\n");
	fprintf(f, "#define spobj_name \"sscphpobj\"\n");
	fprintf(f, "static void _free_spobj( zend_resource *rsrc TSRMLS_DC ) {\n");
	fprintf(f, "	spobj *p = (spobj*)rsrc->ptr;\n");
	fprintf(f, "	if ( p->p_mod ) ssc_module_free( p->p_mod );\n");
	fprintf(f, "	if ( p->p_dat && !p->is_dat_const_ref ) ssc_data_free( p->p_dat );\n");
	fprintf(f, "	efree(p);\n");
	fprintf(f, "};\n");
	fprintf(f, "static spobj *create_ref( ssc_module_t *m, ssc_data_t *d ) {\n");
	fprintf(f, "	spobj *p = emalloc(sizeof(spobj));\n");
	fprintf(f, "	p->p_mod = m;\n");
	fprintf(f, "	p->p_dat = d;\n");
	fprintf(f, "	p->is_dat_const_ref = 0;\n");
	fprintf(f, "	return p;\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_MINIT_FUNCTION(sscphp) {\n");
	fprintf(f, "	spobj_id = zend_register_list_destructors_ex( _free_spobj,\n");
	fprintf(f, "			NULL, spobj_name, module_number );\n");
	fprintf(f, "	return SUCCESS;\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_MSHUTDOWN_FUNCTION(sscphp) {\n");
	fprintf(f, "	return SUCCESS;\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_MINFO_FUNCTION(sscphp) {\n");
	fprintf(f, "	char buf[16];\n");
	fprintf(f, "	sprintf(buf, \"188713200\", ssc_version());\n");
	fprintf(f, "	php_info_print_table_start();\n");
	fprintf(f, "	php_info_print_table_header(2, \"NREL SAM Simulation Core(SSC) support\", \"enabled\");\n");
	fprintf(f, "	php_info_print_table_row(2, \"Version\", buf );\n");
	fprintf(f, "	php_info_print_table_row(2, \"Build\", ssc_build_info() );\n");
	fprintf(f, "	php_info_print_table_end();\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_version ) {\n");
	fprintf(f, "	RETURN_LONG( ssc_version() );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_build_info ) {\n");
	fprintf(f, "	RETURN_STRING( ssc_build_info() );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_create ) {\n");
	fprintf(f, "	spobj *p = create_ref(0, ssc_data_create() );\n");
	fprintf(f, "	RETURN_RES(zend_register_resource( (void *)p, spobj_id ));\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_free ) {\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"r\", &res) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if ( !p ) RETURN_FALSE;\n");
	fprintf(f, "	if ( p->p_dat )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		ssc_data_free( p->p_dat );\n");
	fprintf(f, "		p->p_dat = 0;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	RETURN_TRUE;\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_clear ) {\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"r\", &res) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if ( !p ) RETURN_FALSE;\n");
	fprintf(f, "	if ( p->p_dat )\n");
	fprintf(f, "		ssc_data_clear( p->p_dat );\n");
	fprintf(f, "	RETURN_TRUE;\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_unassign )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	size_t name_len;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rs\", &res, &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if ( !p ) RETURN_FALSE;\n");
	fprintf(f, "	if ( p->p_dat )\n");
	fprintf(f, "		ssc_data_unassign( p->p_dat, name );\n");
	fprintf(f, "	RETURN_TRUE;\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_query )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	size_t name_len;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rs\", &res, &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if ( !p ) RETURN_FALSE;\n");
	fprintf(f, "	if ( p->p_dat )\n");
	fprintf(f, "		RETURN_LONG( ssc_data_query( p->p_dat, name ) );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_first )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"r\", &res) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if ( !p ) RETURN_FALSE;\n");
	fprintf(f, "	if ( p->p_dat )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		const char *str = ssc_data_first(p->p_dat);\n");
	fprintf(f, "		if ( str )\n");
	fprintf(f, "			RETURN_STRING( str );\n");
	fprintf(f, "	}\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_next )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"r\", &res) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if ( !p ) RETURN_FALSE;\n");
	fprintf(f, "	if ( p->p_dat )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		const char *str = ssc_data_next( p->p_dat );\n");
	fprintf(f, "		if ( str )\n");
	fprintf(f, "			RETURN_STRING( str );\n");
	fprintf(f, "	}\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_set_string )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	size_t name_len;\n");
	fprintf(f, "	char *value;\n");
	fprintf(f, "	size_t value_len;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rss\", &res, &name, &name_len, &value, &value_len ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if (!p ) RETURN_FALSE;\n");
	fprintf(f, "	if( p->p_dat )\n");
	fprintf(f, "		ssc_data_set_string( p->p_dat, name, value );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_set_number )\n");
	fprintf(f, "{\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	size_t name_len;\n");
	fprintf(f, "	double value;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rsd\", &res, &name, &name_len, &value ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if (!p ) RETURN_FALSE;\n");
	fprintf(f, "	if( p->p_dat )\n");
	fprintf(f, "		ssc_data_set_number( p->p_dat, name, (ssc_number_t)value );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_set_array )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	size_t name_len;\n");
	fprintf(f, "	long i, len;\n");
	fprintf(f, "	zval *arr;\n");
	fprintf(f, "	zval *data;\n");
	fprintf(f, "	HashTable *hash;\n");
	fprintf(f, "	HashPosition pointer;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rsa\", &res, &name, &name_len, &arr ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if (!p || !p->p_dat) return;\n");
	fprintf(f, "	hash = Z_ARRVAL_P(arr);\n");
	fprintf(f, "	len = zend_hash_num_elements( hash );\n");
	fprintf(f, "	if (len < 1 ) RETURN_FALSE;\n");
	fprintf(f, "	ssc_number_t *vec = (ssc_number_t*) malloc( sizeof(ssc_number_t)*len );\n");
	fprintf(f, "	if ( !vec ) RETURN_FALSE;\n");
	fprintf(f, "	i=0;\n");
	fprintf(f, "	for( zend_hash_internal_pointer_reset_ex( hash, &pointer );\n");
	fprintf(f, "			(data = zend_hash_get_current_data_ex(hash, &pointer )) != NULL;\n");
	fprintf(f, "			zend_hash_move_forward_ex(hash, &pointer ) )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		if ( i >= len ) break;\n");
	fprintf(f, "		vec[i] = 0.0f;\n");
	fprintf(f, "		if ( Z_TYPE_P(data) == IS_DOUBLE )\n");
	fprintf(f, "			vec[i] = (ssc_number_t)Z_DVAL_P(data);\n");
	fprintf(f, "		else if ( Z_TYPE_P(data) == IS_LONG )\n");
	fprintf(f, "			vec[i] = (ssc_number_t)Z_LVAL_P(data);\n");
	fprintf(f, "		i++;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	ssc_data_set_array( p->p_dat, name, vec, len );\n");
	fprintf(f, "	free( vec );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_set_matrix )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	size_t name_len;\n");
	fprintf(f, "	long i, j, nr, nc, nlen, index;\n");
	fprintf(f, "	zval *mat;\n");
	fprintf(f, "	zval *data;\n");
	fprintf(f, "	HashTable *hash, *row;\n");
	fprintf(f, "	HashPosition pointer1, pointer2;\n");
	fprintf(f, "	ssc_number_t *vec;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rsa\", &res, &name, &name_len, &mat ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if (!p || !p->p_dat) RETURN_FALSE;\n");
	fprintf(f, "	hash = Z_ARRVAL_P(mat);\n");
	fprintf(f, "	nr = zend_hash_num_elements( hash );\n");
	fprintf(f, "	if ( nr < 1 ) RETURN_FALSE;\n");
	fprintf(f, "	zend_hash_internal_pointer_reset_ex( hash, &pointer1 );\n");
	fprintf(f, "  	data = zend_hash_get_current_data_ex(hash, &pointer1 );\n");
	fprintf(f, "	row = Z_ARRVAL_P(data);\n");
	fprintf(f, "	nc = zend_hash_num_elements( row );\n");
	fprintf(f, "	if ( nc < 1 ) RETURN_FALSE;\n");
	fprintf(f, "	nlen = nr * nc;\n");
	fprintf(f, "	vec = (ssc_number_t*)malloc( sizeof(ssc_number_t)*nlen );\n");
	fprintf(f, "	if ( !vec ) RETURN_FALSE;\n");
	fprintf(f, "	i = 0;\n");
	fprintf(f, "	for( zend_hash_internal_pointer_reset_ex( hash, &pointer1 );\n");
	fprintf(f, "			(data = zend_hash_get_current_data_ex(hash, &pointer1 )) != NULL;\n");
	fprintf(f, "			zend_hash_move_forward_ex(hash, &pointer1) )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		if ( i >= nr ) break;\n");
	fprintf(f, "		if ( Z_TYPE_P(data) != IS_ARRAY ) continue;\n");
	fprintf(f, "		row = Z_ARRVAL_P(data);\n");
	fprintf(f, "		j = 0;\n");
	fprintf(f, "		nc = zend_hash_num_elements( row );\n");
	fprintf(f, "		for( zend_hash_internal_pointer_reset_ex( row, &pointer2 );\n");
	fprintf(f, "			(data = zend_hash_get_current_data_ex( row, &pointer2 )) != NULL;\n");
	fprintf(f, "			zend_hash_move_forward_ex(row, &pointer2 ) )\n");
	fprintf(f, "		{\n");
	fprintf(f, "			if ( j >= nc ) break;\n");
	fprintf(f, "			index = i*nc+j;\n");
	fprintf(f, "			vec[index] = 0.0f;\n");
	fprintf(f, "			if ( Z_TYPE_P(data) == IS_DOUBLE )\n");
	fprintf(f, "				vec[ index ] = (ssc_number_t)Z_DVAL_P(data);\n");
	fprintf(f, "			else if ( Z_TYPE_P(data) == IS_LONG )\n");
	fprintf(f, "				vec[ index ] = (ssc_number_t)Z_LVAL_P(data);\n");
	fprintf(f, "			j++;\n");
	fprintf(f, "		}\n");
	fprintf(f, "		i++;\n");
	fprintf(f, "	}\n");
	fprintf(f, "\n");
	fprintf(f, "	ssc_data_set_matrix( p->p_dat, name, vec, nr, nc );\n");
	fprintf(f, "	free( vec );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_set_table )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	size_t name_len;\n");
	fprintf(f, "	zval *tab;\n");
	fprintf(f, "	spobj *t;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rsr\", &res, &name, &name_len, &tab ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if (!p || !p->p_dat) RETURN_FALSE;\n");
	fprintf(f, "	if ((t = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "//	ZEND_FETCH_RESOURCE( t, spobj*, &tab, -1, spobj_name, spobj_id );\n");
	fprintf(f, "	if (!t || !t->p_dat) RETURN_FALSE;\n");
	fprintf(f, "	ssc_data_set_table( p->p_dat, name, t->p_dat );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_get_string )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	size_t name_len;\n");
	fprintf(f, "	const char *value;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rs\", &res, &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if (!p || !p->p_dat) RETURN_FALSE;\n");
	fprintf(f, "	value = ssc_data_get_string( p->p_dat, name );\n");
	fprintf(f, "	RETURN_STRING( value ? value : \"\" )\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_get_number )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	size_t name_len;\n");
	fprintf(f, "	ssc_number_t value;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rs\", &res, &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if (!p || !p->p_dat) RETURN_FALSE;\n");
	fprintf(f, "\n");
	fprintf(f, "	if ( ssc_data_get_number( p->p_dat, name, &value ) )\n");
	fprintf(f, "		RETURN_DOUBLE( (double)value );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_get_array )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	size_t name_len;\n");
	fprintf(f, "	ssc_number_t *arr;\n");
	fprintf(f, "	int i, len;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rs\", &res, &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if (!p || !p->p_dat) RETURN_FALSE;\n");
	fprintf(f, "	arr = ssc_data_get_array( p->p_dat, name, &len );\n");
	fprintf(f, "	if ( !arr || len < 1 ) RETURN_FALSE;\n");
	fprintf(f, "	array_init( return_value );\n");
	fprintf(f, "	for( i=0;i<len;i++)\n");
	fprintf(f, "		add_index_double( return_value, i, (double)arr[i] );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_get_matrix )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	size_t name_len;\n");
	fprintf(f, "	ssc_number_t *mat;\n");
	fprintf(f, "	int i,j, nr, nc;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rs\", &res, &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if ( !p || !p->p_dat ) RETURN_FALSE;\n");
	fprintf(f, "	mat = ssc_data_get_matrix( p->p_dat, name, &nr, &nc );\n");
	fprintf(f, "	if ( !mat || nr < 1 || nc < 1 ) RETURN_FALSE;\n");
	fprintf(f, "	array_init( return_value );\n");
	fprintf(f, "	for( i=0;i<nr;i++ )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		zval *row;\n");
	fprintf(f, "//		MAKE_STD_ZVAL( row );\n");
	fprintf(f, "		array_init( row );\n");
	fprintf(f, "		for( j=0;j<nc;j++ )\n");
	fprintf(f, "		{\n");
	fprintf(f, "			int index = i*nc+j;\n");
	fprintf(f, "			add_index_double( row, j, (double)mat[index] );\n");
	fprintf(f, "		}\n");
	fprintf(f, "		add_index_zval( return_value, i, row );\n");
	fprintf(f, "	}\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_data_get_table )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	size_t name_len;\n");
	fprintf(f, "	ssc_data_t table;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rs\", &res, &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if ( !p || !p->p_dat ) RETURN_FALSE;\n");
	fprintf(f, "	table = ssc_data_get_table( p->p_dat, name );\n");
	fprintf(f, "	if ( !table ) RETURN_FALSE;\n");
	fprintf(f, "\n");
	fprintf(f, "	spobj *t = create_ref( 0, 0);\n");
	fprintf(f, "	t->p_dat = table;\n");
	fprintf(f, "	t->is_dat_const_ref = 1; // ssc_data_get_table returns an internal reference\n");
	fprintf(f, "	RETURN_RES(zend_register_resource( (void *)t, spobj_id ));\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_module_entry )\n");
	fprintf(f, "{\n");
	fprintf(f, "	ssc_entry_t ent;\n");
	fprintf(f, "	long index;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"l\", &index ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "\n");
	fprintf(f, "	if ( ent = ssc_module_entry( index ) )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		array_init( return_value );\n");
	fprintf(f, "		add_assoc_string( return_value, \"name\", (char*)ssc_entry_name( ent ) );\n");
	fprintf(f, "		add_assoc_string( return_value, \"description\", (char*)ssc_entry_description( ent ) );\n");
	fprintf(f, "		add_assoc_long( return_value, \"version\", (long)ssc_entry_version( ent ) );\n");
	fprintf(f, "	}\n");
	fprintf(f, "	else\n");
	fprintf(f, "	{\n");
	fprintf(f, "		RETURN_NULL();\n");
	fprintf(f, "	}\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_module_create )\n");
	fprintf(f, "{\n");
	fprintf(f, "	ssc_module_t mod;\n");
	fprintf(f, "	char *name;\n");
	fprintf(f, "	size_t name_len;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"s\", &name, &name_len ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "\n");
	fprintf(f, "	if ( mod = ssc_module_create( name ) )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		spobj *p = create_ref( mod, 0 );\n");
	fprintf(f, "		RETURN_RES(zend_register_resource( (void *)p, spobj_id ));\n");
	fprintf(f, "	}\n");
	fprintf(f, "	else\n");
	fprintf(f, "	{\n");
	fprintf(f, "		RETURN_NULL();\n");
	fprintf(f, "	}\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_module_free )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"r\", &res ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if (!p || !p->p_mod ) RETURN_FALSE;\n");
	fprintf(f, "	ssc_module_free( p->p_mod );\n");
	fprintf(f, "	p->p_mod = 0;\n");
	fprintf(f, "	RETURN_TRUE;\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_module_var_info )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	long index;\n");
	fprintf(f, "	ssc_info_t inf;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rl\", &res, &index ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if ( !p || !p->p_mod ) RETURN_FALSE;\n");
	fprintf(f, "	if ( inf = ssc_module_var_info( p->p_mod, index ) )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		array_init( return_value );\n");
	fprintf(f, "		add_assoc_long( return_value, \"var_type\", (long)ssc_info_var_type( inf ) );\n");
	fprintf(f, "		add_assoc_long( return_value, \"data_type\", (long)ssc_info_data_type( inf ) );\n");
	fprintf(f, "		add_assoc_string( return_value, \"name\", (char*)ssc_info_name( inf ) );\n");
	fprintf(f, "		add_assoc_string( return_value, \"label\", (char*)ssc_info_label( inf ) );\n");
	fprintf(f, "		add_assoc_string( return_value, \"units\", (char*)ssc_info_units( inf ) );\n");
	fprintf(f, "		add_assoc_string( return_value, \"meta\", (char*)ssc_info_meta( inf ) );\n");
	fprintf(f, "		add_assoc_string( return_value, \"group\", (char*)ssc_info_group( inf ) );\n");
	fprintf(f, "		add_assoc_string( return_value, \"required\", (char*)ssc_info_required( inf ) );\n");
	fprintf(f, "		add_assoc_string( return_value, \"constraints\", (char*)ssc_info_constraints( inf ) );\n");
	fprintf(f, "	}\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_module_exec_set_print )\n");
	fprintf(f, "{\n");
	fprintf(f, "	long print;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"l\", &print ) == FAILURE )\n");
	fprintf(f, "		return;\n");
	fprintf(f, "\n");
	fprintf(f, "	ssc_module_exec_set_print( print );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_module_exec )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *mm, *dd;\n");
	fprintf(f, "	zval *res1, *res2;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rr\", &res1, &res2 ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((mm = (spobj *)zend_fetch_resource_ex(res1, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if ( !mm || !mm->p_mod ) RETURN_FALSE;\n");
	fprintf(f, "	if ((dd = (spobj *)zend_fetch_resource_ex(res2, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if ( !dd || !dd->p_dat ) RETURN_FALSE;\n");
	fprintf(f, "	RETURN_BOOL( ssc_module_exec( mm->p_mod, dd->p_dat ) );\n");
	fprintf(f, "}\n");
	fprintf(f, "PHP_FUNCTION( sscphp_module_log )\n");
	fprintf(f, "{\n");
	fprintf(f, "	spobj *p;\n");
	fprintf(f, "	zval *res;\n");
	fprintf(f, "	long index;\n");
	fprintf(f, "	const char *msg;\n");
	fprintf(f, "	int type;\n");
	fprintf(f, "	float time;\n");
	fprintf(f, "	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, \"rl\", &res, &index ) == FAILURE )\n");
	fprintf(f, "		RETURN_FALSE;\n");
	fprintf(f, "	if ((p = (spobj *)zend_fetch_resource_ex(res, spobj_name, spobj_id )) == NULL) {\n");
	fprintf(f, "	    RETURN_FALSE;\n");
	fprintf(f, "	}\n");
	fprintf(f, "	if ( !p || !p->p_mod ) RETURN_FALSE;\n");
	fprintf(f, "\n");
	fprintf(f, "	if ( msg = ssc_module_log( p->p_mod, index, &type, &time ) )\n");
	fprintf(f, "	{\n");
	fprintf(f, "		RETURN_STRING( msg );\n");
	fprintf(f, "	}\n");
	fprintf(f, "}\n");
	fprintf(f, "\n");
	fprintf(f, "static zend_function_entry sscphp_functions[] = {\n");
	fprintf(f, "	PHP_FE( sscphp_version, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_build_info, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_create, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_free, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_clear, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_query, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_first, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_next, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_set_string, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_set_number, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_set_array, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_set_matrix, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_set_table, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_get_string, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_get_number, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_get_array, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_get_matrix, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_data_get_table, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_module_entry, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_module_create, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_module_free, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_module_var_info, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_module_exec_set_print, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_module_exec, NULL )\n");
	fprintf(f, "		PHP_FE( sscphp_module_log, NULL )\n");
	fprintf(f, "		{ NULL, NULL, NULL }\n");
	fprintf(f, "};\n");
	fprintf(f, "zend_module_entry sscphp_module_entry = {\n");
	fprintf(f, "	STANDARD_MODULE_HEADER,\n");
	fprintf(f, "	PHP_SSC_EXTNAME,\n");
	fprintf(f, "	sscphp_functions,\n");
	fprintf(f, "	PHP_MINIT(sscphp),\n");
	fprintf(f, "	PHP_MSHUTDOWN(sscphp),\n");
	fprintf(f, "	NULL,\n");
	fprintf(f, "	NULL,\n");
	fprintf(f, "	PHP_MINFO(sscphp),\n");
	fprintf(f, "	PHP_SSC_VERSION,\n");
	fprintf(f, "	STANDARD_MODULE_PROPERTIES\n");
	fprintf(f, "};\n");
	fprintf(f, "// install module\n");
	fprintf(f, "ZEND_GET_MODULE(sscphp)\n");
	fclose(f);
	// add php.ini for module location
	fn = m_folder + "/php.ini";
	f = fopen(fn.c_str(), "w");
	if (!f) return false;
	fprintf(f, "extension=%s/sscphp.so\n", (const char *)m_folder.c_str());
	fclose(f);
	// add Makefile (currently linux only)
	fn = m_folder + "/Makefile";
	f = fopen(fn.c_str(), "w");
	if (!f) return false;
	fprintf(f, "PHPDIR=/usr/include/php\n");
	fprintf(f, "\n");
	fprintf(f, "sscphp.so: sscphp.c\n");
	fprintf(f, "	gcc -shared -O2 -fPIC -I$(PHPDIR) -I$(PHPDIR)/TSRM -I$(PHPDIR)/main -I$(PHPDIR)/ext -I$(PHPDIR)/Zend -o sscphp.so sscphp.c ./ssc.so\n");
	fprintf(f, "\n");
	fprintf(f, "run:\n");
	fprintf(f, "	php -c ./php.ini %s.php\n", (const char *)m_name.c_str());
	fprintf(f, "\n");
	fprintf(f, "clean:\n");
	fprintf(f, "	rm sscphp.so\n");
	fprintf(f, "\n");
	fprintf(f, "help:\n");
	fprintf(f, "	@echo \"Please check the settings for your system.Your system may not be supported.Please contact sam.support@nrel.gov.System: $(PF) $(VERS)\"\n");
	fclose(f);
	return true;
}

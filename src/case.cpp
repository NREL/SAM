/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/SAM/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#include <wx/datstrm.h>
#include <wx/wfstream.h>

#include <wex/utils.h>

#include "case.h"
#include "equations.h"
#include "main.h"
#include "library.h"
#include "invoke.h"
#include <lk/stdlib.h>
#include <rapidjson/writer.h>
#include <rapidjson/stringbuffer.h>
#include <rapidjson/prettywriter.h> // for stringify JSON
#include <rapidjson/filereadstream.h>
#include <rapidjson/filewritestream.h>


#define __SAVE_AS_JSON__ 1
#define __LOAD_AS_JSON__ 1
     
CaseCallbackContext::CaseCallbackContext( Case *cc, const wxString &name )
	: m_case(cc), m_name(name)
{	
	// nothing to do
}

void CaseCallbackContext::SetCase( Case *cc, const wxString &name )
{
	m_case = cc;
	m_name = name;
}

wxString CaseCallbackContext::GetName() { return m_name; }
VarTable &CaseCallbackContext::GetValues(size_t ndxHybrid) { return GetCase().Values(ndxHybrid); }
Case &CaseCallbackContext::GetCase() { return *m_case; }

void CaseCallbackContext::SetupLibraries( lk::env_t * )
{
	/* nothing here - for descendents like UICallbackContext or ResultsCallbackContext */
}

class CaseScriptInterpreter : public VarTableScriptInterpreter
{
	Case *m_case;
	size_t m_ndxHybrid;
public:	
	CaseScriptInterpreter( lk::node_t *tree, lk::env_t *env, VarTable *vt, Case *cc, size_t ndxHybrid )
		: VarTableScriptInterpreter( tree, env, vt ), m_case( cc ), m_ndxHybrid(ndxHybrid)
	{
	}
	virtual ~CaseScriptInterpreter( ) { /* nothing to do */ };
	virtual bool special_set( const lk_string &name, lk::vardata_t &val )
	{
		if ( VarTableScriptInterpreter::special_set( name, val ) )
		{
			m_case->VariableChanged( name, m_ndxHybrid );
			return true;
		}
		else
			return false;
	}
};
	
bool CaseCallbackContext::Invoke( lk::node_t *root, lk::env_t *parent_env, size_t ndxHybrid )
{
	lk::env_t local_env( parent_env );
	
	local_env.register_funcs( lk::stdlib_basic() );
	local_env.register_funcs( lk::stdlib_sysio() );
	local_env.register_funcs( lk::stdlib_math() );
	local_env.register_funcs( lk::stdlib_string() );
	local_env.register_funcs( lk::stdlib_wxui(), this );
	local_env.register_funcs( invoke_general_funcs(), this );
	local_env.register_funcs( invoke_casecallback_funcs(), this );
	local_env.register_funcs( invoke_ssc_funcs(), this );
	
	// add other callback environment functions
	SetupLibraries( &local_env );

	try {
 
        CaseScriptInterpreter e( root, &local_env, &GetValues(ndxHybrid), m_case, ndxHybrid);
		if ( !e.run() )
		{
			wxString text = "Could not evaluate callback function:" +  m_name + "\n";
			for (size_t i=0;i<e.error_count();i++)
				text += e.get_error(i);

			wxShowTextMessageDialog( text );
		}
		
	} catch(std::exception &e ){
		wxShowTextMessageDialog( "Could not evaluate callback function: " + m_name + wxString("\n\n") + e.what());
		return false;
	}

	return true;
}



static void fcall_technology_pCase( lk::invoke_t &cxt )
{
	LK_DOC( "technology", "Return the current technology option name", "(void):string" );
	if ( Case *cc = static_cast<Case*>( cxt.user_data() ) ) 
		cxt.result().assign( cc->GetTechnology() );
}

static void fcall_financing_pCase( lk::invoke_t &cxt )
{
	LK_DOC( "financing", "Return the current financing option name", "(void):string" );
	if ( Case *cc = static_cast<Case*>( cxt.user_data() ) )
		cxt.result().assign( cc->GetFinancing() );
}

static void fcall_analysis_period_pCase(lk::invoke_t& cxt)
{
	LK_DOC("analysis_period", "Gets current analysis period for case, used for analysis period dependent variables.", "():variant");
	if (Case* cc = static_cast<Case*>(cxt.user_data()))
		cxt.result().assign(cc->m_analysis_period);
}

static void fcall_analysis_period_old_pCase(lk::invoke_t& cxt)
{
	LK_DOC("analysis_period_old", "Gets previous analysis period for case, used for analysis period dependent variables.", "():variant");
	if (Case* cc = static_cast<Case*>(cxt.user_data()))
		cxt.result().assign(cc->m_analysis_period_old);
}


CaseEvaluator::CaseEvaluator( Case *cc, VarTable &vars, EqnFastLookup &efl )
	: EqnEvaluator( vars, efl )
{
	m_case = cc;
	m_vt = &vars;
}

void CaseEvaluator::SetupEnvironment( lk::env_t &env )
{
	// call base version first to register standard functions
	EqnEvaluator::SetupEnvironment( env );

	env.register_func( fcall_technology_pCase, m_case );
	env.register_func(fcall_financing_pCase, m_case);
	env.register_func(fcall_analysis_period_pCase, m_case);
	env.register_func(fcall_analysis_period_old_pCase, m_case);
	env.register_funcs( invoke_ssc_funcs() );
	env.register_funcs( invoke_equation_funcs() );
}
	
int CaseEvaluator::CalculateAll(size_t ndxHybrid)
{
	int nlibchanges = 0;

	/* Check for project file upgrade
	If file version < SAM version then skip recalculate all in Case LoadValuesFroExternal Source*/
	size_t sam_ver = SamApp::Version();
	size_t file_ver = SamApp::Project().GetVersionInfo();
	bool update_lib = (sam_ver == file_ver);

	if (update_lib)	{
		for (VarInfoLookup::iterator it = m_case->Variables(ndxHybrid).begin();
			it != m_case->Variables(ndxHybrid).end();
			++it) {
			if (it->second->Flags & VF_LIBRARY
				&& it->second->Type == VV_STRING)	{
				wxArrayString changed;
				if (!UpdateLibrary(it->first, changed, ndxHybrid))
					return -1;
				else
					nlibchanges += changed.size();
			}
		}
	}

	int nevals = EqnEvaluator::CalculateAll();
	if ( nevals >= 0 ) nevals += nlibchanges;

	return nevals;	
}

int CaseEvaluator::Changed( const wxArrayString &vars, size_t ndxHybrid )
{
	int nlibchanges=0;
	wxArrayString trigger_list;
	for( size_t i=0;i<vars.size();i++ )
	{
		trigger_list.Add( vars[i] );

		wxArrayString changed;
		bool ok = UpdateLibrary( vars[i], changed, ndxHybrid );
		if ( ok && changed.size() > 0 )
		{
			for( size_t j=0;j<changed.size();j++ )
			{
				m_updated.Add( changed[j] );
				trigger_list.Add( changed[j] );
				nlibchanges++;
			}
		}
		else if ( !ok )
			return -1;
	}
	
	int nevals = EqnEvaluator::Changed( trigger_list );
	if ( nevals > 0 ) nevals += nlibchanges;

	return nevals;
}

int CaseEvaluator::Changed( const wxString &trigger, size_t ndxHybrid )
{
	wxArrayString list;
	list.Add(trigger);
	return Changed( list, ndxHybrid );
}

bool CaseEvaluator::UpdateLibrary( const wxString &trigger, wxArrayString &changed, size_t ndxHybrid )
{
	size_t nerrors = 0;
	VarInfo *vi = m_case->Variables(ndxHybrid).Lookup( trigger );
	VarValue *vv = m_vt->Get(trigger);
	if (vv && vv->Type() == VV_STRING && vi && vi->Flags & VF_LIBRARY)
	{
		if ( vi->IndexLabels.size() == 2 )
		{
			// lookup the library name in vi->IndexLabels
			wxString name = vi->IndexLabels[0];
			int varindex = wxAtoi( vi->IndexLabels[1] );
		
			if ( Library *lib = Library::Find( name ) )
			{
				// find the entry
				int entry = lib->FindEntry( vv->String() );
								
				if (entry < 0 || !lib->ApplyEntry(entry, varindex, *m_vt, changed))
				{
//					nerrors++;
//					m_errors.Add("Library error: '" + vv->String() + "'  is not available in the " + name + " library." );
					wxArrayString errs( lib->GetErrors() );
					for (size_t k = 0; k < errs.size(); k++)
					{
						if (!errs[k].IsEmpty())
						{
							m_errors.Add(errs[k]);
							nerrors++;
						}
					}
				}
#ifdef _DEBUG
				else
					wxLogStatus( "applied " + name + ":" + vv->String() + " = " + wxJoin(changed,',') );
#endif

			}
			else
			{
				nerrors++;
				m_errors.Add( "Could not locate referenced library: " + name);
			}
		}
		else
		{
			nerrors++;
			m_errors.Add( "invalid library specification: " + wxJoin(vi->IndexLabels, ',') );
		}
	}

	return nerrors == 0;
}


Case::Case()
	: m_config(0), m_baseCase( this, wxEmptyString ), m_parametric( this )
{
	m_analysis_period = 0;
	m_analysis_period_old = 0;
}

Case::~Case()
{
	ClearListeners();
}
	
Object *Case::Duplicate()
{
	Case *c = new Case();
	c->Copy(this);
	return c;
}

bool Case::Copy( Object *obj )
{
	if ( Case *rhs = dynamic_cast<Case*>( obj ) )
	{
		m_config = 0;
		if (rhs->m_config) {
			SetConfiguration(rhs->m_config->TechnologyFullName, rhs->m_config->Financing);
			m_vals.resize(rhs->m_config->Technology.size());
			for (size_t i = 0; i < rhs->m_config->Technology.size(); i++)
				m_vals[i].Copy(rhs->m_vals[i]);
		}
		m_baseCase.Copy( rhs->m_baseCase );
		m_properties = rhs->m_properties;
		m_notes = rhs->m_notes;
		m_parametric.Copy(rhs->m_parametric);
		m_excelExch.Copy(rhs->m_excelExch);
		m_stochastic.Copy(rhs->m_stochastic);
		m_pvuncertainty.Copy(rhs->m_pvuncertainty);
		m_analysis_period = rhs->m_analysis_period;
		m_analysis_period_old = rhs->m_analysis_period_old;
		m_graphs.clear();
		for( size_t i=0;i<rhs->m_graphs.size();i++ )
			m_graphs.push_back( rhs->m_graphs[i] );
		
		return true;
	}
	else
		return false;
}

wxString Case::GetTypeName()
{
	return "sam.case";
}

void Case::Write( wxOutputStream &_o )
{
	SendEvent( CaseEvent( CaseEvent::SAVE_NOTIFICATION ) );

	wxDataOutputStream out(_o);

	out.Write8( 0x9b );
	out.Write8( 8 ); // include hybrids (multiple m_vals)

	wxString tech, fin;
	if ( m_config != 0 )
	{
		tech = m_config->TechnologyFullName;
		fin = m_config->Financing;
	}

	// write data
	out.WriteString( tech );
	out.WriteString( fin );

	out.Write32((wxUint32)m_vals.size());
	for (size_t i = 0; i < m_vals.size(); i++)
		m_vals[i].Write(_o);

	m_baseCase.Write( _o );
	m_properties.Write( _o );
	m_notes.Write( _o );
	m_excelExch.Write( _o );

	out.Write32((wxUint32)m_graphs.size() );
	for( size_t i=0;i<m_graphs.size();i++ )
		m_graphs[i].Write( _o );

	m_perspective.Write( _o );

	m_parametric.Write( _o );
	m_stochastic.Write( _o );
	m_pvuncertainty.Write(_o);

	out.Write8( 0x9b );
}



bool Case::Read( wxInputStream &_i )
{
	wxDataInputStream in(_i);

	m_lastError = wxEmptyString;

	wxUint8 code = in.Read8();
	wxUint8 ver = in.Read8(); // version

	// read data
	wxString tech = in.ReadString();
	wxString fin = in.ReadString();
	wxArrayString techary = wxSplit(tech, ' ');

	if ( !SetConfiguration( tech, fin ) )
	  {
		wxLogStatus( "Notice: errors occurred while setting configuration during project file read.  Continuing...\n\n" + tech + "/" + fin);
	  }

	// TODO read in the variable table(s) - test hybrid to non-hybrid and vice versa
	size_t i;
	for (i = 0; i < m_oldVals.size(); i++)
		m_oldVals[i].clear();
	m_oldVals.clear();
	LoadStatus di;


	size_t n = 1;
	if (ver >=8)
		n = in.Read32();
	
	m_oldVals.resize(n);

	for (i = 0; i < n; i++) {

		bool ok = LoadValuesFromExternalSource(_i, i, &di, &m_oldVals[i]);

		if (!ok || di.not_found.size() > 0 || di.wrong_type.size() > 0 || di.nread != m_vals[i].size()) {
			wxLogStatus("discrepancy reading in values from project file: %d not found, %d wrong type, %d read != %d in config",
				(int)di.not_found.size(), (int)di.wrong_type.size(), (int)di.nread, (int)m_vals.size());

			if (di.not_found.size() > 0) {
				wxLogStatus("\not found: " + wxJoin(di.not_found, ','));
			}
			if (di.wrong_type.size() > 0) {
				wxLogStatus("\twrong type: " + wxJoin(di.wrong_type, ','));
			}
			if (m_vals.size() > m_oldVals.size()) {
				for (auto& newVal : m_vals[i]) {
					if (!m_oldVals[i].Get(newVal.first))
						wxLogStatus("%s, %s configuration variable %s missing from project file", tech.c_str(), fin.c_str(), newVal.first.c_str());
				}
			}
			if (m_vals[i].size() < m_oldVals[i].size()) {
				for (auto& oldVal : m_oldVals[i]) {
					if (!m_vals[i].Get(oldVal.first))
						wxLogStatus("%s, %s project file variable %s missing from configuration", tech.c_str(), fin.c_str(), oldVal.first.c_str());
				}
			}

		}
	}
	
	if ( ver <= 1 )
	{
		m_baseCase.Clear();
		VarTable dum;
		if ( !dum.Read( _i ) )
		  {
		    wxLogStatus("error reading dummy var table in Case::Read");
			m_lastError += "Error reading dummy var table in Case::Read \n";
			dum.clear();
		  }
	}
	else
		if ( !m_baseCase.Read( _i ) )
		  {
		    wxLogStatus("error reading m_baseCase in Case::Read");
			m_lastError += "Error reading m_baseCase in Case::Read \n";
			m_baseCase.Clear();
		}

	if ( !m_properties.Read( _i ) )
	  {
	    wxLogStatus("error reading m_properties in Case::Read");
		m_lastError += "Error reading m_properties in Case::Read \n";
		m_properties.clear();
	  }
	if ( !m_notes.Read( _i ) )
	  {
	    wxLogStatus("error reading m_notes in Case::Read");
		m_lastError += "Error reading m_notes in Case::Read \n";
		m_notes.clear();
	}

	if ( ver >= 3 )
	{
		if (!m_excelExch.Read( _i ))
		  {
			wxLogStatus("error reading excel exchange data in Case::Read");
			m_lastError += "Error reading excel exchange data in Case::Read \n";
			//m_excelExch.clear();
		}
	}

	if ( ver >= 4 )
	{
		m_graphs.clear();
		size_t n = in.Read32();
		for( size_t i=0;i<n;i++) 
		{
			Graph g;
			if ( !g.Read( _i ) )
			  {
			    wxLogStatus("error reading Graph %d of %d in Case::Read", (int)i, (int)n);
				m_lastError += wxString::Format("Error reading Graph %d of %d in Case::Read", (int)i, (int)n);
			}
			else
				m_graphs.push_back( g );
		}

		if ( !m_perspective.Read( _i ) )
		  {
		    wxLogStatus("error reading perspective of results viewer in Case::Read");
			m_lastError += "Error reading perspective of results viewer in Case::Read \n";
			m_perspective.clear();
		}
	}

	if ( ver >= 5 )
	{
		if (!m_parametric.Read(_i))
		{
			wxLogStatus("error reading parametric simulation information in Case::Read");
			m_lastError += "Error reading parametric simulation information in Case::Read \n";
			m_parametric.ClearRuns();
//			m_parametric.clear();
		}
	}

	if ( ver >= 6 )
	{
		if ( !m_stochastic.Read( _i ) )
		  {
		    wxLogStatus("error reading stochastic simulation information in Case::Read");
			m_lastError += "Error reading stochastic simulation information in Case::Read \n";
//			m_stochastic.clear();
		}
	}

	if (ver >= 7)
	{
		if (!m_pvuncertainty.Read(_i))
		{
			wxLogStatus("error reading pvuncertainty simulation information in Case::Read");
			m_lastError += "Error reading pvuncertainty simulation information in Case::Read \n";
			//			m_pvuncertainty.clear();
		}
	}

	return (in.Read8() == code);
}

bool Case::SaveDefaults(bool quiet)
{
	if (!m_config)
	{
		return false;
	}
	wxString file;
	file = SamApp::GetRuntimePath() + "/defaults/"
		+ m_config->TechnologyFullName + "_" + m_config->Financing + ".json";
	if (!quiet && wxNO == wxMessageBox("Save defaults for configuration:\n\n"
		+ m_config->TechnologyFullName + " / " + m_config->Financing,
		"Save Defaults", wxYES_NO))
		return false;


    rapidjson::Document doc;
    doc.SetObject();

	for (size_t i = 0; i < m_config->Technology.size(); i++) {

		// set default library_folder_list blank
		VarValue* vv = m_vals[i].Get("library_folder_list");
		if (vv)	vv->Set(wxString("x"));

		wxArrayString asCalculated, asIndicator;
		auto& vil = Variables(i);
		for (auto& var : vil) {
			if (var.second->Flags & VF_CHANGE_MODEL)
				continue;
			else if (var.second->Flags & VF_CALCULATED)
				asCalculated.push_back(var.first);
			else if (var.second->Flags & VF_INDICATOR)
				asIndicator.push_back(var.first);
		}
        
        if (m_config->Technology.size()>1) { //hybrid - write out compute module
            rapidjson::Document json_table(&doc.GetAllocator()); // for table inside of json document.
            m_vals[i].Write_JSON(json_table, asCalculated, asIndicator);
            wxString name = m_config->Technology[i]; // TODO: convert to compute module name - can be m_config->Simulations[i] for technologies - need name for rest
            doc.AddMember(rapidjson::Value(name.c_str(), (rapidjson::SizeType)name.size(), doc.GetAllocator()).Move(), json_table.Move(), doc.GetAllocator());
        }
        else {
            m_vals[i].Write_JSON(doc, asCalculated, asIndicator);
        }
        
	}


    rapidjson::StringBuffer os;
    rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(os); // MSPT/MP 64MB JSON, 6.7MB txt, JSON Zip 242kB
    //writer.SetMaxDecimalPlaces(6); // sets small values (e.g. 2.3e-8 to zero so cannot use
    doc.Accept(writer);
    wxString sfn = file;
    wxFileName fn(sfn);
    wxFFileOutputStream out(sfn);
    out.Write(os.GetString(), os.GetSize());
    out.Close();
    
    wxLogStatus("Case: defaults saved for " + file);
	return true;

}


bool Case::SaveAsSSCJSON(wxString filename)
{
	// similar to CodeGen_json but uses RapidJSON instead of fprintf (prototype for rewriting codegenerator) 
	// run equations to update calculated values
	// write out all inputs for all compute modules
	ConfigInfo* cfg = GetConfiguration();
	if (!cfg) return false;

	char json_string[32256];
	// TODO - finish and concatenate based on hybrid compute modules - see test input json for cmod_hybrid
	for (size_t i = 0; i < cfg->Technology.size(); i++) {

		VarTable inputs = Values(i); // SAM VarTable for the case
		CaseEvaluator eval(this, inputs, Equations(i));
		int n = eval.CalculateAll(i);
		if (n < 0) return false;

		// get list of compute modules from case configuration
		wxArrayString simlist = cfg->Simulations;
		if (simlist.size() == 0) return false;
		// go through and translate all SAM UI variables to SSC variables
		ssc_data_t p_data = ssc_data_create();

		for (size_t kk = 0; kk < simlist.size(); kk++)
		{
			ssc_module_t p_mod = ssc_module_create(simlist[kk].c_str());
			if (!p_mod)	continue;

			int pidx = 0;
			while (const ssc_info_t p_inf = ssc_module_var_info(p_mod, pidx++)) {
				int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
				int ssc_data_type = ssc_info_data_type(p_inf); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX
				const char* var_name = ssc_info_name(p_inf);
				wxString name(var_name); // assumed to be non-null
				wxString reqd(ssc_info_required(p_inf));

				if (var_type == SSC_INPUT || var_type == SSC_INOUT) {
					int existing_type = ssc_data_query(p_data, ssc_info_name(p_inf));
					if (existing_type != ssc_data_type) {
						if (VarValue* vv = Values(i).Get(name)) {
							if (!VarValueToSSC(vv, p_data, name, true))
								wxLogStatus("Error translating data from SAM UI to SSC for " + name);
						}
					}
				}
			}
		}
		strcpy(json_string,ssc_data_to_json(p_data)); // TODO - test this

	}

	rapidjson::Document doc;
	doc.Parse(json_string);

	rapidjson::StringBuffer os;
	rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(os);
	doc.Accept(writer);
	wxFFileOutputStream out(filename);
	out.Write(os.GetString(), os.GetSize());
	out.Close();

	return true;
}


bool Case::SaveAsJSON(bool quiet, wxString fn, wxString case_name)
{
	if (!m_config) return false;
	wxFileName filename = wxFileName(fn);
	wxString file;

	if (filename.IsOk()) {
		file = filename.GetLongPath();
		if (!quiet && wxNO == wxMessageBox("Save defaults for configuration:\n\n"
			+ m_config->TechnologyFullName + " / " + m_config->Financing,
			"Save Defaults", wxYES_NO))
			return false;


		for (size_t i = 0; i < m_config->Technology.size(); i++) {
			// set default library_folder_list blank
			VarValue* vv = m_vals[i].Get("library_folder_list");
			if (vv)	vv->Set(wxString("x"));

			m_vals[i].Set("Technology", VarValue(m_config->Technology[i]));
			m_vals[i].Set("Financing", VarValue(m_config->Financing));
			m_vals[i].Set("Case_name", VarValue(case_name));

			wxArrayString asCalculated, asIndicator;
			auto& vil = Variables(i);
			for (auto& var : vil) {
				if (var.second->Flags & VF_CHANGE_MODEL)
					continue;
				else if (var.second->Flags & VF_CALCULATED)
					asCalculated.push_back(var.first);
				else if (var.second->Flags & VF_INDICATOR)
					asIndicator.push_back(var.first);
			}
			m_vals[i].Write_JSON(file.ToStdString(), asCalculated, asIndicator);
		}
		wxLogStatus("Case: saved as JSON: " + file);
		return true;
	}
	else {
		return false;
	}

}


bool Case::VarTablesFromJSONFile(std::vector<VarTable>& vt, const std::string& file)
{
	if (!m_config ||(vt.size() < 1) || (m_config->Technology.size()<1))
		return false;
	else if (m_config->Technology.size() < 2)
		return vt[0].Read_JSON(file);
	else { // hybrid
		rapidjson::Document doc, table;
		wxFileInputStream fis(file);

		if (!fis.IsOk()) {
			wxLogError(wxS("Couldn't open the file '%s'."), file);
			return false;
		}
		wxStringOutputStream os;
		fis.Read(os);

		rapidjson::StringStream is(os.GetString().c_str());

		doc.ParseStream(is);
		if (doc.HasParseError()) {
			wxLogError(wxS("Could not read the json file string conversion '%s'."), file);
			return false;
		}
		else {
			bool ret = true;
			for (size_t i = 0; i < m_config->Technology.size(); i++) {
				table.CopyFrom(doc[m_config->Technology[i].ToStdString().c_str()], doc.GetAllocator());
				ret = ret && vt[i].Read_JSON(table);
			}
			return ret;
		}
	}
}



bool Case::VarTableFromJSONFile(VarTable* vt, const std::string& file)
{
	if (!vt)
		return false;
	else
		return vt->Read_JSON(file);
}


bool Case::LoadValuesFromExternalSource( wxInputStream &in, size_t ndxHybrid, LoadStatus *di, VarTable *oldvals, bool binary)
{
	VarTable vt;
// All project files are assumed to be stored as binary
	bool read_ok = true;
	if (!binary) // text call from LoadDefaults
		read_ok = vt.Read_text(in);
	else
		read_ok = vt.Read(in);

	if (!read_ok)
	{
		wxString e("Error reading inputs from external source");
		if ( di ) di->error = e;
		wxLogStatus( e );
		return false;
	}

	if ( di ) di->nread = vt.size();

	bool ok = (vt.size() == m_vals[ndxHybrid].size());
	// copy over values for variables that already exist
	// in the configuration
	for( VarTable::iterator it = vt.begin();it != vt.end();	++it )	{

        if (VarValue* vv = m_vals[ndxHybrid].Get(it->first))
        {
            if (vv->Type() == it->second->Type()) {
                vv->Copy(*(it->second));
                if (oldvals) oldvals->Set(it->first, *(it->second));
            }
            else
            {
                if (di) di->wrong_type.Add(it->first + wxString::Format(": expected:%d got:%d", vv->Type(), it->second->Type()));
                if (oldvals) oldvals->Set(it->first, *(it->second));
                ok = false;
            }
        }
        else
        {
            if (di) di->not_found.Add(it->first);
            if (oldvals) oldvals->Set(it->first, *(it->second));
            ok = false;
        }
/* TODO: hybrids - turn back on after all vartable dependencies resolved
        if (RecalculateAll(ndxHybrid) < 0)
        {
            wxString e("Error recalculating equations after loading values from external source");
            if (di) di->error = e;
            wxLogStatus(e);
            return false;
        }
 */
	}


	return ok;
}





bool Case::LoadValuesFromExternalSource(const VarTable& vt, size_t ndxHybrid, LoadStatus* di, VarTable* oldvals)
{
	bool read_ok = true;
	if (!read_ok)
	{
		wxString e("Error reading inputs from external source");
		if ( di ) di->error = e;
		wxLogStatus( e );
		return false;
	}

	if ( di ) di->nread = vt.size();

	bool ok = (vt.size() == m_vals[ndxHybrid].size());
	// copy over values for variables that already exist
	// in the configuration
	for( VarTable::const_iterator it = vt.begin();	it != vt.end();	++it )	{

		if (VarValue* vv = m_vals[ndxHybrid].Get(it->first))
		{
			if (vv->Type() == it->second->Type()) {
				vv->Copy(*(it->second));
				if (oldvals) oldvals->Set(it->first, *(it->second));
			}
			else
			{
				if (di) di->wrong_type.Add(it->first + wxString::Format(": expected:%d got:%d", vv->Type(), it->second->Type()));
				if (oldvals) oldvals->Set(it->first, *(it->second));
				ok = false;
			}
		}
		else
		{
			if (di) di->not_found.Add(it->first);
			if (oldvals) oldvals->Set(it->first, *(it->second));
			ok = false;
		}
/*
		if (RecalculateAll(ndxHybrid, true) < 0) // shj - testing
		{
			wxString e("Error recalculating equations after loading values from external source");
			if (di) di->error = e;
			wxLogStatus(e);
			return false;
		}
*/
	}


	return ok;
}

bool Case::PreRunSSCJSON(const wxString& tech, const wxString& fin, const wxString& fn, wxString* error_msg)
{
	m_baseCase.Clear();
	m_config = SamApp::Config().Find(tech, fin);

	return m_baseCase.InvokeSSC(false, fn);
}


/*
bool Case::LoadFromSSCJSON(wxString fn, wxString* pmsg)
{
	if (!m_config) return false;
	bool binary = true;
	LoadStatus di;
	wxString message;
	bool ok = false;
	VarTable vt;
	wxString schk = fn;
	if (wxFileExists(schk))
	{
		ok = VarTablesFromJSONFile(&vt, fn.ToStdString());
		// TODO - separate into hybrid tables for each hybrid technology
		// if no hybrids, then m_config->Technology.size() == 1 and process normally
		// else have separate vartables for each technology and one for the remaining variables (financial model, grid and utility rate)
		// separated in startup.lk by bin_name
		m_oldVals.clear();
		ok &= LoadValuesFromExternalSource(vt, &di, &m_oldVals[0]);
		message = wxString::Format("JSON file is incomplete: " + wxFileNameFromPath(fn) + "\n\n"
			"Variables: %d loaded form JSON file but not in SAM configuration, %d wrong type, JSON file has %d, SAM config has %d\n\n",
			(int)di.not_found.size(), (int)di.wrong_type.size(), (int)di.nread, (int)m_vals.size());

		if (di.wrong_type.size() > 0)
		{
			message += "\nWrong data type: " + wxJoin(di.wrong_type, ',');
			ok = false;
		}

		if (di.not_found.size() > 0)
		{
			message += "\nLoaded but don't exist in config: " + wxJoin(di.not_found, ',');
			ok = false;
		}
	}
	else
	{
		message = "Defaults file does not exist";
		ok = false;
	}

	if (pmsg != 0)
	{
		*pmsg = message;
	}

	rapidjson::Document doc;
	doc.SetObject();

	wxString tech, fin;
	GetConfiguration(&tech, &fin);

	if (!ok || di.not_found.size() > 0 || di.wrong_type.size() > 0 || di.nread != m_vals.size())
	{
		wxLogStatus("discrepancy reading in values from project file: %d not found, %d wrong type, %d read != %d in config",
			(int)di.not_found.size(), (int)di.wrong_type.size(), (int)di.nread, (int)m_vals.size());

		if (di.not_found.size() > 0)
		{
			wxLogStatus("\not found: " + wxJoin(di.not_found, ','));
		}
		if (di.wrong_type.size() > 0)
		{
			wxLogStatus("\twrong type: " + wxJoin(di.wrong_type, ','));
		}
		if (m_vals[0].size() > m_oldVals[0].size())
		{
			// create JSON file with list of missing UI inputs (variable name and group)
			rapidjson::Value json_val;
			wxString x, y;
			for (auto& newVal : m_vals[0]) { // only want SAM inputs - not calculated and indicators (m_vals contain all SAM UI inputs, indicators and calculated values)
				VarInfo* vi = Variables(0).Lookup(newVal.first);
				bool is_input = ((vi != NULL) && !(vi->Flags & VF_INDICATOR) && !(vi->Flags & VF_CALCULATED));
				if (!m_oldVals[0].Get(newVal.first) && is_input) {
					// example using VarInfo for sscVariableName and sscVariableValue for ssc inputs in JSON e.g. ssc variable rec_htf and SAM UI variable csp.pt.rec.htf_type
					wxString sscVariableName = vi->sscVariableName.Trim();
					if (sscVariableName.Len() > 0) {
						if (VarValue* vv = m_oldVals[0].Get(sscVariableName)) {
							int ndx = vi->sscVariableValue.Index(vv->AsString());
							newVal.second->Set(ndx);
						}
					}
					/*
					// here we can process ssc to UI conversion lk script or do the conversion manually
					// manual example for converting rec_htf (SSC input) to csp.pt.rec.htf_type (SAM UI)
					if (newVal.first == "csp.pt.rec.htf_type") {
						if (VarValue* jsonVal = m_oldVals.Get("rec_htf")) {
							if (jsonVal->Value() == 17)
								newVal.second->Set(0);
							else if (jsonVal->Value() == 10)
								newVal.second->Set(1);
							else
								newVal.second->Set(2);
						}
					}
					// continue all mappings here or with a separate script like version upgrade
					//
					else { // track all missing SAM UI inputs

						wxLogStatus("%s, %s configuration input variable %s missing from JSON file", tech.c_str(), fin.c_str(), newVal.first.c_str());
						x = newVal.first;
						y = vi->Group;
						json_val.SetString(y.c_str(), doc.GetAllocator());
						doc.AddMember(rapidjson::Value(x.c_str(), x.size(), doc.GetAllocator()).Move(), json_val.Move(), doc.GetAllocator());
					}

				}
			}
		}
		if (m_vals[0].size() < m_oldVals[0].size())
		{
			for (auto& oldVal : m_oldVals[0]) {
				if (!m_vals[0].Get(oldVal.first)) {
					wxLogStatus("%s, %s JSON file variable %s missing from configuration", tech.c_str(), fin.c_str(), oldVal.first.c_str());
				}
			}
		}

	}

	rapidjson::StringBuffer os;
	rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(os); // MSPT/MP 64MB JSON, 6.7MB txt, JSON Zip 242kB 
	doc.Accept(writer);
	wxString path, name, ext;
	wxFileName::SplitPath(fn, &path, &name, &ext);
	wxString sfn = path + "/" + name + "_missing_SAM_UI_Inputs.json";
	wxFFileOutputStream out(sfn);
	out.Write(os.GetString(), os.GetSize());
	out.Close();

	return ok;
}



bool Case::LoadFromJSON( wxString fn, wxString* pmsg)
{
	if (!m_config) return false;
	bool binary = true;
	LoadStatus di;
	wxString message;
	bool ok = false;
	VarTable vt;
	wxString schk = fn;
	if (wxFileExists(schk))
	{
		ok = VarTableFromJSONFile(&vt, fn.ToStdString());

		m_oldVals.clear();
		ok &= LoadValuesFromExternalSource(vt, &di, &m_oldVals[0]);
		message = wxString::Format("JSON file is incomplete: " + wxFileNameFromPath(fn) + "\n\n"
			"Variables: %d loaded form JSON file but not in SAM configuration, %d wrong type, JSON file has %d, SAM config has %d\n\n", 
			(int)di.not_found.size(), (int)di.wrong_type.size(), (int)di.nread, (int)m_vals.size());

		if (di.wrong_type.size() > 0)
		{
			message += "\nWrong data type: " + wxJoin(di.wrong_type, ',');
			ok = false;
		}

		if (di.not_found.size() > 0)
		{
			message += "\nLoaded but don't exist in config: " + wxJoin(di.not_found, ',');
			ok = false;
		}
	}
	else
	{
		message = "Defaults file does not exist";
		ok = false;
	}

	if (pmsg != 0)
	{
		*pmsg = message;
	}


	wxString tech, fin;
	GetConfiguration(&tech, &fin);

	if (!ok || di.not_found.size() > 0 || di.wrong_type.size() > 0 || di.nread != m_vals[0].size())
	{
		wxLogStatus("discrepancy reading in values from project file: %d not found, %d wrong type, %d read != %d in config",
			(int)di.not_found.size(), (int)di.wrong_type.size(), (int)di.nread, (int)m_vals[0].size());

		if (di.not_found.size() > 0)
		{
			wxLogStatus("\not found: " + wxJoin(di.not_found, ','));
		}
		if (di.wrong_type.size() > 0)
		{
			wxLogStatus("\twrong type: " + wxJoin(di.wrong_type, ','));
		}
		if (m_vals[0].size() > m_oldVals[0].size())
		{
			for (auto &newVal : m_vals[0]) { // only want SAM inputs - not calculated and indicators
				VarInfo* vi = Variables(0).Lookup(newVal.first);
				bool is_input = ((vi != NULL) && !(vi->Flags & VF_INDICATOR) && !(vi->Flags & VF_CALCULATED));
				if (!m_oldVals[0].Get(newVal.first) && is_input)
					wxLogStatus("%s, %s configuration variable %s missing from JSON file", tech.c_str(), fin.c_str(), newVal.first.c_str());
			}
		}
		if (m_vals[0].size() < m_oldVals[0].size())
		{
			for (auto &oldVal : m_oldVals[0]) {
				if (!m_vals[0].Get(oldVal.first)) {
					wxLogStatus("%s, %s JSON file variable %s missing from configuration", tech.c_str(), fin.c_str(), oldVal.first.c_str());
				}
			}
		}

	}

	return ok;
}
*/


bool Case::LoadDefaults(wxString* pmsg)
{
	if (!m_config) return false;
	bool binary = true;
	wxString file = SamApp::GetRuntimePath() + "/defaults/"
		+ m_config->TechnologyFullName + "_" + m_config->Financing + ".json";
	LoadStatus di;
	wxString message;
	bool ok = false;
	std::vector<VarTable> vt; 
	vt.resize(m_config->Technology.size());
	wxString schk = file;
	if (wxFileExists(schk))
	{
		ok = VarTablesFromJSONFile(vt, file.ToStdString());

		for (size_t ndxHybrid = 0; ndxHybrid < vt.size(); ndxHybrid++) {

			ok &= LoadValuesFromExternalSource(vt[ndxHybrid], ndxHybrid, &di, (VarTable*)0);
			message += wxString::Format("Defaults file is likely out of date: " + wxFileNameFromPath(file) + "\n\n"
				"Variables: %d loaded but not in configuration, %d wrong type, defaults file has %d, config has %d\n\n"
				"Would you like to update the defaults with the current values right now?\n"
				"(Otherwise press Shift-F10 later)\n", (int)di.not_found.size(),
				(int)di.wrong_type.size(), (int)di.nread, (int)m_vals[ndxHybrid].size());

			if (di.wrong_type.size() > 0)
			{
				message += "\nWrong data type: " + wxJoin(di.wrong_type, ',');
				ok = false;
			}

			if (di.not_found.size() > 0)
			{
				message += "\nLoaded but don't exist in config: " + wxJoin(di.not_found, ',');
				ok = false;
			}
		}
	}
	else {
		message = "Defaults file does not exist";
		ok = false;
	}

	if ( pmsg != 0 )
		*pmsg += message;
	if ( !ok )	{
		if ( wxYES == wxShowTextMessageDialog( message, "Query", SamApp::Window(), wxDefaultSize, wxYES_NO) )	{
			ok = SaveDefaults();
		}
	}

	return ok;
}



bool Case::SetConfiguration(const wxString& tech, const wxString& fin, bool silent, wxString* message)
{
	wxArrayString notices;

	// erase results
	m_baseCase.Clear();

	m_config = SamApp::Config().Find(tech, fin);

	if (!m_config) {
		notices.Add("Case error: could not find configuration " + tech + ", " + fin);
		return false;
	}


	for (size_t i = 0; i < m_cbEnv.size(); i++) {
		m_cbEnv[i].clear_objs();
		m_cbEnv[i].clear_vars();
	}

	m_vals.resize(m_config->Technology.size()); // TODO: verify switching from hybrid to non-hybrid
	m_cbEnv.resize(m_config->Technology.size()); // TODO: verify switching from hybrid to non-hybrid


	// Load defaults before iterating

	// load the default values for the current
	// configuration from the external data file

#ifdef UI_BINARY
	wxString file = SamApp::GetRuntimePath() + "/defaults/"
		+ m_config->Technology + "_" + m_config->Financing;
#elif defined(__LOAD_AS_JSON__)
	wxString file = SamApp::GetRuntimePath() + "/defaults/"
		+ m_config->TechnologyFullName + "_" + m_config->Financing + ".json";
#else
	wxString file = SamApp::GetRuntimePath() + "/defaults/"
		+ m_config->Technology + "_" + m_config->Financing + ".txt";
#endif

	std::vector<VarTable> vt_defaults;
	vt_defaults.resize(m_config->Technology.size()); // TODO: verify switching from hybrid to non-hybrid

#if defined(__LOAD_AS_JSON__)
	wxString schk = file;
	//schk.Replace(".json", ".zip");
	if (wxFileExists(schk))
	{
		VarTablesFromJSONFile(vt_defaults, file.ToStdString());
#else 
	if (wxFileExists(file))
	{
		wxFFileInputStream in(file);
		if (in.IsOk())
#ifdef UI_BINARY
			vt_defaults.Read(in);
#else
			vt_defaults.Read_text(in);
#endif
#endif
	}

	for (size_t i_var = 0; i_var < m_config->Technology.size(); i_var++) {
		// erase all input variables that are no longer in the current configuration
		wxArrayString to_remove;
		// TODO: iterate over all technologies and remaining variables to set update m_values - read in defaults first to vector of VarTable similarly to SaveDefaults
		VarInfoLookup& vars = m_config->Variables[i_var];

		for (VarTable::iterator it = m_vals[i_var].begin(); it != m_vals[i_var].end(); ++it)
			if (vars.find(it->first) == vars.end())
				to_remove.Add(it->first);

		m_vals[i_var].Delete(to_remove);


		if (vt_defaults[i_var].size() == 0)
			notices.Add("No external default values located for case when setting configuration: " + tech + "/" + fin);

		// set up any remaining new variables with default values
		for (VarInfoLookup::iterator it = vars.begin(); it != vars.end(); ++it)
		{
			// issue a notice if there's a variable table discrepancy in data types for the default value
			if (it->second->Type != it->second->DefaultValue.Type())
				notices.Add("internal variable table type mismatch for " + it->first);

			// find the default value for this variable.  first priority is externally saved default,
			// then as a fallback use the internal default value (UI form default)



			VarValue* val_default = vt_defaults[i_var].Get(it->first);
			if (val_default == 0)
			{
				notices.Add("No default value found for '" + it->first + "' in external file (" + tech + "/" + fin + "), using internal default");
				val_default = &(it->second->DefaultValue);
			}
			else if (val_default->Type() != it->second->DefaultValue.Type()
				|| val_default->Type() != it->second->Type)
			{
				notices.Add("externally loaded default value differs in type from internally specified type for: " + it->first);
				notices.Add("  --> resolving by changing " + it->first + wxString::Format(" to type %d", it->second->Type));
				val_default->SetType(it->second->Type);
			}

			VarValue* vv = m_vals[i_var].Get(it->first);
			if (0 == vv)
			{
				// if the variable doesn't exist in the current configuration
				m_vals[i_var].Set(it->first, *val_default); // will create new variable if it doesn't exist
			}
			else if (vv->Type() != it->second->Type)
			{
				// if the variable exists but is of a different data type
				vv->SetType(it->second->Type);
				vv->Copy(*val_default);
			}
			//else if (it->second->Flags & VF_CALCULATED && it->second->Flags & VF_INDICATOR) 
			else if (it->second->Flags & VF_CHANGE_MODEL)
			{ // assumption that any configuration dependent values that should be overwritten are both calculated and indicators - e.g. "en_batt" - SAM Github issue 395
				vv->Copy(*val_default);
			}

			// Set any ssc variables that are listed as a VarInfo from a SAM UI variable (e.g. ssc var rec_htf and SAM UI csp.pt.rec.htf_type)
			if (it->second->sscVariableName.Trim().Len() > 0) {
				// initially for numbers only and combo box translations
				// get existing SAM UI variable and value
				if (VarValue* UIVal = m_vals[i_var].Get(it->first)) { // should have been set by this point
					VarValue* sscVal = m_vals[i_var].Set(it->second->sscVariableName, VarValue(wxAtof(it->second->sscVariableValue[UIVal->Integer()])));
					// can check validity of sscVal
				}
			}
		}


		// reevaluate all equations
		CaseEvaluator eval(this, m_vals[i_var], m_config->Equations[i_var]);
		int n = eval.CalculateAll(i_var);
		if (n < 0)
		{
			for (size_t i = 0; i < eval.GetErrors().size(); i++)
				notices.Add(eval.GetErrors()[i]);
		}

		// setup the local callback environment
		// by merging all the functions defined
		// in the various input page callback scripts
		// into one runtime environment
		// the parse trees of the actual function implementations
		// are not copied - they just reference those stored in the
		// scriptdatabase(s) that are members of inputpagedata
//		m_cbEnv.clear_objs();
//		m_cbEnv.clear_vars();
		m_cbEnv[i_var].clear_objs();
		m_cbEnv[i_var].clear_vars();


		lk::vardata_t* vdt_on_load = new lk::vardata_t;
		vdt_on_load->empty_hash();
		m_cbEnv[i_var].assign("on_load", vdt_on_load);

		lk::vardata_t* vdt_on_change = new lk::vardata_t;
		vdt_on_change->empty_hash();
		m_cbEnv[i_var].assign("on_change", vdt_on_change);

		for (InputPageDataHash::iterator it = m_config->InputPages[i_var].begin();
			it != m_config->InputPages[i_var].end();
			++it)
		{
			lk::env_t* env = it->second->Callbacks().GetEnv();
			lk_string key;
			lk::vardata_t* val = NULL;
			bool has_more = env->first(key, val);
			while (has_more)
			{
				if (val->type() == lk::vardata_t::FUNCTION)
					m_cbEnv[i_var].assign(key, new lk::vardata_t(*val));
				else if (val->type() == lk::vardata_t::HASH
					&& (key == "on_load" || key == "on_change"))
				{
					lk::vardata_t* target = (key == "on_load") ? vdt_on_load : vdt_on_change;
					lk::varhash_t* hh = val->hash();
					for (lk::varhash_t::iterator ihh = hh->begin();
						ihh != hh->end();
						++ihh)
						target->hash_item(ihh->first, *ihh->second);
				}

				has_more = env->next(key, val);
			}
		}

	} // end iterating over vartables

	 // TODO: update UI - check for hybrids
	SendEvent(CaseEvent(CaseEvent::CONFIG_CHANGED, tech, fin));


	wxString mm(  wxJoin( notices, wxChar('\n') ) );
	if ( !silent && notices.size() > 0 )
		::wxShowTextMessageDialog( mm );
	
	if ( message ) *message = mm;

	return notices.size() == 0;
}

lk::env_t &Case::CallbackEnvironment(size_t i_vt)
{
	return m_cbEnv[i_vt];
}

lk::node_t *Case::QueryCallback( const wxString &method, const wxString &object, size_t i_vt )
{
	
	lk::vardata_t *cbvar = m_cbEnv[i_vt].lookup(method, true);

	if (!cbvar || cbvar->type() != lk::vardata_t::HASH )
	{
		//wxLogStatus("ScriptDatabase::Invoke: could not find " + method_name + " variable or not a hash");
		return 0;
	}

	lk::vardata_t *cbref = cbvar->lookup( object );
	if ( cbref == 0 
		|| cbref->type() != lk::vardata_t::FUNCTION
		|| cbref->deref().func() == 0 )
	{
		// wxLogStatus("ScriptDatabase::Invoke: could not find function entry for '%s'", (const char*)obj_name.c_str() );
		return 0;
	}
	
	lk::expr_t *p_define = cbref->deref().func();
	if ( p_define->oper != lk::expr_t::DEFINE )
	{
		wxLogStatus("Case::QueryCallback: improper function structure, must be a 'define' for %s, instead: %s", (const char*)object.c_str(), cbref->func()->operstr() );
		return 0;
	}
	
	if ( p_define->right == 0 )
	{
		wxLogStatus("Case::QueryCallback: function block nonexistent for '%s'\n", (const char*)object.c_str());
		return 0;
	}

	return p_define->right;
}

void Case::GetConfiguration( wxString *tech, wxString *fin )
{
	if ( m_config )
	{
		if ( tech ) *tech = m_config->TechnologyFullName;
		if ( fin ) *fin = m_config->Financing;
	}
}

VarInfoLookup &Case::Variables(size_t i)
{
	static VarInfoLookup sg_emptyVars;
	return (m_config && (i<m_config->Variables.size())) ? m_config->Variables[i] : sg_emptyVars;
}

EqnFastLookup &Case::Equations(size_t i)
{
	static EqnFastLookup sg_emptyEqns;
	return (m_config && (i<m_config->Equations.size())) ? m_config->Equations[i] : sg_emptyEqns;
}

wxString Case::GetTechnology() const
{
	return m_config ? m_config->TechnologyFullName : wxString(wxEmptyString);
}

wxString Case::GetFinancing() const
{
	return m_config ? m_config->Financing : wxString(wxEmptyString);
}

void Case::VariableChanged( const wxString &var, size_t ndxHybrid)
{
	// Send the additional case event that this variable
	// was programmatically changed and needs to be updated
	CaseEvent ce( CaseEvent::VARS_CHANGED, ndxHybrid );
	ce.GetVars().Add( var );
	SendEvent( ce );

	// issue the request for any calculations to be updated as needed
	Recalculate( var, ndxHybrid);
}

void Case::VariablesChanged( const wxArrayString &list, size_t ndxHybrid)
{
	// Send the additional case event that this variable
	// was programmatically changed and needs to be updated
	CaseEvent ce( CaseEvent::VARS_CHANGED, ndxHybrid );
	ce.GetVars() = list;
	SendEvent( ce );

	// recalculate all the equations
	Recalculate( list, ndxHybrid);
}

int Case::Recalculate( const wxString &trigger, size_t ndxHybrid)
{
	if ( !m_config )
	{
		wxLogStatus( "cannot recalculate: no active configuration" );
		return -1;
	}

	CaseEvaluator eval( this, m_vals[ndxHybrid], m_config->Equations[ndxHybrid]);
	int n = eval.Changed( trigger, ndxHybrid);
	if (n > 0) {
		SendEvent(CaseEvent(CaseEvent::VARS_CHANGED, eval.GetUpdated()));
		// hybrid updating across VarTables using HybridVariableDependencies
// at this point vv is updated and corresponding object is updated
// check through dependencies for obj->GetNatme()
		// eval.GetUpdated() has all affected variable changes (calculated)
		auto& list = eval.GetUpdated();
		for (size_t i = 0; i < list.size(); i++) {
			for (auto& hvd : GetConfiguration()->HybridVariables) {
				if (ndxHybrid == hvd.IndependentVariableVarTable && list[i] == hvd.IndependentVariableName) {
					// update dependent variable and equations 
					if (VarValue* depVar = Values(hvd.DependentVariableVarTable).Get(hvd.DependentVariableName)) {
						if (VarValue* vv = Values(ndxHybrid).Get(list[i])) {
							depVar->Copy(*vv); // update dependent variable value
							Recalculate(hvd.DependentVariableName, hvd.DependentVariableVarTable); //recalculate equations
						}
					}
				}
			}
		}
	}
	else if (n < 0) {
		wxShowTextMessageDialog(wxJoin(eval.GetErrors(), wxChar('\n')));
	}
	return n;

}

int Case::Recalculate( const wxArrayString &triggers, size_t ndxHybrid)
{
	if ( !m_config )
	{
		wxLogStatus( "cannot recalculate: no active configuration" );
		return -1;
	}

	CaseEvaluator eval( this, m_vals[ndxHybrid], m_config->Equations[ndxHybrid]);
	int n = eval.Changed( triggers, ndxHybrid);
	if (n > 0) {
		SendEvent(CaseEvent(CaseEvent::VARS_CHANGED, eval.GetUpdated()));
		// hybrid updating across VarTables using HybridVariableDependencies
// at this point vv is updated and corresponding object is updated
// check through dependencies for obj->GetNatme()
		// eval.GetUpdated() has all affected variable changes (calculated)
		auto& list = eval.GetUpdated();
		for (size_t i = 0; i < list.size(); i++) {
			for (auto& hvd : GetConfiguration()->HybridVariables) {
				if (ndxHybrid == hvd.IndependentVariableVarTable && list[i] == hvd.IndependentVariableName) {
					// update dependent variable and equations 
					if (VarValue* depVar = Values(hvd.DependentVariableVarTable).Get(hvd.DependentVariableName)) {
						if (VarValue* vv = Values(ndxHybrid).Get(list[i])) {
							depVar->Copy(*vv); // update dependent variable value
							Recalculate(hvd.DependentVariableName, hvd.DependentVariableVarTable); //recalculate equations
						}
					}
				}
			}
		}
	}
	else if (n < 0) {
		wxShowTextMessageDialog(wxJoin(eval.GetErrors(), wxChar('\n')));
	}
	return n;

}

int Case::RecalculateAll(size_t ndxHybrid, bool quietly )
{
	if ( !m_config )
	{
		wxLogStatus( "cannot recalculate all, no valid configuration information" );
		return -1;
	}

	CaseEvaluator eval( this, m_vals[ndxHybrid], m_config->Equations[ndxHybrid]);
	int n = eval.CalculateAll(ndxHybrid);
	if ( n > 0 ) SendEvent( CaseEvent( CaseEvent::VARS_CHANGED, eval.GetUpdated() ) );
	else if ( n < 0 && !quietly ) wxShowTextMessageDialog( wxJoin( eval.GetErrors(), wxChar('\n') )  );

	return n;
}

void Case::AddListener( CaseEventListener *cel )
{
	if (cel) {
		m_listeners.push_back(cel);
	}
}

void Case::RemoveListener( CaseEventListener *cel )
{
	if (m_listeners.size() > 0)
	{
		for (size_t i = 0; i < m_listeners.size(); i++)
		{
			if (cel) {
				if (m_listeners[i] == cel)
				{
					m_listeners.erase(m_listeners.begin() + i);
					break;
				}
			}
		}
	}
}

void Case::ClearListeners()
{
	m_listeners.clear();
}

void Case::SendEvent( CaseEvent e )
{
	for( size_t i=0;i<m_listeners.size();i++ )
		m_listeners[i]->OnCaseEvent( this, e );
}

wxString Case::GetProperty( const wxString &id )
{
	StringHash::iterator it = m_properties.find( id );
	if ( it != m_properties.end() )
		return it->second;
	else
		return wxEmptyString;
}
void Case::SetProperty( const wxString &id, const wxString &value )
{
	m_properties[id] = value;
}

wxString Case::RetrieveNote( const wxString &id )
{
	StringHash::iterator it = m_notes.find( id );
	if ( it != m_notes.end() )
		return it->second;
	else return wxEmptyString;
}

void Case::SaveNote( const wxString &id, const wxString &text )
{
	m_notes[id] = text;
}


Simulation &Case::BaseCase()
{
	return m_baseCase;
}

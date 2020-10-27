/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
VarTable &CaseCallbackContext::GetValues() { return GetCase().Values(); }
Case &CaseCallbackContext::GetCase() { return *m_case; }

void CaseCallbackContext::SetupLibraries( lk::env_t * )
{
	/* nothing here - for descendents like UICallbackContext or ResultsCallbackContext */
}

class CaseScriptInterpreter : public VarTableScriptInterpreter
{
	Case *m_case;
public:	
	CaseScriptInterpreter( lk::node_t *tree, lk::env_t *env, VarTable *vt, Case *cc )
		: VarTableScriptInterpreter( tree, env, vt ), m_case( cc )
	{
	}
	virtual ~CaseScriptInterpreter( ) { /* nothing to do */ };
	virtual bool special_set( const lk_string &name, lk::vardata_t &val )
	{
		if ( VarTableScriptInterpreter::special_set( name, val ) )
		{
			m_case->VariableChanged( name );
			return true;
		}
		else
			return false;
	}
};
	
bool CaseCallbackContext::Invoke( lk::node_t *root, lk::env_t *parent_env )
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

		CaseScriptInterpreter e( root, &local_env, &GetValues(), m_case );
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
	env.register_func( fcall_financing_pCase, m_case );
	env.register_funcs( invoke_ssc_funcs() );
	env.register_funcs( invoke_equation_funcs() );
}
	
int CaseEvaluator::CalculateAll()
{
	int nlibchanges = 0;

	/* Check for project file upgrade
	If flie version < SAM version then skip recalculate all in Case LoadValuesFroExternal Source*/
	size_t sam_ver = SamApp::Version();
	size_t file_ver = SamApp::Project().GetVersionInfo();
	bool update_lib = (sam_ver == file_ver);

	if (update_lib)
	{
		for (VarInfoLookup::iterator it = m_case->Variables().begin();
			it != m_case->Variables().end();
			++it)
		{
			if (it->second->Flags & VF_LIBRARY
				&& it->second->Type == VV_STRING)
			{
				wxArrayString changed;
				if (!UpdateLibrary(it->first, changed))
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

int CaseEvaluator::Changed( const wxArrayString &vars )
{
	int nlibchanges=0;
	wxArrayString trigger_list;
	for( size_t i=0;i<vars.size();i++ )
	{
		trigger_list.Add( vars[i] );

		wxArrayString changed;
		bool ok = UpdateLibrary( vars[i], changed );
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

int CaseEvaluator::Changed( const wxString &trigger )
{
	wxArrayString list;
	list.Add(trigger);
	return Changed( list );
}

bool CaseEvaluator::UpdateLibrary( const wxString &trigger, wxArrayString &changed )
{
	size_t nerrors = 0;
	VarInfo *vi = m_case->Variables().Lookup( trigger );
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
		if ( rhs->m_config )
			SetConfiguration( rhs->m_config->Technology, rhs->m_config->Financing );

		m_vals.Copy( rhs->m_vals );
		m_baseCase.Copy( rhs->m_baseCase );
		m_properties = rhs->m_properties;
		m_notes = rhs->m_notes;
		m_parametric.Copy(rhs->m_parametric);
		m_excelExch.Copy(rhs->m_excelExch);
		m_stochastic.Copy(rhs->m_stochastic);
		
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
	out.Write8( 6 );

	wxString tech, fin;
	if ( m_config != 0 )
	{
		tech = m_config->Technology;
		fin = m_config->Financing;
	}

	// write data
	out.WriteString( tech );
	out.WriteString( fin );
	m_vals.Write( _o );
	m_baseCase.Write( _o );
	m_properties.Write( _o );
	m_notes.Write( _o );
	m_excelExch.Write( _o );

	out.Write32( m_graphs.size() );
	for( size_t i=0;i<m_graphs.size();i++ )
		m_graphs[i].Write( _o );

	m_perspective.Write( _o );

	m_parametric.Write( _o );
	m_stochastic.Write( _o );

	out.Write8( 0x9b );
}



bool Case::Read( wxInputStream &_i )
{
	wxDataInputStream in(_i);

	wxUint8 code = in.Read8();
	wxUint8 ver = in.Read8(); // version

	// read data
	wxString tech = in.ReadString();
	wxString fin = in.ReadString();

	if ( !SetConfiguration( tech, fin ) )
	  {
		wxLogStatus( "Notice: errors occurred while setting configuration during project file read.  Continuing...\n\n" + tech + "/" + fin );
	  }

	// read in the variable table
	m_oldVals.clear();
	LoadStatus di;
	bool ok = LoadValuesFromExternalSource( _i, &di, &m_oldVals );

	if ( !ok || di.not_found.size() > 0 || di.wrong_type.size() > 0 || di.nread != m_vals.size() )
	{
		wxLogStatus("discrepancy reading in values from project file: %d not found, %d wrong type, %d read != %d in config",
			(int)di.not_found.size(), (int)di.wrong_type.size(), (int)di.nread, (int)m_vals.size() );
		
		if ( di.not_found.size() > 0 )
		  {
		    wxLogStatus("\not found: " + wxJoin(di.not_found, ',') );
		  }
		if ( di.wrong_type.size() > 0 )
		  {
		    wxLogStatus("\twrong type: " + wxJoin(di.wrong_type, ',') );
		  }
		if (m_vals.size() > m_oldVals.size())
		{
			for (auto newVal : m_vals) {
				if (!m_oldVals.Get(newVal.first))
					wxLogStatus("%s, %s configuration variable %s missing from project file", tech.c_str(), fin.c_str(), newVal.first.c_str());
			}
		}
		if (m_vals.size() < m_oldVals.size())
		{
			for (auto oldVal : m_oldVals) {
				if (!m_vals.Get(oldVal.first))
					wxLogStatus("%s, %s project file variable %s missing from configuration", tech.c_str(), fin.c_str(), oldVal.first.c_str());
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
		  }
	}
	else
		if ( !m_baseCase.Read( _i ) )
		  {
		    wxLogStatus("error reading m_baseCase in Case::Read");
		  }

	if ( !m_properties.Read( _i ) )
	  {
	    wxLogStatus("error reading m_properties in Case::Read");
	  }
	if ( !m_notes.Read( _i ) )
	  {
	    wxLogStatus("error reading m_notes in Case::Read");
	  }

	if ( ver >= 3 )
	{
		if (!m_excelExch.Read( _i ))
		  {
			wxLogStatus("error reading excel exchange data in Case::Read");
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
			  }
			m_graphs.push_back( g );
		}

		if ( !m_perspective.Read( _i ) )
		  {
		    wxLogStatus("error reading perspective of results viewer in Case::Read");
		  }
	}

	if ( ver >= 5 )
	{
		if ( !m_parametric.Read( _i ) )
		  {
		    wxLogStatus("error reading parametric simulation information in Case::Read");
		  }
	}

	if ( ver >= 6 )
	{
		if ( !m_stochastic.Read( _i ) )
		  {
		    wxLogStatus("error reading stochastic simulation information in Case::Read");
		  }
	}
	wxUint8 retCode = in.Read8();
//	return (in.Read8() == code);
	return (retCode == code || retCode == '\0');
}


bool Case::SaveDefaults(bool quiet)
{
	if (!m_config) return false;
#ifdef UI_BINARY
	wxString file = SamApp::GetRuntimePath() + "/defaults/"
		+ m_config->Technology + "_" + m_config->Financing;
#else
	wxString file = SamApp::GetRuntimePath() + "/defaults/"
		+ m_config->Technology + "_" + m_config->Financing + ".txt";
#endif
	if (!quiet && wxNO == wxMessageBox("Save defaults for configuration:\n\n"
		+ m_config->Technology + " / " + m_config->Financing,
		"Save Defaults", wxYES_NO))
		return false;

	wxFFileOutputStream out(file);
	if (!out.IsOk()) return false;


	// set default library_folder_list blank
	VarValue *vv = m_vals.Get("library_folder_list");
	if (vv)	vv->Set(wxString("x"));

#ifdef UI_BINARY
	m_vals.Write(out);
#else
	m_vals.Write_text(out);
#endif
	wxLogStatus("Case: defaults saved for " + file);
	return true;
}

bool Case::LoadValuesFromExternalSource( wxInputStream &in, 
		LoadStatus *di, VarTable *oldvals, bool binary)
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

	bool ok = (vt.size() == m_vals.size());
	// copy over values for variables that already exist
	// in the configuration
	for( VarTable::iterator it = vt.begin();
		it != vt.end();
		++it )
	{
		if( VarValue *vv = m_vals.Get( it->first ) )
		{
			if (vv->Type() == it->second->Type()) {
				vv->Copy(*(it->second));
				if (oldvals) oldvals->Set(it->first, *(it->second));
			}
			else
			{
				if ( di ) di->wrong_type.Add( it->first + wxString::Format(": expected:%d got:%d", vv->Type(), it->second->Type()) );
				if ( oldvals ) oldvals->Set( it->first, *(it->second) );
				ok = false;
			}
		}
		else
		{
			if ( di ) di->not_found.Add( it->first );
			if ( oldvals ) oldvals->Set( it->first, *(it->second) );				
			ok = false;
		}
	}
	

	if (RecalculateAll() < 0 )
	{
		wxString e("Error recalculating equations after loading values from external source");	
		if ( di ) di->error = e;
		wxLogStatus( e );
		return false;
	}

	return ok;
}

bool Case::LoadDefaults( wxString *pmsg )
{
	if (!m_config) return false;
	bool binary = true;
#ifdef UI_BINARY
	wxString file = SamApp::GetRuntimePath() + "/defaults/" 
		+ m_config->Technology + "_" + m_config->Financing;
	binary = true;
#else
	wxString file = SamApp::GetRuntimePath() + "/defaults/"
		+ m_config->Technology + "_" + m_config->Financing + ".txt";
	binary = false;
#endif
	LoadStatus di;
	wxString message;
	bool ok = false;
	if ( wxFileExists(file) )
	{
		wxFFileInputStream in(file);
		if (!in.IsOk())
		{
			if ( pmsg ) *pmsg = "Could not open defaults file";
			return false;
		}
	
		ok = LoadValuesFromExternalSource( in, &di, (VarTable *)0, binary );
		message = wxString::Format("Defaults file is likely out of date: " + wxFileNameFromPath(file) + "\n\n"
				"Variables: %d loaded but not in configuration, %d wrong type, defaults file has %d, config has %d\n\n"
				"Would you like to update the defaults with the current values right now?\n"
				"(Otherwise press Shift-F10 later)\n", (int)di.not_found.size(),
				(int)di.wrong_type.size(), (int)di.nread, (int)m_vals.size());
		
		if ( di.wrong_type.size() > 0 )
		{
			message += "\nWrong data type: " + wxJoin( di.wrong_type, ',' );
			ok = false;
		}

		if ( di.not_found.size() > 0 )
		{
			message += "\nLoaded but don't exist in config: " + wxJoin( di.not_found, ',' );
			ok = false;
		}
	}
	else
	{
		message = "Defaults file does not exist";
		ok = false;
	}

	if ( pmsg != 0 )
	{
		*pmsg = message;
		return ok;
	}
	else if ( !ok || di.not_found.size() > 0 || di.wrong_type.size() > 0 || di.nread != m_vals.size() ) 
	{
		if ( wxYES == wxShowTextMessageDialog( message, "Query", SamApp::Window(), wxDefaultSize, wxYES_NO) )
		{
			wxFFileOutputStream out( file );
			if( out.IsOk() )
			{
#ifdef UI_BINARY
				m_vals.Write( out );
#else
				m_vals.Write_text(out);
#endif
				wxMessageBox("Saved defaults for configuration.");
			}
			else
				wxMessageBox("Error writing to defaults file: " + file );
		}
	}

	return ok;
}



bool Case::SetConfiguration( const wxString &tech, const wxString &fin, bool silent, wxString *message )
{
	wxArrayString notices;

	// erase results
	m_baseCase.Clear();

	m_config = SamApp::Config().Find( tech, fin );
			
	if ( !m_config )
		notices.Add("Case error: could not find configuration " + tech + ", " + fin );

	// erase all input variables that are no longer in the current configuration
	wxArrayString to_remove;
	VarInfoLookup &vars = m_config->Variables;

	for( VarTable::iterator it = m_vals.begin(); it != m_vals.end(); ++it )
		if ( vars.find( it->first ) == vars.end() )
			to_remove.Add( it->first );

	m_vals.Delete( to_remove );

	// load the default values for the current
	// configuration from the external data file
#ifdef UI_BINARY
	wxString file = SamApp::GetRuntimePath() + "/defaults/" 
		+ m_config->Technology + "_" + m_config->Financing;
#else
	wxString file = SamApp::GetRuntimePath() + "/defaults/"
		+ m_config->Technology + "_" + m_config->Financing + ".txt";
#endif

	VarTable vt_defaults;
	if ( wxFileExists(file))
	{
		wxFFileInputStream in(file);
		if ( in.IsOk() )
#ifdef UI_BINARY
			vt_defaults.Read( in );
#else
			vt_defaults.Read_text(in);
#endif
	}

	if ( vt_defaults.size() == 0 )
		notices.Add( "No external default values located for case when setting configuration: " + tech + "/" + fin );
	
	// set up any remaining new variables with default values
	for( VarInfoLookup::iterator it = vars.begin(); it != vars.end(); ++it )
	{
		// issue a notice if there's a variable table discrepancy in data types for the default value
		if ( it->second->Type != it->second->DefaultValue.Type() )
			notices.Add("internal variable table type mismatch for " + it->first );

		// find the default value for this variable.  first priority is externally saved default,
		// then as a fallback use the internal default value
		VarValue *val_default = vt_defaults.Get( it->first );
		if ( val_default == 0 )
		{
			notices.Add( "No default value found for '" + it->first + "' in external file (" + tech + "/" + fin + "), using internal default" );
			val_default = &( it->second->DefaultValue );
		}
		else if ( val_default->Type() != it->second->DefaultValue.Type()
			|| val_default->Type() != it->second->Type )
		{	
			notices.Add("externally loaded default value differs in type from interally specified type for: " + it->first );
			notices.Add("  --> resolving by changing " + it->first + wxString::Format(" to type %d", it->second->Type ) );
			val_default->SetType( it->second->Type );
		}

		VarValue *vv = m_vals.Get( it->first );
		if ( 0 == vv )
		{
			// if the variable doesn't exist in the current configuration
			m_vals.Set( it->first, *val_default ); // will create new variable if it doesnt exist
		}
		else if ( vv->Type() != it->second->Type )
		{
			// if the variable exists but is of a different data type
			vv->SetType( it->second->Type );
			vv->Copy( *val_default );
		}
	}
			
	// reevalute all equations
	CaseEvaluator eval( this, m_vals, m_config->Equations );
	int n = eval.CalculateAll();
	if ( n < 0 )
	{
		for( size_t i=0;i<eval.GetErrors().size();i++ )
			notices.Add( eval.GetErrors()[i] );
	}

	// setup the local callback environment
	// by merging all the functions defined
	// in the various input page callback scripts
	// into one runtime environment
	// the parse trees of the actual function implementations
	// are not copied - they just reference those stored in the
	// scriptdatabase(s) that are members of inputpagedata
	m_cbEnv.clear_objs();
	m_cbEnv.clear_vars();

	lk::vardata_t *vdt_on_load = new lk::vardata_t;
	vdt_on_load->empty_hash();
	m_cbEnv.assign( "on_load", vdt_on_load );

	lk::vardata_t *vdt_on_change = new lk::vardata_t;
	vdt_on_change->empty_hash();
	m_cbEnv.assign( "on_change", vdt_on_change );
	
	for( InputPageDataHash::iterator it = m_config->InputPages.begin();
		it != m_config->InputPages.end();
		++it )
	{
		lk::env_t *env = it->second->Callbacks().GetEnv();
		lk_string key;
		lk::vardata_t *val;
		bool has_more = env->first( key, val );
		while( has_more )
		{
			if ( val->type() == lk::vardata_t::FUNCTION )
				m_cbEnv.assign( key, new lk::vardata_t( *val ) );
			else if ( val->type() == lk::vardata_t::HASH
				&& (key == "on_load" || key == "on_change") )
			{
				lk::vardata_t *target = (key=="on_load") ? vdt_on_load : vdt_on_change;
				lk::varhash_t *hh = val->hash();
				for( lk::varhash_t::iterator ihh = hh->begin();
					ihh != hh->end();
					++ihh )
					target->hash_item( ihh->first, *ihh->second );
			}

			has_more = env->next( key, val );
		}
	}
	
	// update UI
	SendEvent( CaseEvent( CaseEvent::CONFIG_CHANGED, tech, fin ) );

	
	wxString mm(  wxJoin( notices, wxChar('\n') ) );
	if ( !silent && notices.size() > 0 )
		::wxShowTextMessageDialog( mm );
	
	if ( message ) *message = mm;

	return notices.size() == 0;
}

lk::env_t &Case::CallbackEnvironment()
{
	return m_cbEnv;
}

lk::node_t *Case::QueryCallback( const wxString &method, const wxString &object )
{
	
	lk::vardata_t *cbvar = m_cbEnv.lookup( method, true);

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
		if ( tech ) *tech = m_config->Technology;
		if ( fin ) *fin = m_config->Financing;
	}
}

VarInfoLookup &Case::Variables()
{
static VarInfoLookup sg_emptyVars;
	return m_config ? m_config->Variables : sg_emptyVars;
}

EqnFastLookup &Case::Equations()
{
static EqnFastLookup sg_emptyEqns;
	return m_config ? m_config->Equations : sg_emptyEqns;
}

wxString Case::GetTechnology() const
{
	return m_config ? m_config->Technology : wxEmptyString;
}

wxString Case::GetFinancing() const
{
	return m_config ? m_config->Financing : wxEmptyString;
}

void Case::VariableChanged( const wxString &var )
{
	// Send the additional case event that this variable
	// was programmatically changed and needs to be updated
	CaseEvent ce( CaseEvent::VARS_CHANGED );
	ce.GetVars().Add( var );
	SendEvent( ce );

	// issue the request for any calculations to be updated as needed
	Recalculate( var );
}

void Case::VariablesChanged( const wxArrayString &list )
{
	// Send the additional case event that this variable
	// was programmatically changed and needs to be updated
	CaseEvent ce( CaseEvent::VARS_CHANGED );
	ce.GetVars() = list;
	SendEvent( ce );

	// recalculate all the equations
	Recalculate( list );
}

int Case::Recalculate( const wxString &trigger )
{
	if ( !m_config )
	{
		wxLogStatus( "cannot recalculate: no active configuration" );
		return -1;
	}

	CaseEvaluator eval( this, m_vals, m_config->Equations );
	int n = eval.Changed( trigger );	
	if ( n > 0 ) SendEvent( CaseEvent( CaseEvent::VARS_CHANGED, eval.GetUpdated() ) );
	else if ( n < 0 ) wxShowTextMessageDialog( wxJoin( eval.GetErrors(), wxChar('\n') )  );
	return n;

}

int Case::Recalculate( const wxArrayString &triggers )
{
	if ( !m_config )
	{
		wxLogStatus( "cannot recalculate: no active configuration" );
		return -1;
	}

	CaseEvaluator eval( this, m_vals, m_config->Equations );
	int n = eval.Changed( triggers );	
	if ( n > 0 ) SendEvent( CaseEvent( CaseEvent::VARS_CHANGED, eval.GetUpdated() ) );
	else if ( n < 0 ) wxShowTextMessageDialog( wxJoin( eval.GetErrors(), wxChar('\n') )  );
	return n;

}

int Case::RecalculateAll( bool quietly )
{
	if ( !m_config )
	{
		wxLogStatus( "cannot recalculate all, no valid configuration information" );
		return -1;
	}

	CaseEvaluator eval( this, m_vals, m_config->Equations );
	int n = eval.CalculateAll();
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

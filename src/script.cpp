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

#include <wx/splitter.h>
#include <wx/textctrl.h>
#include <wx/busyinfo.h>
#include <wx/progdlg.h>

#include <wex/lkscript.h>
#include <wex/metro.h>
#include <wex/utils.h>

#include "casewin.h"
#include "library.h"
#include "main.h"
#include "invoke.h"
#include "simulation.h"
#include "script.h"
#include "urdb.h"

static void fcall_open_project( lk::invoke_t &cxt )
{
	LK_DOC( "open_project", "Open a SAM project file.", "(string:file):boolean" );

	if ( !SamApp::Window()->CloseProject() ) cxt.result().assign( 0.0 );
	else cxt.result().assign( SamApp::Window()->LoadProject( cxt.arg(0).as_string() ) ? 1.0 : 0.0 );
}

static void fcall_create_case( lk::invoke_t &cxt )
{
	LK_DOC( "create_case", "Create a new case.", "(string:technology, string:financing, [string:name]):boolean" );
	wxString name;
	if ( cxt.arg_count() == 3 )
		name = cxt.arg(2).as_string();

	cxt.result().assign( SamApp::Window()->CreateNewCase( name, 
			cxt.arg(0).as_string(), 
			cxt.arg(1).as_string() ) ? 1.0 : 0.0 );
}

static void fcall_duplicate_case(lk::invoke_t &cxt)
{
	LK_DOC( "duplicate_case", "Duplicates the current SAM case.", "(none):boolean" );
	Case *current_case = SamApp::Window()->GetCurrentCase();
	if (current_case == 0)
	{
		cxt.result().assign(0.0);
		return;
	}
	wxString case_name = SamApp::Window()->Project().GetCaseName(current_case);
	if ( Case *dup = dynamic_cast<Case*>(current_case->Duplicate()) )
	{
		SamApp::Window()->Project().AddCase( SamApp::Window()->GetUniqueCaseName( case_name ), dup );
		SamApp::Window()->CreateCaseWindow( dup );
		cxt.result().assign(1.0);
		return;
	}
	cxt.result().assign(0.0);
}


static void fcall_close_project( lk::invoke_t &cxt )
{
	LK_DOC( "close_project", "Closes the current SAM project, if one is open.", "(none):boolean" );
	cxt.result().assign( SamApp::Window()->CloseProject() ? 1.0 : 0.0 );
}

static void fcall_save_project( lk::invoke_t &cxt )
{
	LK_DOC( "save_project", "Saves the current SAM project to disk.", "(string:file):boolean" );
	cxt.result().assign( SamApp::Window()->SaveProject( cxt.arg(0).as_string() ) ? 1.0 : 0.0 );
}

static void fcall_project_file( lk::invoke_t &cxt )
{
	LK_DOC( "project_file", "Returns the file name of the current project.", "(none):string" );
	cxt.result().assign( SamApp::Window()->GetProjectFileName() );
}

static void fcall_list_cases( lk::invoke_t &cxt )
{
	LK_DOC( "list_cases", "Return a list of the case names in the current project.", "(none):array" );
	cxt.result().empty_vector();
	wxArrayString names = SamApp::Window()->Project().GetCaseNames();
	for( size_t i=0;i<names.size();i++ )
		cxt.result().vec_append( names[i] );
}

static Case *CurrentCase() { return SamApp::Window()->GetCurrentCase(); }

static void fcall_active_case( lk::invoke_t &cxt )
{
	LK_DOC( "active_case", "Sets the currently active case, or returns its name.", "([string:case name]):variant" );
	if (cxt.arg_count() == 0 )
		cxt.result().assign( SamApp::Window()->Project().GetCaseName( CurrentCase() ) );
	else
		cxt.result().assign( SamApp::Window()->SwitchToCaseWindow( cxt.arg(0).as_string() ) ? 1.0 : 0.0 );	
}

static void fcall_varinfo( lk::invoke_t &cxt )
{
	LK_DOC("varinfo", "Gets meta data about an input or output variable. Returns null if the variable does not exist.", "(string:var name):table");
	if ( Case *c = CurrentCase() )
		invoke_get_var_info( c, cxt.arg(0).as_string(), cxt.result() );
	else cxt.error("no active case");

}

static void fcall_selectinputs( lk::invoke_t &cxt )
{
	LK_DOC("select_inputs", "Shows the input variable selection dialog.", "(<array:checked>, [string:title]):boolean")
		
	cxt.result().assign( 0.0 );

	Case *cc = CurrentCase();
	if ( !cc ) 
	{
		cxt.error("no active case");;
		return;
	}

	wxArrayString names;
	wxArrayString labels;

	for( VarInfoLookup::iterator it = cc->Variables().begin();
		it != cc->Variables().end();
		++it )
	{
		VarInfo &vi = *(it->second);

		if ( !vi.Label.IsEmpty()
			&& !(vi.Flags & VF_INDICATOR) 
			&& !(vi.Flags & VF_CALCULATED) )
		{
			if ( vi.Type == VV_NUMBER && vi.IndexLabels.size() > 0 )
				continue;

			wxString label;
			if ( !vi.Group.IsEmpty() )
				label = vi.Group + "/" + vi.Label;
			else
				label = vi.Label;
							
			names.Add( it->first );
			labels.Add( label );
		}
	}

	wxSortByLabels(names, labels);

	wxArrayString list;
	lk::vardata_t &inlist = cxt.arg(0).deref();
	for( size_t i=0;i<inlist.length();i++ )
		list.Add( inlist.index(i)->as_string() );

	wxString title("Select input variables");
	if (cxt.arg_count() > 1 ) title = cxt.arg(1).as_string();

	if ( SelectVariableDialog::Run(title, names, labels, list) )
	{
		inlist.empty_vector();
		for( size_t i=0;i<list.size();i++ )
			inlist.vec_append( list[i] );

		cxt.result().assign( 1.0 );
	}
}


void fcall_set( lk::invoke_t &cxt )
{
	LK_DOC( "set", "Set an input variable's value. Issues a script error if the variable doesn't exist or there is data type error.", "(string:name, variant:value):none" );
	cxt.result().assign( 0.0 );
	wxString name = cxt.arg(0).as_string();
	if ( Case *c = CurrentCase() )
	{
		if ( VarValue *vv = c->Values().Get( name ) )
		{
			if ( vv->Read( cxt.arg(1), false ) )
				c->VariableChanged( name );
			else
				cxt.error( "data type mismatch attempting to set '" + name + "' (" + vv_strtypes[vv->Type()] + ") to " + cxt.arg(1).as_string() + " ("+ wxString(cxt.arg(1).typestr()) + ")"  );
		}
		else
			cxt.error("variable '" + name + "' does not exist in the current case" );
	}
	else cxt.error("no active case");
}

void fcall_get( lk::invoke_t &cxt )
{
	LK_DOC("get", "Get an output or input variable's value.", "(string:name):variant" );
	if ( Case *c = CurrentCase() )
	{
		wxString name = cxt.arg(0).as_string();
		if ( VarValue *vv = c->BaseCase().GetOutput( name ) )
			vv->Write( cxt.result() );
		else if ( VarValue *vv = c->Values().Get( name ) )
			vv->Write( cxt.result() );
		else
			cxt.error("variable '" + name + "' does not exist in this context" );

	}
	else cxt.error("no active case");
}



static bool sg_scriptSimCancel;
class ScriptSimulationHandler : public ISimulationHandler
{
public:
	ScriptSimulationHandler() {
		sg_scriptSimCancel = false;
	}
	
	virtual ~ScriptSimulationHandler() {
	}

	virtual void Update( float, const wxString & )
	{
		wxGetApp().Yield( true );
	}

	virtual bool IsCancelled()
	{
		return sg_scriptSimCancel;
	}

	static void Cancel() { sg_scriptSimCancel = true; }
};



static void fcall_simulate( lk::invoke_t &cxt )
{
	LK_DOC("simulate", "Run the base case simulation for the currently active case.  Errors and warnings are optionally returned in the first parameter.  The results in the user interface are not updated by default, but can be via the second parameter.", "( [string:messages], [boolean: update UI] ):boolean" );
	cxt.result().assign( 0.0 );
	if ( Case *c = CurrentCase() )
	{
		Simulation &bcsim = c->BaseCase();		
		bcsim.Clear();

		ExcelExchange &ex = c->ExcelExch();
		if ( ex.Enabled )
			ExcelExchange::RunExcelExchange( ex, c->Values(), &bcsim );

		if ( !bcsim.Prepare() )
		{
			cxt.result().assign( 0.0 );
			
			if( cxt.arg_count() > 0 )
				cxt.arg(0).assign(  wxJoin( c->BaseCase().GetAllMessages(), '\n' ) );

			return;
		}
		
		ScriptSimulationHandler ssh;		
		cxt.result().assign( bcsim.InvokeWithHandler( &ssh ) ? 1.0 : 0.0 );

		if( cxt.arg_count() > 0 )
			cxt.arg(0).assign(  wxJoin( c->BaseCase().GetAllMessages(), '\n' ) );

		if ( cxt.arg_count() > 1 && cxt.arg(1).as_boolean() )
			if ( CaseWindow *cw = SamApp::Window()->GetCaseWindow( c ) )
					cw->UpdateResults();
	}
	else cxt.error("no active case");
}

static void fcall_configuration( lk::invoke_t &cxt )
{
	LK_DOC( "configuration", "Change the current active case's technology/market configuration, or return the current configuration.", "(string:technology, string:financing):boolean or (none):array");

	Case *cc = CurrentCase();
	if ( !cc )
	{
		cxt.error("no active case");
		return;
	}
	if (cxt.arg_count() == 3)
	{ // go thorugh input pages and return callback and equation errors
		wxString tech = cxt.arg(0).as_string();
		wxString fin = cxt.arg(1).as_string();

		cxt.result().assign(0.0);
		wxArrayString techlist = SamApp::Config().GetTechnologies();
		if (techlist.Index(tech) == wxNOT_FOUND) return;
		wxArrayString finlist = SamApp::Config().GetFinancingForTech(tech);
		if (finlist.Index(fin) == wxNOT_FOUND) return;
		wxString config_messages = wxEmptyString;
		bool bset_config = cc->SetConfiguration(tech, fin, true, &config_messages);
		cxt.result().empty_vector();
		cxt.result().vec_append(bset_config);
		cxt.result().vec_append(config_messages);
	}
	else if ( cxt.arg_count() == 2 )
	{
		wxString tech = cxt.arg(0).as_string();
		wxString fin = cxt.arg(1).as_string();

		cxt.result().assign( 0.0 );
		wxArrayString techlist = SamApp::Config().GetTechnologies();
		if ( techlist.Index( tech ) == wxNOT_FOUND ) return;
		wxArrayString finlist = SamApp::Config().GetFinancingForTech( tech );
		if ( finlist.Index( fin ) == wxNOT_FOUND ) return;

		cxt.result().assign( cc->SetConfiguration( tech, fin, true, 0 ) ? 1.0 : 0.0 ); // invoke silently - do not show error messages
	}
	else
	{
		cxt.result().empty_vector();
		cxt.result().vec_append( cc->GetTechnology() );
		cxt.result().vec_append( cc->GetFinancing() );
	}
}

static void fcall_load_defaults( lk::invoke_t &cxt )
{
	LK_DOC( "load_defaults", "Load SAM default values for the current active case. An optional error message is stored in the first argument, if given.", "([<string:error>]):boolean" );
	if ( Case *c = CurrentCase() )
	{
		wxString err;
		if ( c->LoadDefaults( &err ) )
			cxt.result().assign( 1.0 );
		else
		{
			if ( cxt.arg_count() == 1 )
				cxt.arg(0).assign( err );

			cxt.result().assign( 0.0 );
		}
	}
	else cxt.error("no active case");
}

static void fcall_overwrite_defaults( lk::invoke_t &cxt )
{
	LK_DOC( "overwrite_defaults", "Overwrite SAM default values file for the current configuration with current values.", "(none):boolean");
	if ( Case *c = CurrentCase() )
		cxt.result().assign( c->SaveDefaults( true ) );
	else cxt.error("no active case");
}

static void fcall_list_technologies( lk::invoke_t &cxt )
{
	LK_DOC( "list_technologies", "List available technology options in SAM.", "(none):array" );
	wxArrayString list = SamApp::Config().GetTechnologies();
	cxt.result().empty_vector();
	for( size_t i=0;i<list.size();i++ )
		cxt.result().vec_append( list[i] );
}

static void fcall_list_financing( lk::invoke_t &cxt )
{
	LK_DOC( "list_financing", "List available financial model options for a particular technology.", "(string:technology):array" );
	wxArrayString list = SamApp::Config().GetFinancingForTech( cxt.arg(0).as_string() );
	cxt.result().empty_vector();
	for( size_t i=0;i<list.size();i++ )
		cxt.result().vec_append( list[i] );
}

static void fcall_library( lk::invoke_t &cxt )
{
	LK_DOC( "library", "Obtain a list of library types (no arguments), or all entries for a particular type (1 argument).", "( [string:lib type] ):array" );
	
	wxArrayString list;
	if ( cxt.arg_count() == 0 )
		list = Library::ListAll();
	else if ( Library *lib = Library::Find( cxt.arg(0).as_string() ) )
		list = lib->ListEntries();

	cxt.result().empty_vector();
	for( size_t i=0;i<list.size();i++ )
		cxt.result().vec_append( list[i] );
}

class lkParSimList : public std::vector<Simulation*>
{
public:
	lkParSimList() : std::vector<Simulation*>() { }
	virtual ~lkParSimList()
	{ 
		delete_sims();
	}

	void delete_sims()
	{
		for( iterator it = begin();
			it != end();
			++it )
			delete *it;
		clear();
	}
};

static lkParSimList sg_parSims;

static void fcall_parsim( lk::invoke_t &cxt )
{
	LK_DOC( "parsim", "Run a set of simulations in parallel. Options include 'nthreads'.  Returns the number of successful runs.", "( array-of-tables:runs, [table:options] ):number" );

	sg_parSims.delete_sims();

	Case *cc = CurrentCase();
	if ( !cc )
	{
		cxt.error("no active case");
		return;
	}

	int nthreads = wxThread::GetCPUCount();
	if ( cxt.arg_count() > 1  && cxt.arg(1).type() == lk::vardata_t::HASH )
	{
		if ( lk::vardata_t *x = cxt.arg(1).lookup("nthreads") )
			nthreads = x->as_integer();
	}
	
	lk::vardata_t &runs = cxt.arg(0);
	if ( runs.type() != lk::vardata_t::VECTOR )
	{
		cxt.error("first parameter to parsim() must be an array of tables");
		return;
	}
	
	SimulationDialog tpd( "Preparing simulations...", nthreads );

	for( size_t i=0;i<runs.length();i++ )
	{
		lk::vardata_t &run = runs.index(i)->deref();
		if ( run.type() != lk::vardata_t::HASH )
		{
			cxt.error(wxString::Format("run[%d] is not a valid table of name->value pairs",(int)i) );
			return;
		}

		Simulation *sim = new Simulation( cc, wxString::Format("run %d", (int)i+1 ) );
		sg_parSims.push_back( sim );

		for( lk::varhash_t::iterator it = run.hash()->begin();
			it != run.hash()->end();
			++it )
		{
			wxString name( it->first );
			VarValue value;
			if ( !value.Read( it->second->deref(), true ) )
			{
				cxt.error("error translating value for '" + name + wxString::Format("' in run [%d]", (int)i ) );
				return;
			}
			sim->Override( name, value );
		}

		if ( !sim->Prepare() )
		{
			cxt.error( wxString::Format("internal error preparing run %d in parsim()", (int)(i+1)) );
			return;
		}

		tpd.Update( 0, (float)i / (float)runs.length() * 100.0f, wxString::Format("%d of %d", (int)(i+1), (int)runs.length()  ) );
		
		if ( tpd.Canceled() )
			return;
	}	

	if ( nthreads > (int)sg_parSims.size() ) nthreads = sg_parSims.size();
	tpd.NewStage("Calculating...", nthreads);

	int nok = Simulation::DispatchThreads( tpd, sg_parSims, nthreads );
	cxt.result().assign( (double)nok );
}

void fcall_parout( lk::invoke_t &cxt )
{
	LK_DOC( "parout", "Obtain an output from a parallel run, or retrieve information about the run as a table when no variable name is passed.", "( integer:run number, [string:variable] ):variant" );

	size_t idx = cxt.arg(0).as_unsigned();
	if ( idx >= sg_parSims.size() ) return;
	Simulation &sim = *(sg_parSims[idx]);

	if ( cxt.arg_count() == 1 )
	{
		cxt.result().empty_hash();
		cxt.result().hash_item( "ok", sim.Ok() ? 1.0 : 0.0 );
		cxt.result().hash_item( "time", (double)sim.GetTotalElapsedTime() );
		cxt.result().hash_item( "errors", wxJoin(sim.GetErrors(), ';') );
		cxt.result().hash_item( "warnings", wxJoin(sim.GetWarnings(), ';') );
		cxt.result().hash_item( "notices", wxJoin(sim.GetNotices(), ';') );
	}
	else if ( VarValue *vv = sim.GetValue( cxt.arg(1).as_string() ) )
		vv->Write( cxt.result() );
}

void fcall_show_page(lk::invoke_t &cxt)
{
	LK_DOC("show_page", "Show a specific page in the user interface for the active case", "( string:page name ):boolean");
	wxString page_name = cxt.arg(0).as_string();
	Case *active_case = CurrentCase();
	if (CaseWindow *case_window = SamApp::Window()->GetCaseWindow(active_case))
		cxt.result().assign((case_window->SwitchToPage(page_name) ? 1.0 : 0.0));
	else cxt.error("no active case");
}

void fcall_widgetpos( lk::invoke_t &cxt )
{
	LK_DOC("widgetpos", "Get geometry in pixels in toplevel coordinates of the widget associated with the specified variable.", "(string:name):array" );
	
	Case *active_case = CurrentCase();
	if (CaseWindow *cw = SamApp::Window()->GetCaseWindow(active_case))
	{
		ActiveInputPage *aip = 0;
		wxUIObject *obj = cw->FindActiveObject( cxt.arg(0).as_string(), &aip );
		if (!obj || !aip) return;

		wxRect rct( wxScaleRect(obj->GetGeometry(), wxGetScreenHDScale() ) );
		wxPoint pos = aip->ClientToScreen( wxPoint(rct.x, rct.y) );
		
		cxt.result().empty_vector();
		cxt.result().vec_append( pos.x );
		cxt.result().vec_append( pos.y );
		cxt.result().vec_append( rct.width );
		cxt.result().vec_append( rct.height );
	}
}

void fcall_focusto( lk::invoke_t &cxt )
{
	LK_DOC("focusto", "Set focus and scroll to the specified input if it is visible on screen.", "(string:name):none" );
	
	Case *active_case = CurrentCase();
	if (CaseWindow *cw = SamApp::Window()->GetCaseWindow(active_case))
	{
		ActiveInputPage *aip = 0;
		wxUIObject *obj = cw->FindActiveObject( cxt.arg(0).as_string(), &aip );
		if (!obj || !aip) return;

		if ( wxWindow *native = obj->GetNative() )
			if ( native->IsShownOnScreen() )
				native->SetFocus();
	}
}
	
// external fcalls that are compatible with running in a script environment
extern void fcall_group_read(lk::invoke_t &);
extern void fcall_group_write(lk::invoke_t &);
extern void fcall_calculated_list(lk::invoke_t &);
extern void fcall_urdb_read(lk::invoke_t &);
extern void fcall_urdb_write(lk::invoke_t &);
extern void fcall_urdb_get(lk::invoke_t &);
extern void fcall_urdb_list_utilities(lk::invoke_t &);
extern void fcall_urdb_list_utilities_by_zip_code(lk::invoke_t &);
extern void fcall_urdb_list_rates(lk::invoke_t &);

lk::fcall_t *sam_functions() {
	
	static const lk::fcall_t vec[] = {
		fcall_open_project,
		fcall_close_project,
		fcall_save_project,
		fcall_create_case,
		fcall_duplicate_case,
		fcall_project_file,
		fcall_list_cases,
		fcall_active_case,
		fcall_varinfo,
		fcall_selectinputs,
		fcall_set,
		fcall_get,
		fcall_simulate,
		fcall_show_page,
		fcall_widgetpos,
		fcall_focusto,
		fcall_configuration,
		fcall_library,
		fcall_load_defaults,
		fcall_overwrite_defaults,
		fcall_list_technologies,
		fcall_list_financing,
		fcall_calculated_list,
		fcall_group_read,
		fcall_group_write,
		fcall_urdb_read,
		fcall_urdb_write,
		fcall_urdb_get,
		fcall_urdb_list_utilities,
		fcall_urdb_list_utilities_by_zip_code,
		fcall_urdb_list_rates,
		fcall_parsim,
		fcall_parout,
		0 };
	return (lk::fcall_t*)vec;

};

SamScriptWindowFactory::SamScriptWindowFactory()
{
	// nothing to do
}

SamScriptWindowFactory::~SamScriptWindowFactory()
{
	// nothing to do
}

wxLKScriptWindow *SamScriptWindowFactory::Create()
{
	wxLKScriptWindow *sw = new SamScriptWindow( SamApp::Window(), wxID_ANY );	
#ifdef __WXMSW__
	sw->SetIcon( wxICON( appicon ) );
#endif	
	return sw;
}

enum { ID_VARIABLES = wxID_HIGHEST+494 };

BEGIN_EVENT_TABLE( SamScriptWindow, wxLKScriptWindow )
	EVT_BUTTON( ID_VARIABLES, SamScriptWindow::OnVariables )
END_EVENT_TABLE()

SamScriptWindow::SamScriptWindow( wxWindow *parent, int id )
	: wxLKScriptWindow( parent, id )
{
	int ntools = m_toolbar->GetItemCount();
	m_toolbar->Insert(ntools-3, new wxMetroButton( this, ID_VARIABLES, "Variables" ), 0, wxALL|wxEXPAND, 0 );

	wxLKScriptCtrl *sc = GetEditor();
	sc->RegisterLibrary( invoke_general_funcs(), "General Functions" );
	sc->RegisterLibrary( invoke_ssc_funcs(), "Direct Access To SSC" );
	sc->RegisterLibrary( sam_functions(), "SAM Functions");
}

void SamScriptWindow::OnVariables( wxCommandEvent & )
{
	VarSelectDialog dlg( this, "Browse Variables" );
	if ( Case *c = SamApp::Window()->GetCurrentCase() )
	{
		wxString tech, fin;
		c->GetConfiguration(&tech, &fin);
		dlg.SetConfiguration( tech, fin );
	}

	dlg.CenterOnParent();
	if ( dlg.ShowModal() == wxID_OK )
	{
		GetEditor()->InsertText(
			GetEditor()->GetCurrentPos(), wxJoin(dlg.GetCheckedNames(),',') );
	}
}

void SamScriptWindow::OnHelp( )
{
	SamApp::ShowHelp( "macros" );
}

void SamScriptWindow::OnScriptStarted()
{
	// let the SAM window be the parent for plots
	// rather than the current toplevel window so that they
	// hang around after a script window is closed
	wxLKSetToplevelParentForPlots( SamApp::Window() );

	// make sure there's no current plot active
	wxLKSetPlotTarget( NULL );
}

void SamScriptWindow::OnScriptStopped()
{
	CancelRunningSimulations();
}

void SamScriptWindow::CancelRunningSimulations()
{
	ScriptSimulationHandler::Cancel();
}

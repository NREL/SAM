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
	LK_DOC("varinfo", "Gets meta data about an input or output variable.", "(string:var name):table");
	wxString name = cxt.arg(0).as_string();
	cxt.result().empty_hash();
	if ( Case *c = CurrentCase() )
		invoke_get_var_info( c, name, cxt.result() );
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


static void fcall_set( lk::invoke_t &cxt )
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

static void fcall_get( lk::invoke_t &cxt )
{
	LK_DOC("get", "Get an output or input variable's value.", "(string:name):variant" );
	if ( Case *c = CurrentCase() )
	{
		wxString name = cxt.arg(0).as_string();
		if ( VarValue *vv = c->BaseCase().GetOutput( name ) )
			vv->Write( cxt.result() );
		else if ( VarValue *vv = c->Values().Get( name ) )
			vv->Write( cxt.result() );
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

	virtual void Update( float percent, const wxString &text )
	{
		wxGetApp().Yield( true );
	}

	virtual bool IsCancelled()
	{
		return sg_scriptSimCancel;
	}

	static void Cancel() { sg_scriptSimCancel = true; }
};


void ScriptWindow::CancelRunningSimulations()
{
	ScriptSimulationHandler::Cancel();
}

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

	if ( cxt.arg_count() == 2 )
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

	if ( nthreads > sg_parSims.size() ) nthreads = sg_parSims.size();
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


// external fcalls that are compatible with running in a script environment
extern void fcall_urdb_read( lk::invoke_t & );
extern void fcall_urdb_write( lk::invoke_t & );
extern void fcall_urdb_get( lk::invoke_t & );
extern void fcall_urdb_list_utilities( lk::invoke_t & );
extern void fcall_urdb_list_rates( lk::invoke_t & );

lk::fcall_t *sam_functions() {
	
	static const lk::fcall_t vec[] = {
		fcall_open_project,
		fcall_close_project,
		fcall_save_project,
		fcall_create_case,
		fcall_project_file,
		fcall_list_cases,
		fcall_active_case,
		fcall_varinfo,
		fcall_selectinputs,
		fcall_set,
		fcall_get,
		fcall_simulate,
		fcall_configuration,
		fcall_library,
		fcall_load_defaults,
		fcall_overwrite_defaults,
		fcall_list_technologies,
		fcall_list_financing,
		fcall_urdb_read,
		fcall_urdb_write,
		fcall_urdb_get,
		fcall_urdb_list_utilities,
		fcall_urdb_list_rates,
		fcall_parsim,
		fcall_parout,
		0 };
	return (lk::fcall_t*)vec;

};

class SamScriptCtrl : public wxLKScriptCtrl
{
	ScriptWindow *m_scriptwin;
public:
	SamScriptCtrl( wxWindow *parent, int id, ScriptWindow *scriptwin )
		: wxLKScriptCtrl( parent, id, wxDefaultPosition, wxDefaultSize,
			(wxLK_STDLIB_BASIC|wxLK_STDLIB_STRING|wxLK_STDLIB_MATH|wxLK_STDLIB_WXUI|wxLK_STDLIB_PLOT|wxLK_STDLIB_MISC|wxLK_STDLIB_SOUT) ),
		 m_scriptwin( scriptwin )
	{
		ShowFindInFilesButton( true );

		// register SAM-specific invoke functions here
		RegisterLibrary( invoke_general_funcs(), "General Functions" );
		RegisterLibrary( invoke_ssc_funcs(), "Direct Access To SSC" );
		RegisterLibrary( sam_functions(), "SAM Functions");
	}

	virtual bool OnFindInFiles( const wxString &text, bool match_case, bool whole_word )
	{;
		std::vector<ScriptWindow*> windows = m_scriptwin->GetWindows();
		
		wxProgressDialog dialog( "Find in files", "Searching for " + text, (int)windows.size(), m_scriptwin,
			wxPD_SMOOTH|wxPD_CAN_ABORT );
		dialog.SetClientSize( wxSize(350,100) );
		dialog.CenterOnParent();
		dialog.Show();

		m_scriptwin->ClearOutput();
		int noccur = 0;
		for( size_t i=0;i<windows.size();i++ )
		{
			ScriptWindow *sw = windows[i];

			int iter = 0;
			int pos, line_num;
			wxString line_text;
			while( sw->Find( text, match_case, whole_word,
				iter == 0, &pos, &line_num, &line_text ) )
			{
				m_scriptwin->AddOutput( sw->GetTitle() + " (" + wxString::Format("%d):  ", line_num+1) + line_text );
				noccur++;
				iter++;
			}

			if ( !dialog.Update( i ) )
				break;
		}

		m_scriptwin->AddOutput( wxString::Format("\n%d files searched, %d occurences found.", (int)windows.size(), noccur) );

		return true;
	}
	
	virtual bool OnEval( int line )
	{
		wxGetApp().Yield( true );
		return true;
	}

	virtual void OnOutput( const wxString &out )
	{
		m_scriptwin->AddOutput( out );
	}
};

enum { ID_SCRIPT = wxID_HIGHEST+494 ,
	ID_VARIABLES, ID_FUNCTIONS, ID_OUTPUT_TEXT };

BEGIN_EVENT_TABLE( ScriptWindow, wxFrame )
	EVT_BUTTON( wxID_NEW, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_OPEN, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_SAVE, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_SAVEAS, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_FIND, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_EXECUTE, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_STOP, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_CLOSE, ScriptWindow::OnCommand )
	EVT_BUTTON( wxID_HELP, ScriptWindow::OnCommand )
	EVT_BUTTON( ID_VARIABLES, ScriptWindow::OnCommand )
	EVT_BUTTON( ID_FUNCTIONS, ScriptWindow::OnCommand )
	
	EVT_MENU( wxID_NEW, ScriptWindow::OnCommand )
	EVT_MENU( wxID_OPEN, ScriptWindow::OnCommand )
	EVT_MENU( wxID_SAVE, ScriptWindow::OnCommand )
	EVT_MENU( wxID_FIND, ScriptWindow::OnCommand )
	EVT_MENU( wxID_EXECUTE, ScriptWindow::OnCommand )
	EVT_MENU( wxID_CLOSE, ScriptWindow::OnCommand )
	EVT_MENU( wxID_HELP, ScriptWindow::OnCommand )

	EVT_STC_MODIFIED( ID_SCRIPT, ScriptWindow::OnModified )
	EVT_CLOSE( ScriptWindow::OnClose )
END_EVENT_TABLE()


ScriptWindow::ScriptWindow( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size )
	: wxFrame( parent, id, wxT("untitled"), pos, size ) 
{
	m_lastFindPos = 0;

#ifdef __WXMSW__
	SetIcon( wxICON( appicon ) );
#endif	
	SetBackgroundColour( wxMetroTheme::Colour(wxMT_FOREGROUND) );
	
#ifdef __WXOSX__
	wxMenu *file = new wxMenu;
	file->Append( wxID_NEW );
	file->AppendSeparator();
	file->Append( wxID_OPEN );
	file->Append( wxID_SAVE );
	file->Append( wxID_SAVEAS );
	file->AppendSeparator();
	file->Append( wxID_EXECUTE );
	file->AppendSeparator();
	file->Append( wxID_CLOSE );

	wxMenuBar *menuBar = new wxMenuBar;
	menuBar->Append( file, wxT("&File") );
	SetMenuBar( menuBar );
#endif	

	wxBoxSizer *toolbar = new wxBoxSizer( wxHORIZONTAL );
	toolbar->Add( new wxMetroButton( this, wxID_NEW, "New" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( new wxMetroButton( this, wxID_OPEN, "Open" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( new wxMetroButton( this, wxID_SAVE, "Save" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( new wxMetroButton( this, wxID_SAVEAS, "Save as" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( new wxMetroButton( this, wxID_FIND, "Find" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( m_runBtn=new wxMetroButton( this, wxID_EXECUTE, "Run", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( m_stopBtn=new wxMetroButton( this, wxID_STOP, "Stop" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->AddStretchSpacer();
	toolbar->Add( new wxMetroButton( this, ID_VARIABLES, "Variables" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( new wxMetroButton( this, ID_FUNCTIONS, "Functions" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( new wxMetroButton( this, wxID_HELP, "Help" ), 0, wxALL|wxEXPAND, 0 );
	toolbar->Add( new wxMetroButton( this, wxID_CLOSE, "Close" ), 0, wxALL|wxEXPAND, 0 );

	m_stopBtn->Hide();

	wxSplitterWindow *split = new wxSplitterWindow( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_LIVE_UPDATE|wxSP_3DSASH|wxBORDER_NONE );

	m_script = new SamScriptCtrl( split, ID_SCRIPT, this );

	m_output = new wxTextCtrl( split, ID_OUTPUT_TEXT, wxT("Ready."), wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY|wxBORDER_NONE );
	
	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( toolbar, 0, wxALL|wxEXPAND, 0 );
	sizer->Add( split, 1, wxALL|wxEXPAND, 0 );
	SetSizer( sizer );
	
	split->SetMinimumPaneSize( 100 );
	split->SplitHorizontally( m_script, m_output, -150 );
	split->SetSashGravity( 1.0 );
		
	std::vector<wxAcceleratorEntry> entries;
	entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 'n', wxID_NEW ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 'o', wxID_OPEN ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 's', wxID_SAVE ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 'f', wxID_FIND ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 'w', wxID_CLOSE ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CMD, 'i', wxID_CLOSE ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_NORMAL, WXK_F1, wxID_HELP ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_NORMAL, WXK_F5, wxID_EXECUTE ) );
	SetAcceleratorTable( wxAcceleratorTable( entries.size(), &entries[0] ) );

	UpdateWindowTitle();
}


ScriptWindow *ScriptWindow::CreateNewWindow( bool show )
{
	ScriptWindow *sw = new ScriptWindow( SamApp::Window(), wxID_ANY, wxDefaultPosition, wxSize( 760, 800 ) );
	sw->Show( show );
	return sw;
}

void ScriptWindow::OpenFiles( ScriptWindow *current )
{
	wxWindow *parent = current;
	if ( parent == 0 ) parent = SamApp::Window();

	wxFileDialog dlg( parent, "Open script", 
		wxEmptyString, wxEmptyString, "SAM Script Files (*.lk)|*.lk", wxFD_OPEN|wxFD_MULTIPLE );
	if ( wxID_OK == dlg.ShowModal() )
	{
		wxArrayString files;
		dlg.GetPaths( files );
		for( size_t i=0;i<files.size();i++ )
		{
			if ( ScriptWindow *sw = FindOpenFile( files[i] ) )
			{
				sw->Raise();
				sw->SetFocus();
				continue;
			}

			if ( wxFileExists( files[i] ) )
			{
				ScriptWindow *sw = ( current != 0 && current->GetFileName().IsEmpty() ) ? current : CreateNewWindow( false );
				if ( !sw->Load( files[i] ) )
				{
					wxMessageBox( "Failed to load script.\n\n" + files[i] );
					if ( sw != current ) sw->Destroy();
				}
				else
					sw->Show();
			}
		}
	}
}



std::vector<ScriptWindow*> ScriptWindow::GetWindows()
{
	std::vector<ScriptWindow*> list;
	wxFrame *top = SamApp::Window();
	wxWindowList &wl = top->GetChildren();
	for( wxWindowList::iterator it = wl.begin();
		it != wl.end();
		++it )
		if ( ScriptWindow *scrip = dynamic_cast<ScriptWindow*>( *it ) )
			list.push_back( scrip );

	return list;
}

bool ScriptWindow::CloseAll()
{
	bool closed = true;
	std::vector<ScriptWindow*> list = GetWindows();
	for( size_t i=0;i<list.size();i++ )
		if ( !list[i]->Close() )
			closed = false;

	return closed;
}

ScriptWindow *ScriptWindow::FindOpenFile( const wxString &file )
{
	std::vector<ScriptWindow*> list = GetWindows();
	for( size_t i=0;i<list.size();i++ )
		if ( list[i]->GetFileName() == file )
			return list[i];

	return 0;
}

void ScriptWindow::AddOutput( const wxString &out )
{
	m_output->AppendText( out );
}

void ScriptWindow::ClearOutput()
{
	m_output->Clear();
}

bool ScriptWindow::Save()
{
	if ( m_fileName.IsEmpty() )
		return SaveAs();
	else
		return Write( m_fileName );
}

bool ScriptWindow::SaveAs()
{
	wxFileDialog dlg( this, "Save as...",
		wxPathOnly(m_fileName),
		wxFileNameFromPath(m_fileName),
		"SAM Script Files (*.lk)|*.lk", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
	if (dlg.ShowModal() == wxID_OK)
		return Write( dlg.GetPath() );
	else
		return false;
}

bool ScriptWindow::Load( const wxString &file )
{
	if( m_script->ReadAscii( file ) )
	{
		m_fileName = file;
		UpdateWindowTitle();
		return true;
	}
	else
		return false;
}

bool ScriptWindow::Write( const wxString &file )
{
	wxBusyInfo info( "Saving: " + file, this );
	wxMilliSleep( 120 );

	if ( m_script->WriteAscii( file ) )
	{
		m_fileName = file;
		UpdateWindowTitle();
		return true;
	}
	else return false;
}

void ScriptWindow::UpdateWindowTitle()
{
	wxString title( m_fileName );
	if ( title.IsEmpty() ) title = "untitled";
	if ( m_script->IsModified() ) title += " *";
	if ( m_lastTitle != title )
	{
		SetTitle( title );
		m_lastTitle = title;
	}
}

wxString ScriptWindow::GetFileName()
{
	return m_fileName;
}

void ScriptWindow::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case wxID_NEW:
		CreateNewWindow();
		break;

	case wxID_OPEN:
		OpenFiles( this );
		break;

	case wxID_SAVE:
		Save();
		break;

	case wxID_SAVEAS:
		SaveAs();
		break;

	case wxID_FIND:
		m_script->ShowFindReplaceDialog();
		break;

	case wxID_EXECUTE:
		m_output->Clear();
		m_runBtn->Hide();
		m_stopBtn->Show();
		Layout();
		wxGetApp().Yield( true );
		m_script->Execute( wxPathOnly(m_fileName), SamApp::Window() );
		m_stopBtn->Hide();
		m_runBtn->Show();
		Layout();
		break;

	case wxID_HELP:
		SamApp::ShowHelp( "macros" );
		break;
		
	case wxID_STOP:
		// first cancel any simulations that
		// might be running that were invoked 
		// from a script
		ScriptWindow::CancelRunningSimulations();
		m_script->Stop();
		break;

	case wxID_CLOSE:
		Close();
		break;
		
	case ID_FUNCTIONS:
		m_script->ShowHelpDialog();
		break;

	case ID_VARIABLES:
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
			m_script->InsertText(
				m_script->GetCurrentPos(), wxJoin(dlg.GetCheckedNames(),',') );
		}
	}
		break;

	};
}

bool ScriptWindow::Find( const wxString &text, 
	bool match_case, bool whole_word, bool at_beginning,
	int *pos, int *line, wxString *line_text )
{
	if ( text.Len() == 0 ) return false;

	int flags = 0;	
	if ( whole_word ) flags |= wxSTC_FIND_WHOLEWORD;	
	if ( match_case ) flags |= wxSTC_FIND_MATCHCASE;

	if ( at_beginning )
		m_lastFindPos = 0;

	m_lastFindPos = m_script->FindText( m_lastFindPos, 
		m_script->GetLength(), text, flags );

	if ( m_lastFindPos >= 0 )
	{
		*pos = m_lastFindPos;
		*line = m_script->LineFromPosition( m_lastFindPos );
		*line_text = m_script->GetLine( *line );
		m_lastFindPos += text.Len();
		return true;
	}
	else
		return false;
}

void ScriptWindow::OnModified( wxStyledTextEvent & )
{
	UpdateWindowTitle();
}

void ScriptWindow::OnClose( wxCloseEvent &evt )
{	
	if ( m_script->IsScriptRunning() )
	{
		if ( wxYES == wxMessageBox("The script is running.  Stop it?", "Query", wxYES_NO, this ) )
			m_script->Stop();

		evt.Veto();
		return;
	}

	if ( m_script->IsModified() )
	{
		Raise();
		int ret = wxMessageBox("The script '" + m_fileName + "' has been modified.  Save changes?", "Query", 
			wxICON_EXCLAMATION|wxYES_NO|wxCANCEL, this );
		if (ret == wxYES)
		{
			Save( );
			if ( m_script->IsModified() ) // if failed to save, cancel
			{
				evt.Veto();
				return;
			}
		}
		else if (ret == wxCANCEL)
		{
			evt.Veto();
			return;
		}
	}
	
	wxGetApp().ScheduleForDestruction( this );
}

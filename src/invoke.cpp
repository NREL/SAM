#include <wx/log.h>

#include <wex/plot/plplotctrl.h>
#include <wex/lkscript.h>

#include "main.h"

#include "invoke.h"

static void fcall_log( lk::invoke_t &cxt )
{
	LK_DOC("log", "Output a data line to the SAM log.", "(...):none");	
	wxString output;
	for (size_t i=0;i<cxt.arg_count();i++)
		output += cxt.arg(i).as_string();
	wxLogStatus( output );
}

static void fcall_browse( lk::invoke_t &cxt )
{
	LK_DOC("browse", "Open a URL, local file, or folder using the default browser.", "(string:url):none");
	::wxLaunchDefaultBrowser( cxt.arg(0).as_string() );
}


static void fcall_addconfig( lk::invoke_t &cxt )
{
	LK_DOC("addconfig", "Add a technology+financing options", "( string:tech, array:financings ):none" );

	wxArrayString finlist;
	lk::vardata_t &fins = cxt.arg(1);
	for( size_t i=0;i<fins.length();i++ )
		finlist.Add( fins.index(i)->as_string() );
	SamApp::Config().Add( cxt.arg(0).as_string(), finlist );

	wxLogStatus( "Configuration: " + cxt.arg(0).as_string() + "  -> [ " + wxJoin(finlist,';') + " ]" );
}

static void fcall_setconfig( lk::invoke_t &cxt )
{
	LK_DOC("setconfig", "Sets the currently active configuration for editing", "(string:Tech, string:Financing):none");
	SamApp::Config().SetConfig( cxt.arg(0).as_string(), cxt.arg(1).as_string() );
}

static void fcall_addpage( lk::invoke_t &cxt )
{
	LK_DOC("addpage", "Add an input page group to the currently active configuration (may have multiple pages).", "(array:pages, [string:caption, string:helpcxt, boolean:exclusive, string:exclusive var name]" );
	
	wxArrayString pages;
	lk::vardata_t &grps = cxt.arg(0);
	for( size_t i=0;i<grps.length();i++ )
		pages.Add( grps.index(i)->as_string() );

	if ( pages.size() == 0 ) return;

	wxString capt = pages[0];
	if ( cxt.arg_count() > 1 )
		capt = cxt.arg(1).as_string();

	wxString help = pages[0];
	if (cxt.arg_count() > 2 )
		wxString help = cxt.arg(2).as_string();

	bool subgrps = false;
	if ( cxt.arg_count() > 3 )
		subgrps = cxt.arg(3).as_boolean();

	wxString subgrpvar;
	if ( cxt.arg_count() > 4 )
		subgrpvar = cxt.arg(4).as_string();

	SamApp::Config().AddInputPageGroup( pages, capt, help, subgrps, subgrpvar );
}


static void plottarget( CallbackContext *cc, const wxString &name )
{
	wxLKSetPlotTarget( 0 );
	if ( wxUIObject *obj = cc->GetInputPage()->Find(name) )
		if ( wxPLPlotCtrl *plot = obj->GetNative<wxPLPlotCtrl>() )
			wxLKSetPlotTarget( plot );
}

void fcall_setplot( lk::invoke_t &cxt )
{
	LK_DOC("setplot", "Sets the current plot target by name", "(string:name):boolean");
	
	plottarget( (CallbackContext*)cxt.user_data(), cxt.arg(0).as_string() );
}

void fcall_clearplot( lk::invoke_t &cxt )
{
	LK_DOC("clearplot", "Clears the current plot, and optionally switches the plot target.", "([string:plot name]):none");
	
	if (cxt.arg_count() > 0)
		plottarget( (CallbackContext*)cxt.user_data(), cxt.arg(0).as_string() );

	if ( wxPLPlotCtrl *plot = wxLKGetPlotTarget() )
	{
		plot->DeleteAllPlots();
		plot->Refresh();
	}
}

void fcall_value( lk::invoke_t &cxt )
{
	LK_DOC("value", "Gets or sets the value of a variable by name", "(string:name [,variant:value]):[variant]");
	
	CallbackContext *cc = (CallbackContext*)cxt.user_data();
	wxString name = cxt.arg(0).as_string();
	if ( VarValue *vv = cc->GetVarTable()->Get( name ) )
	{
		if ( cxt.arg_count() == 2 )
		{
			vv->Read( cxt.arg(1) );
			cc->GetInputPage()->OnVariableChanged( name );		
		}
		else
		{
			vv->Write( cxt.result() );
		}
	}
}

void fcall_enable( lk::invoke_t &cxt )
{
	LK_DOC("enable", "Enable or disable a user interface widget", "(string:name, boolean:enable):none");

	CallbackContext *cc = (CallbackContext*)cxt.user_data();
	if ( wxUIObject *obj = cc->GetInputPage()->Find( cxt.arg(0).as_string() ) )
		if ( wxWindow *native = obj->GetNative() )
			native->Enable( cxt.arg(1).as_boolean() );
}
static void fcall_show( lk::invoke_t &cxt )
{
	LK_DOC("show", "Show or hide a user interface widget.", "(string:name, boolean:show):none");

	CallbackContext *cc = (CallbackContext*)cxt.user_data();
	if ( wxUIObject *obj = cc->GetInputPage()->Find( cxt.arg(0).as_string() ) )
		obj->Show( cxt.arg(1).as_boolean() );
}


lk::fcall_t* invoke_general_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_log,
		fcall_browse,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_config_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_addconfig,
		fcall_setconfig,
		fcall_addpage,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_uicallback_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_setplot,
		fcall_clearplot,
		fcall_value,
		fcall_enable,
		fcall_show,
		0 };
	return (lk::fcall_t*)vec;
}
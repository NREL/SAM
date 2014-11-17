#include <algorithm>
#include <wx/log.h>
#include <wx/tokenzr.h>
#include <wx/filename.h>
#include <wx/buffer.h>
#include <wx/mstream.h>
#include <wx/busyinfo.h>

#include <wex/plot/plplotctrl.h>
#include <wex/lkscript.h>
#include <wex/dview/dvplotctrl.h>
#include <wex/utils.h>
#include <wex/csv.h>

#ifdef __WXMSW__
#include <wex/ole/excelauto.h>
#endif

#include <ssc/sscapi.h>

#include "main.h"
#include "case.h"
#include "simulation.h"
#include "casewin.h"
#include "materials.h"
#include "results.h"
#include "solarprospector.h"
#include "windtoolkit.h"
#include "simplecurl.h"
#include "urdb.h"
#include "invoke.h"
#include "s3tool.h"
#include "lossdiag.h"

static void fcall_dview(lk::invoke_t &cxt)
{
	LK_DOC("dview", "Creates a separate dview viewer for viewing specified data.", "( number:num_datasets, number:timestep, string:window name, string:data_name1, string:data_units1, number:multiplier1, variant:data1, [ string:data_name2, string:data_units2, number:multiplier2, variant:data2], ...):none");


	int num_datasets = (int)cxt.arg(0).as_number();
	double timestep = cxt.arg(1).as_number(); // fraction of hour
	wxString win_name = cxt.arg(2).as_string();

	size_t data_length = 8760;
	if (timestep > 0)
		data_length /= timestep;

	size_t ndx = 3;
	if ((4 * num_datasets + ndx) != cxt.arg_count()) return;

	wxFrame *frame = new wxFrame( SamApp::Window(), wxID_ANY, "Data Viewer: " + win_name, wxDefaultPosition, wxSize(1100,700),
		(wxCAPTION | wxCLOSE_BOX | wxCLIP_CHILDREN | wxRESIZE_BORDER | wxFRAME_TOOL_WINDOW | wxFRAME_FLOAT_ON_PARENT) );
	wxDVPlotCtrl *dview = new wxDVPlotCtrl(frame, wxID_ANY);
	
	while (ndx < cxt.arg_count())
	{ 
		wxString data_name = cxt.arg(ndx++).as_string();
		wxString data_units = cxt.arg(ndx++).as_string();
		double scale = cxt.arg(ndx++).as_number();
		lk::vardata_t &data = cxt.arg(ndx++);
		if (data.length() != data_length) return;

		std::vector<double> plot_data( data_length );
		for (size_t i = 0; i < data_length; i++)
			plot_data[i] = scale * data.index(i)->as_number();

		dview->AddDataSet(new wxDVArrayDataSet(data_name, data_units, timestep, plot_data), win_name );
	}

	dview->GetStatisticsTable()->RebuildDataViewCtrl();
	dview->SelectDataIndex(0);
	dview->DisplayTabs();

	frame->Show();
}

struct wfvec {
	char *name;
	char *label;
	char *units;
};

static void fcall_dview_solar_data_file( lk::invoke_t &cxt )
{
	LK_DOC("dview_solar", "Read a solar weather data file on disk (*.tm2,*.tm3,*.epw,*.smw) and popup a frame with a data viewer.", "(string:filename):boolean");

	wxString file( cxt.arg(0).as_string() );
	if ( !wxFileExists( file ) ) {
		cxt.result().assign( 0.0 );
		return;
	}

	
	ssc_data_t pdata = ssc_data_create();
	ssc_data_set_string(pdata, "file_name", (const char*)file.c_str());
	ssc_data_set_number(pdata, "header_only", 0);

	if ( const char *err = ssc_module_exec_simple_nothread( "wfreader", pdata ) )
	{
		wxLogStatus("error scanning '" + file + "'");
		cxt.result().assign(0.0);
		return;
	}

	wxFrame *frame = new wxFrame( SamApp::Window(), wxID_ANY, "Data Viewer: " + file, wxDefaultPosition, wxSize(1100,700),
		(wxCAPTION | wxCLOSE_BOX | wxCLIP_CHILDREN | wxRESIZE_BORDER | wxFRAME_TOOL_WINDOW | wxFRAME_FLOAT_ON_PARENT) );
	wxDVPlotCtrl *dview = new wxDVPlotCtrl(frame, wxID_ANY);

	// this information is consistent with the variable definitions in the wfreader module
	wfvec vars[] = {
		{ "global", "Global irradiance - GHI", "W/m2" },
		{ "beam", "Beam irradiance - DNI", "W/m2" },
		{ "diffuse","Diffuse irradiance - DHI", "W/m2" },
		{ "wspd", "Wind speed", "m/s" },
		{ "wdir", "Wind direction", "deg" },
		{ "tdry", "Dry bulb temp", "C" },
		{ "twet", "Wet bulb temp", "C" },
		{ "tdew", "Dew point temp", "C" },
		{ "rhum", "Relative humidity", "%" },
		{ "pres", "Pressure", "millibar" },
		{ "snow", "Snow depth", "cm" },
		{ "albedo", "Albedo", "fraction" },
		{ 0, 0, 0 } };

	ssc_number_t start, step; // start & step in seconds, then convert to hours
	ssc_data_get_number( pdata, "start", &start ); start /= 3600;
	ssc_data_get_number( pdata, "step", &step ); step /= 3600;

	size_t i=0;
	while( vars[i].name != 0 )
	{
		int len;
		ssc_number_t *p = ssc_data_get_array( pdata, vars[i].name, &len );
		if ( p != 0 && len > 2 )
		{
			std::vector<double> plot_data(len);
			for (int j = 0; j < len; j++)
				plot_data[j] = p[j];

			dview->AddDataSet( new wxDVArrayDataSet( vars[i].label, vars[i].units, start, step, plot_data ), 
				wxFileNameFromPath( file ) );
		}

		i++;
	}

	ssc_data_free( pdata );
	
	dview->GetStatisticsTable()->RebuildDataViewCtrl();
	if ( i > 0 )
		dview->SelectDataIndex(0);

	dview->DisplayTabs();

	frame->Show();
}

static void fcall_logmsg( lk::invoke_t &cxt )
{
	LK_DOC("logmsg", "Output a data line to the SAM log.", "(...):none");	
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

static void fcall_webapi( lk::invoke_t &cxt )
{
	LK_DOC( "webapi", "Returns the URL for the SAM web API requested.  No arguments returns a list of names.", "( [string:name] ):string");
	cxt.result().assign( SamApp::WebApi( cxt.arg_count() > 0 ? cxt.arg(0).as_string() : wxEmptyString ) );
}

static void fcall_geocode( lk::invoke_t &cxt )
{
	LK_DOC( "geocode", "Returns the latitude and longitude of an address using Google's geocoding web API.", "(string:address):table");
	double lat = 0, lon = 0;
	wxSimpleCurl::GeoCode( cxt.arg(0).as_string(), &lat, &lon );
	cxt.result().empty_hash();
	cxt.result().hash_item("lat").assign(lat);
	cxt.result().hash_item("lon").assign(lon);
}

static void fcall_curl( lk::invoke_t &cxt )
{
	LK_DOC( "curl", "Issue a synchronous HTTP/HTTPS request.  By default a GET request, but can support POST via parameter 2.  If a third argument is given, the returned data is downloaded to the specified file on disk rather than returned as a string.", "(string:url, [string:post data], [string:local file]):string" );
	wxSimpleCurl curl;
	if( cxt.arg_count() > 1 )
		curl.SetPostData( cxt.arg(1).as_string() );
	
	wxString url(cxt.arg(0).as_string());
	url.Replace( "<SAMAPIKEY:WIND_BARRIERS>", "bd36882a2551ce65f1326626dcd110785ae967e2" );
	url.Replace( "<SAMAPIKEY:ENERGY_CROP>",   "bb4a09f7833f115ccba081b4a9e028ca904643df" );

	wxLogStatus( "curl: " + url );

	if ( !curl.Start( url, true ) )
	{
		cxt.result().assign( 0.0 );
		return;
	}

	if ( cxt.arg_count() > 2 )
		cxt.result().assign( curl.WriteDataToFile( cxt.arg(2).as_string() ) ? 1.0 : 0.0 );
	else
		cxt.result().assign( curl.GetDataAsString() );
}


static void fcall_appdir( lk::invoke_t &cxt )
{
	LK_DOC("appdir", "Return the application folder.", "(none):string");
	cxt.result().assign( SamApp::GetAppPath() );
}
static void fcall_runtimedir( lk::invoke_t &cxt )
{
	LK_DOC("runtimedir", "Return the application runtime data folder.", "(none):string");
	cxt.result().assign( SamApp::GetRuntimePath() );
}
static void fcall_userlocaldatadir( lk::invoke_t &cxt )
{
	LK_DOC("userlocaldatadir", "Return the user's local data folder in their home folder.", "(none):string");
	cxt.result().assign( SamApp::GetUserLocalDataDir() );
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

static void fcall_configopt( lk::invoke_t &cxt )
{
	LK_DOC("configopt", "Sets configuration options, such as long_name, short_name, description, etc.", "(string:config name, table:options):none");
	ConfigOptions &opt = SamApp::Config().Options( cxt.arg(0).as_string() );

	lk::vardata_t &tab = cxt.arg(1).deref();
	if( lk::vardata_t *vv = tab.lookup( "long_name" ) )
		opt.LongName = vv->as_string();
	if( lk::vardata_t *vv = tab.lookup( "short_name") ) 
		opt.ShortName = vv->as_string();
	if( lk::vardata_t *vv = tab.lookup( "description" ) )
		opt.Description = vv->as_string();

	// eventually can add other options too, such as icon name, etc
}

static void fcall_setmodules( lk::invoke_t &cxt )
{
	LK_DOC("setmodules", "Sets the simulation models for the currently active configuration", "(array:module names):none");
	
	wxArrayString list;
	lk::vardata_t &m = cxt.arg(0);
	for( size_t i=0;i<m.length();i++ )
		list.Add( m.index(i)->as_string() );

	SamApp::Config().SetModules( list );
}

static void fcall_addpage( lk::invoke_t &cxt )
{
	LK_DOC("addpage", "Add an input page group to the currently active configuration (may have multiple pages).", "(array:pages, table:caption,help,exclusive,exclusive_var):none" );
	
	std::vector< std::vector<PageInfo> > pages;
	lk::vardata_t &grps = cxt.arg(0);
	for( size_t i=0;i<grps.length();i++ )
	{
		pages.push_back( std::vector<PageInfo>() );

		for( size_t j=0;j<grps.index(i)->deref().length();j++ )
		{
			PageInfo x;

			lk::vardata_t &item = grps.index(i)->deref().index(j)->deref();

			if ( item.type() == lk::vardata_t::HASH )
			{
				if ( lk::vardata_t *name = item.lookup( "name" ) )
					x.Name = name->as_string();

				x.Caption = x.Name; // by default, caption=name
				x.CollapsedByDefault = true;
			
				if ( lk::vardata_t *capt = item.lookup( "caption" ) )
					x.Caption = capt->as_string();

				if ( lk::vardata_t *is_collap = item.lookup( "collapsible" ) )
					x.Collapsible = is_collap->as_boolean();

				if ( lk::vardata_t *var = item.lookup( "collapsible_var" ) )
					x.CollapsiblePageVar = var->as_string();

				if ( lk::vardata_t *initial = item.lookup( "collapsed_by_default" ) )
					x.CollapsedByDefault = initial->as_boolean();

				if ( lk::vardata_t *shlabel = item.lookup( "label" ) )
					x.ShowHideLabel = shlabel->as_string();
			}
			else
			{
				x.Name = item.as_string();
				x.Caption = x.Name;
			}

			if ( !x.Name.IsEmpty() )
				pages[i].push_back( x );
		}
	}

	if ( pages.size() == 0 || pages[0].size() == 0 ) return;
	
	wxString sidebar = pages[0][0].Name;
	wxString help = sidebar;
	wxString exclusive_var;
	
	if ( cxt.arg_count() > 1 )
	{
		lk::vardata_t &props = cxt.arg(1).deref();

		if( lk::vardata_t *x = props.lookup("sidebar") )
			sidebar = x->as_string();

		if ( lk::vardata_t *x = props.lookup("help") )
			help = x->as_string();
		
		if ( lk::vardata_t *x = props.lookup("exclusive_var") )
			exclusive_var = x->as_string();
	}

	SamApp::Config().AddInputPageGroup( pages, sidebar, help, exclusive_var );
}

static void fcall_setting( lk::invoke_t &cxt )
{
	LK_DOC( "setting", "Sets a setting field for the current configuration", "(string:name, string:value -or- table:name/value pairs):none");

	if ( ConfigInfo *ci = SamApp::Config().CurrentConfig() )
	{
		if( cxt.arg_count() == 2 )
			ci->Settings[ cxt.arg(0).as_string() ] = cxt.arg(1).as_string();
		else if ( cxt.arg_count() == 1 && cxt.arg(0).deref().type() == lk::vardata_t::HASH )
		{
			lk::varhash_t *hash = cxt.arg(0).deref().hash();
			for( lk::varhash_t::iterator it = hash->begin();
				it != hash->end();
				++it )
				ci->Settings[ it->first ] = it->second->deref().as_string();
		}
	}
}

static void fcall_technology( lk::invoke_t &cxt )
{
	LK_DOC( "technology", "Return the current technology option name", "(void):string" );
	CaseCallbackContext &cc = *static_cast<CaseCallbackContext*>( cxt.user_data() ); 
	cxt.result().assign( cc.GetCase().GetTechnology() );
}

static void fcall_financing( lk::invoke_t &cxt )
{
	LK_DOC( "financing", "Return the current financing option name", "(void):string" );
	CaseCallbackContext &cc = *static_cast<CaseCallbackContext*>( cxt.user_data() ); 
	cxt.result().assign( cc.GetCase().GetFinancing() );
}

static void fcall_metric( lk::invoke_t &cxt )
{
	LK_DOC("metric", "Add an output metric to the current configuration. Options include mode,deci,thousep,pre,post,label,scale", "(string:variable, [table:options]):none");
	 
	if ( ResultsCallbackContext *ci = static_cast<ResultsCallbackContext*>(cxt.user_data()) )
	{
		MetricData md;
		md.var = cxt.arg(0).as_string();
		
		if (cxt.arg_count() > 1 )
		{
			lk::vardata_t &opts = cxt.arg(1).deref();
			if ( lk::vardata_t *x = opts.lookup("mode") )
			{
				wxString mm = x->as_string();
				mm.MakeLower();
				if ( mm == "f" ) md.mode = 'f';
				else if ( mm == "e" ) md.mode = 'e';
				else if ( mm == "h" ) md.mode = 'h';			
			}

			if ( lk::vardata_t *x = opts.lookup("deci") )
				md.deci = x->as_integer();

			if ( lk::vardata_t *x = opts.lookup("thousep") )
				md.thousep = x->as_boolean();

			if ( lk::vardata_t *x = opts.lookup("pre") )
				md.pre = x->as_string();

			if ( lk::vardata_t *x = opts.lookup("post") )
				md.post = x->as_string();

			if ( lk::vardata_t *x = opts.lookup("label") )
				md.label = x->as_string();

			if ( lk::vardata_t *x = opts.lookup("scale") )
				md.scale = x->as_number();
		}

		ci->GetResultsViewer()->AddMetric( md );
	}
}

static void fcall_new_baseline( lk::invoke_t &cxt )
{
	LK_DOC("new_baseline", "Creates a new baseline section in the loss diagram.", "(string:variable name, string:text):none");

	if ( LossDiagCallbackContext *ldcc = static_cast<LossDiagCallbackContext*>(cxt.user_data()) )
	{
		Simulation &sim = ldcc->GetSimulation();
		LossDiagramObject &ld = ldcc->GetDiagram();

		if ( VarValue *vv = sim.GetValue( cxt.arg(0).as_string() ) )
		{
			if ( vv->Type() == VV_NUMBER )
				ld.NewBaseline( vv->Value(), cxt.arg(1).as_string() );
		}
	}
}

static void fcall_add_loss_term( lk::invoke_t &cxt )
{
	LK_DOC("add_loss_term", "Adds a loss to the loss diagram.", "(string:variable name, string:text):none" );
	if ( LossDiagCallbackContext *ldcc = static_cast<LossDiagCallbackContext*>(cxt.user_data()) )
	{
		Simulation &sim = ldcc->GetSimulation();
		LossDiagramObject &ld = ldcc->GetDiagram();
		
		if ( VarValue *vv = sim.GetValue( cxt.arg(0).as_string() ) )
		{
			if ( vv->Type() == VV_NUMBER )
				ld.AddLossTerm( vv->Value(), cxt.arg(1).as_string() );
		}
	}
}

static void fcall_agraph( lk::invoke_t &cxt )
{
	LK_DOC("agraph", "Create an autograph", "(string:Y, string:title, string:xlabel, string:ylabel, [int:size], [bool:show_xvalues], [bool:show_legend], [string:legend_position (bootom, right, floating)]):none" );
	
	if ( ResultsCallbackContext *ci = static_cast<ResultsCallbackContext*>(cxt.user_data()) )
	{
		AutoGraph ag;
		ag.yvals = cxt.arg(0).as_string();
		ag.title = cxt.arg(1).as_string();
		ag.xlabel = cxt.arg(2).as_string();
		ag.ylabel = cxt.arg(3).as_string();
		ag.size = 0;
		ag.show_xvalues = true;
		ag.show_legend = true;
		ag.legend_pos = "bottom";
		if (cxt.arg_count() > 4)
			ag.size = cxt.arg(4).as_integer();
		if (cxt.arg_count() > 5)
			ag.show_xvalues = cxt.arg(5).as_boolean();
		if (cxt.arg_count() > 6)
			ag.show_legend = cxt.arg(6).as_boolean();
		if (cxt.arg_count() > 7)
			ag.legend_pos = cxt.arg(7).as_string();

		ci->GetResultsViewer()->AddAutoGraph( ag );
	}
}

static void fcall_cfline(lk::invoke_t &cxt)
{
	LK_DOC("cfline", "Add one or more cashflow line items to the current configuration. Names can be comma-separated list. For spacer use name='', for header use digits=-1, for generic format use digits=-2, for integer cast use digits=-3.", "( string:names, [number:digits], [number:scale], [number:column offset] ):none");

	if (ResultsCallbackContext *ci = static_cast<ResultsCallbackContext*>(cxt.user_data()))
	{
		wxString name = cxt.arg(0).as_string();

		int type = CashFlowLine::VARIABLE;
		int digit = 2;
		float scale = 1.0f;
		size_t coloff = 0;
		if (cxt.arg_count() >= 2)
			digit = cxt.arg(1).as_integer();

		if (cxt.arg_count() >= 3)
			scale = (float)cxt.arg(2).as_number();

		if (cxt.arg_count() >= 4)
			coloff = cxt.arg(3).as_unsigned();

		if (name.IsEmpty()) type = CashFlowLine::SPACER;
		if (digit == -1) type = CashFlowLine::HEADER;

		wxArrayString list = wxSplit(name, ',');
		if (list.size() == 0) list.Add(name); // handle empty string

		for (size_t i = 0; i<list.Count(); i++)
		{
			CashFlowLine cl;
			cl.type = type;
			cl.digits = digit;
			cl.name = list[i];
			cl.scale = scale;
			cl.coloff = coloff;
			ci->GetResultsViewer()->AddCashFlowLine(cl);
		}
	}
}

static void fcall_cfrow(lk::invoke_t &cxt)
{
	LK_DOC("cfrow", "Add one cash flow row with one or more cell items to the current configuration. "
		"Names can be comma-separated list. "
		"For spacer use name='', for cell header use digits=-1, for cell col header use digits=-2",
		"( string:names, [number:digits], [number:scale] ):none");

	if (ResultsCallbackContext *ci = static_cast<ResultsCallbackContext*>(cxt.user_data()))
	{
		wxString name = cxt.arg(0).as_string();

		int type = CashFlowLine::CELLVARIABLE;
		int digit = 2;
		float scale = 1.0f;
		if (cxt.arg_count() == 2)
			digit = cxt.arg(1).as_integer();

		if (cxt.arg_count() == 3)
			scale = (float)cxt.arg(2).as_number();

		if (name.IsEmpty()) type = CashFlowLine::SPACER;
		if (digit == -1) type = CashFlowLine::CELLHEADER;
		if (digit == -2) type = CashFlowLine::CELLCOLHEADER;

		CashFlowLine cl;
		cl.type = type;
		cl.digits = digit;
		cl.name = name;
		cl.scale = scale;
		ci->GetResultsViewer()->AddCashFlowLine(cl);
	}
}

static void fcall_output(lk::invoke_t &cxt)
{
	LK_DOC("output", "Gets the requested output from the base case simulation for the current case.", "( string: output variable name ):none");

	if ( CaseCallbackContext *ci = static_cast<CaseCallbackContext*>(cxt.user_data()) )
		if ( VarValue *vv = ci->GetCase().BaseCase().GetOutput(cxt.arg(0).as_string()) )
			vv->Write( cxt.result() );
}

void invoke_get_var_info( Case *c, const wxString &name, lk::vardata_t &result )
{
	if (VarInfo *vi = c->Variables().Lookup( name ))
	{
		result.hash_item("label").assign( vi->Label );
		result.hash_item("units").assign( vi->Units );
		result.hash_item("group").assign( vi->Group );
	}
	else
	{
		wxArrayString names, labels, units, groups;
		Simulation::ListAllOutputs( c->GetConfiguration(),
			&names, &labels, &units, &groups );
		int idx = names.Index( name );
		if ( idx >=0 )
		{
			result.hash_item("label").assign( labels[idx] );
			result.hash_item("units").assign( units[idx] );
			result.hash_item("group").assign( groups[idx] );
		}
	}
}

static void fcall_varinfo( lk::invoke_t &cxt )
{
	LK_DOC("varinfo", "Gets meta data about an input or output variable.", "(string:var name):table");
	wxString name = cxt.arg(0).as_string();
	cxt.result().empty_hash();
	if ( CaseCallbackContext *ci = static_cast<CaseCallbackContext*>(cxt.user_data()) )
		invoke_get_var_info( &ci->GetCase(), name, cxt.result() );
}

void fcall_value( lk::invoke_t &cxt )
{
	LK_DOC("value", "Gets or sets the value of a variable by name", "(string:name [,variant:value]):[variant]");
	
	CaseCallbackContext &cc = *(CaseCallbackContext*)cxt.user_data();
	wxString name = cxt.arg(0).as_string();
	if ( VarValue *vv = cc.GetValues().Get( name ) )
	{
		if ( cxt.arg_count() == 2 )
		{
			vv->Read( cxt.arg(1) );
			cc.GetCase().VariableChanged( name );		
		}
		else
		{
			vv->Write( cxt.result() );
		}
	}
}

	
static void plottarget( UICallbackContext &cc, const wxString &name )
{
	wxLKSetPlotTarget( 0 );
	if ( wxUIObject *obj = cc.InputPage()->Find(name) )
		if ( wxPLPlotCtrl *plot = obj->GetNative<wxPLPlotCtrl>() )
			wxLKSetPlotTarget( plot );
}

void fcall_setplot( lk::invoke_t &cxt )
{
	LK_DOC("setplot", "Sets the current plot target by name", "(string:name):boolean");
	
	plottarget( *(UICallbackContext*)cxt.user_data(), cxt.arg(0).as_string() );
}

void fcall_clearplot( lk::invoke_t &cxt )
{
	LK_DOC("clearplot", "Clears the current plot, and optionally switches the plot target.", "([string:plot name]):none");
	
	if (cxt.arg_count() > 0)
		plottarget( *(UICallbackContext*)cxt.user_data(), cxt.arg(0).as_string() );

	if ( wxPLPlotCtrl *plot = wxLKGetPlotTarget() )
	{
		plot->DeleteAllPlots();
		plot->Refresh();
	}
}

void fcall_refresh( lk::invoke_t &cxt )
{
	LK_DOC("refresh", "Refresh the current form or a specific widget", "([string:name]):none" );
	
	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();
	if ( cxt.arg_count() == 0 )
		cc.InputPage()->Refresh();
	else
	{
		if ( wxUIObject *obj = cc.InputPage()->FindActiveObject( cxt.arg(0).as_string(), 0 ) )
			if ( wxWindow *win = obj->GetNative() )
				win->Refresh();
	}
}

/*
void fcall_initialize(lk::invoke_t &cxt)
{
	LK_DOC("initialize", "Initialize the current form or a specific widget", "([string:name]):none");

	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();
	cc.InputPage()->Initialize();
}
*/

void fcall_property( lk::invoke_t &cxt )
{
	LK_DOC("property", "Set or get a user interface widget property", "(string:name, string:property[, variant:value]):variant");

	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();
	ActiveInputPage *aip = 0;
	wxUIObject *obj = cc.InputPage()->FindActiveObject( cxt.arg(0).as_string(), &aip );
	if ( !obj ) return;
	if ( !obj->HasProperty( cxt.arg(1).as_string() ) ) return;

	wxUIProperty &p = obj->Property( cxt.arg(1).as_string() );

	if ( cxt.arg_count() == 2 )
	{
		switch( p.GetType() )
		{
		case wxUIProperty::BOOLEAN:
			cxt.result().assign( (double) p.GetBoolean() ? 1.0 : 0.0 );
			break;
		case wxUIProperty::DOUBLE:
			cxt.result().assign( p.GetDouble() );
			break;
		case wxUIProperty::INTEGER:
			cxt.result().assign( (double)p.GetInteger() );
			break;
		case wxUIProperty::COLOUR:
			cxt.result().empty_vector();
			cxt.result().vec_append( p.GetColour().Red() );
			cxt.result().vec_append( p.GetColour().Green() );
			cxt.result().vec_append( p.GetColour().Blue() );
			break;
		case wxUIProperty::STRING:
			cxt.result().assign( p.GetString() );
			break;
		case wxUIProperty::STRINGLIST:
		{
			cxt.result().empty_vector();
			wxArrayString list = p.GetStringList();
			for( size_t i=0;i<list.size();i++ )
				cxt.result().vec_append( list[i] );
		}
			break;
		default:
			break;
		}
	}
	else if ( cxt.arg_count() == 3 )
	{
		lk::vardata_t &val = cxt.arg(2).deref();
		switch( p.GetType() )
		{
		case wxUIProperty::BOOLEAN:
			p.Set( (bool) val.as_boolean() );
			break;
		case wxUIProperty::DOUBLE:
			p.Set( (double) val.as_number() );
			break;
		case wxUIProperty::INTEGER:
			p.Set( (int) val.as_integer() );
			break;
		case wxUIProperty::COLOUR:
			if ( val.type() == lk::vardata_t::VECTOR
				&& val.length() == 3 )
			{
				p.Set( wxColour(
					val.index(0)->as_integer(),
					val.index(1)->as_integer(),
					val.index(2)->as_integer() ) );
			}
			else
			{
				p.Set( wxColour( val.as_string() ) );
			}
			break;
		case wxUIProperty::STRING:
			p.Set( val.as_string() );
			break;
		case wxUIProperty::STRINGLIST:
			if ( val.type() == lk::vardata_t::VECTOR )
			{
				wxArrayString list;
				for( size_t i=0;i<val.length();i++ )
					list.Add( val.index(i)->as_string() );
				p.Set( list );
			}
			break;
		}

		if ( !obj->IsNativeObject() && aip != 0 )
			aip->Refresh(); // repaint the active input page if the object is non-nnative.
	}
}

void fcall_enable( lk::invoke_t &cxt )
{
	LK_DOC("enable", "Enable or disable a user interface widget", "(string:name, boolean:enable):none");

	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();
	if ( wxUIObject *obj = cc.InputPage()->FindActiveObject( cxt.arg(0).as_string(), 0 ) )
		if ( wxWindow *native = obj->GetNative() )
			native->Enable( cxt.arg(1).as_boolean() );
}
static void fcall_show( lk::invoke_t &cxt )
{
	LK_DOC("show", "Show or hide a user interface widget.", "(string:name, boolean:show):none");

	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();
	if ( wxUIObject *obj = cc.InputPage()->FindActiveObject( cxt.arg(0).as_string(), 0 ) )
		obj->Show( cxt.arg(1).as_boolean() );
}


static void fcall_case_name(lk::invoke_t &cxt)
{
	LK_DOC("case_name", "Gets the current case name.", "( none ):none");

	Case *c = SamApp::Window()->GetCurrentCase();
	wxString case_name = SamApp::Project().GetCaseName(c);

	cxt.result().assign(case_name);
}

static void fcall_copy_file(lk::invoke_t &cxt)
{
	LK_DOC("copy_file", "Copy file source to destination. Use full path for source and destination. Overwrite true overwrites destination.", "( string: source file name, string: destination file name, bool: overwrite=false ):none");

	if (!((cxt.arg_count() == 2) || (cxt.arg_count() == 3))) return;

	bool overwrite = false;
	if (cxt.arg_count() == 3) overwrite = cxt.arg(2).as_boolean();
	
	wxString source = cxt.arg(0).as_string();

	if (!wxFileExists(source)) return;

	wxString destination = cxt.arg(1).as_string();

	if (!overwrite)
	{
		wxArrayString dest_file_parts = wxStringTokenize(destination, '.');
		if (dest_file_parts.Count() < 2) return;

		wxString suffix = dest_file_parts[dest_file_parts.Count() - 1];
		wxString prefix = "";
		int i = 0;
		for (i = 0; i < (int)dest_file_parts.Count() - 1; i++)
		{
			if (i == (int)dest_file_parts.Count() - 2)
				prefix += dest_file_parts[i];
			else
				prefix += dest_file_parts[i] + ".";
		}

		i = 0;
		destination = wxString::Format("%s.%s", prefix, suffix);
		while (wxFileExists(destination))
		{
			destination = wxString::Format("%s(%d).%s", prefix, i, suffix);
			i++;
		}
	}

	wxCopyFile(source, destination);
	cxt.result().assign(destination);

}

#ifdef __WXMSW__

class lkXLObject : public lk::objref_t
{
	wxExcelAutomation m_xl;
public: 
	lkXLObject() { };
	virtual ~lkXLObject() { };
	virtual lk_string type_name() { return "ole-excel-automation"; }
	wxExcelAutomation &Excel() { return m_xl; }
	operator wxExcelAutomation&() { return m_xl; }
	wxExcelAutomation &operator*() { return m_xl; }
};

static void fcall_xl_create( lk::invoke_t &cxt )
{
	LK_DOC("xl_create", "Create a new Excel OLE automation object", "( [string: optional file to open] ):xl-object-ref" );

	lkXLObject *xl = new lkXLObject;
	xl->Excel().StartExcel();
	if ( cxt.arg_count() > 0 )
		xl->Excel().OpenFile( cxt.arg(0).as_string() );
	
	xl->Excel().Show( true );
	cxt.result().assign( cxt.env()->insert_object( xl ) );
}

static void fcall_xl_free( lk::invoke_t &cxt )
{
	LK_DOC("xl_free", "Frees up an Excel OLE automation object", "( xl-obj-ref:xl ):none" );
	
	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
	{
		xl->Excel().QuitExcel();
		cxt.env()->destroy_object( xl );
	}
}

static void fcall_xl_open(lk::invoke_t &cxt)
{
	LK_DOC("xl_open", "Opens an Excel file for automation.", "( xl-obj-ref:xl, string:file_name, [boolean:show] ):none");

	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
	{
		xl->Excel().OpenFile( cxt.arg(1).as_string() );
		
		bool show = true;
		if ( cxt.arg_count() > 2 )
			show = cxt.arg(2).as_boolean();
		if ( show )
			xl->Excel().Show( true );
	}
}

static void fcall_xl_close( lk::invoke_t &cxt )
{
	LK_DOC("xl_close", "Close the current Excel files without saving changes.", "( xl-obj-ref:xl ):none" );
	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
		cxt.result().assign( xl->Excel().CloseAllNoSave() ? 1.0 : 0.0 );
}

static void fcall_xl_autosizecols( lk::invoke_t &cxt )
{
	LK_DOC( "xl_autosizecols", "Automatically size columns in the current Excel file", "( xl-obj-ref:xl ):none" );
	
	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
		xl->Excel().AutoFitColumns();
}

static void fcall_xl_wkbook( lk::invoke_t &cxt )
{
	LK_DOC( "xl_wkbook", "Create a new empty workbook in the Excel file", "( xl-obj-ref:xl ):none" );
	
	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
		xl->Excel().NewWorkbook();
}

static void fcall_xl_sheet( lk::invoke_t &cxt )
{
	LK_DOC( "xl_sheet", "Create a new worksheet in the current workbook", "( xl-obj-ref:xl, [string: optional name] ):none");
	
	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
	{
		if ( xl->Excel().AddWorksheet() && cxt.arg_count() == 2 )
			xl->Excel().SetWorksheetName( cxt.arg(1).as_string() );
	}
}

static void fcall_xl_set( lk::invoke_t &cxt )
{
	LK_DOC("xl_set", "Sets a value in an Excel file", "( xl-obj-ref:xl, variant:value, [string:named range -or- row, col] ):none");
	
	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
	{	
		if ( cxt.arg_count() == 4 )
			xl->Excel().SetCellValue( cxt.arg(2).as_integer(), cxt.arg(3).as_integer(), cxt.arg(1).as_string() );
		else if ( cxt.arg_count() == 3 && cxt.arg(1).type() == lk::vardata_t::VECTOR )
		{
			wxArrayString list;
			for( size_t i=0;i<cxt.arg(1).length();i++ )
				list.Add( cxt.arg(1).index( i )->deref().as_string() );
			xl->Excel().SetNamedRangeArray( cxt.arg(2).as_string(), list );
		}
		else if ( cxt.arg_count() == 3 )
			xl->Excel().SetNamedRangeValue( cxt.arg(2).as_string(), cxt.arg(1).as_string() );
	}
}

static void fcall_xl_get( lk::invoke_t &cxt )
{
	LK_DOC( "xl_get", "Gets a value from the current Excel file", "( xl-obj-ref:xl, [string:named range -or- row, col] ):variant" );
	
	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
	{	
		wxString val;
		if ( cxt.arg_count() == 2 )
			ExcelExchange::ParseAndCaptureRange( cxt.arg(1).as_string(), val, xl->Excel() );
		else if ( cxt.arg_count() == 3 )
			xl->Excel().GetCellValue( cxt.arg(1).as_integer(), cxt.arg(2).as_integer(), val );

		cxt.result().assign( val );
	}
}

#endif


static bool lkvar_to_sscvar( ssc_data_t p_dat, const char *name, lk::vardata_t &val )
{	
	switch (val.type())
	{
	case lk::vardata_t::NUMBER:
		ssc_data_set_number( p_dat, name, (ssc_number_t)val.as_number() );
		break;
	case lk::vardata_t::STRING:
		ssc_data_set_string( p_dat, name, (const char*)val.as_string().c_str() );
		break;
	case lk::vardata_t::VECTOR:
		{
			size_t dim1 = val.length(), dim2 = 0;
			for (size_t i=0;i<val.length();i++)
			{
				lk::vardata_t *row = val.index(i);
				if (row->type() == lk::vardata_t::VECTOR && row->length() > dim2 )
					dim2 = row->length();
			}

			if (dim2 == 0 && dim1 > 0)
			{
				ssc_number_t *vec = new ssc_number_t[ dim1 ];
				for ( size_t i=0;i<dim1;i++)
					vec[i] = (ssc_number_t)val.index(i)->as_number();

				ssc_data_set_array( p_dat, name, vec, dim1 );
				delete [] vec;
			}
			else if ( dim1 > 0 && dim2 > 0 )
			{
				
				ssc_number_t *mat = new ssc_number_t[ dim1*dim2 ];
				for ( size_t i=0;i<dim1;i++)
				{
					for ( size_t j=0;j<dim2;j++ )
					{
						ssc_number_t x = 0;
						if ( val.index(i)->type() == lk::vardata_t::VECTOR
							&& j < val.index(i)->length() )
							x = (ssc_number_t)val.index(i)->index(j)->as_number();

						mat[ i*dim2 + j ] = x;
					}
				}

				ssc_data_set_matrix( p_dat, name, mat, dim1, dim2 );
				delete [] mat;
			}
		}
		break;
	case lk::vardata_t::HASH:		
		{
			ssc_data_t table = ssc_data_create();

			lk::varhash_t &hash = *val.hash();
			for ( lk::varhash_t::iterator it = hash.begin();
				it != hash.end();
				++it )
				lkvar_to_sscvar( table, (const char*)(*it).first.c_str(), *(*it).second );

			ssc_data_set_table( p_dat, name, table );
			ssc_data_free( table );
		}
		break;
	}

	return true;
}

static void sscvar_to_lkvar( lk::vardata_t &out, const char *name, ssc_data_t p_dat )
{
	out.nullify();

	int ty = ssc_data_query( p_dat, name );
	switch( ty )
	{
	case SSC_NUMBER:
		{
			ssc_number_t num;
			if ( ssc_data_get_number( p_dat, name, &num ) )
			out.assign( (double) num );
		}
		break;
	case SSC_STRING:
		if ( const char *ss = ssc_data_get_string( p_dat, name ) )
			out.assign( lk_string(ss) );
		break;
	case SSC_ARRAY:
	{
		int n = 0;
		ssc_number_t *vv = ssc_data_get_array( p_dat, name, &n );
		if ( vv && n > 0 )
		{
			out.empty_vector();
			out.vec()->reserve( (size_t) n );
			for (int i=0;i<n;i++)
				out.vec_append( vv[i] );
		}
	}
		break;
	case SSC_MATRIX:
	{
		int nr = 0, nc = 0;
		ssc_number_t *mat = ssc_data_get_matrix( p_dat, name, &nr, &nc );
		if ( mat && nr > 0 && nc > 0 )
		{
			out.empty_vector();
			out.vec()->reserve( nr );
			for (int i=0;i<nr;i++)
			{
				out.vec()->push_back( lk::vardata_t() );
				out.vec()->at(i).empty_vector();
				out.vec()->at(i).vec()->reserve( nc );
				for (int j=0;j<nc;j++)
					out.vec()->at(i).vec_append( mat[ i*nc +j ] );
			}
		}
	}
		break;
	case SSC_TABLE:
		if ( ssc_data_t table = ssc_data_get_table( p_dat, name ) )
		{
			out.empty_hash();
			const char *key = ::ssc_data_first( table );
			while ( key != 0 )
			{
				lk::vardata_t &xvd = out.hash_item( lk_string(key) );
				sscvar_to_lkvar( xvd, key, table );
				key = ssc_data_next( p_dat );
			}
		}
		break;
	}
}



class lkSSCdataObj : public lk::objref_t
{
	ssc_data_t m_data;
public:

	lkSSCdataObj() {
		m_data = ssc_data_create();
	}
	
	virtual ~lkSSCdataObj() {
		ssc_data_free( m_data );
	}

	virtual lk_string type_name() { return "ssc-data-object"; }
	
	operator ssc_data_t() { 
		return m_data;
	}
};

void fcall_ssc_var( lk::invoke_t &cxt )
{
	LK_DOC2( "ssc_var", "Sets or gets a variable value in the SSC data set.", 
		"Set a variable value.", "(ssc-obj-ref:data, string:name, variant:value):none", 
		"Get a variable value", "(ssc-obj-ref:data, string:name):variant" );

	if ( lkSSCdataObj *ssc = dynamic_cast<lkSSCdataObj*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
	{
		wxString name = cxt.arg(1).as_string();
		if (cxt.arg_count() == 2)		
			sscvar_to_lkvar( cxt.result(), (const char*)name.ToUTF8(), *ssc );
		else if (cxt.arg_count() == 3)
			lkvar_to_sscvar( *ssc, (const char*)name.ToUTF8(), cxt.arg(2).deref() );
	}
}

void fcall_ssc_create( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_create", "Create a new empty SSC data container object.", "(none):ssc-obj-ref" );	
	cxt.result().assign( cxt.env()->insert_object( new lkSSCdataObj ) );

}

void fcall_ssc_free( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_free", "Frees up an SSC data object.", "(ssc-obj-ref:data):none" );
	
	if ( lkSSCdataObj *ssc = dynamic_cast<lkSSCdataObj*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
		cxt.env()->destroy_object( ssc );
}

void fcall_ssc_clear( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_clear", "Clears all variables in an SSC data object", "( ssc-obj-ref:data ):none" );
	
	if ( lkSSCdataObj *ssc = dynamic_cast<lkSSCdataObj*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
		ssc_data_clear( *ssc );
}

void fcall_ssc_dump( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_dump", "Dump the contents of an SSC data object to a text file.", "(ssc-obj-ref:data, string:file):boolean" );
	if ( lkSSCdataObj *ssc = dynamic_cast<lkSSCdataObj*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
	{
		bool ok = Simulation::WriteDebugFile( cxt.arg(1).as_string(), *ssc );
		cxt.result().assign( (double)( ok ? 1.0 : 0.0 ) );
	}
	else
		cxt.result().assign( (double)0.0 );
}


static ssc_bool_t ssc_exec_handler( ssc_module_t p_mod, ssc_handler_t p_handler,
	int action_type, float f0, float f1, 
	const char *s0, const char *s1,
	void *user_data )
{
	ThreadProgressDialog *tpd = (ThreadProgressDialog*) user_data;
	if (action_type == SSC_LOG)
	{		
		switch( (int)f0 )
		{
		case SSC_NOTICE:
		case SSC_WARNING:
		case SSC_ERROR:
			tpd->Log( s0 );
			break;
		}
		
		wxGetApp().Yield( true );
		return tpd->IsCanceled() ? 0 : 1;		
	}
	else if (action_type == SSC_UPDATE)
	{
		tpd->Update( 0, f0, s0 );
		wxGetApp().Yield( true );
		return tpd->IsCanceled() ? 0 : 1;
	}
	else
		return 0;
}

void fcall_ssc_exec( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_exec", "Run a compute module with the provided data context. returns zero if successful. Options include: show_dialog, hold_dialog, debug_file, dialog_title", "( ssc-obj-ref:data, string:module, [table:options] ):variant" );
	cxt.result().assign( -999.0 );
	
	lkSSCdataObj *ssc = dynamic_cast<lkSSCdataObj*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) );
	if ( !ssc ) return;
	
	wxString cm(cxt.arg(1).as_string().Lower());

	bool show_dialog = false;
	bool hold_dialog = false;
	wxString debug_file;
	wxString dialog_title;

	if ( cxt.arg_count() > 2 && cxt.arg(2).deref().type() == lk::vardata_t::HASH )
	{
		lk::vardata_t &opts = cxt.arg(2).deref();
		if ( lk::vardata_t *o = opts.lookup("show_dialog") )
			show_dialog = o->as_boolean();

		if ( lk::vardata_t *o = opts.lookup("hold_dialog") )
			hold_dialog = o->as_boolean();

		if ( lk::vardata_t *o = opts.lookup("debug_file") )
			debug_file = o->as_string();

		if ( lk::vardata_t *o = opts.lookup("dialog_title") )
			dialog_title = o->as_string();
	}


	ThreadProgressDialog *tpd = 0;
	if ( show_dialog )
	{
		tpd = new ThreadProgressDialog( SamApp::Window(), 1, true );
		tpd->CenterOnParent();
		tpd->Show();
		tpd->Status( "Calculating...");
		tpd->ShowBars( 1 );
		wxGetApp().Yield( true );
	}

	if ( ssc_module_t mod = ssc_module_create( cm.c_str() ) )
	{
		if ( !debug_file.IsEmpty() )
			Simulation::WriteDebugFile( debug_file, mod, *ssc );

		int result = tpd != 0
			? ssc_module_exec_with_handler( mod, *ssc, ssc_exec_handler, tpd )
			: ssc_module_exec( mod, *ssc );

		if( result )
		{
			cxt.result().assign( 0.0 );
		}
		else
		{
			lk_string errors;
			int idx=0;
			int ty = 0;
			float tm = 0;
			while ( const char *msg = ssc_module_log( mod, idx++, &ty, &tm ) )
			{
				errors += lk_string(msg);
			}

			cxt.result().assign( errors );
		}

		ssc_module_free( mod );
	}

	if ( tpd != 0 )
	{
		if ( hold_dialog )
			tpd->Finalize( dialog_title ); // returns right away if no messages were displayed

		delete tpd;
	}
}

void fcall_substance_density(lk::invoke_t &cxt)
{
	LK_DOC("substance_density", "Return the density given a substance ID and temperature in C", "(variant:substanceID, variant:tempC):variant");
	size_t substanceID = cxt.arg(0).as_unsigned();
	double tempC = cxt.arg(1).as_number();
	cxt.result().assign(substance_dens(substanceID, tempC));
}

void fcall_substance_userhtf(lk::invoke_t &cxt)
{
	LK_DOC("substance_userhtf", "Return the user htf density or specific heat given a user htf matrix  and temperature in C and either density or specific heat request", "(variant:user_matrix, variant:tempC):variant, string::type(density or specific heat)");
	std::vector<lk::vardata_t> *vec = cxt.arg(0).vec();
	double tempC = cxt.arg(1).as_number();
	wxString type = cxt.arg(2).as_string();
	// form is seven columns by 2 or more rows
	int col = 1; // specific heat - can add others
	if (type.Lower() == "density")
		col = 2;
	else if (type.Lower() == "viscosity")
		col = 3;
	else if (type.Lower() == "kinematic viscosity")
		col = 4;
	else if (type.Lower() == "conductivity")
		col = 5;
	else if (type.Lower() == "enthalpy")
		col = 6;
	else
		col = 1; // specific heat default or error out

	double res = 0.0;
	size_t rows = vec->size();
//	wxLogStatus(wxString::Format("user htf, vec size: %d", rows));
	if (rows > 1)
	{
		double xmin = 0, xmax = 0, ymin = 0, ymax = 0, percent = 0;
		for (size_t i = 1; i < rows; i++)
		{
			std::vector<lk::vardata_t> *col_vec_prev = vec->at(i - 1).vec();
			std::vector<lk::vardata_t> *col_vec = vec->at(i).vec();
//			wxLogStatus(wxString::Format("user htf, prev col vec size: %d, col vec size: %d", col_vec_prev->size(), col_vec->size()));
			if ((col_vec_prev->size() != 7) || (col_vec->size() != 7)) break;

//			wxLogStatus(wxString::Format("tempC=%lg,col,prev_col_vec->at(0).as_number()=%lg,col_vec->at(0).as_number()=%lg", tempC, col_vec_prev->at(0).as_number(), col_vec->at(0).as_number()));
			if (col_vec->at(0).as_number() >= tempC)
			{
				/* interp and return */
				xmin = col_vec_prev->at(0).as_number();
				xmax = col_vec->at(0).as_number();
				ymin = col_vec_prev->at(col).as_number();
				ymax = col_vec->at(col).as_number();
				if (xmax <= xmin)
				{
					res = 1.0;
					break;
				}
				if (xmax > xmin)
					percent = (tempC - xmin) / (xmax - xmin);
				else
					percent = 1;
				res = ymin + percent*(ymax - ymin);
//				wxLogStatus(wxString::Format("xmin=%lg,xmax=%lg,ymin=%lg,ymax=%lg,percent=%lg,res=%lg", xmin, xmax, ymin, ymax, percent, res));

				break;
			}
		}
	}
	cxt.result().assign(res);
}

void fcall_substance_specific_heat(lk::invoke_t &cxt)
{
	LK_DOC("substance_specific_heat", "Return the specific heat given a substance ID and temperature in C", "(variant:substanceID, variant:tempC):variant");
	size_t substanceID = cxt.arg(0).as_unsigned();
	double tempC = cxt.arg(1).as_number();
	cxt.result().assign(substance_sph(substanceID, tempC));
}

void fcall_snlinverter( lk::invoke_t &cxt )
{
	LK_DOC( "snlinverter", "Calculates the sandia inverter AC power from DC and specs", "(number:pdc, number:vdc, number:vdco, number:pdco, number:pso, number:paco, number:c0, number:c1, number:c2, number:c3):number" );

	double pdc = cxt.arg(0).as_number();
	double vdc = cxt.arg(1).as_number();
	double vdco = cxt.arg(2).as_number();
	double pdco = cxt.arg(3).as_number();
	double pso = cxt.arg(4).as_number();
	double paco = cxt.arg(5).as_number();
	double c0 = cxt.arg(6).as_number();
	double c1 = cxt.arg(7).as_number();
	double c2 = cxt.arg(8).as_number();
	double c3 = cxt.arg(9).as_number();

	// Pac = {(Paco / (A - B)) � C � (A - B)}� (Pdc- B) + C � (Pdc - B)**2

	
	  double vdcminusvdco = vdc - vdco;
	  double A = pdco * (1 + c1 * vdcminusvdco);
	  double B = pso * (1 + c2 * vdcminusvdco);
	  // Steve Miller email 12/16/10 - check for psc < 0 see Inverters_12_23_10.docx
	  B = (B<0)? 0:B;

	  double C = c0 * (1 + c3 * vdcminusvdco);
	  double pac = ((paco / (A- B)) - C * (A - B)) * (pdc - B) + C * (pdc - B) * (pdc - B);

	  cxt.result().assign( pac );
}

void fcall_current_at_voltage_cec(lk::invoke_t &cxt)
{
	LK_DOC("current_at_voltage_cec", "Calculates the CEC module model voltage from module current and specs", "(number:Vmodule, number:IL_ref, number:IO_ref, number:RS, number:A_ref, number:RSH_ref, number:I_mp_ref):number");

	double Vmodule = cxt.arg(0).as_number();
	double IL_ref = cxt.arg(1).as_number();
	double IO_ref = cxt.arg(2).as_number();
	double RS = cxt.arg(3).as_number();
	double A_ref = cxt.arg(4).as_number();
	double RSH_ref = cxt.arg(5).as_number();
	double I_mp_ref = cxt.arg(6).as_number();

	double F = 0, Fprime = 0;
	double Iold = 0.0;
	double Inew = I_mp_ref;

	while (abs(Inew - Iold) > 1.0e-4)
	{
		Iold = Inew;

		F = IL_ref - Iold - IO_ref *
			(exp((Vmodule + Iold * RS) / A_ref) - 1.0) -
			(Vmodule + Iold * RS) / RSH_ref;

		Fprime = -1.0 - IO_ref * (RS / A_ref) *
			exp((Vmodule + Iold * RS) / A_ref) -
			(RS / RSH_ref);

		Inew = std::max(0.0, (Iold - (F / Fprime)));
	}

	cxt.result().assign(Inew);
}

void fcall_current_at_voltage_sandia(lk::invoke_t &cxt)
{
	LK_DOC("current_at_voltage_sandia", "Calculates the Sandia module model voltage from module current and specs", "(number:V, number:VmaxPow, number:ImaxPow, number:Voc, number:Isc):number");

	double V = cxt.arg(0).as_number(); 
	double VmaxPow = cxt.arg(1).as_number(); 
	double ImaxPow = cxt.arg(2).as_number();
	double Voc = cxt.arg(3).as_number();
	double Isc = cxt.arg(4).as_number();

	double Itrw = 0, C_1 = 0, C_2 = 0;
	if ((Isc > 0) && (Voc > 0)) {
		if (ImaxPow < Isc) C_2 = (VmaxPow / Voc - 1.0) / log(1.0 - ImaxPow / Isc);

		if (C_2 > 0) {
			C_1 = (1.0 - ImaxPow / Isc) * exp(-VmaxPow / C_2 / Voc);
			Itrw = Isc*(1.0 - C_1 * (exp(V / C_2 / Voc) - 1.0));
		}
		else {
			Itrw = 0.0;
		}
	}
	if (Itrw < 0) Itrw = 0;
	cxt.result().assign(Itrw);
}

void fcall_solarprospector(lk::invoke_t &cxt)
{
	LK_DOC("solarprospector", "Creates the solar prospector dialog box, downloads, decompresses, converts, and returns local file name for weather file", "(none) : string");

	//Create the solar prospector object
	SolarProspectorDialog spd( SamApp::Window(), "Download Solar Resource File");
	spd.CenterOnParent();
	int code = spd.ShowModal(); //shows the dialog and makes it so you can't interact with other parts until window is closed

	//Return an empty string if the window was dismissed
	if (code == wxID_CANCEL)
	{
		cxt.result().assign(wxEmptyString);
		return;
	}

	//Get parameters from the dialog box for weather file download
	wxString year;
	year = spd.GetYear();
	double lat, lon;
	wxString locname;
	if (spd.IsAddressMode() == true)	//entered an address instead of a lat/long
	{
		wxString addr( spd.GetAddress() );
		if (!wxSimpleCurl::GeoCode(addr, &lat, &lon))
		{
			wxMessageBox("Failed to geocode address");
			return;
		}
		
		for (int i=0;i<(int)addr.Len();i++)
			if (isalpha(addr[i]) || isdigit(addr[i]) || addr[i] == ' ' || addr[i] == '_')
				locname += addr[i];
	}
	else
	{
		lat = spd.GetLatitude();
		lon = spd.GetLongitude();	
		locname.Printf("lat%.3lf_lon%.3lf", lat, lon);	
	}

	//Create URL for weather file download
	wxString url;
	if (spd.GetYear() == "TMY" || spd.GetYear() == "TGY" || spd.GetYear() == "TDY")
	{
		url = SamApp::WebApi("prospector_typical");
		url.Replace("<TYPE>", spd.GetYear().Lower(), 1);
	}
	else
	{
		url = SamApp::WebApi("prospector_year");
		url.Replace("<YEAR>", spd.GetYear(), 1);
	}

	int short_lat = (int)(fabs(lat));
	int short_lon = (int)(fabs(lon));

	if (short_lat % 2 > 0) --short_lat;
	if (short_lon % 2 > 0) --short_lon;
	//short_lon += 2; // removed for upgrade to prospector 1998-2009

	wxString dir;
	dir.Printf("%d%d", short_lon, short_lat);

	wxString glat = wxString::Format("%d", (int)(fabs(lat) * 10)) + "5";
	wxString glon = wxString::Format("%d", (int)(fabs(lon) * 10)) + "5";

	if (glon.Len()<5) glon = "0" + glon;
	wxString gridcode = glon + glat;

	url.Replace("<DIR>", dir, 1);
	url.Replace("<GRIDCODE>", gridcode, 1);
	url.Replace("sp_data", "prospector_solar_data", 1);	//anonymizes the link in webapis.conf

	//Download the weather file
	wxSimpleCurl curl;
	curl.Start(url, true);	//true won't let it return to code unless it's done downloading
	// would like to put some code here to tell it not to download and to give an error if hits 404 Not Found

	wxString local_file;
	::wxGetTempFileName("samwf", local_file);

	if (!curl.WriteDataToFile(local_file))
	{
		wxMessageBox("Failed to download the 10 km satellite data weather file from NREL for your location.");
		return;
	}

	//Decompress weather file
	wxString dcompfile = local_file + ".extracted.tm2";
	if (!wxGunzipFile(local_file, dcompfile))
	{
		wxMessageBox("Failed to decompress downloaded weather data.");
		return;
	}

	//Convert weather file to csv instead of TMY format
	ssc_data_t data = ssc_data_create();
	ssc_data_set_string(data, "input_file", dcompfile.c_str());

	//Create a folder to put the weather file in
	wxString wfdir;
	SamApp::Settings().Read("solar_download_path", &wfdir);
	if (wfdir.IsEmpty()) wfdir = ::wxGetHomeDir() + "/SAM Downloaded Weather Files";
	if (!wxDirExists(wfdir)) wxFileName::Mkdir(wfdir, 511, ::wxPATH_MKDIR_FULL);

	wxString filename = wfdir + "/" + locname + "_" + year + ".csv";
	ssc_data_set_string(data, "output_file", filename.c_str());

	//Convert the file
	const char* err = ssc_module_exec_simple_nothread("wfcsvconv", data);
	ssc_data_free(data);
	if (err != NULL)
	{
		wxMessageBox("Failed to convert weather file to csv: \n \n " + wxString (err));
		return;
	}

	wxFileName ff(filename);
	ff.Normalize();
	wxString libkey( ff.GetName() );

	//Return the converted filename
	cxt.result().empty_hash();
	cxt.result().hash_item( "filename" ).assign( filename );
	cxt.result().hash_item( "libkey" ).assign( libkey );
}

void fcall_windtoolkit(lk::invoke_t &cxt)
{
	LK_DOC("windtoolkit", "Creates the wind data download dialog box, downloads, decompresses, converts, and returns local file name for weather file", "(none) : string");

	//Create the wind data object
	WindToolkitDialog spd(SamApp::Window(), "Download Wind Resource File");
	spd.CenterOnParent();
	int code = spd.ShowModal(); //shows the dialog and makes it so you can't interact with other parts until window is closed

	//Return an empty string if the window was dismissed
	if (code == wxID_CANCEL)
	{
		cxt.result().assign(wxEmptyString);
		return;
	}

	//Get parameters from the dialog box for weather file download
	wxString year;
	year = spd.GetYear();
	double lat, lon;
	if (spd.IsAddressMode() == true)	//entered an address instead of a lat/long
	{
		if (!wxSimpleCurl::GeoCode(spd.GetAddress(), &lat, &lon))
		{
			wxMessageBox("Failed to geocode address");
			return;
		}
	}
	else
	{
		lat = spd.GetLatitude();
		lon = spd.GetLongitude();
	}

	//Create URL for weather file download
	wxString url;
	url = SamApp::WebApi("windtoolkit");
	url.Replace("<YEAR>", spd.GetYear(), 1);
	url.Replace("<LAT>", wxString::Format(wxT("%f"),lat), 1);
	url.Replace("<LON>", wxString::Format(wxT("%f"), lon), 1);
	url.Replace("<KEY>", wxString::Format("SIWy1e6K6FHjlRnKY3RaJWJS9YMnMWqQ8mgLwLfZ")); // key for production server registered to janine.freeman@nrel.gov

	//Download the weather file
	wxSimpleCurl curl;
	curl.Start(url, true);	//true won't let it return to code unless it's done downloading
	// would like to put some code here to tell it not to download and to give an error if hits 404 Not Found

	//Create a folder to put the weather file in
	wxString wfdir;
	SamApp::Settings().Read("weather_file_dir", &wfdir);
	if (wfdir.IsEmpty()) wfdir = ::wxGetHomeDir() + "/SAM Downloaded Weather Files";
	if (!wxDirExists(wfdir)) wxFileName::Mkdir(wfdir, 511, ::wxPATH_MKDIR_FULL);

	//Create the filename
	wxString location;
	location.Printf("lat%.2lf_lon%.2lf_", lat, lon);
	location = location + year;
	wxString filename = wfdir + "/" + location + ".srw";
	
	//write data to file
	if (!curl.WriteDataToFile(filename))
	{
		wxMessageBox("Failed to download the closest WIND toolkit weather file from NREL for your location.");
		return;
	}

	//Return the downloaded filename
	cxt.result().assign(filename);
}


/********** OPENEI capability ***********/

void fcall_urdb_list_utilities(lk::invoke_t &cxt)
{
	LK_DOC("urdb_list_utilities", "Lists utility companies from the OpenEI Utility Rate Database.", "(none):string");
	wxArrayString names;
	OpenEI api;
	if (api.QueryUtilityCompanies(names))
	{
		cxt.result().empty_vector();
		for (size_t i = 0; i<names.size(); i++)
			cxt.result().vec_append(names[i]);
	}
}

void fcall_urdb_list_rates(lk::invoke_t &cxt)
{
	LK_DOC("urdb_list_rates", "Lists rates for utility argument from OpenEI Utility Rate Database.", "(string:utility):string");
	wxString utility = cxt.arg(0).as_string();

	std::vector<OpenEI::RateInfo> ratelist;
	OpenEI api;
	if (api.QueryUtilityRates(utility, ratelist))
	{
		cxt.result().empty_vector();
		for (int i = 0; i<(int)ratelist.size(); i++)
		{
			cxt.result().vec_append(ratelist[i].Name);
			cxt.result().vec_append(ratelist[i].GUID);
		}
	}
	else
		cxt.result().assign(-1);

}

static bool applydiurnalschedule(lk::invoke_t &cxt, wxString sched_name, double sched[12][24])
{
	int nr = 12, nc = 24;
	lk::vardata_t &val = cxt.result().hash_item(sched_name);
	val.empty_vector();
	val.vec()->reserve(nr);
	for (int i = 0; i<nr; i++)
	{
		val.vec()->push_back(lk::vardata_t());
		val.vec()->at(i).empty_vector();
		val.vec()->at(i).vec()->reserve(nc);
		for (int j = 0; j < nc; j++)
		{
				val.vec()->at(i).vec_append(sched[i][j]);
		}
	}
	return true;
}

void fcall_urdb_write(lk::invoke_t &cxt)
{
	LK_DOC("urdb_write", "Writes rate data from current case to a file.", "(string:filename):boolean");
	
	Case *c = SamApp::Window()->GetCurrentCase();
	if ( !c ) return;

	ConfigInfo *ci = c->GetConfiguration();
	if ( !ci ) return;

	wxCSVData csv;
	int row = 0;
	for (VarInfoLookup::iterator it = ci->Variables.begin();
		it != ci->Variables.end();	++it)
	{
		VarInfo &vi = *(it->second);
//		if (vi.Flags & VF_CALCULATED || vi.Flags & VF_INDICATOR) continue;
		if (vi.Group == "Utility Rate")

		{
			wxString var_name = it->first;
			// get value
			if (VarValue *vv = c->Values().Get(var_name))
			{
				// write out csv with first row var name and second row var values
				wxString value = vv->AsString();
				value.Replace("\n", ";;");
				csv.Set(row, 0, var_name);
				csv.Set(row, 1, value);
				csv.Set(row, 2, vi.Label);
				row++;
			}
		}
	}
	
	cxt.result().assign( csv.WriteFile( cxt.arg(0).as_string() ) ? 1.0 : 0.0 );
}

void fcall_urdb_read(lk::invoke_t &cxt)
{
	LK_DOC("urdb_read", "Reads rate data from a file case to the current case.", "(string:filename):boolean");
	
	Case *c = SamApp::Window()->GetCurrentCase();
	if ( !c ) return;

	wxCSVData csv;
	int row = 0;
	bool ret_val = csv.ReadFile( cxt.arg(0).as_string() );
	if (ret_val)
	{
		wxArrayString errors;
		wxArrayString list;
		for (row = 0; row < (int)csv.NumRows(); row++)
		{
			wxString var_name = csv.Get(row,0);
			// get value
			if (VarValue *vv = c->Values().Get(var_name))
			{
				wxString value = csv.Get(row,1);
				value.Replace(";;", "\n");
				if ( !VarValue::Parse(vv->Type(), value, *vv) )
				{
					errors.Add("Problem assigning " + var_name + " to " + value );
					ret_val = false;
				}
				else
					list.Add( var_name );
			}
		}
		// this causes the UI and other variables to be updated
		c->VariablesChanged( list );
	}

	cxt.result().assign( ret_val ? 1.0 : 0.0 );
}



void fcall_urdb_get(lk::invoke_t &cxt)
{
	LK_DOC("urdb_get", "Returns data for the specified rate schedule from the OpenEI Utility Rate Database.", "(string:guid):boolean");	
	wxString guid = cxt.arg(0).as_string();
	if (guid.IsEmpty()) return;

	OpenEI::RateData rate;
	OpenEI api;

	if (api.RetrieveUtilityRateData(guid, rate))
	{
		cxt.result().empty_hash();

		// meta data
		cxt.result().hash_item("name").assign(rate.Header.Utility);
		cxt.result().hash_item("schedule_name").assign(rate.Header.Name);
		cxt.result().hash_item("source").assign(rate.Header.Source);
		cxt.result().hash_item("description").assign(rate.Header.Description);
		cxt.result().hash_item("basicinformationcomments").assign(rate.Header.BasicInformationComments);
		cxt.result().hash_item("energycomments").assign(rate.Header.EnergyComments);
		cxt.result().hash_item("demandcomments").assign(rate.Header.DemandComments);
		
		// applicability
		cxt.result().hash_item("peakkwcapacityhistory").assign(rate.Applicability.peakkwcapacityhistory);
		cxt.result().hash_item("peakkwcapacitymax").assign(rate.Applicability.peakkwcapacitymax);
		cxt.result().hash_item("peakkwcapacitymin").assign(rate.Applicability.peakkwcapacitymin);
		cxt.result().hash_item("peakkwhusagehistory").assign(rate.Applicability.peakkwhusagehistory);
		cxt.result().hash_item("peakkwhusagemax").assign(rate.Applicability.peakkwhusagemax);
		cxt.result().hash_item("peakkwhusagemin").assign(rate.Applicability.peakkwhusagemin);
		cxt.result().hash_item("voltagemaximum").assign(rate.Applicability.voltagemaximum);
		cxt.result().hash_item("voltageminimum").assign(rate.Applicability.voltageminimum);
		cxt.result().hash_item("voltagecategory").assign(rate.Applicability.voltagecategory);
		cxt.result().hash_item("phasewiring").assign(rate.Applicability.phasewiring);
		
		// URLs
		cxt.result().hash_item("rateurl").assign(rate.Header.RateURL);
		cxt.result().hash_item("jsonurl").assign(rate.Header.JSONURL);

		// net metering
		if (rate.NetMetering)
			cxt.result().hash_item("enable_net_metering").assign(1.0);
		else
			cxt.result().hash_item("enable_net_metering").assign(0.0);

		// fixed charges
		cxt.result().hash_item("monthly_fixed_charge").assign(rate.FixedMonthlyCharge);
		cxt.result().hash_item("monthly_min_charge").assign(rate.MinMonthlyCharge);
		cxt.result().hash_item("annual_min_charge").assign(rate.MinAnnualCharge);

		// schedules
		if (!applydiurnalschedule(cxt, "ec_sched_weekday", rate.EnergyWeekdaySchedule)) return;
		if (!applydiurnalschedule(cxt, "ec_sched_weekend", rate.EnergyWeekendSchedule)) return;

		if (!applydiurnalschedule(cxt, "dc_sched_weekday", rate.DemandWeekdaySchedule)) return;
		if (!applydiurnalschedule(cxt, "dc_sched_weekend", rate.DemandWeekendSchedule)) return;

		// energy rate structure, e.g. "ur_ec_p1_t1_ub"
		bool ec_enable = false;
		for (int period = 0; period < 12; period++)
		{
			for (int tier = 0; tier < 6; tier++)
			{
				wxString period_tier = wxString::Format("ur_ec_p%d_t%d_",period+1,tier+1);
				cxt.result().hash_item(period_tier + "ub").assign(rate.EnergyMax[period][tier]);
				double buy_rate = rate.EnergyBuy[period][tier] + rate.EnergyAdj[period][tier];
				if (!ec_enable && (buy_rate != 0)) ec_enable = true;
				cxt.result().hash_item(period_tier + "br").assign(buy_rate);
				cxt.result().hash_item(period_tier + "sr").assign(rate.EnergySell[period][tier]);
				// todo - handle different energy upper bound units
			}
		}
		if (ec_enable)
			cxt.result().hash_item("ec_enable").assign(1.0);
		else
			cxt.result().hash_item("ec_enable").assign(0.0);

		//flat demand structure, e.g. ur_dc_jan_t1_ub
		wxString months[] = { "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec" };

		bool dc_enable = false;
		for (int month = 0; month < 12; month++)
		{
			for (int tier = 0; tier < 6; tier++)
			{
				wxString period_tier = wxString::Format("ur_dc_%s_t%d_", months[month], tier + 1);
				cxt.result().hash_item(period_tier + "ub").assign(rate.FlatDemandMax[rate.FlatDemandMonth[month]][tier]);
				double charge = rate.FlatDemandCharge[rate.FlatDemandMonth[month]][tier] + rate.FlatDemandAdj[rate.FlatDemandMonth[month]][tier];
				if (!dc_enable && (charge != 0)) dc_enable = true;
				cxt.result().hash_item(period_tier + "dc").assign(charge);
			}
		}


		// demand rate structure, e.g. ur_dc_p1_t1_ub
		for (int period = 0; period < 12; period++)
		{
			for (int tier = 0; tier < 6; tier++)
			{
				wxString period_tier = wxString::Format("ur_dc_p%d_t%d_", period + 1, tier + 1);
				cxt.result().hash_item(period_tier + "ub").assign(rate.DemandMax[period][tier]);
				double charge = rate.DemandCharge[period][tier] + rate.DemandAdj[period][tier];
				if (!dc_enable && (charge != 0)) dc_enable = true;
				cxt.result().hash_item(period_tier + "dc").assign(charge);
			}
		}
		if (dc_enable)
			cxt.result().hash_item("dc_enable").assign(1.0);
		else
			cxt.result().hash_item("dc_enable").assign(0.0);

	}
}

void fcall_openeiutilityrateform(lk::invoke_t &cxt)
{
	LK_DOC("openeiutilityrateform", "Returns a list of utility company names from OpenEI.org", "( NONE ):ARRAY");
	CaseCallbackContext &cc = *static_cast<CaseCallbackContext*>(cxt.user_data());
	wxString market = cc.GetCase().GetFinancing();

	OpenEIUtilityRateDialog openei(SamApp::Window(), "URDB", market);
	openei.CenterOnParent();
	int code = openei.ShowModal(); //shows the dialog and makes it so you can't interact with other parts until window is closed

	//Return an empty string if the window was dismissed
	if (code == wxID_CANCEL)
	{
		cxt.result().assign(wxEmptyString);
		return;
	}

	// interact with data returned and apply to current case
	cxt.result().assign(openei.GetCurrentRateData().Header.GUID);
}

static void copy_mxh( lk::vardata_t &val, matrix_t<float> &mxh )
{
	if ( mxh.nrows() == 12 && mxh.ncols() == 24 )
	{
		val.empty_vector();
		val.resize( 12 );
		for( size_t r=0;r<12;r++ )
		{
			lk::vardata_t *row = val.index( r );
			row->empty_vector();
			row->resize(24);
			for( size_t c=0;c<24;c++ )
				row->index(c)->assign( mxh(r,c) );
		}
	}
}

void fcall_editscene3d( lk::invoke_t &cxt )
{
	LK_DOC( "editscene3d", "Loads the 3D scene editor for a given 3D scene variable name.", "(string:variable, number:lat, number:lon, number:tz, string:location):table");
	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();
	
	cxt.result().empty_hash();

	wxString name( cxt.arg(0).as_string() );
	VarValue *vv = cc.GetCase().Values().Get( name );
	if ( !vv )
	{
		cxt.result().hash_item("ierr").assign( 1.0 );
		cxt.result().hash_item("message").assign( wxString("no variable with that name") );
		return;
	}
	

	if ( VV_BINARY != vv->Type() )
		vv->SetType( VV_BINARY );
	wxMemoryBuffer &bin = vv->Binary();

	wxLogStatus("EDIT SCENE (%s): loaded %d bytes", (const char*)name.c_str(), (int)bin.GetDataLen() );

	wxDialog dlg( SamApp::Window(), wxID_ANY, "Edit 3D Shading Scene", wxDefaultPosition, wxSize(800,600), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER );
	ShadeTool *st = new ShadeTool( &dlg, wxID_ANY );

	if ( cxt.arg_count() > 1 && bin.GetDataLen() == 0 )
	{
		// only update location on the first time
		double lat = cxt.arg(1).as_number();
		double lon = cxt.arg(2).as_number();
		double tz = cxt.arg(3).as_number();
		wxString addr = cxt.arg(4).as_string();
		st->GetLocationSetup()->SetLocation( addr, lat, lon, tz );

		wxMessageBox("The 3D scene requires detailed information about your location to calculate shading losses.\n\n"
			"By default, information about the location you selected in the weather file has been transferred.\n\n"
			"If you update your weather file in the future, please manually ensure that the address, "
			"latitude, longitude, and time zone in the 3D scene editor (Location tab) are updated as necessary.", "Notice", 
			wxICON_INFORMATION|wxOK, SamApp::Window() );
	}

	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( st, 1, wxALL|wxEXPAND, 0 );
	dlg.SetSizer( sizer );
	if (  bin.GetDataLen() > 0 )
	{
		wxMemoryInputStream in( bin.GetData(), bin.GetDataLen() );
		if (!st->Read( in ))
			wxMessageBox("Error loading stored 3D scene data.");
	}

	double lat, lon, tz;
	st->GetLocationSetup()->GetLocation( &lat, &lon, &tz );
	if ( lat != cxt.arg(1).as_number()
		|| lon != cxt.arg(2).as_number()
		|| tz != cxt.arg(3).as_number() )
	{
		if( wxYES==wxMessageBox( "The location information in the shading tool does not match the currently selected weather file.\n\nDo you want to update your location settings in the shading tool to match?", "Query", wxYES_NO ) )
		{
			lat = cxt.arg(1).as_number();
			lon = cxt.arg(2).as_number();
			tz = cxt.arg(3).as_number();
			wxString addr = cxt.arg(4).as_string();
			st->GetLocationSetup()->SetLocation( addr, lat, lon, tz );
		}
	}

	dlg.ShowModal();
	wxMemoryOutputStream out;
	st->Write( out );
	size_t len = out.GetSize();
	if ( len > 0 )
	{
		void *buf = bin.GetWriteBuf( len );
		out.CopyTo( buf, len );
		bin.UngetWriteBuf( len );
		SamApp::Window()->Project().SetModified( true );
	}
	
	std::vector<ShadeTool::diurnal> diurnal;
	if ( st->SimulateDiurnal(diurnal) && diurnal.size() > 0 )
	{
		// overall losses for the system are always in table 0
		lk::vardata_t &v = cxt.result().hash_item( "losses" );
		copy_mxh( v, diurnal[0].mxh );
		
		// now copy over all the subarray sections
		// the user must label them as 'Subarray 1', 'Subarray 2', etc for them to
		// get placed in the right section
		// here, skip table 0 (loop start at i=1) since it's always copied above
		for( size_t i=1;i<diurnal.size();i++ )
		{
			if ( diurnal[i].mxh.nrows() == 12 && diurnal[i].mxh.ncols() == 24 )
			{
				wxString name = diurnal[i].name.Lower();
				name.Replace(" ", "");
				if ( name.IsEmpty() )
					name.Printf("section%d", (int)i);

				lk::vardata_t &sec = cxt.result().hash_item( name );
				copy_mxh( sec, diurnal[i].mxh );
			}
		}

		cxt.result().hash_item("ierr").assign( 0.0 );
		cxt.result().hash_item("nsubarrays").assign( (double)( diurnal.size()-1 ));
	}
	else
	{
		cxt.result().hash_item("ierr").assign( 2.0 );
		cxt.result().hash_item("message").assign( wxString("Error in simulation of diurnal shading factors.") );
	}
}


void fcall_showsettings( lk::invoke_t &cxt )
{
	LK_DOC("showsettings", "Show the settings dialog for either 'solar' or 'wind' data files.", "(string:type):boolean");
	wxString type( cxt.arg(0).as_string().Lower() );
	if ( type == "solar" ) cxt.result().assign( ShowSolarResourceDataSettings() ? 1.0 : 0.0 );
	else if ( type == "wind" ) cxt.result().assign( ShowWindResourceDataSettings() ? 1.0 : 0.0 );
}

void fcall_rescanlibrary( lk::invoke_t &cxt )
{
	LK_DOC("rescanlibrary", "Rescan the indicated resource data library ('solar' or 'wind') and update any library widgets.", "(string:type):boolean");
	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();

	wxString type(cxt.arg(0).as_string().Lower());
	wxBusyInfo info("Scanning folders for " + type + " data files..." );

	Library *reloaded = 0;

	if ( type == "solar" )
	{
		wxString solar_resource_db = SamApp::GetUserLocalDataDir() + "/SolarResourceData.csv";
		ScanSolarResourceData( solar_resource_db );
		reloaded = Library::Load( solar_resource_db );
	}
	else if ( type == "wind" )
	{
		wxString wind_resource_db  = SamApp::GetUserLocalDataDir() + "/WindResourceData.csv";
		ScanWindResourceData( wind_resource_db );
		reloaded = Library::Load( wind_resource_db );
	}

	if ( reloaded != 0 )
	{
		std::vector<wxUIObject*> objs = cc.InputPage()->GetObjects();
		for( size_t i=0;i<objs.size();i++ )
			if ( LibraryCtrl *lc = objs[i]->GetNative<LibraryCtrl>() )
				lc->ReloadLibrary();
	}
}

lk::fcall_t* invoke_general_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_logmsg,
		fcall_browse,
		fcall_webapi,
		fcall_geocode,
		fcall_curl,
		fcall_appdir,
		fcall_runtimedir,
		fcall_userlocaldatadir,
		fcall_copy_file,
		fcall_case_name,
		fcall_dview,
		fcall_dview_solar_data_file,
#ifdef __WXMSW__
		fcall_xl_create,
		fcall_xl_free,
		fcall_xl_open,
		fcall_xl_close,
		fcall_xl_wkbook,
		fcall_xl_sheet,
		fcall_xl_set,
		fcall_xl_get,
		fcall_xl_autosizecols,
#endif
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_ssc_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_ssc_create,
		fcall_ssc_free,
		fcall_ssc_clear,
		fcall_ssc_dump,
		fcall_ssc_var,
		fcall_ssc_exec,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_config_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_addconfig,
		fcall_setconfig,
		fcall_configopt,
		fcall_addpage,
		fcall_setting,
		fcall_setmodules,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_equation_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_substance_density,
		fcall_substance_userhtf,
		fcall_substance_specific_heat,
		fcall_snlinverter,
		fcall_current_at_voltage_cec,
		fcall_current_at_voltage_sandia,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_casecallback_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_value,
		fcall_varinfo,
		fcall_output,
		fcall_technology,
		fcall_financing,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_resultscallback_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_metric,
		fcall_cfline,
		fcall_cfrow,
		fcall_agraph,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_lossdiag_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_new_baseline,
		fcall_add_loss_term,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_uicallback_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_setplot,
		fcall_clearplot,
		fcall_enable,
		fcall_show,
		fcall_property,
		fcall_refresh,
		fcall_substance_density,
		fcall_substance_userhtf,
		fcall_substance_specific_heat,
		fcall_snlinverter,
		fcall_current_at_voltage_cec,
		fcall_current_at_voltage_sandia,
		fcall_solarprospector,
		fcall_windtoolkit,
		fcall_openeiutilityrateform,
		fcall_urdb_read,
		fcall_urdb_write,
		fcall_urdb_get,
		fcall_urdb_list_utilities,
		fcall_urdb_list_rates,
		fcall_editscene3d,
		fcall_showsettings,
		fcall_rescanlibrary,
		0 };
	return (lk::fcall_t*)vec;
}


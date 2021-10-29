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

#include <algorithm>
#include <memory>
#include <fstream>

// threading
#include <thread>
#include <future>
#include <chrono>

#include <lk/parse.h>
#include <lk/codegen.h>
#include <lk/env.h>
#include <lk/stdlib.h>


#include <wx/log.h>
#include <wx/tokenzr.h>
#include <wx/filename.h>
#include <wx/buffer.h>
#include <wx/mstream.h>
#include <wx/busyinfo.h>
#include <wx/arrstr.h>

#include <wex/plot/plplotctrl.h>
#include <wex/lkscript.h>
#include <wex/dview/dvplotctrl.h>
#include <wex/utils.h>
#include <wex/csv.h>
#include <wex/easycurl.h>

#ifdef __WXMSW__
#include <wex/ole/excelauto.h>
#endif

#include <ssc/sscapi.h>
#include <ssc/ssc_equations.h>
#include <json/json.h>

#include "main.h"
#include "case.h"
#include "simulation.h"
#include "casewin.h"
#include "materials.h"
#include "results.h"
#include "windtoolkit.h"
#include "urdb.h"
#include "macro.h"
#include "invoke.h"
#include "s3tool.h"
#include "lossdiag.h"
#include "stochastic.h"
#include "codegencallback.h"
#include "nsrdb.h"
#include "combinecases.h"
#include "wavetoolkit.h"
#include "graph.h"

std::mutex global_mu;

void fcall_samver( lk::invoke_t &cxt )
{
	LK_DOC( "samver", "Returns current SAM version as a string.", "(none):string" );
	cxt.result().assign( SamApp::VersionStr( true, true ) );
}


static void fcall_dview(lk::invoke_t &cxt)
{
	LK_DOC("dview", "Creates a separate dview viewer for viewing specified data.", "( number:num_datasets, number:timestep, string:window name, string:data_name1, string:data_units1, bool:select_dataset, variant:data1, [ string:data_name2, string:data_units2, bool:select_dataset2, variant:data2], ...):none");


	int num_datasets = (int)cxt.arg(0).as_number();
	double timestep = cxt.arg(1).as_number(); // fraction of hour
	wxString win_name = cxt.arg(2).as_string();

	size_t ndx = 3;
	if ((4 * num_datasets + ndx) != cxt.arg_count())
	{
		cxt.error("Incorrect number of arguments");
		return;
	}

	wxFrame *frame = new wxFrame( SamApp::Window(), wxID_ANY, "Data Viewer: " + win_name, wxDefaultPosition, wxScaleSize(1000,700),
		(wxCAPTION | wxCLOSE_BOX | wxCLIP_CHILDREN | wxRESIZE_BORDER ) );
#ifdef __WXMSW__
	frame->SetIcon( wxICON( appicon ) );
#endif

	wxDVPlotCtrl *dview = new wxDVPlotCtrl(frame, wxID_ANY);

	size_t data_index = 0;
	while (ndx < cxt.arg_count())
	{
		wxString data_name = cxt.arg(ndx++).as_string();
		wxString data_units = cxt.arg(ndx++).as_string();
		double select_dataset = cxt.arg(ndx++).as_boolean();
		lk::vardata_t &data = cxt.arg(ndx++);
		if (data.length() == 0)
		{
			cxt.error("Data stream" + data_name + "has length zero.");
			return;
		}

		std::vector<double> plot_data( data.length() );
		for (size_t i = 0; i < data.length(); i++)
			plot_data[i] = data.index(i)->as_number();

		wxDVArrayDataSet *dvset = new wxDVArrayDataSet(data_name, data_units, timestep, plot_data);
		dvset->SetGroupName( win_name );
		dview->AddDataSet( dvset );
		if (select_dataset)
			dview->SelectDataIndex(data_index);
		data_index++;
	}

	dview->GetStatisticsTable()->RebuildDataViewCtrl();
	dview->DisplayTabs();

	frame->Show();
}

struct wfvec {
	char const *name;
	char const *label;
	char const *units;
};

static void fcall_dview_wave_data_file( lk::invoke_t &cxt )
{
	LK_DOC("dview_wave", "Read a solar weather data file on disk (*.csv) and popup a frame with a data viewer.", "(string:filename):boolean");

	wxString file( cxt.arg(0).as_string() );
	if ( !wxFileExists( file ) ) {
		cxt.result().assign( 0.0 );
		return;
	}

	ssc_data_t pdata = ssc_data_create();
    ssc_data_set_number(pdata, "wave_resource_model_choice", 1);
    ssc_data_set_string(pdata, "wave_resource_filename_ts", (const char*)file.c_str());

	if ( const char *err = ssc_module_exec_simple_nothread( "wave_file_reader", pdata ) )
	{
		wxLogStatus("error scanning '" + file + "'");
		cxt.error(err);
		cxt.result().assign(0.0);
		return;
	}

	wxFrame *frame = new wxFrame( SamApp::Window(), wxID_ANY, "Data Viewer: " + file, wxDefaultPosition, wxScaleSize(1000,700),
		(wxCAPTION | wxCLOSE_BOX | wxCLIP_CHILDREN | wxRESIZE_BORDER ) );
#ifdef __WXMSW__
	frame->SetIcon( wxICON( appicon ) );
#endif

	wxDVPlotCtrl *dview = new wxDVPlotCtrl(frame, wxID_ANY);

	// this information is consistent with the variable definitions in the wfreader module
	wfvec vars[] = {
		{ "significant_wave_height", "Significant wave height", "m" },
		{ "energy_period","Wave energy period", "s" },
		{ 0, 0, 0 } };

    ssc_number_t start = 0;
    ssc_number_t step = 3600 * 3; // start & step in seconds, then convert to hours
	start /= 3600;
	step /= 3600;

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

			wxDVArrayDataSet *dvset = new wxDVArrayDataSet( vars[i].label, vars[i].units, start, step, plot_data );
			dvset->SetGroupName( wxFileNameFromPath( file ) );
			dview->AddDataSet( dvset );
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

static void fcall_dview_solar_data_file(lk::invoke_t& cxt)
{
    LK_DOC("dview_solar", "Read a solar weather data file on disk (*.csv,*.tm2,*.tm3,*.epw,*.smw) and popup a frame with a data viewer.", "(string:filename):boolean");

    wxString file(cxt.arg(0).as_string());
    if (!wxFileExists(file)) {
        cxt.result().assign(0.0);
        return;
    }


    ssc_data_t pdata = ssc_data_create();
    ssc_data_set_string(pdata, "file_name", (const char*)file.c_str());
    ssc_data_set_number(pdata, "header_only", 0);

    if (const char* err = ssc_module_exec_simple_nothread("wfreader", pdata))
    {
        wxLogStatus("error scanning '" + file + "'");
        cxt.error(err);
        cxt.result().assign(0.0);
        return;
    }

    wxFrame* frame = new wxFrame(SamApp::Window(), wxID_ANY, "Data Viewer: " + file, wxDefaultPosition, wxScaleSize(1000, 700),
        (wxCAPTION | wxCLOSE_BOX | wxCLIP_CHILDREN | wxRESIZE_BORDER));
#ifdef __WXMSW__
    frame->SetIcon(wxICON(appicon));
#endif

    wxDVPlotCtrl* dview = new wxDVPlotCtrl(frame, wxID_ANY);

    // this information is consistent with the variable definitions in the wfreader module
    wfvec vars[] = {
        { "beam", "Beam irradiance - DNI", "W/m2" },
        { "diff","Diffuse irradiance - DHI", "W/m2" },
        { "glob", "Global irradiance - GHI", "W/m2" },
        { "poa", "Plane of array irradiance -POA", "W/m2" },
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
    ssc_data_get_number(pdata, "start", &start); start /= 3600;
    ssc_data_get_number(pdata, "step", &step); step /= 3600;

    size_t i = 0;
    while (vars[i].name != 0)
    {
        int len;
        ssc_number_t* p = ssc_data_get_array(pdata, vars[i].name, &len);
        if (p != 0 && len > 2)
        {
            std::vector<double> plot_data(len);
            for (int j = 0; j < len; j++)
                plot_data[j] = p[j];

            wxDVArrayDataSet* dvset = new wxDVArrayDataSet(vars[i].label, vars[i].units, start, step, plot_data);
            dvset->SetGroupName(wxFileNameFromPath(file));
            dview->AddDataSet(dvset);
        }

        i++;
    }

    ssc_data_free(pdata);

    dview->GetStatisticsTable()->RebuildDataViewCtrl();
    if (i > 0)
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

static void fcall_webapi( lk::invoke_t &cxt )
{
	LK_DOC( "webapi", "Returns the URL for the SAM web API requested.  No arguments returns the list of options: windtoolkit, biomass_resource, energy_crop, nsrdb_query, android_build, ios_build, website, ...", "( [string:name] ):string");
	cxt.result().assign( SamApp::WebApi( cxt.arg_count() > 0 ? cxt.arg(0).as_string() : wxEmptyString ) );
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
	if (lk::vardata_t *vv = tab.lookup("description"))
		opt.Description = vv->as_string();
	if (lk::vardata_t *vv = tab.lookup("tree_parent"))
		opt.TreeParent = vv->as_string();

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
	bool exclusive_tabs = false;
    bool exclusive_radio = false;
    bool exclusive_hide = false;
	std::vector<PageInfo> excl_header_pages;
    std::vector<PageInfo> excl_footer_pages;

	if ( cxt.arg_count() > 1 )
	{
		lk::vardata_t &props = cxt.arg(1).deref();

		if( lk::vardata_t *x = props.lookup("sidebar") )
			sidebar = x->as_string();

		if ( lk::vardata_t *x = props.lookup("help") )
			help = x->as_string();

		if ( lk::vardata_t *x = props.lookup("exclusive_var") )
			exclusive_var = x->as_string();

		if ( lk::vardata_t *x = props.lookup("exclusive_tabs") )
			exclusive_tabs = x->as_boolean();

        if (lk::vardata_t* x = props.lookup("exclusive_radio"))
            exclusive_radio = x->as_boolean();

        if (lk::vardata_t* x = props.lookup("exclusive_hide"))
            exclusive_hide = x->as_boolean();

		if ( lk::vardata_t *x = props.lookup("exclusive_header_pages") )
		{
			lk::vardata_t &vec = x->deref();
			if ( vec.type() == lk::vardata_t::VECTOR )
			{
				for( size_t i=0;i<vec.length();i++ )
				{
					PageInfo pi;
					pi.Name = vec.index(i)->as_string();
					pi.Caption = pi.Name;
					excl_header_pages.push_back( pi );
				}
			}

		}

        if (lk::vardata_t* x = props.lookup("exclusive_footer_pages"))
        {
            lk::vardata_t& vec = x->deref();
            if (vec.type() == lk::vardata_t::VECTOR)
            {
                for (size_t i = 0; i < vec.length(); i++)
                {
                    PageInfo pi;
                    pi.Name = vec.index(i)->as_string();
                    pi.Caption = pi.Name;
                    excl_footer_pages.push_back(pi);
                }
            }

        }
	}
	SamApp::Config().AddInputPageGroup( pages, sidebar, help, exclusive_var, excl_header_pages, exclusive_tabs, exclusive_hide);
}


static void fcall_getsettings(lk::invoke_t &cxt)
{
	LK_DOC("get_settings", "Gets a setting field for the current project", "(string:name):string:value");

	wxString str = cxt.arg(0).as_string();
	wxString buf = "";
	SamApp::Settings().Read(str, &buf);
	cxt.result().assign(buf);

	/*
		wxString buf="";
		SamApp::Settings().Read("solar_data_paths", &buf);
		cxt.result().assign(buf);
    */
}

static void fcall_setsettings(lk::invoke_t &cxt)
{
	LK_DOC("set_settings", "Sets a setting field for the current project", "(string:name, string:value):none");

	wxString str = cxt.arg(0).as_string();
	wxString buf = cxt.arg(1).as_string();
	SamApp::Settings().Write(str, buf);

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

static void fcall_codegen_metric(lk::invoke_t &cxt)
{
	LK_DOC("metric", "Add an output metric to the current configuration. Options include mode,deci,thousep,pre,post,label,scale", "(string:variable, [table:options]):none");

	if (CodeGenCallbackContext *ci = static_cast<CodeGenCallbackContext*>(cxt.user_data()))
	{
		CodeGenData md;
		md.var = cxt.arg(0).as_string();

		if (cxt.arg_count() > 1)
		{
			lk::vardata_t &opts = cxt.arg(1).deref();
			if (lk::vardata_t *x = opts.lookup("mode"))
			{
				wxString mm = x->as_string();
				mm.MakeLower();
				if (mm == "f") md.mode = 'f';
				else if (mm == "e") md.mode = 'e';
				else if (mm == "h") md.mode = 'h';
			}

			if (lk::vardata_t *x = opts.lookup("deci"))
				md.deci = x->as_integer();

			if (lk::vardata_t *x = opts.lookup("thousep"))
				md.thousep = x->as_boolean();

			if (lk::vardata_t *x = opts.lookup("pre"))
				md.pre = x->as_string();

			if (lk::vardata_t *x = opts.lookup("post"))
				md.post = x->as_string();

			if (lk::vardata_t *x = opts.lookup("label"))
				md.label = x->as_string();

			if (lk::vardata_t *x = opts.lookup("scale"))
				md.scale = x->as_number();
		}

		ci->GetCodeGen_Base()->AddData(md);
	}
}

static void fcall_metric_table(lk::invoke_t& cxt)
{
	LK_DOC("metric_table", "Add an output metric table to the current configuration. Options include headers", "(string:tableName, [table:options]):none");

	if (ResultsCallbackContext* ci = static_cast<ResultsCallbackContext*>(cxt.user_data()))
	{
		MetricTable mt;
		mt.tableName = cxt.arg(0).as_string().MakeLower();

		if (cxt.arg_count() > 1)
		{
			lk::vardata_t& opts = cxt.arg(1).deref();
			if (lk::vardata_t* x = opts.lookup("headers"))
			{
				wxString ch = x->as_string();
				mt.headers = wxSplit(ch, ',');
			}
		}
		ci->GetResultsViewer()->AddMetricTable(mt);
	}
}

static void fcall_metric_row(lk::invoke_t& cxt)
{
	LK_DOC("metric_row", "Add an output metric row to the current configuration. Options include mode(s),deci(s),thousep(s),pre(s),post(s),label,scale(s)", "(string:variable(s)), [table:options]):none");

	if (ResultsCallbackContext* ci = static_cast<ResultsCallbackContext*>(cxt.user_data()))
	{
		MetricRow mr;
		wxArrayString vars = wxSplit(cxt.arg(0).as_string(), ',');
		wxArrayString modes, decis, thouseps, pres, posts, scales;

		if (cxt.arg_count() > 1)
		{
			lk::vardata_t& opts = cxt.arg(1).deref();
			if (lk::vardata_t* x = opts.lookup("mode")) {
				modes = wxSplit(x->as_string(), ',');
			}

			if (lk::vardata_t* x = opts.lookup("deci")) {
				decis = wxSplit(x->as_string(), ',');
			}

			if (lk::vardata_t* x = opts.lookup("thousep")) {
				thouseps = wxSplit(x->as_string(), ',');
			}

			if (lk::vardata_t* x = opts.lookup("pre"))
				pres = wxSplit(x->as_string(), ',');

			if (lk::vardata_t* x = opts.lookup("post"))
				posts = wxSplit(x->as_string(), ',');

			if (lk::vardata_t* x = opts.lookup("scale")) {
				scales = wxSplit(x->as_string(), ',');
			}


			if (lk::vardata_t* x = opts.lookup("label"))
				mr.label = x->as_string();

			if (lk::vardata_t* x = opts.lookup("tableName"))
				mr.tableName = x->as_string().MakeLower();
		}


		for (size_t i = 0; i < vars.GetCount(); i++) {
			MetricData md;
			md.var = vars[i];

			if (i < modes.GetCount())
			{
				wxString mm = modes[i];
				mm.MakeLower();
				if (mm == "f") md.mode ='f';
				else if (mm == "e")md.mode = 'e';
				else if (mm == "h") md.mode = 'h';
			}

			if (i <decis.GetCount()) {
				int deci = wxAtoi(decis[i]);
				md.deci = deci;
			}

			if ( i < thouseps.GetCount()) {
				wxString mm = thouseps[i];
				mm.MakeLower();
				if (mm == "f") md.thousep = false;
				else md.thousep = true;
			}

			if (i < pres.GetCount()) {
				md.pre = pres[i];
			}

			if (i < posts.GetCount()) {
				md.post = posts[i];
			}

			if (i < scales.GetCount())	{
				double scale = wxAtof(scales[i]);
				md.scale = scale;
			}

			mr.metrics.push_back(md);
		}

/*		MetricRow mr;
		mr.vars = wxSplit(cxt.arg(0).as_string(),',');

		if (cxt.arg_count() > 1)
		{
			lk::vardata_t& opts = cxt.arg(1).deref();

			if (lk::vardata_t* x = opts.lookup("mode"))	{
				wxArrayString modes = wxSplit(x->as_string(), ',');
				for (size_t i = 0; i < modes.GetCount(); i++) {
					wxString mm = modes[i];
					mm.MakeLower();
					if (mm == "f") mr.modes.push_back('f');
					else if (mm == "e")mr.modes.push_back('e');
					else if (mm == "h") mr.modes.push_back('h');
					else mr.modes.push_back('g');
				}
			}

			if (lk::vardata_t* x = opts.lookup("deci")) {
				wxArrayString decis = wxSplit(x->as_string(), ',');
				for (size_t i = 0; i < decis.GetCount(); i++) {
					int deci = wxAtoi(decis[i]);
					mr.decis.push_back(deci);
				}
			}

			if (lk::vardata_t* x = opts.lookup("thousep")) {
				wxArrayString thouseps = wxSplit(x->as_string(), ',');
				for (size_t i = 0; i < thouseps.GetCount(); i++) {
					wxString mm = thouseps[i];
					mm.MakeLower();
					if (mm == "f") mr.thouseps.push_back(false);
					else mr.thouseps.push_back(true);
				}
			}

			if (lk::vardata_t* x = opts.lookup("pre"))
				mr.pres = wxSplit(x->as_string(),',');

			if (lk::vardata_t* x = opts.lookup("post"))
				mr.posts = wxSplit(x->as_string(), ',');

			if (lk::vardata_t* x = opts.lookup("label"))
				mr.label = x->as_string();

			if (lk::vardata_t* x = opts.lookup("scale")) {
				wxArrayString scales = wxSplit(x->as_string(), ',');
				for (size_t i = 0; i < scales.GetCount(); i++) {
					double scale = wxAtof(scales[i]);
					mr.scales.push_back(scale);
				}
			}

			if (lk::vardata_t* x = opts.lookup("tableName"))
				mr.tableName = x->as_string().MakeLower();
		}
*/
		ci->GetResultsViewer()->AddMetricRow(mr);
	}
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

//			if (lk::vardata_t* x = opts.lookup("tableName"))
//				md.tableName = x->as_string().MakeLower();
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

static void fcall_add_gain_term(lk::invoke_t &cxt)
{
	LK_DOC("add_gain_term", "Adds a gain to the loss diagram.", "(string:variable name, string:text):none");
	if (LossDiagCallbackContext *ldcc = static_cast<LossDiagCallbackContext*>(cxt.user_data()))
	{
		Simulation &sim = ldcc->GetSimulation();
		LossDiagramObject &ld = ldcc->GetDiagram();

		if (VarValue *vv = sim.GetValue(cxt.arg(0).as_string()))
		{
			if (vv->Type() == VV_NUMBER)
				ld.AddLossTerm(-vv->Value(), cxt.arg(1).as_string());
		}
	}
}

static void fcall_agraph( lk::invoke_t &cxt )
{
	LK_DOC("agraph", "Create an autograph", "(string:Y, string:title, string:xlabel, string:ylabel, [int:size], [bool:show_xvalues], [bool:show_legend], [string:legend_position (bottom, right, floating)], [integer:graph_type(BAR, STACKED, LINE, SCATTER, CONTOUR, SECTOR), [number:Xmin value], [number:Xmax value]]:none" );

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
		ag.Type = Graph::BAR;
		ag.XMin = -1;
		ag.XMax = -1;
		if (cxt.arg_count() > 4)
			ag.size = cxt.arg(4).as_integer();
		if (cxt.arg_count() > 5)
			ag.show_xvalues = cxt.arg(5).as_boolean();
		if (cxt.arg_count() > 6)
			ag.show_legend = cxt.arg(6).as_boolean();
		if (cxt.arg_count() > 7)
			ag.legend_pos = cxt.arg(7).as_string();
		if (cxt.arg_count() > 8)
			ag.Type = cxt.arg(8).as_integer();
		if (cxt.arg_count() > 9)
            ag.XMin = cxt.arg(9).as_number();
        if (cxt.arg_count() > 10)
            ag.XMax = cxt.arg(10).as_number();
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
	result.nullify();
	if (VarInfo *vi = c->Variables().Lookup( name ))
	{
		result.empty_hash();
		result.hash_item("label").assign( vi->Label );
		result.hash_item("units").assign( vi->Units );
		result.hash_item("group").assign( vi->Group );
	}
	else
	{
		wxArrayString names, labels, units, groups;
		Simulation::ListAllOutputs( c->GetConfiguration(),
			&names, &labels, &units, &groups, nullptr );
		int idx = names.Index( name );
		if ( idx >=0 )
		{
			result.empty_hash();
			result.hash_item("label").assign( labels[idx] );
			result.hash_item("units").assign( units[idx] );
			result.hash_item("group").assign( groups[idx] );
		}
	}
}

static void fcall_varinfo( lk::invoke_t &cxt )
{
	LK_DOC("varinfo", "Gets meta data about an input or output variable. Returns null if the variable does not exist.", "(string:var name):table");
	if ( CaseCallbackContext *ci = static_cast<CaseCallbackContext*>(cxt.user_data()) )
		invoke_get_var_info( &ci->GetCase(), cxt.arg(0).as_string(), cxt.result() );
}

void fcall_value( lk::invoke_t &cxt )
{
	LK_DOC("value", "Gets or sets the value of a variable by name", "(string:name [,variant:value, bool:trigger value change (default true)]):[variant]");

	CaseCallbackContext &cc = *(CaseCallbackContext*)cxt.user_data();
	wxString name = cxt.arg(0).as_string();
	if ( VarValue *vv = cc.GetValues().Get( name ) )
	{
		if ( cxt.arg_count() > 1 )
		{
			if ( vv->Read( cxt.arg(1), false ) ){
			    bool trigger = true;
			    if (cxt.arg_count() == 3 ) {
			        trigger = cxt.arg(2).as_boolean();
			    }
			    if (trigger) {
			      cc.GetCase().VariableChanged( name );
			    }
			}
			else
				cxt.error( "data type mismatch attempting to set '" + name + "' (" + vv_strtypes[vv->Type()] + ") to " + cxt.arg(1).as_string() + " ("+ wxString(cxt.arg(1).typestr()) + ")"  );
		}
		else
			vv->Write( cxt.result() );
	}
	else
		cxt.error("variable '" + name + "' does not exist in this context" );
}

void fcall_is_assigned( lk::invoke_t &cxt )
{
    LK_DOC("is_assigned", "Check by name if an input or output variable exists in current case", "(string:name):bool");

    CaseCallbackContext &cc = *(CaseCallbackContext*)cxt.user_data();
    wxString name = cxt.arg(0).as_string();
    if ( cc.GetCase().BaseCase().GetValue( name ) )
        cxt.result().assign(1);
    else
        cxt.result().assign((double)0);
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
    Case* cur_case = &cc.GetCase();
    ActiveInputPage *ipage = 0;
	if ( cxt.arg_count() == 0 ){
	    ipage = cc.InputPage();
		ipage->Refresh();
        auto UIObjects = ipage->GetObjects();
        for (auto &i: UIObjects){
            VarValue *vv = cur_case->Values().Get( i->GetName() );
            if ( wxWindow *win = i->GetNative() )
                win->Refresh();
            if ( ipage && vv )
            {
                ipage->DataExchange( i, *vv, ActiveInputPage::VAR_TO_OBJ );
            }
        }
	}
	else
	{
	    wxString var = cxt.arg(0).as_string();
        wxUIObject *obj = cc.InputPage()->FindActiveObject( var, &ipage );
        VarValue *vv = cur_case->Values().Get( var );
        if ( obj ){
			if ( wxWindow *win = obj->GetNative() )
				win->Refresh();
            if ( ipage && vv )
            {
                ipage->DataExchange( obj, *vv, ActiveInputPage::VAR_TO_OBJ );
            }
		}
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
// Strange error about conversion to wxColourBase::ChannelType, doesn't appear valid
#pragma warning (push)
#pragma warning (disable: 4242)
				p.Set( wxColour(
					val.index(0)->as_unsigned(),
					val.index(1)->as_unsigned(),
					val.index(2)->as_unsigned()) );
#pragma warning (pop)
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

static void fcall_macrocall( lk::invoke_t &cxt )
{
	LK_DOC( "macrocall", "Run a macro (or external script in the macro environment), optionally passing arguments to it.", "(string:macro name or file, [variant:args]):boolean" );

	cxt.result().assign( 0.0 );

	Case *c = SamApp::Window()->GetCurrentCase();
	if ( !c ) return;

	wxString tech, fin;
	c->GetConfiguration( &tech, &fin );
	wxArrayString macros = MacroEngine::ListMacrosForConfiguration( tech, fin );

	wxString name = cxt.arg(0).as_string();
	name.MakeLower();

	int imacro = -1;
	for( size_t i=0;i<macros.size();i++ )
	{
		if ( wxFileName(macros[i]).GetName().Lower() == name )
		{
			imacro = (int)i;
			break;
		}
	}

	if ( imacro < 0 )
		return;

	wxString script;
	wxFile fp( macros[imacro] );
	if ( fp.IsOpened() )
		fp.ReadAll( &script );
	else
		return;

	lk::vardata_t *args = 0;
	if ( cxt.arg_count() > 1 )
	{
		args = new lk::vardata_t;
		args->copy( cxt.arg(1) );
	}

	MacroEngine me;
	bool ok = me.Run( script, args ); // takes ownership of args
	cxt.result().assign( ok ? 1.0 : 0.0 );
}

static void fcall_pagenote( lk::invoke_t &cxt )
{
	LK_DOC( "pagenote", "Set or get the page note for the current user interface page in the current case.", "( [string:note] ):string" );

	CaseWindow *cw = SamApp::Window()->GetCurrentCaseWindow();
	if ( !cw ) return;

	if ( cxt.arg_count() == 0 )
		cxt.result().assign( cw->GetCase()->RetrieveNote( cw->GetCurrentContext() ) );
	else {
		cw->SetPageNote( cxt.arg(0).as_string() );
		SamApp::Project().SetModified( true );
	}
}

static void fcall_pdfreport( lk::invoke_t &cxt )
{
	LK_DOC( "pdfreport", "Generates a PDF report for the current case. Options include 'template', 'metadata'.", "( string:file [, table:options] ):boolean" );

	CaseWindow *c = SamApp::Window()->GetCurrentCaseWindow();
	if ( !c ) {
		cxt.result().assign( 0.0 );
		return;
	}

	wxString file(cxt.arg(0).as_string() );
	wxString templ;
	VarValue meta;

	if ( cxt.arg_count() > 1 )
	{
		lk::vardata_t &hh = cxt.arg(1).deref();

		if ( lk::vardata_t *x = hh.lookup("template") )
			templ = x->as_string();

		if ( lk::vardata_t *x = hh.lookup("metadata") )
			meta.Read( x->deref(), true );
	}

	bool ok = c->GenerateReport( file, templ, &meta );

	cxt.result().assign( ok ? 1.0 : 0.0 );
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
	LK_DOC("xl_create", "Create a new Excel OLE automation object", "( [string: optional file to open], [boolean:show] ):xl-object-ref" );

	lkXLObject *xl = new lkXLObject;
	xl->Excel().StartExcel();
	if ( cxt.arg_count() > 0 )
	{
		wxString fn = cxt.arg(0).as_string();
		if (wxFileExists(fn))
			xl->Excel().OpenFile( fn );
	}
	bool show = true;
	if ( cxt.arg_count() > 1 )
		show = cxt.arg(1).as_boolean();
	if ( show )
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
	else
		cxt.error( "invalid xl-obj-ref" );
}

static void fcall_xl_open(lk::invoke_t &cxt)
{
	LK_DOC("xl_open", "Opens an Excel file for automation.", "( xl-obj-ref:xl, string:file_name, [boolean:show] ):none");

	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
	{
		wxString fn = cxt.arg(1).as_string();
		if (wxFileExists(fn))
			xl->Excel().OpenFile( fn );

		bool show = true;
		if ( cxt.arg_count() > 2 )
			show = cxt.arg(2).as_boolean();
		if ( show )
			xl->Excel().Show( true );
	}
	else
		cxt.error( "invalid xl-obj-ref" );
}

static void fcall_xl_close( lk::invoke_t &cxt )
{
	LK_DOC("xl_close", "Close the current Excel files without saving changes.", "( xl-obj-ref:xl ):none" );
	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
		cxt.result().assign( xl->Excel().CloseAllNoSave() ? 1.0 : 0.0 );
	else
		cxt.error( "invalid xl-obj-ref" );
}

static void fcall_xl_autosizecols( lk::invoke_t &cxt )
{
	LK_DOC( "xl_autosizecols", "Automatically size columns in the current Excel file", "( xl-obj-ref:xl ):none" );

	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
		xl->Excel().AutoFitColumns();
	else
		cxt.error( "invalid xl-obj-ref" );
}

static void fcall_xl_wkbook( lk::invoke_t &cxt )
{
	LK_DOC( "xl_wkbook", "Create a new empty workbook in the Excel file", "( xl-obj-ref:xl ):none" );

	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
		xl->Excel().NewWorkbook();
	else
		cxt.error( "invalid xl-obj-ref" );
}

static void fcall_xl_sheet( lk::invoke_t &cxt )
{
	LK_DOC( "xl_sheet", "Create a new worksheet in the current workbook", "( xl-obj-ref:xl, [string: optional name] ):none");

	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
	{
		if ( xl->Excel().AddWorksheet() && cxt.arg_count() == 2 )
			xl->Excel().SetWorksheetName( cxt.arg(1).as_string() );
	}
	else
		cxt.error( "invalid xl-obj-ref" );
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
	else
		cxt.error( "invalid xl-obj-ref" );
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
	else
		cxt.error( "invalid xl-obj-ref" );
}

static void fcall_xl_read(lk::invoke_t &cxt)
{
	LK_DOC("xl_read", "Read an excel file into a 2D array (default) or a table. Options: "
		"'skip' (header lines to skip), "
		"'numeric' (t/f to return numbers), "
		"'order' (r/c, row-major order default), "
		"'table' (t/f to return a table assuming 1 header line with names)",
		"(string:file[, table:options]):array or table");

	lk::vardata_t &out = cxt.result();
	out.empty_hash();

	size_t nskip = 0;
	bool rowMajor = true;
	bool tonum = false;
	bool astable = false;
	if (cxt.arg_count() > 1 && cxt.arg(1).deref().type() == lk::vardata_t::HASH)
	{
		lk::vardata_t &opts = cxt.arg(1).deref();

		if (lk::vardata_t *item = opts.lookup("skip"))
			nskip = item->as_unsigned();

		if (lk::vardata_t *item = opts.lookup("numeric"))
			tonum = item->as_boolean();

		if (lk::vardata_t *item = opts.lookup("table"))
			astable = item->as_boolean();

		if (lk::vardata_t *item = opts.lookup("order"))
			rowMajor = (item->as_string() == "c") ? false : true;
	}

	if (lkXLObject *xl = dynamic_cast<lkXLObject*>(cxt.env()->query_object(cxt.arg(0).as_integer())))
	{
		wxArrayString vals;
		int rowCount, columnCount;
		xl->Excel().getUsedCellRange(rowCount, columnCount, vals);

		if ((int)nskip >= rowCount - 1) nskip = 0;
		if (astable)
		{
			if (rowMajor) {
				wxArrayString colHeaders;
				//read column headers from first nonskipped row
				for (int c = 0; c < columnCount; c++) {
					wxString name = vals[c*rowCount + nskip];
					colHeaders.push_back(name);
					if (name.IsEmpty()) continue;

					lk::vardata_t &it = out.hash_item(name);
					it.empty_vector();
					it.resize(rowCount - 1 - nskip);
				}
				for (int c = 0; c < columnCount; c++) {
					lk::vardata_t* it = out.lookup(colHeaders[c]);
					if (it == NULL) continue;
					for (int r = 1 + nskip; r < rowCount; r++) {
						if (vals[c*rowCount + r] != wxEmptyString) {
							if (tonum) it->index(r - 1 - nskip)->assign(wxAtof(vals[c*rowCount + r]));
							else it->index(r - 1 - nskip)->assign(vals[c*rowCount + r]);
						}
					}
				}
			}
			else {
				wxArrayString rowHeaders;
				//read row headers from first nonskipped column
				for (int r = 0; r < rowCount; r++) {
					wxString name = vals[nskip*rowCount + r];
					rowHeaders.push_back(name);
					if (name.IsEmpty()) continue;

					lk::vardata_t &it = out.hash_item(name);
					it.empty_vector();
					it.resize(columnCount - 1 - nskip);
				}

				for (int r = 0; r < rowCount; r++) {
					lk::vardata_t* it = out.lookup(rowHeaders[r]);
					if (it == NULL) continue;
					for (int c = 1 + nskip; c < columnCount; c++) {
						if (vals[c*rowCount + r] != wxEmptyString) {
							if (tonum) it->index(c - 1 - nskip)->assign(wxAtof(vals[c*rowCount + r]));
							else it->index(c - 1 - nskip)->assign(vals[c*rowCount + r]);
						}
					}
				}
			}
		}
		else {
			if (rowMajor == true) {
				out.empty_vector();
				out.vec()->resize(rowCount - nskip);
				for (int r = nskip; r < rowCount; r++) {
					lk::vardata_t *row = out.index(r-nskip);
					row->empty_vector();
					row->vec()->resize(columnCount);
					for (int c = 0; c < columnCount; c++)
					{
						if (vals[c*rowCount + r] == wxEmptyString) continue;
						if (tonum) {
							row->index(c)->assign(wxAtof(vals[c*rowCount + r]));
						}
						else {
							row->index(c)->assign(vals[c*rowCount + r]);
						}
					}
				}
			}
			else {
				out.empty_vector();
				out.vec()->resize(columnCount - nskip);
				for (int c = nskip; c < columnCount; c++) {
					lk::vardata_t *col = out.index(c-nskip);
					col->empty_vector();
					col->vec()->resize(rowCount);
					for (int r = 0; r < rowCount; r++)
					{
						if (vals[c*rowCount + r] == wxEmptyString) continue;
						if (tonum) {
							col->index(r)->assign(wxAtof(vals[c*rowCount + r]));
						}
						else {
							col->index(r)->assign(vals[c*rowCount + r]);
						}
					}
				}
			}
		}
	}
	else
		cxt.error("invalid xl-obj-ref");

}
#endif



class lkSSCdataObj : public lk::objref_t
{
	ssc_data_t m_data;
public:

	lkSSCdataObj() {
		m_data = ssc_data_create();
	}

	lkSSCdataObj(ssc_data_t p_data) {
		m_data = p_data;
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
            sscdata_to_lkvar(*ssc, (const char *) name.ToUTF8(), cxt.result());
		else if (cxt.arg_count() == 3)
            assign_lkvar_to_sscdata(cxt.arg(2).deref(), (const char *) name.ToUTF8(), *ssc);
	}
	else
		cxt.error( "invalid ssc-obj-ref" );
}

void fcall_ssc_create( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_create", "Create a new empty SSC data container object.", "(none):ssc-obj-ref" );
	cxt.result().assign( cxt.env()->insert_object( new lkSSCdataObj ) );
}

void fcall_ssc_module_create_from_case(lk::invoke_t &cxt)
{
	LK_DOC("ssc_module_create_from_case", "Create a new SSC data container object populated from the input compute module values defined in the current case", "(string:compute_module_name):ssc-obj-ref");

	// Get the existing simulation object from the base
	Case *c = SamApp::Window()->GetCurrentCase();
	Simulation &sim = c->BaseCase();

	// Create the ssc_data and compute module
	wxString cm = cxt.arg(0).as_string();
	ssc_data_t p_data = ssc_data_create();
	ssc_module_t p_mod = ssc_module_create((const char*)cm.ToUTF8());
	if (!p_mod)
	{
		cxt.error("could not create ssc module: " + cm);
		return;
	}

	// Assign the compute module with existing values
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
				if (VarValue *vv = sim.GetInput(name))
				{
					if (!field.IsEmpty())
					{
						if (vv->Type() != VV_TABLE)
							cxt.error("SSC variable has table:field specification, but '" + name + "' is not a table in SAM");

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
									cxt.error("Error translating table:field variable from SAM UI to SSC for '" + name + "':" + field);
							}
						}

					}

					if (!VarValueToSSC(vv, p_data, name))
						cxt.error("Error translating data from SAM UI to SSC for " + name);

				}
				else if (reqd == "*")
					cxt.error("SSC requires input '" + name + "', but was not found in the SAM UI or from previous simulations");
			}
		}
	}

	// Run the ssc compute module and dump results into the cxt
	cxt.result().assign(cxt.env()->insert_object(new lkSSCdataObj(p_data)));
	ssc_module_free(p_mod);

	// ssc_data_free is called later, as creating the new lkSSCdataObj doesn't actually create a deep copy of p_data
}

void fcall_ssc_free( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_free", "Frees up an SSC data object.", "(ssc-obj-ref:data):none" );

	if ( lkSSCdataObj *ssc = dynamic_cast<lkSSCdataObj*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
		cxt.env()->destroy_object( ssc );
	else
		cxt.error( "invalid ssc-obj-ref" );
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
		cxt.error( "invalid ssc-obj-ref" );
}


static ssc_bool_t ssc_exec_handler( ssc_module_t, ssc_handler_t,
	int action_type, float f0, float,
	const char *s0, const char *,
	void *user_data )
{
	wxThreadProgressDialog *tpd = (wxThreadProgressDialog*) user_data;
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
	if ( !ssc ) {
		cxt.error( "invalid ssc-obj-ref" );
		return;
	}

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


	wxThreadProgressDialog *tpd = 0;
	if ( show_dialog )
	{
		tpd = new wxThreadProgressDialog( SamApp::Window(), 1, true );
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

void fcall_ssc_eqn(lk::invoke_t &cxt)
{
    LK_DOC("ssc_eqn", "Call equation with var_table inputs. Returns true upon success. Errors reported in inputs table.", "(string: eqn_name, table:inputs):bool");
    wxString eqn_name = cxt.arg(0).as_string();

    ssc_data_t vd_data = ssc_data_create();
    if (cxt.arg(1).deref().type() != lk::vardata_t::HASH)
        throw lk::error_t(lk_tr("Inputs to equation must be a table"));
    lkhash_to_sscdata(cxt.arg(1).deref(), vd_data);

    size_t i = 0;
    while ( ssc_equation_table[i].func){
        if (wxStrcmp(eqn_name, ssc_equation_table[i].name) == 0){
            try {
                (*ssc_equation_table[i].func)(vd_data);
                cxt.result().assign(1.);
            }
            catch (std::runtime_error &e){
				cxt.arg(1).deref().hash_item("error", e.what());
                cxt.result().assign(0.);
                return;
            }
            sscdata_to_lkhash(vd_data, cxt.arg(1).deref());
            return;
        }
        i++;
    }
    throw lk::error_t(lk_tr("Equation by that name does not exist."));
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

	// Pac = {(Paco / (A - B))  C · (A - B)}· (Pdc- B) + C · (Pdc - B)**2


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

	int it = 0;
	const int maxit = 4000;
	while (fabs(Inew - Iold) > 1.0e-4 && it++ < maxit )
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

void fcall_wfdownloaddir( lk::invoke_t &cxt)
{
	LK_DOC( "wfdownloaddir", "Returns the folder into which solar data files are downloaded.", "(none):string" );

	//Create a folder to put the weather file in
	wxString wfdir;
	SamApp::Settings().Read("solar_download_path", &wfdir);
	if (wfdir.IsEmpty()) wfdir = ::wxGetHomeDir() + "/SAM Downloaded Weather Files";
	if (!wxDirExists(wfdir)) wxFileName::Mkdir(wfdir, 511, ::wxPATH_MKDIR_FULL);
	// save to settings (addresses user support issue 69194 12/11/14)
	SamApp::Settings().Write("solar_download_path", wfdir);
	cxt.result().assign(wfdir);
}


void fcall_nsrdbquery(lk::invoke_t &cxt)
{
	LK_DOC("nsrdbquery", "Creates the NSRDB data download dialog box, lists all avaialble resource files, downloads multiple solar resource files, and returns local file name for weather file", "(none) : string");
	//Create the wind data object
	NSRDBDialog dlgNSRDB(SamApp::Window(), "Advanced NSRDB Download");
	dlgNSRDB.CenterOnParent();
	int code = dlgNSRDB.ShowModal(); //shows the dialog and makes it so you can't interact with other parts until window is closed

	//Return an empty string if the window was dismissed
	if (code == wxID_CANCEL)
	{
		cxt.result().assign(wxEmptyString);
		return;
	}

	//Get selected filename
	wxString foldername = dlgNSRDB.GetWeatherFolder();
	wxString filename = dlgNSRDB.GetWeatherFile();
	wxString addfolder = dlgNSRDB.GetAddFolder();

	cxt.result().empty_hash();

	// meta data
	cxt.result().hash_item("file").assign(filename);
	cxt.result().hash_item("folder").assign(foldername);
	cxt.result().hash_item("addfolder").assign(addfolder);
}

void fcall_combinecasesquery(lk::invoke_t& cxt)
{
	LK_DOC("combinecasesquery", "Creates the Combine Cases dialog box, lists all open cases, simulates selected cases and returns a combined generation profile", "(none) : string");
	CombineCasesDialog dlgCombineCases(SamApp::Window(), "Combine Cases", cxt);
	dlgCombineCases.CenterOnParent();
	int code = dlgCombineCases.ShowModal(); //shows the dialog and makes it so you can't interact with other parts until window is closed

	//Return an empty string if the window was dismissed
	if (code == wxID_CANCEL)
	{
		cxt.result().assign(wxEmptyString);
		return;
	}

	int result = dlgCombineCases.GetResultCode();		// 0 = success, 1 = error

	cxt.result().empty_hash();

	// meta data
	cxt.result().hash_item("result_code").assign(result);
}

void fcall_wavetoolkit(lk::invoke_t& cxt)
{
    LK_DOC("wavetoolkit", "Creates the Wave data download dialog box, lists all avaialble resource files, downloads multiple solar resource files, and returns local file name for weather file", "(none) : string");
    //Create the wind data object
    WaveDownloadDialog dlgWave(SamApp::Window(), "Advanced Wave Download");
    dlgWave.CenterOnParent();
    int code = dlgWave.ShowModal(); //shows the dialog and makes it so you can't interact with other parts until window is closed

    //Return an empty string if the window was dismissed
    if (code == wxID_CANCEL)
    {
        cxt.result().assign(wxEmptyString);
        return;
    }

    //Get selected filename
    wxString foldername = dlgWave.GetWeatherFolder();
    wxString filename = dlgWave.GetWeatherFile();
    wxString addfolder = dlgWave.GetAddFolder();

    wxEasyCurlDialog ecd = wxEasyCurlDialog("Setting up location", 1);

    //Get parameters from the dialog box for weather file download
    wxString year;
    wxArrayString multi_year;
    wxArrayString years_final;
    
    years_final = dlgWave.GetMultiYear();
    
    double lat, lon;
    ecd.Update(1, 50.0f);
    if (dlgWave.IsAddressMode() == true)	//entered an address instead of a lat/long
    {
        if (!wxEasyCurl::GeoCodeDeveloper(dlgWave.GetAddress(), &lat, &lon, NULL, false))
        {
            ecd.Log("Failed to geocode address");
            ecd.Finalize();
            return;
        }
    }
    else
    {
        lat = dlgWave.GetLatitude();
        lon = dlgWave.GetLongitude();
    }
    ecd.Update(1, 100.0f);
    ecd.Log(wxString::Format("Retrieving data at lattitude = %.2lf and longitude = %.2lf", lat, lon));


    wxString location;
    location.Printf("lat%.2lf_lon%.2lf_", lat, lon);
    location = location + "_";
    wxArrayString filename_array;
    filename_array.resize(years_final.Count());

    //Create a folder to put the weather file in
    wxString wfdir;
    //wfdir = ::wxGetUserHome() + "/SAM Downloaded Weather Files";
    wfdir = foldername;
    if (!wxDirExists(wfdir)) wxFileName::Mkdir(wfdir, 511, ::wxPATH_MKDIR_FULL);


    wxArrayString wfs;

    //Create URL for each hub height file download
    wxString url;
    bool success = true;
    wxArrayString urls, displaynames;
    wxCSVData csv_main, csv;

    //Create the filename
    //filename = wfdir + "/" + location;

    std::vector<wxEasyCurl*> curls;

    for (size_t i = 0; i < years_final.Count(); i++)
    {
        url = SamApp::WebApi("wave_query");
        url.Replace("<YEAR>", years_final[i]);
        url.Replace("<LAT>", wxString::Format("%lg", lat));
        url.Replace("<LON>", wxString::Format("%lg", lon));
        wxEasyCurl* curl = new wxEasyCurl;
        curls.push_back(curl);
        urls.push_back(url);
        displaynames.push_back(years_final[i]);
        filename_array[i] = wfdir + "/" + location + "_" + years_final[i];
    }

    int nthread = years_final.Count();
    nthread = 1;
    // no need to create extra unnecessary threads
    if (nthread > (int)urls.size()) nthread = (int)urls.size();

    ecd.NewStage("Retrieving weather data", nthread);



    std::vector<wxEasyCurlThread*> threads;
    for (int i = 0; i < nthread; i++)
    {
        wxEasyCurlThread* t = new wxEasyCurlThread(i);
        threads.push_back(t);
        t->Create();
    }

    // round robin assign each simulation to a thread
    size_t ithr = 0;
    for (size_t i = 0; i < urls.size(); i++)
    {
        threads[ithr++]->Add(curls[i], urls[i], displaynames[i]);
        if (ithr == threads.size())
            ithr = 0;
    }

    // start the threads
    for (int i = 0; i < nthread; i++)
        threads[i]->Run();

    size_t its = 0, its0 = 0;
    unsigned long ms = 500; // 0.5s
    // can time first download to get better estimate
    float tot_time = 25 * (float)years_final.Count(); // 25 s guess based on test downloads
    float per = 0.0f, act_time;
    int curhh = 0;
    wxString cur_hh = "";
    wxString file_list = "";
    int num_downloaded = 0;
    while (1)
    {
        size_t i, num_finished = 0;
        for (i = 0; i < threads.size(); i++)
            if (!threads[i]->IsRunning())
                num_finished++;

        if (num_finished == threads.size())
            break;

        // threads still running so update interface
        for (i = 0; i < threads.size(); i++)
        {
            wxString update;
            per += (float)(ms) / (10 * tot_time); // 1/10 = 100 (percent) / (1000 ms/s)
            if (per > 100.0) per = (float)curhh / (float)years_final.Count() * 100.0 - 10.0; // reset 10%
            ecd.Update(i, per, update);
            wxArrayString msgs = threads[i]->GetNewMessages();
            ecd.Log(msgs);
            if (threads[i]->GetDataAsString() != cur_hh)
            {
                if (cur_hh != "")
                { // adjust actual time based on first download
                    act_time = (float)((its - its0) * ms) / 1000.0f;
                    tot_time = act_time * (float)years_final.Count();
                    its0 = its;
                }
                cur_hh = threads[i]->GetDataAsString();
                ecd.Log("Downloading data for " + cur_hh + " hub height.");
                per = (float)curhh / (float)years_final.Count() * 100.0;
                curhh++;
            }
        }

        wxGetApp().Yield();

        // if dialog's cancel button was pressed, send cancel signal to all threads
        if (ecd.Canceled())
        {
            for (i = 0; i < threads.size(); i++)
                threads[i]->Cancel();
            if (success)
            {
                ecd.Log("Download Cancelled.");
                success = false;
            }
        }
        its++;
        ::wxMilliSleep(ms);
    }

    if (success)
    {
        size_t nok = 0;
        // wait on the joinable threads
        for (size_t i = 0; i < threads.size(); i++)
        {
            threads[i]->Wait();
            nok += threads[i]->NOk();

            // update final progress
            float per = threads[i]->GetPercent();
            ecd.Update(i, per);

            // get any final simulation messages
            wxArrayString msgs = threads[i]->GetNewMessages();
            ecd.Log(msgs);
        }

        for (size_t i = 0; i < years_final.Count(); i++)
        {
            bool ok = curls[i]->Get(urls[i]);
            // try without attributes
            if (ok && (curls[i]->GetDataAsString().Length() < 1000))
                ok = curls[i]->Get(urls[i]);
            if (!ok)
                wxMessageBox("Download failed.\n\n" + urls[i] + "\n\nThere may be a problem with your internet connection,\nor the NSRDB web service may be down.", "NSRDB Download Message", wxOK);
            else if (curls[i]->GetDataAsString().Length() < 1000)
                wxMessageBox("Weather file not available.\n\n" + urls[i] + "\n\n" + curls[i]->GetDataAsString(), "NSRDB Download Message", wxOK);
            else
            {
                wxString fn = filename_array[i] + ".csv";
                //fn = m_weatherFolder + "/" + fn;
                file_list += filename_array[i] + "\n";
                if (!curls[i]->WriteDataToFile(fn))
                {
                    wxMessageBox("Failed to write file.\n\n" + fn, "NSRDB Download Message", wxOK);
                    //break;
                }
                num_downloaded++;
            }

            //if (pdlg.WasCancelled())
              //  break;

            /*
            wxString wave_csv_data = curls[i]->GetDataAsString();
            if (!csv.ReadString(wave_csv_data))
            {
                //			wxMessageBox(wxString::Format("Failed to read downloaded weather file %s.", filename));
                ecd.Log(wxString::Format("Failed to read downloaded weather file %s.", filename));
                success = false;
            }
            
            filename_array[i] += ".csv";
            if (!csv.WriteFile(filename_array[i]))
            {
                ecd.Log(wxString::Format("Failed to write downloaded weather file %s.", filename_array[i]));
                ecd.Finalize();
                return;
            }
            */
        }
        // write out combined hub height file
        if (num_downloaded > 0)
        {
            if (wxDirExists(wfdir))
            {
                wxArrayString paths;
                wxString buf;
                if (SamApp::Settings().Read("wave_data_paths", &buf))
                    paths = wxStringTokenize(buf, ";");
                if (paths.Index(foldername) == wxNOT_FOUND)
                {
                    paths.Add(foldername);
                    SamApp::Settings().Write("wave_data_paths", wxJoin(paths, ';'));
                }
            }
            if (file_list != "") wxMessageBox("Download complete.\n\nThe following files have been downloaded and added to your solar resource library:\n\n" + file_list, "NSRDB Download Message", wxOK);
            //EndModal(wxID_OK);
        }
        
    }



    // delete all the thread objects
    for (size_t i = 0; i < curls.size(); i++)
        delete curls[i];
    for (size_t i = 0; i < threads.size(); i++)
        delete threads[i];

    threads.clear();
    curls.clear();
    if (!success)
    {
        ecd.Finalize();
        return;
    }
    /*
    if (!csv_main.WriteFile(filename))
    {
        ecd.Log(wxString::Format("Failed to write downloaded weather file %s.", filename));
        ecd.Finalize();
        return;
    }
    */
    cxt.result().empty_hash();

    // meta data
    cxt.result().hash_item("file").assign(filename);
    cxt.result().hash_item("folder").assign(foldername);
    cxt.result().hash_item("addfolder").assign(addfolder);
    //Return the downloaded filename
    
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
	// Setup progress dialog in main UI thread
	wxEasyCurlDialog ecd = wxEasyCurlDialog("Setting up location",1);

	//Get parameters from the dialog box for weather file download
	wxString year;
	year = spd.GetYear();
	double lat, lon;
	ecd.Update(1, 50.0f);
	if (spd.IsAddressMode() == true)	//entered an address instead of a lat/long
	{
		if (!wxEasyCurl::GeoCodeDeveloper(spd.GetAddress(), &lat, &lon, NULL, false))
		{
			ecd.Log("Failed to geocode address");
			ecd.Finalize();
			return;
		}
	}
	else
	{
		lat = spd.GetLatitude();
		lon = spd.GetLongitude();
	}
	ecd.Update(1, 100.0f);
	ecd.Log(wxString::Format("Retrieving data at lattitude = %.2lf and longitude = %.2lf", lat, lon));

	wxArrayString hh = spd.GetHubHeights();

	wxString location;
	location.Printf("lat%.2lf_lon%.2lf_", lat, lon);
	location = location + "_" + year;
	wxString filename;

	//Create a folder to put the weather file in
	wxString wfdir;
	wfdir = ::wxGetUserHome() + "/SAM Downloaded Weather Files";
	if (!wxDirExists(wfdir)) wxFileName::Mkdir(wfdir, 511, ::wxPATH_MKDIR_FULL);


	wxArrayString wfs;

	//Create URL for each hub height file download
	wxString url;
	bool success=true;
	wxArrayString urls, displaynames;
	wxCSVData csv_main, csv;

	//Create the filename
	filename = wfdir + "/" + location;

	std::vector<wxEasyCurl*> curls;

	for (size_t i = 0; i < hh.Count(); i++)
	{
		url = SamApp::WebApi("windtoolkit");
		url.Replace("<YEAR>", year);
		url.Replace("<HUBHEIGHT>", hh[i].Left(hh[i].Len() - 1));
		url.Replace("<LAT>", wxString::Format("%lg", lat));
		url.Replace("<LON>", wxString::Format("%lg", lon));
		wxEasyCurl *curl = new wxEasyCurl;
		curls.push_back(curl);
		urls.push_back(url);
		displaynames.push_back(hh[i]);
	}

	int nthread = hh.Count();
	nthread = 1;
	// no need to create extra unnecessary threads
	if (nthread > (int)urls.size()) nthread = (int)urls.size();

	ecd.NewStage("Retrieving weather data", nthread);



	std::vector<wxEasyCurlThread*> threads;
	for (int i = 0; i < nthread; i++)
	{
		wxEasyCurlThread *t = new wxEasyCurlThread(i);
		threads.push_back(t);
		t->Create();
	}

	// round robin assign each simulation to a thread
	size_t ithr = 0;
	for (size_t i = 0; i < urls.size(); i++)
	{
		threads[ithr++]->Add(curls[i],urls[i],displaynames[i]);
		if (ithr == threads.size())
			ithr = 0;
	}

	// start the threads
	for (int i = 0; i < nthread; i++)
		threads[i]->Run();

	size_t its = 0, its0=0;
	unsigned long ms = 500; // 0.5s
	// can time first download to get better estimate
	float tot_time = 25 * (float)hh.Count(); // 25 s guess based on test downloads
	float per=0.0f,act_time;
	int curhh = 0;
	wxString cur_hh="";
	while (1)
	{
		size_t i, num_finished = 0;
		for (i = 0; i < threads.size(); i++)
			if (!threads[i]->IsRunning())
				num_finished++;

		if (num_finished == threads.size())
			break;

		// threads still running so update interface
		for (i = 0; i < threads.size(); i++)
		{
			wxString update;
			per += (float)(ms) / (10 * tot_time); // 1/10 = 100 (percent) / (1000 ms/s)
			if (per > 100.0) per = (float)curhh / (float)hh.Count() * 100.0 - 10.0; // reset 10%
			ecd.Update(i, per, update);
			wxArrayString msgs = threads[i]->GetNewMessages();
			ecd.Log(msgs);
			if (threads[i]->GetDataAsString() != cur_hh)
			{
				if (cur_hh != "")
					{ // adjust actual time based on first download
					act_time = (float)((its-its0) * ms) / 1000.0f;
					tot_time = act_time * (float)hh.Count();
					its0 = its;
				}
				cur_hh = threads[i]->GetDataAsString();
				ecd.Log("Downloading data for " + cur_hh + " hub height.");
				per = (float)curhh / (float)hh.Count() * 100.0;
				curhh++;
			}
		}

		wxGetApp().Yield();

		// if dialog's cancel button was pressed, send cancel signal to all threads
		if (ecd.Canceled())
		{
			for (i = 0; i < threads.size(); i++)
				threads[i]->Cancel();
			if (success)
			{
				ecd.Log("Download Cancelled.");
				success = false;
			}
		}
		its++;
		::wxMilliSleep(ms);
	}

	if (success)
	{
		size_t nok = 0;
		// wait on the joinable threads
		for (size_t i = 0; i < threads.size(); i++)
		{
			threads[i]->Wait();
			nok += threads[i]->NOk();

			// update final progress
			float per = threads[i]->GetPercent();
			ecd.Update(i, per);

			// get any final simulation messages
			wxArrayString msgs = threads[i]->GetNewMessages();
			ecd.Log(msgs);
		}

		for (size_t i = 0; i < hh.Count(); i++)
		{
			wxString srw_api_data = curls[i]->GetDataAsString();
			if (!csv.ReadString(srw_api_data))
			{
				//			wxMessageBox(wxString::Format("Failed to read downloaded weather file %s.", filename));
				ecd.Log(wxString::Format("Failed to read downloaded weather file %s.", filename));
				success=false;
			}
			if (i == 0)
				csv_main.Copy(csv);
			else
			{
				// add header (row 2), units (row 3) and hub heights (row 4)
				// add data (rows 5 through end of data)
				for (size_t j = 2; j < csv.NumRows() && j < csv_main.NumRows(); j++)
					for (size_t k = 0; k < 4; k++)
						csv_main(j, i * 4 + k) = csv(j, k);
			}
			filename += "_" + hh[i];
		}
		// write out combined hub height file
		filename += ".srw";
	}



	// delete all the thread objects
	for (size_t i = 0; i < curls.size(); i++)
		delete curls[i];
	for (size_t i = 0; i < threads.size(); i++)
		delete threads[i];

	threads.clear();
	curls.clear();
	if (!success)
	{
		ecd.Finalize();
		return;
	}

	if (!csv_main.WriteFile(filename))
	{
		ecd.Log(wxString::Format("Failed to write downloaded weather file %s.", filename));
		ecd.Finalize();
		return;
	}
	//Return the downloaded filename
	cxt.result().assign(filename);
}


/********** OPENEI capability ***********/

void fcall_urdb_list_utilities(lk::invoke_t &cxt)
{
	LK_DOC("urdb_list_utilities", "Returns a list of all utility company names in the OpenEI Utility Rate Database.", "(none):string");
	wxArrayString names;
	OpenEI api;
	if (api.QueryUtilityCompanies(names))
	{
		cxt.result().empty_vector();
		for (size_t i = 0; i<names.size(); i++)
			cxt.result().vec_append(names[i]);
	}
}

void fcall_urdb_list_utilities_by_zip_code(lk::invoke_t &cxt)
{
	LK_DOC("urdb_list_utilities_by_zip_code", "Returns a list of utility company names for a given zip code from the OpenEI Utility Rate Database.", "(string:zip_code):array");
	wxString zip_code = cxt.arg(0).as_string();
	wxArrayString names;
	OpenEI api;
	if (api.QueryUtilityCompaniesbyZipcode(zip_code, names))
	{
		cxt.result().empty_vector();
		for (size_t i = 0; i<names.size(); i++)
			cxt.result().vec_append(names[i]);
	}
}

void fcall_urdb_list_rates(lk::invoke_t &cxt)
{
	LK_DOC("urdb_list_rates", "Returns a list of rate names and GUIDs for a given utility company name from OpenEI Utility Rate Database.", "(string:utility):array");

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


void fcall_calculated_list(lk::invoke_t &cxt)
{
	LK_DOC("calculated_list", "Returns all SSC compute module inputs from the current case that are a SAM UI calculated variable.", "():array");

	wxString msg="";
	Case *c = SamApp::Window()->GetCurrentCase();
	if (!c) return;

	ConfigInfo *ci = c->GetConfiguration();
	if (!ci) return;

	wxArrayString sim_list = ci->Simulations;
	VarInfoLookup &vil = ci->Variables;

	if (sim_list.size() == 0)
	{
		return cxt.result().assign("No simulation compute modules defined for this configuration.");
	}

	cxt.result().empty_vector();

	for (size_t kk = 0; kk < sim_list.size(); kk++)
	{
		ssc_module_t p_mod = ssc_module_create(sim_list[kk].c_str());
		if (!p_mod)
		{
			msg += ("could not create ssc module: " + sim_list[kk]);
			break;
		}

		int pidx = 0;
		while (const ssc_info_t p_inf = ssc_module_var_info(p_mod, pidx++))
		{
			int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
			wxString name(ssc_info_name(p_inf)); // assumed to be non-null
			wxString reqd(ssc_info_required(p_inf));

			if (var_type == SSC_INPUT || var_type == SSC_INOUT)
			{
				VarInfo* vi = vil.Lookup(name);
				if (vi && (vi->Flags & VF_CALCULATED))
					cxt.result().vec_append(name);
			}
		}
	}
	if (msg != "")
		return	cxt.result().assign(msg);
}




void fcall_group_write(lk::invoke_t &cxt)
{
	LK_DOC("group_write", "Writes group data from current case to a file.", "(string:groupname, string:filename):boolean");

	Case *c = SamApp::Window()->GetCurrentCase();
	if (!c) return;

	ConfigInfo *ci = c->GetConfiguration();
	if (!ci) return;

	wxString groupname = cxt.arg(0).as_string();
	wxString filename = cxt.arg(1).as_string();

	wxCSVData csv;
	int row = 0;
    if (groupname == "PV Module (CEC User Specified)") {
        for (VarInfoLookup::iterator it = ci->Variables.begin();
            it != ci->Variables.end();	++it)
        {
            VarInfo& vi = *(it->second);
            // skip calculated and indicator values
            if (vi.Flags & VF_CALCULATED || vi.Flags & VF_INDICATOR) continue;
            if (vi.Group.Lower() == groupname.Lower())

            {
                wxString var_name = it->first;
                // get value
                if (VarValue* vv = c->Values().Get(var_name))
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
    }
    else if (groupname == "ME Tidal Converter") {
        csv.Set(0, 0, "Name");
        csv.Set(0, 1, "Technology Type");
        csv.Set(0, 2, "PTO Type");
        csv.Set(0, 3, "Characteristic Diameter");
        csv.Set(0, 4, "Unballasted Structural Mass");
        csv.Set(0, 5, "Foundation Type");
        csv.Set(0, 6, "Mooring Type");
        csv.Set(0, 7, "Primary Structure Material");
        csv.Set(2, 0, "Velocity");
        csv.Set(2, 1, "Power");
        if (VarValue* vv = c->Values().Get("tidal_power_curve")) {
            wxString vel = "";
            wxString power = "";
            matrix_t<double> power_curve = vv->Matrix();
            for (int r = 3; r < (int)vv->Rows(); r++) {
                vel = wxString::Format("%g", power_curve.at(r-3, 0));
                power = wxString::Format("%g", power_curve.at(r-3, 1));
                csv.Set(r, 0, vel);
                csv.Set(r, 1, power);
            }
        }
        if (VarValue* vv = c->Values().Get("device_name")) {
            csv.Set(1, 0, vv->AsString());
        }
        if (VarValue* vv = c->Values().Get("device_tech_type")) {
            csv.Set(1, 1, vv->AsString());
        }
        if (VarValue* vv = c->Values().Get("device_pto_type")) {
            csv.Set(1, 2, vv->AsString());
        }
        if (VarValue* vv = c->Values().Get("device_characteristic_diameter")) {
            csv.Set(1, 3, vv->AsString());
        }
        if (VarValue* vv = c->Values().Get("device_mass")) {
            csv.Set(1, 4, vv->AsString());
        }
        if (VarValue* vv = c->Values().Get("device_foundation")) {
            csv.Set(1, 5, vv->AsString());
        }
        if (VarValue* vv = c->Values().Get("device_mooring")) {
            csv.Set(1, 6, vv->AsString());
        }
        if (VarValue* vv = c->Values().Get("device_material")) {
            csv.Set(1, 7, vv->AsString());
        }
    }
	cxt.result().assign(csv.WriteFile(filename) ? 1.0 : 0.0);
}


void fcall_group_read(lk::invoke_t &cxt)
{
	LK_DOC("group_read", "Reads group data from a file case to the current case.", "(string:groupname, string:filename):boolean");

	Case *c = SamApp::Window()->GetCurrentCase();
	if (!c) return;

	wxString groupname = cxt.arg(0).as_string();
	wxString filename = cxt.arg(1).as_string();

	wxCSVData csv;
	int row = 0;
	bool ret_val = csv.ReadFile(filename);
    wxArrayString errors;
    wxArrayString list;
	if (ret_val)
	{
        
        if (groupname == "PV Module (CEC User Specified)") {
            

            for (row = 0; row < (int)csv.NumRows(); row++)
            {
                wxString var_name = csv.Get(row, 0);
                // get value
                wxString value = csv.Get(row, 1);
                value.Replace(";;", "\n");
                if (VarValue* vv = c->Values().Get(var_name))
                {
                    if (!VarValue::Parse(vv->Type(), value, *vv))
                    {
                        errors.Add("Problem assigning " + var_name + " to " + value);
                        ret_val = false;
                    }
                    else
                        list.Add(var_name);
                }
                else
                {// variable not found
                    errors.Add("Problem assigning " + var_name + " missing with " + value);
                    ret_val = false;
                }
            }
            
        }
        else if (groupname == "ME Tidal Converter") {
            wxString tidal_power_curve = "";
            wxString vel = "";
            wxString power = "";
            for (int r = 3; r < (int)csv.NumRows(); r++) {
                tidal_power_curve += "[" + csv.Get(r, 0) + ";" + csv.Get(r, 1) + "]";
            }
            if (VarValue* vv = c->Values().Get("tidal_power_curve")) {
                if (!VarValue::Parse(vv->Type(), tidal_power_curve, *vv))
                {
                    errors.Add("Problem assigning tidal_power_curve to" + tidal_power_curve);
                    ret_val = false;
                }
                else
                    list.Add("tidal_power_curve");
            }
            else
            {// variable not found
                errors.Add("Problem assigning tidal_power_curve to" + tidal_power_curve);
                ret_val = false;
            }
            

        }
	}
    // this causes the UI and other variables to be updated
    c->VariablesChanged(list);
	cxt.result().assign(ret_val ? 1.0 : 0.0);
}





void fcall_urdb_write(lk::invoke_t &cxt)
{
	LK_DOC("urdb_write", "Writes inputs from Electricity Rates page for the current case to a CSV file.", "(string:filename):boolean");

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
		if (vi.Group.Left(16).Lower() == "electricity rate")

		{
			wxString var_name = it->first;
			// get value
			if (VarValue *vv = c->Values().Get(var_name))
			{
				// write out csv with first row var name and second row var values
				wxString value = vv->AsString();
				value.Replace("\n", ";;");
				value.Replace("\r", ";;");
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
	LK_DOC("urdb_read", "Loads rate data from a CSV file to the Electricity Rates page for the current case.", "(string:filename):boolean");

	Case *c = SamApp::Window()->GetCurrentCase();
	if ( !c ) return;

	wxCSVData csv;
	int row = 0;
	bool ret_val = csv.ReadFile( cxt.arg(0).as_string() );
	if (ret_val)
	{
		wxArrayString errors;
		wxArrayString list;

		wxArrayString upgrade_list;
		wxArrayString upgrade_value;

		for (row = 0; row < (int)csv.NumRows(); row++)
		{
			wxString var_name = csv.Get(row,0);
			// get value
			wxString value = csv.Get(row, 1);
			value.Replace(";;", "\n");
			if (VarValue *vv = c->Values().Get(var_name))
			{
				if ( !VarValue::Parse(vv->Type(), value, *vv) )
				{
					errors.Add("Problem assigning " + value + " to " + var_name );
					ret_val = false;
				}
				else
					list.Add( var_name );
			}
			else
			{// variable not found
				// try upgrading - see project file upgrader for 2015.11.16
				// update to matrix for ec and dc
				//errors.Add("Problem assigning " + var_name + " missing with " + value);
				ret_val = false;
				upgrade_list.Add(var_name);
				upgrade_value.Add(value);
			}
		}
		// try upgrading to matrix format
		if (upgrade_list.Count() > 0)
		{
			// try upgrading - see project file upgrader for 2015.11.16
			matrix_t<float> ec_tou_mat(72, 6); // will resize
			matrix_t<float> dc_tou_mat(72, 4); // will resize
			matrix_t<float> dc_flat_mat(72, 4); // will resize
			int ec_tou_row=0;
			int dc_tou_row=0;
			int dc_flat_row=0;
			int ndx;
			double br, sr, ub, dc;
			wxString per_tier;
			wxString var_name;
			wxString months[] = { "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec" };
			bool overwrite = true;
			// "cr" is for unused coincident rate
			double cr;
			matrix_t<float> cr_tou_mat(72, 4); // will resize
			int cr_tou_row = 0;
			int cr_row = 0;
			/*ndx = upgrade_list.Index("ur_enable_net_metering");
			int nm = 1; // default to net metering
			if (ndx > -1 && ndx < (int)upgrade_value.Count())
				nm = (int)atof(upgrade_value[ndx].c_str());*/
			int metering_option = 0;
			ndx = upgrade_list.Index("ur_metering_option");
			if (ndx > -1 && ndx < (int)upgrade_value.Count())
				metering_option = (int)atof(upgrade_value[ndx].c_str());
			double flat_buy_rate = 0;
			double flat_sell_rate = 0;
			ndx = upgrade_list.Index("ur_flat_buy_rate");
			if (ndx > -1 && ndx < (int)upgrade_value.Count())
				flat_buy_rate = atof(upgrade_value[ndx].c_str());
			ndx = upgrade_list.Index("ur_flat_sell_rate");
			if (ndx > -1 && ndx < (int)upgrade_value.Count())
				flat_sell_rate = atof(upgrade_value[ndx].c_str());
			//if (nm > 0) flat_sell_rate = flat_buy_rate;
			// energy charge matrix inputs
			for (int per=1;per<13 && overwrite;per++)
			{
				for (int tier=1;tier<7 && overwrite;tier++)
				{
			// ec tou
					br = -1;
					sr = -1;
					ub = -1;
					per_tier = wxString::Format("ur_ec_p%d_t%d_", per, tier);
					ndx = upgrade_list.Index(per_tier + "br");
					if (ndx > -1 && ndx < (int)upgrade_value.Count())
						br = atof(upgrade_value[ndx].c_str());
					else
						overwrite = false;
					ndx = upgrade_list.Index(per_tier + "sr");
					if (ndx > -1 && ndx < (int)upgrade_value.Count())
						sr = atof(upgrade_value[ndx].c_str());
					else
						overwrite = false;
					//if (nm > 0) sr = br;
					ndx = upgrade_list.Index(per_tier + "ub");
					if (ndx > -1 && ndx < (int)upgrade_value.Count())
						ub = atof(upgrade_value[ndx].c_str());
					else
						overwrite = false;
					if (!overwrite) continue;
					if (sr > 0 || br > 0 || ec_tou_row == 0) // must have one row
					{
						if (sr == 0 && br == 0 && ec_tou_row == 0)
						{
							br = flat_buy_rate;
							sr = flat_sell_rate;
							ub = 1e38;
						}
						ec_tou_mat.at(ec_tou_row,0)=per;
						ec_tou_mat.at(ec_tou_row,1) = tier;
						ec_tou_mat.at(ec_tou_row,2) = ub;
						ec_tou_mat.at(ec_tou_row,3) = 0; // units
						ec_tou_mat.at(ec_tou_row,4) = br;
						ec_tou_mat.at(ec_tou_row,5) = sr;
						ec_tou_row++;
					}
			// demand tou
					dc = -1;
					ub = -1;
					per_tier = wxString::Format("ur_dc_p%d_t%d_", per, tier);
					ndx = upgrade_list.Index(per_tier + "dc");
					if (ndx > -1 && ndx < (int)upgrade_value.Count())
						dc = atof(upgrade_value[ndx].c_str());
					else
						overwrite = false;
					ndx = upgrade_list.Index(per_tier + "ub");
					if (ndx > -1 && ndx < (int)upgrade_value.Count())
						ub = atof(upgrade_value[ndx].c_str());
					else
						overwrite = false;
					if (!overwrite) continue;
					if (dc > 0 || dc_tou_row == 0) // must have one row
					{
						dc_tou_mat.at(dc_tou_row,0)=per;
						dc_tou_mat.at(dc_tou_row,1)=tier;
						dc_tou_mat.at(dc_tou_row,2)=ub;
						dc_tou_mat.at(dc_tou_row,3)=dc;
						dc_tou_row++;
					}
			// unused coincident rate tou
					dc = -1;
					ub = -1;
					per_tier = wxString::Format("ur_cr_p%d_t%d_", per, tier);
					ndx = upgrade_list.Index(per_tier + "dc");
					if (ndx > -1 && ndx < (int)upgrade_value.Count())
						dc = atof(upgrade_value[ndx].c_str());
					else
						overwrite = false;
					ndx = upgrade_list.Index(per_tier + "ub");
					if (ndx > -1 && ndx < (int)upgrade_value.Count())
						ub = atof(upgrade_value[ndx].c_str());
					else
						overwrite = false;
					if (!overwrite) continue;
					if (dc > 0 || cr_tou_row == 0) // must have one row
					{
						cr_tou_mat.at(cr_tou_row, 0) = per;
						cr_tou_mat.at(cr_tou_row, 1) = tier;
						cr_tou_mat.at(cr_tou_row, 2) = ub;
						cr_tou_mat.at(cr_tou_row, 3) = dc;
						cr_tou_row++;
					}
					// flat demand
					dc = -1;
					ub = -1;
					per_tier = wxString::Format("ur_dc_%s_t%d_", months[per - 1], tier);
					ndx = upgrade_list.Index(per_tier + "dc");
					if (ndx > -1 && ndx < (int)upgrade_value.Count())
						dc = atof(upgrade_value[ndx].c_str());
					else
						overwrite = false;
					ndx = upgrade_list.Index(per_tier + "ub");
					if (ndx > -1 && ndx < (int)upgrade_value.Count())
						ub = atof(upgrade_value[ndx].c_str());
					else
						overwrite = false;
					if (!overwrite) continue;
					if (dc > 0 || tier == 1) // must have one value for each month
						{
						dc_flat_mat.at(dc_flat_row,0)=per-1; // month index
						dc_flat_mat.at(dc_flat_row,1)=tier;
						dc_flat_mat.at(dc_flat_row,2)=ub;
						dc_flat_mat.at(dc_flat_row,3)=dc;
						dc_flat_row++;
					}
				}
			}
			// resize matrices
			if (overwrite)
			{
				ec_tou_mat.resize_preserve(ec_tou_row, 6, 0);
				dc_tou_mat.resize_preserve(dc_tou_row, 4, 0);
				dc_flat_mat.resize_preserve(dc_flat_row, 4, 0);
				var_name = "ur_ec_tou_mat";
				if (VarValue *vv = c->Values().Get(var_name))
				{
					vv->Set(ec_tou_mat);
					list.Add(var_name);
				}
				var_name = "ur_dc_tou_mat";
				if (VarValue *vv = c->Values().Get(var_name))
				{
					vv->Set(dc_tou_mat);
					list.Add(var_name);
				}
				var_name = "ur_cr_tou_mat";
				if (VarValue* vv = c->Values().Get(var_name))
				{
					vv->Set(cr_tou_mat);
					list.Add(var_name);
				}
				var_name = "ur_dc_flat_mat";
				if (VarValue *vv = c->Values().Get(var_name))
				{
					vv->Set(dc_flat_mat);
					list.Add(var_name);
				}
			}
		}

		// this causes the UI and other variables to be updated
		c->VariablesChanged( list );
	}

	cxt.result().assign( ret_val ? 1.0 : 0.0 );
}

static bool copy_mat(lk::invoke_t &cxt, wxString sched_name, matrix_t<double> &mat)
{
	lk::vardata_t &val = cxt.result().hash_item(sched_name);
	if ((mat.nrows() > 0) && (mat.ncols() >0))
	{
		val.empty_vector();
		size_t nrows = mat.nrows();
		size_t ncols = mat.ncols();
		val.resize(nrows);
		for (size_t r = 0; r<nrows; r++)
		{
			lk::vardata_t *row = val.index(r);
			row->empty_vector();
			row->resize(ncols);
			for (size_t c = 0; c<ncols; c++)
				row->index(c)->assign(mat(r, c));
		}
	}
	return true;
}

void fcall_urdb_get(lk::invoke_t &cxt)
{
	LK_DOC("urdb_get", "Returns rate data from the OpenEI Utility Rate Database given a GUID.", "(string:guid):table");
	wxString guid = cxt.arg(0).as_string();
	if (guid.IsEmpty()) return;

	OpenEI::RateData rate;
	OpenEI api;

    wxString rate_notes;

	if (api.RetrieveUtilityRateData(guid, rate))
	{
		cxt.result().empty_hash();

        // meta data
        cxt.result().hash_item("utility").assign(rate.Header.Utility);
        cxt.result().hash_item("name").assign(rate.Header.Name);
        cxt.result().hash_item("source").assign(rate.Header.Source);
        cxt.result().hash_item("description").assign(rate.Header.Description);
        cxt.result().hash_item("start_date").assign(rate.Header.StartDate);
        cxt.result().hash_item("end_date").assign(rate.Header.EndDate);
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

		// unused items
		cxt.result().hash_item("hasunuseditems").assign(rate.Unused.HasUnusedItems);
		cxt.result().hash_item("isdefault").assign(rate.Unused.IsDefault);
		cxt.result().hash_item("servicetype").assign(rate.Unused.ServiceType);
		cxt.result().hash_item("demandwindow").assign(rate.Unused.DemandWindow);
		for (int i = 0; i < 12; i++)
		{
			cxt.result().hash_item(wxString::Format("fueladjustmentsmonthly%d", i)).assign(rate.Unused.FuelAdjustmentsMonthly[i]);
			cxt.result().hash_item(wxString::Format("demandratchetpercentage%d", i)).assign(rate.Unused.DemandRatchetPercentage[i]);
		}
		if (!applydiurnalschedule(cxt, "cr_sched", rate.Unused.CoincidentSchedule)) return;
		if (!copy_mat(cxt, "cr_tou_mat", rate.Unused.CoincidentRateStructure)) return;

		cxt.result().hash_item("energyattrs").assign(rate.Unused.EnergyAttrs);
		cxt.result().hash_item("demandattrs").assign(rate.Unused.DemandAttrs);
		cxt.result().hash_item("fixedattrs").assign(rate.Unused.FixedAttrs);

		// URLs
		cxt.result().hash_item("rateurl").assign(rate.Header.RateURL);
		cxt.result().hash_item("jsonurl").assign(rate.Header.JSONURL);

		// metering option
		// "Net Metering", "Net Billing Instantaneous", "Net Billing Hourly", or "Buy All Sell All"
		if (rate.DgRules == "Net Metering")
			cxt.result().hash_item("metering_option").assign(0.0);
		else if (rate.DgRules == "Net Billing Instantaneous")
			cxt.result().hash_item("metering_option").assign(2.0);
		else if (rate.DgRules == "Net Billing Hourly")
			cxt.result().hash_item("metering_option").assign(2.0);
		else if (rate.DgRules == "Buy All Sell All")
			cxt.result().hash_item("metering_option").assign(4.0);
        else // set to default Net Energy Metering
        {
            cxt.result().hash_item("metering_option").assign(0.0);
            rate_notes.append(wxString::Format("Metering option not provided with rate data.\n"));
        }

		// fixed charges
		//  "$/day", "$/month" or "$/year" TO DO handle $/day and $/year
        double fixed_charges = rate.FixedChargeFirstMeter + rate.FixedChargeAddlMeter;
        cxt.result().hash_item("monthly_fixed_charge").assign(0.0);
        if ((rate.FixedChargeUnits) == "$/month")
            cxt.result().hash_item("monthly_fixed_charge").assign(fixed_charges);
        else if ( fixed_charges > 0 )
            rate_notes.append(wxString::Format("SAM does not model fixed charge rate of %f with %s units.\n", fixed_charges, rate.FixedChargeUnits));

		// minimum charges
		// "$/day", "$/month" or "$/year" TO DO handle $/day
        cxt.result().hash_item("monthly_min_charge").assign(0.0);
        cxt.result().hash_item("annual_min_charge").assign(0.0);
        if (rate.MinChargeUnits == "$/month")
			cxt.result().hash_item("monthly_min_charge").assign(rate.MinCharge);
		else if ( rate.MinChargeUnits == "$/year")
			cxt.result().hash_item("annual_min_charge").assign(rate.MinCharge);
        else if (rate.MinCharge > 0 )
            rate_notes.append(wxString::Format("SAM does not model minimum charge rate of %f with %s units.\n", rate.MinCharge, rate.MinChargeUnits));

		// schedules
		if (!applydiurnalschedule(cxt, "ec_sched_weekday", rate.EnergyWeekdaySchedule)) return;
		if (!applydiurnalschedule(cxt, "ec_sched_weekend", rate.EnergyWeekendSchedule)) return;

		if (!applydiurnalschedule(cxt, "dc_sched_weekday", rate.DemandWeekdaySchedule)) return;
		if (!applydiurnalschedule(cxt, "dc_sched_weekend", rate.DemandWeekendSchedule)) return;

		cxt.result().hash_item("ec_enable").assign(1.0);
		if (!copy_mat(cxt, "ec_tou_mat", rate.EnergyStructure)) return;

		cxt.result().hash_item("dc_enable").assign(1.0);
		if (!copy_mat(cxt, "dc_flat_mat", rate.DemandFlatStructure)) return;
		if (!copy_mat(cxt, "dc_tou_mat", rate.DemandTOUStructure)) return;

        cxt.result().hash_item("ratenotes").assign(rate_notes);

	}
}

void fcall_openeiutilityrateform(lk::invoke_t &cxt)
{
	LK_DOC("openeiutilityrateform", "Returns a list of utility company names from OpenEI.org", "( NONE ):ARRAY");
	CaseCallbackContext &cc = *static_cast<CaseCallbackContext*>(cxt.user_data());
	wxString market = cc.GetCase().GetFinancing();

	OpenEIUtilityRateDialog openei(SamApp::Window(), "OpenEI Utility Rate Database", market);
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
/*
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
*/
static void copy_matts(lk::vardata_t &val, matrix_t<float> &mts)
{
	if (((mts.nrows() % 8760) == 0) && (mts.ncols() >0))
	{
		val.empty_vector();
		size_t nrows = mts.nrows();
		size_t ncols = mts.ncols();
		val.resize(nrows);
		for (size_t r = 0; r<nrows; r++)
		{
			lk::vardata_t *row = val.index(r);
			row->empty_vector();
			row->resize(ncols);
			for (size_t c = 0; c<ncols; c++)
				row->index(c)->assign(mts(r, c));
		}
	}
}


void fcall_editscene3d(lk::invoke_t &cxt)
{
	LK_DOC("editscene3d", "Loads the 3D scene editor for a given 3D scene variable name.", "(string:variable, number:lat, number:lon, number:tz, string:location, number:minute_step,[bool:use_groups]):table");
	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();

	bool use_groups = false;
	if (cxt.arg_count() > 6)
		use_groups = cxt.arg(6).as_boolean();
	int min_step = cxt.arg(5).as_integer();

	cxt.result().empty_hash();

	wxString name(cxt.arg(0).as_string());
	VarValue *vv = cc.GetCase().Values().Get(name);
	if (!vv)
	{
		cxt.result().hash_item("ierr").assign(1.0);
		cxt.result().hash_item("message").assign(wxString("no variable with that name"));
		return;
	}


	if (VV_BINARY != vv->Type())
		vv->SetType(VV_BINARY);
	wxMemoryBuffer &bin = vv->Binary();

	wxLogStatus("EDIT SCENE (%s): loaded %d bytes", (const char*)name.c_str(), (int)bin.GetDataLen());

	wxDialog dlg(SamApp::Window(), wxID_ANY, "Edit 3D Shading Scene", wxDefaultPosition, wxScaleSize(800, 600), wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);
	ShadeTool *st = new ShadeTool(&dlg, wxID_ANY);

	if (cxt.arg_count() > 1 && bin.GetDataLen() == 0)
	{
		// only update location on the first time
		double lat = cxt.arg(1).as_number();
		double lon = cxt.arg(2).as_number();
		double tz = cxt.arg(3).as_number();
		wxString addr = cxt.arg(4).as_string();
		st->GetLocationSetup()->SetLocation(addr, lat, lon, tz);

		wxMessageBox("The 3D scene requires detailed information about your location to calculate shading losses.\n\n"
			"By default, information about the location you selected in the weather file has been transferred.\n\n"
			"If you update your weather file in the future, please manually ensure that the address, "
			"latitude, longitude, and time zone in the 3D scene editor (Location tab) are updated as necessary.", "Notice",
			wxICON_INFORMATION | wxOK, SamApp::Window());
	}

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	sizer->Add(st, 1, wxALL | wxEXPAND, 0);
	dlg.SetSizer(sizer);
	if (bin.GetDataLen() > 0)
	{
		wxMemoryInputStream in(bin.GetData(), bin.GetDataLen());
		if (!st->Read(in))
			wxMessageBox("Error loading stored 3D scene data.");
	}

	double lat, lon, tz;
	st->GetLocationSetup()->GetLocation(&lat, &lon, &tz);
	if (lat != cxt.arg(1).as_number()
		|| lon != cxt.arg(2).as_number()
		|| tz != cxt.arg(3).as_number())
	{
		if (wxYES == wxMessageBox("The location information in the shading tool does not match the currently selected weather file.\n\nDo you want to update your location settings in the shading tool to match?", "Query", wxYES_NO))
		{
			lat = cxt.arg(1).as_number();
			lon = cxt.arg(2).as_number();
			tz = cxt.arg(3).as_number();
			wxString addr = cxt.arg(4).as_string();
			st->GetLocationSetup()->SetLocation(addr, lat, lon, tz);
		}
	}

	int retval = dlg.ShowModal();
	// 5100 == SAVE and CLOSE, 5101 = X
	if (retval == 5101)
	{
		cxt.result().hash_item("ierr").assign(-1.0);
		cxt.result().hash_item("message").assign(wxString("closed and do not apply"));
		return;
	}
	else
	{
		//wxMessageBox(wxString::Format("dialog close value = %d", retval));
		wxMemoryOutputStream out;
		st->Write(out);
		size_t len = out.GetSize();
		if (len > 0)
		{
			bin.Clear();
			void *buf = bin.GetWriteBuf(len);
			out.CopyTo(buf, len);
			bin.UngetWriteBuf(len);
			SamApp::Window()->Project().SetModified(true);
			// refresh any changes - fixes issue of following analysis differences when updating scene
			wxMemoryInputStream in(bin.GetData(), bin.GetDataLen());
			if (!st->Read(in))
				wxMessageBox("Error loading stored 3D scene data.");
		}



		// Timeseries


		std::vector<ShadeTool::shadets> shadets;
		if (st->SimulateTimeseries(min_step, shadets, use_groups) && shadets.size() > 0)
		{
			wxArrayInt order1;
			wxArrayString part1;
			if (!use_groups) // use overal shading factor
			{
				// overall losses for the system are always in table 0
				lk::vardata_t &v = cxt.result().hash_item("losses");
				matrix_t<float> mat_ts(shadets[0].ts.size(), 1);
				for (size_t i = 0; i < shadets[0].ts.size(); i++)
					mat_ts.at(i, 0) = shadets[0].ts[i];
				copy_matts(v, mat_ts);
				order1.push_back(0);
			}
			else
			{
				// now copy over all the subarray sections
				// the user must label them as 'Subarray1.string1', 'Subarray1.string2', etc for them to get placed in the right section

				// return in order of subarray 1, subarray 2, subarray 3, subarray 4 for application in ui
				// first parse and group first part of names
				for (size_t i = 0; i < shadets.size(); i++)
				{

					wxString name = shadets[i].name.Lower();
					wxArrayString p1p2 = wxSplit(name, '.');
					if (p1p2.Count() > 0)
						name = p1p2[0];

					if (part1.Count() < 1) part1.push_back(name);

					if (part1.Index(name) == wxNOT_FOUND)
						part1.push_back(name);
				}
				// TODO: here can check for at most 4 subarrays
				for (size_t i = 0; i < part1.Count(); i++)
					order1.push_back(i);
				// check group names and reorder if necessary
				wxArrayInt tmp_order;
				int j = 0;
				while ((tmp_order.Count() < order1.Count()) && (j < 100)) // allow for double digit numbering
				{
					// Index does not work with null values,
					for (size_t i = 0; i < part1.size(); i++)
					{
						wxString name = part1[i];
						if (name.Find(wxString::Format("%d", j)) != wxNOT_FOUND)
						{
							if (tmp_order.Count() < 1) tmp_order.push_back(i);
							if (tmp_order.Index(i) == wxNOT_FOUND)
								tmp_order.push_back(i);
						}
					}
					j++;
				}
				if (tmp_order.Count() == order1.Count())
				{
					for (size_t i = 0; i < order1.size(); i++)
						order1[i] = tmp_order[i];
				}
				// the callback only considers first four subarrays


				wxArrayInt order2;
				wxArrayString part2;
				wxArrayInt sort_order2;
				// construct matrices for each
				for (size_t io1 = 0; io1 < order1.Count(); io1++)
				{
					size_t nrows = 8760;
					size_t ncols = 1;
					order2.Clear();
					part2.Clear();

					for (size_t its = 0; its < shadets.size(); its++)
					{
						wxString name = shadets[its].name.Lower();
						wxArrayString p1p2 = wxSplit(name, '.');
						if (p1p2.Count() < 1) continue;
						if (p1p2[0] == part1[order1[io1]])  // part of this sub array
						{
							order2.push_back(its); // save index
							nrows = shadets[its].ts.size(); // number of rows for matrix should be 8760 * 60/ num_min;
							// can save string names and reorder
							if (p1p2.Count() > 1) // parallel strings
								name = p1p2[1];
							// null comparisons do not work using Index!
							if (part2.Count() < 1) part2.push_back(name);
							if (part2.Index(name) == wxNOT_FOUND)
								part2.push_back(name);
						}
					}


					// check string names and reorder if necessary
					// up to 8 parallel strings considered in callback
					sort_order2.Clear();
					int j = 0;
					while ((sort_order2.Count() < order2.Count()) && (j < 100)) // allow for double digit numbering
					{
						for (size_t i = 0; i < part2.size(); i++)
						{
							wxString name = part2[i];
							if (name.Find(wxString::Format("%d", j)) != wxNOT_FOUND)
							{
								if (sort_order2.Index(i) == wxNOT_FOUND)
									sort_order2.push_back(i);
							}
						}
						j++;
					}

					if (sort_order2.Count() != order2.Count())
					{
						sort_order2.Clear();
						for (size_t i = 0; i < order2.size(); i++)
							sort_order2.push_back(i);
					}


					ncols = order2.size();
					matrix_t<float> mat_ts(nrows, ncols);
					for (size_t c = 0; c < order2.Count(); c++)
						for (size_t r = 0; r < shadets[order2[sort_order2[c]]].ts.size(); r++)
							mat_ts.at(r, c) = shadets[order2[sort_order2[c]]].ts[r];


					//	set names for subarray processing in callback independent of what set in scene 3d but order is preserved.
					wxString name;
					name.Printf("subarray%d", (int)(io1 + 1));

					lk::vardata_t &sec = cxt.result().hash_item(name);
					copy_matts(sec, mat_ts);
				}
			}

			// diffuse
			std::vector<ShadeTool::diffuse> diffuse;
			if (st->SimulateDiffuse(diffuse, use_groups) && diffuse.size() > 0)
			{
				if (diffuse.size() != shadets.size()) // TODO how to combine diffuse for parallel strings
				{
					cxt.result().hash_item("ierr").assign(4.0);
					cxt.result().hash_item("message").assign(wxString("Error in simulation of diffuse shading factors not equal to diurnal timeseries count."));
				}
				else
				{
					if (!use_groups)
					{
						if (diffuse.size() != 1 || order1.Count() != 1)
						{
							cxt.result().hash_item("ierr").assign(4.0);
							cxt.result().hash_item("message").assign(wxString::Format("Error in simulation of diffuse shading factors %d not equal 1.", (int)diffuse.size()));
						}
						else
						{
							lk::vardata_t &ds = cxt.result().hash_item("diffuse");
							ds.empty_vector();
							ds.vec()->reserve(diffuse.size());
							for (size_t i = 0; i < order1.Count(); i++)
							{
								ds.vec_append(diffuse[order1[i]].shade_percent);
							}
						}
					}
					else
					{
						lk::vardata_t &ds = cxt.result().hash_item("diffuse");
						ds.empty_vector();
						ds.vec()->reserve(diffuse.size());
						// group and calculate for each subarray
						for (size_t io1 = 0; io1 < order1.Count(); io1++)
						{
							double average = 0;
							double count = 0;
							for (size_t its = 0; its < diffuse.size(); its++)
							{
								wxString name = diffuse[its].name.Lower();
								wxArrayString p1p2 = wxSplit(name, '.');
								if (p1p2.Count() < 1) continue;
								if (p1p2[0] == part1[order1[io1]])  // part of this sub array
								{
									average += diffuse[its].shade_factor;
									count += diffuse[its].shade_count;
								}
							}
							if (count > 0)
								average /= count;
							else
								average = 0;
							ds.vec_append(average);
						}
					}
				}
			}
			else
			{
				cxt.result().hash_item("ierr").assign(3.0);
				cxt.result().hash_item("message").assign(wxString("Error in simulation of diffuse shading factors."));
			}
			cxt.result().hash_item("ierr").assign(0.0);
			cxt.result().hash_item("nsubarrays").assign((double)(order1.Count()));
		}
		else
		{
			cxt.result().hash_item("ierr").assign(2.0);
			cxt.result().hash_item("message").assign(wxString("Error in simulation of timeseries shading factors."));
		}
	}
}


void fcall_showsettings( lk::invoke_t &cxt )
{
	LK_DOC("showsettings", "Show the settings dialog for either 'solar', 'wind', or 'wave' data files.", "(string:type):boolean");
	wxString type( cxt.arg(0).as_string().Lower() );
    if (type == "solar") cxt.result().assign(ShowSolarResourceDataSettings() ? 1.0 : 0.0);
    else if (type == "wind") cxt.result().assign(ShowWindResourceDataSettings() ? 1.0 : 0.0);
    else if (type == "wave") cxt.result().assign(ShowWaveResourceDataSettings() ? 1.0 : 0.0);
    else if (type == "tidal_device") cxt.result().assign(ShowTidalDeviceDataSettings() ? 1.0 : 0.0);
}

void fcall_rescanlibrary( lk::invoke_t &cxt )
{
	LK_DOC("rescanlibrary", "Rescan the indicated resource data library ('solar' or 'wind' or 'wave' or 'wave_ts') and update any library widgets.", "(string:type):boolean");
	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();

	wxString type(cxt.arg(0).as_string().Lower());
	Library *reloaded = 0;

	if ( type == "solar" )
	{
		wxString solar_resource_db = SamApp::GetUserLocalDataDir() + "/SolarResourceData.csv";
		ScanSolarResourceData( solar_resource_db, true );
		reloaded = Library::Load( solar_resource_db );
	}
	else if (type == "wind")
	{
		wxString wind_resource_db = SamApp::GetUserLocalDataDir() + "/WindResourceData.csv";
		ScanWindResourceData(wind_resource_db, true);
		reloaded = Library::Load(wind_resource_db);
	}
	else if (type == "wave")
	{
		wxString wave_resource_db = SamApp::GetUserLocalDataDir() + "/WaveResourceData.csv";
		ScanWaveResourceData(wave_resource_db, true);
		reloaded = Library::Load(wave_resource_db);
	}
    else if (type == "wave_ts")
    {
        wxString wave_resource_ts_db = SamApp::GetUserLocalDataDir() + "/WaveResourceTSData.csv";
        wxString wave_resource_db = SamApp::GetRuntimePath() + "../wave_resource/test_time_series_jpd.csv";
        ScanWaveResourceTSData(wave_resource_ts_db, true);
        //WaveResourceTSData_makeJPD(wave_resource_db, true);
        reloaded = Library::Load(wave_resource_ts_db);
        //reloaded2 = Library::Load(wave_resource_db);
    }

    else if (type == "tidal_device")
    {
        wxString tidal_device_db = SamApp::GetUserLocalDataDir() + "/TidalEnergyConverters.csv";
        ScanTidalConverterData(tidal_device_db, true);
        reloaded = Library::Load(tidal_device_db);
    }


	if ( reloaded != 0 )
	{
		if (&cc != NULL) {
			std::vector<wxUIObject*> objs = cc.InputPage()->GetObjects();
			for (size_t i = 0; i < objs.size(); i++)
				if (LibraryCtrl* lc = objs[i]->GetNative<LibraryCtrl>())
					lc->ReloadLibrary();
		}
	}

}

void fcall_librarygetcurrentselection(lk::invoke_t &cxt)
{
	LK_DOC("librarygetcurrentselection", "Return the text of the current selection for the library specified", "(string:libraryctrlname):string");
	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();
	lk_string ret_val = "";

	wxString name(cxt.arg(0).as_string().Lower());
	std::vector<wxUIObject*> objs = cc.InputPage()->GetObjects();
	for (size_t i = 0; i < objs.size(); i++)
		if (LibraryCtrl *lc = objs[i]->GetNative<LibraryCtrl>())
		{
			if (objs[i]->GetName().Lower() == name)
			{
				ret_val = lc->GetEntrySelection();
				break;
			}
		}
	cxt.result().assign(ret_val);
}

void fcall_librarygetfiltertext(lk::invoke_t &cxt)
{
	LK_DOC("librarygetfiltertext", "Return the text of the search string for the library on the", "(string:libraryctrlname):string");
	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();
	lk_string ret_val = "";

	wxString name(cxt.arg(0).as_string().Lower());
	std::vector<wxUIObject*> objs = cc.InputPage()->GetObjects();
	for (size_t i = 0; i < objs.size(); i++)
		if (LibraryCtrl *lc = objs[i]->GetNative<LibraryCtrl>())
		{
			if (objs[i]->GetName().Lower() == name)
			{
				ret_val = lc->GetFilterText();
				break;
			}
		}
	cxt.result().assign(ret_val);
}


void fcall_librarygetnumbermatches(lk::invoke_t &cxt)
{
	LK_DOC("librarygetnumbermatches", "Return the number of library items matching the search string for the library specified", "(string:libraryctrlname):number");
	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();
	double ret_val = 0;

	wxString name(cxt.arg(0).as_string().Lower());
	std::vector<wxUIObject*> objs = cc.InputPage()->GetObjects();
	for (size_t i = 0; i < objs.size(); i++)
		if (LibraryCtrl *lc = objs[i]->GetNative<LibraryCtrl>())
		{
			if (objs[i]->GetName().Lower() == name)
			{
				ret_val = (double)lc->GetNumberMatches();
				break;
			}
		}
	cxt.result().assign(ret_val);
}


void fcall_librarynotifytext(lk::invoke_t &cxt)
{
	LK_DOC("librarynotifytext", "Gets or sets the notify string for the library specified", "(string:libraryctrlname, [string:notifytext]):string");
	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();
	wxString ret_val = "";

	wxString name(cxt.arg(0).as_string().Lower());
	std::vector<wxUIObject*> objs = cc.InputPage()->GetObjects();
	for (size_t i = 0; i < objs.size(); i++)
		if (LibraryCtrl *lc = objs[i]->GetNative<LibraryCtrl>())
		{
			if (objs[i]->GetName().Lower() == name)
			{
				if (cxt.arg_count() == 2)
				{
					wxString str = cxt.arg(1).as_string();
					lc->SetNotifyText(str);
				}
				ret_val = lc->GetNotifyText();
				break;
			}
		}
	cxt.result().assign(ret_val);
}

// threading ported over from lk to use lhs

// async thread function
lk::vardata_t sam_async_thread( lk::invoke_t cxt, lk::bytecode lkbc, lk_string lk_result, lk_string input_name, lk::vardata_t input_value)
{
	lk::vardata_t ret_hash;
	ret_hash.empty_hash();

	lk_string err_str, parse_time, env_time, bc_time, vminit_time, vmrun_time, rt_time;

//
	std::chrono::system_clock::time_point start = std::chrono::system_clock::now();

	lk::env_t myenv(cxt.env());
	myenv.clear_objs();
	myenv.clear_vars();

	auto end = std::chrono::system_clock::now();
	auto diff = std::chrono::duration_cast < std::chrono::milliseconds > (end - start).count();
	env_time = " Env time: " + std::to_string(diff) + "ms ";


	lk::vm myvm;
	lk::bytecode bc(lkbc); // can explicitly copy if in doubt
//
	start = std::chrono::system_clock::now();

	// Get string value of "ASSIGN|VALUE|HERE" set in _async and then update to input value
	size_t ndx_c = bc.constants.size() + 1;
	for (size_t i = 0; i < bc.constants.size(); i++)
	{
		if (bc.constants[i].type() == lk::vardata_t::STRING)
		{
			if (bc.constants[i].as_string() == "ASSIGN|VALUE|HERE")
			{
				ndx_c = i;
				break;
			}
		}
	}

	if (ndx_c > bc.constants.size())
	{
		err_str = "async_thread: No value found to change.";
		ret_hash.hash_item("error", err_str);
		return ret_hash;
	}
	else
	  bc.constants[ndx_c] = input_value;

	myvm.load(&bc);
	myvm.initialize(&myenv);

	end = std::chrono::system_clock::now();
	diff = std::chrono::duration_cast < std::chrono::milliseconds > (end - start).count();
	vminit_time = " vm init time: " + std::to_string(diff) + "ms ";
	start = std::chrono::system_clock::now();

		bool ok1 = myvm.run();
//
	end = std::chrono::system_clock::now();
	diff = std::chrono::duration_cast < std::chrono::milliseconds > (end - start).count();
	vmrun_time = " vm run time: " + std::to_string(diff) + "ms ";

		if (ok1)
		{

//
	start = std::chrono::system_clock::now();

			wxArrayString list = wxSplit(lk_result, ',');
			for (size_t i_res = 0; i_res < list.size(); i_res++)
			{

				lk::vardata_t *vd = myenv.lookup(list[i_res], true);
				if (vd)
				{
					ret_hash.hash_item(list[i_res], *vd);
				}
				else
				{
					size_t nfrm;
					lk::vardata_t *v;
					lk::vm::frame **frames = myvm.get_frames(&nfrm);
					bool found = false;
					for (size_t i_frm = 0; (i_frm < nfrm) && !found; i_frm++)
					{
						lk::vm::frame &F = *frames[nfrm - i_frm - 1];
						if ((v = F.env.lookup(list[i_res], true)) != NULL)
						{
							ret_hash.hash_item(list[i_res], *v);
							found = true;
						}
					}
					if (!found) // look in byte code constants collection
					{
						size_t ndx_i = std::find(myvm.get_bytecode()->identifiers.begin(), myvm.get_bytecode()->identifiers.end(), list[i_res]) - myvm.get_bytecode()->identifiers.begin();
						if (ndx_i < myvm.get_bytecode()->identifiers.size())
							err_str += list[i_res] + wxString::Format(" in bytecode at identifier index %d\n", (int)ndx_i);
						else
							err_str += list[i_res] + " lookup error\n";
					}
				}
			} // for i_res
		}
		else
		{
			err_str += ("error running vm: " + myvm.error());
		}
		end = std::chrono::system_clock::now();
		diff = std::chrono::duration_cast < std::chrono::milliseconds > (end - start).count();
		rt_time = " result lookup time: " + std::to_string(diff) + "ms ";
		ret_hash.hash_item("thread time : ", parse_time + env_time + bc_time + vminit_time + vmrun_time + rt_time);

	ret_hash.hash_item("error", err_str);
	return ret_hash;
}



static void fcall_sam_async( lk::invoke_t &cxt )
{
	LK_DOC("sam_async", "For running function in a thread using std::async.", "(string: file containg lk code to run in separate thread, string: variable to set for each separate thread, vector: arguments for variable for each thread, [string: name of result variable to get from threaded script, string: global variable to set, lk value of global]");
	// wrapper around std::async to run functions with argument
	// will use std::promise, std::future in combination with std::package or std::async
	//lk_string func_name = cxt.arg(0).as_string();

// checking for bottlenecks
//	std::chrono::system_clock::time_point start = std::chrono::system_clock::now();
	lk_string err_str = "";// , file_time, parse_time, bc_time, add_input_time, loop_time;
	cxt.result().empty_hash();

	lk_string fn = cxt.arg(0).as_string();
	FILE *fp = fopen(fn.c_str(), "r");
	if (!fp)
	{
		err_str += "No valid input file " + fn + "specified\n";
		cxt.result().hash_item("error", err_str);
		return;
	}

	lk_string file_contents;
	char buf[1024];
	while (fgets(buf, 1023, fp) != 0)
		file_contents += buf;
	fclose(fp);

//
//	auto end = std::chrono::system_clock::now();
//	auto diff = std::chrono::duration_cast < std::chrono::milliseconds > (end - start).count();
//	file_time = " File time: " + std::to_string(diff) + "ms ";

// additional input time
//	start = std::chrono::system_clock::now();

// add input value
	// required input - changes in each thread
	lk_string input_name = cxt.arg(1).as_string();
	lk_string value = "\"ASSIGN|VALUE|HERE\";\n";
	file_contents = input_name + "=" + value + file_contents;

	// optional global additional input - e.g. hash for pvrpm same for all threads
	if (cxt.arg_count() > 5)
	{
		lk_string add_input_name = cxt.arg(4).as_string();
		value = "\"ADDITIONALASSIGN|VALUE|HERE\";\n";
		lk::vardata_t add_input_value = cxt.arg(5);
		file_contents = add_input_name + "=" + value + file_contents;
	}


// file contents parsed to node_t
// checking for bottlenecks
//	start = std::chrono::system_clock::now();

	lk::input_string p(file_contents);
	lk::parser parse(p);
	smart_ptr<lk::node_t>::ptr tree(parse.script());
	int i = 0;
	while (i < parse.error_count())
		err_str += lk_string(parse.error(i++)) + "\n";

	if (parse.token() != lk::lexer::END)
		err_str += "parsing did not reach end of input\n";

	if (err_str.Len() > 0)
	{
		cxt.result().hash_item("error", err_str);
		return;
	}
//
//	end = std::chrono::system_clock::now();
//	diff = std::chrono::duration_cast < std::chrono::milliseconds > (end - start).count();
//	parse_time = " Parse time: " + std::to_string(diff) + "ms ";


// bytecode time
//	start = std::chrono::system_clock::now();

	lk::bytecode bc;
	lk::codegen cg;
	if (cg.generate(tree.get()))
		cg.get(bc);
	else
	{
		err_str += "bytecode not generated.\n";
		cxt.result().hash_item("error", err_str);
		return;
	}
//
//	end = std::chrono::system_clock::now();
//	diff = std::chrono::duration_cast < std::chrono::milliseconds > (end - start).count();
//	bc_time = " bytecode time: " + std::to_string(diff) + "ms ";


//	start = std::chrono::system_clock::now();

// additional common inputs; e.g., meta hash for pvrpm

	if (cxt.arg_count() > 5)
	{
		// replace bc.constants with updated lk:vardata_t input value
		value = "ADDITIONALASSIGN|VALUE|HERE";
		size_t ndx_c = bc.constants.size() + 1;
		for (size_t i = 0; i < bc.constants.size(); i++)
		{
			if (bc.constants[i].type() == lk::vardata_t::STRING)
			{
				if (bc.constants[i].as_string() == value)
				{
					ndx_c = i;
					break;
				}
			}
		}
		if (ndx_c > bc.constants.size())
		{
			err_str += "_async: No additional input value found to change.\n";
		}
		else
		{
			lk::vardata_t add_input_value = cxt.arg(5);
			bc.constants[ndx_c] = add_input_value;
		}
	}

	if (err_str.Len() > 0)
	{
		cxt.result().hash_item("error", err_str);
		return;
	}



//	end = std::chrono::system_clock::now();
//	diff = std::chrono::duration_cast < std::chrono::milliseconds > (end - start).count();
//	add_input_time = " bytecode additional input time: " + std::to_string(diff) + "ms ";

//
//	start = std::chrono::system_clock::now();

	lk_string lk_result = "lk_result";
	if (cxt.arg_count() > 3)
		lk_result = cxt.arg(3).as_string();


	// testing with vector and then will move to table or other files as inputs.
	if (cxt.arg(2).deref().type() == lk::vardata_t::VECTOR)
	{
		int num_runs = cxt.arg(2).length();

		// std::async implementation - speed up of about 5.2 for 8 threads or more
		std::vector< std::future<lk::vardata_t> > results;
		for (int i = 0; i < num_runs; i++)
		{
			lk::vardata_t input_value = cxt.arg(2).vec()->at(i);
			results.push_back(std::async(std::launch::async, sam_async_thread, cxt, bc, lk_result, input_name, input_value));
		}

// Will block till data is available in future<std::string> object.
		for (int i=0; i<num_runs; i++)
		{
			lk_string result_name = wxString::Format("result %d", i);
			cxt.result().hash_item(result_name, results[i].get());
		}

	}
//
//	end = std::chrono::system_clock::now();
//	diff = std::chrono::duration_cast < std::chrono::milliseconds > (end - start).count();
//	loop_time = " loop time: " + std::to_string(diff) + "ms \n";

//	cxt.result().hash_item("async_time", file_time + loop_time);
	cxt.result().hash_item("error", err_str);
}



static void fcall_sam_packaged_task(lk::invoke_t &cxt)
{
	LK_DOC("sam_packaged_task", "For running function in a thread using std::async.", "(string: file containg lk code to run in separate thread, string: variable to set for each separate thread, vector: arguments for variable for each thread, [string: name of result variable to get from threaded script, string: global variable to set, lk value of global]");
	// wrapper around std::async to run functions with argument
	// will use std::promise, std::future in combination with std::package or std::async
	//lk_string func_name = cxt.arg(0).as_string();

	// checking for bottlenecks
	//	std::chrono::system_clock::time_point start = std::chrono::system_clock::now();
	lk_string err_str = "";// , file_time, parse_time, bc_time, add_input_time, loop_time;
	cxt.result().empty_hash();

	lk_string fn = cxt.arg(0).as_string();
	FILE *fp = fopen(fn.c_str(), "r");
	if (!fp)
	{
		err_str += "No valid input file " + fn + "specified\n";
		cxt.result().hash_item("error", err_str);
		return;
	}

	lk_string file_contents;
	char buf[1024];
	while (fgets(buf, 1023, fp) != 0)
		file_contents += buf;
	fclose(fp);

	//
	//	auto end = std::chrono::system_clock::now();
	//	auto diff = std::chrono::duration_cast < std::chrono::milliseconds > (end - start).count();
	//	file_time = " File time: " + std::to_string(diff) + "ms ";

	// additional input time
	//	start = std::chrono::system_clock::now();

	// add input value
	// required input - changes in each thread
	lk_string input_name = cxt.arg(1).as_string();
	lk_string value = "\"ASSIGN|VALUE|HERE\";\n";
	file_contents = input_name + "=" + value + file_contents;

	// optional global additional input - e.g. hash for pvrpm same for all threads
	if (cxt.arg_count() > 5)
	{
		lk_string add_input_name = cxt.arg(4).as_string();
		value = "\"ADDITIONALASSIGN|VALUE|HERE\";\n";
		lk::vardata_t add_input_value = cxt.arg(5);
		file_contents = add_input_name + "=" + value + file_contents;
	}


	// file contents parsed to node_t
	// checking for bottlenecks
	//	start = std::chrono::system_clock::now();

	lk::input_string p(file_contents);
	lk::parser parse(p);
	smart_ptr<lk::node_t>::ptr tree(parse.script());
	int i = 0;
	while (i < parse.error_count())
		err_str += lk_string(parse.error(i++)) + "\n";

	if (parse.token() != lk::lexer::END)
		err_str += "parsing did not reach end of input\n";

	if (err_str.Len() > 0)
	{
		cxt.result().hash_item("error", err_str);
		return;
	}
	//
	//	end = std::chrono::system_clock::now();
	//	diff = std::chrono::duration_cast < std::chrono::milliseconds > (end - start).count();
	//	parse_time = " Parse time: " + std::to_string(diff) + "ms ";


	// bytecode time
	//	start = std::chrono::system_clock::now();

	lk::bytecode bc;
	lk::codegen cg;
	if (cg.generate(tree.get()))
		cg.get(bc);
	else
	{
		err_str += "bytecode not generated.\n";
		cxt.result().hash_item("error", err_str);
		return;
	}
	//
	//	end = std::chrono::system_clock::now();
	//	diff = std::chrono::duration_cast < std::chrono::milliseconds > (end - start).count();
	//	bc_time = " bytecode time: " + std::to_string(diff) + "ms ";


	//	start = std::chrono::system_clock::now();

	// additional common inputs; e.g., meta hash for pvrpm

	if (cxt.arg_count() > 5)
	{
		// replace bc.constants with updated lk:vardata_t input value
		value = "ADDITIONALASSIGN|VALUE|HERE";
		size_t ndx_c = bc.constants.size() + 1;
		for (size_t i = 0; i < bc.constants.size(); i++)
		{
			if (bc.constants[i].type() == lk::vardata_t::STRING)
			{
				if (bc.constants[i].as_string() == value)
				{
					ndx_c = i;
					break;
				}
			}
		}
		if (ndx_c > bc.constants.size())
		{
			err_str += "_async: No additional input value found to change.\n";
		}
		else
		{
			lk::vardata_t add_input_value = cxt.arg(5);
			bc.constants[ndx_c] = add_input_value;
		}
	}

	if (err_str.Len() > 0)
	{
		cxt.result().hash_item("error", err_str);
		return;
	}



	//	end = std::chrono::system_clock::now();
	//	diff = std::chrono::duration_cast < std::chrono::milliseconds > (end - start).count();
	//	add_input_time = " bytecode additional input time: " + std::to_string(diff) + "ms ";

	//
	//	start = std::chrono::system_clock::now();

	lk_string lk_result = "lk_result";
	if (cxt.arg_count() > 3)
		lk_result = cxt.arg(3).as_string();


	// testing with vector and then will move to table or other files as inputs.
	if (cxt.arg(2).deref().type() == lk::vardata_t::VECTOR)
	{
		int num_runs = cxt.arg(2).length();
//		int nthread = wxThread::GetCPUCount();

//		SimulationDialog tpd("Preparing simulations...", nthread);

		// std::async implementation - speed up of about 5.2 for 8 threads or more
		std::vector< std::packaged_task<lk::vardata_t(lk::invoke_t, lk::bytecode, lk_string, lk_string, lk::vardata_t) > > tasks;
		std::vector< std::future<lk::vardata_t> > results;
		std::vector<std::thread> threads;
		for (int i = 0; i < num_runs; i++)
		{
			lk::vardata_t input_value = cxt.arg(2).vec()->at(i);
			tasks.push_back(std::packaged_task<lk::vardata_t(lk::invoke_t, lk::bytecode, lk_string, lk_string, lk::vardata_t)>(&sam_async_thread));
			results.push_back(tasks[i].get_future());
			threads.push_back(std::thread(std::move(tasks[i]), cxt, bc, lk_result, input_name, input_value));
		}
/* Artificial progress and especially when locked as in lhs_threaded
		if (nthread >(int)num_runs) nthread = num_runs;
		tpd.NewStage("Calculating...", nthread);
		int ms_interval = 1000;
		bool done = false;
		int i_progress = 0;
		while (!done)
		{
			done = true;
			for (int i = 0; i < num_runs; i++)
				done = done && (results[i].wait_for(std::chrono::milliseconds(ms_interval)) == std::future_status::ready);
			i_progress++;
			if (i_progress > num_runs) i_progress = 1;
			for (int i = 0; i < nthread; i++)
				tpd.Update(i, 100.0 * (float)i_progress / (float)num_runs);
		}
*/

		//clean up threaded
		for (int i = 0; i < num_runs; i++)
		{
			threads[i].join();
		}

		// Will block till data is available in future<std::string> object.
		for (int i = 0; i<num_runs; i++)
		{
			lk_string result_name = wxString::Format("result %d", i);
			cxt.result().hash_item(result_name, results[i].get());
		}

//		tpd.Finalize();
	}
	//
	//	end = std::chrono::system_clock::now();
	//	diff = std::chrono::duration_cast < std::chrono::milliseconds > (end - start).count();
	//	loop_time = " loop time: " + std::to_string(diff) + "ms \n";

	//	cxt.result().hash_item("async_time", file_time + loop_time);
	cxt.result().hash_item("error", err_str);
}

#ifdef __WXMSW__
// windows system call to hide output
int windows_system(wxString args)
{
	PROCESS_INFORMATION p_info;
	STARTUPINFO s_info;
	LPWSTR cmdline;

	memset(&s_info, 0, sizeof(s_info));
	memset(&p_info, 0, sizeof(p_info));
	s_info.dwFlags = STARTF_USESHOWWINDOW;
	s_info.wShowWindow = SW_HIDE;
	s_info.cb = sizeof(s_info);
	cmdline = (LPWSTR)_tcsdup(args.wc_str());

	int ret = CreateProcess(NULL, cmdline, NULL, NULL, TRUE, 0, NULL, NULL, &s_info, &p_info);

	if (ret != 0)
	{
		WaitForSingleObject(p_info.hProcess, INFINITE);
		CloseHandle(p_info.hProcess);
		CloseHandle(p_info.hThread);
		return 0;
	}
	else
		return GetLastError();
}
#endif

void lhs_threaded(lk::invoke_t &cxt, wxString &workdir, int &sv, int &num_samples, int &idist, wxString &dist_name, wxString &lhsexe, wxString &err_msg, std::vector<double> &params)
{
	// lock guar for duration of this function.
	std::lock_guard<std::mutex> mtx_lock(global_mu);
	// delete any output or error that may exist
	if (wxFileExists(workdir + "/SAMLHS.LHI"))
		wxRemoveFile(workdir + "/SAMLHS.LHI");

	// write lhsinputs.lhi file
	wxString inputfile = workdir + "/SAMLHS.LHI";
	FILE *fp = fopen(inputfile.c_str(), "w");
	if (!fp)
	{
		cxt.error("Could not write to LHS input file " + inputfile);
		return;
	}

	fprintf(fp, "LHSTITL SAM LHS RUN\n");
	fprintf(fp, "LHSOBS %d\n", num_samples);
	fprintf(fp, "LHSSEED %d\n", sv);
	fprintf(fp, "LHSRPTS CORR DATA\n");
	fprintf(fp, "LHSSCOL\n");
	fprintf(fp, "LHSOUT samlhs.lsp\n");
	fprintf(fp, "LHSPOST samlhs.msp\n");
	fprintf(fp, "LHSMSG samlhs.lmo\n");
	fprintf(fp, "DATASET:\n");

	int ncdfpairs;
	int nminparams = wxStringTokenize(lhs_dist_names[idist], ",").Count() - 1;
	if ((int)params.size() < nminparams)
	{
		cxt.error(wxString::Format("Dist '%s' requires minimum %d params, only %d specified.",
			(const char*)dist_name.c_str(), nminparams, (int)params.size()));
		fclose(fp);
		return;
	}

	switch (idist)
	{
	case LHS_UNIFORM:
		fprintf(fp, "%s UNIFORM %lg %lg\n", (const char*)dist_name.c_str(),
			params[0],
			params[1]);
		break;
	case LHS_NORMAL:
		fprintf(fp, "%s NORMAL %lg %lg\n", (const char*)dist_name.c_str(),
			params[0],
			params[1]);
		break;
	case LHS_LOGNORMAL:
		fprintf(fp, "%s LOGNORMAL %lg %lg\n", (const char*)dist_name.c_str(),
			params[0],
			params[1]);
		break;
	case LHS_LOGNORMAL_N:
		fprintf(fp, "%s LOGNORMAL-N %lg %lg\n", (const char*)dist_name.c_str(),
			params[0],
			params[1]);
		break;
	case LHS_TRIANGULAR:
		fprintf(fp, "%s %lg TRIANGULAR %lg %lg %lg\n", (const char*)dist_name.c_str(), params[1],
			params[0],
			params[1],
			params[2]);
		break;
	case LHS_GAMMA:
		fprintf(fp, "%s GAMMA %lg %lg\n", (const char*)dist_name.c_str(),
			params[0],
			params[1]);
		break;
	case LHS_POISSON:
		fprintf(fp, "%s POISSON %lg\n", (const char*)dist_name.c_str(),
			params[0]);
		break;
	case LHS_BINOMIAL:
		fprintf(fp, "%s BINOMIAL %lg %lg\n", (const char*)dist_name.c_str(),
			params[0],
			params[1]);
		break;
	case LHS_EXPONENTIAL:
		fprintf(fp, "%s EXPONENTIAL %lg\n", (const char*)dist_name.c_str(),
			params[0]);
		break;
	case LHS_WEIBULL:
		fprintf(fp, "%s WEIBULL %lg %lg\n", (const char*)dist_name.c_str(),
			params[0],
			params[1]);
		break;
	case LHS_USERCDF:
		ncdfpairs = (int)params[0];
		fprintf(fp, "%s DISCRETE CUMULATIVE %d #\n", (const char*)dist_name.c_str(), ncdfpairs);
		// update for uniform discrete distributions initially
		if (ncdfpairs <= 0)
		{
			cxt.error(wxString::Format("user defined CDF error: too few [value,cdf] pairs in list: %d pairs should exist.", ncdfpairs));
			fclose(fp);
			return;
		}
		/*
		for (int j = 0; j<ncdfpairs; j++)
		{
		double cdf = (j + 1);
		cdf /= (double)ncdfpairs;
		if (cdf > 1.0) cdf = 1.0;
		fprintf(fp, "  %d %lg", j, cdf);
		if (j == ncdfpairs - 1) fprintf(fp, "\n");
		else fprintf(fp, " #\n");
		}
		*/

		for (int j = 0; j<ncdfpairs; j++)
		{
			if (2 + 2 * j >= (int)params.size())
			{
				cxt.error(wxString::Format("user defined CDF error: too few [value,cdf] pairs in list: %d pairs should exist.", ncdfpairs));
				fclose(fp);
				return;
			}

			fprintf(fp, "  %lg %lg", params[1 + 2 * j], params[2 + 2 * j]);
			if (j == ncdfpairs - 1) fprintf(fp, "\n");
			else fprintf(fp, " #\n");
		}

		break;
	}
	/*
	for (size_t i = 0; i<m_corr.size(); i++)
	{
	if (Find(m_corr[i].name1) >= 0 && Find(m_corr[i].name2) >= 0)
	fprintf(fp, "CORRELATE %s %s %lg\n", (const char*)m_corr[i].name1.c_str(), (const char*)m_corr[i].name2.c_str(), m_corr[i].corr);
	}
	*/
	fclose(fp);

	// delete any output or error that may exist
	if (wxFileExists(workdir + "/SAMLHS.LSP"))
		wxRemoveFile(workdir + "/SAMLHS.LSP");

	if (wxFileExists(workdir + "/LHS.ERR"))
		wxRemoveFile(workdir + "/LHS.ERR");

	// run the executable synchronously
	wxString curdir = wxGetCwd();
	wxSetWorkingDirectory(workdir);

#ifdef __WXMSW__
	wxString execstr = wxString('"' + lhsexe + "\" SAMLHS.LHI"); // shows window
	bool exe_ok = (0 == windows_system(execstr));
#else // untested
	wxString execstr = wxString('"' + lhsexe + "\" SAMLHS.LHI  >nul  2>&1"); // shows window
	bool exe_ok = (0 == std::system((const char*) execstr.c_str()));
#endif

	wxSetWorkingDirectory(curdir);

	if (wxFileExists(workdir + "/LHS.ERR"))
	{
		err_msg = "LHS error.  There could be a problem with the input setup.";
		FILE *ferr = fopen(wxString(workdir + "/LHS.ERR").c_str(), "r");
		if (ferr)
		{
			char buf[256];
			err_msg += "\n\n";
			wxString line;
			while (!feof(ferr))
			{
				fgets(buf, 255, ferr);
				err_msg += wxString(buf) + "\n";
			}
			fclose(ferr);
		}
		cxt.error(err_msg);
		return;
	}

	if (!exe_ok)
	{
		cxt.error("Failed to run LHS executable");
		return;
	}

	// read the lsp output file
	wxString outputfile = workdir + "/SAMLHS.LSP";
	fp = fopen(outputfile.c_str(), "r");
	if (!fp)
	{
		cxt.error("Could not read output file " + outputfile);
		return;
	}

	// output vector
	cxt.result().empty_vector();

	int nline = 0;
	char cbuf[1024];
	int n_runs = 0;
	bool found_data = false;
	while (!feof(fp))
	{
		fgets(cbuf, 1023, fp);
		wxString buf(cbuf);
		nline++;

		if (buf.Trim() == "@SAMPLEDATA")
		{
			found_data = true;
			continue;
		}

		if (found_data)
		{
			if (n_runs == num_samples)
				break;

			n_runs++;
			int n = atoi(buf.c_str());
			if (n != n_runs)
			{
				cxt.error(wxString::Format("output file formatting error (run count %d!=%d) at line %d: ", n, n_runs, nline) + buf);
				fclose(fp);
				return;
			}

			fgets(cbuf, 1023, fp);
			wxString buf(cbuf);
			nline++;
			n = atoi(buf.c_str());
			if (n != 1)
			{
				cxt.error("output file formatting error (ndist count) at line " + wxString::Format("%d", nline));
				fclose(fp);
				return;
			}

			fgets(cbuf, 1023, fp);
			wxString val(cbuf);
			nline++;
			cxt.result().vec_append(wxAtof(val));
		}
	}
	fclose(fp);
}


// LHS thread safe implementation for threading pvrpm samples
void fcall_lhs_threaded(lk::invoke_t &cxt)
{
	LK_DOC("lhs_threaded", "Run a Latin Hypercube Sampling and return samples", "(string:distribution, array:distribution_parameters, int:num_samples, [int: seed_value]): array:samples");
	lk_string err_msg = "";
	wxString workdir(wxFileName::GetTempDir());
	// inputs
	lk_string dist_name = cxt.arg(0).as_string();
	int idist = -1;
	for (int i = 0; i<LHS_NUMDISTS; i++)
	{
		wxArrayString distinfo(wxStringTokenize(lhs_dist_names[i], ","));
		if (distinfo.size() > 0 && dist_name.CmpNoCase(distinfo[0]) == 0)
			idist = i;
	}
	if (idist < 0)
	{
		cxt.error("invalid LHS distribution name: " + dist_name);
		return;
	}
	int num_parms = 0;
	std::vector<double> params;
	if (cxt.arg(1).deref().type() == lk::vardata_t::VECTOR)
	{
		num_parms = cxt.arg(1).length();
		for (int i = 0; i < num_parms; i++)
			params.push_back(cxt.arg(1).vec()->at(i).as_number());
	}
	else
	{
		cxt.error("Sandia LHS executable no distribution parameters specified.");
		return;
	}
	int num_samples = cxt.arg(2).as_integer();
	int seed_val = 0;
	if (cxt.arg_count() > 3)
		seed_val = cxt.arg(3).as_integer();

	int sv = wxGetLocalTime();
	if (seed_val > 0)
		sv = seed_val;

	wxString lhsexe(SamApp::GetRuntimePath() + "/bin/" + wxString(LHSBINARY));
	if (!wxFileExists(lhsexe))
	{
		cxt.error("Sandia LHS executable does not exist: " + lhsexe);
		return;
	}


	lhs_threaded(cxt, workdir, sv, num_samples, idist, dist_name, lhsexe, err_msg, params);
}


class lkLHSobject : public lk::objref_t, public LHS
{
public:
	lkLHSobject() {	}
	virtual ~lkLHSobject() { }
	virtual lk_string type_name() { return "lhs-object"; }
};


void fcall_lhs_create( lk::invoke_t &cxt )
{
	LK_DOC( "lhs_create", "Create a new Latin Hypercube Sampling object.", "(none):lhs-obj-ref");
	cxt.result().assign( cxt.env()->insert_object( new lkLHSobject ) );
}


#define GETLHS lkLHSobject *lhs = dynamic_cast<lkLHSobject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) )

void fcall_lhs_free( lk::invoke_t &cxt )
{
	LK_DOC( "lhs_free", "Free an LHS object", "(lhs-obj-ref):none");
	if ( GETLHS )
		cxt.env()->destroy_object( lhs );
	else
		cxt.error( "invalid lhs-obj-ref" );
}

void fcall_lhs_dist( lk::invoke_t &cxt )
{
	LK_DOC("lhs_dist", "Adds a variable and configures its distribution.", "(lhs-obj-ref, string:variable name, string:distribution, array:parameters, [array:cdfvalues] ):boolean" );
	if ( GETLHS )
	{
		wxString name( cxt.arg(1).as_string() );
		wxString dist( cxt.arg(2).as_string().Lower() );

		int idist = -1;
		for (int i=0;i<LHS_NUMDISTS;i++)
		{
			wxArrayString distinfo( wxStringTokenize( lhs_dist_names[i], "," ) );
			if ( distinfo.size() > 0 && dist.CmpNoCase( distinfo[0] ) == 0 )
				idist = i;
		}

		if (idist < 0)
		{
			cxt.error("invalid LHS distribution name: " + dist);
			return;
		}

		std::vector<double> params;
		if ( cxt.arg_count() == 4 && cxt.arg(3).type() == lk::vardata_t::VECTOR )
		{
			lk::vardata_t &p = cxt.arg(3).deref();
			for( size_t i=0;i<p.length();i++ )
				params.push_back( p.index(i)->as_number() );
		}
		else if ( cxt.arg_count() == 5
			&& cxt.arg(3).type() == lk::vardata_t::VECTOR
			&& cxt.arg(4).type() == lk::vardata_t::VECTOR
			&& cxt.arg(3).length() == cxt.arg(4).length()
			&& idist == LHS_USERCDF )
		{
			size_t len = cxt.arg(3).length();
			params.push_back( len );
			for( size_t i=0;i<len;i++ )
			{
				params.push_back( cxt.arg(3).index(i)->as_number() );
				params.push_back( cxt.arg(4).index(i)->as_number() );
			}
		}

		lhs->Distribution( idist, name, params );
		cxt.result().assign( 1.0 );
	}
	else
		cxt.error("invalid lhs-obj-ref");
}

void fcall_lhs_corr( lk::invoke_t &cxt )
{
	LK_DOC( "lhs_corr", "Set up a correlation between two variables", "(lhs-obj-ref, string:variable 1, string:variable 2, number:correlation):none");
	if ( GETLHS )
		lhs->Correlate( cxt.arg(1).as_string(), cxt.arg(2).as_string(), cxt.arg(3).as_number() );
	else
		cxt.error("invalid lhs-obj-ref");
}

void fcall_lhs_reset( lk::invoke_t &cxt )
{
	LK_DOC( "lhs_reset", "Reset the LHS object", "(lhs-obj-ref):none");
	if ( GETLHS )
		lhs->Reset();
	else
		cxt.error("invalid lhs-obj-ref");
}

void fcall_lhs_run( lk::invoke_t &cxt )
{
	LK_DOC( "lhs_run", "Run the LHS sampling algorithm to produce the sample vectors", "(lhs-obj-ref, number:samples, [number:seed]):boolean");

	if ( GETLHS )
	{
		lhs->Points( cxt.arg(1).as_number() );

		int seed = -1;
		if ( cxt.arg_count() > 2 )
			seed = cxt.arg(2).as_number();

		lhs->SeedVal( seed );

		cxt.result().assign( lhs->Exec() ? 1.0 : 0.0 );
	}
	else
		cxt.error("invalid lhs-obj-ref");
}

void fcall_lhs_error( lk::invoke_t &cxt )
{
	LK_DOC("lhs_error", "Return any error messages from the LHS algorithm", "(lhs-obj-ref):string");
	if ( GETLHS )
		cxt.result().assign( lhs->ErrorMessage() );
	else
		cxt.error("invalid lhs-obj-ref");
}

void fcall_lhs_vector( lk::invoke_t &cxt )
{
	LK_DOC("lhs_vector", "Returns the sampled values for a variable", "(lhs-obj-ref, string:variable):array");
	if ( GETLHS )
	{
		std::vector<double> samples;
		lhs->Retrieve( cxt.arg(1).as_string(), samples );
		cxt.result().empty_vector();
		if ( samples.size() > 0 )
		{
			cxt.result().resize( samples.size() );
			for( size_t i=0;i<samples.size();i++ )
				cxt.result().index(i)->assign( samples[i] );
		}
	}
	else
		cxt.error("invalid lhs-obj-ref");
}


class lkSTEPobject : public lk::objref_t, public Stepwise
{
public:
	lkSTEPobject() {	}
	virtual ~lkSTEPobject() { }
	virtual lk_string type_name() { return "stepwise-object"; }
};


void fcall_step_create( lk::invoke_t &cxt )
{
	LK_DOC( "step_create", "Create a new stepwise regression object.", "(none):step-obj-ref");
	cxt.result().assign( cxt.env()->insert_object( new lkSTEPobject ) );
}


#define GETSTEP lkSTEPobject *step = dynamic_cast<lkSTEPobject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) )

void fcall_step_free( lk::invoke_t &cxt )
{
	LK_DOC( "step_free", "Free an stepwise regression object", "(step-obj-ref):none");
	if ( GETSTEP )
		cxt.env()->destroy_object( step );
	else
		cxt.error( "invalid step-obj-ref" );
}

void fcall_step_vector( lk::invoke_t &cxt )
{
	LK_DOC("step_vector", "Sets a stepwise input vector", "(step-obj-ref, string:name, array:values):none");
	if ( GETSTEP )
	{
		std::vector<double> values;
		lk::vardata_t &p = cxt.arg(2).deref();
		for( size_t i=0;i<p.length();i++ )
			values.push_back( p.index(i)->as_number() );
		step->SetInputVector( cxt.arg(1).as_string(), values );
	}
	else
		cxt.error( "invalid step-obj-ref");
}

void fcall_step_run( lk::invoke_t &cxt )
{
	LK_DOC("step_run", "Runs the stepwise regression analysis with the given output vector", "(step-obj-ref, array:values):boolean" );
	if ( GETSTEP )
	{
		std::vector<double> values;
		lk::vardata_t &p = cxt.arg(1).deref();
		for( size_t i=0;i<p.length();i++ )
			values.push_back( p.index(i)->as_number() );

		step->SetOutputVector( values );
		cxt.result().assign( step->Exec() ? 1.0 : 0.0 );
	}
	else
		cxt.error("invalid step-obj-ref");
}

void fcall_step_error( lk::invoke_t &cxt )
{
	LK_DOC("step_error", "Returns any error messages from the stepwise regression algorithm", "(step-obj-ref):string");
	if ( GETSTEP )
		cxt.result().assign( step->ErrorMessage() );
	else
		cxt.error("invalid step-obj-ref");
}

void fcall_step_result( lk::invoke_t &cxt )
{
	LK_DOC("step_result", "Returns R^2 (coeff. of determination), delta R^2 (incremental contribution), and beta (standard rank regression coefficient) for a given input vector", "(step-obj-ref, string:name):array");
	if ( GETSTEP )
	{
		cxt.result().empty_vector();
		double R2, deltaR2, beta;
		if ( step->GetStatistics( cxt.arg(1).as_string(), &R2, &deltaR2, &beta ) )
		{
			cxt.result().vec_append( R2 );
			cxt.result().vec_append( deltaR2 );
			cxt.result().vec_append( beta );
		}
	}
	else
		cxt.error("invalid step-obj-ref");
}

static void fcall_parametric_run(lk::invoke_t &cxt)
{
	LK_DOC("parametric_run", "Run the parametrics for the currently active case, returns 0 if no errors.  Errors and warnings are optinally returned in the first parameter.", "( [string:messages] ):boolean");

	CaseWindow *cw = SamApp::Window()->GetCurrentCaseWindow();
	if (!cw) {
		cxt.error("no case found");
		cxt.result().assign(1.0);
		return;
	}
	cw->GetParametricViewer()->RunSimulationsFromMacro();
	cxt.result().assign(0.0);
}

void fcall_parametric_get(lk::invoke_t &cxt)
{
	LK_DOC("parametric_get", "Returns array of output variable from parametric simulation within the current case, or the output from a single run", "(string: output variable, {number:index}):variant");

	Case *c = SamApp::Window()->GetCurrentCase();
	if (!c) {
		cxt.error("no case found");
		return;
	}
	int singleVal = -1;
	if (cxt.arg_count() > 1) {
		singleVal = cxt.arg(1).as_integer();
	}

	lk::vardata_t &out = cxt.result();
	std::vector<Simulation*> sims = c->Parametric().Runs;
	size_t start = 0, end = sims.size();
	if (singleVal == -1) {
		out.empty_vector();
		out.vec()->resize(sims.size());
	}
	else {
		start = singleVal;
		end = singleVal + 1;
	}
	wxString vName = cxt.arg(0).as_string();
	VarValue* vv = sims[0]->GetValue(vName);
	if (!vv) return;
	if (vv->Type() == VV_STRING) {
		for (size_t i = start; i < end; i++) {
			wxString val = sims[i]->GetValue(cxt.arg(0).as_string())->String();
			if (singleVal > -1) {
				out.assign(val);
				return;
			}
			out.index(i)->assign(val);
		}
	}
	else if (vv->Type() == VV_NUMBER) {
		for (size_t i = start; i < end; i++) {
			float val = sims[i]->GetValue(cxt.arg(0).as_string())->Value();
			if (singleVal > -1) {
				out.assign(val);
				return;
			}
			out.index(i)->assign(val);
		}
	}
	else if (vv->Type() == VV_ARRAY) {
		size_t n = 0;
		for (size_t i = start; i < end; i++) {
			double* val = sims[i]->GetValue(cxt.arg(0).as_string())->Array(&n);
			lk::vardata_t* row = nullptr;
			if (singleVal > -1) {
				out.empty_vector();
				out.vec()->resize(n);
				row = &out;
			}
			else {
				row = out.index(i);
				row->empty_vector();
				row->vec()->resize(n);
			}
			for (size_t j = 0; j < n; j++)
				row->index(j)->assign(val[j]);
		}
	}
	else if (vv->Type() == VV_MATRIX) {
		size_t nr = 0;
		size_t nc = 0;
		for (size_t i = start; i < end; i++) {
			double* val = sims[i]->GetValue(cxt.arg(0).as_string())->Matrix(&nr, &nc);
			lk::vardata_t* rows = nullptr;
			if (singleVal > -1) {
				out.empty_vector();
				out.vec()->resize(nr);
				rows = &out;
			}
			else {
				rows = out.index(i);
				rows->empty_vector();
				rows->vec()->resize(nr);
			}
			for (size_t n = 0; n < nr; n++) {
				lk::vardata_t *col = rows->index(n);
				col->empty_vector();
				col->vec()->resize(nc);
				for (size_t m = 0; m < nc; m++) {
					col->index(m)->assign(val[ n*nc+m ]);
				}
			}
		}
	}
	else {
		cxt.error("variable type is not string, number, array or matrix.");
	}
}

static void fcall_parametric_set(lk::invoke_t &cxt)
{
	LK_DOC("parametric_set", "Sets existing input variable for a single parametric simulation within the current case, returns 0 if error", "(string:input variable, number:index, variant:value):bool");

	CaseWindow *cw = SamApp::Window()->GetCurrentCaseWindow();
	if (!cw) {
		cxt.error("no case found");
		cxt.result().assign(0.0);
		return;
	}
	wxString vName = cxt.arg(0).as_string();
	int index = cxt.arg(1).as_integer();

	wxString val = cxt.arg(2).as_string();
	if (!cw->GetParametricViewer()->SetInputFromMacro(vName, index, val)) {
		wxString err = "error setting parametric variable " + vName + " at index " + wxString::Format("%d", index);
		cxt.error(err);
		cxt.result().assign(0.0);
		return;
	}
	cxt.result().assign(1.0);
}

static void fcall_parametric_export(lk::invoke_t &cxt)
{
	LK_DOC("parametric_export", "Export the parametric table to a csv (default) or Excel file. Returns 1 upon success.", "( string:file, [boolean:excel] ):boolean");

	CaseWindow *cw = SamApp::Window()->GetCurrentCaseWindow();
	if (!cw) {
		cxt.error("no case found");
		cxt.result().assign(0.0);
		return;
	}
	wxString file = cxt.arg(0).as_string();
	bool asExcel = false;
	if (cxt.arg_count() > 1) {
		asExcel = cxt.arg(1).as_boolean();
	}
	if (cw->GetParametricViewer()->ExportFromMacro(file, asExcel)) cxt.result().assign(1.0);
	else cxt.result().assign(0.0);
}

static void fcall_read_json(lk::invoke_t &cxt)
{
    LK_DOC("read_json", "Returns contents of JSON file as a hash table", "( string:file ): table")

    wxString file(cxt.arg(0).as_string());
    if (!wxFileExists(file)) {
        cxt.error("file does not exist");
        return;
    }

    std::ifstream ifs(file.ToStdString().c_str(), std::ios::in | std::ios::binary | std::ios::ate);

    std::ifstream::pos_type fileSize = ifs.tellg();
    ifs.seekg(0, std::ios::beg);

    std::vector<char> bytes(fileSize);
    ifs.read(bytes.data(), fileSize);

    std::string input_string = std::string(bytes.data(), fileSize);

    lk_string err;
    lk::vardata_t results;
    auto data = json_to_ssc_data(input_string.c_str());
    if (ssc_data_lookup(data, "error"))
        cxt.result().assign("read_json error: " + std::string(ssc_data_get_string(data, "error")));
    sscdata_to_lkhash(data, cxt.result());
}

static void fcall_reopt_size_battery(lk::invoke_t &cxt)
{
    LK_DOC("reopt_size_battery", "From a detailed or simple photovoltaic with residential, commercial, third party or host developer model, get the optimal battery sizing using inputs set in activate case.", "( none ): table");

    ssc_data_t p_data = ssc_data_create();

    // check if case exists and is correct configuration
    Case *sam_case = SamApp::Window()->GetCurrentCaseWindow()->GetCase();
    if (!sam_case || ((sam_case->GetTechnology() != "PV Battery" && sam_case->GetTechnology() != "PVWatts Battery") ||
            (sam_case->GetFinancing() != "Residential" && sam_case->GetFinancing() != "Commercial" &&
             sam_case->GetFinancing() != "Third Party" && sam_case->GetFinancing() != "Host Developer")))
        throw lk::error_t("Must be run from Photovoltaic case with Residential, Commercial, Third Party or Host Developer model.");
    bool pvsam = sam_case->GetTechnology() == "PV Battery";

    Simulation base_case = sam_case->BaseCase();
    base_case.Clear();
    base_case.Prepare();
    bool success = base_case.Invoke(true);
    if (!success){
        ssc_data_free(p_data);
        throw lk::error_t(base_case.GetErrors()[0]);
    }

    //
    // copy over required inputs from SAM
    //
    VarValue* losses;
    if (pvsam){
        losses = base_case.GetOutput("annual_total_loss_percent");
    }
    else{
        losses = base_case.GetInput("losses");
    }
    ssc_data_set_number(p_data, "losses", losses->Value());
    ssc_data_set_number(p_data, "lat", base_case.GetInput("lat")->Value());
    ssc_data_set_number(p_data, "lon", base_case.GetInput("lon")->Value());

    auto copy_vars_into_ssc_data = [&base_case, &p_data](std::vector<std::string>& captured_vec){
        for (auto& i : captured_vec){
            auto vd = base_case.GetValue(i);
            if (vd){
                switch(vd->Type()){
                    case VV_NUMBER:
                        ssc_data_set_number(p_data, i.c_str(), vd->Value());
                        break;
                    case VV_ARRAY:{
                        size_t n;
                        double* arr = vd->Array(&n);
                        ssc_data_set_array(p_data, i.c_str(), arr, n);
                        break;
                    }
                    case VV_MATRIX:{
                        size_t n, m;
                        double* mat = vd->Matrix(&n, &m);
                        ssc_data_set_matrix(p_data, i.c_str(), mat, n, m);
                        break;
                    }
                    default:
                        throw lk::error_t("ReOpt_size_battery input error: " + i + " type must be number, array or matrix");
                }
            }
        }
    };

    // variables that are disjoint between PVWatts and PVSam
    std::vector<std::string> pvwatts_vars = {"array_type", "azimuth", "tilt",  "gcr", "inv_eff", "dc_ac_ratio",
											 "module_type"};

    std::vector<std::string> pvsam_vars = {"subarray1_track_mode", "subarray1_backtrack", "subarray1_azimuth",
                                           "subarray1_tilt", "subarray1_gcr", "inverter_model", "inverter_count",
                                           "inv_snl_eff_cec", "inv_ds_eff", "inv_pd_eff", "inv_cec_cg_eff",
                                           "inv_snl_paco", "inv_ds_paco", "inv_pd_paco", "inv_cec_cg_paco",
                                           "batt_dc_ac_efficiency", "batt_ac_dc_efficiency", "batt_initial_SOC",
                                           "batt_minimum_SOC"};

    if (pvsam){
        copy_vars_into_ssc_data(pvsam_vars);
    }
    else{
        copy_vars_into_ssc_data(pvwatts_vars);
    }

    // variables common to both models
    std::vector<std::string> pv_vars = {"degradation", "itc_fed_percent", "system_capacity",
                                        "pbi_fed_amount", "pbi_fed_term", "ibi_sta_percent", "ibi_sta_percent_maxvalue",
                                        "ibi_uti_percent", "ibi_uti_percent_maxvalue", "om_fixed", "om_production",
                                        "total_installed_cost", "depr_bonus_fed", "depr_bonus_fed"};

    std::vector<std::string> batt_vars = {"battery_per_kW", "battery_per_kWh", "om_replacement_cost1", "batt_replacement_schedule"};

    std::vector<std::string> rate_vars = {"ur_monthly_fixed_charge", "ur_dc_sched_weekday", "ur_dc_sched_weekend",
                                          "ur_dc_tou_mat", "ur_dc_flat_mat", "ur_ec_sched_weekday", "ur_ec_sched_weekend",
                                          "ur_ec_tou_mat", "ur_metering_option", "ur_monthly_min_charge", "ur_annual_min_charge",
                                          "load", "crit_load"};

    std::vector<std::string> fin_vars = {"analysis_period", "federal_tax_rate", "state_tax_rate", "rate_escalation",
                                         "inflation_rate", "real_discount_rate", "om_fixed_escal", "om_production_escal",
                                         "total_installed_cost"};

    copy_vars_into_ssc_data(pv_vars);
    copy_vars_into_ssc_data(batt_vars);
    copy_vars_into_ssc_data(rate_vars);
    copy_vars_into_ssc_data(fin_vars);

    try{
        Reopt_size_battery_params(p_data);
    }
    catch( std::exception& e){
        ssc_data_free(p_data);
        throw lk::error_t(e.what());
    }

    auto reopt_scenario = new lk::vardata_t;
    sscdata_to_lkvar(p_data, "reopt_scenario", *reopt_scenario);
    ssc_data_free(p_data);

    cxt.result().empty_hash();
    lk_string reopt_jsonpost = lk::json_write(*reopt_scenario);
    cxt.result().hash_item("scenario", reopt_jsonpost);

    // send the post
    wxString post_url = SamApp::WebApi("reopt_post");
    post_url.Replace("<SAMAPIKEY>", wxString(sam_api_key));

    wxEasyCurl curl;
    curl.AddHttpHeader("Accept: application/json");
    curl.AddHttpHeader("Content-Type: application/json");
    curl.SetPostData(reopt_jsonpost);

    wxString msg, err;
    if (!curl.Get(post_url, msg))
    {
        cxt.result().assign(msg);
        return;
    }

    // get the run_uuid to poll for result, checking the status
    lk::vardata_t results;
    if (!lk::json_read(curl.GetDataAsString(), results, &err))
        cxt.result().assign("<ReOpt-error> " + err);

    if (auto err_vd = results.lookup("messages"))
        throw lk::error_t(err_vd->lookup("error")->as_string() + "\n" + err_vd->lookup("input_errors")->as_string() );
    if (auto err_vd = results.lookup("error")){
        cxt.result().hash_item("error", err_vd->as_string());
        return;
    }

    wxString poll_url = SamApp::WebApi("reopt_poll");
    poll_url.Replace("<SAMAPIKEY>", wxString(sam_api_key));
    poll_url.Replace("<RUN_UUID>", results.lookup("run_uuid")->str());
    curl = wxEasyCurl();
    cxt.result().hash_item("response", lk::vardata_t());
    lk::vardata_t* cxt_result = cxt.result().lookup("response");

    MyMessageDialog dlg(GetCurrentTopLevelWindow(), "Polling for result...this may take a few minutes.", "ReOpt Lite API",
            wxCENTER, wxDefaultPosition, wxDefaultSize);
    dlg.Show();
    wxGetApp().Yield( true );
    std::string optimizing_status = "Optimizing...";
    while (optimizing_status == "Optimizing..."){
        if (!curl.Get(poll_url, msg))
        {
            cxt.result().hash_item("error", msg);
            break;
        }
        if (!lk::json_read(curl.GetDataAsString(), *cxt_result, &err)){
            cxt.result().hash_item("error", "<json-error> " + err);
            break;
        }
        if (lk::vardata_t* res = cxt_result->lookup("outputs")){
            optimizing_status = res->lookup("Scenario")->lookup("status")->as_string();
            if (optimizing_status.find("error") != std::string::npos){
                std::string error = res->lookup("messages")->lookup("error")->as_string();
                cxt.result().hash_item("error", error);
                break;
            }
        }
    }
    dlg.Close();
}

static void fcall_setup_landbosse(lk::invoke_t &cxt)
{
    LK_DOC("setup_landbosse", "Checks if LandBOSSE model is setup and if not, installs the package.", "( none ): bool");

    if (SamApp::CheckPythonPackage("landbosse"))
        return;

    MyMessageDialog dlg(GetCurrentTopLevelWindow(), "Installing the balance-of-system (BOS) cost model. Please note that it may take a few minutes to complete the initial installation. Once installed, you will be able to quickly estimate BOS costs using NREL's Land-based Balance-of-System Systems Engineering (LandBOSSE).\n"
                                                    "\n"
                                                    "While you wait, please refer to Eberle et al. 2019 for more information about the methods that were used to develop LandBOSSE. It is important to note that the SAM user interface only includes a limited set of LandBOSSE inputs. If you would like to access more detailed inputs, please use the LandBOSSE model directly",
                        "Land-Based Balance of System Cost Model",
                        wxCENTER, wxDefaultPosition, wxDefaultSize);
    dlg.Show();
    wxGetApp().Yield( true );

    SamApp::InstallPython();

    SamApp::InstallPythonPackage("landbosse");
    dlg.Close();
}

static void fcall_run_landbosse(lk::invoke_t & cxt)
{
    LK_DOC("run_landbosse", "Runs the LandBOSSE model on the current case.", "( none ): none");

    Case *sam_case = SamApp::Window()->GetCurrentCaseWindow()->GetCase();

    VarTable* vartable = &sam_case->Values();

    ssc_data_t landbosse_data = ssc_data_create();

    auto varValue = vartable->Get("en_landbosse");
    if (!varValue || !varValue->Boolean()){
        MyMessageDialog dlg(GetCurrentTopLevelWindow(), "LandBOSSE not enabled. Change `en_landbosse` to 1.", "Windpower error",
                            wxCENTER, wxDefaultPosition, wxDefaultSize, true);
        dlg.ShowModal();
        return;
    }
    ssc_data_set_number(landbosse_data, "en_landbosse", 1);

    varValue = vartable->Get("wind_resource_filename");
    if (!varValue){
        MyMessageDialog dlg(GetCurrentTopLevelWindow(), "Run LandBOSSE error: wind_resource_filename was not assigned.", "Windpower error",
                            wxCENTER, wxDefaultPosition, wxDefaultSize, true);
        dlg.ShowModal();
        return;
    }
    ssc_data_set_string(landbosse_data, "wind_resource_filename", varValue->String().c_str());

    std::vector<std::string> input_names = {"distance_to_interconnect_mi",
                                            "interconnect_voltage_kV", "depth", "rated_thrust_N",
                                            "labor_cost_multiplier", "gust_velocity_m_per_s", "wind_resource_shear",
                                            "num_turbines", "turbine_spacing_rotor_diameters",
                                            "row_spacing_rotor_diameters", "turbine_rating_MW", "wind_turbine_hub_ht",
                                            "wind_turbine_rotor_diameter"};

    for (auto & i : input_names){
        varValue = vartable->Get(i);
        if (!varValue){
            ssc_data_free(landbosse_data);
            MyMessageDialog dlg(GetCurrentTopLevelWindow(), "Run LandBOSSE error: " + i + " was not assigned.", "Windpower error",
                                wxCENTER, wxDefaultPosition, wxDefaultSize, true);
            dlg.ShowModal();
            return;
        }
        ssc_data_set_number(landbosse_data, i.c_str(), varValue->Value());
    }


    auto module = ssc_module_create("wind_landbosse");
    bool success = ssc_module_exec(module, landbosse_data);

    if (!success){
        std::string error = std::string(ssc_data_get_string(landbosse_data, "errors"));
        if (error.empty())
            error = std::string(ssc_module_log(module, 0, nullptr, nullptr));
        ssc_data_free(landbosse_data);
        ssc_module_free(module);
        MyMessageDialog dlg(GetCurrentTopLevelWindow(), "Run LandBOSSE error: " + error, "Windpower error",
                            wxCENTER, wxDefaultPosition, wxDefaultSize, true);
        dlg.ShowModal();
        return;
    }

    auto outputs = VarTable(landbosse_data);
    vartable->Merge(outputs, true);

    ssc_data_free(landbosse_data);
    ssc_module_free(module);

    std::vector<std::string> total_costs = {"total_development_cost", "total_erection_cost",
            "total_foundation_cost", "total_gridconnection_cost", "total_management_cost",
            "total_sitepreparation_cost", "total_substation_cost", "total_collection_cost"};

    double capacity = vartable->Get("system_capacity")->Value();
    double turbine = vartable->Get("num_turbines")->Value();
    double value = vartable->Get("total_bos_cost")->Value() / capacity;
    vartable->Set("total_bos_cost_per_capacity", VarValue(value));
    for (auto &name : total_costs){
        value = vartable->Get(name)->Value();
        vartable->Set(name + "_per_capacity", VarValue(value / capacity));
        vartable->Set(name + "_per_turbine", VarValue(value / turbine));
    }
}

lk::fcall_t* invoke_general_funcs()
{
	static const lk::fcall_t vec[] = {
            fcall_samver,
            fcall_logmsg,
            fcall_wfdownloaddir,
            fcall_webapi,
            fcall_appdir,
            fcall_runtimedir,
            fcall_userlocaldatadir,
            fcall_copy_file,
            fcall_case_name,
            fcall_dview,
            fcall_dview_solar_data_file,
            fcall_dview_wave_data_file,
            fcall_pdfreport,
            fcall_pagenote,
            fcall_macrocall,
            fcall_read_json,
#ifdef __WXMSW__
		fcall_xl_create,
		fcall_xl_free,
		fcall_xl_open,
		fcall_xl_close,
		fcall_xl_wkbook,
		fcall_xl_sheet,
		fcall_xl_read,
		fcall_xl_set,
		fcall_xl_get,
		fcall_xl_autosizecols,
#endif
            fcall_lhs_threaded,
            fcall_lhs_create,
            fcall_lhs_free,
            fcall_lhs_reset,
            fcall_lhs_dist,
            fcall_lhs_corr,
            fcall_lhs_run,
            fcall_lhs_error,
            fcall_lhs_vector,
            fcall_parametric_get,
            fcall_parametric_set,
            fcall_parametric_run,
            fcall_parametric_export,
            fcall_step_create,
            fcall_step_free,
            fcall_step_vector,
            fcall_step_run,
            fcall_step_error,
            fcall_step_result,
            fcall_sam_async,
            fcall_sam_packaged_task,
            fcall_showsettings,
            fcall_setting,
            fcall_getsettings,
            fcall_setsettings,
            fcall_rescanlibrary,
            fcall_librarygetcurrentselection,
            fcall_librarygetfiltertext,
            fcall_librarygetnumbermatches,
            fcall_librarynotifytext,
            fcall_reopt_size_battery,
            fcall_setup_landbosse,
            fcall_run_landbosse,
            0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_ssc_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_ssc_create,
		fcall_ssc_module_create_from_case,
		fcall_ssc_free,
		fcall_ssc_dump,
		fcall_ssc_var,
		fcall_ssc_exec,
		fcall_ssc_eqn,
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
		fcall_property,
		fcall_setup_landbosse,
        fcall_run_landbosse,
		// fcall_logmsg,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_casecallback_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_value,
		fcall_is_assigned,
		fcall_varinfo,
		fcall_output,
		fcall_technology,
		fcall_financing,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_codegencallback_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_codegen_metric,
			0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_resultscallback_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_metric,
		fcall_metric_row,
		fcall_metric_table,
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
		fcall_add_gain_term,
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
		fcall_windtoolkit,
		fcall_nsrdbquery,
		fcall_combinecasesquery,
        fcall_wavetoolkit,
		fcall_openeiutilityrateform,
		fcall_group_read,
		fcall_group_write,
		fcall_calculated_list,
		fcall_urdb_read,
		fcall_urdb_write,
		fcall_urdb_get,
		fcall_urdb_list_utilities,
		fcall_urdb_list_utilities_by_zip_code,
		fcall_urdb_list_rates,
		fcall_editscene3d,
		fcall_showsettings,
		0 };
	return (lk::fcall_t*)vec;
}

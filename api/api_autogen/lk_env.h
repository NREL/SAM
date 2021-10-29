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

#ifndef SYSTEM_ADVISOR_MODEL_LK_ENV_H
#define SYSTEM_ADVISOR_MODEL_LK_ENV_H

#include <fstream>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

#include "data_structures.h"

/* Set up LK environment specific for export_config */

/// Bookmarks active configuration during startup.lk parsing
extern std::string active_config;
extern std::string active_cmod;
extern int active_method; // LOAD or CHNG
extern bool map_subobject; // true if subobject hasn't been mapped before
extern std::vector<std::string> subobjects_completed;


/// export_config_funcs

static void fcall_addconfig( lk::invoke_t &cxt )
{
    LK_DOC("addconfig", "Add a technology+financing options", "( string:tech, array:financings ):none" );
    // nothing to do
}

static void fcall_setconfig( lk::invoke_t &cxt )
{
    LK_DOC("setconfig", "Sets the currently active configuration for editing", "(string:Tech, string:Financing):none");

    active_config = cxt.arg(0).as_string() + "-" + cxt.arg(1).as_string();
}

static void fcall_configopt( lk::invoke_t &cxt )
{
    LK_DOC("configopt", "Sets configuration options, such as long_name, short_name, description, etc.", "(string:config name, table:options):none");
    lk::vardata_t &tab = cxt.arg(1).deref();
    std::string config = cxt.arg(0).as_string();
    std::string long_name, desc;
    if (lk::vardata_t *vv = tab.lookup("long_name"))
        long_name = vv->as_string();
    if (lk::vardata_t *vv = tab.lookup("description"))
        desc = vv->as_string();

    SAM_option_to_description.insert({config, {long_name, desc}});
}

static void fcall_setting( lk::invoke_t &cxt )
{
    LK_DOC( "setting", "Sets a setting field for the current configuration", "(string:name, string:value -or- table:name/value pairs):none");
    // nothing to do
}

static void fcall_setmodules( lk::invoke_t &cxt )
{
    LK_DOC("setmodules", "Sets the simulation models for the currently active configuration", "(array:module names):none");

    std::vector<std::string> list;
    lk::vardata_t &m = cxt.arg(0);
    for (size_t i = 0; i < m.length(); i++)
        list.push_back(m.index(i)->as_string().ToStdString());

    SAM_config_to_primary_modules[active_config] = list;
}

static void fcall_addpage( lk::invoke_t &cxt )
{
    LK_DOC("addpage", "Add an input page group to the currently active configuration (may have multiple pages).", "(array:pages, table:caption,help,exclusive,exclusive_var):none" );

    lk::vardata_t &grps = cxt.arg(0);
    page_info new_page;
    for( size_t i=0;i<grps.length();i++ )
    {
        for( size_t j=0;j<grps.index(i)->deref().length();j++ )
        {
            lk::vardata_t &item = grps.index(i)->deref().index(j)->deref();

            if ( item.type() == lk::vardata_t::HASH )
            {
                if ( lk::vardata_t *name = item.lookup( "name" ) )
                    new_page.common_uiforms.push_back(name->as_string().ToStdString());
            }
            else
            {
                new_page.common_uiforms.push_back(item.as_string().ToStdString());
            }
        }
    }

    if ( new_page.common_uiforms.size() == 0 || new_page.common_uiforms[0].size() == 0 ) return;

    new_page.sidebar_title = new_page.common_uiforms[0];
    std::vector<std::string> excl_header_pages;

    if ( cxt.arg_count() > 1 )
    {
        lk::vardata_t &props = cxt.arg(1).deref();

        if( lk::vardata_t *x = props.lookup("sidebar") )
            new_page.sidebar_title = x->as_string();

        if ( lk::vardata_t *x = props.lookup("exclusive_var") ){
            new_page.exclusive_var = x->as_string();
            new_page.exclusive_uiforms = new_page.common_uiforms;
            new_page.common_uiforms.clear();
        }

        if ( lk::vardata_t *x = props.lookup("exclusive_header_pages") )
        {
            lk::vardata_t &vec = x->deref();
            if ( vec.type() == lk::vardata_t::VECTOR )
            {
                for( size_t i=0;i<vec.length();i++ )
                {
                    new_page.common_uiforms.push_back(vec.index(i)->as_string().ToStdString());
                }
            }

        }
    }
    SAM_config_to_input_pages[active_config].push_back(new_page);
}

/**
 *
 * These versions of lk functions are used to export a dictionary of configurations: input pages.
 * Only to be used in lk environment for export_config.exe
 */

static lk::fcall_t* export_config_funcs() {
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

/// invoke_casecallback_funcs

static void fcall_value( lk::invoke_t &cxt )
{
    LK_DOC("value", "Gets or sets the case value of a variable by name", "(string:name [,variant:value]):[variant]");


    // if getting a variable, return config-dependent default
    if ( cxt.arg_count() == 1 ){
        std::string var_name = cxt.arg(0).as_string().ToStdString();

        VarValue* def_vv = SAM_config_to_defaults.find(active_config)->second.Get(var_name);

        if (def_vv)
            def_vv->Write(cxt.result());

        // if setting values into a secondary cmod, return the name and value of the variable
//        if (active_cmod.length() > 0 ){
//            std::string map = var_left;// + ":" + def_vv->AsString().ToStdString();
//            cxt.result().assign(map);
//        }
    }
    else {

        auto var_graph = SAM_config_to_variable_graph.find(active_config)->second;

        std::vector<std::string> args = split_identity_string(cxt.error().ToStdString(), 2);
        std::string dest_name = cxt.arg(0).as_string().ToStdString();

        // the source could be a local variable, a literal, a constant, a special_get, or a call to value(...)
        std::string src_name = args[1];

        // check if the dest is an ssc variable
        bool dest_is_ssc = which_cmod_as_input(dest_name, active_config).length() > 0;

        bool src_is_ssc;
        if (argument_of_special(src_name)) {
            src_is_ssc = which_cmod_as_input(src_name, active_config).length() > 0;
        } else if (argument_of_value(src_name)) {
            // check if the ui variable is an ssc variable
            src_is_ssc = which_cmod_as_input(src_name, active_config).length() > 0;
        }

        std::string obj_stack = active_object
                                + (active_subobject.length() > 0 ? ":" + active_subobject : "");

        if (map_subobject){
            std::string ui = find_ui_of_variable(dest_name, active_config);
            // add the source vertex & edge if they don't exist already
            var_graph->add_vertex(src_name, src_is_ssc, ui);

            var_graph->add_edge(src_name, src_is_ssc, dest_name, dest_is_ssc,
                                active_method, obj_stack, "value(" + cxt.error().ToStdString() + ")",
                                ui, nullptr);

        }
    }

}

static void fcall_varinfo( lk::invoke_t &cxt )
{
    LK_DOC("varinfo", "Assign empty strings", "(string:var name):table");
    cxt.result().empty_hash();
    cxt.result().hash_item("label").assign( "" );
    cxt.result().hash_item("units").assign( "" );
    cxt.result().hash_item("group").assign( "" );
}

static void fcall_output(lk::invoke_t &cxt)
{
    LK_DOC("output", "nothing to do", "( string: output variable name ):none");
}

static void fcall_technology( lk::invoke_t &cxt )
{
    LK_DOC( "technology", "nothing to do", "(void):string" );
}

static void fcall_financing( lk::invoke_t &cxt )
{
    LK_DOC( "financing", "return financial model", "(void):string" );
    size_t pos = active_config.find("-");
    std::string f = active_config.substr(pos+1);
    if (f.find("-") != std::string::npos){
        pos = f.find("-");
        f = f.substr(pos+1);
    }
    cxt.result().assign( f );
}

static void fcall_wfdownloaddir( lk::invoke_t &cxt){
    LK_DOC( "wfdownloaddir", "Returns the folder into which solar data files are downloaded.", "(none):string" );
}

static void _wx_date_time(lk::invoke_t &cxt)
{
    LK_DOC("date_time", "Nothing to do", "(none):string");
}

static void _wx_msgbox(lk::invoke_t &cxt)
{
    LK_DOC("msgbox", "Nothing to do", "(string:message, [array:window position [x,y] or geometry [x,y,w,h]):boolean");
}

static void _wx_progressbar(lk::invoke_t &cxt) {
    LK_DOC("progressbar", "Nothing to do", "");
}

static void _wx_showsettings(lk::invoke_t &cxt) {
    LK_DOC("showsettings", "Nothing to do", "");
}

static void fcall_rescanlibrary(lk::invoke_t &cxt) {
    LK_DOC("rescanlibrary", "Nothing to do", "");
}

static void fcall_show(lk::invoke_t &cxt) {
    LK_DOC("show", "Nothing to do", "");
}

static void fcall_get_settings(lk::invoke_t &cxt) {
    LK_DOC("get_settings", "Nothing to do", "");
}

static void fcall_set_settings(lk::invoke_t &cxt) {
    LK_DOC("set_settings", "Nothing to do", "");
}

static void fcall_dview_solar(lk::invoke_t &cxt) {
    LK_DOC("dview_solar", "Nothing to do", "");
}

static void fcall_geocode(lk::invoke_t &cxt) {
    LK_DOC("geocode", "Do not enable geocoding", "");
    cxt.result().empty_hash();
    cxt.result().hash_item("ok").assign(0.0);
}

static void fcall_refresh(lk::invoke_t &cxt) {
    LK_DOC("refresh", "Nothing to do", "");
}

static void fcall_nsrdbquery(lk::invoke_t &cxt)
{
    LK_DOC("nsrdbquery", "Do not enable nsrdb", "(none) : string");
    cxt.result().assign("");
}

static void fcall_librarygetnumbermatches(lk::invoke_t &cxt) {
    LK_DOC("librarygetnumbermatches", "No matches", "(string:libraryctrlname):number");
    cxt.result().assign(0.0);
}

static void fcall_librarygetfiltertext(lk::invoke_t &cxt){
    LK_DOC("librarygetfiltertext", "Return empty", "(string:libraryctrlname):number");
    cxt.result().assign("");
}

static void fcall_librarynotifytext(lk::invoke_t &cxt){
    LK_DOC("librarynotifytext", "Nothing to do", "(string:libraryctrlname):number");
}

static void _wx_yesno(lk::invoke_t &cxt){
    LK_DOC("yesno", "Nothing to do", "");
}

static void fcall_logmsg( lk::invoke_t &cxt )
{
    LK_DOC("logmsg", "Nothing to do", "(...):none");
}

static void fcall_enable( lk::invoke_t &cxt )
{
    LK_DOC("enable", "Record logic of which ui widgets should be enabled ", "(string:name, boolean:enable):none");
    std::string ui_obj_name = cxt.arg(0).as_string().ToStdString();
    if (SAM_ui_obj_to_enabled_variables.find(active_object) != SAM_ui_obj_to_enabled_variables.end()){
        auto it = SAM_ui_obj_to_enabled_variables.find(active_object);
        if (std::find(it->second.begin(), it->second.end(), ui_obj_name) == it->second.end()){
            it->second.push_back(ui_obj_name);
            return;
        }
    }
    else{
        std::vector<std::string> vec;
        vec.push_back(ui_obj_name);
        SAM_ui_obj_to_enabled_variables.insert({active_object, vec});
    }
}

static void _wx_choose_file(lk::invoke_t &cxt) {
    LK_DOC("choose_file", "Nothing to do", "");
    cxt.result().assign("");

}

static void fcall_property(lk::invoke_t &cxt) {
    LK_DOC("property", "Nothing to do", "");
    cxt.result().assign(1.0);

}

static void fcall_webapi(lk::invoke_t &cxt) {
    LK_DOC("webapi", "Nothing to do", "");
    cxt.result().assign("");
}

static void fcall_curl(lk::invoke_t &cxt) {
    LK_DOC("curl", "Nothing to do", "");
}


static void fcall_editscene3d(lk::invoke_t &cxt) {
    LK_DOC("editscene3d", "Nothing to do", "");
}

static void fcall_clearplot(lk::invoke_t &cxt) {
    LK_DOC("clearplot", "Nothing to do", "");
}

static void fcall_setplot(lk::invoke_t &cxt) {
    LK_DOC("setplot", "Nothing to do", "");
}

static void fcall_plot(lk::invoke_t &cxt) {
    LK_DOC("plot", "Nothing to do", "");
}

static void fcall_axis(lk::invoke_t &cxt) {
    LK_DOC("axis", "Nothing to do", "");
}

static void fcall_in(lk::invoke_t &cxt) {
    LK_DOC("in", "Nothing to do", "");
}

static void fcall_windtoolkit(lk::invoke_t &cxt) {
    LK_DOC("windtoolkit", "Nothing to do", "");
}

static void fcall_json_read(lk::invoke_t &cxt) {
    LK_DOC("json_read", "Nothing to do", "");
    cxt.result().assign("");
}

static void fcall_plot_inverter_curve(lk::invoke_t &cxt) {
    LK_DOC("plot_inverter_curve", "Nothing to do", "");
}

static void _alloc(lk::invoke_t & cxt)
{
    LK_DOC("alloc", "Allocates an array of one or two dimensions.", "(integer, {integer}):array");
    cxt.result().empty_vector();

    int dim1 = (int)cxt.arg(0).as_number();
    int dim2 = (cxt.arg_count() == 2) ? (int)cxt.arg(1).as_number() : -1;

    if (dim1 < 1)
        return;

    cxt.result().resize(dim1);

    if (dim2 > 0)
    {
        for (int i = 0; i < dim1; i++)
        {
            lk::vardata_t *item = cxt.result().index(i);
            item->empty_vector();
            item->resize(dim2);
        }
    }
}
static void fcall_current_at_voltage_cec(lk::invoke_t &cxt) {
    LK_DOC("current_at_voltage_cec", "Nothing do to", "...");
}


static void fcall_plotopt(lk::invoke_t &cxt) {
    LK_DOC("plotopt", "Nothing to do", "");
}

static void _editscene3d(lk::invoke_t &cxt) {
    LK_DOC("editscene3d", "Set as error", "");
    cxt.result().empty_hash();
    cxt.result().hash_item("ierr").assign(1.0);
}

static void fcall_substance_density(lk::invoke_t &cxt)
{
    LK_DOC("substance_density", "Assign as 0", "");
    std::cout << "substance_density not implemented\n";
}


static void fcall_snlinverter( lk::invoke_t &cxt )
{
    LK_DOC( "snlinverter", "Map calculation of the sandia inverter AC power from DC and specs", "(number:pdc, number:vdc, number:vdco, number:pdco, number:pso, number:paco, number:c0, number:c1, number:c2, number:c3):number" );

//    std::vector<std::string> args = split_identity_string(cxt.error(), 10);
//
//    digraph* graph = SAM_config_to_variable_graph.find(active_config)->second;
//
//    // if the arguments are in the graph, insert the output as snlinverter:tbd
//    // where tbd will be replaced by name of variable later
//    graph->add_vertex("snlinverter:tbd", false);
//    for (size_t i = 0; i < args.size(); i++){
//        if (graph->find_vertex(args[i])){
//            graph->add_edge(args[i], "snlinverter:tbd", active_method, active_subobject);
//        }
//    }

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

    double vdcminusvdco = vdc - vdco;
    double A = pdco * (1 + c1 * vdcminusvdco);
    double B = pso * (1 + c2 * vdcminusvdco);
    B = (B<0)? 0:B;

    double C = c0 * (1 + c3 * vdcminusvdco);
    double pac = ((paco / (A- B)) - C * (A - B)) * (pdc - B) + C * (pdc - B) * (pdc - B);

    cxt.result().assign( pac );
}

static void _html_dialog(lk::invoke_t &cxt){
    LK_DOC("html_dialog", "Do nothing.", "(string:html source, [string:title], [array:window size [w,h] or geometry [x,y,w,h]]):none");
}

static void _librarygetcurrentselection(lk::invoke_t &cxt){
    LK_DOC("librarygetcurrentselection", "Do nothing.", "");
    cxt.result().assign("");
}

static void fcall_userlocaldatadir( lk::invoke_t &cxt )
{
    LK_DOC("userlocaldatadir", "Nothing", "(none):string");
    cxt.result().assign( "");
}


/**
 *
 * These versions of lk functions are used to export a map of ui to ssc variables.
 * Only to be used in lk environment for export_config.exe
 */

static lk::fcall_t* invoke_casecallback_funcs()
{
    static const lk::fcall_t vec[] = {
            fcall_value,
            fcall_varinfo,
            fcall_output,
            fcall_technology,
            fcall_financing,
            fcall_wfdownloaddir,
            _wx_date_time,
            _wx_msgbox,
            _wx_progressbar,
            _wx_showsettings,
            fcall_rescanlibrary,
            fcall_show,
            fcall_get_settings,
            fcall_set_settings,
            fcall_dview_solar,
            fcall_geocode,
            fcall_refresh,
            fcall_nsrdbquery,
            fcall_librarygetnumbermatches,
            fcall_librarygetfiltertext,
            fcall_librarynotifytext,
            _wx_yesno,
            fcall_logmsg,
            fcall_enable,
            _wx_choose_file,
            fcall_property,
            fcall_webapi,
            fcall_curl,
            fcall_editscene3d,
            fcall_setplot,
            fcall_clearplot,
            fcall_plot,
            fcall_axis,
            fcall_in,
            fcall_windtoolkit,
            fcall_json_read,
            fcall_plot_inverter_curve,
            _alloc,
            fcall_current_at_voltage_cec,
            fcall_plotopt,
            _editscene3d,
            fcall_substance_density,
            fcall_snlinverter,
            _html_dialog,
            _librarygetcurrentselection,
            fcall_userlocaldatadir,
            0 };
    return (lk::fcall_t*)vec;
}

/// invoke_ssc_funcs

static void fcall_ssc_var( lk::invoke_t &cxt )
{
    LK_DOC2( "ssc_var", "Sets or gets a variable value in the SSC data set.",
             "Set a variable value.", "(ssc-obj-ref:data, string:name, variant:value):none",
             "Get a variable value", "(ssc-obj-ref:data, string:name):variant" );


    // get needs to return value of proper type, which can be found by populating secondary_cmod_defaults
    if (cxt.arg_count() == 2){
        std::string var_name = cxt.arg(1).as_string().ToStdString();
        auto map = SAM_cmod_to_outputs.find(active_cmod)->second;
        auto it = map.find(var_name);
        assert(it != map.end());
        VarValue& vv = it->second;
        vv.Write(cxt.result());
    }
    // set will map the argument to the secondary compute module vertex
    else{
        auto var_graph = SAM_config_to_variable_graph.find(active_config)->second;

        std::vector<std::string> args = split_identity_string(cxt.error().ToStdString(), 3);
        std::string dest_name = active_cmod;    // likely "tbd"

        // the source could be a local variable, a literal, a constant, a special_get, or a call to value(...)
        std::string src_name = args[2];

        // check if the ui variable is an ssc variable
        bool is_ssc;
        if (argument_of_special(src_name)){
            is_ssc = which_cmod_as_input(src_name, active_config).length() > 0;
        }
        else if (argument_of_value(src_name)){
            // check if the ui variable is an ssc variable
            is_ssc = which_cmod_as_input(src_name, active_config).length() > 0;
        }

        std::string obj_stack = active_object
                + ":" + (active_subobject.length() > 0 ? active_subobject + ":" : ":")
                + active_cmod; // probably "tbd" since the identity is unknown until ssc_exec

        if (map_subobject) {
            std::string ui = find_ui_of_variable(dest_name, active_config);
            // add the vertices & edge if they don't exist already
            var_graph->add_vertex(src_name, is_ssc, ui);
            var_graph->add_vertex(dest_name, false, ui);

            var_graph->add_edge(src_name, is_ssc, dest_name, false,
                                active_method, obj_stack, "ssc_var(" + cxt.error().ToStdString() + ")",
                                ui, nullptr);
        }
    }
}

static void fcall_ssc_create( lk::invoke_t &cxt )
{
    LK_DOC( "ssc_create", "Activate active_cmod for value tracking", "(none):ssc-obj-ref" );
    active_cmod = "tbd";
}

static void fcall_ssc_module_create_from_case(lk::invoke_t &cxt)
{
    LK_DOC("ssc_module_create_from_case", "Nothing to do", "(string:compute_module_name):ssc-obj-ref");
}

static void fcall_ssc_free( lk::invoke_t &cxt )
{
    LK_DOC( "ssc_free", "Frees up an SSC data object.", "(ssc-obj-ref:data):none" );
    active_cmod = "";
}

static void fcall_ssc_dump( lk::invoke_t &cxt )
{
    LK_DOC( "ssc_dump", "Dump the contents of an SSC data object to a text file.", "(ssc-obj-ref:data, string:file):boolean" );
    // nothing to do
}

static void fcall_ssc_exec( lk::invoke_t &cxt )
{
    LK_DOC( "ssc_exec", "Save name of active secondary cmod & update maps, assign as success", "( ssc-obj-ref:data, string:module, [table:options] ):variant" );

    active_cmod = cxt.arg(1).as_string().ToStdString();

    // make sure all secondary cmod vars info is loaded into SAM_cmod_to_inputs and SAM_cmod_to_outputs
    if (SAM_cmod_to_inputs.find(active_cmod) == SAM_cmod_to_inputs.end()){
        std::vector<std::string> inputs_vec = get_cmod_var_info(active_cmod, "in");
        SAM_cmod_to_inputs.insert({active_cmod, inputs_vec});
    }

    load_secondary_cmod_outputs(active_cmod);

    cxt.result().assign(0.);

    if (map_subobject) {
        // rename vertices in graph with "tbd:var"
        digraph *graph = SAM_config_to_variable_graph.find(active_config)->second;
        graph->rename_cmod_vertices(active_cmod);

        graph->rename_vertex("tbd", false, active_cmod);

    }
}

/**
 *
 * These versions of lk functions are used to map primary compute_module variables to
 * secondary compute_modules.
 * Only to be used in lk environment for export_config.exe
 */

static lk::fcall_t* invoke_ssc_funcs()
{
    static const lk::fcall_t vec[] = {
            fcall_ssc_create,
            fcall_ssc_module_create_from_case,
            fcall_ssc_free,
            fcall_ssc_dump,
            fcall_ssc_var,
            fcall_ssc_exec,
            0 };
    return (lk::fcall_t*)vec;
}


#endif //SYSTEM_ADVISOR_MODEL_LK_ENV_H

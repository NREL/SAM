#ifndef SYSTEM_ADVISOR_MODEL_LK_ENV_H
#define SYSTEM_ADVISOR_MODEL_LK_ENV_H

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

#include "data_structures.h"

extern std::unordered_map<std::string, std::unordered_map<std::string, secondary_cmod_info>> SAM_config_to_secondary_cmod_info;


/* Set up LK environment specific for export_config */

/// Bookmarks active configuration during startup.lk parsing
extern std::string active_config;
extern std::string active_cmod;

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
    // nothing to do
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
    for( size_t i=0;i<m.length();i++ )
        list.push_back( m.index(i)->as_string() );

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
                    new_page.common_uiforms.push_back(name->as_string());
            }
            else
            {
                new_page.common_uiforms.push_back(item.as_string());
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
                    new_page.common_uiforms.push_back(vec.index(i)->as_string());
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

    std::string var_left = cxt.arg(0).as_string().ToStdString();

    // find which ui form the left hand variable is from
    std::string ui_source = find_ui_form_source(var_left, active_config);

    // if getting a variable, return config-independent default found in ui form and save the mapping
    if ( cxt.arg_count() == 1 ){
        VarValue* def_vv = find_default_from_ui(var_left, active_config);

        if (def_vv)
            def_vv->Write(cxt.result());

        // if setting values into a secondary cmod, return the name and value of the variable
        if (active_cmod.length() > 0 ){
            std::string map = var_left + ":" + def_vv->AsString().ToStdString();
            cxt.result().assign(map);
        }
    }
    else{
        // check if the variable being assigned and the value being assigned are ssc inputs
        if (std::strcmp(cxt.arg(1).typestr(), "number") == 0){
            // if it's a number, simply assign it
            cxt.result().assign(cxt.arg(1).as_number());
            return;
        }

        std::string var_right = cxt.arg(1).as_string().ToStdString();

        std::string ui_handle;
        size_t pos = var_right.find(":");
        if (pos != std::string::npos){
            ui_handle = var_right.substr(0, pos);
        }
        else{
            ui_handle = var_right;
        }

        std::string assigning_to_ssc_cmod = which_cmod_as_input(var_left, active_config);
        bool assigning_from_ssc_var = false;

        if (ui_source == "" && (assigning_to_ssc_cmod.length() > 0)){
            std::cout << "fcall_value error: could not find left hand variable " << var_left ;
            std::cout << " in config " << active_config << "\n";
            throw std::exception();
        }

        VarValue vv;
        vv.Set(var_left + ":" + ui_handle);

        vv.Write(cxt.result());

        // if var_left is an ssc variable
        if (assigning_to_ssc_cmod.length() > 0){

        }
    }

}

static void fcall_varinfo( lk::invoke_t &cxt )
{
    LK_DOC("varinfo", "nothing to do", "(string:var name):table");
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
    LK_DOC( "financing", "nothing to do", "(void):string" );
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
    cxt.result().assign(wxEmptyString);
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
    cxt.result().assign("");

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
            0 };
    return (lk::fcall_t*)vec;
}

/// invoke_ssc_funcs

static void fcall_ssc_var( lk::invoke_t &cxt )
{
    LK_DOC2( "ssc_var", "Sets or gets a variable value in the SSC data set.",
             "Set a variable value.", "(ssc-obj-ref:data, string:name, variant:value):none",
             "Get a variable value", "(ssc-obj-ref:data, string:name):variant" );

    // get
    std::string var_left = cxt.arg(1).as_string().ToStdString();
    if (cxt.arg_count() == 2){

        // return a mapping: "Pdco"
    }
    // set
    else{
        // if there isn't data for this config yet, create a new vector of secondary_cmod_infos
        if (SAM_config_to_secondary_cmod_info.find(active_config) == SAM_config_to_secondary_cmod_info.end())
            SAM_config_to_secondary_cmod_info.insert({active_config,
                                                      std::unordered_map<std::string, secondary_cmod_info>()});
        auto cmods = &SAM_config_to_secondary_cmod_info.find(active_config)->second;

        // if there isn't info for the current cmod, add new
        if (cmods->find(active_cmod) == cmods->end()){
            // if the cmod isn't registered yet, add it
            cmods->insert({active_cmod, secondary_cmod_info()});
            secondary_cmod_info* cmod_info = &(cmods->find(active_cmod)->second);

            cmod_info->map_of_input(var_left, cxt.arg(2).as_string().ToStdString());

        }

    }
//    if ( lkSSCdataObj *ssc = dynamic_cast<lkSSCdataObj*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
//    {
//        wxString name = cxt.arg(1).as_string();
//        if (cxt.arg_count() == 2)
//            sscvar_to_lkvar( cxt.result(), (const char*)name.ToUTF8(), *ssc );
//        else if (cxt.arg_count() == 3)
//            lkvar_to_sscvar( *ssc, (const char*)name.ToUTF8(), cxt.arg(2).deref() );
//    }
//    else
//        cxt.error( "invalid ssc-obj-ref" );
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
    // nothing to do
}

static void fcall_ssc_dump( lk::invoke_t &cxt )
{
    LK_DOC( "ssc_dump", "Dump the contents of an SSC data object to a text file.", "(ssc-obj-ref:data, string:file):boolean" );
    // nothing to do
}

static void fcall_ssc_exec( lk::invoke_t &cxt )
{
    LK_DOC( "ssc_exec", "Save name of active secondary cmod & update map", "( ssc-obj-ref:data, string:module, [table:options] ):variant" );
    // make copy of info for "tbd" active_cmod before removing it
    std::unordered_map<std::string, secondary_cmod_info> info = SAM_config_to_secondary_cmod_info[active_cmod];
    SAM_config_to_secondary_cmod_info.erase(active_cmod);

    active_cmod = cxt.arg(1).as_string().ToStdString();
    SAM_config_to_secondary_cmod_info.insert({active_cmod, info});

    cxt.result().assign(1);
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

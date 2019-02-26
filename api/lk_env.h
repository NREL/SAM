#ifndef SYSTEM_ADVISOR_MODEL_LK_ENV_H
#define SYSTEM_ADVISOR_MODEL_LK_ENV_H

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

#include "data_structures.h"

/* Set up LK environment specific for export_config */

/// Bookmarks active configuration during startup.lk parsing
static std::string active_config;

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

    lk::vardata_t vdt_1 = cxt.arg(0);
    std::string var_left = cxt.arg(0).as_string().ToStdString();
    // find which ui form the left hand variable is from
    std::string ui_source = "";
    std::vector<std::string> all_ui = ui_forms_for_config(active_config);

    for (size_t i = 0; i < all_ui.size(); i++){
        std::unordered_map<std::string, VarValue> ui_def = SAM_ui_form_to_defaults[all_ui[i]];
        auto it = ui_def.find(var_left);
        if (it != ui_def.end()){
            // record as 'handle name: value' to enable many levels of indirection
            VarValue vv;
            vv.Set(var_left + ":" + it->second.AsString());

            vv.Write(cxt.result());

            ui_source = all_ui[i];
            break;
        }
    }

    if (ui_source == ""){
        std::cout << "fcall_value error: could not find left hand variable " << var_left ;
        std::cout << " in config " << active_config << "\n";
        throw std::exception();
    }

    if ( cxt.arg_count() > 1 ){
        // check if the variable being assigned and the value being assigned are ssc inputs
        std::string var_right = cxt.arg(1).as_string().ToStdString();

        std::string ui_handle;
        size_t pos = var_right.find(":");
        if (pos != std::string::npos){
            ui_handle = var_right.substr(0, pos);
        }
        else{
            ui_handle = var_right;
        }

        bool assigning_to_ssc_var = false;
        bool assigning_from_ssc_var = false;

        auto primary_cmods = SAM_config_to_primary_modules[active_config];
        for (size_t i = 0; i < primary_cmods.size(); i++){
            std::string cmod = primary_cmods[i];

            auto inputs_vec = SAM_cmod_to_inputs[cmod];

            if (std::find(inputs_vec.begin(), inputs_vec.end(), var_left) != inputs_vec.end()){
                assigning_to_ssc_var = true;
            }
            if (std::find(inputs_vec.begin(), inputs_vec.end(), ui_handle) != inputs_vec.end()){
                assigning_from_ssc_var = true;
            }
        }

        // if var_left is not an ssc variable, it can be an ui variable or a constant
    }
//    if ( VarValue *vv = cc.GetValues().Get( name ) )
//    {
//        if ( cxt.arg_count() == 2 )
//        {
//            if ( vv->Read( cxt.arg(1), false ) )
//                cc.GetCase().VariableChanged( name );
//            else
//                cxt.error( "data type mismatch attempting to set '" + name + "' (" + vv_strtypes[vv->Type()] + ") to " + cxt.arg(1).as_string() + " ("+ wxString(cxt.arg(1).typestr()) + ")"  );
//        }
//        else
//            vv->Write( cxt.result() );
//    }
//    else
//        cxt.error("variable '" + name + "' does not exist in this context" );
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
            0 };
    return (lk::fcall_t*)vec;
}

/// invoke_ssc_funcs

static void fcall_ssc_var( lk::invoke_t &cxt )
{
    LK_DOC2( "ssc_var", "Sets or gets a variable value in the SSC data set.",
             "Set a variable value.", "(ssc-obj-ref:data, string:name, variant:value):none",
             "Get a variable value", "(ssc-obj-ref:data, string:name):variant" );

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
    LK_DOC( "ssc_create", "Create a new empty SSC data container object.", "(none):ssc-obj-ref" );
//    cxt.result().assign( cxt.env()->insert_object( new lkSSCdataObj ) );
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
    LK_DOC( "ssc_exec", "Run a compute module with the provided data context. returns zero if successful. Options include: show_dialog, hold_dialog, debug_file, dialog_title", "( ssc-obj-ref:data, string:module, [table:options] ):variant" );
    //save
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

#ifndef SYSTEM_ADVISOR_MODEL_EXTRACT_STARTUP_H
#define SYSTEM_ADVISOR_MODEL_EXTRACT_STARTUP_H


#include <string>
#include <vector>
#include <unordered_map>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

/**
 * Each input page consists of a page_info with the sidebar title in the SAM GUI, ui forms which are common
 * to the page no matter what selection of variables is active, an exclusive variable which determines what
 * subset of ui forms should be shown, and those exclusive ui forms, of which only one is shown at a time.
 */

struct page_info{
    std::string sidebar_title;
    std::vector<std::string> common_uiforms;
    std::string exclusive_var = "";
    std::vector<std::string> exclusive_uiforms;
};

static std::unordered_map<std::string, std::vector<page_info>> SAM_config_to_input_pages;
static std::unordered_map<std::string, std::vector<std::string>> SAM_config_to_modules;
static std::string active_config;



/**
 * This class extracts input pages for each technology-financial configuration from the startup.lk
 * script, as well as the set of compute modules required for each configuration.
 */

class startup_extractor{
private:
    std::unordered_map<std::string, std::vector<page_info>> config_to_input_pages;
    std::unordered_map<std::string, std::vector<std::string>> config_to_modules;

public:
    std::unordered_map<std::string, std::vector<page_info>> get_config_to_input_pages(){
        return config_to_input_pages;
    };

    std::unordered_map<std::string, std::vector<std::string>> get_config_to_modules(){
        return config_to_modules;
    };

    void print_config_to_input();

    void print_config_to_modules();

    bool load_startup_script(const std::string script_file, std::vector<std::string>* errors);
};


/**
 *
 * These versions of lk functions are used to export a dictionary of configurations: input pages.
 * Only to be used in lk environment for export_config.exe
 */

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

    SAM_config_to_modules[active_config] = list;
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

template <typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& v)
{
    os << "[";
    for (int i = 0; i < v.size(); ++i) {
        os << "'" <<v[i] << "'";
        if (i != v.size() - 1)
            os << ", ";
    }
    os << "]";
    return os;
}

#endif //SYSTEM_ADVISOR_MODEL_EXTRACT_STARTUP_H

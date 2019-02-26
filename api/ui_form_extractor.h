#ifndef SYSTEM_ADVISOR_MODEL_EXTRACT_INPUT_PAGE_H
#define SYSTEM_ADVISOR_MODEL_EXTRACT_INPUT_PAGE_H

#include <string>
#include <vector>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

#include <wx/filename.h>
#include <wx/wfstream.h>
#include <wx/txtstrm.h>

#include "variables.h"
#include "lk_env.h"

/**
 * This class extracts the variables, equations and callbacks LK script from the ui form file.
 * It skips through the ui objects, extracts VarValues from the variable definitions to populate
 * the configuration-independent defaults, and stores the equation and callback script strings.
 */

class ui_form_extractor {
private:
    std::string ui_form_name;
    std::string m_eqn_script;
    std::string m_callback_script;
    std::vector<std::string> m_direct_variables;
    std::vector<std::string> m_calculated_variables;

    lk::env_t m_env;

    /// Gets default values and stores into SAM_config_to_defaults
    VarValue get_varvalue(wxInputStream &is, wxString var_name);

    /// Stores the eqn and callback LK script
    void get_eqn_and_callback_script(wxInputStream& is);

public:

    ui_form_extractor(std::string n){
        ui_form_name = n;
    };

    bool extract(std::string file);

    std::string get_eqn_script() {return m_eqn_script;}

    std::string get_callback_script() {return m_callback_script;}
};



/**
 * Maps each ui form to class containing its eqn and callback scripts
 */
class ui_form_extractor_database{
private:
    std::unordered_map<std::string, ui_form_extractor*> ui_form_map;

public:
    ui_form_extractor_database() = default;

    ~ui_form_extractor_database(){
        for (auto it = ui_form_map.begin(); it != ui_form_map.end(); it++)
            delete it->second;
    }

    ui_form_extractor* find(std::string n){
        auto it = ui_form_map.find(n);
        if (it != ui_form_map.end())
            return it->second;
        else
            return NULL;
    }

    ui_form_extractor* make_entry(std::string n) {
        ui_form_extractor* ufe = new ui_form_extractor(n);
        ui_form_map.insert({n, ufe});
        return ufe;
    }

    bool populate_ui_data(std::string ui_path, std::vector<std::string> ui_form_names);
};

#endif //SYSTEM_ADVISOR_MODEL_EXTRACT_INPUT_PAGE_H

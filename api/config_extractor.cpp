#include "config_extractor.h"
#include "ui_form_extractor.h"
#include "callback_extractor.h"
#include "data_structures.h"

std::unordered_map<std::string, VarTable> SAM_config_to_defaults;

bool config_extractor::load_defaults_for_config(){
    if (SAM_config_to_defaults.find(active_config) != SAM_config_to_defaults.end())
        return true;

    size_t pos = config_name.find('-');
    if (config_name.find('-', pos+1) != std::string::npos)
        pos = config_name.find('-', pos+1);
    std::string filename = config_name.substr(0, pos) + "_" + config_name.substr(pos+1);
    std::string file = defaults_file_dir + filename + ".txt";

    wxFFileInputStream in(file);
    if (!in.IsOk())
    {
        std::cout << "config_extractor could not load defaults for " + active_config << "\n";
            return false;
    }

    VarTable vt;
    bool read_ok = true;

    read_ok = vt.Read_text(in);

    if (!read_ok)
    {
        std::cout << "Error reading inputs from external source " << file <<"\n";
        return false;
    }

    SAM_config_to_defaults.insert({active_config, vt});

    return true;
}

/// setting active_config and active_ui
void config_extractor::register_callback_functions() {
    std::vector<page_info> pages = SAM_config_to_input_pages[config_name];

    callback_extractor cb_ext(config_name, m_env);

    // for all the ui forms, parse the callback functions and save into m_env
    std::vector<std::string> all_ui = find_ui_forms_for_config(config_name);

//    cb_ext.parse_script(SAM_ui_extracted_db.find("Financial TOD Factors")->get_callback_script());
//    cb_ext.extract_functions();
//    return;

    for (size_t i = 0; i < all_ui.size(); i++){
        active_ui = all_ui[i];
        cb_ext.parse_script(SAM_ui_extracted_db.find(all_ui[i])->get_callback_script());
    }
    // not used during extract_functions since all functions have been collected for a single config
    active_ui = "";

    // run through first time to get variable mapping, making sure to run the equations for special variables...

    cb_ext.extract_functions();

    // run through until nothing changes...
}
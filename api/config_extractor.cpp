#include "config_extractor.h"
#include "ui_form_extractor.h"
#include "callback_extractor.h"
#include "data_structures.h"

/// setting active_config and active_ui
void config_extractor::register_callback_functions() {
    std::vector<page_info> pages = SAM_config_to_input_pages[config_name];
    active_config = config_name;

    callback_extractor cb_ext(config_name, m_env);

    // for all the ui forms, parse the callback functions and save into m_env
    std::vector<std::string> all_ui = ui_forms_for_config(config_name);

    cb_ext.parse_script(SAM_ui_extracted_db.find("Financial TOD Factors")->get_callback_script());
    cb_ext.extract_functions();

    return;
    for (size_t i = 0; i < all_ui.size(); i++){

        cb_ext.parse_script(SAM_ui_extracted_db.find(all_ui[i])->get_callback_script());
    }


    cb_ext.extract_functions();
}
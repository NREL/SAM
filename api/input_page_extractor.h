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
 * This class extracts the equations and callbacks LK script from the ui form file by
 * skipping through the ui objects and variable definitions.
 */

class input_page_extractor {
private:
    std::string m_eqn_script;
    std::string m_callback_script;
    std::vector<std::string> m_direct_variables;
    std::vector<std::string> m_calculated_variables;

    /// Structure after VarValue::Read_text
    VarValue get_varvalue(wxInputStream &is, wxString var_name);

    /// Structured after VarInfo::Read_text, except returns variable's flag
    VarValue get_var_default(wxInputStream &is, wxString var_name);

    /// Stores the eqn and callback LK script
    void get_eqn_and_callback_script(wxInputStream& is);

public:

    input_page_extractor() = default;
    ~input_page_extractor(){
        m_env.clear_vars();
        m_env.clear_objs();
    }

    bool extract(std::string file);

    lk::env_t m_env;

    std::string get_eqn_script() {return m_eqn_script;}

    std::string get_callback_script() {return m_callback_script;}
};





#endif //SYSTEM_ADVISOR_MODEL_EXTRACT_INPUT_PAGE_H

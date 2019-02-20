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

/**
 * This class extracts the equations and callbacks LK script from the ui form file by
 * skipping through the ui objects and variable definitions.
 */

class input_page_extractor {
private:
    std::string m_eqn_script;
    std::string m_callback_script;
    std::vector<std::string> m_variables;

    /// Structure after VarValue::Read_text
    void get_varvalue(wxInputStream &is, wxString var_name);

    /// Structured after VarInfo::Read_text for reading variable info and default
    void get_varinfo(wxInputStream &is, wxString var_name);

    /// Stores the eqn and callback LK script
    void get_eqn_and_callback_script(wxInputStream& is);


public:
    input_page_extractor() = default;

    bool extract(std::string file);

    std::string get_eqn_script() {return m_eqn_script;}

    std::string get_callback_script() {return m_callback_script;}
};


#endif //SYSTEM_ADVISOR_MODEL_EXTRACT_INPUT_PAGE_H

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

class input_page_extractor {
private:
    wxString m_eqn_script;
    wxString m_callback_script;
    std::vector<std::string> m_variables;

    void get_variable_name(wxInputStream &is, wxString var_name);
    void get_equations_script(wxInputStream& is);


public:
    void load_ui_form(std::string file) {
        wxFileName ff(file);
        wxString name(ff.GetName());

        bool ok = true;
        wxFFileInputStream is(file, "r");
        bool bff = is.IsOk();
        get_equations_script(is);
    }
};


#endif //SYSTEM_ADVISOR_MODEL_EXTRACT_INPUT_PAGE_H

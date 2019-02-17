#include <string>
#include <vector>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

#include <wx/filename.h>
#include <wx/wfstream.h>
#include <wx/txtstrm.h>



#include "sam_api.h"

class eqn_datacenter {
private:
    std::unordered_map<std::string, std::vector<std::string>> cmod_to_ui_forms;
    std::unordered_map<std::string, EqnDatabase> eqn_databases;

public:
    eqn_datacenter(){
        // populate cmod_to_ui_forms
    };

    bool load_cmod_eqn_db(std::string cmod){
//        std::vector<std::string> ui_forms = cmod_to_ui_forms[cmod];
//
//        // load them
//        EqnDatabase eqn_db;
//        eqn_db.LoadScript();
    }
};

class eqn_evaluator {
private:
    lk::varhash_t* var_eqn_map;
    std::vector<std::string> log;
    
public:
    eqn_evaluator(std::string cmod_type){};

    bool load_script(std::string source);

    void log_status(std::string msg) {
        log.push_back(msg);
    }
};

class input_page_loader {
private:
    wxString m_eqn_script;
    wxString m_callback_script;
    std::vector<std::string> m_variables;

    void get_variable_name(wxInputStream& is, wxString var_name) {
        wxTextInputStream in(is);

        int ver = in.Read8(); // ver
        if (ver < 2)
            in.ReadWord();
        for (size_t j = 0; j < 4; j++)
            in.ReadLine();
        if (in.Read32() > 0)
            in.ReadLine();
        in.ReadLine(); // flags

        std::vector<wxString> table_names;
        wxString desc = in.ReadWord();

        in.Read8(); // ver

        // read default
        unsigned char m_type = in.Read8();
        if (m_type > 0 && m_type < 4){
            int nr = in.Read32();
            int nc = in.Read32();
            if (nc*nr > 1) {
                for (size_t r = 0; r < nr; r++) {
                    in.ReadLine();
                }
            }
            else in.ReadLine();
        }
        // string
        else if (m_type == 4){
            if (in.Read32() > 0) in.ReadLine();
        }
        // table
        else if (m_type == 5){
            in.Read8(); //ver

            size_t m = in.Read32();
            for (size_t j = 0; j<m; j++)
            {
                table_names.push_back(in.ReadWord());

                size_t n = in.Read32();
                for (size_t k = 0; k<n; k++)
                {
                    wxString name = in.ReadWord();
                    get_variable_name(is, var_name);

                }
            }
        }
        if (table_names.size() > 0 ) {
            for (size_t n = 0; n<table_names.size(); n++) {
                m_variables.push_back(var_name + ":" + table_names[n]);
            }
        }
        else m_variables.push_back(var_name);
    }

    void get_equations_script(wxInputStream& is) {
        wxTextInputStream in(is, "\n");

        for (size_t i = 0; i < 3; i++)
            in.ReadLine();

        // skipping through UI objects
        size_t n = in.Read32();

        for (size_t i = 0; i < n; i++){
            in.ReadLine(); // type
            in.ReadLine(); // space
            in.ReadLine(); // visible
            size_t m = in.Read32(); // ui objects
            for (size_t j = 0; j < m; j++) {
                wxString name = in.ReadLine(); // name
                int type = in.Read16(); // property type
                if (type == 6) {
                    // STRINGLIST
                    size_t count = in.Read32();
                    for (size_t k = 0; k < count; k++)
                        in.ReadWord();
                }
                else if (type == 5) {
                    if (in.Read32() > 0) in.ReadLine();
                }
                else if (type == 4) {
                    // COLOR
                    for (size_t k = 0; k < 4; k++) in.ReadLine();
                } else in.ReadLine();
            }

        }

        // save variable names while skipping through variable info
        in.ReadLine();
        n = in.Read32();

        for (size_t i = 0; i < n; i++){
            wxString name = in.ReadWord();
            get_variable_name(is, name);
        }
        in.ReadLine();

        // get equation script
        m_eqn_script.Clear();
        n = in.Read32();
        if (n > 0)
        {
            for (size_t i = 0; i < n; i++)
                m_eqn_script.Append(in.GetChar());
        }
        m_callback_script.Clear();
        n = in.Read32();
        if (n > 0)
        {
            for (size_t i = 0; i < n; i++)
                m_callback_script.Append(in.GetChar());
        }
    }

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

int main(int argc, char *argv[]) {
    wxString ui_path = wxString(std::getenv("SAMDIR")) + "/deploy/ui/";

    input_page_loader ipl;
    ipl.load_ui_form("/Users/dguittet/SAM-Development/sam/SAMd.app/Contents/runtime//ui/Financial Salvage Value.txt");


    return 0;
}
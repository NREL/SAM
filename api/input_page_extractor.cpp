#include "input_page_extractor.h"

void input_page_extractor::get_variable_name(wxInputStream& is, wxString var_name) {
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

    // save variable names
    if (table_names.size() > 0 ) {
        for (size_t n = 0; n<table_names.size(); n++) {
            m_variables.push_back(var_name + ":" + table_names[n]);
        }
    }
    else m_variables.push_back(var_name);
}

void input_page_extractor::get_equations_script(wxInputStream& is) {
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


int extract() {
    wxString ui_path = wxString(std::getenv("SAMDIR")) + "/deploy/ui/";

    input_page_extractor ipl;
    ipl.load_ui_form("/Users/dguittet/SAM-Development/sam/SAMd.app/Contents/runtime//ui/Financial Salvage Value.txt");


    return 0;
}
#include "input_page_extractor.h"

void input_page_extractor::get_varvalue(wxInputStream &is, wxString var_name) {
    wxTextInputStream in(is);

    std::vector<wxString> table_names;

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
            std::string entry = in.ReadWord();
            table_names.push_back(entry);

            get_varvalue(is, entry);
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

void input_page_extractor::get_varinfo(wxInputStream &is, wxString var_name) {
    wxTextInputStream in(is);

    int ver = in.Read8(); // ver
    if (ver < 2)
        in.ReadWord();
    // type, label, units, group
    for (size_t j = 0; j < 4; j++)
        in.ReadLine();
    // index labels
    if (in.Read32() > 0)
        in.ReadLine();
    in.ReadLine(); // flags

    get_varvalue(is, var_name);
    if (ver >= 3) in.ReadLine();
}

/// Formatting of UI form txt taken from InputPageData::Read, VarDatabase::Read
void input_page_extractor::get_eqn_and_callback_script(wxInputStream& is) {
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
        get_varinfo(is, name);
    }
    in.ReadLine();

    // get equation script
    m_eqn_script.clear();
    n = in.Read32();
    wxString tmp;
    if (n > 0)
    {
        for (size_t i = 0; i < n; i++)
            tmp.Append(in.GetChar());
    }
    m_eqn_script = tmp.ToStdString();
    tmp.clear();

    m_callback_script.clear();
    n = in.Read32();
    if (n > 0)
    {
        for (size_t i = 0; i < n; i++)
            tmp.Append(in.GetChar());
    }
    m_callback_script = tmp.ToStdString();
}

bool input_page_extractor::extract(std::string file) {
    wxFileName ff(file);
    wxString name(ff.GetName());

    wxFFileInputStream is(file, "r");
    bool bff = is.IsOk();
    if (!bff) return false;
    get_eqn_and_callback_script(is);
    return true;
}
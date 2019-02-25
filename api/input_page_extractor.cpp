#include <stdexcept>
#include <iostream>

#include "input_page_extractor.h"
#include "variables.h"

std::unordered_map<std::string, std::unordered_map<std::string, VarValue>> SAM_config_to_defaults;

VarValue input_page_extractor::get_varvalue(wxInputStream &is, wxString var_name) {
    wxTextInputStream in(is, "\n");
    VarValue vv;

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
            // need to do maybe
            in.ReadLine();
        }
        else{
            double def;
            in.ReadLine().ToDouble(&def);
            vv.Set(def);
        }
    }
    // string
    else if (m_type == 4){
        if (in.Read32() > 0) vv.Set(in.ReadLine());
    }
    // table
    else if (m_type == 5){
        in.Read8(); //ver

        size_t m = in.Read32();
        VarTable vt;
        for (size_t j = 0; j<m; j++)
        {
            std::string entry = in.ReadWord();
            vt.Set(entry, get_varvalue(is, entry));
        }
        vv.Set(vt);
    }
    // binary
    else if (m_type == 6){
        size_t len = in.Read32();
        for (size_t i = 0; i <len; i++)
            in.GetChar();
        vv.Set(wxMemoryBuffer());
    }

}

VarValue input_page_extractor::get_var_default(wxInputStream &is, wxString var_name) {
    wxTextInputStream in(is, "\n");

    wxArrayString table_names;


    int ver = in.Read8(); // ver
    if (ver < 2)
        in.ReadWord();
    // type, label, units, group
    for (size_t j = 0; j < 4; j++)
        in.ReadLine();
    // index labels for table
    size_t n = in.Read32();
    if (n > 0){
        wxString x;
        for (size_t i = 0; i < n; i++)
            x.Append(in.GetChar());
        table_names = wxSplit(x, '|');

        in.ReadLine();
    }
    std::string flag = in.ReadLine(); // flags
    int calculated = 0;

    // conversion to string serves as check on parsing process
    try{
        calculated = std::stoi(flag) & VF_CALCULATED;
    }
    catch (std::invalid_argument){
        std::cout << "flag error: " << flag << " while parsing var info " << var_name << "\n";
    }

    VarValue vv;
    vv.Read_text(is);

    if (ver >= 3) in.ReadLine();

    // save variable names
    if (table_names.size() > 0 ) {
        for (size_t i = 0; i<table_names.size(); i++) {
            if (calculated)
                m_calculated_variables.push_back(var_name + "." + table_names[i]);
            else
                m_direct_variables.push_back(var_name + "." + table_names[i]);
        }
    }
    else{
        if (calculated) m_calculated_variables.push_back(var_name);
        else m_direct_variables.push_back(var_name);
    }

    return vv;
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
        std::string name = in.ReadWord().ToStdString();
        auto it = SAM_config_to_defaults[active_config].find(name);
        if (it != SAM_config_to_defaults[active_config].end())
            it->second.Read_text(is);
        else{
            VarInfo vi;
            vi.Read_text(is);
            SAM_config_to_defaults[active_config].insert({name, vi.DefaultValue});
        }

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
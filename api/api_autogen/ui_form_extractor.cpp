/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/SAM/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#include <stdexcept>
#include <iostream>

#include "ui_form_extractor.h"
#include "equation_extractor.h"
#include "variables.h"


#include <rapidjson/stringbuffer.h>
#include <rapidjson/istreamwrapper.h>


std::unordered_map<std::string, std::unordered_map<std::string, VarValue>> SAM_ui_form_to_defaults;
ui_form_extractor_database SAM_ui_extracted_db;

/*
VarValue ui_form_extractor::get_varvalue(rapidjson::Document& doc, const wxString& var_name) {


}


VarValue ui_form_extractor::get_varvalue(wxInputStream &is, const wxString& var_name) {
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
            std::string entry = in.ReadWord().ToStdString();
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
	return vv;
}
*/
/// Formatting of UI form txt taken from InputPageData::Read, VarDatabase::Read
void ui_form_extractor::get_eqn_and_callback_script(rapidjson::Document& doc) {
    auto json_vardatabase = doc["VarDatabase"].GetObject();

    for (rapidjson::Value::ConstMemberIterator itr = json_vardatabase.MemberBegin(); itr != json_vardatabase.MemberEnd(); ++itr) {
        VarInfo vi;
        wxString name = itr->name.GetString();
        vi.Read_JSON(itr->value);
        SAM_ui_form_to_defaults[ui_form_name].insert({ name.ToStdString(), vi.DefaultValue});
    }

    m_eqn_script.clear();
    m_eqn_script = Read_JSON_multiline_value(doc, "Equations");
    m_callback_script.clear();
    m_callback_script = Read_JSON_multiline_value(doc, "Callbacks");

}


bool ui_form_extractor::extract(const std::string& file) {
    rapidjson::Document doc;
    std::ifstream ifs(file);
    rapidjson::IStreamWrapper is(ifs);

    doc.ParseStream(is);

    if (doc.HasParseError()) {
        wxLogError(wxS("Could not read the json file '%s'.\nError: %d"), file, doc.GetParseError());
        return false;
    }
    else {
        get_eqn_and_callback_script(doc);
        return true;
    }
}

/// Populates SAM_ui_extracted_db, SAM_ui_form_to_eqn_info, and
bool ui_form_extractor_database::populate_ui_data(const std::string& ui_path, const std::vector<std::string>& ui_form_names){
    for (const auto& ui_name : ui_form_names){
        ui_form_extractor* ui_fe = SAM_ui_extracted_db.make_entry(ui_name);
        bool success = ui_fe->extract(ui_path + ui_name + ".json");

        if (!success){
            std::cout << "ui_form_extractor_database error: Cannot open " << ui_name << " file at " << ui_path;
            return false;
        }

        ui_fe->export_eqn_infos();
    }
    return true;
}

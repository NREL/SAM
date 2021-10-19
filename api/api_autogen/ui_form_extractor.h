/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

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
#include "equation_extractor.h"
#include "callback_extractor.h"
#include <cstddef>

/**
 * This class extracts the variables, equations and callbacks LK script from the ui form file.
 * It skips through the ui objects, extracts VarValues from the variable definitions to populate
 * the configuration-independent defaults, and stores the equation and callback script strings.
 */

class ui_form_extractor {

private:
    std::string m_eqn_script;
    std::string m_callback_script;

    lk::env_t m_env;
    friend class equation_extractor;
    equation_extractor* eqn_extractor;

    /// Gets default values and stores into SAM_config_to_defaults
    VarValue get_varvalue(wxInputStream &is, const wxString& var_name);

    /// Stores the eqn and callback LK script
    void get_eqn_and_callback_script(wxInputStream& is);

public:
    std::string ui_form_name;

    std::vector<std::string> m_onload_obj;
    std::vector<std::string> m_onchange_obj;

    std::vector<std::string> m_functions;


    ui_form_extractor(const std::string& n){
        ui_form_name = n;
        eqn_extractor = new equation_extractor(n);
    };

    ~ui_form_extractor(){
        delete eqn_extractor;
    }

    bool extract(const std::string& file);

    std::string get_callback_script() {return m_callback_script;}

    bool export_eqn_infos(){
        return eqn_extractor->parse_and_export_eqns(m_eqn_script);
    }

    std::vector<equation_info>* get_eqn_infos() const{
        if (SAM_ui_form_to_eqn_info.find(ui_form_name) != SAM_ui_form_to_eqn_info.end())
            return &(SAM_ui_form_to_eqn_info.find(ui_form_name)->second);
        else
            return nullptr;
    }

};



/**
 * Maps each ui form to class containing its eqn and callback scripts
 */
class ui_form_extractor_database{
private:
    std::unordered_map<std::string, ui_form_extractor*> ui_form_map;

public:
    ui_form_extractor_database() = default;

    ~ui_form_extractor_database(){
        for (auto & it : ui_form_map)
            delete it.second;
    }

    ui_form_extractor* find(const std::string& ui_name){
        auto it = ui_form_map.find(ui_name);
        if (it != ui_form_map.end())
            return it->second;
        else
            return nullptr;
    }

    ui_form_extractor* make_entry(std::string ui_form_name) {
        auto* ufe = new ui_form_extractor(ui_form_name);
        ui_form_map.insert({ui_form_name, ufe});
        return ufe;
    }

    static bool populate_ui_data(const std::string& ui_path, const std::vector<std::string>& ui_form_names);

};

#endif //SYSTEM_ADVISOR_MODEL_EXTRACT_INPUT_PAGE_H

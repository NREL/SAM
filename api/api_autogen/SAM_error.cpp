#include <string>
#include <vector>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

#include "startup_extractor.h"
#include "ui_form_extractor.h"

#include "sam_api.h"

class eqn_datacenter {
private:
    std::unordered_map<std::string, std::vector<std::string>> cmod_to_ui_forms;
    std::unordered_map<std::string, EqnDatabase> eqn_databases;

public:
    eqn_datacenter() {
        // populate cmod_to_ui_forms
    };

    bool load_cmod_eqn_db(std::string cmod) {
//        std::vector<std::string> ui_forms = cmod_to_ui_forms[cmod];
//
//        // load them
//        EqnDatabase eqn_db;
//        eqn_db.LoadScript();
    }
};

class eqn_evaluator {
private:
    lk::varhash_t *var_eqn_map;
    std::vector<std::string> log;

public:
    eqn_evaluator(std::string cmod_type) {};

    bool load_script(std::string source);

    void log_status(std::string msg) {
        log.push_back(msg);
    }
};


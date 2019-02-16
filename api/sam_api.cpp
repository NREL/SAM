#include <string>
#include <vector>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

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
        std::vector<std::string> ui_forms = cmod_to_ui_forms[cmod];

        // load them
        EqnDatabase eqn_db;
        eqn_db.LoadScript()
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

bool eqn_evaluator::load_script(std::string source){
    lk::input_string data( source );
    lk::parser parse( data );
    lk::node_t *tree = parse.script();

    if ( parse.error_count() != 0
         || parse.token() != lk::lexer::END)
    {
        log_status("fail: callback script load: parsing did not reach end of input ");
        for (int x=0; x < parse.error_count(); x++)
            log_status( parse.error(x));

        return false;
    }
    else if ( tree != 0 )
    {
        cb_data *cbf = new cb_data;
        cbf->source = source;
        cbf->tree = tree;
        m_cblist.push_back(cbf);

        lk::eval e( tree, &m_cbenv );

        if ( !e.run() )
        {
            log_status("uicb script eval fail" );
            for (size_t i=0;i<e.error_count();i++)
                log_status( e.get_error(i) );

            return false;
        }
    }

    return true;

}

int main(int argc, char *argv[]) {
    return 0;
}
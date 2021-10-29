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

#ifndef SYSTEM_ADVISOR_MODEL_LK_EVAL_H
#define SYSTEM_ADVISOR_MODEL_LK_EVAL_H

#include <string>
#include <vector>
#include <lk/eval.h>

/**
 * This evaluator maps variable assigments
 */
class extractor_interpreter : public lk::eval{
protected:
    std::string spell_out(lk::expr_t *n, std::vector<std::string> *vertex_names, std::vector<bool> *vertex_is_ssc);

    std::string spell_list(lk::list_t *l, std::vector<std::string> *vertex_names, std::vector<bool> *vertex_is_ssc,
                           bool map_literals_only);

public:
    extractor_interpreter(lk::node_t *tree, lk::env_t* env):lk::eval(tree, env){}

    bool interpret(lk::node_t *root, lk::env_t*cur_env, lk::vardata_t &result, unsigned int flags,
                   unsigned int &ctl_id);

    virtual bool special_set( const lk_string &name, lk::vardata_t &val );
    virtual bool special_get( const lk_string &name, lk::vardata_t &val );

    void map_assignment(lk::node_t *src, lk::node_t *dest);

};


/**
 * This evaluator translates LK into C++
 */
struct function_builder{
    std::string ret_type;
    std::string name;
    // all functions require a var_table to act on
    std::vector<std::string> args = {"var_table* vt"};
    std::string block;
};

std::string indent(std::string s, size_t n = 1);

std::string format_as_variable(std::string str);

std::string format_as_symbol(std::string s);

class translator : public lk::eval{
private:
    std::string ui_form_name;
    std::string code;

    std::unordered_map<std::string, std::string> aux_functions;

    std::string where_at_vector(std::string){
        assert(false);
//        std::vector<lk::vardata_t> *vv = l.deref().vec();
//                        for (size_t i = 0; i < vv->size(); i++)
//                        {
//                            if ((*vv)[i].equals(r.deref()))
//                            {
//                                result.assign((double)i);
//                                return ok;
//                            }
//                        }
//
//                        result.assign(-1.0);
		return "";
    }

public:
    translator(lk::node_t *tree, lk::env_t* env):lk::eval(tree, env){}

    bool translate(lk::node_t *root, lk::env_t *cur_env, std::string &result, unsigned int flags, unsigned int &ctl_id,
                   std::string &output_prefix);

    virtual bool special_set( const lk_string &name, lk::vardata_t &val );

    virtual bool special_get( const lk_string &name, lk::vardata_t &val );

    std::string get_vv_type(const lk_string &name);

    void set_ui_source(std::string ui){
        ui_form_name = ui;
    }



    std::unordered_map<std::string, std::string> get_aux_functions(){ return aux_functions; };

};

#endif //SYSTEM_ADVISOR_MODEL_LK_EVAL_H

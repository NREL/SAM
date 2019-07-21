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

std::string remove_periods(std::string str);

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

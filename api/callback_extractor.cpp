#include <iostream>
#include <sstream>
#include <algorithm>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>
#include <lk/env.h>

#include "ui_form_extractor.h"
#include "callback_extractor.h"
#include "lk_env.h"
#include "data_structures.h"

std::string active_object;
std::string active_ui;
std::string active_subobject;
std::string active_cmod;
int active_method = -1;
bool map_subobject;
std::vector<std::string> subobjects_completed;

std::unordered_map<std::string, std::vector<std::string>> SAM_ui_obj_to_enabled_variables;
extern std::unordered_map<std::string, digraph*> SAM_config_to_variable_graph;

#define ENV_MUTABLE 0x0001

bool extractor_interpreter::special_get(const lk_string &name, lk::vardata_t &val) {
    bool ok = false;
    VarTable* vt = &SAM_config_to_defaults[active_config];
    if ( VarValue *vv = vt->Get( name.ToStdString() ) )
        ok = vv->Write( val );

    if (!ok){
        errors().push_back("special_get error: could not find " + name + " in " + active_config);
    }
    return ok;
}

bool extractor_interpreter::special_set(const lk_string &name, lk::vardata_t &val) {
    bool ok = false;
    VarTable* vt = &SAM_config_to_defaults[active_config];
    if ( VarValue *vv = vt->Get( name ) ){
        ok = vv->Read( val, false );
        if (!ok)
            ok = vv->Read( val, true);
    }

    if (!ok){
        errors().push_back("special_set error: could not find " + name + " in " + active_config);
    }
    return ok;
}


std::string spell_list(lk::list_t *l, std::vector<std::string> *vertex_names, std::vector<bool> *vertex_is_ssc,
                       bool map_literals_only) {
    digraph* graph = SAM_config_to_variable_graph.find(active_config)->second;

    std::string s;

    for (size_t i = 0; i < l->items.size(); i++){
        if (i==0) s += "( ";
        if (lk::literal_t* lit = dynamic_cast<lk::literal_t*>(l->items[i])){
            s += "\"" + lit->value + "\"";
            if (map_literals_only){
                bool is_ssc_var = which_cmod_as_input(lit->value, active_config).length() > 0;
                if (graph->find_vertex(lit->value, is_ssc_var)){
                    if (vertex_names) vertex_names->push_back(lit->value);
                    if (vertex_is_ssc) vertex_is_ssc->push_back(is_ssc_var);
                }
                continue;
            }
        }
        else if (lk::iden_t* id = dynamic_cast<lk::iden_t*>(l->items[i])){
            s += id->name;
            if (graph->find_vertex(id->name, false)){
                if (vertex_names) vertex_names->push_back(id->name);
                if (vertex_is_ssc) vertex_is_ssc->push_back(false);
            }
        }
        else if (lk::constant_t* c = dynamic_cast<lk::constant_t*>(l->items[i])){
            s += std::to_string(c->value);
        }
        else if (lk::expr_t* ex = dynamic_cast<lk::expr_t*>(l->items[i])){
            s += spell_out(ex, vertex_names, vertex_is_ssc);
        }
        else{
            std::cout << "spell_list error in " << active_config << ", " << active_object << ", " << active_subobject;
            return "";
        }
        if (i != l->items.size()-1)
            s += ", ";
    }
    if (l->items.size() > 0)
        s += " )";
    return s;
}

std::string spell_out(lk::expr_t *n, std::vector<std::string> *vertex_names, std::vector<bool> *vertex_is_ssc) {
    digraph* graph = SAM_config_to_variable_graph.find(active_config)->second;
    std::string s;

    // if the expression is calling "value(...)" then the variable is potentially an ssc variable
    bool calling_value_function = false;
    bool calling_ssc_var_function = false;

    if (lk::expr_t* left_e = dynamic_cast<lk::expr_t*>(n->left)){
        s += spell_out(left_e, vertex_names, vertex_is_ssc);
    }
    else if (lk::iden_t* left_i = dynamic_cast<lk::iden_t*>(n->left)){
        s += left_i->name;
        // if function identity is value(...), then getting a value
        if (std::strcmp(left_i->name.c_str(), "value") == 0){
            calling_value_function = true;
            size_t arg_count = dynamic_cast<lk::list_t*>(n->right)->items.size();
            assert(arg_count == 1);
        }
        // if function identity is ssc_var(...), then getting an output from secondary cmod
        else if (std::strcmp(left_i->name.c_str(), "ssc_var") == 0){
            size_t arg_count = dynamic_cast<lk::list_t*>(n->right)->items.size();
            assert(arg_count == 2);
            if (vertex_names) vertex_names->push_back(active_cmod);
            if (vertex_is_ssc) vertex_is_ssc->push_back(false);
            calling_ssc_var_function = true;
        }
        else if (graph->find_vertex(left_i->name, false)){
            if (vertex_names) vertex_names->push_back(left_i->name);
            if (vertex_is_ssc) vertex_is_ssc->push_back(false);
        }
    }
    else if (lk::literal_t* left_l = dynamic_cast<lk::literal_t*>(n->left)){
        s += "\"" +left_l->value + "\"";
    }
    else if (lk::list_t* left_lt = dynamic_cast<lk::list_t*>(n->left)){
        s += spell_list(left_lt, vertex_names, vertex_is_ssc, false);
    }
    else if (lk::constant_t* left_c = dynamic_cast<lk::constant_t*>(n->left)){
        s += std::to_string(left_c->value);
    }
    else if (n->left){
        std::cout << "spell_out error in " << active_config << ", " << active_object << ", " << active_subobject;
        return "";
    }

    s += n->operstr();

    if (lk::expr_t* right_e = dynamic_cast<lk::expr_t*>(n->right)){
        s += spell_out(right_e, vertex_names, vertex_is_ssc);
    }
    else if (lk::iden_t* right_i = dynamic_cast<lk::iden_t*>(n->right)){
        s += right_i->name;
        if (graph->find_vertex(right_i->name, false)){
            if (vertex_names) vertex_names->push_back(right_i->name);
            if (vertex_is_ssc) vertex_is_ssc->push_back(false);
        }
    }
    else if (lk::constant_t* right_c = dynamic_cast<lk::constant_t*>(n->right)){
        s += std::to_string(right_c->value);
    }
    else if (lk::literal_t* right_l = dynamic_cast<lk::literal_t*>(n->right)){
        s += "\"" + right_l->value + "\"";
        if (calling_value_function){
            if (graph->find_vertex(right_l->value, true)){
                if (vertex_names) vertex_names->push_back(right_l->value);
                if (vertex_is_ssc) vertex_is_ssc->push_back(true);
            }
        }
    }
    else if (lk::list_t* right_lt = dynamic_cast<lk::list_t*>(n->right)){
        if (calling_value_function)
            s += spell_list(right_lt, vertex_names, vertex_is_ssc, true);
        else if (calling_ssc_var_function)
            s += spell_list(right_lt, nullptr, nullptr, false);
        else
            s += spell_list(right_lt, vertex_names, vertex_is_ssc, false);
    }
    else if (n->right){
        std::cout << "spell_out error in " << active_config << ", " << active_object << ", " << active_subobject;
        return "";
    }
    return s;
}



void extractor_interpreter::map_assignment(lk::node_t *src, lk::node_t *dest) {

    digraph* graph = SAM_config_to_variable_graph.find(active_config)->second;

    std::string dest_var, expression;
    bool dest_is_ssc = false;

    // find the destination variable
    if (lk::iden_t* dest_id = dynamic_cast<lk::iden_t*>(dest)){
        dest_var = dest_id->name;
        if (dest_id->special && (which_cmod_as_input(dest_id->name.ToStdString(), active_config).length() > 0))
            dest_is_ssc = true;
    }
    else if (lk::expr_t* expr_var = dynamic_cast<lk::expr_t*>(dest)){
        // likely an entry in an array
        while(dynamic_cast<lk::expr_t*>(expr_var->left)){
            expr_var = dynamic_cast<lk::expr_t*>(expr_var->left);
        }
        dest_var = dynamic_cast<lk::iden_t*>(expr_var->left)->name;
    }
    else{
        std::cout << "extractor_interpreter::map_assignment error in dest" << active_config << ", " << active_object;
        std::cout  << ", " << active_subobject << " at " << dest->line() << " in ";
        std::cout << find_ui_of_object(active_subobject, active_config)->ui_form_name << "\n";
        assert(false);
    }


    // source variables and whether they are primary ssc variables
    std::vector<std::string> vertex_names;
    std::vector<bool> vertex_is_ssc;

    if (lk::iden_t* src_id = dynamic_cast<lk::iden_t*>(src)){
        vertex_names.push_back(src_id->name);
        bool is_ssc = false;
        if ( src_id->special && (which_cmod_as_input(src_id->name, active_config).length() > 0))
            is_ssc = true;
        vertex_is_ssc.push_back(is_ssc);
    }
    // if value or special_get was called, may be primary ssc variable
    else if (lk::expr_t* node = dynamic_cast<lk::expr_t*>(src)){
        expression = spell_out(node, &vertex_names, &vertex_is_ssc);
    }
    else if (dynamic_cast<lk::literal_t*>(src) || dynamic_cast<lk::constant_t*>(src)){
        // if assigning a string or number, don't need to map anything but save the vertex for future references
        graph->add_vertex(dest_var, dest_is_ssc);
        return;
    }
    else if (lk::cond_t* cond = dynamic_cast<lk::cond_t*>(src)){
        map_assignment(cond->on_true, dest);
        map_assignment(cond->on_false, dest);
        return;
    }
    else{
        std::cout << "extractor_interpreter::map_assignment error in source" << active_config << ", " << active_object;
        std::cout  << ", " << active_subobject << " at " << src->line() << " in ";
        std::cout << find_ui_of_object(active_subobject, active_config)->ui_form_name << "\n";
        assert(false);
    }

    if (expression.find("ssc_var") != std::string::npos && expression.find("value") != std::string::npos){
        std::cout << expression << " has both ssc_var and value in same assignment, damn\n";
        assert(false);
    }

    // add variable and mapping to graph if they don't already exist
    vertex* v = graph->add_vertex(dest_var, dest_is_ssc);
    if (active_cmod.length() > 0)
        v->cmod = active_cmod;

    std::string obj_stack = active_object
                            + (active_subobject.length() > 0 ? ":" + active_subobject  : "")
                            + (active_cmod.length() > 0 ? ":" + active_cmod  : "");

    for (size_t i = 0; i < vertex_names.size(); i++){
        graph->add_edge(vertex_names[i], vertex_is_ssc[i], dest_var, dest_is_ssc, active_method, obj_stack, expression);
    }
}

static void do_plus_eq(lk::vardata_t &l, lk::vardata_t &r)
{
    if (l.deref().type() == lk::vardata_t::STRING)
        l.deref().assign(l.deref().as_string() + r.deref().as_string());
    else if (l.deref().type() == lk::vardata_t::VECTOR)
    {
        if (r.deref().type() == lk::vardata_t::VECTOR)
        {
            for (size_t i = 0; i < r.deref().length(); i++)
                l.deref().vec()->push_back(*r.deref().index(i));
        }
        else
            // append to the vector
            l.deref().vec()->push_back(r.deref());
    }
    else
        l.deref().assign(l.deref().num() + r.deref().as_number());
}

static void do_minus_eq(lk::vardata_t &l, lk::vardata_t &r)
{
    l.deref().assign(l.deref().num() - r.deref().num());
}

static void do_mult_eq(lk::vardata_t &l, lk::vardata_t &r)
{
    l.deref().assign(l.deref().num() * r.deref().num());
}

static void do_div_eq(lk::vardata_t &l, lk::vardata_t &r)
{
    l.deref().assign(l.deref().num() / r.deref().num());
}

bool extractor_interpreter::interpret(lk::node_t *root,
                         lk::env_t *cur_env,
                         lk::vardata_t &result,
                         unsigned int flags,
                         unsigned int &ctl_id)
{
    using namespace lk;
    if (!root) return true;

    if (!on_run(root->line()))
        return false; /* abort script execution */

    if (list_t *n1 = dynamic_cast<list_t*>(root))
    {
        ctl_id = CTL_NONE;
        bool ok = true;
        for (size_t i = 0; i < n1->items.size() && ctl_id == CTL_NONE; i++)
        {
            ok = interpret(n1->items[i], cur_env, result, flags, ctl_id);
            if (!ok)
            {
                std::cout << "error at line: " << n1->line() << " eval error in statement list\n";
                return false;
            }
        }

        return ok;
    }
    else if (iter_t *n2 = dynamic_cast<iter_t*>(root))
    {
        if (!interpret(n2->init, cur_env, result, flags, ctl_id)) return false;

        while (1)
        {
            // test the condition
            vardata_t outcome;
            outcome.assign(0.0);

            if (!interpret(n2->test, cur_env, outcome, flags, ctl_id)) return false;

            if (!outcome.as_boolean())
                break;

            if (!interpret(n2->block, cur_env, result, flags, ctl_id)) return false;

            switch (ctl_id)
            {
                case CTL_BREAK:
                    ctl_id = CTL_NONE;
                case CTL_RETURN:
                case CTL_EXIT:
                    return true;

                case CTL_CONTINUE:
                default:
                    ctl_id = CTL_NONE;
            }

            if (!interpret(n2->adv, cur_env, result, flags, ctl_id)) return false;
        }

        return true;
    }
    else if (cond_t *n3 = dynamic_cast<cond_t*>(root))
    {
        vardata_t outcome;
        outcome.assign(0.0);
        if (!interpret(n3->test, cur_env, outcome, flags, ctl_id)) return false;

        if (outcome.as_boolean())
            return interpret(n3->on_true, cur_env, result, flags, ctl_id);
        else
            return interpret(n3->on_false, cur_env, result, flags, ctl_id);
    }
    else if (expr_t *n4 = dynamic_cast<expr_t*>(root))
    {
        try
        {
            bool ok = true;
            vardata_t l, r;
            double newval;

            switch (n4->oper)
            {
                case expr_t::PLUS:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);
                    if (l.deref().type() == vardata_t::STRING
                        || r.deref().type() == vardata_t::STRING)
                    {
                        result.assign(l.deref().as_string() + r.deref().as_string());
                    }
                    else
                        result.assign(l.deref().num() + r.deref().num());
                    return ok;
                case expr_t::MINUS:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);
                    result.assign(l.deref().num() - r.deref().num());
                    return ok;
                case expr_t::MULT:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);
                    result.assign(l.deref().num() * r.deref().num());
                    return ok;
                case expr_t::DIV:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);
                    if (r.deref().num() == 0)
                        result.assign(std::numeric_limits<double>::quiet_NaN());
                    else
                        result.assign(l.deref().num() / r.deref().num());
                    return ok;

                case expr_t::PLUSEQ:
                    do_op_eq(do_plus_eq, n4, cur_env, flags, ctl_id, result, l, r);
                    return ok;
                case expr_t::MINUSEQ:
                    do_op_eq(do_minus_eq, n4, cur_env, flags, ctl_id, result, l, r);
                    return ok;
                case expr_t::MULTEQ:
                    do_op_eq(do_mult_eq, n4, cur_env, flags, ctl_id, result, l, r);
                    return ok;
                case expr_t::DIVEQ:
                    do_op_eq(do_div_eq, n4, cur_env, flags, ctl_id, result, l, r);
                    return ok;

                case expr_t::MINUSAT:
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);

                    if (l.deref().type() == vardata_t::HASH)
                    {
                        lk::varhash_t *hh = l.deref().hash();
                        lk::varhash_t::iterator it = hh->find(r.deref().as_string());
                        if (it != hh->end())
                            hh->erase(it);
                    }
                    else if (l.deref().type() == vardata_t::VECTOR)
                    {
                        std::vector<lk::vardata_t> *vv = l.deref().vec();
                        size_t idx = r.deref().as_unsigned();
                        if (idx < vv->size())
                            vv->erase(vv->begin() + idx);
                    }
                    else
                    {
                        std::cout << "error at line " << n4->line();
                        std::cout << "-@ operator requires a hash or vector left hand side\n";
                        return false;
                    }

                    return true;

                case expr_t::INCR:
                    ok = ok && interpret(n4->left, cur_env, l, flags | ENV_MUTABLE, ctl_id);
                    newval = l.deref().num() + 1;
                    l.deref().assign(newval);
                    result.assign(newval);
                    return ok;
                case expr_t::DECR:
                    ok = ok && interpret(n4->left, cur_env, l, flags | ENV_MUTABLE, ctl_id);
                    newval = l.deref().num() - 1;
                    l.deref().assign(newval);
                    result.assign(newval);
                    return ok;
                case expr_t::DEFINE:
                    result.assign(n4);
                    return ok;
                case expr_t::ASSIGN:


                    // evaluate expression before the lhs identifier
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);

                    // active_method only active during equation or callback parsing
                    if (active_method >= 0) {
                        // if parsing a subobject, check if it's been mapped before
                        if ((active_subobject.length() > 0) && map_subobject)
                        {
                            map_assignment(n4->right, n4->left);
                        }
                        else if (active_subobject.length() == 0){
                            map_assignment(n4->right, n4->left);
                        }
                    }

                    // if on the LHS of the assignment we have a special variable i.e. ${xy}, use a
                    // hack to assign the value to the storage location
                    if (lk::iden_t *iden = dynamic_cast<lk::iden_t*>(n4->left))
                        if (iden->special)
                            return ok && special_set(iden->name, r.deref()); // don't bother to copy rhs to result either.

                    // otherwise evaluate the LHS in a mutable context, as normal.
                    ok = ok && interpret(n4->left, cur_env, l, flags | ENV_MUTABLE, ctl_id);
                    l.deref().copy(r.deref());
                    result.copy(r.deref());

                    return ok;
                case expr_t::LOGIOR:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    if (((int)l.deref().num()) != 0) // short circuit evaluation
                    {
                        result.assign(1.0);
                        return ok;
                    }
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);
                    result.assign((((int)l.deref().num()) || ((int)r.deref().num())) ? 1 : 0);
                    return ok;
                case expr_t::LOGIAND:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    if (((int)l.deref().num()) == 0) // short circuit evaluation
                    {
                        result.assign(0.0);
                        return ok;
                    }
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);
                    result.assign((((int)l.deref().num()) && ((int)r.deref().num())) ? 1 : 0);
                    return ok;
                case expr_t::NOT:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    result.assign(((int)l.deref().num()) ? 0 : 1);
                    return ok;
                case expr_t::EQ:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);
                    result.assign(l.deref().equals(r.deref()) ? 1 : 0);
                    return ok;
                case expr_t::NE:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);
                    result.assign(l.deref().equals(r.deref()) ? 0 : 1);
                    return ok;
                case expr_t::LT:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);
                    result.assign(l.deref().lessthan(r.deref()) ? 1 : 0);
                    return ok;
                case expr_t::LE:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);
                    result.assign(l.deref().lessthan(r.deref()) || l.deref().equals(r.deref()) ? 1 : 0);
                    return ok;
                case expr_t::GT:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);
                    result.assign(!l.deref().lessthan(r.deref()) && !l.deref().equals(r.deref()) ? 1 : 0);
                    return ok;
                case expr_t::GE:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);
                    result.assign(!l.deref().lessthan(r.deref()) ? 1 : 0);
                    return ok;
                case expr_t::EXP:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);
                    result.assign(pow(l.deref().num(), r.deref().num()));
                    return ok;
                case expr_t::NEG:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    result.assign(0 - l.deref().num());
                    return ok;
                case expr_t::WHEREAT:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    ok = ok && interpret(n4->right, cur_env, r, flags, ctl_id);
                    if (l.deref().type() == vardata_t::HASH)
                    {
                        lk::varhash_t *hh = l.deref().hash();
                        result.assign(hh->find(r.deref().as_string()) != hh->end() ? 1.0 : 0.0);
                    }
                    else if (l.deref().type() == vardata_t::VECTOR)
                    {
                        std::vector<lk::vardata_t> *vv = l.deref().vec();
                        for (size_t i = 0; i < vv->size(); i++)
                        {
                            if ((*vv)[i].equals(r.deref()))
                            {
                                result.assign((double)i);
                                return ok;
                            }
                        }

                        result.assign(-1.0);
                        return ok;
                    }
                    else if (l.deref().type() == vardata_t::STRING)
                    {
                        lk_string::size_type pos = l.deref().str().find(r.deref().as_string());
                        result.assign(pos != lk_string::npos ? (int)pos : -1.0);
                    }
                    else
                    {
                        std::cout << "error at line: " << n4->line();
                        std::cout << "left hand side to find operator ?@ must be a hash, vector, or string\n";
                        return false;
                    }
                    return ok;
                case expr_t::INDEX:
                {
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    bool anonymous = (l.type() == vardata_t::VECTOR);

                    vardata_t &arr = l.deref();

                    if (!(flags&ENV_MUTABLE) && arr.type() != vardata_t::VECTOR)
                    {
                        std::cout << "error at line: " << n4->left->line();
                        std::cout << "cannot index non array data in non mutable context\n";
                        return false;
                    }

                    ok = ok && interpret(n4->right, cur_env, r, 0, ctl_id);
                    size_t idx = r.deref().as_unsigned();

                    if ((flags&ENV_MUTABLE)
                        && (arr.type() != vardata_t::VECTOR
                            || arr.length() <= idx))
                        arr.resize(idx + 1);

                    vardata_t *item = arr.index(idx);
                    if (anonymous)
                        result.copy(*item);
                    else
                        result.assign(item);

                    return ok;
                }
                case expr_t::HASH:
                {
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    bool anonymous = (l.type() == vardata_t::HASH);

                    vardata_t &hash = l.deref();

                    if ((flags&ENV_MUTABLE)
                        && (hash.type() != vardata_t::HASH))
                        hash.empty_hash();

                    ok = ok && interpret(n4->right, cur_env, r, 0, ctl_id);
                    vardata_t &val = r.deref();

                    vardata_t *x = hash.lookup(val.as_string());
                    if (x)
                    {
                        if (anonymous)
                            result.copy(*x);
                        else
                            result.assign(x);
                    }
                    else if ((flags&ENV_MUTABLE))
                    {
                        hash.assign(val.as_string(), x = new vardata_t);
                        result.assign(x);
                    }
                    else
                        result.nullify();

                    return ok;
                }
                case expr_t::CALL:
                case expr_t::THISCALL:
                {
                    expr_t *cur_expr = n4;

                    if (iden_t *iden = dynamic_cast<iden_t*>(n4->left))
                    {
                        // query function table for identifier
                        if (lk::fcallinfo_t *fi = cur_env->lookup_func(iden->name))
                        {
                            lk::invoke_t cxt(cur_env, result, fi->user_data);
                            list_t *argvals = dynamic_cast<list_t*>(n4->right);

                            // first determine number of arguments
                            size_t nargs = 0;
                            if (argvals) nargs = argvals->items.size();

                            if (nargs > 0)
                            {
                                // allocate argument vector and evaluate each argument
                                cxt.arg_list().resize(nargs, vardata_t());

                                std::string identity_tracker = "args";
                                for (size_t iarg = 0; iarg < nargs; iarg++)
                                {
                                    unsigned int c = CTL_NONE;

                                    lk::vardata_t &argval = cxt.arg_list()[iarg];
                                    if (!interpret(argvals->items[iarg], cur_env, argval, flags, c))
                                    {
                                        std::cout << "error at line: " << argvals->line();
                                        std::cout << "failed to evaluate function call argument\n";
                                        return false;
                                    }
                                    // save the name of the argument as it gets replaced by the calculated value
                                    if (lk::iden_t* var_iden = dynamic_cast<lk::iden_t*>(argvals->items[iarg])){
                                        identity_tracker += ":" + var_iden->name;
                                    }
                                    else if (lk::literal_t* var_lit = dynamic_cast<lk::literal_t*>(argvals->items[iarg])){
                                        identity_tracker += ":" + var_lit->value;
                                    }
                                    else if (lk::constant_t* var_const = dynamic_cast<lk::constant_t*>(argvals->items[iarg])){
                                        identity_tracker += ":" + std::to_string(var_const->value);
                                    }
                                    else if (lk::expr_t* var_expr = dynamic_cast<lk::expr_t*>(argvals->items[iarg])){
                                        identity_tracker += ":" + spell_out(var_expr, nullptr, nullptr);
                                    }
                                    else{
                                        identity_tracker += ":" + cxt.arg(iarg).as_string().ToStdString();
                                    }
                                }
                                cxt.error(identity_tracker);
                            }

                            try {
                                if (fi->f) {
                                    (*(fi->f))(cxt);
                                    clear_arg_string(cxt);

                                }
                                else if (fi->f_ext){
                                    lk::external_call(fi->f_ext, cxt);
                                    clear_arg_string(cxt);
                                }
                                else cxt.error(lk_tr("invalid internal reference to function callback") + " " + iden->name);
                            }
                            catch (std::exception &e)
                            {
                                cxt.error(e.what());
                            }

                            if (cxt.has_error())
                                std::cout << "error calling " << iden->name + "\n" ;

                            // do a deep copy of internalized references
                            result.deep_localize();

                            return !cxt.has_error();
                        }
                    }

                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    expr_t *define = dynamic_cast<expr_t*>(l.deref().func());
                    if (!define)
                    {
                        std::cout << "error at line: " << n4->line ();
                        std::cout << "error in function call: malformed 'define'\n";
                        return false;
                    }

                    // mark entrance into another defined object
                    if (lk::iden_t* iden = dynamic_cast<iden_t*>(n4->left))
                        active_subobject = iden->name.ToStdString();
                    else if (lk::expr_t* expr = dynamic_cast<expr_t*>(n4->left)){
                        std::string str = "";
                        if (lk::literal_t* node_right = dynamic_cast<literal_t*>(expr->right)){
                            str += node_right->value.ToStdString();
                        }
                        active_subobject = str;
                    }
                    else{
                        std::cout << "could not identify called function\n";
                        assert(false);
                    }

                    // save the subobject so we don't repeat the map_assignment
                    if (std::find(subobjects_completed.begin(), subobjects_completed.end(), active_subobject)
                        == subobjects_completed.end()){
                        subobjects_completed.push_back(active_subobject);
                        map_subobject = true;
                    }
                    else{
                        map_subobject = false;
                    }

                    node_t *block = define->right;

                    // create new environment frame
                    env_t frame(cur_env);

                    // number of expected arguments
                    list_t *argnames = dynamic_cast<list_t*>(define->left);
                    size_t nargs_expected = argnames ? argnames->items.size() : 0;

                    // number of provided arguments
                    list_t *argvals = dynamic_cast<list_t*>(n4->right);
                    size_t nargs_given = argvals ? argvals->items.size() : 0;

                    if (n4->oper == expr_t::THISCALL)
                        nargs_given++;

                    if (nargs_given < nargs_expected)
                    {
                        std::cout << "error at line: " << n4->line();
                        return false;
                    }

                    // evaluate each argument and assign it into the new environment
                    expr_t *thisexpr = dynamic_cast<expr_t*>(cur_expr->left);
                    if (cur_expr->oper == expr_t::THISCALL
                        &&  thisexpr != 0
                        && thisexpr->left != 0)
                    {
                        vardata_t thisobj;
                        unsigned int c = CTL_NONE;
                        if (!interpret(thisexpr->left, cur_env, thisobj, flags, c))
                        {
                            std::cout << "error at line: " << cur_expr->line();
                            std::cout << "failed to evaluate 'this' parameter 0 for THISCALL -> method\n";
                            return false;
                        }

                        if (thisobj.type() != vardata_t::REFERENCE)
                        {
                            std::cout << "error at line: " << cur_expr->line();
                            std::cout << "'this' parameter did not evaluate to a reference\n";
                            return false;
                        }

                        frame.assign("this", new vardata_t(thisobj));
                    }

                    vardata_t *__args = new vardata_t;
                    __args->empty_vector();

                    if (argvals)
                    {
                        for (size_t argindex = 0;
                             argindex < argvals->items.size();
                             argindex++)
                        {
                            vardata_t v;
                            iden_t *id = 0;

                            unsigned int c = CTL_NONE;
                            if (!interpret(argvals->items[argindex], cur_env, v, flags, c))
                            {
                                std::cout << "error at line: " << argvals->items[argindex];
                                std::cout << "failed to initialize function call argument\n";
                                return false;
                            }

                            if (argindex < argnames->items.size() && ((id = dynamic_cast<iden_t*>(argnames->items[argindex]))!=0))
                                frame.assign(id->name, new vardata_t(v));

                            __args->vec()->push_back(vardata_t(v));
                        }
                    }

                    frame.assign("__args", __args);

                    // now evaluate the function block in the new environment
                    if (!interpret(block, &frame, result, flags, ctl_id))
                    {
                        std::cout << "error at line: " << block->line() << " error inside function call\n";
                        return false;
                    }

                    // mark return from the defined object
                    active_subobject = "";
                    map_subobject = true;

                    // do a deep copy of internalized references
                    result.deep_localize();

                    // reset the sequence control
                    if (ctl_id != CTL_EXIT)	ctl_id = CTL_NONE;

                    // environment frame will automatically be destroyed here
                    return true;
                }
                    break;
                case expr_t::SIZEOF:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    if (l.deref().type() == vardata_t::VECTOR)
                    {
                        result.assign((int)l.deref().length());
                        return ok;
                    }
                    else if (l.deref().type() == vardata_t::STRING)
                    {
                        result.assign((int)l.deref().str().length());
                        return ok;
                    }
                    else if (l.deref().type() == vardata_t::HASH)
                    {
                        int count = 0;

                        varhash_t *h = l.deref().hash();
                        for (varhash_t::iterator it = h->begin();
                             it != h->end();
                             ++it)
                        {
                            if ((*it).second->deref().type() != vardata_t::NULLVAL)
                                count++;
                        }
                        result.assign(count);
                        return ok;
                    }
                    else
                    {
                        std::cout << "error at line: " << n4->line();
                        std::cout << "operand to # ('sizeof') must be a array, string, or table type\n";
                        return false;
                    }
                    break;
                case expr_t::KEYSOF:
                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
                    if (l.deref().type() == vardata_t::HASH)
                    {
                        varhash_t *h = l.deref().hash();
                        result.empty_vector();
                        result.vec()->reserve(h->size());
                        for (varhash_t::iterator it = h->begin();
                             it != h->end();
                             ++it)
                        {
                            if ((*it).second->deref().type() != vardata_t::NULLVAL)
                                result.vec_append((*it).first);
                        }
                        return true;
                    }
                    else
                    {
                        std::cout << "error at line: " << n4->line() << " operand to @ (keysof) must be a table\n";
                        return false;
                    }
                    break;
                case expr_t::TYPEOF:
                    if (lk::iden_t *iden = dynamic_cast<lk::iden_t*>(n4->left))
                    {
                        if (lk::vardata_t *vv = cur_env->lookup(iden->name, true)) result.assign(vv->typestr());
                        else result.assign("unknown");
                        return true;
                    }
                    else
                    {
                        std::cout << "error at line: " << n4->line() << " argument to typeof(...) must be an identifier\n";
                        return false;
                    }
                    break;
                case expr_t::INITVEC:
                {
                    result.empty_vector();
                    list_t *p = dynamic_cast<list_t*>(n4->left);
                    if (p)
                    {
                        for (size_t i = 0; i < p->items.size(); i++)
                        {
                            vardata_t v;
                            ok = ok && interpret(p->items[i], cur_env, v, flags, ctl_id);
                            result.vec()->push_back(v.deref());
                        }
                    }
                }
                    return ok && ctl_id == CTL_NONE;
                case expr_t::INITHASH:
                {
                    result.empty_hash();
                    list_t *p = dynamic_cast<list_t*>(n4->left);
                    if (p)
                    {
                        for (size_t i = 0; i < p->items.size(); i++)
                        {
                            expr_t *assign = dynamic_cast<expr_t*>(p->items[i]);
                            if (assign && assign->oper == expr_t::ASSIGN)
                            {
                                vardata_t vkey, vval;
                                ok = ok && interpret(assign->left, cur_env, vkey, flags, ctl_id)
                                     && interpret(assign->right, cur_env, vval, flags, ctl_id);

                                if (ok)
                                {
                                    lk_string key = vkey.as_string();
                                    varhash_t *h = result.hash();
                                    varhash_t::iterator it = h->find(key);
                                    if (it != h->end())
                                        (*it).second->copy(vval.deref());
                                    else
                                        (*h)[key] = new vardata_t(vval.deref());
                                }
                            }
                        }
                    }
                }
                    return ok && ctl_id == CTL_NONE;
                case expr_t::SWITCH:
                {
                    vardata_t switchval;
                    switchval.assign(-1.0);
                    if (!interpret(n4->left, cur_env, switchval, flags, ctl_id)) return false;
                    list_t *p = dynamic_cast<list_t*>(n4->right);
                    size_t index = switchval.as_unsigned();
                    if (!p || index >= p->items.size())
                    {
                        std::cout << "error at line: " << n4->line() << " invalid switch statement index\n";
                        return false;
                    }

                    if (!interpret(p->items[index], cur_env, result, flags, ctl_id)) return false;

                    return ok;
                }
                default:
                    break;
            }
        }
        catch (lk::error_t &e) {
            std::cout << "caught error at line: " << n4->line() << "\n";
            return false;
        }
    }
    else if (ctlstmt_t *n5 = dynamic_cast<ctlstmt_t*>(root))
    {
        try {
            vardata_t l;
            bool ok = true;
            switch (n5->ictl)
            {
                case ctlstmt_t::RETURN:
                    if (n5->rexpr != 0)
                    {
                        ok = ok && interpret(n5->rexpr, cur_env, l, flags, ctl_id);
                        result.copy(l.deref());
                    }
                    ctl_id = CTL_RETURN;
                    return ok;
                case ctlstmt_t::EXIT:
                    ctl_id = CTL_EXIT;
                    return true;
                    break;
                case ctlstmt_t::BREAK:
                    ctl_id = CTL_BREAK;
                    return true;
                    break;
                case ctlstmt_t::CONTINUE:
                    ctl_id = CTL_CONTINUE;
                    return true;
                    break;
            }
        }
        catch (lk::error_t &e) {
            std::cout << "caught error at line: " << n5->line() << "\n";
            return false;
        }
    }
    else if (iden_t *n6 = dynamic_cast<iden_t*>(root))
    {
        if (n6->special && !(flags&ENV_MUTABLE))
            return special_get(n6->name, result);

        vardata_t *x = cur_env->lookup(n6->name, !(flags&ENV_MUTABLE));

        if (x)
        {
            if (n6->constval)
            {
                std::cout << "error at line: " << n6->line();
                std::cout << " overriding previous non-const identifier with const-ness not allowed: " + n6->name + "\n";
                result.nullify();
                return false;
            }

            if (n6->globalval)
            {
                std::cout << "error at line: " << n6->line();
                std::cout << " overriding previous non-global identifier with global-ness not allowed: " + n6->name + "\n";
                result.nullify();
                return false;
            }

            result.assign(x);
            return true;
        }
        else if (!x && (flags&ENV_MUTABLE))
        {
            // check if the variable exists in the global frame
            // and it was originally created as a global variable
            x = cur_env->global()->lookup(n6->name, false);
            if (x && x->flagval(vardata_t::GLOBALVAL))
            {
                result.assign(x);
                return true;
            }

            x = new vardata_t;

            if (n6->constval)
            {
                x->set_flag(vardata_t::CONSTVAL);
                x->clear_flag(vardata_t::ASSIGNED);
            }

            if (n6->globalval)
            {
                x->set_flag(vardata_t::GLOBALVAL);
                cur_env->global()->assign(n6->name, x);
            }
            else
                cur_env->assign(n6->name, x);

            // also save to ui_form_extractor_database
            if (active_ui.length() > 0){
                ui_form_extractor* ui_fe = SAM_ui_extracted_db.find(active_ui);
                if (n6->name != "on_load" && n6->name != "on_change")
                    ui_fe->m_functions.push_back(n6->name.ToStdString());
            }

            result.assign(x);
            return true;
        }
        else
        {
            std::cout << "error at line: " << n6->line();
            std::cout << " reference to unassigned variable: " + n6->name + "\n";
            result.nullify();
            return false;
        }
    }
    else if (constant_t *n7 = dynamic_cast<constant_t*>(root))
    {
        result.assign(n7->value);
        return true;
    }
    else if (literal_t *n8 = dynamic_cast<literal_t*>(root))
    {
        result.assign(n8->value);
        return true;
    }
    else if (0 != dynamic_cast<null_t*>(root))
    {
        result.nullify();
        return true;
    }

    return false;
}



size_t callback_extractor::parse_cmod_statement(std::string callback_script, size_t pos_start){
    size_t start = callback_script.find_first_of("\'\"", pos_start);
    size_t end = callback_script.find_first_of("\'\"", start+1);
    if (start >= end){
        std::cout << "Error extracting cmod name from callback script for " << config_name << "\n";
        return 0;
    }
    std::string str = callback_script.substr(start+1, (end-start-1));

    // remove white space & quotations
    std::string::iterator end_pos = std::remove(str.begin(), str.end(), ' ');
    str.erase(end_pos, str.end());

    //compute_modules.push_back(str);
    return end;
}


bool callback_extractor::parse_script(std::string callback_script) {
    lk::input_string in( callback_script );
    lk::parser parse( in );
    lk::node_t *tree = parse.script();

    if ( parse.error_count() != 0 || parse.token() != lk::lexer::END){
        for( int i=0;i<parse.error_count();i++ )
            errors.push_back( parse.error(i) );
        errors.push_back( "callback script parsing did not reach end of input" );
        if ( tree ) delete tree;
        return false;
    }
    else
    {
        extractor_interpreter e( tree, m_env );

        bool ok = e.run();
        if ( !ok ){
            for( size_t i=0;i<e.error_count();i++ )
                errors.push_back( e.get_error(i) );
            return false;
        }

    }
    return true;
}

int callback_extractor::invoke_method_type(const std::string &method_name) {
    lk::vardata_t *cbvar = m_env->lookup( method_name, true);

    if (!cbvar || cbvar->type() != lk::vardata_t::HASH )
    {
        std::cout << "callback_extractor::invoke_method_type: could not find " + method_name + " in " + config_name << "\n";
        return 0;
    }

    // get all instances of method
    int error = 0;

    lk::varhash_t* h = cbvar->hash();
    for (auto it = h->begin(); it != h->end(); ++it){

        std::string obj_name = it->first.ToStdString();
        lk::expr_t *p_define = it->second->deref().func();
        if ( p_define->oper != lk::expr_t::DEFINE )
        {
            std::cout << "callback_extractor::invoke_method_type: structure of " + obj_name;
            std::cout << " not " + method_name + " in " + config_name << "\n";
            return 0;
        }

        if ( p_define->right == 0 )
        {
            std::cout << "callback_extractor::invoke_method_type: block undefined for" + obj_name;
            std::cout << " not " + method_name + " in " + config_name << "\n";
            return 0;
        }

        // clear active_cmod in case it was set previously in another object invocation
        active_cmod = "";
        active_object = obj_name;
        active_subobject = "";
        map_subobject = true;
        if (active_object== "Inverter CEC Coefficient Generator"){
//            std::cout << "stophere\n";
        }
        if (!invoke_function(p_define->right, obj_name))
            error += 1;

    }
    return error;
}

bool callback_extractor::invoke_function(lk::node_t *root, std::string f_name) {

    try {
        extractor_interpreter eval = extractor_interpreter(root, m_env);
        if ( !eval.run())
        {
            for (size_t i=0;i<eval.error_count();i++)
                errors.push_back(eval.get_error(i));
            return false;
        }
        return true;

    } catch(std::exception &e ){
        std::cout << "callback_extractor::invoke_function error: could not invoke "<< f_name << " for " << config_name << "\n";
        return false;
    }

}

bool callback_extractor::extract_functions() {
    active_method = LOAD;
    int nerrors = invoke_method_type("on_load");
    if (nerrors > 0){
        std::cout << "callback_extractor::extract_functions error: " << nerrors << " 'on_load' obj errors\n";
        for (size_t i = 0; i < errors.size(); i++)
            std::cout << errors[i] << "\n";
    }

    active_method = CHNG;
    nerrors = invoke_method_type("on_change");
    if (nerrors > 0){
        std::cout << "callback_extractor::extract_functions error: " << nerrors << " 'on_change' obj errors\n";
        for (size_t i = 0; i < errors.size(); i++)
            std::cout << errors[i] << "\n";
    }

    active_method = -1;

    return true;
}
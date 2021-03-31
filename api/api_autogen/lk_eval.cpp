#include <iostream>
#include <sstream>
#include <algorithm>
#include <cctype>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>
#include <lk/env.h>

#include "lk_eval.h"
#include "lk_env.h"
#include "data_structures.h"

#include "ui_form_extractor.h"

std::vector<std::string> subobjects_completed;

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


std::string extractor_interpreter::spell_list(lk::list_t *l, std::vector<std::string> *vertex_names, std::vector<bool> *vertex_is_ssc,
                       bool map_literals_only) {
    digraph* graph = SAM_config_to_variable_graph.find(active_config)->second;

    std::string s;

    for (size_t i = 0; i < l->items.size(); i++){
        if (i==0) s += "( ";
        if (lk::literal_t* lit = dynamic_cast<lk::literal_t*>(l->items[i])){
            s += "\"";
            s += lit->value;
            s += "\"";
            if (map_literals_only){
                bool is_ssc_var = which_cmod_as_input(lit->value.ToStdString(), active_config).length() > 0;
                if (graph->find_vertex(std::string(lit->value), is_ssc_var)){
                    if (vertex_names) vertex_names->push_back(lit->value.ToStdString());
                    if (vertex_is_ssc) vertex_is_ssc->push_back(is_ssc_var);
                }
                continue;
            }
        }
        else if (lk::iden_t* id = dynamic_cast<lk::iden_t*>(l->items[i])){
            s += id->name;
            if (graph->find_vertex(id->name.ToStdString(), false)){
                if (vertex_names) vertex_names->push_back(id->name.ToStdString());
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

std::string extractor_interpreter::spell_out(lk::expr_t *n, std::vector<std::string> *vertex_names, std::vector<bool> *vertex_is_ssc) {
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
        else if (graph->find_vertex(left_i->name.ToStdString(), false)){
            if (vertex_names) vertex_names->push_back(left_i->name.ToStdString());
            if (vertex_is_ssc) vertex_is_ssc->push_back(false);
        }
    }
    else if (lk::literal_t* left_l = dynamic_cast<lk::literal_t*>(n->left)){
        s += "\"";
        s += left_l->value;
        s += "\"";
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
        if (graph->find_vertex(right_i->name.ToStdString(), false)){
            if (vertex_names) vertex_names->push_back(right_i->name.ToStdString());
            if (vertex_is_ssc) vertex_is_ssc->push_back(false);
        }
    }
    else if (lk::constant_t* right_c = dynamic_cast<lk::constant_t*>(n->right)){
        s += std::to_string(right_c->value);
    }
    else if (lk::literal_t* right_l = dynamic_cast<lk::literal_t*>(n->right)){
        s += "\"" + right_l->value + "\"";
        if (calling_value_function){
            if (graph->find_vertex(right_l->value.ToStdString(), true)){
                if (vertex_names) vertex_names->push_back(right_l->value.ToStdString());
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

    std::string ui_name = find_ui_of_variable(dest_var, active_config);

    // source variables and whether they are primary ssc variables
    std::vector<std::string> vertex_names;
    std::vector<bool> vertex_is_ssc;

    if (lk::iden_t* src_id = dynamic_cast<lk::iden_t*>(src)){
        vertex_names.push_back(src_id->name.ToStdString());
        bool is_ssc = false;
        if ( src_id->special && (which_cmod_as_input(src_id->name.ToStdString(), active_config).length() > 0))
            is_ssc = true;
        vertex_is_ssc.push_back(is_ssc);
    }
        // if value or special_get was called, may be primary ssc variable
    else if (lk::expr_t* node = dynamic_cast<lk::expr_t*>(src)){
        expression = spell_out(node, &vertex_names, &vertex_is_ssc);
    }
    else if (dynamic_cast<lk::literal_t*>(src) || dynamic_cast<lk::constant_t*>(src)){
        // if assigning a string or number, don't need to map anything but save the vertex for future references
        graph->add_vertex(dest_var, dest_is_ssc, ui_name);
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
        std::cout << ui_name << "\n";
        assert(false);
    }

    if (expression.find("ssc_var") != std::string::npos && expression.find("value") != std::string::npos){
        std::cout << expression << " has both ssc_var and value in same assignment, damn\n";
        assert(false);
    }

    // add variable and mapping to graph if they don't already exist
    vertex* v = graph->add_vertex(dest_var, dest_is_ssc, ui_name);
    if (active_cmod.length() > 0)
        v->cmod = active_cmod;

    std::string obj_stack = active_object
                            + (active_subobject.length() > 0 ? ":" + active_subobject  : "")
                            + (active_cmod.length() > 0 ? ":" + active_cmod  : "");

    for (size_t i = 0; i < vertex_names.size(); i++){
        graph->add_edge(vertex_names[i], vertex_is_ssc[i], dest_var, dest_is_ssc, active_method, obj_stack, expression,
                        ui_name, nullptr);
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
                    try{
                        l.deref().copy(r.deref());
                        result.copy(r.deref());
                    }
                    catch(lk::error_t& e){
                        std:: cout << "could not deref " << e.what() << "\n";
                    }

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
            std::cout << "caught error at line: " << " for " << e.text << "\n";
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
            std::cout << "caught error at line: " << n5->line() << " for " << e.text << "\n";
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
//                return false;
            }

            if (n6->globalval)
            {
                std::cout << "error at line: " << n6->line();
                std::cout << " overriding previous non-global identifier with global-ness not allowed: " + n6->name + "\n";
                result.nullify();
//                return false;
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


std::string indent(std::string s, size_t n){
    std::string tabs;
    for (size_t i = 0; i < n; i++){
        tabs = "\t" + tabs;
    }
    s = tabs + s;
    size_t pos = s.find('\n');
    while(pos != std::string::npos && pos != s.length()-1){
        s.insert(pos+1,tabs);
        pos = s.find('\n', pos+1);
    }
    return s;
}

std::string remove_periods(std::string str){
    std::replace(str.begin(), str.end(), '.', '_');
    return str;
}

std::string format_as_symbol(std::string s){
    char cs[128] = {'\0'};
    size_t i = 0, j = 0;
    cs[i] = (char)std::toupper(s[j]);
    i++;
    j++;
    while (s[j])
    {
        if (s[j] == ' ' || s[j] == '-' || s[j] == '_'){
            cs[i] = (char)std::toupper(s[j+1]);
            j++;
        }
        else if (s[j] == '.'){
            j++;
            continue;
        }
        else{
            cs[i] = s[j];
        }
        i++;
        j++;
    }
    return cs;
}


bool translator::special_set(const lk_string &name, lk::vardata_t &val) {
    assert(false);
	return false;
}

bool translator::special_get(const lk_string &name, lk::vardata_t &val) {
    bool ok = false;
    VarTable* vt = &SAM_config_to_defaults[active_config];
    if ( VarValue *vv = vt->Get( name.ToStdString() ) )
        ok = vv->Write( val );

    if (!ok){
        errors().push_back("special_get error: could not find " + name + " in " + active_config);
    }
    return ok;
}


std::string translator::get_vv_type(const lk_string &name) {
    int type;
    VarTable* vt = &SAM_config_to_defaults[active_config];
    if ( VarValue *vv2 = vt->Get( name ) )
        type = vv2->Type();
    else
        type = 0;

    std::vector<std::string> vv_typestr = {"undefined", "float", "util::matrix_t<float>"
            , "util::matrix_t<float>", "std::string", "var_table", "binary"};

    return vv_typestr[type];
}

bool translator::translate(lk::node_t *root, lk::env_t *cur_env, std::string &result, unsigned int flags,
                           unsigned int &ctl_id,
                           std::string &output_prefix) {
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
            std::string statement;
            translate(n1->items[i], cur_env, statement, flags, ctl_id, output_prefix);
            result += statement;
            if(result.back() != ';' && result.back() != '}')
                result += ";";
            result += "\n";
        }

        return ok;
    }
    else if (iter_t *n2 = dynamic_cast<iter_t*>(root))
    {
        std::string init, test, adv, block;
        translate(n2->init, cur_env, init, flags, ctl_id, output_prefix);
        translate(n2->test, cur_env, test, flags, ctl_id, output_prefix);
        translate(n2->adv, cur_env, adv, flags, ctl_id, output_prefix);
        translate(n2->block, cur_env, block, flags, ctl_id, output_prefix);

        result += "for ( " + init + "; " + test + "; " + adv + " ){\n";
        result += indent(block) + "\n}";

        return true;
    }
    else if (cond_t *n3 = dynamic_cast<cond_t*>(root))
    {
        std::string test, ontrue, onfalse, type_name;
        translate(n3->test, cur_env, test, flags, ctl_id, type_name);

        // set up error message in case
        std::string msg = " std::runtime_error(\"";
        msg += ui_form_name + " conditional error: " + test + "\")";

        result += "if ( " + test + " ) {\n";

        translate(n3->on_true, cur_env, ontrue, flags, ctl_id, output_prefix);

        if (ontrue.find("throw") != std::string::npos){
            size_t pos = ontrue.find("throw");
            ontrue.insert(pos+5, msg);
        }

        result += indent(ontrue) + "\n}";

        if (n3->on_false){
            result += "\nelse ";

            translate(n3->on_false, cur_env, onfalse, flags, ctl_id, output_prefix);

            if (onfalse.find("throw") != std::string::npos){
                size_t pos = onfalse.find("throw");
                onfalse.insert(pos+5, msg);
            }

            if (onfalse.substr(0,2) != "if")
                result += "{\n" + indent(onfalse) + "}";
            else
                result += onfalse;
        }
        return true;
    }
    else if (expr_t *n4 = dynamic_cast<expr_t*>(root))
    {
        try
        {
            bool ok = true;
            std::string l, r;
            vardata_t vd_l;

            switch (n4->oper) {
                case expr_t::PLUS:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    result += l + " + " + r;
                    return ok;
                case expr_t::MINUS:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    result += l + " - " + r;
                    return ok;
                case expr_t::MULT:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    result += l + " * " + r;
                    return ok;
                case expr_t::DIV:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    result += l + " / " + r;
                    return ok;

                case expr_t::PLUSEQ:
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    result += l + " += " + r;
                    return false;
                case expr_t::MINUSEQ:
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    result += l + " -= " + r;
                    return false;
                case expr_t::MULTEQ:
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    result += l + " *= " + r;
                    return false;
                case expr_t::DIVEQ:
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    result += l + " /= " + r;
                    return false;

                case expr_t::MINUSAT:
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);

//                    if (l.deref().type() == vardata_t::HASH)
//                    {
//                        lk::varhash_t *hh = l.deref().hash();
//                        lk::varhash_t::iterator it = hh->find(r.deref().as_string());
//                        if (it != hh->end())
//                            hh->erase(it);
//                    }
//                    else if (l.deref().type() == vardata_t::VECTOR)
//                    {
//                        std::vector<lk::vardata_t> *vv = l.deref().vec();
//                        size_t idx = r.deref().as_unsigned();
//                        if (idx < vv->size())
//                            vv->erase(vv->begin() + idx);
//                    }
//                    else
//                    {
                    std::cout << "error at line " << n4->line();
                    std::cout << "-@ operator requires a hash or vector left hand side\n";
                    assert(false);

                    return false;
//                    }

//                    return true;

                case expr_t::INCR:
                    translate(n4->left, cur_env, l, flags | ENV_MUTABLE, ctl_id, output_prefix);
                    result += l + " += 1";
                    return ok;
                case expr_t::DECR:
                    translate(n4->left, cur_env, l, flags | ENV_MUTABLE, ctl_id, output_prefix);
                    result += l + " -= 1";
                    return ok;
                case expr_t::DEFINE:{
                    std::cout << "Define not implemented";
                    std::string def;
                    translate(n4->right, cur_env, def, flags, ctl_id, output_prefix);
                    result += def;
                    return ok;
                }
                case expr_t::ASSIGN:
                {
                    // evaluate expression and type before the lhs identifier
                    std::string type_name;
                    translate(n4->right, cur_env, r, flags, ctl_id, type_name);

                    // if on the LHS of the assignment we have a special variable i.e. ${xy}, use a
                    // hack to assign the value to the storage location
                    if (lk::iden_t *iden = dynamic_cast<lk::iden_t *>(n4->left)) {
                        if (iden->special) {
                            result += iden->name.ToStdString() + " = " + r;
                            return ok;
                        }
                    }

                    // otherwise evaluate the LHS in a mutable context, as normal.
                    translate(n4->left, cur_env, l, flags | ENV_MUTABLE, ctl_id, type_name);

                    // check if it's a vector or single-valued
                    std::string arr_name, indexer;
                    if (lk::expr_t* expr = dynamic_cast<lk::expr_t*>(n4->left)){
                        if (lk::iden_t* id_l = dynamic_cast<lk::iden_t*>(expr->left)){
                            arr_name = id_l->name.ToStdString();
                        }
                        if (lk::iden_t* id_r = dynamic_cast<lk::iden_t*>(expr->right)){
                            indexer = id_r->name.ToStdString();
                        }
                        else if (lk::constant_t* cnt_r = dynamic_cast<lk::constant_t*>(expr->right)){
                            indexer = std::to_string((size_t)cnt_r->value);
                        }
                        else{
                            size_t pos = l.find("[");
                            size_t pos2 = l.find("]");
                            assert( pos != std::string::npos && pos2 != std::string::npos);
                            indexer = l.substr(pos, pos2-pos);
                        }
                    }

                    // if the LHS variable is newly created in environment, save it and print its type
                    if (type_name != ""){
                        if (lk::iden_t* id = dynamic_cast<lk::iden_t*>(n4->left)){
                            vardata_t* v = cur_env->lookup(id->name, !(flags&ENV_MUTABLE));
                            if (type_name == "float")
                                v->assign(0.0);
                            else if (type_name == "std::string")
                                v->assign("");
                            else if (type_name == "util::matrix_t<float>")
                                v->empty_vector();
                            else if (type_name == "var_table")
                                v->empty_hash();
                            result += type_name + " ";
                        }
                        else if (lk::expr_t* ex = dynamic_cast<lk::expr_t*>(n4->left)){

                            if (indexer == "")
                                assert(!"not implemented");

                            vardata_t* v = cur_env->lookup(arr_name, !(flags&ENV_MUTABLE));

                            if (v->type() == vardata_t::NULLVAL){
                                // save the types
                                v->empty_vector();
                                if (type_name == "float")
                                    v->vec_append(0.0);
                                else
                                    v->vec_append("");

                                result += "std::vector<" + type_name + "> " + arr_name + ";\n";
                            }
                        }
                    }

                    // print
                    if (indexer.length() == 0)
                        result += l + " = " + r;
                    else{
                        result += arr_name + ".insert(" + arr_name + ".begin()+" + indexer + ", " + r+ ");" ;
                    }
                    return ok;
                }
                case expr_t::LOGIOR:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    result += l + " || " + r;
                    return ok;
                case expr_t::LOGIAND:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    result += l + " && " + r;
                    return ok;
                case expr_t::NOT:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    result += "!" + l;
                    return ok;
                case expr_t::EQ:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    result += l + " == " + r;
                    return ok;
                case expr_t::NE:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    result += l + " != " + r;
                    return ok;
                case expr_t::LT:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    result += l + " < " + r;
                    return ok;
                case expr_t::LE:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    result += l + " <= " + r;
                    return ok;
                case expr_t::GT:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    result += l + " > " + r;
                    return ok;
                case expr_t::GE:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    result += l + " >= " + r;
                    return ok;
                case expr_t::EXP:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);
                    result += "pow( " + l + ", " + r + " )";
                    return ok;
                case expr_t::NEG:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    result += "( " + l + " * -1 )";
                    return ok;
                case expr_t::WHEREAT:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    translate(n4->right, cur_env, r, flags, ctl_id, output_prefix);

                    interpret(n4->left, cur_env, vd_l, flags, ctl_id);

                    if (vd_l.deref().type() == vardata_t::HASH) {
//                        lk::varhash_t *hh = l.deref().hash();
//                        result.assign(hh->find(r.deref().as_string()) != hh->end() ? 1.0 : 0.0);
                        assert(false);
                        return ok;

                    } else if (vd_l.deref().type() == vardata_t::VECTOR) {
                        result += where_at_vector(l);
                        return ok;
                    } else if (vd_l.deref().type() == vardata_t::STRING) {
//                        lk_string::size_type pos = l.deref().str().find(r.deref().as_string());
//                        result.assign(pos != lk_string::npos ? (int)pos : -1.0);
                        assert(false);
                        return ok;
                    } else {
                        std::cout << "error at line: " << n4->line();
                        std::cout << "left hand side to find operator ?@ must be a hash, vector, or string\n";
                        return false;
                    }
                case expr_t::INDEX:
                {

                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);

                    translate(n4->right, cur_env, r, 0, ctl_id, output_prefix);

                    result += l + "[" + r + "]";
                    return ok;
                }
                case expr_t::HASH:
                    return ok;
//                    ok = ok && interpret(n4->left, cur_env, l, flags, ctl_id);
//                    bool anonymous = (l.type() == vardata_t::HASH);
//
//                    vardata_t &hash = l.deref();
//
//                    if ((flags&ENV_MUTABLE)
//                        && (hash.type() != vardata_t::HASH))
//                        hash.empty_hash();
//
//                    ok = ok && interpret(n4->right, cur_env, r, 0, ctl_id);
//                    vardata_t &val = r.deref();
//
//                    vardata_t *x = hash.lookup(val.as_string());
//                    if (x)
//                    {
//                        if (anonymous)
//                            result.copy(*x);
//                        else
//                            result.assign(x);
//                    }
//                    else if ((flags&ENV_MUTABLE))
//                    {
//                        hash.assign(val.as_string(), x = new vardata_t);
//                        result.assign(x);
//                    }
//                    else
//                        result.nullify();
                case expr_t::CALL:
                case expr_t::THISCALL: {
                    expr_t *cur_expr = n4;

                    if (iden_t *iden = dynamic_cast<iden_t *>(n4->left)) {
                        // query function table for identifier
                        if (lk::fcallinfo_t *fi = cur_env->lookup_func(iden->name)) {
                            result += iden->name + "( ";
                            list_t *argvals = dynamic_cast<list_t *>(n4->right);

                            // first determine number of arguments
                            size_t nargs = 0;
                            if (argvals) nargs = argvals->items.size();

                            bool ok = true;
                            if (nargs > 0) {
                                for (size_t iarg = 0; iarg < nargs; iarg++) {
                                    std::string argval;
                                    unsigned int c = 0;
                                    ok = ok &&
                                         translate(argvals->items[iarg], cur_env, argval, flags, c, output_prefix);
                                    result += argval;
                                    if (iarg != nargs - 1)
                                        result += ", ";
                                }
                            }
                            result += " )";
                            return ok;
                        }
                    }

                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);
                    if (l.find("plot") != std::string::npos){
                        std::cout << "translator warning: skipping 'plot' function " << l << " in " << ui_form_name << "\n";
                        return true;
                    }
                    result += l;

                    vardata_t vd_t;
                    interpret(n4->left, cur_env, vd_t, flags, ctl_id);
                    expr_t *define = dynamic_cast<expr_t *>(vd_t.deref().func());
                    if (!define) {
                        std::cout << "error at line: " << n4->line();
                        std::cout << "error in function call: malformed 'define'\n";
                        return false;
                    } else {
                        node_t *block = define->right;

                        // create new environment frame
                        env_t frame(cur_env);

                        // number of expected arguments
                        list_t *argnames = dynamic_cast<list_t *>(define->left);
                        size_t nargs_expected = argnames ? argnames->items.size() : 0;

                        // number of provided arguments
                        list_t *argvals = dynamic_cast<list_t *>(n4->right);
                        size_t nargs_given = argvals ? argvals->items.size() : 0;

                        if (n4->oper == expr_t::THISCALL)
                            nargs_given++;

                        if (nargs_given < nargs_expected) {
                            std::cout << "error at line: " << n4->line() << "nargs_given < nargs_expected\n";
                            return false;
                        }

                        // evaluate each argument and assign it into the new environment
                        expr_t *thisexpr = dynamic_cast<expr_t *>(cur_expr->left);
                        if (cur_expr->oper == expr_t::THISCALL
                            && thisexpr != 0
                            && thisexpr->left != 0) {
                            vardata_t thisobj;
                            unsigned int c = CTL_NONE;
                            if (!interpret(thisexpr->left, cur_env, thisobj, flags, c)) {
                                std::cout << "error at line: " << cur_expr->line();
                                std::cout << "failed to evaluate 'this' parameter 0 for THISCALL -> method\n";
                                return false;
                            }

                            if (thisobj.type() != vardata_t::REFERENCE) {
                                std::cout << "error at line: " << cur_expr->line();
                                std::cout << "'this' parameter did not evaluate to a reference\n";
                                return false;
                            }

                            std::string fx, type_name;
                            c = CTL_NONE;
                            translate(thisexpr->left, cur_env, fx, flags, c, type_name);

                            result += fx;

                            frame.assign("this", new vardata_t(thisobj));
                        }

                        vardata_t *__args = new vardata_t;
                        __args->empty_vector();

                        if (argvals) {
                            for (size_t argindex = 0;
                                 argindex < argvals->items.size();
                                 argindex++) {
                                vardata_t v;
                                iden_t *id = 0;

                                unsigned int c = CTL_NONE;
                                if (!interpret(argvals->items[argindex], cur_env, v, flags, c)) {
                                    std::cout << "error at line: " << argvals->items[argindex];
                                    std::cout << "failed to initialize function call argument\n";
                                    return false;
                                }
                                std::string str, type_name;
                                translate(argvals->items[argindex], cur_env, str, flags, c, type_name);

                                result += str + "\n";

                                if (argindex < argnames->items.size() &&
                                    ((id = dynamic_cast<iden_t *>(argnames->items[argindex])) != 0))
                                    frame.assign(id->name, new vardata_t(v));

                                __args->vec()->push_back(vardata_t(v));
                            }
                        }

                        frame.assign("__args", __args);

                        // now evaluate the function block in the new environment
                        std::string type_name, block_str;
                        translate(block, &frame, block_str, flags, ctl_id, type_name);
                        result += block_str;

                        // reset the sequence control
                        if (ctl_id != CTL_EXIT) ctl_id = CTL_NONE;

                        // environment frame will automatically be destroyed here
                        return true;
                    }
                }
                case expr_t::SIZEOF:
                    translate(n4->left, cur_env, l, flags, ctl_id, output_prefix);

                    interpret(n4->left, cur_env, vd_l, flags, ctl_id);
                    if (vd_l.deref().type() == vardata_t::VECTOR)
                    {
                        std::vector<lk::vardata_t>* tmp = vd_l.deref().vec();
                        std::string type;
                        if (tmp->size() > 0){
                            if ((*tmp)[0].type() == vardata_t::NUMBER){
                                result += "sizeof( " + l + " )/sizeof(" + l + "[0])";
                                return ok;
                            }
                            else{
                                std::cout << "sizeof string not implemented\n";
                                assert(false);
                                return false;
                            }

                        }
                        else{
                            // try as a matrix
                            result += l + ".size()";
                        }
                    }
                    else if (vd_l.deref().type() == vardata_t::STRING)
                    {
                        result += "sizeof( " + l + " )";
                        return ok;
                    }
                    else if (vd_l.deref().type() == vardata_t::HASH)
                    {
//                        int count = 0;
//
//                        varhash_t *h = l.deref().hash();
//                        for (varhash_t::iterator it = h->begin();
//                             it != h->end();
//                             ++it)
//                        {
//                            if ((*it).second->deref().type() != vardata_t::NULLVAL)
//                                count++;
//                        }
//                        result.assign(count);
                        std::cout << "sizeof hash not implemeneted\n";
                        assert(false);
                        return ok;
                    }
                    else
                    {
                        // guess
                        result += l + ".size()";
                    }
                    break;
                case expr_t::KEYSOF:
                    ok = ok && interpret(n4->left, cur_env, vd_l, flags, ctl_id);
                    if (vd_l.deref().type() == vardata_t::HASH)
                    {
                        varhash_t *h = vd_l.deref().hash();
                        std::cout <<"keysof not implemented\n";
//                        result.empty_vector();
//                        result.vec()->reserve(h->size());
//                        for (varhash_t::iterator it = h->begin();
//                             it != h->end();
//                             ++it)
//                        {
//                            if ((*it).second->deref().type() != vardata_t::NULLVAL)
//                                result.vec_append((*it).first);
//                        }
                        assert(false);

                        return true;
                    }
                    else
                    {
                        assert(false);

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
                    result += "{ ";
                    list_t *p = dynamic_cast<list_t*>(n4->left);
                    if (p)
                    {
                        for (size_t i = 0; i < p->items.size(); i++)
                        {
                            std::string v;
                            translate(p->items[i], cur_env, v, flags, ctl_id, output_prefix);
                            if (i != p->items.size()-1)
                                result += ", ";
                        }
                    }
                    result += " }";
                }
                    return ok && ctl_id == CTL_NONE;
                case expr_t::INITHASH:
                {
//                    vardata_t vd;
//                    vd.empty_hash();
                    // create table
                    result += "{";

                    std::string valtype;
                    list_t *p = dynamic_cast<list_t*>(n4->left);
                    if (p)
                    {
                        for (size_t i = 0; i < p->items.size(); i++)
                        {
                            expr_t *assign = dynamic_cast<expr_t*>(p->items[i]);
                            if (assign && assign->oper == expr_t::ASSIGN)
                            {
                                std::string vkey, vval, type;
                                translate(assign->left, cur_env, vkey, flags, ctl_id, type);
                                translate(assign->right, cur_env, vval, flags, ctl_id, type);

                                result += "\"" + vkey + "\": \"" + vval + "\"";
                                if (valtype != type){
                                    valtype = "mixed";
                                    std::cout << "inithash error " << ui_form_name << "types are mixed\n";
                                }

                                if (i != p->items.size())
                                    result += ", ";

//                                    lk_string key = vkey.as_string();
//                                    varhash_t *h = vd.hash();
//                                    varhash_t::iterator it = h->find(key);
//                                    if (it != h->end())
//                                        (*it).second->copy(vval.deref());
//                                    else
//                                        (*h)[key] = new vardata_t(vval.deref());
                            }
                        }
                        result += "}";
                    }
                    // save into env
                    return ok;
                }
                case expr_t::SWITCH:
                {
                    // create lambda switch
                    std::string lambda;

                    // get the switch variable as an argument
                    std::string switchval, case_block;
                    translate(n4->left, cur_env, switchval, flags, ctl_id, output_prefix);

                    lambda += "auto switch_" + switchval + " = [&]{\n";

                    list_t *p = dynamic_cast<list_t*>(n4->right);

                    // get return type
                    vardata_t vd;
                    interpret(p->items[0], cur_env, vd, flags, ctl_id);
                    if (vd.type() != vardata_t::NUMBER){
                        std::cout << "warning: switch types must be numeric but type of";
                        std::cout << switchval << " is " << vd.typestr() <<"\n";
                    }

                    lambda += "\tfloat switch_result;\n";
                    lambda += "\tswitch( " + switchval + " ){\n";
                    for (size_t i = 0; i < p->items.size(); i++){
                        lambda += "\t\tcase " + std::to_string(i) + ":\n";
                        translate(p->items[i], cur_env, case_block, flags, ctl_id, output_prefix);
                        lambda += "\t\t\tswitch_result = "+ case_block + ";\n";
                        lambda += "\t\t\tbreak;\n";
                        case_block.clear();
                    }
                    lambda += "\t\tdefault:\n\t\t\tthrow std::runtime_error(\"" + ui_form_name;
                    lambda += " switch undefined case for " + switchval + "\");\n";
                    lambda += "\t\t}\n\treturn switch_result;\n};\n";


                    aux_functions.insert({switchval, lambda});

                    // let code call to lambda fx
                    result += "switch_" + switchval + "()";
                    return ok;
                }
                default:
                    break;
            }
        }
        catch (lk::error_t &e) {
            std::cout << "caught error at line: " << n4->line() << " for " << e.text << "\n";
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
                        std::string s, type_name;
                        translate(n5->rexpr, cur_env, s, flags, ctl_id, type_name);

                        if (output_prefix == "vt"){
                            // if returning in a MIMO equation, likely logic-error exit
                            result += "throw;";
                        }
                        else if (output_prefix == "return"){
                            result += "return " + s + ";";
                        }
                        else{
                            result += output_prefix + " = ";
                            result += s + ";";
                        }
                    }
//                    ctl_id = CTL_RETURN;
                    return ok;
                case ctlstmt_t::EXIT:
//                    ctl_id = CTL_EXIT;
                    result += "exit();";
                    return true;
                    break;
                case ctlstmt_t::BREAK:
//                    ctl_id = CTL_BREAK;
                    result += "break;";
                    return true;
                    break;
                case ctlstmt_t::CONTINUE:
//                    ctl_id = CTL_CONTINUE;
                    result += "continue;";
                    return true;
                    break;
            }
        }
        catch (lk::error_t &e) {
            std::cout << "caught error at line: " << n5->line() << " for " << e.text << "\n";
            return false;
        }
    }
    else if (iden_t *n6 = dynamic_cast<iden_t*>(root))
    {
        std::string id;
        if (n6->special && !(flags&ENV_MUTABLE)){
            if (output_prefix == "")
                output_prefix = get_vv_type(n6->name);

            result += n6->name.ToStdString();
            return true;
        }

        vardata_t *x = cur_env->lookup(n6->name, !(flags&ENV_MUTABLE));

        if (x)
        {
            if (n6->constval)
            {
                std::cout << "error at line: " << n6->line();
                std::cout << " overriding previous non-const identifier with const-ness not allowed: " << n6->name << "\n";
                return false;
            }

            if (n6->globalval)
            {
                std::cout << "error at line: " << n6->line();
                std::cout << " overriding previous non-global identifier with global-ness not allowed: " << n6->name << "\n";
                return false;
            }

            size_t type = (size_t)x->type();
            assert(type < 8);
            if (type == 2){
                type = (size_t )x->deref().type();
            }

            std::vector<std::string> typestr = {"", "auto", "reference", "float", "std::string",
                                                "util::matrix_t<float>", "var_table", "function"};
            if (output_prefix == "")
                output_prefix =

                        typestr[type];
            result += n6->name;
            return true;
        }
        else if (!x && (flags&ENV_MUTABLE))
        {
            // check if the variable exists in the global frame
            // and it was originally created as a global variable
            x = cur_env->global()->lookup(n6->name, false);
            if (x && x->flagval(vardata_t::GLOBALVAL))
            {
                result += x->as_string();
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

            result += n6->name.ToStdString();

            return true;
        }
        else
        {
            std::cout << "error at line: " << n6->line();
            std::cout << " reference to unassigned variable: " + n6->name + "\n";
            return false;
        }
    }
    else if (constant_t *n7 = dynamic_cast<constant_t*>(root))
    {
        result += std::to_string(n7->value);
        output_prefix = "float";
        return true;
    }
    else if (literal_t *n8 = dynamic_cast<literal_t*>(root))
    {
        result += n8->value.ToStdString();
        output_prefix = "std::string";
        return true;
    }
    else if (0 != dynamic_cast<null_t*>(root))
    {
        std::cout << "last line error\n";
        return true;
    }

    return false;
}

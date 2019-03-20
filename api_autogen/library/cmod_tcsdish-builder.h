#ifndef _CMOD_TCSDISH_BUILDER_H_
#define _CMOD_TCSDISH_BUILDER_H_

#ifdef LK_USE_WXWIDGETS
#include <lk/env.h>
typedef lk::invoke_t invoke_t;
#else
typedef void invoke_t;
#endif

#include "vartab.h"


//
// Evaluates csp.ds.ncollectors for a Dish Solar Field module
// @param *vt: a var_table* that contains: n_ew, n_ns
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float Tcsdish_DishSolarField_csp.ds.ncollectors_eval(var_table* vt, invoke_t* cxt = 0)


#endif
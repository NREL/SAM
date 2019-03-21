#ifndef _CMOD_TCSDISH_BUILDER_H_
#define _CMOD_TCSDISH_BUILDER_H_

#include "vartab.h"


//
// Evaluates csp.ds.ncollectors for a Dish Solar Field module
// @param *vt: a var_table* that contains: n_ew, n_ns
// @returns single value or var_table
//
float Tcsdish_csp.ds.ncollectors_eval(var_table* vt);

#endif
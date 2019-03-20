#ifndef _CMOD_GENERIC_SYSTEM_BUILDER_H_
#define _CMOD_GENERIC_SYSTEM_BUILDER_H_

#ifdef LK_USE_WXWIDGETS
#include <lk/env.h>
typedef lk::invoke_t invoke_t;
#else
typedef void invoke_t;
#endif

#include "vartab.h"


//
// Evaluates conv_eff for a Generic System Plant module
// @param *vt: a var_table* that contains: heat_rate
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float GenericSystem_GenericSystemPlant_conv_eff_eval(var_table* vt, invoke_t* cxt = 0)


#endif
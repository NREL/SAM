#ifndef _CMOD_GENERIC_SYSTEM_BUILDER_H_
#define _CMOD_GENERIC_SYSTEM_BUILDER_H_

#include "vartab.h"


//
// Evaluates conv_eff for a Generic System Plant module
// @param *vt: a var_table* that contains: heat_rate
// @returns single value or var_table
//
float GenericSystem_conv_eff_eval(var_table* vt);

#endif
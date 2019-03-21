#ifndef _CMOD_SWH_BUILDER_H_
#define _CMOD_SWH_BUILDER_H_

#include "vartab.h"


//
// Evaluates system_capacity for a Solar Water Heating module
// @param *vt: a var_table* that contains: area_coll, ncoll, FRta, FRUL
// @returns single value or var_table
//
float Swh_system_capacity_eval(var_table* vt);

#endif
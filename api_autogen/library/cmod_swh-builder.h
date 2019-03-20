#ifndef _CMOD_SWH_BUILDER_H_
#define _CMOD_SWH_BUILDER_H_

#ifdef LK_USE_WXWIDGETS
#include <lk/env.h>
typedef lk::invoke_t invoke_t;
#else
typedef void invoke_t;
#endif

#include "vartab.h"


//
// Evaluates system_capacity for a Solar Water Heating module
// @param *vt: a var_table* that contains: area_coll, ncoll, FRta, FRUL
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float Swh_SolarWaterHeating_system_capacity_eval(var_table* vt, invoke_t* cxt = 0)


#endif
#ifndef _CMOD_TCSGENERIC_SOLAR_BUILDER_H_
#define _CMOD_TCSGENERIC_SOLAR_BUILDER_H_

#ifdef LK_USE_WXWIDGETS
#include <lk/env.h>
typedef lk::invoke_t invoke_t;
#else
typedef void invoke_t;
#endif

#include "vartab.h"


//
// Evaluates system_capacity for a Generic CSP Power Block module
// @param *vt: a var_table* that contains: csp.gss.pwrb.nameplate
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcsgenericSolar_GenericCSPPowerBlock_system_capacity_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates qsf_des for a Generic CSP Solar Field module
// @param *vt: a var_table* that contains: w_des, eta_des, solarm
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcsgenericSolar_GenericCSPSolarField_qsf_des_eval(var_table* vt, invoke_t* cxt = 0)


#endif
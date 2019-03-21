#ifndef _CMOD_TCSGENERIC_SOLAR_BUILDER_H_
#define _CMOD_TCSGENERIC_SOLAR_BUILDER_H_

#include "vartab.h"


//
// Evaluates system_capacity for a Generic CSP Power Block module
// @param *vt: a var_table* that contains: csp.gss.pwrb.nameplate
// @returns single value or var_table
//
float TcsgenericSolar_system_capacity_eval(var_table* vt);

//
// Evaluates qsf_des for a Generic CSP Solar Field module
// @param *vt: a var_table* that contains: w_des, eta_des, solarm
// @returns single value or var_table
//
float TcsgenericSolar_qsf_des_eval(var_table* vt);

#endif
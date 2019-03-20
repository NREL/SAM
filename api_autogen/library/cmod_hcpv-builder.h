#ifndef _CMOD_HCPV_BUILDER_H_
#define _CMOD_HCPV_BUILDER_H_

#ifdef LK_USE_WXWIDGETS
#include <lk/env.h>
typedef lk::invoke_t invoke_t;
#else
typedef void invoke_t;
#endif

#include "vartab.h"


//
// Evaluates array_num_inverters for a HCPV Array module
// @param *vt: a var_table* that contains: hcpv.array.nameplate, inv_snl_pdco
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float Hcpv_HCPVArray_array_num_inverters_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates hcpv.module.area for a HCPV Module module
// @param *vt: a var_table* that contains: module_concentration, module_cell_area, module_ncells
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float Hcpv_HCPVModule_hcpv.module.area_eval(var_table* vt, invoke_t* cxt = 0)


#endif
#ifndef _CMOD_HCPV_BUILDER_H_
#define _CMOD_HCPV_BUILDER_H_

#include "vartab.h"


//
// Evaluates array_num_inverters for a HCPV Array module
// @param *vt: a var_table* that contains: hcpv.array.nameplate, inv_snl_pdco
// @returns single value or var_table
//
float Hcpv_array_num_inverters_eval(var_table* vt);

//
// Evaluates hcpv.module.area for a HCPV Module module
// @param *vt: a var_table* that contains: module_concentration, module_cell_area, module_ncells
// @returns single value or var_table
//
float Hcpv_hcpv.module.area_eval(var_table* vt);

#endif
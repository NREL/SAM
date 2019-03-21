#ifndef _CMOD_LINEAR_FRESNEL_DSG_IPH_BUILDER_H_
#define _CMOD_LINEAR_FRESNEL_DSG_IPH_BUILDER_H_

#include "vartab.h"


//
// Evaluates csp.lf.sf.dp.loop_therm_eff for a LF DSG Solar Field module
// @param *vt: a var_table* that contains: csp.lf.geom1.rec_thermal_derate
// @returns single value or var_table
//
float LinearFresnelDsgIph_csp.lf.sf.dp.loop_therm_eff_eval(var_table* vt);

//
// Evaluates csp.lf.geom1.rec_thermal_derate for a Linear Fresnel Boiler Geometry module
// @param *vt: a var_table* that contains: csp.lf.geom1.heat_loss_at_design, I_bn_des, csp.lf.geom1.refl_aper_area, csp.lf.geom1.coll_length
// @returns single value or var_table
//
float LinearFresnelDsgIph_csp.lf.geom1.rec_thermal_derate_eval(var_table* vt);

#endif
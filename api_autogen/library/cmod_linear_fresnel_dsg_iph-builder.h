#ifndef _CMOD_LINEAR_FRESNEL_DSG_IPH_BUILDER_H_
#define _CMOD_LINEAR_FRESNEL_DSG_IPH_BUILDER_H_

#ifdef LK_USE_WXWIDGETS
#include <lk/env.h>
typedef lk::invoke_t invoke_t;
#else
typedef void invoke_t;
#endif

#include "vartab.h"


//
// Evaluates csp.lf.sf.dp.loop_therm_eff for a LF DSG Solar Field module
// @param *vt: a var_table* that contains: csp.lf.geom1.rec_thermal_derate
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float LinearFresnelDsgIph_LFDSGSolarField_csp.lf.sf.dp.loop_therm_eff_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates csp.lf.geom1.rec_thermal_derate for a Linear Fresnel Boiler Geometry module
// @param *vt: a var_table* that contains: csp.lf.geom1.heat_loss_at_design, I_bn_des, csp.lf.geom1.refl_aper_area, csp.lf.geom1.coll_length
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float LinearFresnelDsgIph_LinearFresnelBoilerGeometry_csp.lf.geom1.rec_thermal_derate_eval(var_table* vt, invoke_t* cxt = 0)


#endif
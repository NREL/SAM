#ifndef _CMOD_TCSLINEAR_FRESNEL_BUILDER_H_
#define _CMOD_TCSLINEAR_FRESNEL_BUILDER_H_

#ifdef LK_USE_WXWIDGETS
#include <lk/env.h>
typedef lk::invoke_t invoke_t;
#else
typedef void invoke_t;
#endif

#include "vartab.h"


//
// Evaluates csp.lf.geom1.avg_field_temp_dt_design for a Linear Fresnel Boiler Geometry module
// @param *vt: a var_table* that contains: T_cold_ref, T_hot, T_amb_des_sf
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcslinearFresnel_LinearFresnelBoilerGeometry_csp.lf.geom1.avg_field_temp_dt_design_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates csp.lf.geom2.rec_thermal_derate for a Linear Fresnel Superheater Geometry module
// @param *vt: a var_table* that contains: csp.lf.geom2.heat_loss_at_design, I_bn_des, csp.lf.geom2.refl_aper_area, csp.lf.geom2.coll_length
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcslinearFresnel_LinearFresnelSuperheaterGeometry_csp.lf.geom2.rec_thermal_derate_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates system_capacity for a PBNS Power Block module
// @param *vt: a var_table* that contains: nameplate
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcslinearFresnel_PBNSPowerBlock_system_capacity_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates csp.lf.sf.geom2_area_frac for a Linear Fresnel Solar Field module
// @param *vt: a var_table* that contains: csp.lf.sf.sh_geom_unique, nModSH, csp.lf.geom2.refl_aper_area, nModBoil, csp.lf.geom1.refl_aper_area
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcslinearFresnel_LinearFresnelSolarField_csp.lf.sf.geom2_area_frac_eval(var_table* vt, invoke_t* cxt = 0)


#endif
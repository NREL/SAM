#ifndef _CMOD_TCSMSLF_BUILDER_H_
#define _CMOD_TCSMSLF_BUILDER_H_

#ifdef LK_USE_WXWIDGETS
#include <lk/env.h>
typedef lk::invoke_t invoke_t;
#else
typedef void invoke_t;
#endif

#include "vartab.h"


//
// Function mslf_is_hx for a Molten Salt Linear Fresnel Storage module
// @param *vt: a var_table* that contains: is_hx
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcsMSLF_MoltenSaltLinearFresnelStorage_MoltenSaltLinearFresnelStorage_func(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates sm1_aperture for a Molten Salt Linear Fresnel Solar Field module
// @param *vt: a var_table* that contains: sf_q_design, I_bn_des, loop_eff
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcsMSLF_MoltenSaltLinearFresnelSolarField_sm1_aperture_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates system_capacity for a MSLF Power Cycle Common module
// @param *vt: a var_table* that contains: nameplate
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcsMSLF_MSLFPowerCycleCommon_system_capacity_eval(var_table* vt, invoke_t* cxt = 0)


//
// Function ret, is_hx for a  module
// @param *vt: a var_table* that contains: store_fl_props, HTF_code1, fl_props1, HTF_code2, fl_props2, Fluid, field_fl_props, obj, store_fluid
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
var_table TcsMSLF_MoltenSaltLinearFresnelStorage_MoltenSaltLinearFresnelStorage_func(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates hl_derate for a Molten Salt Linear Fresnel Collector and Receiver module
// @param *vt: a var_table* that contains: hl_des, I_bn_des, A_aperture, L_mod
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcsMSLF_MoltenSaltLinearFresnelCollectorAndReceiver_hl_derate_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates csp.mslf.control.tes_dens for a Molten Salt Linear Fresnel Storage module
// @param *vt: a var_table* that contains: csp.mslf.control.store_fluid, tes_temp, store_fl_props
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcsMSLF_MoltenSaltLinearFresnelStorage_csp.mslf.control.tes_dens_eval(var_table* vt, invoke_t* cxt = 0)


#endif
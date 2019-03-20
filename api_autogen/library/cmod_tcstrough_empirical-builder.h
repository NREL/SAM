#ifndef _CMOD_TCSTROUGH_EMPIRICAL_BUILDER_H_
#define _CMOD_TCSTROUGH_EMPIRICAL_BUILDER_H_

#ifdef LK_USE_WXWIDGETS
#include <lk/env.h>
typedef lk::invoke_t invoke_t;
#else
typedef void invoke_t;
#endif

#include "vartab.h"


//
// Evaluates PFSmax for a Empirical Trough Thermal Storage module
// @param *vt: a var_table* that contains: ui_tes_htf_type, ui_field_htf_type, ui_q_design, TurTesOutAdj, TurTesEffAdj, MaxGrOut
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcstroughEmpirical_EmpiricalTroughThermalStorage_PFSmax_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates system_capacity for a Empirical Trough Power Block module
// @param *vt: a var_table* that contains: ui_net_capacity
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcstroughEmpirical_EmpiricalTroughPowerBlock_system_capacity_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates ui_hce_opt_eff_1 for a Empirical Trough HCE module
// @param *vt: a var_table* that contains: calc_hce_col_factor, ui_hce_broken_glass_1, ui_hce_HCEdust, HCEBelShad_1, HCEEnvTrans_1, HCEabs_1, HCEmisc_1
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcstroughEmpirical_EmpiricalTroughHCE_ui_hce_opt_eff_1_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates calc_col_factor for a Empirical Trough SCA module
// @param *vt: a var_table* that contains: TrkTwstErr, GeoAcc, MirRef, MirCln, ConcFac
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcstroughEmpirical_EmpiricalTroughSCA_calc_col_factor_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates ui_piping_heat_loss for a Empirical Trough Solar Field module
// @param *vt: a var_table* that contains: SfPipeHl3, calc_field_htf_average_temp, SfPipeHl2, SfPipeHl1, SfPipeHl300
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcstroughEmpirical_EmpiricalTroughSolarField_ui_piping_heat_loss_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates HhtfPar for a Empirical Trough Parasitics module
// @param *vt: a var_table* that contains: HhtfParPF, ui_par_tes_const, ui_par_turb_out_gr
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcstroughEmpirical_EmpiricalTroughParasitics_HhtfPar_eval(var_table* vt, invoke_t* cxt = 0)


#endif
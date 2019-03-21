#ifndef _CMOD_TCSTROUGH_EMPIRICAL_BUILDER_H_
#define _CMOD_TCSTROUGH_EMPIRICAL_BUILDER_H_

#include "vartab.h"


//
// Evaluates PFSmax for a Empirical Trough Thermal Storage module
// @param *vt: a var_table* that contains: ui_tes_htf_type, ui_field_htf_type, ui_q_design, TurTesOutAdj, TurTesEffAdj, MaxGrOut
// @returns single value or var_table
//
float TcstroughEmpirical_PFSmax_eval(var_table* vt);

//
// Evaluates system_capacity for a Empirical Trough Power Block module
// @param *vt: a var_table* that contains: ui_net_capacity
// @returns single value or var_table
//
float TcstroughEmpirical_system_capacity_eval(var_table* vt);

//
// Evaluates ui_hce_opt_eff_1 for a Empirical Trough HCE module
// @param *vt: a var_table* that contains: calc_hce_col_factor, ui_hce_broken_glass_1, ui_hce_HCEdust, HCEBelShad_1, HCEEnvTrans_1, HCEabs_1, HCEmisc_1
// @returns single value or var_table
//
float TcstroughEmpirical_ui_hce_opt_eff_1_eval(var_table* vt);

//
// Evaluates calc_col_factor for a Empirical Trough SCA module
// @param *vt: a var_table* that contains: TrkTwstErr, GeoAcc, MirRef, MirCln, ConcFac
// @returns single value or var_table
//
float TcstroughEmpirical_calc_col_factor_eval(var_table* vt);

//
// Evaluates ui_piping_heat_loss for a Empirical Trough Solar Field module
// @param *vt: a var_table* that contains: SfPipeHl3, calc_field_htf_average_temp, SfPipeHl2, SfPipeHl1, SfPipeHl300
// @returns single value or var_table
//
float TcstroughEmpirical_ui_piping_heat_loss_eval(var_table* vt);

//
// Evaluates HhtfPar for a Empirical Trough Parasitics module
// @param *vt: a var_table* that contains: HhtfParPF, ui_par_tes_const, ui_par_turb_out_gr
// @returns single value or var_table
//
float TcstroughEmpirical_HhtfPar_eval(var_table* vt);

#endif
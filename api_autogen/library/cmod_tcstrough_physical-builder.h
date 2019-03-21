#ifndef _CMOD_TCSTROUGH_PHYSICAL_BUILDER_H_
#define _CMOD_TCSTROUGH_PHYSICAL_BUILDER_H_

#include "vartab.h"


//
// Evaluates system_capacity for a Physical Trough Power Block Common module
// @param *vt: a var_table* that contains: csp.dtr.pwrb.nameplate
// @returns single value or var_table
//
float TcstroughPhysical_system_capacity_eval(var_table* vt);

//
// Evaluates total_loop_conversion_efficiency for a Physical Trough Solar Field module
// @param *vt: a var_table* that contains: loop_optical_efficiency, cspdtr_loop_hce_heat_loss
// @returns single value or var_table
//
float TcstroughPhysical_total_loop_conversion_efficiency_eval(var_table* vt);

//
// Evaluates W_aperture, max_collector_width, A_aperture, TrackingError, GeomEffects, Rho_mirror_clean, Dirt_mirror, Error, Ave_Focal_Length, L_SCA, L_aperture, ColperSCA, Distance_SCA for a Physical Trough Collector Header module
// @param *vt: a var_table* that contains: csp_dtr_sca_w_profile_1, csp_dtr_sca_w_profile_2, csp_dtr_sca_w_profile_3, csp_dtr_sca_w_profile_4, arr_collectors_in_loop, csp_dtr_sca_aperture_1, csp_dtr_sca_aperture_2, csp_dtr_sca_aperture_3, csp_dtr_sca_aperture_4, csp_dtr_sca_tracking_error_1, csp_dtr_sca_tracking_error_2, csp_dtr_sca_tracking_error_3, csp_dtr_sca_tracking_error_4, csp_dtr_sca_geometry_effects_1, csp_dtr_sca_geometry_effects_2, csp_dtr_sca_geometry_effects_3, csp_dtr_sca_geometry_effects_4, csp_dtr_sca_clean_reflectivity_1, csp_dtr_sca_clean_reflectivity_2, csp_dtr_sca_clean_reflectivity_3, csp_dtr_sca_clean_reflectivity_4, csp_dtr_sca_mirror_dirt_1, csp_dtr_sca_mirror_dirt_2, csp_dtr_sca_mirror_dirt_3, csp_dtr_sca_mirror_dirt_4, csp_dtr_sca_general_error_1, csp_dtr_sca_general_error_2, csp_dtr_sca_general_error_3, csp_dtr_sca_general_error_4, csp_dtr_sca_ave_focal_len_1, csp_dtr_sca_ave_focal_len_2, csp_dtr_sca_ave_focal_len_3, csp_dtr_sca_ave_focal_len_4, csp_dtr_sca_length_1, csp_dtr_sca_length_2, csp_dtr_sca_length_3, csp_dtr_sca_length_4, csp_dtr_sca_ap_length_1, csp_dtr_sca_ap_length_2, csp_dtr_sca_ap_length_3, csp_dtr_sca_ap_length_4, csp_dtr_sca_ncol_per_sca_1, csp_dtr_sca_ncol_per_sca_2, csp_dtr_sca_ncol_per_sca_3, csp_dtr_sca_ncol_per_sca_4, csp_dtr_sca_piping_dist_1, csp_dtr_sca_piping_dist_2, csp_dtr_sca_piping_dist_3, csp_dtr_sca_piping_dist_4
// @returns single value or var_table
//
var_table TcstroughPhysical_W_aperture_MIMO_eval(var_table* vt);

//
// Evaluates W_pb_design, q_pb_design, q_max_aux for a Physical Trough Power Block Common module
// @param *vt: a var_table* that contains: P_ref, eta_ref
// @returns single value or var_table
//
var_table TcstroughPhysical_W_pb_design_MIMO_eval(var_table* vt);

//
// Function ret, is_hx for a  module
// @param *vt: a var_table* that contains: field_fl_props, obj, HTF_code1, fl_props1, HTF_code2, fl_props2, store_fluid, Fluid, store_fl_props
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
var_table TcstroughPhysical_PhysicalTroughSolarField_PhysicalTroughSolarField_func(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates collectors_in_field, arr_collectors_in_loop for a Physical Trough Collector Header module
// @param *vt: a var_table* that contains: SCAInfoArray, nColt
// @returns single value or var_table
//
var_table TcstroughPhysical_collectors_in_field_MIMO_eval(var_table* vt);

//
// Function is_hx for a Physical Trough Thermal Storage module
// @param *vt: a var_table* that contains: is_hx
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float TcstroughPhysical_PhysicalTroughSolarField_PhysicalTroughSolarField_func(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates csp.dtr.tes.hx_derate for a Physical Trough Thermal Storage module
// @param *vt: a var_table* that contains: is_hx, dt_hot, dt_cold, T_loop_out, T_loop_in_des
// @returns single value or var_table
//
float TcstroughPhysical_csp.dtr.tes.hx_derate_eval(var_table* vt);

#endif
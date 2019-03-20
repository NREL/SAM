#ifndef _CMOD_TROUGH_PHYSICAL_PROCESS_HEAT_BUILDER_H_
#define _CMOD_TROUGH_PHYSICAL_PROCESS_HEAT_BUILDER_H_

#ifdef LK_USE_WXWIDGETS
#include <lk/env.h>
typedef lk::invoke_t invoke_t;
#else
typedef void invoke_t;
#endif

#include "vartab.h"


//
// Evaluates W_aperture, max_collector_width, A_aperture, TrackingError, GeomEffects, Rho_mirror_clean, Dirt_mirror, Error, Ave_Focal_Length, L_SCA, L_aperture, ColperSCA, Distance_SCA for a Physical Trough Collector Header module
// @param *vt: a var_table* that contains: csp_dtr_sca_w_profile_1, csp_dtr_sca_w_profile_2, csp_dtr_sca_w_profile_3, csp_dtr_sca_w_profile_4, arr_collectors_in_loop, csp_dtr_sca_aperture_1, csp_dtr_sca_aperture_2, csp_dtr_sca_aperture_3, csp_dtr_sca_aperture_4, csp_dtr_sca_tracking_error_1, csp_dtr_sca_tracking_error_2, csp_dtr_sca_tracking_error_3, csp_dtr_sca_tracking_error_4, csp_dtr_sca_geometry_effects_1, csp_dtr_sca_geometry_effects_2, csp_dtr_sca_geometry_effects_3, csp_dtr_sca_geometry_effects_4, csp_dtr_sca_clean_reflectivity_1, csp_dtr_sca_clean_reflectivity_2, csp_dtr_sca_clean_reflectivity_3, csp_dtr_sca_clean_reflectivity_4, csp_dtr_sca_mirror_dirt_1, csp_dtr_sca_mirror_dirt_2, csp_dtr_sca_mirror_dirt_3, csp_dtr_sca_mirror_dirt_4, csp_dtr_sca_general_error_1, csp_dtr_sca_general_error_2, csp_dtr_sca_general_error_3, csp_dtr_sca_general_error_4, csp_dtr_sca_ave_focal_len_1, csp_dtr_sca_ave_focal_len_2, csp_dtr_sca_ave_focal_len_3, csp_dtr_sca_ave_focal_len_4, csp_dtr_sca_length_1, csp_dtr_sca_length_2, csp_dtr_sca_length_3, csp_dtr_sca_length_4, csp_dtr_sca_ap_length_1, csp_dtr_sca_ap_length_2, csp_dtr_sca_ap_length_3, csp_dtr_sca_ap_length_4, csp_dtr_sca_ncol_per_sca_1, csp_dtr_sca_ncol_per_sca_2, csp_dtr_sca_ncol_per_sca_3, csp_dtr_sca_ncol_per_sca_4, csp_dtr_sca_piping_dist_1, csp_dtr_sca_piping_dist_2, csp_dtr_sca_piping_dist_3, csp_dtr_sca_piping_dist_4
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
var_table TroughPhysicalProcessHeat_PhysicalTroughCollectorHeader_W_aperture_MIMO_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates collectors_in_field, arr_collectors_in_loop for a Physical Trough Collector Header module
// @param *vt: a var_table* that contains: SCAInfoArray, nColt
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
var_table TroughPhysicalProcessHeat_PhysicalTroughCollectorHeader_collectors_in_field_MIMO_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates D_cpnt for a Phys Trough Solar Field module
// @param *vt: a var_table* that contains: nSCA
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
util::matrix_t<ssc_number_t> TroughPhysicalProcessHeat_PhysTroughSolarField_D_cpnt_eval(var_table* vt, invoke_t* cxt = 0)


#endif
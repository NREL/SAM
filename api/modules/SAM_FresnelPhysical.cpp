#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_FresnelPhysical.h"

SAM_EXPORT int SAM_FresnelPhysical_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("fresnel_physical", data, verbosity, err);
}

SAM_EXPORT void SAM_FresnelPhysical_SystemControl_disp_inventory_incentive_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_inventory_incentive", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sim_type", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Weather_file_name_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "file_name", str);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SystemDesign_I_bn_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_bn_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SystemDesign_P_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_ref", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SystemDesign_T_loop_in_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_in_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SystemDesign_T_loop_out_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_out", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SystemDesign_eta_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_ref", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SystemDesign_gross_net_conversion_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gross_net_conversion_factor", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SystemDesign_solar_mult_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "solar_mult_in", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SystemDesign_solar_mult_or_Ap_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "solar_mult_or_Ap", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SystemDesign_total_Ap_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "total_Ap_in", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SystemDesign_tshours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tshours", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_FieldConfig_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "FieldConfig", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_Fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Fluid", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_HDR_rough_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HDR_rough", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_L_rnr_pb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_rnr_pb", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_Pipe_hl_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Pipe_hl_coef", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_SCA_drives_elec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SCA_drives_elec", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_T_amb_sf_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_sf_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_T_fp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_fp", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_T_startup_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_startup", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_V_hdr_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_max", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_V_hdr_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_min", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_V_wind_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_wind_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_eta_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pump", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_f_htfmax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_htfmax", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_f_htfmin_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_htfmin", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "field_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_land_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_mult", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_m_dot_htfmax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htfmax", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_m_dot_htfmin_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htfmin", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_mc_bal_cold_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_cold", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_mc_bal_hot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_hot", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_mc_bal_sca_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_sca", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_nMod_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nMod", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_p_start_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "p_start", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_rec_htf_vol_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_htf_vol", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_theta_dep_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_dep", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_theta_stow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_stow", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_use_abs_or_rel_mdot_limit_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "use_abs_or_rel_mdot_limit", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_washes_per_year_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "washes_per_year", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SolarField_water_per_wash_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "water_per_wash", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_A_aperture_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "A_aperture", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_AbsorberMaterial_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "AbsorberMaterial", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_AnnulusGas_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "AnnulusGas", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_ColAz_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ColAz", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_DP_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "DP_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_DP_nominal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "DP_nominal", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_D_abs_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "D_abs_in", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_D_abs_out_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "D_abs_out", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_D_glass_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "D_glass_in", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_D_glass_out_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "D_glass_out", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_D_plug_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "D_plug", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_Design_loss_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Design_loss", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_Dirt_mirror_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Dirt_mirror", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_Error_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Error", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_Flow_type_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Flow_type", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_GeomEffects_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "GeomEffects", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_GlazingIntactIn_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "GlazingIntactIn", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_HCE_FieldFrac_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCE_FieldFrac", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_HL_T_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HL_T_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_HL_w_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HL_w_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_IAM_L_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "IAM_L_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_IAM_T_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "IAM_T_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_L_crossover_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_crossover", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_L_mod_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_mod", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_L_mod_spacing_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_mod_spacing", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_OpticalTable_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "OpticalTable", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_P_a_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "P_a", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_Rough_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Rough", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_Shadowing_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Shadowing", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_Tau_envelope_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Tau_envelope", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_TrackingError_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TrackingError", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_alpha_abs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "alpha_abs", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_alpha_env_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "alpha_env", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_dirt_env_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dirt_env", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_epsilon_abs_1_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_abs_1", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_epsilon_abs_2_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_abs_2", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_epsilon_abs_3_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_abs_3", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_epsilon_abs_4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_abs_4", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_epsilon_glass_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "epsilon_glass", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_nRecVar_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nRecVar", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_opt_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_model", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_rec_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_model", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ColRec_reflectivity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "reflectivity", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_CT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_DP_SGS_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "DP_SGS", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_F_wc_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "F_wc", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_P_cond_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_min", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_P_cond_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_ratio", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ITD_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_T_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_approach", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_cycle_cutoff_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_cutoff_frac", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_cycle_max_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_max_frac", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_dT_cw_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_cw_ref", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_n_pl_inc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_pl_inc", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_pb_bd_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_bd_frac", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_pump_coef", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_pc_config_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pc_config", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_sby_frac", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_startup_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_frac", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_startup_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_time", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Powerblock_tech_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tech_type", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_UserDefinedPC_ud_f_W_dot_cool_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_f_W_dot_cool_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_UserDefinedPC_ud_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_UserDefinedPC_ud_is_sco2_regr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_is_sco2_regr", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_UserDefinedPC_ud_m_dot_water_cool_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_m_dot_water_cool_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_V_tes_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_tes_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_dt_cold_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_cold", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_dt_hot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_hot", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_h_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_min", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "init_hot_htf_percent", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_store_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "store_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_store_fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "store_fluid", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_pairs", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_tanks_in_parallel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tanks_in_parallel", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_tes_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pump_coef", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Storage_u_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Tou_can_cycle_use_standby_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "can_cycle_use_standby", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Tou_disp_reporting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_reporting", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Tou_disp_spec_bb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_bb", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Tou_disp_spec_presolve_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_presolve", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Tou_disp_spec_scaling_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_scaling", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Tou_disp_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_steps_per_hour", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Tou_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_factors_ts", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Tou_f_turb_tou_periods_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "f_turb_tou_periods", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Tou_is_timestep_load_fractions_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_timestep_load_fractions", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Tou_is_tod_pc_target_also_pc_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_tod_pc_target_also_pc_max", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Tou_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_multiplier_model", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Tou_q_rec_heattrace_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_rec_heattrace", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Tou_q_rec_standby_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_rec_standby", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Tou_timestep_load_fractions_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "timestep_load_fractions", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_aux_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "aux_array", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_bop_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "bop_array", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_disp_csu_cost_rel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_csu_cost_rel", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_disp_frequency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_frequency", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_disp_horizon_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_horizon", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_disp_max_iter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_max_iter", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_disp_mip_gap_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_mip_gap", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_disp_pen_ramping_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_pen_ramping", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_disp_rsu_cost_rel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_rsu_cost_rel", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_disp_time_weighting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_time_weighting", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_disp_timeout_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_timeout", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_is_dispatch_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_dispatch", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_fixed_par", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_rec_qf_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_qf_delay", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_rec_su_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_su_delay", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekday_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_SysControl_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekend_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialModel_csp_financial_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp_financial_model", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_ElectricityRates_en_electricity_rates_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_electricity_rates", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_TimeOfDeliveryFactors_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_TimeOfDeliveryFactors_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_TimeOfDeliveryFactors_dispatch_tod_factors_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_tod_factors", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialSolutionMode_ppa_price_input_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ppa_price_input", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialSolutionMode_ppa_soln_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_soln_mode", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_Revenue_mp_energy_market_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "mp_energy_market_revenue", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_bop_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_spec_cost", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_contingency_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "contingency_percent", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_epc_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_fixed", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_epc_cost_per_acre_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_per_acre", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_epc_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_per_watt", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_epc_cost_percent_direct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_percent_direct", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_fossil_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fossil_spec_cost", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_htf_system_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "htf_system_spec_cost", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_plm_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "plm_cost_fixed", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_plm_cost_per_acre_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "plm_cost_per_acre", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_plm_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "plm_cost_per_watt", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_plm_cost_percent_direct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "plm_cost_percent_direct", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_power_plant_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "power_plant_spec_cost", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_sales_tax_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_percent", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_rate", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_site_improvements_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "site_improvements_spec_cost", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_solar_field_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "solar_field_spec_cost", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_CapitalCosts_storage_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "storage_spec_cost", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate1", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate2", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate3", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate4", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate5", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months1", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months2", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months3", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months4", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months5", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent1", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent2", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent3", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent4", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent5", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate1", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate2", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate3", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate4", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate5", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_AdjustmentFactors_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_constant", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_AdjustmentFactors_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_periods", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_AdjustmentFactors_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_timeindex", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_AdjustmentFactors_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "adjust_periods", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysical_AdjustmentFactors_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "adjust_timeindex", arr, length);
	});
}

SAM_EXPORT double SAM_FresnelPhysical_SystemControl_disp_inventory_incentive_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_inventory_incentive", &result))
		make_access_error("SAM_FresnelPhysical", "disp_inventory_incentive");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_type", &result))
		make_access_error("SAM_FresnelPhysical", "sim_type");
	});
	return result;
}

SAM_EXPORT const char* SAM_FresnelPhysical_Weather_file_name_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "file_name");
	if (!result)
		make_access_error("SAM_FresnelPhysical", "file_name");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SystemDesign_I_bn_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_bn_des", &result))
		make_access_error("SAM_FresnelPhysical", "I_bn_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SystemDesign_P_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_ref", &result))
		make_access_error("SAM_FresnelPhysical", "P_ref");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SystemDesign_T_loop_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_in_des", &result))
		make_access_error("SAM_FresnelPhysical", "T_loop_in_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SystemDesign_T_loop_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_out", &result))
		make_access_error("SAM_FresnelPhysical", "T_loop_out");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SystemDesign_eta_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ref", &result))
		make_access_error("SAM_FresnelPhysical", "eta_ref");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SystemDesign_gross_net_conversion_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gross_net_conversion_factor", &result))
		make_access_error("SAM_FresnelPhysical", "gross_net_conversion_factor");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SystemDesign_solar_mult_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_mult_in", &result))
		make_access_error("SAM_FresnelPhysical", "solar_mult_in");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SystemDesign_solar_mult_or_Ap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_mult_or_Ap", &result))
		make_access_error("SAM_FresnelPhysical", "solar_mult_or_Ap");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SystemDesign_total_Ap_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_Ap_in", &result))
		make_access_error("SAM_FresnelPhysical", "total_Ap_in");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SystemDesign_tshours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours", &result))
		make_access_error("SAM_FresnelPhysical", "tshours");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_FieldConfig_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "FieldConfig", &result))
		make_access_error("SAM_FresnelPhysical", "FieldConfig");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_Fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Fluid", &result))
		make_access_error("SAM_FresnelPhysical", "Fluid");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_HDR_rough_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HDR_rough", &result))
		make_access_error("SAM_FresnelPhysical", "HDR_rough");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_L_rnr_pb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_rnr_pb", &result))
		make_access_error("SAM_FresnelPhysical", "L_rnr_pb");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_Pipe_hl_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Pipe_hl_coef", &result))
		make_access_error("SAM_FresnelPhysical", "Pipe_hl_coef");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_SCA_drives_elec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SCA_drives_elec", &result))
		make_access_error("SAM_FresnelPhysical", "SCA_drives_elec");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_T_amb_sf_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_sf_des", &result))
		make_access_error("SAM_FresnelPhysical", "T_amb_sf_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_T_fp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_fp", &result))
		make_access_error("SAM_FresnelPhysical", "T_fp");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_T_startup_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_startup", &result))
		make_access_error("SAM_FresnelPhysical", "T_startup");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_V_hdr_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_max", &result))
		make_access_error("SAM_FresnelPhysical", "V_hdr_max");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_V_hdr_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_min", &result))
		make_access_error("SAM_FresnelPhysical", "V_hdr_min");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_V_wind_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_wind_des", &result))
		make_access_error("SAM_FresnelPhysical", "V_wind_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_eta_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pump", &result))
		make_access_error("SAM_FresnelPhysical", "eta_pump");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_f_htfmax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_htfmax", &result))
		make_access_error("SAM_FresnelPhysical", "f_htfmax");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_f_htfmin_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_htfmin", &result))
		make_access_error("SAM_FresnelPhysical", "f_htfmin");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_SolarField_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "field_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "field_fl_props");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_land_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_mult", &result))
		make_access_error("SAM_FresnelPhysical", "land_mult");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_m_dot_htfmax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmax", &result))
		make_access_error("SAM_FresnelPhysical", "m_dot_htfmax");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_m_dot_htfmin_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmin", &result))
		make_access_error("SAM_FresnelPhysical", "m_dot_htfmin");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_mc_bal_cold_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_cold", &result))
		make_access_error("SAM_FresnelPhysical", "mc_bal_cold");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_mc_bal_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_hot", &result))
		make_access_error("SAM_FresnelPhysical", "mc_bal_hot");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_mc_bal_sca_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_sca", &result))
		make_access_error("SAM_FresnelPhysical", "mc_bal_sca");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_nMod_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nMod", &result))
		make_access_error("SAM_FresnelPhysical", "nMod");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_p_start_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "p_start", &result))
		make_access_error("SAM_FresnelPhysical", "p_start");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_rec_htf_vol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_htf_vol", &result))
		make_access_error("SAM_FresnelPhysical", "rec_htf_vol");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_theta_dep_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_dep", &result))
		make_access_error("SAM_FresnelPhysical", "theta_dep");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_theta_stow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_stow", &result))
		make_access_error("SAM_FresnelPhysical", "theta_stow");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_use_abs_or_rel_mdot_limit_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "use_abs_or_rel_mdot_limit", &result))
		make_access_error("SAM_FresnelPhysical", "use_abs_or_rel_mdot_limit");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_washes_per_year_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "washes_per_year", &result))
		make_access_error("SAM_FresnelPhysical", "washes_per_year");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SolarField_water_per_wash_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "water_per_wash", &result))
		make_access_error("SAM_FresnelPhysical", "water_per_wash");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_ColRec_A_aperture_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_aperture", &result))
		make_access_error("SAM_FresnelPhysical", "A_aperture");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_AbsorberMaterial_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "AbsorberMaterial", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "AbsorberMaterial");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_AnnulusGas_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "AnnulusGas", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "AnnulusGas");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_ColRec_ColAz_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ColAz", &result))
		make_access_error("SAM_FresnelPhysical", "ColAz");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_DP_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "DP_coefs", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "DP_coefs");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_ColRec_DP_nominal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "DP_nominal", &result))
		make_access_error("SAM_FresnelPhysical", "DP_nominal");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_D_abs_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "D_abs_in", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "D_abs_in");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_D_abs_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "D_abs_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "D_abs_out");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_D_glass_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "D_glass_in", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "D_glass_in");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_D_glass_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "D_glass_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "D_glass_out");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_D_plug_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "D_plug", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "D_plug");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_Design_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Design_loss", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "Design_loss");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_ColRec_Dirt_mirror_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Dirt_mirror", &result))
		make_access_error("SAM_FresnelPhysical", "Dirt_mirror");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_ColRec_Error_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Error", &result))
		make_access_error("SAM_FresnelPhysical", "Error");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_Flow_type_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Flow_type", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "Flow_type");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_ColRec_GeomEffects_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "GeomEffects", &result))
		make_access_error("SAM_FresnelPhysical", "GeomEffects");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_GlazingIntactIn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "GlazingIntactIn", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "GlazingIntactIn");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_HCE_FieldFrac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCE_FieldFrac", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "HCE_FieldFrac");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_HL_T_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HL_T_coefs", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "HL_T_coefs");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_HL_w_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HL_w_coefs", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "HL_w_coefs");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_IAM_L_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "IAM_L_coefs", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "IAM_L_coefs");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_IAM_T_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "IAM_T_coefs", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "IAM_T_coefs");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_ColRec_L_crossover_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_crossover", &result))
		make_access_error("SAM_FresnelPhysical", "L_crossover");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_ColRec_L_mod_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_mod", &result))
		make_access_error("SAM_FresnelPhysical", "L_mod");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_ColRec_L_mod_spacing_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_mod_spacing", &result))
		make_access_error("SAM_FresnelPhysical", "L_mod_spacing");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_OpticalTable_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "OpticalTable", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "OpticalTable");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_P_a_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_a", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "P_a");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_Rough_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Rough", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "Rough");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_Shadowing_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Shadowing", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "Shadowing");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_Tau_envelope_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Tau_envelope", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "Tau_envelope");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_ColRec_TrackingError_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TrackingError", &result))
		make_access_error("SAM_FresnelPhysical", "TrackingError");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_alpha_abs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "alpha_abs", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "alpha_abs");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_alpha_env_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "alpha_env", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "alpha_env");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_dirt_env_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dirt_env", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "dirt_env");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_epsilon_abs_1_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_abs_1", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "epsilon_abs_1");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_epsilon_abs_2_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_abs_2", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "epsilon_abs_2");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_epsilon_abs_3_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_abs_3", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "epsilon_abs_3");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_epsilon_abs_4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_abs_4", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "epsilon_abs_4");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_ColRec_epsilon_glass_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "epsilon_glass", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "epsilon_glass");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_ColRec_nRecVar_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nRecVar", &result))
		make_access_error("SAM_FresnelPhysical", "nRecVar");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_ColRec_opt_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_model", &result))
		make_access_error("SAM_FresnelPhysical", "opt_model");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_ColRec_rec_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_model", &result))
		make_access_error("SAM_FresnelPhysical", "rec_model");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_ColRec_reflectivity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "reflectivity", &result))
		make_access_error("SAM_FresnelPhysical", "reflectivity");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_CT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT", &result))
		make_access_error("SAM_FresnelPhysical", "CT");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_DP_SGS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "DP_SGS", &result))
		make_access_error("SAM_FresnelPhysical", "DP_SGS");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Powerblock_F_wc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "F_wc", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "F_wc");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_P_cond_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_min", &result))
		make_access_error("SAM_FresnelPhysical", "P_cond_min");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_P_cond_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_ratio", &result))
		make_access_error("SAM_FresnelPhysical", "P_cond_ratio");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_T_ITD_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ITD_des", &result))
		make_access_error("SAM_FresnelPhysical", "T_ITD_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_FresnelPhysical", "T_amb_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_T_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_approach", &result))
		make_access_error("SAM_FresnelPhysical", "T_approach");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_cycle_cutoff_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_cutoff_frac", &result))
		make_access_error("SAM_FresnelPhysical", "cycle_cutoff_frac");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_cycle_max_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_max_frac", &result))
		make_access_error("SAM_FresnelPhysical", "cycle_max_frac");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_dT_cw_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_cw_ref", &result))
		make_access_error("SAM_FresnelPhysical", "dT_cw_ref");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_n_pl_inc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_pl_inc", &result))
		make_access_error("SAM_FresnelPhysical", "n_pl_inc");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_pb_bd_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_bd_frac", &result))
		make_access_error("SAM_FresnelPhysical", "pb_bd_frac");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_pb_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_pump_coef", &result))
		make_access_error("SAM_FresnelPhysical", "pb_pump_coef");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_pc_config_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_config", &result))
		make_access_error("SAM_FresnelPhysical", "pc_config");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_q_sby_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_sby_frac", &result))
		make_access_error("SAM_FresnelPhysical", "q_sby_frac");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_startup_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_frac", &result))
		make_access_error("SAM_FresnelPhysical", "startup_frac");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_startup_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_time", &result))
		make_access_error("SAM_FresnelPhysical", "startup_time");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Powerblock_tech_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tech_type", &result))
		make_access_error("SAM_FresnelPhysical", "tech_type");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_UserDefinedPC_ud_f_W_dot_cool_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_f_W_dot_cool_des", &result))
		make_access_error("SAM_FresnelPhysical", "ud_f_W_dot_cool_des");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_UserDefinedPC_ud_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "ud_ind_od");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_UserDefinedPC_ud_is_sco2_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_is_sco2_regr", &result))
		make_access_error("SAM_FresnelPhysical", "ud_is_sco2_regr");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_UserDefinedPC_ud_m_dot_water_cool_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_m_dot_water_cool_des", &result))
		make_access_error("SAM_FresnelPhysical", "ud_m_dot_water_cool_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Storage_V_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tes_des", &result))
		make_access_error("SAM_FresnelPhysical", "V_tes_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Storage_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_Thtr", &result))
		make_access_error("SAM_FresnelPhysical", "cold_tank_Thtr");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Storage_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_max_heat", &result))
		make_access_error("SAM_FresnelPhysical", "cold_tank_max_heat");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Storage_dt_cold_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_cold", &result))
		make_access_error("SAM_FresnelPhysical", "dt_cold");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Storage_dt_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_hot", &result))
		make_access_error("SAM_FresnelPhysical", "dt_hot");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Storage_h_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank", &result))
		make_access_error("SAM_FresnelPhysical", "h_tank");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Storage_h_tank_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_min", &result))
		make_access_error("SAM_FresnelPhysical", "h_tank_min");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Storage_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_Thtr", &result))
		make_access_error("SAM_FresnelPhysical", "hot_tank_Thtr");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Storage_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_max_heat", &result))
		make_access_error("SAM_FresnelPhysical", "hot_tank_max_heat");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Storage_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "init_hot_htf_percent", &result))
		make_access_error("SAM_FresnelPhysical", "init_hot_htf_percent");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Storage_store_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "store_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "store_fl_props");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Storage_store_fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "store_fluid", &result))
		make_access_error("SAM_FresnelPhysical", "store_fluid");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Storage_tank_pairs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_pairs", &result))
		make_access_error("SAM_FresnelPhysical", "tank_pairs");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Storage_tanks_in_parallel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tanks_in_parallel", &result))
		make_access_error("SAM_FresnelPhysical", "tanks_in_parallel");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Storage_tes_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pump_coef", &result))
		make_access_error("SAM_FresnelPhysical", "tes_pump_coef");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Storage_u_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_tank", &result))
		make_access_error("SAM_FresnelPhysical", "u_tank");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Tou_can_cycle_use_standby_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "can_cycle_use_standby", &result))
		make_access_error("SAM_FresnelPhysical", "can_cycle_use_standby");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Tou_disp_reporting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_reporting", &result))
		make_access_error("SAM_FresnelPhysical", "disp_reporting");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Tou_disp_spec_bb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_bb", &result))
		make_access_error("SAM_FresnelPhysical", "disp_spec_bb");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Tou_disp_spec_presolve_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_presolve", &result))
		make_access_error("SAM_FresnelPhysical", "disp_spec_presolve");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Tou_disp_spec_scaling_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_scaling", &result))
		make_access_error("SAM_FresnelPhysical", "disp_spec_scaling");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Tou_disp_steps_per_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_steps_per_hour", &result))
		make_access_error("SAM_FresnelPhysical", "disp_steps_per_hour");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Tou_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_factors_ts", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "dispatch_factors_ts");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Tou_f_turb_tou_periods_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "f_turb_tou_periods", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "f_turb_tou_periods");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Tou_is_timestep_load_fractions_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_timestep_load_fractions", &result))
		make_access_error("SAM_FresnelPhysical", "is_timestep_load_fractions");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Tou_is_tod_pc_target_also_pc_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_tod_pc_target_also_pc_max", &result))
		make_access_error("SAM_FresnelPhysical", "is_tod_pc_target_also_pc_max");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Tou_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_multiplier_model", &result))
		make_access_error("SAM_FresnelPhysical", "ppa_multiplier_model");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Tou_q_rec_heattrace_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_rec_heattrace", &result))
		make_access_error("SAM_FresnelPhysical", "q_rec_heattrace");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Tou_q_rec_standby_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_rec_standby", &result))
		make_access_error("SAM_FresnelPhysical", "q_rec_standby");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Tou_timestep_load_fractions_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "timestep_load_fractions", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "timestep_load_fractions");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_SysControl_aux_array_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "aux_array", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "aux_array");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_SysControl_bop_array_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "bop_array", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "bop_array");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SysControl_disp_csu_cost_rel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_csu_cost_rel", &result))
		make_access_error("SAM_FresnelPhysical", "disp_csu_cost_rel");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SysControl_disp_frequency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_frequency", &result))
		make_access_error("SAM_FresnelPhysical", "disp_frequency");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SysControl_disp_horizon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_horizon", &result))
		make_access_error("SAM_FresnelPhysical", "disp_horizon");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SysControl_disp_max_iter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_max_iter", &result))
		make_access_error("SAM_FresnelPhysical", "disp_max_iter");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SysControl_disp_mip_gap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_mip_gap", &result))
		make_access_error("SAM_FresnelPhysical", "disp_mip_gap");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SysControl_disp_pen_ramping_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_pen_ramping", &result))
		make_access_error("SAM_FresnelPhysical", "disp_pen_ramping");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SysControl_disp_rsu_cost_rel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_rsu_cost_rel", &result))
		make_access_error("SAM_FresnelPhysical", "disp_rsu_cost_rel");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SysControl_disp_time_weighting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_time_weighting", &result))
		make_access_error("SAM_FresnelPhysical", "disp_time_weighting");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SysControl_disp_timeout_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_timeout", &result))
		make_access_error("SAM_FresnelPhysical", "disp_timeout");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SysControl_is_dispatch_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_dispatch", &result))
		make_access_error("SAM_FresnelPhysical", "is_dispatch");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SysControl_pb_fixed_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_fixed_par", &result))
		make_access_error("SAM_FresnelPhysical", "pb_fixed_par");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SysControl_rec_qf_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_qf_delay", &result))
		make_access_error("SAM_FresnelPhysical", "rec_qf_delay");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_SysControl_rec_su_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_su_delay", &result))
		make_access_error("SAM_FresnelPhysical", "rec_su_delay");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_SysControl_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekday_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "weekday_schedule");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_SysControl_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekend_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "weekend_schedule");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialModel_csp_financial_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp_financial_model", &result))
		make_access_error("SAM_FresnelPhysical", "csp_financial_model");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_ElectricityRates_en_electricity_rates_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_electricity_rates", &result))
		make_access_error("SAM_FresnelPhysical", "en_electricity_rates");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_TimeOfDeliveryFactors_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "dispatch_sched_weekday");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_TimeOfDeliveryFactors_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "dispatch_sched_weekend");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_TimeOfDeliveryFactors_dispatch_tod_factors_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_tod_factors", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "dispatch_tod_factors");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_FinancialSolutionMode_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ppa_price_input", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "ppa_price_input");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialSolutionMode_ppa_soln_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_soln_mode", &result))
		make_access_error("SAM_FresnelPhysical", "ppa_soln_mode");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Revenue_mp_energy_market_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mp_energy_market_revenue", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "mp_energy_market_revenue");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_bop_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_spec_cost", &result))
		make_access_error("SAM_FresnelPhysical", "bop_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_contingency_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_percent", &result))
		make_access_error("SAM_FresnelPhysical", "contingency_percent");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_epc_cost_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_fixed", &result))
		make_access_error("SAM_FresnelPhysical", "epc_cost_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_epc_cost_per_acre_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_per_acre", &result))
		make_access_error("SAM_FresnelPhysical", "epc_cost_per_acre");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_epc_cost_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_per_watt", &result))
		make_access_error("SAM_FresnelPhysical", "epc_cost_per_watt");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_epc_cost_percent_direct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_percent_direct", &result))
		make_access_error("SAM_FresnelPhysical", "epc_cost_percent_direct");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_fossil_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fossil_spec_cost", &result))
		make_access_error("SAM_FresnelPhysical", "fossil_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_htf_system_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "htf_system_spec_cost", &result))
		make_access_error("SAM_FresnelPhysical", "htf_system_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_plm_cost_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plm_cost_fixed", &result))
		make_access_error("SAM_FresnelPhysical", "plm_cost_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_plm_cost_per_acre_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plm_cost_per_acre", &result))
		make_access_error("SAM_FresnelPhysical", "plm_cost_per_acre");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_plm_cost_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plm_cost_per_watt", &result))
		make_access_error("SAM_FresnelPhysical", "plm_cost_per_watt");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_plm_cost_percent_direct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plm_cost_percent_direct", &result))
		make_access_error("SAM_FresnelPhysical", "plm_cost_percent_direct");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_power_plant_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "power_plant_spec_cost", &result))
		make_access_error("SAM_FresnelPhysical", "power_plant_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_sales_tax_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_percent", &result))
		make_access_error("SAM_FresnelPhysical", "sales_tax_percent");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_sales_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_rate", &result))
		make_access_error("SAM_FresnelPhysical", "sales_tax_rate");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_site_improvements_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "site_improvements_spec_cost", &result))
		make_access_error("SAM_FresnelPhysical", "site_improvements_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_solar_field_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_field_spec_cost", &result))
		make_access_error("SAM_FresnelPhysical", "solar_field_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_CapitalCosts_storage_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "storage_spec_cost", &result))
		make_access_error("SAM_FresnelPhysical", "storage_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate1", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_interest_rate1");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate2", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_interest_rate2");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate3", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_interest_rate3");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate4", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_interest_rate4");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate5", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_interest_rate5");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months1", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_months1");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months2", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_months2");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months3", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_months3");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months4", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_months4");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months5", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_months5");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent1", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_percent1");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent2", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_percent2");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent3", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_percent3");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent4", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_percent4");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent5", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_percent5");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate1", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_upfront_rate1");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate2", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_upfront_rate2");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate3", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_upfront_rate3");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate4", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_upfront_rate4");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate5", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_upfront_rate5");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_AdjustmentFactors_adjust_constant_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_constant", &result))
		make_access_error("SAM_FresnelPhysical", "adjust_constant");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_AdjustmentFactors_adjust_en_periods_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_periods", &result))
		make_access_error("SAM_FresnelPhysical", "adjust_en_periods");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_AdjustmentFactors_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_timeindex", &result))
		make_access_error("SAM_FresnelPhysical", "adjust_en_timeindex");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_AdjustmentFactors_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "adjust_periods", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "adjust_periods");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_AdjustmentFactors_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "adjust_timeindex", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "adjust_timeindex");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_A_field_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_field", &result))
		make_access_error("SAM_FresnelPhysical", "A_field");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_A_loop_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_loop", &result))
		make_access_error("SAM_FresnelPhysical", "A_loop");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_DP_pressure_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "DP_pressure_loss", &result))
		make_access_error("SAM_FresnelPhysical", "DP_pressure_loss");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_EqOpteff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EqOpteff", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "EqOpteff");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_P_cooling_tower_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cooling_tower_tot", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "P_cooling_tower_tot");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_P_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cycle", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "P_cycle");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_P_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_fixed", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "P_fixed");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_P_out_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_out_net", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "P_out_net");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_P_plant_balance_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_plant_balance_tot", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "P_plant_balance_tot");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_Q_field_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_field_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "Q_field_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_Q_loop_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_loop_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "Q_loop_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_Q_loss_hdr_rnr_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_loss_hdr_rnr_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "Q_loss_hdr_rnr_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_Q_loss_receiver_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_loss_receiver_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "Q_loss_receiver_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_Q_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_tes_des", &result))
		make_access_error("SAM_FresnelPhysical", "Q_tes_des");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_SCAs_def_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "SCAs_def", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "SCAs_def");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_T_field_cold_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_field_cold_in", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "T_field_cold_in");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_T_field_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_field_hot_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "T_field_hot_out");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_T_field_out_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_field_out_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "T_field_out_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_T_loop_out_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_out_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "T_loop_out_des_SS");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_T_pc_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pc_in", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "T_pc_in");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_T_pc_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pc_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "T_pc_out");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_T_rec_cold_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_cold_in", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "T_rec_cold_in");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_T_rec_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_hot_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "T_rec_hot_out");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_cold", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "T_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_hot", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "T_tes_hot");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_V_hdr_max_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_max_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "V_hdr_max_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_V_hdr_min_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_min_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "V_hdr_min_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_W_dot_bop_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_bop_design", &result))
		make_access_error("SAM_FresnelPhysical", "W_dot_bop_design");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_W_dot_field_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_field_pump", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "W_dot_field_pump");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_W_dot_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_fixed", &result))
		make_access_error("SAM_FresnelPhysical", "W_dot_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_W_dot_pump_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_pump_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "W_dot_pump_des_SS");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_W_dot_sca_track_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_sca_track", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "W_dot_sca_track");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_annual_W_cycle_gross_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_W_cycle_gross", &result))
		make_access_error("SAM_FresnelPhysical", "annual_W_cycle_gross");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_FresnelPhysical", "annual_energy");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_annual_field_freeze_protection_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_field_freeze_protection", &result))
		make_access_error("SAM_FresnelPhysical", "annual_field_freeze_protection");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_annual_tes_freeze_protection_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_tes_freeze_protection", &result))
		make_access_error("SAM_FresnelPhysical", "annual_tes_freeze_protection");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_annual_thermal_consumption_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_thermal_consumption", &result))
		make_access_error("SAM_FresnelPhysical", "annual_thermal_consumption");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_annual_total_water_use_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_total_water_use", &result))
		make_access_error("SAM_FresnelPhysical", "annual_total_water_use");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_aux_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_design", &result))
		make_access_error("SAM_FresnelPhysical", "aux_design");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_avg_dt_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "avg_dt_des", &result))
		make_access_error("SAM_FresnelPhysical", "avg_dt_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_avg_suboptimal_rel_mip_gap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "avg_suboptimal_rel_mip_gap", &result))
		make_access_error("SAM_FresnelPhysical", "avg_suboptimal_rel_mip_gap");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "beam");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_bop_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_cost", &result))
		make_access_error("SAM_FresnelPhysical", "bop_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_FresnelPhysical", "capacity_factor");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_interest1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest1", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_interest1");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_interest2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest2", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_interest2");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_interest3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest3", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_interest3");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_interest4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest4", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_interest4");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_interest5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest5", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_interest5");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_interest_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_total", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_interest_total");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_percent_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent_total", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_percent_total");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_principal1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal1", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_principal1");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_principal2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal2", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_principal2");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_principal3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal3", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_principal3");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_principal4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal4", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_principal4");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_principal5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal5", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_principal5");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_principal_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal_total", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_principal_total");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_total1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total1", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_total1");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_total2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total2", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_total2");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_total3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total3", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_total3");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_total4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total4", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_total4");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_const_per_total5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total5", &result))
		make_access_error("SAM_FresnelPhysical", "const_per_total5");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_financing_cost", &result))
		make_access_error("SAM_FresnelPhysical", "construction_financing_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_contingency_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_cost", &result))
		make_access_error("SAM_FresnelPhysical", "contingency_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_conversion_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conversion_factor", &result))
		make_access_error("SAM_FresnelPhysical", "conversion_factor");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_cp_battery_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cp_battery_nameplate", &result))
		make_access_error("SAM_FresnelPhysical", "cp_battery_nameplate");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_cp_system_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cp_system_nameplate", &result))
		make_access_error("SAM_FresnelPhysical", "cp_system_nameplate");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_cycle_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cycle_htf_pump_power", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "cycle_htf_pump_power");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_dP_field_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dP_field_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "dP_field_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_d_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_tank", &result))
		make_access_error("SAM_FresnelPhysical", "d_tank");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_defocus_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "defocus", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "defocus");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_deltaP_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "deltaP_field", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "deltaP_field");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_obj_relax_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_obj_relax", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_obj_relax");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_objective_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_objective", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_objective");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_pceff_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_pceff_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_pceff_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_presolve_nconstr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_presolve_nconstr", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_presolve_nconstr");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_presolve_nvar_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_presolve_nvar", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_presolve_nvar");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_qpbsu_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qpbsu_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_qpbsu_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_qsf_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsf_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_qsf_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_qsfprod_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsfprod_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_qsfprod_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_qsfsu_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsfsu_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_qsfsu_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_rel_mip_gap_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_rel_mip_gap", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_rel_mip_gap");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_rev_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_rev_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_rev_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_solve_iter_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_iter", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_solve_iter");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_solve_state_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_state", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_solve_state");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_solve_time_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_time", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_solve_time");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_subopt_flag_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_subopt_flag", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_subopt_flag");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_tes_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_tes_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_tes_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_thermeff_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_thermeff_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_thermeff_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_disp_wpb_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_wpb_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "disp_wpb_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_ch_tes", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "e_ch_tes");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_e_dot_field_int_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_dot_field_int_energy", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "e_dot_field_int_energy");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_eff_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eff_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "eff_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_eff_loop_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eff_loop_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "eff_loop_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_epc_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_total_cost", &result))
		make_access_error("SAM_FresnelPhysical", "epc_total_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_eta_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "eta");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_eta_optical_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_optical_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "eta_optical_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_f_htfmax_actual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_htfmax_actual", &result))
		make_access_error("SAM_FresnelPhysical", "f_htfmax_actual");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_f_htfmin_actual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_htfmin_actual", &result))
		make_access_error("SAM_FresnelPhysical", "f_htfmin_actual");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_field_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_area", &result))
		make_access_error("SAM_FresnelPhysical", "field_area");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_field_htf_max_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_htf_max_temp", &result))
		make_access_error("SAM_FresnelPhysical", "field_htf_max_temp");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_field_htf_min_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_htf_min_temp", &result))
		make_access_error("SAM_FresnelPhysical", "field_htf_min_temp");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_fossil_backup_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fossil_backup_cost", &result))
		make_access_error("SAM_FresnelPhysical", "fossil_backup_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "gen");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_hl_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hl_des", &result))
		make_access_error("SAM_FresnelPhysical", "hl_des");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_hour_day_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour_day", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "hour_day");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_htf_system_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "htf_system_cost", &result))
		make_access_error("SAM_FresnelPhysical", "htf_system_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_installed_per_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "installed_per_capacity", &result))
		make_access_error("SAM_FresnelPhysical", "installed_per_capacity");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_is_pc_sb_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_sb_allowed", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "is_pc_sb_allowed");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_is_pc_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_su_allowed", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "is_pc_su_allowed");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_is_rec_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_rec_su_allowed", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "is_rec_su_allowed");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_FresnelPhysical", "kwh_per_kw");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_loop_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loop_eff", &result))
		make_access_error("SAM_FresnelPhysical", "loop_eff");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_loop_opt_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loop_opt_eff", &result))
		make_access_error("SAM_FresnelPhysical", "loop_opt_eff");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_loop_therm_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loop_therm_eff", &result))
		make_access_error("SAM_FresnelPhysical", "loop_therm_eff");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_balance", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "m_dot_balance");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_m_dot_cold_tank_to_hot_tank_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_cold_tank_to_hot_tank", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "m_dot_cold_tank_to_hot_tank");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_m_dot_cr_to_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_cr_to_tes_hot", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "m_dot_cr_to_tes_hot");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_m_dot_cycle_to_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_cycle_to_field", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "m_dot_cycle_to_field");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_m_dot_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "m_dot_des_SS");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_m_dot_field_delivered_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_field_delivered", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "m_dot_field_delivered");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_m_dot_field_recirc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_field_recirc", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "m_dot_field_recirc");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_m_dot_field_to_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_field_to_cycle", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "m_dot_field_to_cycle");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_m_dot_htfmax_actual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmax_actual", &result))
		make_access_error("SAM_FresnelPhysical", "m_dot_htfmax_actual");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_m_dot_htfmin_actual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmin_actual", &result))
		make_access_error("SAM_FresnelPhysical", "m_dot_htfmin_actual");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_m_dot_loop_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_loop", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "m_dot_loop");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_m_dot_loop_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_loop_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "m_dot_loop_des_SS");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_m_dot_pc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pc", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "m_dot_pc");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_m_dot_pc_to_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pc_to_tes_cold", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "m_dot_pc_to_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_m_dot_tes_cold_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_cold_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "m_dot_tes_cold_out");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_m_dot_tes_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_hot_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "m_dot_tes_hot_out");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_m_dot_water_pc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_water_pc", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "m_dot_water_pc");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_mass_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_cold", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "mass_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_mass_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_hot", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "mass_tes_hot");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_mdot_cycle_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mdot_cycle_des", &result))
		make_access_error("SAM_FresnelPhysical", "mdot_cycle_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_mdot_field_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mdot_field_des", &result))
		make_access_error("SAM_FresnelPhysical", "mdot_field_des");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "month");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "monthly_energy");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_nLoops_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nLoops", &result))
		make_access_error("SAM_FresnelPhysical", "nLoops");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "n_op_modes", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "n_op_modes");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nameplate", &result))
		make_access_error("SAM_FresnelPhysical", "nameplate");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_1", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "op_mode_1");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_2", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "op_mode_2");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_3", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "op_mode_3");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_operating_modes_a_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_a", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "operating_modes_a");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_operating_modes_b_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_b", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "operating_modes_b");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_operating_modes_c_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_c", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "operating_modes_c");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_opt_derate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_derate", &result))
		make_access_error("SAM_FresnelPhysical", "opt_derate");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_opt_normal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_normal", &result))
		make_access_error("SAM_FresnelPhysical", "opt_normal");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_pipe_tes_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_P_dsn", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "pipe_tes_P_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_pipe_tes_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_T_dsn", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "pipe_tes_T_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_pipe_tes_diams_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_diams", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "pipe_tes_diams");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_pipe_tes_lengths_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_lengths", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "pipe_tes_lengths");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_pipe_tes_mdot_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_mdot_dsn", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "pipe_tes_mdot_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_pipe_tes_vel_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_vel_dsn", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "pipe_tes_vel_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_pipe_tes_wallthk_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_wallthk", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "pipe_tes_wallthk");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_plm_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plm_total_cost", &result))
		make_access_error("SAM_FresnelPhysical", "plm_total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_power_plant_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "power_plant_cost", &result))
		make_access_error("SAM_FresnelPhysical", "power_plant_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_pres_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pres", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "pres");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_pricing_mult_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pricing_mult", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "pricing_mult");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_balance", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_balance");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_ch_tes", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_ch_tes");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dc_tes", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dc_tes");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_q_dot_cycle_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_cycle_des", &result))
		make_access_error("SAM_FresnelPhysical", "q_dot_cycle_des");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dot_est_cr_on_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_cr_on", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dot_est_cr_on");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dot_est_cr_su_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_cr_su", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dot_est_cr_su");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dot_est_tes_ch_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_tes_ch", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dot_est_tes_ch");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dot_est_tes_dc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_tes_dc", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dot_est_tes_dc");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dot_freeze_prot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_freeze_prot", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dot_freeze_prot");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dot_htf_sf_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_htf_sf_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dot_htf_sf_out");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_q_dot_loss_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_loss_tes_des", &result))
		make_access_error("SAM_FresnelPhysical", "q_dot_loss_tes_des");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dot_pc_max_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_max", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dot_pc_max");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dot_pc_min_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_min", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dot_pc_min");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dot_pc_sb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_sb", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dot_pc_sb");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dot_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_startup", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dot_pc_startup");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dot_pc_target_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_target", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dot_pc_target");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dot_piping_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_piping_loss", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dot_piping_loss");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dot_rec_abs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_abs", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dot_rec_abs");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dot_rec_inc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_inc", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dot_rec_inc");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_dot_rec_thermal_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_thermal_loss", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_dot_rec_thermal_loss");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_q_field_des_actual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_field_des_actual", &result))
		make_access_error("SAM_FresnelPhysical", "q_field_des_actual");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_q_field_des_ideal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_field_des_ideal", &result))
		make_access_error("SAM_FresnelPhysical", "q_field_des_ideal");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_inc_sf_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_inc_sf_tot", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_inc_sf_tot");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_pb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pb", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_pb");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pc_startup", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_pc_startup");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_q_tes_heater_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_tes_heater", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "q_tes_heater");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_rec_thermal_eff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rec_thermal_eff", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "rec_thermal_eff");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_recirculating_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "recirculating", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "recirculating");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_rh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rh", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "rh");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_sales_tax_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_total", &result))
		make_access_error("SAM_FresnelPhysical", "sales_tax_total");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_sim_duration_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_duration", &result))
		make_access_error("SAM_FresnelPhysical", "sim_duration");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_site_improvements_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "site_improvements_cost", &result))
		make_access_error("SAM_FresnelPhysical", "site_improvements_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_sm1_aperture_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sm1_aperture", &result))
		make_access_error("SAM_FresnelPhysical", "sm1_aperture");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_sm1_nLoops_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sm1_nLoops", &result))
		make_access_error("SAM_FresnelPhysical", "sm1_nLoops");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_solar_field_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_field_cost", &result))
		make_access_error("SAM_FresnelPhysical", "solar_field_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_solar_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_mult", &result))
		make_access_error("SAM_FresnelPhysical", "solar_mult");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_solazi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solazi", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "solazi");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solzen", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "solzen");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_FresnelPhysical", "system_capacity");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_tank_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tank_losses", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "tank_losses");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "tdry");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_tes_htf_cp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_htf_cp", &result))
		make_access_error("SAM_FresnelPhysical", "tes_htf_cp");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_tes_htf_dens_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_htf_dens", &result))
		make_access_error("SAM_FresnelPhysical", "tes_htf_dens");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_tes_htf_max_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_htf_max_temp", &result))
		make_access_error("SAM_FresnelPhysical", "tes_htf_max_temp");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_tes_htf_min_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_htf_min_temp", &result))
		make_access_error("SAM_FresnelPhysical", "tes_htf_min_temp");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_tes_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_htf_pump_power", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "tes_htf_pump_power");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_therm_eff_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "therm_eff_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "therm_eff_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_therm_eff_loop_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "therm_eff_loop_des_SS", &result))
		make_access_error("SAM_FresnelPhysical", "therm_eff_loop_des_SS");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "time_hr", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "time_hr");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_total_Ap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_Ap", &result))
		make_access_error("SAM_FresnelPhysical", "total_Ap");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_total_direct_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_direct_cost", &result))
		make_access_error("SAM_FresnelPhysical", "total_direct_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_total_indirect_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_indirect_cost", &result))
		make_access_error("SAM_FresnelPhysical", "total_indirect_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_FresnelPhysical", "total_installed_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_total_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_land_area", &result))
		make_access_error("SAM_FresnelPhysical", "total_land_area");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_total_tracking_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_tracking_power", &result))
		make_access_error("SAM_FresnelPhysical", "total_tracking_power");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_tou_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tou_value", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "tou_value");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_ts_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ts_cost", &result))
		make_access_error("SAM_FresnelPhysical", "ts_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "twet");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_vol_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vol_min", &result))
		make_access_error("SAM_FresnelPhysical", "vol_min");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysical_Outputs_vol_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vol_tank", &result))
		make_access_error("SAM_FresnelPhysical", "vol_tank");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysical_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_FresnelPhysical", "wspd");
	});
	return result;
}


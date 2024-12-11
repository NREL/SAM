#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_FresnelPhysicalIph.h"

SAM_EXPORT int SAM_FresnelPhysicalIph_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("fresnel_physical_iph", data, verbosity, err);
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SystemControl_disp_inventory_incentive_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_inventory_incentive", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sim_type", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Weather_file_name_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "file_name", str);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_I_bn_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_bn_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_T_loop_in_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_in_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_T_loop_out_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_out", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_q_pb_design_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_pb_design", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_solar_mult_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "solar_mult_in", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_solar_mult_or_Ap_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "solar_mult_or_Ap", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_total_Ap_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "total_Ap_in", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_tshours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tshours", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_FieldConfig_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "FieldConfig", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_Fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Fluid", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_HDR_rough_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HDR_rough", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_L_rnr_pb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_rnr_pb", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_Pipe_hl_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Pipe_hl_coef", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_SCA_drives_elec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SCA_drives_elec", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_T_amb_sf_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_sf_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_T_fp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_fp", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_T_startup_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_startup", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_V_hdr_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_max", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_V_hdr_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_min", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_V_wind_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_wind_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_eta_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pump", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_f_htfmax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_htfmax", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_f_htfmin_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_htfmin", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "field_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_land_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_mult", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_m_dot_htfmax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htfmax", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_m_dot_htfmin_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htfmin", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_mc_bal_cold_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_cold", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_mc_bal_hot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_hot", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_mc_bal_sca_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_sca", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_nMod_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nMod", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_p_start_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "p_start", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_rec_htf_vol_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_htf_vol", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_rec_qf_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_qf_delay", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_rec_su_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_su_delay", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_theta_dep_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_dep", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_theta_stow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_stow", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_use_abs_or_rel_mdot_limit_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "use_abs_or_rel_mdot_limit", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_washes_per_year_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "washes_per_year", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_water_per_wash_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "water_per_wash", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_A_aperture_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "A_aperture", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_AbsorberMaterial_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "AbsorberMaterial", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_AnnulusGas_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "AnnulusGas", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_ColAz_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ColAz", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_DP_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "DP_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_DP_nominal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "DP_nominal", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_D_abs_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "D_abs_in", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_D_abs_out_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "D_abs_out", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_D_glass_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "D_glass_in", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_D_glass_out_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "D_glass_out", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_D_plug_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "D_plug", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_Design_loss_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Design_loss", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_Dirt_mirror_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Dirt_mirror", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_Error_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Error", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_Flow_type_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Flow_type", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_GeomEffects_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "GeomEffects", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_GlazingIntactIn_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "GlazingIntactIn", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_HCE_FieldFrac_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCE_FieldFrac", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_HL_T_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HL_T_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_HL_w_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HL_w_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_IAM_L_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "IAM_L_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_IAM_T_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "IAM_T_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_L_crossover_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_crossover", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_L_mod_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_mod", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_L_mod_spacing_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_mod_spacing", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_OpticalTable_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "OpticalTable", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_P_a_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "P_a", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_Rough_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Rough", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_Shadowing_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Shadowing", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_Tau_envelope_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Tau_envelope", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_TrackingError_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TrackingError", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_alpha_abs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "alpha_abs", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_alpha_env_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "alpha_env", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_dirt_env_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dirt_env", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_epsilon_abs_1_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_abs_1", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_epsilon_abs_2_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_abs_2", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_epsilon_abs_3_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_abs_3", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_epsilon_abs_4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_abs_4", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_epsilon_glass_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "epsilon_glass", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_nRecVar_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nRecVar", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_opt_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_model", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_rec_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_model", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_reflectivity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "reflectivity", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_phys_N_sub_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_phys_N_sub", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_phys_P_steam_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_phys_P_steam_hot_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_phys_Q_steam_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_phys_Q_steam_hot_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_phys_T_steam_cold_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_phys_T_steam_cold_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_phys_f_mdot_steam_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_phys_f_mdot_steam_max", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_phys_f_mdot_steam_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_phys_f_mdot_steam_min", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_phys_tol_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_phys_tol", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_type", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_pump_coef", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_V_tes_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_tes_des", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_dt_cold_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_cold", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_dt_hot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_hot", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_h_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_min", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "init_hot_htf_percent", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_store_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "store_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_store_fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "store_fluid", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_pairs", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_tanks_in_parallel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tanks_in_parallel", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_tes_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pump_coef", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_u_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_disp_reporting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_reporting", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_disp_spec_bb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_bb", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_disp_spec_presolve_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_presolve", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_disp_spec_scaling_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_scaling", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_disp_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_steps_per_hour", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_factors_ts", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_f_turb_tou_periods_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "f_turb_tou_periods", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_is_timestep_load_fractions_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_timestep_load_fractions", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_is_tod_pc_target_also_pc_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_tod_pc_target_also_pc_max", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_multiplier_model", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_q_rec_heattrace_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_rec_heattrace", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_q_rec_standby_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_rec_standby", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_timestep_load_abs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "timestep_load_abs", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_timestep_load_abs_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "timestep_load_abs_factor", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_timestep_load_fractions_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "timestep_load_fractions", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekday_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekend_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_aux_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "aux_array", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_bop_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "bop_array", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_disp_frequency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_frequency", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_disp_horizon_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_horizon", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_disp_max_iter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_max_iter", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_disp_mip_gap_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_mip_gap", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_disp_rsu_cost_rel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_rsu_cost_rel", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_disp_time_weighting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_time_weighting", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_disp_timeout_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_timeout", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_is_dispatch_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_dispatch", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_fixed_par", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialModel_csp_financial_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp_financial_model", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialSolutionMode_ppa_soln_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_soln_mode", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_TimeOfDeliveryFactors_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_TimeOfDeliveryFactors_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_TimeOfDeliveryFactors_dispatch_tod_factors_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_tod_factors", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Revenue_ppa_price_input_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ppa_price_input_heat_btu", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_bop_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_spec_cost", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_contingency_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "contingency_percent", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_fixed", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_per_acre_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_per_acre", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_per_watt", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_percent_direct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_percent_direct", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_heat_sink_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heat_sink_spec_cost", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_htf_system_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "htf_system_spec_cost", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "plm_cost_fixed", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_per_acre_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "plm_cost_per_acre", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "plm_cost_per_watt", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_percent_direct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "plm_cost_percent_direct", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_sales_tax_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_percent", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_rate", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_site_improvements_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "site_improvements_spec_cost", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_solar_field_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "solar_field_spec_cost", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_storage_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "storage_spec_cost", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate1", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate2", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate3", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate4", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate5", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months1", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months2", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months3", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months4", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months5", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent1", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent2", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent3", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent4", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent5", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate1", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate2", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate3", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate4", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate5", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_constant", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_periods", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_timeindex", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "adjust_periods", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "adjust_timeindex", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_en_electricity_rates_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_electricity_rates", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_rate_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "rate_escalation", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_annual_min_charge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_annual_min_charge", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_billing_demand_lookback_percentages_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_billing_demand_lookback_percentages", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_billing_demand_lookback_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_billing_demand_lookback_period", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_billing_demand_minimum_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_billing_demand_minimum", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_billing_demand_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_billing_demand_periods", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_enable_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_enable", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_flat_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_flat_mat", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_tou_mat", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_ec_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_ec_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_ec_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_tou_mat", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_en_ts_buy_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_en_ts_buy_rate", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_en_ts_sell_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_en_ts_sell_rate", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_enable_billing_demand_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_enable_billing_demand", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_metering_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_metering_option", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_monthly_fixed_charge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_monthly_fixed_charge", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_monthly_min_charge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_monthly_min_charge", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_nb_apply_credit_current_month_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_nb_apply_credit_current_month", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_nb_credit_expire_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_nb_credit_expire", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_nm_credit_month_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_nm_credit_month", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_nm_credit_rollover_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_nm_credit_rollover", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_nm_yearend_sell_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_nm_yearend_sell_rate", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_sell_eq_buy_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_sell_eq_buy", number);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_ts_buy_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ur_ts_buy_rate", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_ts_sell_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ur_ts_sell_rate", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_yearzero_usage_peaks_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ur_yearzero_usage_peaks", arr, length);
	});
}

SAM_EXPORT void SAM_FresnelPhysicalIph_Lifetime_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inflation_rate", number);
	});
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SystemControl_disp_inventory_incentive_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_inventory_incentive", &result))
		make_access_error("SAM_FresnelPhysicalIph", "disp_inventory_incentive");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_type", &result))
		make_access_error("SAM_FresnelPhysicalIph", "sim_type");
	});
	return result;
}

SAM_EXPORT const char* SAM_FresnelPhysicalIph_Weather_file_name_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "file_name");
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "file_name");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_I_bn_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_bn_des", &result))
		make_access_error("SAM_FresnelPhysicalIph", "I_bn_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_T_loop_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_in_des", &result))
		make_access_error("SAM_FresnelPhysicalIph", "T_loop_in_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_T_loop_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_out", &result))
		make_access_error("SAM_FresnelPhysicalIph", "T_loop_out");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_q_pb_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_pb_design", &result))
		make_access_error("SAM_FresnelPhysicalIph", "q_pb_design");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_solar_mult_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_mult_in", &result))
		make_access_error("SAM_FresnelPhysicalIph", "solar_mult_in");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_solar_mult_or_Ap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_mult_or_Ap", &result))
		make_access_error("SAM_FresnelPhysicalIph", "solar_mult_or_Ap");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_total_Ap_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_Ap_in", &result))
		make_access_error("SAM_FresnelPhysicalIph", "total_Ap_in");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_tshours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours", &result))
		make_access_error("SAM_FresnelPhysicalIph", "tshours");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_FieldConfig_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "FieldConfig", &result))
		make_access_error("SAM_FresnelPhysicalIph", "FieldConfig");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_Fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Fluid", &result))
		make_access_error("SAM_FresnelPhysicalIph", "Fluid");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_HDR_rough_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HDR_rough", &result))
		make_access_error("SAM_FresnelPhysicalIph", "HDR_rough");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_L_rnr_pb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_rnr_pb", &result))
		make_access_error("SAM_FresnelPhysicalIph", "L_rnr_pb");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_Pipe_hl_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Pipe_hl_coef", &result))
		make_access_error("SAM_FresnelPhysicalIph", "Pipe_hl_coef");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_SCA_drives_elec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SCA_drives_elec", &result))
		make_access_error("SAM_FresnelPhysicalIph", "SCA_drives_elec");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_T_amb_sf_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_sf_des", &result))
		make_access_error("SAM_FresnelPhysicalIph", "T_amb_sf_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_T_fp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_fp", &result))
		make_access_error("SAM_FresnelPhysicalIph", "T_fp");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_T_startup_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_startup", &result))
		make_access_error("SAM_FresnelPhysicalIph", "T_startup");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_V_hdr_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_max", &result))
		make_access_error("SAM_FresnelPhysicalIph", "V_hdr_max");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_V_hdr_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_min", &result))
		make_access_error("SAM_FresnelPhysicalIph", "V_hdr_min");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_V_wind_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_wind_des", &result))
		make_access_error("SAM_FresnelPhysicalIph", "V_wind_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_eta_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pump", &result))
		make_access_error("SAM_FresnelPhysicalIph", "eta_pump");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_f_htfmax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_htfmax", &result))
		make_access_error("SAM_FresnelPhysicalIph", "f_htfmax");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_f_htfmin_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_htfmin", &result))
		make_access_error("SAM_FresnelPhysicalIph", "f_htfmin");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_SolarField_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "field_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "field_fl_props");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_land_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_mult", &result))
		make_access_error("SAM_FresnelPhysicalIph", "land_mult");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_m_dot_htfmax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmax", &result))
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_htfmax");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_m_dot_htfmin_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmin", &result))
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_htfmin");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_mc_bal_cold_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_cold", &result))
		make_access_error("SAM_FresnelPhysicalIph", "mc_bal_cold");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_mc_bal_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_hot", &result))
		make_access_error("SAM_FresnelPhysicalIph", "mc_bal_hot");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_mc_bal_sca_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_sca", &result))
		make_access_error("SAM_FresnelPhysicalIph", "mc_bal_sca");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_nMod_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nMod", &result))
		make_access_error("SAM_FresnelPhysicalIph", "nMod");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_p_start_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "p_start", &result))
		make_access_error("SAM_FresnelPhysicalIph", "p_start");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_rec_htf_vol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_htf_vol", &result))
		make_access_error("SAM_FresnelPhysicalIph", "rec_htf_vol");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_rec_qf_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_qf_delay", &result))
		make_access_error("SAM_FresnelPhysicalIph", "rec_qf_delay");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_rec_su_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_su_delay", &result))
		make_access_error("SAM_FresnelPhysicalIph", "rec_su_delay");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_theta_dep_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_dep", &result))
		make_access_error("SAM_FresnelPhysicalIph", "theta_dep");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_theta_stow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_stow", &result))
		make_access_error("SAM_FresnelPhysicalIph", "theta_stow");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_use_abs_or_rel_mdot_limit_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "use_abs_or_rel_mdot_limit", &result))
		make_access_error("SAM_FresnelPhysicalIph", "use_abs_or_rel_mdot_limit");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_washes_per_year_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "washes_per_year", &result))
		make_access_error("SAM_FresnelPhysicalIph", "washes_per_year");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_water_per_wash_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "water_per_wash", &result))
		make_access_error("SAM_FresnelPhysicalIph", "water_per_wash");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_A_aperture_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_aperture", &result))
		make_access_error("SAM_FresnelPhysicalIph", "A_aperture");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_AbsorberMaterial_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "AbsorberMaterial", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "AbsorberMaterial");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_AnnulusGas_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "AnnulusGas", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "AnnulusGas");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_ColAz_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ColAz", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ColAz");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_DP_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "DP_coefs", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "DP_coefs");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_DP_nominal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "DP_nominal", &result))
		make_access_error("SAM_FresnelPhysicalIph", "DP_nominal");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_D_abs_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "D_abs_in", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "D_abs_in");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_D_abs_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "D_abs_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "D_abs_out");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_D_glass_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "D_glass_in", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "D_glass_in");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_D_glass_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "D_glass_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "D_glass_out");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_D_plug_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "D_plug", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "D_plug");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_Design_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Design_loss", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "Design_loss");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_Dirt_mirror_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Dirt_mirror", &result))
		make_access_error("SAM_FresnelPhysicalIph", "Dirt_mirror");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_Error_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Error", &result))
		make_access_error("SAM_FresnelPhysicalIph", "Error");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_Flow_type_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Flow_type", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "Flow_type");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_GeomEffects_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "GeomEffects", &result))
		make_access_error("SAM_FresnelPhysicalIph", "GeomEffects");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_GlazingIntactIn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "GlazingIntactIn", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "GlazingIntactIn");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_HCE_FieldFrac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCE_FieldFrac", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "HCE_FieldFrac");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_HL_T_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HL_T_coefs", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "HL_T_coefs");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_HL_w_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HL_w_coefs", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "HL_w_coefs");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_IAM_L_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "IAM_L_coefs", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "IAM_L_coefs");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_IAM_T_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "IAM_T_coefs", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "IAM_T_coefs");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_L_crossover_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_crossover", &result))
		make_access_error("SAM_FresnelPhysicalIph", "L_crossover");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_L_mod_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_mod", &result))
		make_access_error("SAM_FresnelPhysicalIph", "L_mod");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_L_mod_spacing_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_mod_spacing", &result))
		make_access_error("SAM_FresnelPhysicalIph", "L_mod_spacing");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_OpticalTable_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "OpticalTable", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "OpticalTable");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_P_a_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_a", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "P_a");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_Rough_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Rough", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "Rough");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_Shadowing_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Shadowing", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "Shadowing");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_Tau_envelope_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Tau_envelope", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "Tau_envelope");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_TrackingError_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TrackingError", &result))
		make_access_error("SAM_FresnelPhysicalIph", "TrackingError");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_alpha_abs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "alpha_abs", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "alpha_abs");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_alpha_env_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "alpha_env", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "alpha_env");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_dirt_env_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dirt_env", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "dirt_env");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_epsilon_abs_1_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_abs_1", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "epsilon_abs_1");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_epsilon_abs_2_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_abs_2", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "epsilon_abs_2");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_epsilon_abs_3_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_abs_3", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "epsilon_abs_3");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_epsilon_abs_4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_abs_4", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "epsilon_abs_4");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_epsilon_glass_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "epsilon_glass", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "epsilon_glass");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_nRecVar_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nRecVar", &result))
		make_access_error("SAM_FresnelPhysicalIph", "nRecVar");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_opt_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_model", &result))
		make_access_error("SAM_FresnelPhysicalIph", "opt_model");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_rec_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_model", &result))
		make_access_error("SAM_FresnelPhysicalIph", "rec_model");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_reflectivity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "reflectivity", &result))
		make_access_error("SAM_FresnelPhysicalIph", "reflectivity");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_phys_N_sub_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_phys_N_sub", &result))
		make_access_error("SAM_FresnelPhysicalIph", "hs_phys_N_sub");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_phys_P_steam_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_phys_P_steam_hot_des", &result))
		make_access_error("SAM_FresnelPhysicalIph", "hs_phys_P_steam_hot_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_phys_Q_steam_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_phys_Q_steam_hot_des", &result))
		make_access_error("SAM_FresnelPhysicalIph", "hs_phys_Q_steam_hot_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_phys_T_steam_cold_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_phys_T_steam_cold_des", &result))
		make_access_error("SAM_FresnelPhysicalIph", "hs_phys_T_steam_cold_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_phys_f_mdot_steam_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_phys_f_mdot_steam_max", &result))
		make_access_error("SAM_FresnelPhysicalIph", "hs_phys_f_mdot_steam_max");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_phys_f_mdot_steam_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_phys_f_mdot_steam_min", &result))
		make_access_error("SAM_FresnelPhysicalIph", "hs_phys_f_mdot_steam_min");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_phys_tol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_phys_tol", &result))
		make_access_error("SAM_FresnelPhysicalIph", "hs_phys_tol");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_type", &result))
		make_access_error("SAM_FresnelPhysicalIph", "hs_type");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_pb_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_pump_coef", &result))
		make_access_error("SAM_FresnelPhysicalIph", "pb_pump_coef");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_V_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tes_des", &result))
		make_access_error("SAM_FresnelPhysicalIph", "V_tes_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_Thtr", &result))
		make_access_error("SAM_FresnelPhysicalIph", "cold_tank_Thtr");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_max_heat", &result))
		make_access_error("SAM_FresnelPhysicalIph", "cold_tank_max_heat");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_dt_cold_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_cold", &result))
		make_access_error("SAM_FresnelPhysicalIph", "dt_cold");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_dt_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_hot", &result))
		make_access_error("SAM_FresnelPhysicalIph", "dt_hot");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_h_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank", &result))
		make_access_error("SAM_FresnelPhysicalIph", "h_tank");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_h_tank_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_min", &result))
		make_access_error("SAM_FresnelPhysicalIph", "h_tank_min");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_Thtr", &result))
		make_access_error("SAM_FresnelPhysicalIph", "hot_tank_Thtr");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_max_heat", &result))
		make_access_error("SAM_FresnelPhysicalIph", "hot_tank_max_heat");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "init_hot_htf_percent", &result))
		make_access_error("SAM_FresnelPhysicalIph", "init_hot_htf_percent");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Storage_store_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "store_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "store_fl_props");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_store_fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "store_fluid", &result))
		make_access_error("SAM_FresnelPhysicalIph", "store_fluid");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_tank_pairs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_pairs", &result))
		make_access_error("SAM_FresnelPhysicalIph", "tank_pairs");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_tanks_in_parallel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tanks_in_parallel", &result))
		make_access_error("SAM_FresnelPhysicalIph", "tanks_in_parallel");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_tes_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pump_coef", &result))
		make_access_error("SAM_FresnelPhysicalIph", "tes_pump_coef");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_u_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_tank", &result))
		make_access_error("SAM_FresnelPhysicalIph", "u_tank");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_disp_reporting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_reporting", &result))
		make_access_error("SAM_FresnelPhysicalIph", "disp_reporting");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_disp_spec_bb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_bb", &result))
		make_access_error("SAM_FresnelPhysicalIph", "disp_spec_bb");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_disp_spec_presolve_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_presolve", &result))
		make_access_error("SAM_FresnelPhysicalIph", "disp_spec_presolve");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_disp_spec_scaling_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_scaling", &result))
		make_access_error("SAM_FresnelPhysicalIph", "disp_spec_scaling");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_disp_steps_per_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_steps_per_hour", &result))
		make_access_error("SAM_FresnelPhysicalIph", "disp_steps_per_hour");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Tou_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_factors_ts", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "dispatch_factors_ts");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Tou_f_turb_tou_periods_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "f_turb_tou_periods", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "f_turb_tou_periods");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_is_timestep_load_fractions_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_timestep_load_fractions", &result))
		make_access_error("SAM_FresnelPhysicalIph", "is_timestep_load_fractions");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_is_tod_pc_target_also_pc_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_tod_pc_target_also_pc_max", &result))
		make_access_error("SAM_FresnelPhysicalIph", "is_tod_pc_target_also_pc_max");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_multiplier_model", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ppa_multiplier_model");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_q_rec_heattrace_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_rec_heattrace", &result))
		make_access_error("SAM_FresnelPhysicalIph", "q_rec_heattrace");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_q_rec_standby_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_rec_standby", &result))
		make_access_error("SAM_FresnelPhysicalIph", "q_rec_standby");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Tou_timestep_load_abs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "timestep_load_abs", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "timestep_load_abs");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_timestep_load_abs_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "timestep_load_abs_factor", &result))
		make_access_error("SAM_FresnelPhysicalIph", "timestep_load_abs_factor");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Tou_timestep_load_fractions_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "timestep_load_fractions", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "timestep_load_fractions");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Tou_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekday_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "weekday_schedule");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Tou_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekend_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "weekend_schedule");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_SysControl_aux_array_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "aux_array", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "aux_array");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_SysControl_bop_array_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "bop_array", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "bop_array");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_disp_frequency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_frequency", &result))
		make_access_error("SAM_FresnelPhysicalIph", "disp_frequency");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_disp_horizon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_horizon", &result))
		make_access_error("SAM_FresnelPhysicalIph", "disp_horizon");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_disp_max_iter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_max_iter", &result))
		make_access_error("SAM_FresnelPhysicalIph", "disp_max_iter");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_disp_mip_gap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_mip_gap", &result))
		make_access_error("SAM_FresnelPhysicalIph", "disp_mip_gap");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_disp_rsu_cost_rel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_rsu_cost_rel", &result))
		make_access_error("SAM_FresnelPhysicalIph", "disp_rsu_cost_rel");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_disp_time_weighting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_time_weighting", &result))
		make_access_error("SAM_FresnelPhysicalIph", "disp_time_weighting");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_disp_timeout_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_timeout", &result))
		make_access_error("SAM_FresnelPhysicalIph", "disp_timeout");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_is_dispatch_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_dispatch", &result))
		make_access_error("SAM_FresnelPhysicalIph", "is_dispatch");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_pb_fixed_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_fixed_par", &result))
		make_access_error("SAM_FresnelPhysicalIph", "pb_fixed_par");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialModel_csp_financial_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp_financial_model", &result))
		make_access_error("SAM_FresnelPhysicalIph", "csp_financial_model");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialSolutionMode_ppa_soln_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_soln_mode", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ppa_soln_mode");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_TimeOfDeliveryFactors_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "dispatch_sched_weekday");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_TimeOfDeliveryFactors_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "dispatch_sched_weekend");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_TimeOfDeliveryFactors_dispatch_tod_factors_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_tod_factors", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "dispatch_tod_factors");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Revenue_ppa_price_input_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ppa_price_input_heat_btu", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "ppa_price_input_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_bop_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_spec_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "bop_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_contingency_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_percent", &result))
		make_access_error("SAM_FresnelPhysicalIph", "contingency_percent");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_fixed", &result))
		make_access_error("SAM_FresnelPhysicalIph", "epc_cost_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_per_acre_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_per_acre", &result))
		make_access_error("SAM_FresnelPhysicalIph", "epc_cost_per_acre");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_per_watt", &result))
		make_access_error("SAM_FresnelPhysicalIph", "epc_cost_per_watt");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_percent_direct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_percent_direct", &result))
		make_access_error("SAM_FresnelPhysicalIph", "epc_cost_percent_direct");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_heat_sink_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heat_sink_spec_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "heat_sink_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_htf_system_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "htf_system_spec_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "htf_system_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plm_cost_fixed", &result))
		make_access_error("SAM_FresnelPhysicalIph", "plm_cost_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_per_acre_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plm_cost_per_acre", &result))
		make_access_error("SAM_FresnelPhysicalIph", "plm_cost_per_acre");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plm_cost_per_watt", &result))
		make_access_error("SAM_FresnelPhysicalIph", "plm_cost_per_watt");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_percent_direct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plm_cost_percent_direct", &result))
		make_access_error("SAM_FresnelPhysicalIph", "plm_cost_percent_direct");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_sales_tax_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_percent", &result))
		make_access_error("SAM_FresnelPhysicalIph", "sales_tax_percent");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_sales_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_rate", &result))
		make_access_error("SAM_FresnelPhysicalIph", "sales_tax_rate");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_site_improvements_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "site_improvements_spec_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "site_improvements_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_solar_field_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_field_spec_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "solar_field_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_storage_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "storage_spec_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "storage_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate1", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_interest_rate1");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate2", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_interest_rate2");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate3", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_interest_rate3");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate4", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_interest_rate4");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate5", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_interest_rate5");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months1", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_months1");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months2", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_months2");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months3", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_months3");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months4", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_months4");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months5", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_months5");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent1", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_percent1");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent2", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_percent2");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent3", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_percent3");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent4", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_percent4");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent5", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_percent5");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate1", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_upfront_rate1");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate2", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_upfront_rate2");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate3", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_upfront_rate3");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate4", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_upfront_rate4");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate5", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_upfront_rate5");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_constant_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_constant", &result))
		make_access_error("SAM_FresnelPhysicalIph", "adjust_constant");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_en_periods_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_periods", &result))
		make_access_error("SAM_FresnelPhysicalIph", "adjust_en_periods");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_timeindex", &result))
		make_access_error("SAM_FresnelPhysicalIph", "adjust_en_timeindex");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "adjust_periods", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "adjust_periods");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "adjust_timeindex", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "adjust_timeindex");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_en_electricity_rates_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_electricity_rates", &result))
		make_access_error("SAM_FresnelPhysicalIph", "en_electricity_rates");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_rate_escalation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rate_escalation", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "rate_escalation");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_annual_min_charge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_annual_min_charge", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_annual_min_charge");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_billing_demand_lookback_percentages_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_billing_demand_lookback_percentages", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "ur_billing_demand_lookback_percentages");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_billing_demand_lookback_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_billing_demand_lookback_period", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_billing_demand_lookback_period");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_billing_demand_minimum_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_billing_demand_minimum", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_billing_demand_minimum");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_billing_demand_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_billing_demand_periods", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "ur_dc_billing_demand_periods");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_enable_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_enable", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_dc_enable");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_flat_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_flat_mat", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "ur_dc_flat_mat");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "ur_dc_sched_weekday");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "ur_dc_sched_weekend");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_tou_mat", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "ur_dc_tou_mat");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_ec_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "ur_ec_sched_weekday");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_ec_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "ur_ec_sched_weekend");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_ec_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_tou_mat", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "ur_ec_tou_mat");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_en_ts_buy_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_en_ts_buy_rate", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_en_ts_buy_rate");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_en_ts_sell_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_en_ts_sell_rate", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_en_ts_sell_rate");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_enable_billing_demand_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_enable_billing_demand", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_enable_billing_demand");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_metering_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_metering_option", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_metering_option");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_monthly_fixed_charge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_monthly_fixed_charge", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_monthly_fixed_charge");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_monthly_min_charge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_monthly_min_charge", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_monthly_min_charge");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_nb_apply_credit_current_month_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_nb_apply_credit_current_month", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_nb_apply_credit_current_month");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_nb_credit_expire_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_nb_credit_expire", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_nb_credit_expire");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_nm_credit_month_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_nm_credit_month", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_nm_credit_month");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_nm_credit_rollover_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_nm_credit_rollover", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_nm_credit_rollover");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_nm_yearend_sell_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_nm_yearend_sell_rate", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_nm_yearend_sell_rate");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_sell_eq_buy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_sell_eq_buy", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ur_sell_eq_buy");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_ts_buy_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ur_ts_buy_rate", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "ur_ts_buy_rate");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_ts_sell_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ur_ts_sell_rate", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "ur_ts_sell_rate");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_yearzero_usage_peaks_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ur_yearzero_usage_peaks", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "ur_yearzero_usage_peaks");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Lifetime_inflation_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inflation_rate", &result))
		make_access_error("SAM_FresnelPhysicalIph", "inflation_rate");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_A_field_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_field", &result))
		make_access_error("SAM_FresnelPhysicalIph", "A_field");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_A_loop_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_loop", &result))
		make_access_error("SAM_FresnelPhysicalIph", "A_loop");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_DP_pressure_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "DP_pressure_loss", &result))
		make_access_error("SAM_FresnelPhysicalIph", "DP_pressure_loss");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_EqOpteff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EqOpteff", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "EqOpteff");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_P_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_fixed", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "P_fixed");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_P_plant_balance_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_plant_balance_tot", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "P_plant_balance_tot");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_Q_field_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_field_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "Q_field_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_Q_loop_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_loop_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "Q_loop_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_Q_loss_hdr_rnr_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_loss_hdr_rnr_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "Q_loss_hdr_rnr_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_Q_loss_receiver_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_loss_receiver_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "Q_loss_receiver_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_Q_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_tes_des", &result))
		make_access_error("SAM_FresnelPhysicalIph", "Q_tes_des");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_SCAs_def_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "SCAs_def", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "SCAs_def");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_field_cold_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_field_cold_in", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "T_field_cold_in");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_field_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_field_hot_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "T_field_hot_out");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_T_field_out_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_field_out_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "T_field_out_des_SS");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_heat_sink_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_heat_sink_in", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "T_heat_sink_in");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_heat_sink_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_heat_sink_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "T_heat_sink_out");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_T_loop_out_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_out_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "T_loop_out_des_SS");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_rec_cold_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_cold_in", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "T_rec_cold_in");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_rec_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_hot_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "T_rec_hot_out");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_cold", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "T_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_hot", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "T_tes_hot");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_V_hdr_max_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_max_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "V_hdr_max_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_V_hdr_min_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_min_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "V_hdr_min_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_W_dot_bop_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_bop_design", &result))
		make_access_error("SAM_FresnelPhysicalIph", "W_dot_bop_design");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_W_dot_field_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_field_pump", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "W_dot_field_pump");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_W_dot_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_fixed", &result))
		make_access_error("SAM_FresnelPhysicalIph", "W_dot_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_W_dot_par_tot_haf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_par_tot_haf", &result))
		make_access_error("SAM_FresnelPhysicalIph", "W_dot_par_tot_haf");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_W_dot_parasitic_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_parasitic_tot", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "W_dot_parasitic_tot");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_W_dot_pc_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_pc_pump", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "W_dot_pc_pump");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_W_dot_pump_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_pump_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "W_dot_pump_des_SS");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_W_dot_sca_track_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_sca_track", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "W_dot_sca_track");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_annual_electricity_consumption_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_electricity_consumption", &result))
		make_access_error("SAM_FresnelPhysicalIph", "annual_electricity_consumption");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_FresnelPhysicalIph", "annual_energy");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution_time", nrows, ncols);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "annual_energy_distribution_time");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_annual_energy_heat_btu_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy_heat_btu", &result))
		make_access_error("SAM_FresnelPhysicalIph", "annual_energy_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_annual_field_freeze_protection_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_field_freeze_protection", &result))
		make_access_error("SAM_FresnelPhysicalIph", "annual_field_freeze_protection");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_annual_tes_freeze_protection_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_tes_freeze_protection", &result))
		make_access_error("SAM_FresnelPhysicalIph", "annual_tes_freeze_protection");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_annual_thermal_consumption_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_thermal_consumption", &result))
		make_access_error("SAM_FresnelPhysicalIph", "annual_thermal_consumption");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_annual_total_water_use_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_total_water_use", &result))
		make_access_error("SAM_FresnelPhysicalIph", "annual_total_water_use");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_aux_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_design", &result))
		make_access_error("SAM_FresnelPhysicalIph", "aux_design");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_avg_dt_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "avg_dt_des", &result))
		make_access_error("SAM_FresnelPhysicalIph", "avg_dt_des");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_avg_suboptimal_rel_mip_gap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "avg_suboptimal_rel_mip_gap", &result))
		make_access_error("SAM_FresnelPhysicalIph", "avg_suboptimal_rel_mip_gap");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "beam");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_bop_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "bop_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_FresnelPhysicalIph", "capacity_factor");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_interest1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest1", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_interest1");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_interest2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest2", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_interest2");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_interest3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest3", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_interest3");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_interest4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest4", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_interest4");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_interest5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest5", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_interest5");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_interest_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_total", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_interest_total");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_percent_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent_total", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_percent_total");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_principal1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal1", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_principal1");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_principal2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal2", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_principal2");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_principal3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal3", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_principal3");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_principal4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal4", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_principal4");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_principal5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal5", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_principal5");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_principal_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal_total", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_principal_total");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_total1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total1", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_total1");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_total2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total2", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_total2");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_total3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total3", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_total3");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_total4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total4", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_total4");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_total5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total5", &result))
		make_access_error("SAM_FresnelPhysicalIph", "const_per_total5");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_financing_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "construction_financing_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_contingency_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "contingency_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_cp_battery_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cp_battery_nameplate", &result))
		make_access_error("SAM_FresnelPhysicalIph", "cp_battery_nameplate");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_cp_system_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cp_system_nameplate", &result))
		make_access_error("SAM_FresnelPhysicalIph", "cp_system_nameplate");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_dP_field_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dP_field_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "dP_field_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_d_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_tank", &result))
		make_access_error("SAM_FresnelPhysicalIph", "d_tank");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_defocus_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "defocus", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "defocus");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_deltaP_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "deltaP_field", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "deltaP_field");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_obj_relax_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_obj_relax", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_obj_relax");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_objective_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_objective", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_objective");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_pceff_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_pceff_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_pceff_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_presolve_nconstr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_presolve_nconstr", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_presolve_nconstr");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_presolve_nvar_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_presolve_nvar", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_presolve_nvar");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_qpbsu_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qpbsu_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_qpbsu_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_qsf_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsf_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_qsf_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_qsfprod_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsfprod_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_qsfprod_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_qsfsu_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsfsu_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_qsfsu_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_rel_mip_gap_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_rel_mip_gap", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_rel_mip_gap");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_rev_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_rev_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_rev_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_solve_iter_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_iter", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_solve_iter");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_solve_state_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_state", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_solve_state");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_solve_time_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_time", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_solve_time");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_subopt_flag_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_subopt_flag", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_subopt_flag");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_tes_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_tes_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_tes_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_thermeff_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_thermeff_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_thermeff_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_wpb_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_wpb_expected", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "disp_wpb_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_ch_tes", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "e_ch_tes");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_e_dot_field_int_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_dot_field_int_energy", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "e_dot_field_int_energy");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_eff_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eff_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "eff_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_eff_loop_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eff_loop_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "eff_loop_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_epc_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_total_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "epc_total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_eta_optical_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_optical_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "eta_optical_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_f_htfmax_actual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_htfmax_actual", &result))
		make_access_error("SAM_FresnelPhysicalIph", "f_htfmax_actual");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_f_htfmin_actual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_htfmin_actual", &result))
		make_access_error("SAM_FresnelPhysicalIph", "f_htfmin_actual");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_field_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_area", &result))
		make_access_error("SAM_FresnelPhysicalIph", "field_area");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_field_htf_max_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_htf_max_temp", &result))
		make_access_error("SAM_FresnelPhysicalIph", "field_htf_max_temp");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_field_htf_min_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_htf_min_temp", &result))
		make_access_error("SAM_FresnelPhysicalIph", "field_htf_min_temp");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "gen");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_gen_heat_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen_heat", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "gen_heat");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_gen_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen_heat_btu", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "gen_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_heat_sink_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heat_sink_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "heat_sink_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_hl_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hl_des", &result))
		make_access_error("SAM_FresnelPhysicalIph", "hl_des");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_hour_day_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour_day", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "hour_day");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_htf_system_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "htf_system_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "htf_system_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_installed_per_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "installed_per_capacity", &result))
		make_access_error("SAM_FresnelPhysicalIph", "installed_per_capacity");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_is_pc_sb_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_sb_allowed", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "is_pc_sb_allowed");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_is_pc_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_su_allowed", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "is_pc_su_allowed");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_is_rec_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_rec_su_allowed", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "is_rec_su_allowed");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_FresnelPhysicalIph", "kwh_per_kw");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_loop_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loop_eff", &result))
		make_access_error("SAM_FresnelPhysicalIph", "loop_eff");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_loop_opt_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loop_opt_eff", &result))
		make_access_error("SAM_FresnelPhysicalIph", "loop_opt_eff");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_loop_therm_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loop_therm_eff", &result))
		make_access_error("SAM_FresnelPhysicalIph", "loop_therm_eff");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_balance", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_balance");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_cold_tank_to_hot_tank_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_cold_tank_to_hot_tank", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_cold_tank_to_hot_tank");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_cr_to_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_cr_to_tes_hot", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_cr_to_tes_hot");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_cycle_to_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_cycle_to_field", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_cycle_to_field");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_m_dot_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_des_SS");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_field_delivered_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_field_delivered", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_field_delivered");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_field_recirc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_field_recirc", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_field_recirc");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_field_to_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_field_to_cycle", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_field_to_cycle");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_htf_heat_sink_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_htf_heat_sink", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_htf_heat_sink");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_m_dot_htfmax_actual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmax_actual", &result))
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_htfmax_actual");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_m_dot_htfmin_actual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmin_actual", &result))
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_htfmin_actual");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_loop_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_loop", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_loop");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_m_dot_loop_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_loop_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_loop_des_SS");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_pc_to_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pc_to_tes_cold", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_pc_to_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_tes_cold_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_cold_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_tes_cold_out");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_tes_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_hot_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "m_dot_tes_hot_out");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_mass_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_cold", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "mass_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_mass_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_hot", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "mass_tes_hot");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_mdot_field_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mdot_field_des", &result))
		make_access_error("SAM_FresnelPhysicalIph", "mdot_field_des");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "month");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "monthly_energy");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_monthly_energy_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy_heat_btu", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "monthly_energy_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_nLoops_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nLoops", &result))
		make_access_error("SAM_FresnelPhysicalIph", "nLoops");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "n_op_modes", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "n_op_modes");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nameplate", &result))
		make_access_error("SAM_FresnelPhysicalIph", "nameplate");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_1", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "op_mode_1");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_2", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "op_mode_2");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_3", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "op_mode_3");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_operating_modes_a_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_a", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "operating_modes_a");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_operating_modes_b_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_b", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "operating_modes_b");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_operating_modes_c_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_c", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "operating_modes_c");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_opt_derate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_derate", &result))
		make_access_error("SAM_FresnelPhysicalIph", "opt_derate");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_opt_normal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_normal", &result))
		make_access_error("SAM_FresnelPhysicalIph", "opt_normal");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pipe_tes_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_P_dsn", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "pipe_tes_P_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pipe_tes_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_T_dsn", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "pipe_tes_T_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pipe_tes_diams_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_diams", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "pipe_tes_diams");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pipe_tes_lengths_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_lengths", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "pipe_tes_lengths");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pipe_tes_mdot_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_mdot_dsn", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "pipe_tes_mdot_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pipe_tes_vel_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_vel_dsn", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "pipe_tes_vel_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pipe_tes_wallthk_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_wallthk", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "pipe_tes_wallthk");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_plm_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plm_total_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "plm_total_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ppa_price_input", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "ppa_price_input");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pres_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pres", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "pres");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pricing_mult_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pricing_mult", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "pricing_mult");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_balance", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_balance");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_ch_tes", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_ch_tes");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dc_tes", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dc_tes");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_est_cr_on_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_cr_on", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_est_cr_on");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_est_cr_su_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_cr_su", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_est_cr_su");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_est_tes_ch_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_tes_ch", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_est_tes_ch");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_est_tes_dc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_tes_dc", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_est_tes_dc");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_freeze_prot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_freeze_prot", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_freeze_prot");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_htf_sf_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_htf_sf_out", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_htf_sf_out");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_q_dot_loss_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_loss_tes_des", &result))
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_loss_tes_des");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_pc_max_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_max", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_pc_max");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_pc_min_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_min", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_pc_min");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_pc_sb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_sb", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_pc_sb");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_pc_target_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_target", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_pc_target");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_piping_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_piping_loss", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_piping_loss");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_rec_abs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_abs", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_rec_abs");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_rec_inc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_inc", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_rec_inc");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_rec_thermal_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_thermal_loss", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_rec_thermal_loss");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_to_heat_sink_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_to_heat_sink", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_dot_to_heat_sink");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_q_field_des_actual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_field_des_actual", &result))
		make_access_error("SAM_FresnelPhysicalIph", "q_field_des_actual");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_q_field_des_ideal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_field_des_ideal", &result))
		make_access_error("SAM_FresnelPhysicalIph", "q_field_des_ideal");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_inc_sf_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_inc_sf_tot", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_inc_sf_tot");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_tes_heater_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_tes_heater", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "q_tes_heater");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_rec_thermal_eff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rec_thermal_eff", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "rec_thermal_eff");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_recirculating_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "recirculating", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "recirculating");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_rh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rh", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "rh");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_sales_tax_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_total", &result))
		make_access_error("SAM_FresnelPhysicalIph", "sales_tax_total");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_sim_duration_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_duration", &result))
		make_access_error("SAM_FresnelPhysicalIph", "sim_duration");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_site_improvements_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "site_improvements_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "site_improvements_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_sm1_aperture_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sm1_aperture", &result))
		make_access_error("SAM_FresnelPhysicalIph", "sm1_aperture");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_sm1_nLoops_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sm1_nLoops", &result))
		make_access_error("SAM_FresnelPhysicalIph", "sm1_nLoops");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_solar_field_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_field_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "solar_field_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_solar_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_mult", &result))
		make_access_error("SAM_FresnelPhysicalIph", "solar_mult");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_solazi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solazi", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "solazi");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solzen", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "solzen");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_FresnelPhysicalIph", "system_capacity");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_tank_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tank_losses", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "tank_losses");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "tdry");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_tes_htf_cp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_htf_cp", &result))
		make_access_error("SAM_FresnelPhysicalIph", "tes_htf_cp");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_tes_htf_dens_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_htf_dens", &result))
		make_access_error("SAM_FresnelPhysicalIph", "tes_htf_dens");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_tes_htf_max_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_htf_max_temp", &result))
		make_access_error("SAM_FresnelPhysicalIph", "tes_htf_max_temp");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_tes_htf_min_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_htf_min_temp", &result))
		make_access_error("SAM_FresnelPhysicalIph", "tes_htf_min_temp");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_tes_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_htf_pump_power", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "tes_htf_pump_power");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_therm_eff_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "therm_eff_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "therm_eff_des_SS");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_therm_eff_loop_des_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "therm_eff_loop_des_SS", &result))
		make_access_error("SAM_FresnelPhysicalIph", "therm_eff_loop_des_SS");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_thermal_load_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "thermal_load_heat_btu", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "thermal_load_heat_btu");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "time_hr", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "time_hr");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_timestep_load_abs_calc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "timestep_load_abs_calc", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "timestep_load_abs_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_timestep_load_fractions_calc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "timestep_load_fractions_calc", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "timestep_load_fractions_calc");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_total_Ap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_Ap", &result))
		make_access_error("SAM_FresnelPhysicalIph", "total_Ap");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_total_direct_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_direct_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "total_direct_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_total_indirect_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_indirect_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "total_indirect_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "total_installed_cost");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_total_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_land_area", &result))
		make_access_error("SAM_FresnelPhysicalIph", "total_land_area");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_total_tracking_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_tracking_power", &result))
		make_access_error("SAM_FresnelPhysicalIph", "total_tracking_power");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_tou_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tou_value", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "tou_value");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_ts_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ts_cost", &result))
		make_access_error("SAM_FresnelPhysicalIph", "ts_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "twet");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_vol_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vol_min", &result))
		make_access_error("SAM_FresnelPhysicalIph", "vol_min");
	});
	return result;
}

SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_vol_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vol_tank", &result))
		make_access_error("SAM_FresnelPhysicalIph", "vol_tank");
	});
	return result;
}

SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_FresnelPhysicalIph", "wspd");
	});
	return result;
}


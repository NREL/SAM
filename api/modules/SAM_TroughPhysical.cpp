#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_TroughPhysical.h"

SAM_EXPORT int SAM_TroughPhysical_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("trough_physical", data, verbosity, err);
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_T_in_loop_initial_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_in_loop_initial", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_T_out_loop_initial_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_out_loop_initial", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_T_out_scas_initial_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "T_out_scas_initial", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_T_tank_cold_init_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_tank_cold_init", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_T_tank_hot_init_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_tank_hot_init", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_defocus_initial_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "defocus_initial", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_disp_csu_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_csu_cost", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_disp_inventory_incentive_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_inventory_incentive", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_disp_rsu_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_rsu_cost", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_is_dispatch_targets_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_dispatch_targets", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_is_pc_sb_allowed_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "is_pc_sb_allowed_in", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_is_pc_su_allowed_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "is_pc_su_allowed_in", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_is_rec_su_allowed_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "is_rec_su_allowed_in", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_pc_op_mode_initial_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pc_op_mode_initial", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_pc_startup_energy_remain_initial_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pc_startup_energy_remain_initial", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_pc_startup_time_remain_init_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pc_startup_time_remain_init", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_q_pc_max_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "q_pc_max_in", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_q_pc_target_on_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "q_pc_target_on_in", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_q_pc_target_su_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "q_pc_target_su_in", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_rec_op_mode_initial_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_op_mode_initial", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_rec_qf_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_qf_delay", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_rec_su_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_su_delay", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sim_type", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_time_start_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_start", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_time_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_steps_per_hour", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_time_stop_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_stop", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SystemControl_vacuum_arrays_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "vacuum_arrays", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Weather_file_name_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "file_name", str);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Weather_solar_resource_data_tset(SAM_table ptr, SAM_table tab, SAM_error *err){
	SAM_table_set_table(ptr, "solar_resource_data", tab, err);
}



SAM_EXPORT void SAM_TroughPhysical_SolarField_A_aperture_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "A_aperture", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_AbsorberMaterial_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "AbsorberMaterial", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_AnnulusGas_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "AnnulusGas", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_Ave_Focal_Length_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Ave_Focal_Length", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_ColperSCA_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ColperSCA", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_D_2_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_2", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_D_3_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_3", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_D_4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_4", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_D_5_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_5", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_D_p_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_p", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_Design_loss_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Design_loss", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_Dirt_HCE_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Dirt_HCE", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_Dirt_mirror_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Dirt_mirror", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_Distance_SCA_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Distance_SCA", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_EPSILON_4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "EPSILON_4", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_EPSILON_5_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "EPSILON_5", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_Error_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Error", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_FieldConfig_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "FieldConfig", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_Flow_type_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Flow_type", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_Fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Fluid", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_GeomEffects_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "GeomEffects", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_GlazingIntactIn_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "GlazingIntactIn", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_HCE_FieldFrac_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "HCE_FieldFrac", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_HDR_rough_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HDR_rough", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_IAM_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "IAM_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_I_bn_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_bn_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_L_SCA_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "L_SCA", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_L_aperture_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "L_aperture", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_L_power_block_piping_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_power_block_piping", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_L_rnr_per_xpan_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_rnr_per_xpan", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_L_xpan_hdr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_xpan_hdr", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_L_xpan_rnr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_xpan_rnr", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_Min_rnr_xpans_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Min_rnr_xpans", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_N_hdr_per_xpan_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "N_hdr_per_xpan", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_N_max_hdr_diams_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "N_max_hdr_diams", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_P_a_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "P_a", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_Pipe_hl_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Pipe_hl_coef", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_Rho_mirror_clean_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Rho_mirror_clean", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_Rough_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Rough", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_Row_Distance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Row_Distance", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_SCA_drives_elec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SCA_drives_elec", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_Shadowing_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Shadowing", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_T_fp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_fp", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_T_loop_in_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_in_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_T_loop_out_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_out", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_T_shutdown_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_shutdown", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_T_startup_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_startup", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_Tau_envelope_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Tau_envelope", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_TrackingError_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "TrackingError", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_V_hdr_cold_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_cold_max", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_V_hdr_cold_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_cold_min", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_V_hdr_hot_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_hot_max", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_V_hdr_hot_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_hot_min", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_W_aperture_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "W_aperture", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_accept_init_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "accept_init", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_accept_loc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "accept_loc", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_accept_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "accept_mode", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_alpha_abs_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "alpha_abs", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_alpha_env_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "alpha_env", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_azimuth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "azimuth", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_calc_design_pipe_vals_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calc_design_pipe_vals", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_custom_sf_pipe_sizes_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "custom_sf_pipe_sizes", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_11_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_11", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_12_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_12", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_13_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_13", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_14_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_14", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_21_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_21", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_22_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_22", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_23_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_23", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_24_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_24", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_31_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_31", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_32_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_32", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_33_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_33", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_34_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_34", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_41_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_41", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_42_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_42", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_43_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_43", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_44_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_44", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_eta_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pump", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_f_htfmax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_htfmax", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_f_htfmin_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_htfmin", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "field_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_include_fixed_power_block_runner_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "include_fixed_power_block_runner", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_m_dot_htfmax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htfmax", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_m_dot_htfmin_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htfmin", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_mc_bal_cold_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_cold", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_mc_bal_hot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_hot", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_mc_bal_sca_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_sca", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_nColt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nColt", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_nHCEVar_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nHCEVar", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_nHCEt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nHCEt", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_northsouth_field_sep_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "northsouth_field_sep", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_offset_xpan_hdr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "offset_xpan_hdr", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_p_start_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "p_start", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_sf_hdr_diams_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sf_hdr_diams", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_sf_hdr_lengths_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sf_hdr_lengths", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_sf_hdr_wallthicks_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sf_hdr_wallthicks", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_sf_rnr_diams_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sf_rnr_diams", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_sf_rnr_lengths_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sf_rnr_lengths", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_sf_rnr_wallthicks_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sf_rnr_wallthicks", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_theta_dep_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_dep", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_theta_stow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_stow", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_tilt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tilt", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_use_abs_or_rel_mdot_limit_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "use_abs_or_rel_mdot_limit", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_SolarField_wind_stow_speed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wind_stow_speed", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_CT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_F_wc_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "F_wc", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_L_rnr_pb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_rnr_pb", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_P_boil_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_boil", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_P_cond_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_min", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_P_cond_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_ratio", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_P_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_ref", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ITD_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_T_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_approach", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_cycle_cutoff_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_cutoff_frac", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_cycle_max_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_max_frac", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_dT_cw_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_cw_ref", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_eta_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_ref", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_n_pl_inc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_pl_inc", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_pb_bd_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_bd_frac", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_pump_coef", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_pc_config_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pc_config", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_sby_frac", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_startup_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_frac", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_startup_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_time", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_tech_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tech_type", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_ud_f_W_dot_cool_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_f_W_dot_cool_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_ud_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_ud_is_sco2_regr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_is_sco2_regr", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Powerblock_ud_m_dot_water_cool_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_m_dot_water_cool_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_d_tank_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "d_tank_in", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_dt_hot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_hot", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_h_tank_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_in", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_min", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "init_hot_htf_percent", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_is_h_tank_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_h_tank_fixed", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_store_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "store_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_store_fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "store_fluid", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_pairs", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_cyl_piston_loss_poly_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "tes_cyl_piston_loss_poly", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_cyl_tank_cp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_cyl_tank_cp", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_cyl_tank_dens_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_cyl_tank_dens", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_cyl_tank_insul_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_cyl_tank_insul_percent", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_cyl_tank_thick_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_cyl_tank_thick", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_n_tsteps_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_n_tsteps", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_pb_T_charge_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_T_charge_min", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_pb_T_cold_delta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_T_cold_delta", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_pb_T_hot_delta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_T_hot_delta", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_pb_cp_solid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_cp_solid", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_pb_dens_solid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_dens_solid", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_pb_f_oversize_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_f_oversize", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_pb_k_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_k_eff", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_pb_n_xsteps_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_n_xsteps", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_pb_void_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_void_frac", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tes_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_type", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_tshours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tshours", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TES_u_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_DP_SGS_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "DP_SGS", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_T_tank_hot_inlet_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_tank_hot_inlet_min", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_V_tes_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_tes_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_custom_tes_p_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "custom_tes_p_loss", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_custom_tes_pipe_sizes_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "custom_tes_pipe_sizes", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_has_hot_tank_bypass_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "has_hot_tank_bypass", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_k_tes_loss_coeffs_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "k_tes_loss_coeffs", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_non_solar_field_land_area_multiplier_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "non_solar_field_land_area_multiplier", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_specified_solar_multiple_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "specified_solar_multiple", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_specified_total_aperture_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "specified_total_aperture", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_tanks_in_parallel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tanks_in_parallel", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_tes_diams_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "tes_diams", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_tes_lengths_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "tes_lengths", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_tes_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pump_coef", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_tes_wallthicks_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "tes_wallthicks", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_trough_loop_control_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "trough_loop_control", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Controller_use_solar_mult_or_aperture_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "use_solar_mult_or_aperture_area", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_ampl_data_dir_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "ampl_data_dir", str);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_ampl_exec_call_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "ampl_exec_call", str);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_can_cycle_use_standby_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "can_cycle_use_standby", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_disp_csu_cost_rel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_csu_cost_rel", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_disp_frequency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_frequency", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_disp_horizon_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_horizon", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_disp_max_iter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_max_iter", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_disp_mip_gap_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_mip_gap", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_disp_pen_delta_w_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_pen_delta_w", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_disp_pen_ramping_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_pen_ramping", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_disp_reporting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_reporting", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_disp_rsu_cost_rel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_rsu_cost_rel", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_disp_spec_bb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_bb", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_disp_spec_presolve_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_presolve", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_disp_spec_scaling_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_scaling", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_disp_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_steps_per_hour", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_disp_time_weighting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_time_weighting", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_disp_timeout_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_timeout", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_factors_ts", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_f_turb_tou_periods_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "f_turb_tou_periods", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_is_ampl_engine_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_ampl_engine", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_is_dispatch_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_dispatch", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_is_timestep_load_fractions_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_timestep_load_fractions", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_is_tod_pc_target_also_pc_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_tod_pc_target_also_pc_max", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_is_write_ampl_dat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_write_ampl_dat", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_multiplier_model", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_q_rec_heattrace_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_rec_heattrace", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_q_rec_standby_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_rec_standby", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_timestep_load_fractions_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "timestep_load_fractions", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekday_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Tou_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekend_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialModel_csp_financial_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp_financial_model", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialSolutionMode_ppa_soln_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_soln_mode", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_ElectricityRates_en_electricity_rates_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_electricity_rates", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TimeOfDeliveryFactors_dispatch_tod_factors_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_tod_factors", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Revenue_mp_energy_market_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "mp_energy_market_revenue", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_Revenue_ppa_price_input_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ppa_price_input", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_System_aux_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "aux_array", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_System_bop_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "bop_array", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysical_System_gross_net_conversion_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gross_net_conversion_factor", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_System_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_fixed_par", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_System_washing_frequency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "washing_frequency", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_System_water_usage_per_wash_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "water_usage_per_wash", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_TowerAndReceiver_piping_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_loss", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_bop_per_kwe_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.bop_per_kwe", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_contingency_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.contingency_percent", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_epc_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.epc.fixed", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_epc_per_acre_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.epc.per_acre", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_epc_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.epc.per_watt", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_epc_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.epc.percent", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_fossil_backup_cost_per_kwe_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.fossil_backup.cost_per_kwe", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_htf_system_cost_per_m2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.htf_system.cost_per_m2", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_plm_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.plm.fixed", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_plm_per_acre_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.plm.per_acre", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_plm_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.plm.per_watt", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_plm_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.plm.percent", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_power_plant_cost_per_kwe_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.power_plant.cost_per_kwe", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_sales_tax_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.sales_tax.percent", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_site_improvements_cost_per_m2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.site_improvements.cost_per_m2", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_solar_field_cost_per_m2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.solar_field.cost_per_m2", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_storage_cost_per_kwht_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.dtr.cost.storage.cost_per_kwht", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_CapitalCosts_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_rate", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate1", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate2", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate3", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate4", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate5", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months1", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months2", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months3", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months4", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months5", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent1", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent2", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent3", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent4", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent5", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate1", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate2", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate3", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate4", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate5", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_AdjustmentFactors_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_constant", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_AdjustmentFactors_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_periods", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_AdjustmentFactors_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_timeindex", number);
	});
}

SAM_EXPORT void SAM_TroughPhysical_AdjustmentFactors_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "adjust_periods", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysical_AdjustmentFactors_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "adjust_timeindex", arr, length);
	});
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_T_in_loop_initial_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_in_loop_initial", &result))
		make_access_error("SAM_TroughPhysical", "T_in_loop_initial");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_T_out_loop_initial_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_out_loop_initial", &result))
		make_access_error("SAM_TroughPhysical", "T_out_loop_initial");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SystemControl_T_out_scas_initial_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_out_scas_initial", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_out_scas_initial");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_T_tank_cold_init_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_tank_cold_init", &result))
		make_access_error("SAM_TroughPhysical", "T_tank_cold_init");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_T_tank_hot_init_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_tank_hot_init", &result))
		make_access_error("SAM_TroughPhysical", "T_tank_hot_init");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_defocus_initial_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "defocus_initial", &result))
		make_access_error("SAM_TroughPhysical", "defocus_initial");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_disp_csu_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_csu_cost", &result))
		make_access_error("SAM_TroughPhysical", "disp_csu_cost");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_disp_inventory_incentive_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_inventory_incentive", &result))
		make_access_error("SAM_TroughPhysical", "disp_inventory_incentive");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_disp_rsu_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_rsu_cost", &result))
		make_access_error("SAM_TroughPhysical", "disp_rsu_cost");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_is_dispatch_targets_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_dispatch_targets", &result))
		make_access_error("SAM_TroughPhysical", "is_dispatch_targets");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SystemControl_is_pc_sb_allowed_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_sb_allowed_in", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "is_pc_sb_allowed_in");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SystemControl_is_pc_su_allowed_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_su_allowed_in", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "is_pc_su_allowed_in");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SystemControl_is_rec_su_allowed_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_rec_su_allowed_in", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "is_rec_su_allowed_in");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_pc_op_mode_initial_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_op_mode_initial", &result))
		make_access_error("SAM_TroughPhysical", "pc_op_mode_initial");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_pc_startup_energy_remain_initial_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_startup_energy_remain_initial", &result))
		make_access_error("SAM_TroughPhysical", "pc_startup_energy_remain_initial");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_pc_startup_time_remain_init_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_startup_time_remain_init", &result))
		make_access_error("SAM_TroughPhysical", "pc_startup_time_remain_init");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SystemControl_q_pc_max_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pc_max_in", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_pc_max_in");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SystemControl_q_pc_target_on_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pc_target_on_in", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_pc_target_on_in");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SystemControl_q_pc_target_su_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pc_target_su_in", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_pc_target_su_in");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_rec_op_mode_initial_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_op_mode_initial", &result))
		make_access_error("SAM_TroughPhysical", "rec_op_mode_initial");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_rec_qf_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_qf_delay", &result))
		make_access_error("SAM_TroughPhysical", "rec_qf_delay");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_rec_su_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_su_delay", &result))
		make_access_error("SAM_TroughPhysical", "rec_su_delay");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_type", &result))
		make_access_error("SAM_TroughPhysical", "sim_type");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_time_start_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_start", &result))
		make_access_error("SAM_TroughPhysical", "time_start");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_time_steps_per_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_steps_per_hour", &result))
		make_access_error("SAM_TroughPhysical", "time_steps_per_hour");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_time_stop_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_stop", &result))
		make_access_error("SAM_TroughPhysical", "time_stop");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SystemControl_vacuum_arrays_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vacuum_arrays", &result))
		make_access_error("SAM_TroughPhysical", "vacuum_arrays");
	});
	return result;
}

SAM_EXPORT const char* SAM_TroughPhysical_Weather_file_name_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "file_name");
	if (!result)
		make_access_error("SAM_TroughPhysical", "file_name");
	});
	return result;
}

SAM_EXPORT SAM_table SAM_TroughPhysical_Weather_solar_resource_data_tget(SAM_table ptr, SAM_error *err){
	SAM_table result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_table(ptr, "solar_resource_data");
	if (!result)
		make_access_error("SAM_TroughPhysical", "solar_resource_data");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_A_aperture_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "A_aperture", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "A_aperture");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_AbsorberMaterial_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "AbsorberMaterial", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "AbsorberMaterial");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_AnnulusGas_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "AnnulusGas", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "AnnulusGas");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_Ave_Focal_Length_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Ave_Focal_Length", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "Ave_Focal_Length");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_ColperSCA_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ColperSCA", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "ColperSCA");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_D_2_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_2", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "D_2");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_D_3_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_3", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "D_3");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_D_4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_4", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "D_4");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_D_5_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_5", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "D_5");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_D_p_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_p", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "D_p");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_Design_loss_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Design_loss", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "Design_loss");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_Dirt_HCE_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Dirt_HCE", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "Dirt_HCE");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_Dirt_mirror_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Dirt_mirror", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "Dirt_mirror");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_Distance_SCA_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Distance_SCA", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "Distance_SCA");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_EPSILON_4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "EPSILON_4", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "EPSILON_4");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_EPSILON_5_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "EPSILON_5", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "EPSILON_5");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_Error_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Error", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "Error");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_FieldConfig_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "FieldConfig", &result))
		make_access_error("SAM_TroughPhysical", "FieldConfig");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_Flow_type_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Flow_type", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "Flow_type");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_Fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Fluid", &result))
		make_access_error("SAM_TroughPhysical", "Fluid");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_GeomEffects_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "GeomEffects", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "GeomEffects");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_GlazingIntactIn_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "GlazingIntactIn", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "GlazingIntactIn");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_HCE_FieldFrac_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "HCE_FieldFrac", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "HCE_FieldFrac");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_HDR_rough_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HDR_rough", &result))
		make_access_error("SAM_TroughPhysical", "HDR_rough");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_IAM_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "IAM_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "IAM_matrix");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_I_bn_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_bn_des", &result))
		make_access_error("SAM_TroughPhysical", "I_bn_des");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_L_SCA_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "L_SCA", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "L_SCA");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_L_aperture_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "L_aperture", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "L_aperture");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_L_power_block_piping_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_power_block_piping", &result))
		make_access_error("SAM_TroughPhysical", "L_power_block_piping");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_L_rnr_per_xpan_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_rnr_per_xpan", &result))
		make_access_error("SAM_TroughPhysical", "L_rnr_per_xpan");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_L_xpan_hdr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_xpan_hdr", &result))
		make_access_error("SAM_TroughPhysical", "L_xpan_hdr");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_L_xpan_rnr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_xpan_rnr", &result))
		make_access_error("SAM_TroughPhysical", "L_xpan_rnr");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_Min_rnr_xpans_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Min_rnr_xpans", &result))
		make_access_error("SAM_TroughPhysical", "Min_rnr_xpans");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_N_hdr_per_xpan_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_hdr_per_xpan", &result))
		make_access_error("SAM_TroughPhysical", "N_hdr_per_xpan");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_N_max_hdr_diams_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_max_hdr_diams", &result))
		make_access_error("SAM_TroughPhysical", "N_max_hdr_diams");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_P_a_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "P_a", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "P_a");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_Pipe_hl_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Pipe_hl_coef", &result))
		make_access_error("SAM_TroughPhysical", "Pipe_hl_coef");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_Rho_mirror_clean_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Rho_mirror_clean", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "Rho_mirror_clean");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_Rough_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Rough", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "Rough");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_Row_Distance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Row_Distance", &result))
		make_access_error("SAM_TroughPhysical", "Row_Distance");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_SCA_drives_elec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SCA_drives_elec", &result))
		make_access_error("SAM_TroughPhysical", "SCA_drives_elec");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_Shadowing_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Shadowing", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "Shadowing");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_T_fp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_fp", &result))
		make_access_error("SAM_TroughPhysical", "T_fp");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_T_loop_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_in_des", &result))
		make_access_error("SAM_TroughPhysical", "T_loop_in_des");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_T_loop_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_out", &result))
		make_access_error("SAM_TroughPhysical", "T_loop_out");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_T_shutdown_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_shutdown", &result))
		make_access_error("SAM_TroughPhysical", "T_shutdown");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_T_startup_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_startup", &result))
		make_access_error("SAM_TroughPhysical", "T_startup");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_Tau_envelope_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Tau_envelope", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "Tau_envelope");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_TrackingError_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "TrackingError", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "TrackingError");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_V_hdr_cold_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_cold_max", &result))
		make_access_error("SAM_TroughPhysical", "V_hdr_cold_max");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_V_hdr_cold_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_cold_min", &result))
		make_access_error("SAM_TroughPhysical", "V_hdr_cold_min");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_V_hdr_hot_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_hot_max", &result))
		make_access_error("SAM_TroughPhysical", "V_hdr_hot_max");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_V_hdr_hot_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_hot_min", &result))
		make_access_error("SAM_TroughPhysical", "V_hdr_hot_min");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_W_aperture_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_aperture", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "W_aperture");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_accept_init_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "accept_init", &result))
		make_access_error("SAM_TroughPhysical", "accept_init");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_accept_loc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "accept_loc", &result))
		make_access_error("SAM_TroughPhysical", "accept_loc");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_accept_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "accept_mode", &result))
		make_access_error("SAM_TroughPhysical", "accept_mode");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_alpha_abs_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "alpha_abs", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "alpha_abs");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_alpha_env_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "alpha_env", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "alpha_env");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_azimuth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "azimuth", &result))
		make_access_error("SAM_TroughPhysical", "azimuth");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_calc_design_pipe_vals_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calc_design_pipe_vals", &result))
		make_access_error("SAM_TroughPhysical", "calc_design_pipe_vals");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_custom_sf_pipe_sizes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "custom_sf_pipe_sizes", &result))
		make_access_error("SAM_TroughPhysical", "custom_sf_pipe_sizes");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_11_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_11", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_11");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_12_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_12", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_12");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_13_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_13", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_13");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_14_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_14", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_14");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_21_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_21", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_21");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_22_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_22", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_22");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_23_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_23", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_23");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_24_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_24", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_24");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_31_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_31", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_31");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_32_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_32", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_32");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_33_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_33", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_33");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_34_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_34", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_34");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_41_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_41", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_41");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_42_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_42", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_42");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_43_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_43", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_43");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_44_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_44", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "epsilon_3_44");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_eta_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pump", &result))
		make_access_error("SAM_TroughPhysical", "eta_pump");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_f_htfmax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_htfmax", &result))
		make_access_error("SAM_TroughPhysical", "f_htfmax");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_f_htfmin_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_htfmin", &result))
		make_access_error("SAM_TroughPhysical", "f_htfmin");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "field_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "field_fl_props");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_include_fixed_power_block_runner_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "include_fixed_power_block_runner", &result))
		make_access_error("SAM_TroughPhysical", "include_fixed_power_block_runner");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_m_dot_htfmax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmax", &result))
		make_access_error("SAM_TroughPhysical", "m_dot_htfmax");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_m_dot_htfmin_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmin", &result))
		make_access_error("SAM_TroughPhysical", "m_dot_htfmin");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_mc_bal_cold_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_cold", &result))
		make_access_error("SAM_TroughPhysical", "mc_bal_cold");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_mc_bal_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_hot", &result))
		make_access_error("SAM_TroughPhysical", "mc_bal_hot");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_mc_bal_sca_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_sca", &result))
		make_access_error("SAM_TroughPhysical", "mc_bal_sca");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_nColt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nColt", &result))
		make_access_error("SAM_TroughPhysical", "nColt");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_nHCEVar_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nHCEVar", &result))
		make_access_error("SAM_TroughPhysical", "nHCEVar");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_nHCEt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nHCEt", &result))
		make_access_error("SAM_TroughPhysical", "nHCEt");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_northsouth_field_sep_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "northsouth_field_sep", &result))
		make_access_error("SAM_TroughPhysical", "northsouth_field_sep");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_offset_xpan_hdr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "offset_xpan_hdr", &result))
		make_access_error("SAM_TroughPhysical", "offset_xpan_hdr");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_p_start_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "p_start", &result))
		make_access_error("SAM_TroughPhysical", "p_start");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_sf_hdr_diams_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sf_hdr_diams", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "sf_hdr_diams");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_sf_hdr_lengths_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sf_hdr_lengths", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "sf_hdr_lengths");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_sf_hdr_wallthicks_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sf_hdr_wallthicks", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "sf_hdr_wallthicks");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_sf_rnr_diams_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sf_rnr_diams", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "sf_rnr_diams");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_sf_rnr_lengths_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sf_rnr_lengths", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "sf_rnr_lengths");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_SolarField_sf_rnr_wallthicks_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sf_rnr_wallthicks", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "sf_rnr_wallthicks");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_theta_dep_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_dep", &result))
		make_access_error("SAM_TroughPhysical", "theta_dep");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_theta_stow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_stow", &result))
		make_access_error("SAM_TroughPhysical", "theta_stow");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_tilt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tilt", &result))
		make_access_error("SAM_TroughPhysical", "tilt");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_use_abs_or_rel_mdot_limit_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "use_abs_or_rel_mdot_limit", &result))
		make_access_error("SAM_TroughPhysical", "use_abs_or_rel_mdot_limit");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_SolarField_wind_stow_speed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wind_stow_speed", &result))
		make_access_error("SAM_TroughPhysical", "wind_stow_speed");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_CT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT", &result))
		make_access_error("SAM_TroughPhysical", "CT");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Powerblock_F_wc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "F_wc", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "F_wc");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_L_rnr_pb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_rnr_pb", &result))
		make_access_error("SAM_TroughPhysical", "L_rnr_pb");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_P_boil_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_boil", &result))
		make_access_error("SAM_TroughPhysical", "P_boil");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_P_cond_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_min", &result))
		make_access_error("SAM_TroughPhysical", "P_cond_min");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_P_cond_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_ratio", &result))
		make_access_error("SAM_TroughPhysical", "P_cond_ratio");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_P_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_ref", &result))
		make_access_error("SAM_TroughPhysical", "P_ref");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_T_ITD_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ITD_des", &result))
		make_access_error("SAM_TroughPhysical", "T_ITD_des");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_TroughPhysical", "T_amb_des");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_T_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_approach", &result))
		make_access_error("SAM_TroughPhysical", "T_approach");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_cycle_cutoff_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_cutoff_frac", &result))
		make_access_error("SAM_TroughPhysical", "cycle_cutoff_frac");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_cycle_max_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_max_frac", &result))
		make_access_error("SAM_TroughPhysical", "cycle_max_frac");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_dT_cw_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_cw_ref", &result))
		make_access_error("SAM_TroughPhysical", "dT_cw_ref");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_eta_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ref", &result))
		make_access_error("SAM_TroughPhysical", "eta_ref");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_n_pl_inc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_pl_inc", &result))
		make_access_error("SAM_TroughPhysical", "n_pl_inc");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_pb_bd_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_bd_frac", &result))
		make_access_error("SAM_TroughPhysical", "pb_bd_frac");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_pb_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_pump_coef", &result))
		make_access_error("SAM_TroughPhysical", "pb_pump_coef");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_pc_config_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_config", &result))
		make_access_error("SAM_TroughPhysical", "pc_config");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_q_sby_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_sby_frac", &result))
		make_access_error("SAM_TroughPhysical", "q_sby_frac");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_startup_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_frac", &result))
		make_access_error("SAM_TroughPhysical", "startup_frac");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_startup_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_time", &result))
		make_access_error("SAM_TroughPhysical", "startup_time");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_tech_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tech_type", &result))
		make_access_error("SAM_TroughPhysical", "tech_type");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_ud_f_W_dot_cool_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_f_W_dot_cool_des", &result))
		make_access_error("SAM_TroughPhysical", "ud_f_W_dot_cool_des");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Powerblock_ud_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "ud_ind_od");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_ud_is_sco2_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_is_sco2_regr", &result))
		make_access_error("SAM_TroughPhysical", "ud_is_sco2_regr");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Powerblock_ud_m_dot_water_cool_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_m_dot_water_cool_des", &result))
		make_access_error("SAM_TroughPhysical", "ud_m_dot_water_cool_des");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_Thtr", &result))
		make_access_error("SAM_TroughPhysical", "cold_tank_Thtr");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_max_heat", &result))
		make_access_error("SAM_TroughPhysical", "cold_tank_max_heat");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_d_tank_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_tank_in", &result))
		make_access_error("SAM_TroughPhysical", "d_tank_in");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_dt_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_hot", &result))
		make_access_error("SAM_TroughPhysical", "dt_hot");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_h_tank_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_in", &result))
		make_access_error("SAM_TroughPhysical", "h_tank_in");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_h_tank_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_min", &result))
		make_access_error("SAM_TroughPhysical", "h_tank_min");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_Thtr", &result))
		make_access_error("SAM_TroughPhysical", "hot_tank_Thtr");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_max_heat", &result))
		make_access_error("SAM_TroughPhysical", "hot_tank_max_heat");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "init_hot_htf_percent", &result))
		make_access_error("SAM_TroughPhysical", "init_hot_htf_percent");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_is_h_tank_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_h_tank_fixed", &result))
		make_access_error("SAM_TroughPhysical", "is_h_tank_fixed");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_TES_store_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "store_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "store_fl_props");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_store_fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "store_fluid", &result))
		make_access_error("SAM_TroughPhysical", "store_fluid");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tank_pairs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_pairs", &result))
		make_access_error("SAM_TroughPhysical", "tank_pairs");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_TES_tes_cyl_piston_loss_poly_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_cyl_piston_loss_poly", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_cyl_piston_loss_poly");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tes_cyl_tank_cp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_cyl_tank_cp", &result))
		make_access_error("SAM_TroughPhysical", "tes_cyl_tank_cp");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tes_cyl_tank_dens_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_cyl_tank_dens", &result))
		make_access_error("SAM_TroughPhysical", "tes_cyl_tank_dens");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tes_cyl_tank_insul_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_cyl_tank_insul_percent", &result))
		make_access_error("SAM_TroughPhysical", "tes_cyl_tank_insul_percent");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tes_cyl_tank_thick_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_cyl_tank_thick", &result))
		make_access_error("SAM_TroughPhysical", "tes_cyl_tank_thick");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tes_n_tsteps_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_n_tsteps", &result))
		make_access_error("SAM_TroughPhysical", "tes_n_tsteps");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tes_pb_T_charge_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_T_charge_min", &result))
		make_access_error("SAM_TroughPhysical", "tes_pb_T_charge_min");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tes_pb_T_cold_delta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_T_cold_delta", &result))
		make_access_error("SAM_TroughPhysical", "tes_pb_T_cold_delta");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tes_pb_T_hot_delta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_T_hot_delta", &result))
		make_access_error("SAM_TroughPhysical", "tes_pb_T_hot_delta");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tes_pb_cp_solid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_cp_solid", &result))
		make_access_error("SAM_TroughPhysical", "tes_pb_cp_solid");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tes_pb_dens_solid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_dens_solid", &result))
		make_access_error("SAM_TroughPhysical", "tes_pb_dens_solid");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tes_pb_f_oversize_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_f_oversize", &result))
		make_access_error("SAM_TroughPhysical", "tes_pb_f_oversize");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tes_pb_k_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_k_eff", &result))
		make_access_error("SAM_TroughPhysical", "tes_pb_k_eff");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tes_pb_n_xsteps_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_n_xsteps", &result))
		make_access_error("SAM_TroughPhysical", "tes_pb_n_xsteps");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tes_pb_void_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_void_frac", &result))
		make_access_error("SAM_TroughPhysical", "tes_pb_void_frac");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tes_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_type", &result))
		make_access_error("SAM_TroughPhysical", "tes_type");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_tshours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours", &result))
		make_access_error("SAM_TroughPhysical", "tshours");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TES_u_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_tank", &result))
		make_access_error("SAM_TroughPhysical", "u_tank");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Controller_DP_SGS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "DP_SGS", &result))
		make_access_error("SAM_TroughPhysical", "DP_SGS");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Controller_T_tank_hot_inlet_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_tank_hot_inlet_min", &result))
		make_access_error("SAM_TroughPhysical", "T_tank_hot_inlet_min");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Controller_V_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tes_des", &result))
		make_access_error("SAM_TroughPhysical", "V_tes_des");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Controller_custom_tes_p_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "custom_tes_p_loss", &result))
		make_access_error("SAM_TroughPhysical", "custom_tes_p_loss");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Controller_custom_tes_pipe_sizes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "custom_tes_pipe_sizes", &result))
		make_access_error("SAM_TroughPhysical", "custom_tes_pipe_sizes");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Controller_has_hot_tank_bypass_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "has_hot_tank_bypass", &result))
		make_access_error("SAM_TroughPhysical", "has_hot_tank_bypass");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Controller_k_tes_loss_coeffs_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "k_tes_loss_coeffs", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "k_tes_loss_coeffs");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Controller_non_solar_field_land_area_multiplier_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "non_solar_field_land_area_multiplier", &result))
		make_access_error("SAM_TroughPhysical", "non_solar_field_land_area_multiplier");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Controller_specified_solar_multiple_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "specified_solar_multiple", &result))
		make_access_error("SAM_TroughPhysical", "specified_solar_multiple");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Controller_specified_total_aperture_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "specified_total_aperture", &result))
		make_access_error("SAM_TroughPhysical", "specified_total_aperture");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Controller_tanks_in_parallel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tanks_in_parallel", &result))
		make_access_error("SAM_TroughPhysical", "tanks_in_parallel");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Controller_tes_diams_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "tes_diams", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_diams");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Controller_tes_lengths_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "tes_lengths", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_lengths");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Controller_tes_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pump_coef", &result))
		make_access_error("SAM_TroughPhysical", "tes_pump_coef");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Controller_tes_wallthicks_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "tes_wallthicks", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_wallthicks");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Controller_trough_loop_control_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "trough_loop_control", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "trough_loop_control");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Controller_use_solar_mult_or_aperture_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "use_solar_mult_or_aperture_area", &result))
		make_access_error("SAM_TroughPhysical", "use_solar_mult_or_aperture_area");
	});
	return result;
}

SAM_EXPORT const char* SAM_TroughPhysical_Tou_ampl_data_dir_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "ampl_data_dir");
	if (!result)
		make_access_error("SAM_TroughPhysical", "ampl_data_dir");
	});
	return result;
}

SAM_EXPORT const char* SAM_TroughPhysical_Tou_ampl_exec_call_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "ampl_exec_call");
	if (!result)
		make_access_error("SAM_TroughPhysical", "ampl_exec_call");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_can_cycle_use_standby_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "can_cycle_use_standby", &result))
		make_access_error("SAM_TroughPhysical", "can_cycle_use_standby");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_disp_csu_cost_rel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_csu_cost_rel", &result))
		make_access_error("SAM_TroughPhysical", "disp_csu_cost_rel");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_disp_frequency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_frequency", &result))
		make_access_error("SAM_TroughPhysical", "disp_frequency");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_disp_horizon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_horizon", &result))
		make_access_error("SAM_TroughPhysical", "disp_horizon");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_disp_max_iter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_max_iter", &result))
		make_access_error("SAM_TroughPhysical", "disp_max_iter");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_disp_mip_gap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_mip_gap", &result))
		make_access_error("SAM_TroughPhysical", "disp_mip_gap");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_disp_pen_delta_w_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_pen_delta_w", &result))
		make_access_error("SAM_TroughPhysical", "disp_pen_delta_w");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_disp_pen_ramping_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_pen_ramping", &result))
		make_access_error("SAM_TroughPhysical", "disp_pen_ramping");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_disp_reporting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_reporting", &result))
		make_access_error("SAM_TroughPhysical", "disp_reporting");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_disp_rsu_cost_rel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_rsu_cost_rel", &result))
		make_access_error("SAM_TroughPhysical", "disp_rsu_cost_rel");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_disp_spec_bb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_bb", &result))
		make_access_error("SAM_TroughPhysical", "disp_spec_bb");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_disp_spec_presolve_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_presolve", &result))
		make_access_error("SAM_TroughPhysical", "disp_spec_presolve");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_disp_spec_scaling_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_scaling", &result))
		make_access_error("SAM_TroughPhysical", "disp_spec_scaling");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_disp_steps_per_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_steps_per_hour", &result))
		make_access_error("SAM_TroughPhysical", "disp_steps_per_hour");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_disp_time_weighting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_time_weighting", &result))
		make_access_error("SAM_TroughPhysical", "disp_time_weighting");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_disp_timeout_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_timeout", &result))
		make_access_error("SAM_TroughPhysical", "disp_timeout");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Tou_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_factors_ts", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "dispatch_factors_ts");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Tou_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "dispatch_sched_weekday");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Tou_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "dispatch_sched_weekend");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Tou_f_turb_tou_periods_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "f_turb_tou_periods", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "f_turb_tou_periods");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_is_ampl_engine_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_ampl_engine", &result))
		make_access_error("SAM_TroughPhysical", "is_ampl_engine");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_is_dispatch_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_dispatch", &result))
		make_access_error("SAM_TroughPhysical", "is_dispatch");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_is_timestep_load_fractions_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_timestep_load_fractions", &result))
		make_access_error("SAM_TroughPhysical", "is_timestep_load_fractions");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_is_tod_pc_target_also_pc_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_tod_pc_target_also_pc_max", &result))
		make_access_error("SAM_TroughPhysical", "is_tod_pc_target_also_pc_max");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_is_write_ampl_dat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_write_ampl_dat", &result))
		make_access_error("SAM_TroughPhysical", "is_write_ampl_dat");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_multiplier_model", &result))
		make_access_error("SAM_TroughPhysical", "ppa_multiplier_model");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_q_rec_heattrace_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_rec_heattrace", &result))
		make_access_error("SAM_TroughPhysical", "q_rec_heattrace");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Tou_q_rec_standby_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_rec_standby", &result))
		make_access_error("SAM_TroughPhysical", "q_rec_standby");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Tou_timestep_load_fractions_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "timestep_load_fractions", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "timestep_load_fractions");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Tou_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekday_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "weekday_schedule");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Tou_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekend_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "weekend_schedule");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialModel_csp_financial_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp_financial_model", &result))
		make_access_error("SAM_TroughPhysical", "csp_financial_model");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialSolutionMode_ppa_soln_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_soln_mode", &result))
		make_access_error("SAM_TroughPhysical", "ppa_soln_mode");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_ElectricityRates_en_electricity_rates_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_electricity_rates", &result))
		make_access_error("SAM_TroughPhysical", "en_electricity_rates");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_TimeOfDeliveryFactors_dispatch_tod_factors_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_tod_factors", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "dispatch_tod_factors");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Revenue_mp_energy_market_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mp_energy_market_revenue", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "mp_energy_market_revenue");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Revenue_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ppa_price_input", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "ppa_price_input");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_System_aux_array_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "aux_array", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "aux_array");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_System_bop_array_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "bop_array", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "bop_array");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_System_gross_net_conversion_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gross_net_conversion_factor", &result))
		make_access_error("SAM_TroughPhysical", "gross_net_conversion_factor");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_System_pb_fixed_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_fixed_par", &result))
		make_access_error("SAM_TroughPhysical", "pb_fixed_par");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_System_washing_frequency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "washing_frequency", &result))
		make_access_error("SAM_TroughPhysical", "washing_frequency");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_System_water_usage_per_wash_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "water_usage_per_wash", &result))
		make_access_error("SAM_TroughPhysical", "water_usage_per_wash");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_TowerAndReceiver_piping_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_loss", &result))
		make_access_error("SAM_TroughPhysical", "piping_loss");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_bop_per_kwe_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.bop_per_kwe", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.bop_per_kwe");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_contingency_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.contingency_percent", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.contingency_percent");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_epc_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.epc.fixed", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.epc.fixed");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_epc_per_acre_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.epc.per_acre", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.epc.per_acre");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_epc_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.epc.per_watt", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.epc.per_watt");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_epc_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.epc.percent", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.epc.percent");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_fossil_backup_cost_per_kwe_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.fossil_backup.cost_per_kwe", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.fossil_backup.cost_per_kwe");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_htf_system_cost_per_m2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.htf_system.cost_per_m2", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.htf_system.cost_per_m2");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_plm_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.plm.fixed", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.plm.fixed");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_plm_per_acre_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.plm.per_acre", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.plm.per_acre");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_plm_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.plm.per_watt", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.plm.per_watt");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_plm_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.plm.percent", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.plm.percent");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_power_plant_cost_per_kwe_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.power_plant.cost_per_kwe", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.power_plant.cost_per_kwe");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_sales_tax_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.sales_tax.percent", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.sales_tax.percent");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_site_improvements_cost_per_m2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.site_improvements.cost_per_m2", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.site_improvements.cost_per_m2");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_solar_field_cost_per_m2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.solar_field.cost_per_m2", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.solar_field.cost_per_m2");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_csp_dtr_cost_storage_cost_per_kwht_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.storage.cost_per_kwht", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.storage.cost_per_kwht");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_CapitalCosts_sales_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_rate", &result))
		make_access_error("SAM_TroughPhysical", "sales_tax_rate");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate1", &result))
		make_access_error("SAM_TroughPhysical", "const_per_interest_rate1");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate2", &result))
		make_access_error("SAM_TroughPhysical", "const_per_interest_rate2");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate3", &result))
		make_access_error("SAM_TroughPhysical", "const_per_interest_rate3");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate4", &result))
		make_access_error("SAM_TroughPhysical", "const_per_interest_rate4");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate5", &result))
		make_access_error("SAM_TroughPhysical", "const_per_interest_rate5");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months1", &result))
		make_access_error("SAM_TroughPhysical", "const_per_months1");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months2", &result))
		make_access_error("SAM_TroughPhysical", "const_per_months2");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months3", &result))
		make_access_error("SAM_TroughPhysical", "const_per_months3");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months4", &result))
		make_access_error("SAM_TroughPhysical", "const_per_months4");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months5", &result))
		make_access_error("SAM_TroughPhysical", "const_per_months5");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent1", &result))
		make_access_error("SAM_TroughPhysical", "const_per_percent1");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent2", &result))
		make_access_error("SAM_TroughPhysical", "const_per_percent2");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent3", &result))
		make_access_error("SAM_TroughPhysical", "const_per_percent3");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent4", &result))
		make_access_error("SAM_TroughPhysical", "const_per_percent4");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent5", &result))
		make_access_error("SAM_TroughPhysical", "const_per_percent5");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate1", &result))
		make_access_error("SAM_TroughPhysical", "const_per_upfront_rate1");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate2", &result))
		make_access_error("SAM_TroughPhysical", "const_per_upfront_rate2");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate3", &result))
		make_access_error("SAM_TroughPhysical", "const_per_upfront_rate3");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate4", &result))
		make_access_error("SAM_TroughPhysical", "const_per_upfront_rate4");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate5", &result))
		make_access_error("SAM_TroughPhysical", "const_per_upfront_rate5");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_AdjustmentFactors_adjust_constant_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_constant", &result))
		make_access_error("SAM_TroughPhysical", "adjust_constant");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_AdjustmentFactors_adjust_en_periods_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_periods", &result))
		make_access_error("SAM_TroughPhysical", "adjust_en_periods");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_AdjustmentFactors_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_timeindex", &result))
		make_access_error("SAM_TroughPhysical", "adjust_en_timeindex");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_AdjustmentFactors_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "adjust_periods", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "adjust_periods");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_AdjustmentFactors_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "adjust_timeindex", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "adjust_timeindex");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_CosTh_ave_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "CosTh_ave", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "CosTh_ave");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_D_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_cpnt", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "D_cpnt");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_EndLoss_ave_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EndLoss_ave", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "EndLoss_ave");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_EqOpteff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EqOpteff", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "EqOpteff");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_IAM_ave_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "IAM_ave", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "IAM_ave");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_K_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "K_cpnt", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "K_cpnt");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_L_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "L_cpnt", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "L_cpnt");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_P_cooling_tower_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cooling_tower_tot", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "P_cooling_tower_tot");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_P_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cycle", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "P_cycle");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_P_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_fixed", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "P_fixed");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_P_out_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_out_net", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "P_out_net");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_P_plant_balance_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_plant_balance_tot", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "P_plant_balance_tot");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_RowShadow_ave_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "RowShadow_ave", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "RowShadow_ave");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_SCADefocusArray_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "SCADefocusArray", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "SCADefocusArray");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_SCAInfoArray_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "SCAInfoArray", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "SCAInfoArray");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_SCAs_def_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "SCAs_def", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "SCAs_def");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_field_cold_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_field_cold_in", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_field_cold_in");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_field_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_field_hot_out", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_field_hot_out");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_grad_0_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_grad_0", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_grad_0");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_grad_1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_grad_1", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_grad_1");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_grad_2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_grad_2", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_grad_2");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_grad_3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_grad_3", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_grad_3");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_grad_4_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_grad_4", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_grad_4");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_grad_5_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_grad_5", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_grad_5");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_grad_6_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_grad_6", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_grad_6");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_grad_7_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_grad_7", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_grad_7");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_grad_8_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_grad_8", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_grad_8");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_grad_9_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_grad_9", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_grad_9");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_in_loop_final_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_in_loop_final", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_in_loop_final");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_out_loop_final_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_out_loop_final", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_out_loop_final");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_out_scas_last_final_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_out_scas_last_final", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_out_scas_last_final");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_pc_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pc_in", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_pc_in");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_pc_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pc_out", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_pc_out");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_rec_cold_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_cold_in", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_rec_cold_in");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_rec_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_hot_out", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_rec_hot_out");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_cold", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_hot", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "T_tes_hot");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_Theta_ave_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Theta_ave", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "Theta_ave");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_Type_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Type_cpnt", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "Type_cpnt");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_V_tank_hot_ini_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tank_hot_ini", &result))
		make_access_error("SAM_TroughPhysical", "V_tank_hot_ini");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_W_dot_field_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_field_pump", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "W_dot_field_pump");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_W_dot_pump_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_pump_SS", &result))
		make_access_error("SAM_TroughPhysical", "W_dot_pump_SS");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_W_dot_sca_track_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_sca_track", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "W_dot_sca_track");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_annual_W_cycle_gross_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_W_cycle_gross", &result))
		make_access_error("SAM_TroughPhysical", "annual_W_cycle_gross");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_TroughPhysical", "annual_energy");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_annual_field_freeze_protection_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_field_freeze_protection", &result))
		make_access_error("SAM_TroughPhysical", "annual_field_freeze_protection");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_annual_tes_freeze_protection_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_tes_freeze_protection", &result))
		make_access_error("SAM_TroughPhysical", "annual_tes_freeze_protection");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_annual_thermal_consumption_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_thermal_consumption", &result))
		make_access_error("SAM_TroughPhysical", "annual_thermal_consumption");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_annual_total_water_use_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_total_water_use", &result))
		make_access_error("SAM_TroughPhysical", "annual_total_water_use");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_aux_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_design", &result))
		make_access_error("SAM_TroughPhysical", "aux_design");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_avg_suboptimal_rel_mip_gap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "avg_suboptimal_rel_mip_gap", &result))
		make_access_error("SAM_TroughPhysical", "avg_suboptimal_rel_mip_gap");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "beam");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_bop_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_design", &result))
		make_access_error("SAM_TroughPhysical", "bop_design");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_TroughPhysical", "capacity_factor");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_interest1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest1", &result))
		make_access_error("SAM_TroughPhysical", "const_per_interest1");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_interest2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest2", &result))
		make_access_error("SAM_TroughPhysical", "const_per_interest2");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_interest3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest3", &result))
		make_access_error("SAM_TroughPhysical", "const_per_interest3");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_interest4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest4", &result))
		make_access_error("SAM_TroughPhysical", "const_per_interest4");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_interest5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest5", &result))
		make_access_error("SAM_TroughPhysical", "const_per_interest5");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_interest_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_total", &result))
		make_access_error("SAM_TroughPhysical", "const_per_interest_total");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_percent_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent_total", &result))
		make_access_error("SAM_TroughPhysical", "const_per_percent_total");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_principal1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal1", &result))
		make_access_error("SAM_TroughPhysical", "const_per_principal1");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_principal2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal2", &result))
		make_access_error("SAM_TroughPhysical", "const_per_principal2");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_principal3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal3", &result))
		make_access_error("SAM_TroughPhysical", "const_per_principal3");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_principal4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal4", &result))
		make_access_error("SAM_TroughPhysical", "const_per_principal4");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_principal5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal5", &result))
		make_access_error("SAM_TroughPhysical", "const_per_principal5");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_principal_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal_total", &result))
		make_access_error("SAM_TroughPhysical", "const_per_principal_total");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_total1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total1", &result))
		make_access_error("SAM_TroughPhysical", "const_per_total1");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_total2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total2", &result))
		make_access_error("SAM_TroughPhysical", "const_per_total2");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_total3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total3", &result))
		make_access_error("SAM_TroughPhysical", "const_per_total3");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_total4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total4", &result))
		make_access_error("SAM_TroughPhysical", "const_per_total4");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_const_per_total5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total5", &result))
		make_access_error("SAM_TroughPhysical", "const_per_total5");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_financing_cost", &result))
		make_access_error("SAM_TroughPhysical", "construction_financing_cost");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_conversion_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conversion_factor", &result))
		make_access_error("SAM_TroughPhysical", "conversion_factor");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_cp_battery_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cp_battery_nameplate", &result))
		make_access_error("SAM_TroughPhysical", "cp_battery_nameplate");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_cp_system_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cp_system_nameplate", &result))
		make_access_error("SAM_TroughPhysical", "cp_system_nameplate");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_cost_bop_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.bop", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.bop");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_cost_contingency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.contingency", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.contingency");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_cost_epc_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.epc.total", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.epc.total");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_cost_fossil_backup_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.fossil_backup", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.fossil_backup");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_cost_htf_system_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.htf_system", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.htf_system");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_cost_installed_per_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.installed_per_capacity", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.installed_per_capacity");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_cost_plm_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.plm.total", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.plm.total");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_cost_power_plant_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.power_plant", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.power_plant");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_cost_sales_tax_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.sales_tax.total", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.sales_tax.total");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_cost_site_improvements_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.site_improvements", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.site_improvements");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_cost_solar_field_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.solar_field", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.solar_field");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_cost_storage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.dtr.cost.storage", &result))
		make_access_error("SAM_TroughPhysical", "csp.dtr.cost.storage");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_csp_dtr_hce_design_heat_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "csp_dtr_hce_design_heat_losses", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "csp_dtr_hce_design_heat_losses");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_csp_dtr_hce_optical_effs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "csp_dtr_hce_optical_effs", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "csp_dtr_hce_optical_effs");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_loop_hce_heat_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp_dtr_loop_hce_heat_loss", &result))
		make_access_error("SAM_TroughPhysical", "csp_dtr_loop_hce_heat_loss");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_csp_dtr_sca_ap_lengths_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "csp_dtr_sca_ap_lengths", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "csp_dtr_sca_ap_lengths");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_sca_calc_costh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp_dtr_sca_calc_costh", &result))
		make_access_error("SAM_TroughPhysical", "csp_dtr_sca_calc_costh");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_csp_dtr_sca_calc_end_gains_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "csp_dtr_sca_calc_end_gains", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "csp_dtr_sca_calc_end_gains");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_csp_dtr_sca_calc_end_losses_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "csp_dtr_sca_calc_end_losses", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "csp_dtr_sca_calc_end_losses");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_csp_dtr_sca_calc_iams_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "csp_dtr_sca_calc_iams", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "csp_dtr_sca_calc_iams");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_sca_calc_latitude_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp_dtr_sca_calc_latitude", &result))
		make_access_error("SAM_TroughPhysical", "csp_dtr_sca_calc_latitude");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_csp_dtr_sca_calc_sca_effs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "csp_dtr_sca_calc_sca_effs", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "csp_dtr_sca_calc_sca_effs");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_sca_calc_theta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp_dtr_sca_calc_theta", &result))
		make_access_error("SAM_TroughPhysical", "csp_dtr_sca_calc_theta");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_dtr_sca_calc_zenith_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp_dtr_sca_calc_zenith", &result))
		make_access_error("SAM_TroughPhysical", "csp_dtr_sca_calc_zenith");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_pt_tes_htf_density_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp_pt_tes_htf_density", &result))
		make_access_error("SAM_TroughPhysical", "csp_pt_tes_htf_density");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_pt_tes_tank_diameter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp_pt_tes_tank_diameter", &result))
		make_access_error("SAM_TroughPhysical", "csp_pt_tes_tank_diameter");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_csp_pt_tes_tank_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp_pt_tes_tank_height", &result))
		make_access_error("SAM_TroughPhysical", "csp_pt_tes_tank_height");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_cycle_Tdb_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "cycle_Tdb_table", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "cycle_Tdb_table");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_cycle_eff_load_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "cycle_eff_load_table", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysical", "cycle_eff_load_table");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_cycle_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cycle_htf_pump_power", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "cycle_htf_pump_power");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_dP_sf_SS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dP_sf_SS", &result))
		make_access_error("SAM_TroughPhysical", "dP_sf_SS");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_defocus_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "defocus", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "defocus");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_defocus_final_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "defocus_final", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "defocus_final");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_deltaP_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "deltaP_field", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "deltaP_field");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_direct_subtotal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "direct_subtotal", &result))
		make_access_error("SAM_TroughPhysical", "direct_subtotal");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_obj_relax_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_obj_relax", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_obj_relax");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_objective_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_objective", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_objective");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_pceff_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_pceff_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_pceff_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_presolve_nconstr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_presolve_nconstr", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_presolve_nconstr");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_presolve_nvar_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_presolve_nvar", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_presolve_nvar");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_qpbsu_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qpbsu_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_qpbsu_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_qsf_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsf_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_qsf_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_qsfprod_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsfprod_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_qsfprod_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_qsfsu_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsfsu_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_qsfsu_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_rel_mip_gap_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_rel_mip_gap", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_rel_mip_gap");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_rev_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_rev_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_rev_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_solve_iter_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_iter", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_solve_iter");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_solve_state_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_state", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_solve_state");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_solve_time_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_time", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_solve_time");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_subopt_flag_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_subopt_flag", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_subopt_flag");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_tes_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_tes_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_tes_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_thermeff_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_thermeff_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_thermeff_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_wpb_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_wpb_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "disp_wpb_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_dni_costh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dni_costh", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "dni_costh");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_ch_tes", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "e_ch_tes");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_e_dot_field_int_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_dot_field_int_energy", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "e_dot_field_int_energy");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_eta_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "eta");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_f_htfmax_actual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_htfmax_actual", &result))
		make_access_error("SAM_TroughPhysical", "f_htfmax_actual");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_f_htfmin_actual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_htfmin_actual", &result))
		make_access_error("SAM_TroughPhysical", "f_htfmin_actual");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_field_htf_cp_avg_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_htf_cp_avg_des", &result))
		make_access_error("SAM_TroughPhysical", "field_htf_cp_avg_des");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_field_htf_max_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_htf_max_temp", &result))
		make_access_error("SAM_TroughPhysical", "field_htf_max_temp");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_field_htf_min_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_htf_min_temp", &result))
		make_access_error("SAM_TroughPhysical", "field_htf_min_temp");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_field_thermal_output_actual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_thermal_output_actual", &result))
		make_access_error("SAM_TroughPhysical", "field_thermal_output_actual");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_field_thermal_output_ideal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_thermal_output_ideal", &result))
		make_access_error("SAM_TroughPhysical", "field_thermal_output_ideal");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_fixed_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fixed_land_area", &result))
		make_access_error("SAM_TroughPhysical", "fixed_land_area");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "gen");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_hot_tank_htf_percent_final_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hot_tank_htf_percent_final", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "hot_tank_htf_percent_final");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_hour_day_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour_day", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "hour_day");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_is_hx_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_hx", &result))
		make_access_error("SAM_TroughPhysical", "is_hx");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_is_pc_sb_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_sb_allowed", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "is_pc_sb_allowed");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_is_pc_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_su_allowed", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "is_pc_su_allowed");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_is_rec_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_rec_su_allowed", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "is_rec_su_allowed");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_TroughPhysical", "kwh_per_kw");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_loop_optical_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loop_optical_efficiency", &result))
		make_access_error("SAM_TroughPhysical", "loop_optical_efficiency");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_balance", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "m_dot_balance");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_cold_tank_to_hot_tank_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_cold_tank_to_hot_tank", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "m_dot_cold_tank_to_hot_tank");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_cr_to_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_cr_to_tes_hot", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "m_dot_cr_to_tes_hot");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_cycle_to_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_cycle_to_field", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "m_dot_cycle_to_field");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_field_delivered_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_field_delivered", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "m_dot_field_delivered");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_field_recirc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_field_recirc", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "m_dot_field_recirc");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_field_to_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_field_to_cycle", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "m_dot_field_to_cycle");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_m_dot_htf_cycle_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_cycle_des", &result))
		make_access_error("SAM_TroughPhysical", "m_dot_htf_cycle_des");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_m_dot_htfmax_actual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmax_actual", &result))
		make_access_error("SAM_TroughPhysical", "m_dot_htfmax_actual");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_m_dot_htfmin_actual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmin_actual", &result))
		make_access_error("SAM_TroughPhysical", "m_dot_htfmin_actual");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_loop_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_loop", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "m_dot_loop");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_pc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pc", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "m_dot_pc");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_pc_to_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pc_to_tes_cold", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "m_dot_pc_to_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_tes_cold_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_cold_out", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "m_dot_tes_cold_out");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_tes_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_hot_out", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "m_dot_tes_hot_out");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_water_pc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_water_pc", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "m_dot_water_pc");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_mass_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_cold", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "mass_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_mass_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_hot", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "mass_tes_hot");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_max_field_flow_velocity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "max_field_flow_velocity", &result))
		make_access_error("SAM_TroughPhysical", "max_field_flow_velocity");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_max_loop_flow_vel_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "max_loop_flow_vel_des", &result))
		make_access_error("SAM_TroughPhysical", "max_loop_flow_vel_des");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_min_field_flow_velocity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "min_field_flow_velocity", &result))
		make_access_error("SAM_TroughPhysical", "min_field_flow_velocity");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_min_inner_diameter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "min_inner_diameter", &result))
		make_access_error("SAM_TroughPhysical", "min_inner_diameter");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_min_loop_flow_vel_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "min_loop_flow_vel_des", &result))
		make_access_error("SAM_TroughPhysical", "min_loop_flow_vel_des");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "month");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "monthly_energy");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_nLoops_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nLoops", &result))
		make_access_error("SAM_TroughPhysical", "nLoops");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_nSCA_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nSCA", &result))
		make_access_error("SAM_TroughPhysical", "nSCA");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "n_op_modes", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "n_op_modes");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nameplate", &result))
		make_access_error("SAM_TroughPhysical", "nameplate");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_1", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "op_mode_1");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_2", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "op_mode_2");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_3", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "op_mode_3");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_operating_modes_a_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_a", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "operating_modes_a");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_operating_modes_b_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_b", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "operating_modes_b");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_operating_modes_c_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_c", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "operating_modes_c");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pc_op_mode_final_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_op_mode_final", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pc_op_mode_final");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pc_startup_energy_remain_final_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_startup_energy_remain_final", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pc_startup_energy_remain_final");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pc_startup_time_remain_final_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_startup_time_remain_final", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pc_startup_time_remain_final");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_P_dsn", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_header_P_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_T_dsn", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_header_T_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_diams_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_diams", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_header_diams");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_expansions_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_expansions", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_header_expansions");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_lengths_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_lengths", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_header_lengths");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_mdot_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_mdot_dsn", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_header_mdot_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_vel_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_vel_dsn", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_header_vel_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_wallthk_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_wallthk", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_header_wallthk");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_loop_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_loop_P_dsn", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_loop_P_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_loop_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_loop_T_dsn", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_loop_T_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_P_dsn", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_runner_P_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_T_dsn", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_runner_T_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_diams_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_diams", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_runner_diams");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_expansions_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_expansions", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_runner_expansions");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_lengths_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_lengths", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_runner_lengths");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_mdot_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_mdot_dsn", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_runner_mdot_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_vel_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_vel_dsn", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_runner_vel_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_wallthk_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_wallthk", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_runner_wallthk");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_tes_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_P_dsn", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_tes_P_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_tes_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_T_dsn", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_tes_T_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_tes_diams_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_diams", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_tes_diams");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_tes_lengths_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_lengths", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_tes_lengths");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_tes_mdot_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_mdot_dsn", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_tes_mdot_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_tes_vel_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_vel_dsn", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_tes_vel_dsn");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_tes_wallthk_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_tes_wallthk", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pipe_tes_wallthk");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pres_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pres", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pres");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_pricing_mult_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pricing_mult", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "pricing_mult");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_balance", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_balance");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_ch_tes", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_ch_tes");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dc_tes", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dc_tes");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_q_dot_cycle_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_cycle_des", &result))
		make_access_error("SAM_TroughPhysical", "q_dot_cycle_des");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_est_cr_on_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_cr_on", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dot_est_cr_on");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_est_cr_su_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_cr_su", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dot_est_cr_su");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_est_tes_ch_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_tes_ch", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dot_est_tes_ch");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_est_tes_dc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_tes_dc", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dot_est_tes_dc");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_freeze_prot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_freeze_prot", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dot_freeze_prot");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_htf_sf_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_htf_sf_out", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dot_htf_sf_out");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_pc_max_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_max", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dot_pc_max");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_pc_min_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_min", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dot_pc_min");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_pc_sb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_sb", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dot_pc_sb");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_startup", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dot_pc_startup");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_pc_target_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_target", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dot_pc_target");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_piping_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_piping_loss", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dot_piping_loss");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_rec_abs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_abs", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dot_rec_abs");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_rec_inc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_inc", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dot_rec_inc");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_rec_thermal_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_thermal_loss", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_dot_rec_thermal_loss");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_q_dot_tes_est_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_tes_est", &result))
		make_access_error("SAM_TroughPhysical", "q_dot_tes_est");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_inc_sf_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_inc_sf_tot", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_inc_sf_tot");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_pb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pb", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_pb");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pc_startup", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_pc_startup");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_q_tes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_tes", &result))
		make_access_error("SAM_TroughPhysical", "q_tes");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_tes_heater_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_tes_heater", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "q_tes_heater");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_qinc_costh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "qinc_costh", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "qinc_costh");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_rec_op_mode_final_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rec_op_mode_final", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "rec_op_mode_final");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_recirculating_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "recirculating", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "recirculating");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_required_number_of_loops_for_SM1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "required_number_of_loops_for_SM1", &result))
		make_access_error("SAM_TroughPhysical", "required_number_of_loops_for_SM1");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_rh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rh", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "rh");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_sim_duration_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_duration", &result))
		make_access_error("SAM_TroughPhysical", "sim_duration");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_single_loop_aperture_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "single_loop_aperture", &result))
		make_access_error("SAM_TroughPhysical", "single_loop_aperture");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_solar_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_mult", &result))
		make_access_error("SAM_TroughPhysical", "solar_mult");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_solazi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solazi", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "solazi");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solzen", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "solzen");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_TroughPhysical", "system_capacity");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tank_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tank_losses", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tank_losses");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tdry");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tes_SA_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_SA_cold", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_SA_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tes_SA_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_SA_hot", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_SA_hot");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tes_SA_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_SA_tot", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_SA_tot");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_tes_avail_vol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_avail_vol", &result))
		make_access_error("SAM_TroughPhysical", "tes_avail_vol");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tes_cold_vol_frac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_cold_vol_frac", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_cold_vol_frac");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tes_error_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_error", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_error");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tes_error_corrected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_error_corrected", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_error_corrected");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tes_error_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_error_percent", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_error_percent");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_tes_htf_avg_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_htf_avg_temp", &result))
		make_access_error("SAM_TroughPhysical", "tes_htf_avg_temp");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_tes_htf_max_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_htf_max_temp", &result))
		make_access_error("SAM_TroughPhysical", "tes_htf_max_temp");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_tes_htf_min_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_htf_min_temp", &result))
		make_access_error("SAM_TroughPhysical", "tes_htf_min_temp");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tes_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_htf_pump_power", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_htf_pump_power");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tes_leak_error_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_leak_error", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_leak_error");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tes_mass_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_mass_tot", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_mass_tot");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tes_piston_frac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_piston_frac", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_piston_frac");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tes_piston_loc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_piston_loc", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_piston_loc");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tes_wall_error_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_wall_error", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tes_wall_error");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "time_hr", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "time_hr");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_total_aperture_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_aperture", &result))
		make_access_error("SAM_TroughPhysical", "total_aperture");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_total_direct_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_direct_cost", &result))
		make_access_error("SAM_TroughPhysical", "total_direct_cost");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_total_indirect_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_indirect_cost", &result))
		make_access_error("SAM_TroughPhysical", "total_indirect_cost");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_TroughPhysical", "total_installed_cost");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_total_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_land_area", &result))
		make_access_error("SAM_TroughPhysical", "total_land_area");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_total_loop_conversion_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_loop_conversion_efficiency", &result))
		make_access_error("SAM_TroughPhysical", "total_loop_conversion_efficiency");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_total_required_aperture_for_SM1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_required_aperture_for_SM1", &result))
		make_access_error("SAM_TroughPhysical", "total_required_aperture_for_SM1");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_total_tracking_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_tracking_power", &result))
		make_access_error("SAM_TroughPhysical", "total_tracking_power");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_tou_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tou_value", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "tou_value");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "twet");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_vel_loop_max_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "vel_loop_max", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "vel_loop_max");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_vel_loop_min_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "vel_loop_min", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "vel_loop_min");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_vol_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vol_min", &result))
		make_access_error("SAM_TroughPhysical", "vol_min");
	});
	return result;
}

SAM_EXPORT double SAM_TroughPhysical_Outputs_vol_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vol_tank", &result))
		make_access_error("SAM_TroughPhysical", "vol_tank");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_vol_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "vol_tes_cold", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "vol_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_vol_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "vol_tes_hot", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "vol_tes_hot");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_vol_tes_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "vol_tes_tot", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "vol_tes_tot");
	});
	return result;
}

SAM_EXPORT double* SAM_TroughPhysical_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_TroughPhysical", "wspd");
	});
	return result;
}


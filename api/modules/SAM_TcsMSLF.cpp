#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_TcsMSLF.h"

SAM_EXPORT SAM_TcsMSLF SAM_TcsMSLF_construct(const char* def, SAM_error* err){
	SAM_TcsMSLF result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_TcsMSLF_execute(SAM_TcsMSLF data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("tcsMSLF", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_TcsMSLF_destruct(SAM_TcsMSLF system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_TcsMSLF_Weather_azimuth_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "azimuth", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Weather_file_name_sset(SAM_TcsMSLF ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "file_name", str);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Weather_tilt_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tilt", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Weather_track_mode_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "track_mode", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Mslf_system_capacity_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_TouTranslator_weekday_schedule_mset(SAM_TcsMSLF ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekday_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsMSLF_TouTranslator_weekend_schedule_mset(SAM_TcsMSLF ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekend_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_A_aperture_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "A_aperture", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_AbsorberMaterial_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "AbsorberMaterial", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_AnnulusGas_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "AnnulusGas", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_ColAz_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ColAz", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_DP_SGS_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "DP_SGS", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_DP_coefs_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "DP_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_DP_nominal_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "DP_nominal", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_D_abs_in_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "D_abs_in", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_D_abs_out_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "D_abs_out", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_D_glass_in_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "D_glass_in", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_D_glass_out_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "D_glass_out", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_D_plug_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "D_plug", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_Design_loss_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Design_loss", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_Dirt_mirror_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Dirt_mirror", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_Error_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Error", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_FieldConfig_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "FieldConfig", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_Flow_type_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Flow_type", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_Fluid_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Fluid", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_GeomEffects_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "GeomEffects", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_GlazingIntactIn_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "GlazingIntactIn", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_HCE_FieldFrac_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCE_FieldFrac", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_HDR_rough_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HDR_rough", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_HL_T_coefs_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HL_T_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_HL_w_coefs_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HL_w_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_IAM_L_coefs_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "IAM_L_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_IAM_T_coefs_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "IAM_T_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_I_b_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_b", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_I_bn_des_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_bn_des", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_L_crossover_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_crossover", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_L_mod_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_mod", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_L_mod_spacing_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_mod_spacing", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_OpticalTable_mset(SAM_TcsMSLF ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "OpticalTable", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_P_a_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "P_a", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_P_amb_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_amb", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_Pipe_hl_coef_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Pipe_hl_coef", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_Rough_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Rough", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_SCA_drives_elec_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SCA_drives_elec", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_Shadowing_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Shadowing", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_T_amb_sf_des_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_sf_des", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_T_cold_in_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_cold_in", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_T_db_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_db", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_T_dp_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_dp", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_T_field_in_des_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_field_in_des", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_T_field_ini_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_field_ini", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_T_field_out_des_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_field_out_des", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_T_fp_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_fp", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_T_loop_in_des_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_in_des", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_T_loop_out_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_out", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_T_set_aux_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_set_aux", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_T_startup_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_startup", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_T_tank_cold_ini_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_tank_cold_ini", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_T_tank_hot_ini_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_tank_hot_ini", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_T_tank_hot_inlet_min_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_tank_hot_inlet_min", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_Tau_envelope_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Tau_envelope", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_TrackingError_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TrackingError", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_V_hdr_max_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_max", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_V_hdr_min_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_min", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_V_tank_hot_ini_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_tank_hot_ini", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_V_tes_des_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_tes_des", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_V_wind_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_wind", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_V_wind_des_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_wind_des", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_W_pb_design_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "W_pb_design", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_alpha_abs_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "alpha_abs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_alpha_env_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "alpha_env", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_aux_array_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "aux_array", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_bop_array_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "bop_array", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_calc_design_pipe_vals_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calc_design_pipe_vals", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_cold_tank_Thtr_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_custom_sgs_pipe_sizes_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "custom_sgs_pipe_sizes", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_custom_tes_p_loss_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "custom_tes_p_loss", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_cycle_cutoff_frac_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_cutoff_frac", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_cycle_max_frac_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_max_frac", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_defocus_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "defocus", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_dirt_env_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dirt_env", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_dt_cold_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_cold", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_dt_hot_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_hot", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_epsilon_abs_1_mset(SAM_TcsMSLF ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_abs_1", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_epsilon_abs_2_mset(SAM_TcsMSLF ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_abs_2", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_epsilon_abs_3_mset(SAM_TcsMSLF ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_abs_3", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_epsilon_abs_4_mset(SAM_TcsMSLF ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_abs_4", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_epsilon_glass_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "epsilon_glass", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_eta_pump_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pump", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_f_tc_cold_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_tc_cold", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_fc_on_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fc_on", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_ffrac_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ffrac", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_field_fl_props_mset(SAM_TcsMSLF ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "field_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_field_fluid_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "field_fluid", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_fossil_mode_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fossil_mode", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_fthr_ok_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fthr_ok", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_fthrctrl_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fthrctrl", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_fthrok_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fthrok", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_h_tank_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_h_tank_min_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_min", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_has_hot_tank_bypass_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "has_hot_tank_bypass", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_hot_tank_Thtr_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_hx_config_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hx_config", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_is_hx_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_hx", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_k_tes_loss_coeffs_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "k_tes_loss_coeffs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_m_dot_htfmax_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htfmax", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_m_dot_htfmin_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htfmin", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_mc_bal_cold_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_cold", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_mc_bal_hot_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_hot", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_mc_bal_sca_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_sca", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_nLoops_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nLoops", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_nMod_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nMod", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_nRecVar_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nRecVar", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_nSCA_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nSCA", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_nodes_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nodes", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_opt_model_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_model", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_pb_fixed_par_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_fixed_par", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_pb_pump_coef_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_pump_coef", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_pb_rated_cap_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_rated_cap", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_q_max_aux_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_max_aux", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_q_pb_design_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_pb_design", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_rec_htf_vol_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_htf_vol", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_rec_model_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_model", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_reflectivity_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "reflectivity", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_sgs_diams_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sgs_diams", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_sgs_lengths_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sgs_lengths", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_sgs_wallthicks_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sgs_wallthicks", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_solar_mult_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "solar_mult", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_solarm_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "solarm", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_store_fl_props_mset(SAM_TcsMSLF ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "store_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_store_fluid_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "store_fluid", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_t_ch_out_max_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "t_ch_out_max", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_t_dis_out_min_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "t_dis_out_min", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_t_standby_reset_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "t_standby_reset", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_tank_max_heat_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_tank_pairs_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_pairs", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_tanks_in_parallel_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tanks_in_parallel", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_tc_fill_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tc_fill", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_tc_void_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tc_void", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_tes_pump_coef_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pump_coef", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_tes_temp_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_temp", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_tes_type_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_type", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_theta_dep_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_dep", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_theta_stow_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_stow", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_tshours_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tshours", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_tslogic_a_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "tslogic_a", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_tslogic_b_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "tslogic_b", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_tslogic_c_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "tslogic_c", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_u_tank_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Controller_vol_tank_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "vol_tank", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_SolarField_washes_per_year_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "washes_per_year", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_SolarField_water_per_wash_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "water_per_wash", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_CT_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_F_wc_aset(SAM_TcsMSLF ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "F_wc", arr, length);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_P_boil_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_boil", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_P_cond_min_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_min", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_P_cond_ratio_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_ratio", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_P_ref_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_ref", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_T_ITD_des_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ITD_des", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_T_amb_des_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_T_approach_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_approach", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_T_htf_cold_ref_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_cold_ref", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_T_htf_hot_ref_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_hot_ref", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_dT_cw_ref_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_cw_ref", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_eta_ref_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_ref", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_n_pl_inc_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_pl_inc", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_pb_bd_frac_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_bd_frac", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_pc_config_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pc_config", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_q_sby_frac_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_sby_frac", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_startup_frac_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_frac", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_startup_time_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_time", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Powerblock_tech_type_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tech_type", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_UserDefinedPC_ud_f_W_dot_cool_des_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_f_W_dot_cool_des", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_UserDefinedPC_ud_ind_od_mset(SAM_TcsMSLF ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsMSLF_UserDefinedPC_ud_m_dot_water_cool_des_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_m_dot_water_cool_des", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Enet_eta_lhv_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_lhv", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Enet_eta_tes_htr_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_tes_htr", number);
	});
}

SAM_EXPORT void SAM_TcsMSLF_Enet_fp_mode_nset(SAM_TcsMSLF ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fp_mode", number);
	});
}

SAM_EXPORT double SAM_TcsMSLF_Weather_azimuth_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "azimuth", &result))
		make_access_error("SAM_TcsMSLF", "azimuth");
	});
	return result;
}



SAM_EXPORT const char* SAM_TcsMSLF_Weather_file_name_sget(SAM_TcsMSLF ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "file_name");
	if (!result)
		make_access_error("SAM_TcsMSLF", "file_name");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Weather_tilt_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tilt", &result))
		make_access_error("SAM_TcsMSLF", "tilt");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Weather_track_mode_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "track_mode", &result))
		make_access_error("SAM_TcsMSLF", "track_mode");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Mslf_system_capacity_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_TcsMSLF", "system_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_TouTranslator_weekday_schedule_mget(SAM_TcsMSLF ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekday_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsMSLF", "weekday_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_TouTranslator_weekend_schedule_mget(SAM_TcsMSLF ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekend_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsMSLF", "weekend_schedule");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_A_aperture_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_aperture", &result))
		make_access_error("SAM_TcsMSLF", "A_aperture");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_AbsorberMaterial_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "AbsorberMaterial", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "AbsorberMaterial");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_AnnulusGas_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "AnnulusGas", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "AnnulusGas");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_ColAz_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ColAz", &result))
		make_access_error("SAM_TcsMSLF", "ColAz");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_DP_SGS_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "DP_SGS", &result))
		make_access_error("SAM_TcsMSLF", "DP_SGS");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_DP_coefs_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "DP_coefs", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "DP_coefs");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_DP_nominal_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "DP_nominal", &result))
		make_access_error("SAM_TcsMSLF", "DP_nominal");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_D_abs_in_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "D_abs_in", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "D_abs_in");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_D_abs_out_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "D_abs_out", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "D_abs_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_D_glass_in_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "D_glass_in", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "D_glass_in");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_D_glass_out_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "D_glass_out", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "D_glass_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_D_plug_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "D_plug", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "D_plug");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_Design_loss_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Design_loss", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "Design_loss");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_Dirt_mirror_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Dirt_mirror", &result))
		make_access_error("SAM_TcsMSLF", "Dirt_mirror");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_Error_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Error", &result))
		make_access_error("SAM_TcsMSLF", "Error");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_FieldConfig_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "FieldConfig", &result))
		make_access_error("SAM_TcsMSLF", "FieldConfig");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_Flow_type_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Flow_type", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "Flow_type");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_Fluid_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Fluid", &result))
		make_access_error("SAM_TcsMSLF", "Fluid");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_GeomEffects_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "GeomEffects", &result))
		make_access_error("SAM_TcsMSLF", "GeomEffects");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_GlazingIntactIn_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "GlazingIntactIn", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "GlazingIntactIn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_HCE_FieldFrac_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCE_FieldFrac", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "HCE_FieldFrac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_HDR_rough_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HDR_rough", &result))
		make_access_error("SAM_TcsMSLF", "HDR_rough");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_HL_T_coefs_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HL_T_coefs", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "HL_T_coefs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_HL_w_coefs_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HL_w_coefs", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "HL_w_coefs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_IAM_L_coefs_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "IAM_L_coefs", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "IAM_L_coefs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_IAM_T_coefs_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "IAM_T_coefs", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "IAM_T_coefs");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_I_b_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_b", &result))
		make_access_error("SAM_TcsMSLF", "I_b");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_I_bn_des_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_bn_des", &result))
		make_access_error("SAM_TcsMSLF", "I_bn_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_L_crossover_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_crossover", &result))
		make_access_error("SAM_TcsMSLF", "L_crossover");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_L_mod_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_mod", &result))
		make_access_error("SAM_TcsMSLF", "L_mod");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_L_mod_spacing_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_mod_spacing", &result))
		make_access_error("SAM_TcsMSLF", "L_mod_spacing");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_OpticalTable_mget(SAM_TcsMSLF ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "OpticalTable", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsMSLF", "OpticalTable");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_P_a_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_a", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "P_a");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_P_amb_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_amb", &result))
		make_access_error("SAM_TcsMSLF", "P_amb");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_Pipe_hl_coef_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Pipe_hl_coef", &result))
		make_access_error("SAM_TcsMSLF", "Pipe_hl_coef");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_Rough_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Rough", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "Rough");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_SCA_drives_elec_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SCA_drives_elec", &result))
		make_access_error("SAM_TcsMSLF", "SCA_drives_elec");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_Shadowing_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Shadowing", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "Shadowing");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_T_amb_sf_des_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_sf_des", &result))
		make_access_error("SAM_TcsMSLF", "T_amb_sf_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_T_cold_in_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_cold_in", &result))
		make_access_error("SAM_TcsMSLF", "T_cold_in");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_T_db_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_db", &result))
		make_access_error("SAM_TcsMSLF", "T_db");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_T_dp_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_dp", &result))
		make_access_error("SAM_TcsMSLF", "T_dp");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_T_field_in_des_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_field_in_des", &result))
		make_access_error("SAM_TcsMSLF", "T_field_in_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_T_field_ini_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_field_ini", &result))
		make_access_error("SAM_TcsMSLF", "T_field_ini");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_T_field_out_des_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_field_out_des", &result))
		make_access_error("SAM_TcsMSLF", "T_field_out_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_T_fp_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_fp", &result))
		make_access_error("SAM_TcsMSLF", "T_fp");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_T_loop_in_des_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_in_des", &result))
		make_access_error("SAM_TcsMSLF", "T_loop_in_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_T_loop_out_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_out", &result))
		make_access_error("SAM_TcsMSLF", "T_loop_out");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_T_set_aux_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_set_aux", &result))
		make_access_error("SAM_TcsMSLF", "T_set_aux");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_T_startup_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_startup", &result))
		make_access_error("SAM_TcsMSLF", "T_startup");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_T_tank_cold_ini_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_tank_cold_ini", &result))
		make_access_error("SAM_TcsMSLF", "T_tank_cold_ini");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_T_tank_hot_ini_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_tank_hot_ini", &result))
		make_access_error("SAM_TcsMSLF", "T_tank_hot_ini");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_T_tank_hot_inlet_min_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_tank_hot_inlet_min", &result))
		make_access_error("SAM_TcsMSLF", "T_tank_hot_inlet_min");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_Tau_envelope_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Tau_envelope", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "Tau_envelope");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_TrackingError_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TrackingError", &result))
		make_access_error("SAM_TcsMSLF", "TrackingError");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_V_hdr_max_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_max", &result))
		make_access_error("SAM_TcsMSLF", "V_hdr_max");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_V_hdr_min_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_min", &result))
		make_access_error("SAM_TcsMSLF", "V_hdr_min");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_V_tank_hot_ini_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tank_hot_ini", &result))
		make_access_error("SAM_TcsMSLF", "V_tank_hot_ini");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_V_tes_des_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tes_des", &result))
		make_access_error("SAM_TcsMSLF", "V_tes_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_V_wind_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_wind", &result))
		make_access_error("SAM_TcsMSLF", "V_wind");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_V_wind_des_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_wind_des", &result))
		make_access_error("SAM_TcsMSLF", "V_wind_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_W_pb_design_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_pb_design", &result))
		make_access_error("SAM_TcsMSLF", "W_pb_design");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_alpha_abs_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "alpha_abs", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "alpha_abs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_alpha_env_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "alpha_env", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "alpha_env");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_aux_array_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "aux_array", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "aux_array");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_bop_array_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "bop_array", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "bop_array");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_calc_design_pipe_vals_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calc_design_pipe_vals", &result))
		make_access_error("SAM_TcsMSLF", "calc_design_pipe_vals");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_cold_tank_Thtr_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_Thtr", &result))
		make_access_error("SAM_TcsMSLF", "cold_tank_Thtr");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_custom_sgs_pipe_sizes_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "custom_sgs_pipe_sizes", &result))
		make_access_error("SAM_TcsMSLF", "custom_sgs_pipe_sizes");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_custom_tes_p_loss_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "custom_tes_p_loss", &result))
		make_access_error("SAM_TcsMSLF", "custom_tes_p_loss");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_cycle_cutoff_frac_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_cutoff_frac", &result))
		make_access_error("SAM_TcsMSLF", "cycle_cutoff_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_cycle_max_frac_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_max_frac", &result))
		make_access_error("SAM_TcsMSLF", "cycle_max_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_defocus_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "defocus", &result))
		make_access_error("SAM_TcsMSLF", "defocus");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_dirt_env_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dirt_env", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "dirt_env");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_dt_cold_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_cold", &result))
		make_access_error("SAM_TcsMSLF", "dt_cold");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_dt_hot_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_hot", &result))
		make_access_error("SAM_TcsMSLF", "dt_hot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_epsilon_abs_1_mget(SAM_TcsMSLF ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_abs_1", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsMSLF", "epsilon_abs_1");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_epsilon_abs_2_mget(SAM_TcsMSLF ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_abs_2", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsMSLF", "epsilon_abs_2");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_epsilon_abs_3_mget(SAM_TcsMSLF ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_abs_3", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsMSLF", "epsilon_abs_3");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_epsilon_abs_4_mget(SAM_TcsMSLF ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_abs_4", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsMSLF", "epsilon_abs_4");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_epsilon_glass_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "epsilon_glass", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "epsilon_glass");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_eta_pump_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pump", &result))
		make_access_error("SAM_TcsMSLF", "eta_pump");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_f_tc_cold_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_tc_cold", &result))
		make_access_error("SAM_TcsMSLF", "f_tc_cold");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_fc_on_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fc_on", &result))
		make_access_error("SAM_TcsMSLF", "fc_on");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_ffrac_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ffrac", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "ffrac");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_field_fl_props_mget(SAM_TcsMSLF ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "field_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsMSLF", "field_fl_props");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_field_fluid_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_fluid", &result))
		make_access_error("SAM_TcsMSLF", "field_fluid");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_fossil_mode_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fossil_mode", &result))
		make_access_error("SAM_TcsMSLF", "fossil_mode");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_fthr_ok_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fthr_ok", &result))
		make_access_error("SAM_TcsMSLF", "fthr_ok");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_fthrctrl_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fthrctrl", &result))
		make_access_error("SAM_TcsMSLF", "fthrctrl");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_fthrok_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fthrok", &result))
		make_access_error("SAM_TcsMSLF", "fthrok");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_h_tank_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank", &result))
		make_access_error("SAM_TcsMSLF", "h_tank");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_h_tank_min_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_min", &result))
		make_access_error("SAM_TcsMSLF", "h_tank_min");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_has_hot_tank_bypass_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "has_hot_tank_bypass", &result))
		make_access_error("SAM_TcsMSLF", "has_hot_tank_bypass");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_hot_tank_Thtr_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_Thtr", &result))
		make_access_error("SAM_TcsMSLF", "hot_tank_Thtr");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_hx_config_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hx_config", &result))
		make_access_error("SAM_TcsMSLF", "hx_config");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_is_hx_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_hx", &result))
		make_access_error("SAM_TcsMSLF", "is_hx");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_k_tes_loss_coeffs_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "k_tes_loss_coeffs", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "k_tes_loss_coeffs");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_m_dot_htfmax_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmax", &result))
		make_access_error("SAM_TcsMSLF", "m_dot_htfmax");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_m_dot_htfmin_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmin", &result))
		make_access_error("SAM_TcsMSLF", "m_dot_htfmin");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_mc_bal_cold_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_cold", &result))
		make_access_error("SAM_TcsMSLF", "mc_bal_cold");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_mc_bal_hot_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_hot", &result))
		make_access_error("SAM_TcsMSLF", "mc_bal_hot");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_mc_bal_sca_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_sca", &result))
		make_access_error("SAM_TcsMSLF", "mc_bal_sca");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_nLoops_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nLoops", &result))
		make_access_error("SAM_TcsMSLF", "nLoops");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_nMod_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nMod", &result))
		make_access_error("SAM_TcsMSLF", "nMod");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_nRecVar_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nRecVar", &result))
		make_access_error("SAM_TcsMSLF", "nRecVar");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_nSCA_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nSCA", &result))
		make_access_error("SAM_TcsMSLF", "nSCA");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_nodes_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nodes", &result))
		make_access_error("SAM_TcsMSLF", "nodes");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_opt_model_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_model", &result))
		make_access_error("SAM_TcsMSLF", "opt_model");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_pb_fixed_par_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_fixed_par", &result))
		make_access_error("SAM_TcsMSLF", "pb_fixed_par");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_pb_pump_coef_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_pump_coef", &result))
		make_access_error("SAM_TcsMSLF", "pb_pump_coef");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_pb_rated_cap_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_rated_cap", &result))
		make_access_error("SAM_TcsMSLF", "pb_rated_cap");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_q_max_aux_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_max_aux", &result))
		make_access_error("SAM_TcsMSLF", "q_max_aux");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_q_pb_design_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_pb_design", &result))
		make_access_error("SAM_TcsMSLF", "q_pb_design");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_rec_htf_vol_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_htf_vol", &result))
		make_access_error("SAM_TcsMSLF", "rec_htf_vol");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_rec_model_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_model", &result))
		make_access_error("SAM_TcsMSLF", "rec_model");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_reflectivity_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "reflectivity", &result))
		make_access_error("SAM_TcsMSLF", "reflectivity");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_sgs_diams_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sgs_diams", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "sgs_diams");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_sgs_lengths_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sgs_lengths", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "sgs_lengths");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_sgs_wallthicks_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sgs_wallthicks", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "sgs_wallthicks");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_solar_mult_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_mult", &result))
		make_access_error("SAM_TcsMSLF", "solar_mult");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_solarm_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solarm", &result))
		make_access_error("SAM_TcsMSLF", "solarm");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_store_fl_props_mget(SAM_TcsMSLF ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "store_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsMSLF", "store_fl_props");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_store_fluid_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "store_fluid", &result))
		make_access_error("SAM_TcsMSLF", "store_fluid");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_t_ch_out_max_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_ch_out_max", &result))
		make_access_error("SAM_TcsMSLF", "t_ch_out_max");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_t_dis_out_min_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_dis_out_min", &result))
		make_access_error("SAM_TcsMSLF", "t_dis_out_min");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_t_standby_reset_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_standby_reset", &result))
		make_access_error("SAM_TcsMSLF", "t_standby_reset");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_tank_max_heat_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_max_heat", &result))
		make_access_error("SAM_TcsMSLF", "tank_max_heat");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_tank_pairs_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_pairs", &result))
		make_access_error("SAM_TcsMSLF", "tank_pairs");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_tanks_in_parallel_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tanks_in_parallel", &result))
		make_access_error("SAM_TcsMSLF", "tanks_in_parallel");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_tc_fill_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tc_fill", &result))
		make_access_error("SAM_TcsMSLF", "tc_fill");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_tc_void_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tc_void", &result))
		make_access_error("SAM_TcsMSLF", "tc_void");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_tes_pump_coef_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pump_coef", &result))
		make_access_error("SAM_TcsMSLF", "tes_pump_coef");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_tes_temp_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_temp", &result))
		make_access_error("SAM_TcsMSLF", "tes_temp");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_tes_type_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_type", &result))
		make_access_error("SAM_TcsMSLF", "tes_type");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_theta_dep_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_dep", &result))
		make_access_error("SAM_TcsMSLF", "theta_dep");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_theta_stow_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_stow", &result))
		make_access_error("SAM_TcsMSLF", "theta_stow");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_tshours_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours", &result))
		make_access_error("SAM_TcsMSLF", "tshours");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_tslogic_a_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tslogic_a", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "tslogic_a");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_tslogic_b_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tslogic_b", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "tslogic_b");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Controller_tslogic_c_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tslogic_c", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "tslogic_c");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_u_tank_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_tank", &result))
		make_access_error("SAM_TcsMSLF", "u_tank");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Controller_vol_tank_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vol_tank", &result))
		make_access_error("SAM_TcsMSLF", "vol_tank");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_SolarField_washes_per_year_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "washes_per_year", &result))
		make_access_error("SAM_TcsMSLF", "washes_per_year");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_SolarField_water_per_wash_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "water_per_wash", &result))
		make_access_error("SAM_TcsMSLF", "water_per_wash");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_CT_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT", &result))
		make_access_error("SAM_TcsMSLF", "CT");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Powerblock_F_wc_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "F_wc", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "F_wc");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_P_boil_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_boil", &result))
		make_access_error("SAM_TcsMSLF", "P_boil");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_P_cond_min_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_min", &result))
		make_access_error("SAM_TcsMSLF", "P_cond_min");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_P_cond_ratio_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_ratio", &result))
		make_access_error("SAM_TcsMSLF", "P_cond_ratio");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_P_ref_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_ref", &result))
		make_access_error("SAM_TcsMSLF", "P_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_T_ITD_des_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ITD_des", &result))
		make_access_error("SAM_TcsMSLF", "T_ITD_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_T_amb_des_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_TcsMSLF", "T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_T_approach_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_approach", &result))
		make_access_error("SAM_TcsMSLF", "T_approach");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_T_htf_cold_ref_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_cold_ref", &result))
		make_access_error("SAM_TcsMSLF", "T_htf_cold_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_T_htf_hot_ref_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_hot_ref", &result))
		make_access_error("SAM_TcsMSLF", "T_htf_hot_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_dT_cw_ref_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_cw_ref", &result))
		make_access_error("SAM_TcsMSLF", "dT_cw_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_eta_ref_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ref", &result))
		make_access_error("SAM_TcsMSLF", "eta_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_n_pl_inc_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_pl_inc", &result))
		make_access_error("SAM_TcsMSLF", "n_pl_inc");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_pb_bd_frac_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_bd_frac", &result))
		make_access_error("SAM_TcsMSLF", "pb_bd_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_pc_config_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_config", &result))
		make_access_error("SAM_TcsMSLF", "pc_config");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_q_sby_frac_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_sby_frac", &result))
		make_access_error("SAM_TcsMSLF", "q_sby_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_startup_frac_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_frac", &result))
		make_access_error("SAM_TcsMSLF", "startup_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_startup_time_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_time", &result))
		make_access_error("SAM_TcsMSLF", "startup_time");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Powerblock_tech_type_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tech_type", &result))
		make_access_error("SAM_TcsMSLF", "tech_type");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_UserDefinedPC_ud_f_W_dot_cool_des_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_f_W_dot_cool_des", &result))
		make_access_error("SAM_TcsMSLF", "ud_f_W_dot_cool_des");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_UserDefinedPC_ud_ind_od_mget(SAM_TcsMSLF ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsMSLF", "ud_ind_od");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_UserDefinedPC_ud_m_dot_water_cool_des_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_m_dot_water_cool_des", &result))
		make_access_error("SAM_TcsMSLF", "ud_m_dot_water_cool_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Enet_eta_lhv_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_lhv", &result))
		make_access_error("SAM_TcsMSLF", "eta_lhv");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Enet_eta_tes_htr_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_tes_htr", &result))
		make_access_error("SAM_TcsMSLF", "eta_tes_htr");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Enet_fp_mode_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fp_mode", &result))
		make_access_error("SAM_TcsMSLF", "fp_mode");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_DP_tot_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "DP_tot", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "DP_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_E_bal_startup_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "E_bal_startup", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "E_bal_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_EqOptEff_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EqOptEff", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "EqOptEff");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_P_cycle_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cycle", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "P_cycle");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_Pipe_hl_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Pipe_hl", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "Pipe_hl");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_Q_par_sf_fp_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_par_sf_fp", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "Q_par_sf_fp");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_Q_par_tes_fp_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_par_tes_fp", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "Q_par_tes_fp");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_T_pb_in_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pb_in", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "T_pb_in");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_T_pb_out_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pb_out", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "T_pb_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_T_sys_c_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_sys_c", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "T_sys_c");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_T_sys_h_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_sys_h", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "T_sys_h");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_T_tank_cold_fin_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tank_cold_fin", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "T_tank_cold_fin");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_T_tank_cold_in_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tank_cold_in", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "T_tank_cold_in");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_T_tank_hot_fin_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tank_hot_fin", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "T_tank_hot_fin");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_T_tank_hot_in_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tank_hot_in", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "T_tank_hot_in");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_Ts_cold_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Ts_cold", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "Ts_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_Ts_hot_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Ts_hot", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "Ts_hot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_W_cool_par_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_cool_par", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "W_cool_par");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_W_dot_pump_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_pump", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "W_dot_pump");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_W_net_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_net", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "W_net");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_W_par_BOP_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_par_BOP", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "W_par_BOP");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_W_par_aux_boiler_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_par_aux_boiler", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "W_par_aux_boiler");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Outputs_annual_W_cycle_gross_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_W_cycle_gross", &result))
		make_access_error("SAM_TcsMSLF", "annual_W_cycle_gross");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Outputs_annual_energy_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_TcsMSLF", "annual_energy");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Outputs_annual_fuel_usage_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_fuel_usage", &result))
		make_access_error("SAM_TcsMSLF", "annual_fuel_usage");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Outputs_annual_total_water_use_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_total_water_use", &result))
		make_access_error("SAM_TcsMSLF", "annual_total_water_use");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_beam_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "beam");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Outputs_capacity_factor_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_TcsMSLF", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Outputs_conversion_factor_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conversion_factor", &result))
		make_access_error("SAM_TcsMSLF", "conversion_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_eta_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "eta");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_eta_optical_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_optical", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "eta_optical");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_eta_thermal_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_thermal", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "eta_thermal");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_fixed_par_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fixed_par", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "fixed_par");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_gen_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_hour_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "hour");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_htf_pump_power_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "htf_pump_power", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "htf_pump_power");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Outputs_kwh_per_kw_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_TcsMSLF", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_m_dot_avail_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_avail", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "m_dot_avail");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_m_dot_charge_field_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_charge_field", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "m_dot_charge_field");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_m_dot_discharge_tank_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_discharge_tank", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "m_dot_discharge_tank");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_m_dot_htf2_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_htf2", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "m_dot_htf2");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_m_dot_makeup_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_makeup", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "m_dot_makeup");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_m_dot_pb_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pb", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "m_dot_pb");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_mass_tank_cold_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tank_cold", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "mass_tank_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_mass_tank_hot_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tank_hot", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "mass_tank_hot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_month_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "month");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_monthly_energy_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "monthly_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_phi_t_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "phi_t", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "phi_t");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_pipe_sgs_P_dsn_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_sgs_P_dsn", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "pipe_sgs_P_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_pipe_sgs_T_dsn_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_sgs_T_dsn", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "pipe_sgs_T_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_pipe_sgs_diams_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_sgs_diams", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "pipe_sgs_diams");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_pipe_sgs_mdot_dsn_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_sgs_mdot_dsn", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "pipe_sgs_mdot_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_pipe_sgs_vel_dsn_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_sgs_vel_dsn", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "pipe_sgs_vel_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_pipe_sgs_wallthk_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_sgs_wallthk", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "pipe_sgs_wallthk");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_pres_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pres", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "pres");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_q_abs_tot_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_abs_tot", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "q_abs_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_q_aux_fuel_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_aux_fuel", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "q_aux_fuel");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_q_avail_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_avail", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "q_avail");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_q_dump_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dump", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "q_dump");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_q_inc_sf_tot_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_inc_sf_tot", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "q_inc_sf_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_q_loss_spec_tot_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_loss_spec_tot", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "q_loss_spec_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_q_loss_tot_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_loss_tot", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "q_loss_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_q_pb_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pb", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "q_pb");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_q_to_tes_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_to_tes", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "q_to_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_sf_def_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sf_def", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "sf_def");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_solazi_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solazi", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "solazi");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_solzen_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solzen", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "solzen");
	});
	return result;
}



SAM_EXPORT double SAM_TcsMSLF_Outputs_system_heat_rate_nget(SAM_TcsMSLF ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_heat_rate", &result))
		make_access_error("SAM_TcsMSLF", "system_heat_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_t_loop_outlet_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "t_loop_outlet", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "t_loop_outlet");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_tank_losses_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tank_losses", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "tank_losses");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_tdry_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "tdry");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_theta_L_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "theta_L", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "theta_L");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_tou_value_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tou_value", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "tou_value");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_track_par_tot_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "track_par_tot", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "track_par_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_twet_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "twet");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_vol_tank_cold_fin_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "vol_tank_cold_fin", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "vol_tank_cold_fin");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_vol_tank_hot_fin_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "vol_tank_hot_fin", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "vol_tank_hot_fin");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_vol_tank_total_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "vol_tank_total", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "vol_tank_total");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsMSLF_Outputs_wspd_aget(SAM_TcsMSLF ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_TcsMSLF", "wspd");
	});
	return result;
}




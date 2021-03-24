#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_TcstroughPhysical.h"

SAM_EXPORT int SAM_TcstroughPhysical_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("tcstrough_physical", data, verbosity, err);
}

SAM_EXPORT void SAM_TcstroughPhysical_Weather_azimuth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "azimuth", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Weather_file_name_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "file_name", str);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Weather_tilt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tilt", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Weather_track_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "track_mode", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Trough_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_A_aperture_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "A_aperture", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_AbsorberMaterial_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "AbsorberMaterial", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_AnnulusGas_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "AnnulusGas", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Ave_Focal_Length_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Ave_Focal_Length", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_ColperSCA_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ColperSCA", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_D_2_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_2", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_D_3_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_3", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_D_4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_4", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_D_5_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_5", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_D_cpnt_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_cpnt", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_D_p_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_p", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Design_loss_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Design_loss", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Dirt_HCE_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Dirt_HCE", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Dirt_mirror_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Dirt_mirror", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Distance_SCA_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Distance_SCA", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_EPSILON_4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "EPSILON_4", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_EPSILON_5_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "EPSILON_5", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Error_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Error", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_FieldConfig_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "FieldConfig", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Flow_type_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Flow_type", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Fluid", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_GeomEffects_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "GeomEffects", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_GlazingIntactIn_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "GlazingIntactIn", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_HCE_FieldFrac_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "HCE_FieldFrac", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_HDR_rough_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HDR_rough", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_IAM_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "IAM_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_I_bn_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_bn_des", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_K_cpnt_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "K_cpnt", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_L_SCA_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "L_SCA", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_L_aperture_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "L_aperture", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_L_cpnt_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "L_cpnt", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_L_rnr_pb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_rnr_pb", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_L_rnr_per_xpan_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_rnr_per_xpan", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_L_xpan_hdr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_xpan_hdr", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_L_xpan_rnr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_xpan_rnr", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Min_rnr_xpans_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Min_rnr_xpans", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_N_hdr_per_xpan_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "N_hdr_per_xpan", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_N_max_hdr_diams_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "N_max_hdr_diams", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_P_a_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "P_a", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_P_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_ref", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Pipe_hl_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Pipe_hl_coef", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Rho_mirror_clean_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Rho_mirror_clean", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Rough_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Rough", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Row_Distance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Row_Distance", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_SCADefocusArray_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "SCADefocusArray", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_SCAInfoArray_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "SCAInfoArray", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_SCA_drives_elec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SCA_drives_elec", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Shadowing_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Shadowing", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_T_fp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_fp", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_T_loop_in_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_in_des", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_T_loop_out_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_out", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_T_startup_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_startup", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Tau_envelope_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Tau_envelope", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_TrackingError_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "TrackingError", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Type_cpnt_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Type_cpnt", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_V_hdr_cold_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_cold_max", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_V_hdr_cold_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_cold_min", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_V_hdr_hot_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_hot_max", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_V_hdr_hot_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_hot_min", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_W_aperture_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "W_aperture", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_accept_init_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "accept_init", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_accept_loc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "accept_loc", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_accept_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "accept_mode", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_alpha_abs_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "alpha_abs", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_alpha_env_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "alpha_env", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_calc_design_pipe_vals_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calc_design_pipe_vals", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_custom_sf_pipe_sizes_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "custom_sf_pipe_sizes", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_11_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_11", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_12_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_12", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_13_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_13", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_14_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_14", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_21_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_21", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_22_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_22", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_23_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_23", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_24_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_24", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_31_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_31", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_32_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_32", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_33_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_33", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_34_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_34", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_41_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_41", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_42_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_42", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_43_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_43", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_44_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_44", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_eta_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pump", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_fthrctrl_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fthrctrl", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_fthrok_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fthrok", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_m_dot_htfmax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htfmax", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_m_dot_htfmin_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htfmin", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_mc_bal_cold_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_cold", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_mc_bal_hot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_hot", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_mc_bal_sca_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_sca", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_nColt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nColt", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_nHCEVar_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nHCEVar", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_nHCEt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nHCEt", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_nLoops_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nLoops", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_nSCA_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nSCA", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_northsouth_field_sep_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "northsouth_field_sep", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_offset_xpan_hdr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "offset_xpan_hdr", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_sf_hdr_diams_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sf_hdr_diams", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_sf_hdr_lengths_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sf_hdr_lengths", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_sf_hdr_wallthicks_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sf_hdr_wallthicks", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_sf_rnr_diams_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sf_rnr_diams", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_sf_rnr_lengths_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sf_rnr_lengths", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_sf_rnr_wallthicks_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sf_rnr_wallthicks", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_solar_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "solar_mult", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_theta_dep_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_dep", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_theta_stow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_stow", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_washing_frequency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "washing_frequency", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_SolarField_water_usage_per_wash_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "water_usage_per_wash", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_DP_SGS_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "DP_SGS", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_HDR_rough_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HDR_rough", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_T_set_aux_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_set_aux", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_T_tank_cold_ini_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_tank_cold_ini", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_T_tank_hot_inlet_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_tank_hot_inlet_min", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_V_tank_hot_ini_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_tank_hot_ini", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_V_tes_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_tes_des", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_W_pb_design_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "W_pb_design", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_aux_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "aux_array", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_bop_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "bop_array", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_custom_sgs_pipe_sizes_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "custom_sgs_pipe_sizes", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_custom_tes_p_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "custom_tes_p_loss", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_cycle_cutoff_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_cutoff_frac", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_cycle_max_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_max_frac", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_dt_cold_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_cold", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_dt_hot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_hot", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_eta_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pump", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_f_tc_cold_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_tc_cold", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_ffrac_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ffrac", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "field_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_fossil_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fossil_mode", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_h_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_min", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_has_hot_tank_bypass_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "has_hot_tank_bypass", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_hx_config_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hx_config", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_is_hx_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_hx", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_k_tes_loss_coeffs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "k_tes_loss_coeffs", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_nodes_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nodes", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_fixed_par", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_pump_coef", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_q_max_aux_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_max_aux", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_q_pb_design_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_pb_design", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_sf_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sf_type", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_sgs_diams_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sgs_diams", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_sgs_lengths_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sgs_lengths", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_sgs_wallthicks_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sgs_wallthicks", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_store_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "store_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_store_fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "store_fluid", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_t_ch_out_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "t_ch_out_max", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_t_dis_out_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "t_dis_out_min", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_t_standby_reset_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "t_standby_reset", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_pairs", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_tanks_in_parallel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tanks_in_parallel", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_tc_fill_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tc_fill", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_tc_void_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tc_void", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_tes_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pump_coef", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_tes_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_type", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_tshours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tshours", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_tslogic_a_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "tslogic_a", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_tslogic_b_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "tslogic_b", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_tslogic_c_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "tslogic_c", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_u_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Controller_vol_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "vol_tank", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_TouTranslator_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekday_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_TouTranslator_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekend_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_CT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_F_wc_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "F_wc", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_P_boil_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_boil", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_P_cond_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_min", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_P_cond_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_ratio", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ITD_des", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_T_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_approach", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_dT_cw_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_cw_ref", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_eta_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_ref", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_n_pl_inc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_pl_inc", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_pb_bd_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_bd_frac", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_pc_config_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pc_config", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_sby_frac", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_startup_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_frac", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_startup_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_time", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_tech_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tech_type", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_T_amb_des", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_high_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_T_amb_high", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_T_amb_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_low_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_T_amb_low", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_high_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_T_htf_high", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_T_htf_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_low_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_T_htf_low", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_f_W_dot_cool_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_f_W_dot_cool_des", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_high_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_m_dot_htf_high", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_m_dot_htf_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_low_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_m_dot_htf_low", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_water_cool_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_m_dot_water_cool_des", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Enet_eta_lhv_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_lhv", number);
	});
}

SAM_EXPORT void SAM_TcstroughPhysical_Enet_eta_tes_htr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_tes_htr", number);
	});
}

SAM_EXPORT double SAM_TcstroughPhysical_Weather_azimuth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "azimuth", &result))
		make_access_error("SAM_TcstroughPhysical", "azimuth");
	});
	return result;
}



SAM_EXPORT const char* SAM_TcstroughPhysical_Weather_file_name_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "file_name");
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "file_name");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Weather_tilt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tilt", &result))
		make_access_error("SAM_TcstroughPhysical", "tilt");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Weather_track_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "track_mode", &result))
		make_access_error("SAM_TcstroughPhysical", "track_mode");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Trough_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_TcstroughPhysical", "system_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_A_aperture_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "A_aperture", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "A_aperture");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_AbsorberMaterial_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "AbsorberMaterial", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "AbsorberMaterial");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_AnnulusGas_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "AnnulusGas", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "AnnulusGas");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Ave_Focal_Length_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Ave_Focal_Length", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Ave_Focal_Length");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_ColperSCA_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ColperSCA", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "ColperSCA");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_D_2_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_2", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "D_2");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_D_3_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_3", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "D_3");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_D_4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_4", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "D_4");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_D_5_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_5", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "D_5");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_D_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_cpnt", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "D_cpnt");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_D_p_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_p", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "D_p");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Design_loss_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Design_loss", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Design_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Dirt_HCE_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Dirt_HCE", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Dirt_HCE");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Dirt_mirror_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Dirt_mirror", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Dirt_mirror");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Distance_SCA_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Distance_SCA", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Distance_SCA");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_EPSILON_4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "EPSILON_4", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "EPSILON_4");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_EPSILON_5_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "EPSILON_5", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "EPSILON_5");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Error_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Error", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Error");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_FieldConfig_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "FieldConfig", &result))
		make_access_error("SAM_TcstroughPhysical", "FieldConfig");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Flow_type_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Flow_type", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Flow_type");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_Fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Fluid", &result))
		make_access_error("SAM_TcstroughPhysical", "Fluid");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_GeomEffects_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "GeomEffects", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "GeomEffects");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_GlazingIntactIn_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "GlazingIntactIn", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "GlazingIntactIn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_HCE_FieldFrac_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "HCE_FieldFrac", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "HCE_FieldFrac");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_HDR_rough_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HDR_rough", &result))
		make_access_error("SAM_TcstroughPhysical", "HDR_rough");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_IAM_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "IAM_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "IAM_matrix");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_I_bn_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_bn_des", &result))
		make_access_error("SAM_TcstroughPhysical", "I_bn_des");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_K_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "K_cpnt", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "K_cpnt");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_L_SCA_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "L_SCA", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "L_SCA");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_L_aperture_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "L_aperture", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "L_aperture");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_L_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "L_cpnt", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "L_cpnt");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_L_rnr_pb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_rnr_pb", &result))
		make_access_error("SAM_TcstroughPhysical", "L_rnr_pb");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_L_rnr_per_xpan_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_rnr_per_xpan", &result))
		make_access_error("SAM_TcstroughPhysical", "L_rnr_per_xpan");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_L_xpan_hdr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_xpan_hdr", &result))
		make_access_error("SAM_TcstroughPhysical", "L_xpan_hdr");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_L_xpan_rnr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_xpan_rnr", &result))
		make_access_error("SAM_TcstroughPhysical", "L_xpan_rnr");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_Min_rnr_xpans_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Min_rnr_xpans", &result))
		make_access_error("SAM_TcstroughPhysical", "Min_rnr_xpans");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_N_hdr_per_xpan_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_hdr_per_xpan", &result))
		make_access_error("SAM_TcstroughPhysical", "N_hdr_per_xpan");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_N_max_hdr_diams_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_max_hdr_diams", &result))
		make_access_error("SAM_TcstroughPhysical", "N_max_hdr_diams");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_P_a_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "P_a", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "P_a");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_P_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_ref", &result))
		make_access_error("SAM_TcstroughPhysical", "P_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_Pipe_hl_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Pipe_hl_coef", &result))
		make_access_error("SAM_TcstroughPhysical", "Pipe_hl_coef");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Rho_mirror_clean_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Rho_mirror_clean", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Rho_mirror_clean");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Rough_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Rough", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Rough");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_Row_Distance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Row_Distance", &result))
		make_access_error("SAM_TcstroughPhysical", "Row_Distance");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_SCADefocusArray_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "SCADefocusArray", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "SCADefocusArray");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_SCAInfoArray_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "SCAInfoArray", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "SCAInfoArray");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_SCA_drives_elec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SCA_drives_elec", &result))
		make_access_error("SAM_TcstroughPhysical", "SCA_drives_elec");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Shadowing_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Shadowing", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Shadowing");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_T_fp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_fp", &result))
		make_access_error("SAM_TcstroughPhysical", "T_fp");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_T_loop_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_in_des", &result))
		make_access_error("SAM_TcstroughPhysical", "T_loop_in_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_T_loop_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_out", &result))
		make_access_error("SAM_TcstroughPhysical", "T_loop_out");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_T_startup_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_startup", &result))
		make_access_error("SAM_TcstroughPhysical", "T_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Tau_envelope_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Tau_envelope", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Tau_envelope");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_TrackingError_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "TrackingError", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "TrackingError");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Type_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Type_cpnt", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Type_cpnt");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_V_hdr_cold_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_cold_max", &result))
		make_access_error("SAM_TcstroughPhysical", "V_hdr_cold_max");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_V_hdr_cold_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_cold_min", &result))
		make_access_error("SAM_TcstroughPhysical", "V_hdr_cold_min");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_V_hdr_hot_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_hot_max", &result))
		make_access_error("SAM_TcstroughPhysical", "V_hdr_hot_max");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_V_hdr_hot_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_hot_min", &result))
		make_access_error("SAM_TcstroughPhysical", "V_hdr_hot_min");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_W_aperture_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_aperture", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "W_aperture");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_accept_init_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "accept_init", &result))
		make_access_error("SAM_TcstroughPhysical", "accept_init");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_accept_loc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "accept_loc", &result))
		make_access_error("SAM_TcstroughPhysical", "accept_loc");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_accept_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "accept_mode", &result))
		make_access_error("SAM_TcstroughPhysical", "accept_mode");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_alpha_abs_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "alpha_abs", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "alpha_abs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_alpha_env_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "alpha_env", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "alpha_env");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_calc_design_pipe_vals_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calc_design_pipe_vals", &result))
		make_access_error("SAM_TcstroughPhysical", "calc_design_pipe_vals");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_custom_sf_pipe_sizes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "custom_sf_pipe_sizes", &result))
		make_access_error("SAM_TcstroughPhysical", "custom_sf_pipe_sizes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_11_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_11", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_11");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_12_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_12", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_12");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_13_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_13", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_13");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_14_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_14", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_14");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_21_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_21", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_21");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_22_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_22", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_22");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_23_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_23", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_23");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_24_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_24", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_24");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_31_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_31", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_31");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_32_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_32", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_32");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_33_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_33", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_33");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_34_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_34", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_34");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_41_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_41", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_41");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_42_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_42", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_42");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_43_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_43", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_43");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_44_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_44", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "epsilon_3_44");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_eta_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pump", &result))
		make_access_error("SAM_TcstroughPhysical", "eta_pump");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_fthrctrl_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fthrctrl", &result))
		make_access_error("SAM_TcstroughPhysical", "fthrctrl");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_fthrok_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fthrok", &result))
		make_access_error("SAM_TcstroughPhysical", "fthrok");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_m_dot_htfmax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmax", &result))
		make_access_error("SAM_TcstroughPhysical", "m_dot_htfmax");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_m_dot_htfmin_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmin", &result))
		make_access_error("SAM_TcstroughPhysical", "m_dot_htfmin");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_mc_bal_cold_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_cold", &result))
		make_access_error("SAM_TcstroughPhysical", "mc_bal_cold");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_mc_bal_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_hot", &result))
		make_access_error("SAM_TcstroughPhysical", "mc_bal_hot");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_mc_bal_sca_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_sca", &result))
		make_access_error("SAM_TcstroughPhysical", "mc_bal_sca");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_nColt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nColt", &result))
		make_access_error("SAM_TcstroughPhysical", "nColt");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_nHCEVar_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nHCEVar", &result))
		make_access_error("SAM_TcstroughPhysical", "nHCEVar");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_nHCEt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nHCEt", &result))
		make_access_error("SAM_TcstroughPhysical", "nHCEt");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_nLoops_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nLoops", &result))
		make_access_error("SAM_TcstroughPhysical", "nLoops");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_nSCA_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nSCA", &result))
		make_access_error("SAM_TcstroughPhysical", "nSCA");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_northsouth_field_sep_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "northsouth_field_sep", &result))
		make_access_error("SAM_TcstroughPhysical", "northsouth_field_sep");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_offset_xpan_hdr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "offset_xpan_hdr", &result))
		make_access_error("SAM_TcstroughPhysical", "offset_xpan_hdr");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_sf_hdr_diams_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sf_hdr_diams", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "sf_hdr_diams");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_sf_hdr_lengths_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sf_hdr_lengths", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "sf_hdr_lengths");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_sf_hdr_wallthicks_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sf_hdr_wallthicks", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "sf_hdr_wallthicks");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_sf_rnr_diams_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sf_rnr_diams", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "sf_rnr_diams");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_sf_rnr_lengths_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sf_rnr_lengths", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "sf_rnr_lengths");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_sf_rnr_wallthicks_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sf_rnr_wallthicks", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "sf_rnr_wallthicks");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_solar_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_mult", &result))
		make_access_error("SAM_TcstroughPhysical", "solar_mult");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_theta_dep_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_dep", &result))
		make_access_error("SAM_TcstroughPhysical", "theta_dep");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_theta_stow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_stow", &result))
		make_access_error("SAM_TcstroughPhysical", "theta_stow");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_washing_frequency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "washing_frequency", &result))
		make_access_error("SAM_TcstroughPhysical", "washing_frequency");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_SolarField_water_usage_per_wash_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "water_usage_per_wash", &result))
		make_access_error("SAM_TcstroughPhysical", "water_usage_per_wash");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_DP_SGS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "DP_SGS", &result))
		make_access_error("SAM_TcstroughPhysical", "DP_SGS");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_HDR_rough_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HDR_rough", &result))
		make_access_error("SAM_TcstroughPhysical", "HDR_rough");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_T_set_aux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_set_aux", &result))
		make_access_error("SAM_TcstroughPhysical", "T_set_aux");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_T_tank_cold_ini_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_tank_cold_ini", &result))
		make_access_error("SAM_TcstroughPhysical", "T_tank_cold_ini");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_T_tank_hot_inlet_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_tank_hot_inlet_min", &result))
		make_access_error("SAM_TcstroughPhysical", "T_tank_hot_inlet_min");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_V_tank_hot_ini_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tank_hot_ini", &result))
		make_access_error("SAM_TcstroughPhysical", "V_tank_hot_ini");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_V_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tes_des", &result))
		make_access_error("SAM_TcstroughPhysical", "V_tes_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_W_pb_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_pb_design", &result))
		make_access_error("SAM_TcstroughPhysical", "W_pb_design");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Controller_aux_array_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "aux_array", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "aux_array");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Controller_bop_array_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "bop_array", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "bop_array");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_Thtr", &result))
		make_access_error("SAM_TcstroughPhysical", "cold_tank_Thtr");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_custom_sgs_pipe_sizes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "custom_sgs_pipe_sizes", &result))
		make_access_error("SAM_TcstroughPhysical", "custom_sgs_pipe_sizes");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_custom_tes_p_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "custom_tes_p_loss", &result))
		make_access_error("SAM_TcstroughPhysical", "custom_tes_p_loss");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_cycle_cutoff_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_cutoff_frac", &result))
		make_access_error("SAM_TcstroughPhysical", "cycle_cutoff_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_cycle_max_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_max_frac", &result))
		make_access_error("SAM_TcstroughPhysical", "cycle_max_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_dt_cold_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_cold", &result))
		make_access_error("SAM_TcstroughPhysical", "dt_cold");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_dt_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_hot", &result))
		make_access_error("SAM_TcstroughPhysical", "dt_hot");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_eta_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pump", &result))
		make_access_error("SAM_TcstroughPhysical", "eta_pump");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_f_tc_cold_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_tc_cold", &result))
		make_access_error("SAM_TcstroughPhysical", "f_tc_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Controller_ffrac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ffrac", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "ffrac");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Controller_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "field_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "field_fl_props");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_fossil_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fossil_mode", &result))
		make_access_error("SAM_TcstroughPhysical", "fossil_mode");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_h_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank", &result))
		make_access_error("SAM_TcstroughPhysical", "h_tank");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_h_tank_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_min", &result))
		make_access_error("SAM_TcstroughPhysical", "h_tank_min");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_has_hot_tank_bypass_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "has_hot_tank_bypass", &result))
		make_access_error("SAM_TcstroughPhysical", "has_hot_tank_bypass");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_Thtr", &result))
		make_access_error("SAM_TcstroughPhysical", "hot_tank_Thtr");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_hx_config_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hx_config", &result))
		make_access_error("SAM_TcstroughPhysical", "hx_config");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_is_hx_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_hx", &result))
		make_access_error("SAM_TcstroughPhysical", "is_hx");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Controller_k_tes_loss_coeffs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "k_tes_loss_coeffs", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "k_tes_loss_coeffs");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_nodes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nodes", &result))
		make_access_error("SAM_TcstroughPhysical", "nodes");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_pb_fixed_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_fixed_par", &result))
		make_access_error("SAM_TcstroughPhysical", "pb_fixed_par");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_pb_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_pump_coef", &result))
		make_access_error("SAM_TcstroughPhysical", "pb_pump_coef");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_q_max_aux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_max_aux", &result))
		make_access_error("SAM_TcstroughPhysical", "q_max_aux");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_q_pb_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_pb_design", &result))
		make_access_error("SAM_TcstroughPhysical", "q_pb_design");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_sf_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sf_type", &result))
		make_access_error("SAM_TcstroughPhysical", "sf_type");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Controller_sgs_diams_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sgs_diams", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "sgs_diams");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Controller_sgs_lengths_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sgs_lengths", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "sgs_lengths");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Controller_sgs_wallthicks_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sgs_wallthicks", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "sgs_wallthicks");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Controller_store_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "store_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "store_fl_props");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_store_fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "store_fluid", &result))
		make_access_error("SAM_TcstroughPhysical", "store_fluid");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_t_ch_out_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_ch_out_max", &result))
		make_access_error("SAM_TcstroughPhysical", "t_ch_out_max");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_t_dis_out_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_dis_out_min", &result))
		make_access_error("SAM_TcstroughPhysical", "t_dis_out_min");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_t_standby_reset_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_standby_reset", &result))
		make_access_error("SAM_TcstroughPhysical", "t_standby_reset");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_max_heat", &result))
		make_access_error("SAM_TcstroughPhysical", "tank_max_heat");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_tank_pairs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_pairs", &result))
		make_access_error("SAM_TcstroughPhysical", "tank_pairs");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_tanks_in_parallel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tanks_in_parallel", &result))
		make_access_error("SAM_TcstroughPhysical", "tanks_in_parallel");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_tc_fill_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tc_fill", &result))
		make_access_error("SAM_TcstroughPhysical", "tc_fill");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_tc_void_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tc_void", &result))
		make_access_error("SAM_TcstroughPhysical", "tc_void");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_tes_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pump_coef", &result))
		make_access_error("SAM_TcstroughPhysical", "tes_pump_coef");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_tes_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_type", &result))
		make_access_error("SAM_TcstroughPhysical", "tes_type");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_tshours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours", &result))
		make_access_error("SAM_TcstroughPhysical", "tshours");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Controller_tslogic_a_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tslogic_a", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "tslogic_a");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Controller_tslogic_b_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tslogic_b", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "tslogic_b");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Controller_tslogic_c_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tslogic_c", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "tslogic_c");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_u_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_tank", &result))
		make_access_error("SAM_TcstroughPhysical", "u_tank");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Controller_vol_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vol_tank", &result))
		make_access_error("SAM_TcstroughPhysical", "vol_tank");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_TouTranslator_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekday_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "weekday_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_TouTranslator_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekend_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "weekend_schedule");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_CT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT", &result))
		make_access_error("SAM_TcstroughPhysical", "CT");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Powerblock_F_wc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "F_wc", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "F_wc");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_P_boil_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_boil", &result))
		make_access_error("SAM_TcstroughPhysical", "P_boil");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_P_cond_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_min", &result))
		make_access_error("SAM_TcstroughPhysical", "P_cond_min");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_P_cond_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_ratio", &result))
		make_access_error("SAM_TcstroughPhysical", "P_cond_ratio");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_T_ITD_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ITD_des", &result))
		make_access_error("SAM_TcstroughPhysical", "T_ITD_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_TcstroughPhysical", "T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_T_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_approach", &result))
		make_access_error("SAM_TcstroughPhysical", "T_approach");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_dT_cw_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_cw_ref", &result))
		make_access_error("SAM_TcstroughPhysical", "dT_cw_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_eta_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ref", &result))
		make_access_error("SAM_TcstroughPhysical", "eta_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_n_pl_inc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_pl_inc", &result))
		make_access_error("SAM_TcstroughPhysical", "n_pl_inc");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_pb_bd_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_bd_frac", &result))
		make_access_error("SAM_TcstroughPhysical", "pb_bd_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_pc_config_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_config", &result))
		make_access_error("SAM_TcstroughPhysical", "pc_config");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_q_sby_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_sby_frac", &result))
		make_access_error("SAM_TcstroughPhysical", "q_sby_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_startup_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_frac", &result))
		make_access_error("SAM_TcstroughPhysical", "startup_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_startup_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_time", &result))
		make_access_error("SAM_TcstroughPhysical", "startup_time");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_tech_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tech_type", &result))
		make_access_error("SAM_TcstroughPhysical", "tech_type");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_T_amb_des", &result))
		make_access_error("SAM_TcstroughPhysical", "ud_T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_T_amb_high", &result))
		make_access_error("SAM_TcstroughPhysical", "ud_T_amb_high");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_T_amb_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "ud_T_amb_ind_od");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_T_amb_low", &result))
		make_access_error("SAM_TcstroughPhysical", "ud_T_amb_low");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_T_htf_high", &result))
		make_access_error("SAM_TcstroughPhysical", "ud_T_htf_high");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_T_htf_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "ud_T_htf_ind_od");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_T_htf_low", &result))
		make_access_error("SAM_TcstroughPhysical", "ud_T_htf_low");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_f_W_dot_cool_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_f_W_dot_cool_des", &result))
		make_access_error("SAM_TcstroughPhysical", "ud_f_W_dot_cool_des");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_UserDefinedPC_ud_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "ud_ind_od");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_m_dot_htf_high", &result))
		make_access_error("SAM_TcstroughPhysical", "ud_m_dot_htf_high");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_m_dot_htf_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "ud_m_dot_htf_ind_od");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_m_dot_htf_low", &result))
		make_access_error("SAM_TcstroughPhysical", "ud_m_dot_htf_low");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_water_cool_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_m_dot_water_cool_des", &result))
		make_access_error("SAM_TcstroughPhysical", "ud_m_dot_water_cool_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Enet_eta_lhv_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_lhv", &result))
		make_access_error("SAM_TcstroughPhysical", "eta_lhv");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Enet_eta_tes_htr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_tes_htr", &result))
		make_access_error("SAM_TcstroughPhysical", "eta_tes_htr");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_CosTh_ave_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "CosTh_ave", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "CosTh_ave");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_DP_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "DP_tot", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "DP_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_E_bal_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "E_bal_startup", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "E_bal_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_EndLoss_ave_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EndLoss_ave", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "EndLoss_ave");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_EqOpteff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EqOpteff", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "EqOpteff");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Fuel_usage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Fuel_usage", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Fuel_usage");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_IAM_ave_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "IAM_ave", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "IAM_ave");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Pipe_hl_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Pipe_hl", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Pipe_hl");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Q_aux_backup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_aux_backup", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Q_aux_backup");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Q_par_sf_fp_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_par_sf_fp", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Q_par_sf_fp");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Q_par_tes_fp_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_par_tes_fp", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Q_par_tes_fp");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_RowShadow_ave_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "RowShadow_ave", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "RowShadow_ave");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_SCA_par_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "SCA_par_tot", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "SCA_par_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_SCAs_def_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "SCAs_def", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "SCAs_def");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_field_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_field_in", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "T_field_in");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_pb_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pb_in", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "T_pb_in");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_pb_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pb_out", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "T_pb_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_sys_c_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_sys_c", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "T_sys_c");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_sys_h_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_sys_h", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "T_sys_h");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_tank_cold_fin_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tank_cold_fin", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "T_tank_cold_fin");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_tank_cold_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tank_cold_in", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "T_tank_cold_in");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_tank_hot_fin_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tank_hot_fin", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "T_tank_hot_fin");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_tank_hot_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tank_hot_in", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "T_tank_hot_in");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Theta_ave_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Theta_ave", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Theta_ave");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Ts_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Ts_cold", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Ts_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Ts_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Ts_hot", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "Ts_hot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_W_cool_par_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_cool_par", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "W_cool_par");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_W_cycle_gross_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_cycle_gross", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "W_cycle_gross");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_W_dot_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_pump", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "W_dot_pump");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_W_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_net", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "W_net");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_W_cycle_gross_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_W_cycle_gross", &result))
		make_access_error("SAM_TcstroughPhysical", "annual_W_cycle_gross");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_TcstroughPhysical", "annual_energy");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_fuel_usage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_fuel_usage", &result))
		make_access_error("SAM_TcstroughPhysical", "annual_fuel_usage");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_q_abs_tot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_abs_tot", &result))
		make_access_error("SAM_TcstroughPhysical", "annual_q_abs_tot");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_q_aux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_aux", &result))
		make_access_error("SAM_TcstroughPhysical", "annual_q_aux");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_q_avail_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_avail", &result))
		make_access_error("SAM_TcstroughPhysical", "annual_q_avail");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_q_dump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_dump", &result))
		make_access_error("SAM_TcstroughPhysical", "annual_q_dump");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_q_inc_sf_tot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_inc_sf_tot", &result))
		make_access_error("SAM_TcstroughPhysical", "annual_q_inc_sf_tot");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_q_pb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_pb", &result))
		make_access_error("SAM_TcstroughPhysical", "annual_q_pb");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_q_to_tes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_to_tes", &result))
		make_access_error("SAM_TcstroughPhysical", "annual_q_to_tes");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_total_water_use_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_total_water_use", &result))
		make_access_error("SAM_TcstroughPhysical", "annual_total_water_use");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_aux_par_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "aux_par", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "aux_par");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "beam");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_bop_par_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "bop_par", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "bop_par");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_TcstroughPhysical", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Outputs_conversion_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conversion_factor", &result))
		make_access_error("SAM_TcstroughPhysical", "conversion_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_dni_costh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dni_costh", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "dni_costh");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_eta_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "eta");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_fixed_par_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fixed_par", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "fixed_par");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_hour_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "hour");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "htf_pump_power", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "htf_pump_power");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_TcstroughPhysical", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_m_dot_aux_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_aux", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "m_dot_aux");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_m_dot_avail_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_avail", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "m_dot_avail");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_m_dot_charge_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_charge_field", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "m_dot_charge_field");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_m_dot_discharge_tank_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_discharge_tank", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "m_dot_discharge_tank");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_m_dot_htf2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_htf2", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "m_dot_htf2");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_m_dot_makeup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_makeup", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "m_dot_makeup");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_m_dot_pb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pb", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "m_dot_pb");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_mass_tank_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tank_cold", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "mass_tank_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_mass_tank_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tank_hot", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "mass_tank_hot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "month");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_Fuel_usage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_Fuel_usage", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "monthly_Fuel_usage");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_W_cycle_gross_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_W_cycle_gross", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "monthly_W_cycle_gross");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "monthly_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_m_dot_makeup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_m_dot_makeup", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "monthly_m_dot_makeup");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_q_abs_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_abs_tot", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "monthly_q_abs_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_q_avail_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_avail", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "monthly_q_avail");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_q_dump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_dump", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "monthly_q_dump");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_q_inc_sf_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_inc_sf_tot", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "monthly_q_inc_sf_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_q_pb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_pb", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "monthly_q_pb");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_q_to_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_to_tes", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "monthly_q_to_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_P_dsn", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_header_P_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_T_dsn", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_header_T_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_diams_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_diams", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_header_diams");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_expansions_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_expansions", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_header_expansions");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_lengths_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_lengths", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_header_lengths");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_mdot_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_mdot_dsn", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_header_mdot_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_vel_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_vel_dsn", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_header_vel_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_wallthk_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_header_wallthk", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_header_wallthk");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_loop_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_loop_P_dsn", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_loop_P_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_loop_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_loop_T_dsn", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_loop_T_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_P_dsn", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_runner_P_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_T_dsn", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_runner_T_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_diams_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_diams", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_runner_diams");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_expansions_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_expansions", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_runner_expansions");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_lengths_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_lengths", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_runner_lengths");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_mdot_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_mdot_dsn", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_runner_mdot_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_vel_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_vel_dsn", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_runner_vel_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_wallthk_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_runner_wallthk", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_runner_wallthk");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_sgs_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_sgs_P_dsn", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_sgs_P_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_sgs_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_sgs_T_dsn", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_sgs_T_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_sgs_diams_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_sgs_diams", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_sgs_diams");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_sgs_mdot_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_sgs_mdot_dsn", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_sgs_mdot_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_sgs_vel_dsn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_sgs_vel_dsn", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_sgs_vel_dsn");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_sgs_wallthk_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pipe_sgs_wallthk", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pipe_sgs_wallthk");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pres_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pres", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "pres");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_abs_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_abs_tot", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "q_abs_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_avail_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_avail", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "q_avail");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_dump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dump", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "q_dump");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_inc_sf_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_inc_sf_tot", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "q_inc_sf_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_loss_spec_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_loss_spec_tot", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "q_loss_spec_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_loss_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_loss_tot", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "q_loss_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_pb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pb", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "q_pb");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_to_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_to_tes", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "q_to_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_qinc_costh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "qinc_costh", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "qinc_costh");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_recirculating_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "recirculating", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "recirculating");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_solazi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solazi", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "solazi");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solzen", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "solzen");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughPhysical_Outputs_system_heat_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_heat_rate", &result))
		make_access_error("SAM_TcstroughPhysical", "system_heat_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_tank_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tank_losses", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "tank_losses");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "tdry");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_tou_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tou_value", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "tou_value");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "twet");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_vol_tank_cold_fin_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "vol_tank_cold_fin", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "vol_tank_cold_fin");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_vol_tank_hot_fin_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "vol_tank_hot_fin", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "vol_tank_hot_fin");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_vol_tank_total_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "vol_tank_total", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "vol_tank_total");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_TcstroughPhysical", "wspd");
	});
	return result;
}




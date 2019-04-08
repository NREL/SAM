#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_TroughPhysicalProcessHeat.h"

SAM_EXPORT SAM_TroughPhysicalProcessHeat SAM_TroughPhysicalProcessHeat_construct(const char* def, SAM_error* err){
	SAM_TroughPhysicalProcessHeat result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_TroughPhysicalProcessHeat_execute(SAM_TroughPhysicalProcessHeat data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("trough_physical_process_heat", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_TroughPhysicalProcessHeat_destruct(SAM_TroughPhysicalProcessHeat system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Weather_azimuth_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "azimuth", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Weather_file_name_sset(SAM_TroughPhysicalProcessHeat ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "file_name", str);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Weather_tilt_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tilt", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Weather_track_mode_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "track_mode", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_A_aperture_aset(SAM_TroughPhysicalProcessHeat ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "A_aperture", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_AbsorberMaterial_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "AbsorberMaterial", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_AnnulusGas_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "AnnulusGas", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Ave_Focal_Length_aset(SAM_TroughPhysicalProcessHeat ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Ave_Focal_Length", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_ColperSCA_aset(SAM_TroughPhysicalProcessHeat ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ColperSCA", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_D_2_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_2", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_D_3_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_3", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_D_4_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_4", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_D_5_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_5", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_D_cpnt_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_cpnt", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_D_p_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_p", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Design_loss_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Design_loss", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Dirt_HCE_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Dirt_HCE", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Dirt_mirror_aset(SAM_TroughPhysicalProcessHeat ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Dirt_mirror", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Distance_SCA_aset(SAM_TroughPhysicalProcessHeat ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Distance_SCA", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_EPSILON_4_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "EPSILON_4", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_EPSILON_5_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "EPSILON_5", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Error_aset(SAM_TroughPhysicalProcessHeat ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Error", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_FieldConfig_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "FieldConfig", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Flow_type_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Flow_type", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Fluid_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Fluid", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_GeomEffects_aset(SAM_TroughPhysicalProcessHeat ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "GeomEffects", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_GlazingIntactIn_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "GlazingIntactIn", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_HCE_FieldFrac_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "HCE_FieldFrac", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_HDR_rough_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HDR_rough", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_IAM_matrix_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "IAM_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_I_bn_des_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_bn_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_K_cpnt_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "K_cpnt", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_L_SCA_aset(SAM_TroughPhysicalProcessHeat ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "L_SCA", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_L_aperture_aset(SAM_TroughPhysicalProcessHeat ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "L_aperture", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_L_cpnt_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "L_cpnt", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_L_heat_sink_piping_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_heat_sink_piping", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_L_rnr_per_xpan_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_rnr_per_xpan", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_L_xpan_hdr_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_xpan_hdr", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_L_xpan_rnr_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_xpan_rnr", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Min_rnr_xpans_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Min_rnr_xpans", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_N_hdr_per_xpan_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "N_hdr_per_xpan", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_N_max_hdr_diams_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "N_max_hdr_diams", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_P_a_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "P_a", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Pipe_hl_coef_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Pipe_hl_coef", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Rho_mirror_clean_aset(SAM_TroughPhysicalProcessHeat ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Rho_mirror_clean", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Rough_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Rough", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Row_Distance_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Row_Distance", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_SCADefocusArray_aset(SAM_TroughPhysicalProcessHeat ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "SCADefocusArray", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_SCAInfoArray_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "SCAInfoArray", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_SCA_drives_elec_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SCA_drives_elec", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Shadowing_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Shadowing", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_T_fp_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_fp", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_T_loop_in_des_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_in_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_T_loop_out_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_out", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Tau_envelope_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Tau_envelope", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_TrackingError_aset(SAM_TroughPhysicalProcessHeat ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "TrackingError", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Type_cpnt_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Type_cpnt", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_V_hdr_max_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_max", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_V_hdr_min_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_min", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_W_aperture_aset(SAM_TroughPhysicalProcessHeat ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "W_aperture", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_accept_init_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "accept_init", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_accept_loc_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "accept_loc", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_accept_mode_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "accept_mode", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_alpha_abs_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "alpha_abs", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_alpha_env_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "alpha_env", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_calc_design_pipe_vals_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calc_design_pipe_vals", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_custom_sf_pipe_sizes_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "custom_sf_pipe_sizes", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_11_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_11", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_12_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_12", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_13_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_13", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_14_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_14", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_21_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_21", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_22_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_22", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_23_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_23", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_24_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_24", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_31_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_31", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_32_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_32", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_33_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_33", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_34_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_34", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_41_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_41", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_42_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_42", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_43_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_43", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_44_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_44", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_eta_pump_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pump", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_is_model_heat_sink_piping_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_model_heat_sink_piping", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_m_dot_htfmax_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htfmax", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_m_dot_htfmin_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htfmin", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_mc_bal_cold_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_cold", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_mc_bal_hot_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_hot", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_mc_bal_sca_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_sca", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_nColt_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nColt", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_nHCEVar_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nHCEVar", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_nHCEt_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nHCEt", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_nLoops_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nLoops", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_nSCA_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nSCA", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_northsouth_field_sep_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "northsouth_field_sep", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_offset_xpan_hdr_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "offset_xpan_hdr", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_sf_hdr_diams_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sf_hdr_diams", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_sf_hdr_lengths_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sf_hdr_lengths", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_sf_hdr_wallthicks_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sf_hdr_wallthicks", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_sf_rnr_diams_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sf_rnr_diams", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_sf_rnr_lengths_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sf_rnr_lengths", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_sf_rnr_wallthicks_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sf_rnr_wallthicks", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_solar_mult_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "solar_mult", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_theta_dep_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_dep", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_theta_stow_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_stow", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_washing_frequency_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "washing_frequency", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_water_usage_per_wash_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "water_usage_per_wash", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_wind_stow_speed_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wind_stow_speed", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Controller_field_fl_props_mset(SAM_TroughPhysicalProcessHeat ptr, float* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "field_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Controller_pb_pump_coef_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_pump_coef", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Controller_q_pb_design_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_pb_design", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SystemDesign_tshours_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tshours", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_TES_cold_tank_Thtr_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_TES_cold_tank_max_heat_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_TES_h_tank_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_TES_init_hot_htf_percent_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "init_hot_htf_percent", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_TES_tank_pairs_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_pairs", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_TES_u_tank_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_TES2tank_h_tank_min_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_min", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_TES2tank_hot_tank_Thtr_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_TES2tank_hot_tank_max_heat_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Powerblock_L_rnr_pb_fset(SAM_TroughPhysicalProcessHeat ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_rnr_pb", number);
	});
}

SAM_EXPORT float SAM_TroughPhysicalProcessHeat_Weather_azimuth_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "azimuth", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "azimuth");
	});
	return result;
}



SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Weather_file_name_sget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "file_name");
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "file_name");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_Weather_tilt_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tilt", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "tilt");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_Weather_track_mode_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "track_mode", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "track_mode");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_A_aperture_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "A_aperture", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "A_aperture");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_AbsorberMaterial_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "AbsorberMaterial", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "AbsorberMaterial");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_AnnulusGas_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "AnnulusGas", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "AnnulusGas");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_Ave_Focal_Length_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Ave_Focal_Length", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "Ave_Focal_Length");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_ColperSCA_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ColperSCA", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "ColperSCA");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_D_2_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_2", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "D_2");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_D_3_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_3", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "D_3");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_D_4_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_4", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "D_4");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_D_5_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_5", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "D_5");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_D_cpnt_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_cpnt", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "D_cpnt");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_D_p_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_p", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "D_p");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_Design_loss_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Design_loss", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "Design_loss");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_Dirt_HCE_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Dirt_HCE", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "Dirt_HCE");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_Dirt_mirror_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Dirt_mirror", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "Dirt_mirror");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_Distance_SCA_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Distance_SCA", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "Distance_SCA");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_EPSILON_4_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "EPSILON_4", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "EPSILON_4");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_EPSILON_5_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "EPSILON_5", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "EPSILON_5");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_Error_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Error", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "Error");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_FieldConfig_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "FieldConfig", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "FieldConfig");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_Flow_type_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Flow_type", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "Flow_type");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_Fluid_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Fluid", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "Fluid");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_GeomEffects_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "GeomEffects", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "GeomEffects");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_GlazingIntactIn_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "GlazingIntactIn", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "GlazingIntactIn");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_HCE_FieldFrac_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "HCE_FieldFrac", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "HCE_FieldFrac");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_HDR_rough_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HDR_rough", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "HDR_rough");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_IAM_matrix_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "IAM_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "IAM_matrix");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_I_bn_des_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_bn_des", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "I_bn_des");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_K_cpnt_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "K_cpnt", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "K_cpnt");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_L_SCA_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "L_SCA", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "L_SCA");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_L_aperture_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "L_aperture", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "L_aperture");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_L_cpnt_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "L_cpnt", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "L_cpnt");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_L_heat_sink_piping_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_heat_sink_piping", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "L_heat_sink_piping");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_L_rnr_per_xpan_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_rnr_per_xpan", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "L_rnr_per_xpan");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_L_xpan_hdr_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_xpan_hdr", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "L_xpan_hdr");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_L_xpan_rnr_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_xpan_rnr", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "L_xpan_rnr");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_Min_rnr_xpans_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Min_rnr_xpans", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "Min_rnr_xpans");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_N_hdr_per_xpan_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_hdr_per_xpan", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "N_hdr_per_xpan");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_N_max_hdr_diams_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_max_hdr_diams", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "N_max_hdr_diams");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_P_a_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "P_a", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "P_a");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_Pipe_hl_coef_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Pipe_hl_coef", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "Pipe_hl_coef");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_Rho_mirror_clean_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Rho_mirror_clean", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "Rho_mirror_clean");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_Rough_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Rough", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "Rough");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_Row_Distance_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Row_Distance", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "Row_Distance");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_SCADefocusArray_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "SCADefocusArray", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "SCADefocusArray");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_SCAInfoArray_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "SCAInfoArray", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "SCAInfoArray");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_SCA_drives_elec_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SCA_drives_elec", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "SCA_drives_elec");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_Shadowing_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Shadowing", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "Shadowing");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_T_fp_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_fp", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "T_fp");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_T_loop_in_des_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_in_des", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "T_loop_in_des");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_T_loop_out_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_out", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "T_loop_out");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_Tau_envelope_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Tau_envelope", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "Tau_envelope");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_TrackingError_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "TrackingError", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "TrackingError");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_Type_cpnt_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Type_cpnt", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "Type_cpnt");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_V_hdr_max_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_max", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "V_hdr_max");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_V_hdr_min_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_min", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "V_hdr_min");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_W_aperture_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_aperture", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "W_aperture");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_accept_init_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "accept_init", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "accept_init");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_accept_loc_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "accept_loc", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "accept_loc");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_accept_mode_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "accept_mode", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "accept_mode");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_alpha_abs_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "alpha_abs", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "alpha_abs");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_alpha_env_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "alpha_env", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "alpha_env");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_calc_design_pipe_vals_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calc_design_pipe_vals", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "calc_design_pipe_vals");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_custom_sf_pipe_sizes_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "custom_sf_pipe_sizes", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "custom_sf_pipe_sizes");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_11_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_11", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_11");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_12_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_12", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_12");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_13_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_13", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_13");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_14_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_14", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_14");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_21_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_21", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_21");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_22_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_22", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_22");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_23_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_23", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_23");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_24_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_24", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_24");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_31_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_31", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_31");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_32_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_32", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_32");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_33_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_33", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_33");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_34_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_34", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_34");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_41_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_41", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_41");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_42_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_42", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_42");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_43_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_43", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_43");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_44_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_44", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "epsilon_3_44");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_eta_pump_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pump", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "eta_pump");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_is_model_heat_sink_piping_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_model_heat_sink_piping", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "is_model_heat_sink_piping");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_m_dot_htfmax_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmax", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "m_dot_htfmax");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_m_dot_htfmin_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmin", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "m_dot_htfmin");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_mc_bal_cold_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_cold", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "mc_bal_cold");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_mc_bal_hot_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_hot", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "mc_bal_hot");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_mc_bal_sca_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_sca", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "mc_bal_sca");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_nColt_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nColt", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "nColt");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_nHCEVar_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nHCEVar", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "nHCEVar");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_nHCEt_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nHCEt", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "nHCEt");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_nLoops_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nLoops", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "nLoops");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_nSCA_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nSCA", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "nSCA");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_northsouth_field_sep_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "northsouth_field_sep", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "northsouth_field_sep");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_offset_xpan_hdr_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "offset_xpan_hdr", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "offset_xpan_hdr");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_sf_hdr_diams_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sf_hdr_diams", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "sf_hdr_diams");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_sf_hdr_lengths_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sf_hdr_lengths", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "sf_hdr_lengths");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_sf_hdr_wallthicks_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sf_hdr_wallthicks", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "sf_hdr_wallthicks");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_sf_rnr_diams_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sf_rnr_diams", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "sf_rnr_diams");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_sf_rnr_lengths_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sf_rnr_lengths", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "sf_rnr_lengths");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_sf_rnr_wallthicks_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sf_rnr_wallthicks", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "sf_rnr_wallthicks");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_solar_mult_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_mult", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "solar_mult");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_theta_dep_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_dep", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "theta_dep");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_theta_stow_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_stow", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "theta_stow");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_washing_frequency_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "washing_frequency", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "washing_frequency");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_water_usage_per_wash_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "water_usage_per_wash", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "water_usage_per_wash");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_wind_stow_speed_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wind_stow_speed", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "wind_stow_speed");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Controller_field_fl_props_mget(SAM_TroughPhysicalProcessHeat ptr, int* nrows, int* ncols, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "field_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "field_fl_props");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_Controller_pb_pump_coef_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_pump_coef", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "pb_pump_coef");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_Controller_q_pb_design_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_pb_design", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "q_pb_design");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SystemDesign_tshours_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "tshours");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_TES_cold_tank_Thtr_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_Thtr", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "cold_tank_Thtr");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_TES_cold_tank_max_heat_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_max_heat", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "cold_tank_max_heat");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_TES_h_tank_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "h_tank");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_TES_init_hot_htf_percent_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "init_hot_htf_percent", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "init_hot_htf_percent");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_TES_tank_pairs_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_pairs", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "tank_pairs");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_TES_u_tank_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_tank", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "u_tank");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_TES2tank_h_tank_min_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_min", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "h_tank_min");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_TES2tank_hot_tank_Thtr_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_Thtr", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "hot_tank_Thtr");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_TES2tank_hot_tank_max_heat_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_max_heat", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "hot_tank_max_heat");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_Powerblock_L_rnr_pb_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_rnr_pb", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "L_rnr_pb");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_CosTh_ave_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "CosTh_ave", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "CosTh_ave");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_EndLoss_ave_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EndLoss_ave", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "EndLoss_ave");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_EqOpteff_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EqOpteff", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "EqOpteff");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_IAM_ave_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "IAM_ave", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "IAM_ave");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_RowShadow_ave_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "RowShadow_ave", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "RowShadow_ave");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_SCAs_def_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "SCAs_def", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "SCAs_def");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_T_field_cold_in_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_field_cold_in", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "T_field_cold_in");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_T_field_hot_out_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_field_hot_out", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "T_field_hot_out");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_T_heat_sink_in_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_heat_sink_in", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "T_heat_sink_in");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_T_heat_sink_out_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_heat_sink_out", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "T_heat_sink_out");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_T_rec_cold_in_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_cold_in", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "T_rec_cold_in");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_T_rec_hot_out_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_hot_out", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "T_rec_hot_out");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_T_tes_cold_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_cold", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "T_tes_cold");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_T_tes_hot_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_hot", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "T_tes_hot");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_Theta_ave_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Theta_ave", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "Theta_ave");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_W_dot_field_pump_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_field_pump", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "W_dot_field_pump");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_W_dot_parasitic_tot_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_parasitic_tot", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "W_dot_parasitic_tot");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_W_dot_pc_pump_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_pc_pump", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "W_dot_pc_pump");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_W_dot_sca_track_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_sca_track", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "W_dot_sca_track");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_Outputs_annual_electricity_consumption_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_electricity_consumption", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "annual_electricity_consumption");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_Outputs_annual_energy_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "annual_energy");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_Outputs_annual_field_freeze_protection_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_field_freeze_protection", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "annual_field_freeze_protection");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_Outputs_annual_gross_energy_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_gross_energy", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "annual_gross_energy");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_Outputs_annual_tes_freeze_protection_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_tes_freeze_protection", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "annual_tes_freeze_protection");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_Outputs_annual_thermal_consumption_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_thermal_consumption", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "annual_thermal_consumption");
	});
	return result;
}



SAM_EXPORT float SAM_TroughPhysicalProcessHeat_Outputs_annual_total_water_use_fget(SAM_TroughPhysicalProcessHeat ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_total_water_use", &result))
		make_access_error("SAM_TroughPhysicalProcessHeat", "annual_total_water_use");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_beam_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "beam");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_deltaP_field_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "deltaP_field", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "deltaP_field");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_dni_costh_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dni_costh", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "dni_costh");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_e_ch_tes_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_ch_tes", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "e_ch_tes");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_e_dot_field_int_energy_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_dot_field_int_energy", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "e_dot_field_int_energy");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_hour_day_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour_day", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "hour_day");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_m_dot_balance_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_balance", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "m_dot_balance");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_m_dot_field_delivered_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_field_delivered", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "m_dot_field_delivered");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_m_dot_field_recirc_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_field_recirc", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "m_dot_field_recirc");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_m_dot_htf_heat_sink_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_htf_heat_sink", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "m_dot_htf_heat_sink");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_m_dot_loop_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_loop", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "m_dot_loop");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_m_dot_tes_ch_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_ch", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "m_dot_tes_ch");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_m_dot_tes_dc_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_dc", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "m_dot_tes_dc");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_month_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "month");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_op_mode_1_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_1", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "op_mode_1");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_op_mode_2_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_2", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "op_mode_2");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_op_mode_3_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_3", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "op_mode_3");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_pres_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pres", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "pres");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_q_balance_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_balance", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "q_balance");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_q_ch_tes_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_ch_tes", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "q_ch_tes");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_q_dc_tes_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dc_tes", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "q_dc_tes");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_q_dot_freeze_prot_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_freeze_prot", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "q_dot_freeze_prot");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_q_dot_htf_sf_out_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_htf_sf_out", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "q_dot_htf_sf_out");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_q_dot_piping_loss_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_piping_loss", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "q_dot_piping_loss");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_q_dot_rec_abs_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_abs", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "q_dot_rec_abs");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_q_dot_rec_inc_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_inc", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "q_dot_rec_inc");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_q_dot_rec_thermal_loss_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_thermal_loss", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "q_dot_rec_thermal_loss");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_q_dot_to_heat_sink_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_to_heat_sink", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "q_dot_to_heat_sink");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_q_inc_sf_tot_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_inc_sf_tot", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "q_inc_sf_tot");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_q_tes_heater_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_tes_heater", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "q_tes_heater");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_qinc_costh_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "qinc_costh", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "qinc_costh");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_solazi_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solazi", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "solazi");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_solzen_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solzen", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "solzen");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_tank_losses_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tank_losses", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "tank_losses");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_tdry_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "tdry");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_time_hr_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "time_hr", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "time_hr");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_twet_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "twet");
	});
	return result;
}



SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Outputs_wspd_aget(SAM_TroughPhysicalProcessHeat ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalProcessHeat", "wspd");
	});
	return result;
}




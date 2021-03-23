#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_TroughPhysicalCspSolver.h"

SAM_EXPORT int SAM_TroughPhysicalCspSolver_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("trough_physical_csp_solver", data, verbosity, err);
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Weather_azimuth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "azimuth", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Weather_file_name_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "file_name", str);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Weather_tilt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tilt", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Weather_track_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "track_mode", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Trough_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_TimeOfDelivery_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_factors_ts", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_TimeOfDelivery_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_multiplier_model", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_A_aperture_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "A_aperture", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_AbsorberMaterial_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "AbsorberMaterial", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_AnnulusGas_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "AnnulusGas", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_Ave_Focal_Length_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Ave_Focal_Length", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_ColperSCA_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ColperSCA", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_D_2_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_2", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_D_3_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_3", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_D_4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_4", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_D_5_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_5", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_D_p_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_p", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_Design_loss_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Design_loss", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_Dirt_HCE_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Dirt_HCE", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_Dirt_mirror_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Dirt_mirror", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_Distance_SCA_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Distance_SCA", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_EPSILON_4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "EPSILON_4", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_EPSILON_5_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "EPSILON_5", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_Error_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Error", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_FieldConfig_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "FieldConfig", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_Flow_type_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Flow_type", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_Fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Fluid", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_GeomEffects_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "GeomEffects", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_GlazingIntactIn_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "GlazingIntactIn", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_HCE_FieldFrac_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "HCE_FieldFrac", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_HDR_rough_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HDR_rough", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_IAM_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "IAM_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_I_bn_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_bn_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_L_SCA_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "L_SCA", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_L_aperture_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "L_aperture", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_P_a_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "P_a", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_P_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_ref", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_Pipe_hl_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Pipe_hl_coef", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_Rho_mirror_clean_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Rho_mirror_clean", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_Rough_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Rough", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_Row_Distance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Row_Distance", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_SCADefocusArray_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "SCADefocusArray", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_SCAInfoArray_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "SCAInfoArray", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_SCA_drives_elec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SCA_drives_elec", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_Shadowing_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Shadowing", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_T_fp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_fp", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_T_loop_in_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_in_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_T_loop_out_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_out", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_T_startup_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_startup", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_Tau_envelope_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Tau_envelope", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_TrackingError_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "TrackingError", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_V_hdr_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_max", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_V_hdr_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_hdr_min", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_W_aperture_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "W_aperture", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_accept_init_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "accept_init", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_accept_loc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "accept_loc", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_accept_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "accept_mode", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_alpha_abs_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "alpha_abs", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_alpha_env_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "alpha_env", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_11_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_11", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_12_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_12", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_13_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_13", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_14_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_14", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_21_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_21", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_22_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_22", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_23_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_23", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_24_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_24", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_31_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_31", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_32_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_32", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_33_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_33", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_34_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_34", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_41_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_41", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_42_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_42", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_43_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_43", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_44_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "epsilon_3_44", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_eta_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pump", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_fthrctrl_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fthrctrl", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_fthrok_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fthrok", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_m_dot_htfmax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htfmax", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_m_dot_htfmin_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htfmin", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_mc_bal_cold_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_cold", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_mc_bal_hot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_hot", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_mc_bal_sca_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mc_bal_sca", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_nColt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nColt", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_nHCEVar_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nHCEVar", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_nHCEt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nHCEt", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_nLoops_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nLoops", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_nSCA_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nSCA", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_solar_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "solar_mult", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_theta_dep_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_dep", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_theta_stow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_stow", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_washing_frequency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "washing_frequency", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_SolarField_water_usage_per_wash_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "water_usage_per_wash", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_T_set_aux_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_set_aux", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_T_tank_cold_ini_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_tank_cold_ini", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_V_tank_hot_ini_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_tank_hot_ini", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_W_pb_design_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "W_pb_design", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_aux_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "aux_array", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_bop_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "bop_array", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_cycle_cutoff_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_cutoff_frac", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_cycle_max_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_max_frac", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_dt_cold_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_cold", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_dt_hot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_hot", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_f_tc_cold_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_tc_cold", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_ffrac_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ffrac", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "field_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_fossil_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fossil_mode", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_h_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_min", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_hx_config_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hx_config", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_is_hx_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_hx", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_nodes_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nodes", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_fixed_par", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_pump_coef", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_q_max_aux_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_max_aux", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_q_pb_design_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_pb_design", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_sby_frac", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_sf_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sf_type", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_store_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "store_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_store_fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "store_fluid", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_t_ch_out_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "t_ch_out_max", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_t_dis_out_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "t_dis_out_min", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_t_standby_reset_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "t_standby_reset", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_pairs", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_tc_fill_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tc_fill", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_tc_void_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tc_void", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_tes_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pump_coef", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_tes_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_type", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_tshours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tshours", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_tslogic_a_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "tslogic_a", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_tslogic_b_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "tslogic_b", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_tslogic_c_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "tslogic_c", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_u_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Controller_vol_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "vol_tank", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_TouTranslator_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekday_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_TouTranslator_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekend_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_CT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_F_wc_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "F_wc", arr, length);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_P_boil_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_boil", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_P_cond_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_min", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_P_cond_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_ratio", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ITD_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_T_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_approach", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_dT_cw_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_cw_ref", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_eta_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_ref", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_n_pl_inc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_pl_inc", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_pb_bd_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_bd_frac", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_pc_config_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pc_config", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_sby_frac", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_startup_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_frac", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_startup_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_time", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Powerblock_tech_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tech_type", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_T_amb_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_T_amb_high_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_T_amb_high", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_T_amb_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_T_amb_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_T_amb_low_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_T_amb_low", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_T_htf_high_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_T_htf_high", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_T_htf_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_T_htf_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_T_htf_low_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_T_htf_low", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_f_W_dot_cool_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_f_W_dot_cool_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_m_dot_htf_high_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_m_dot_htf_high", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_m_dot_htf_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_m_dot_htf_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_m_dot_htf_low_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_m_dot_htf_low", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_m_dot_water_cool_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_m_dot_water_cool_des", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Enet_eta_lhv_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_lhv", number);
	});
}

SAM_EXPORT void SAM_TroughPhysicalCspSolver_Enet_eta_tes_htr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_tes_htr", number);
	});
}

SAM_EXPORT double SAM_TroughPhysicalCspSolver_Weather_azimuth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "azimuth", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "azimuth");
	});
	return result;
}



SAM_EXPORT const char* SAM_TroughPhysicalCspSolver_Weather_file_name_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "file_name");
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "file_name");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Weather_tilt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tilt", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "tilt");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Weather_track_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "track_mode", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "track_mode");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Trough_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "system_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_TimeOfDelivery_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_factors_ts", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "dispatch_factors_ts");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_TimeOfDelivery_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_multiplier_model", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "ppa_multiplier_model");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_A_aperture_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "A_aperture", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "A_aperture");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_AbsorberMaterial_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "AbsorberMaterial", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "AbsorberMaterial");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_AnnulusGas_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "AnnulusGas", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "AnnulusGas");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_Ave_Focal_Length_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Ave_Focal_Length", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "Ave_Focal_Length");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_ColperSCA_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ColperSCA", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "ColperSCA");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_D_2_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_2", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "D_2");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_D_3_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_3", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "D_3");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_D_4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_4", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "D_4");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_D_5_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_5", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "D_5");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_D_p_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_p", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "D_p");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_Design_loss_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Design_loss", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "Design_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_Dirt_HCE_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Dirt_HCE", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "Dirt_HCE");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_Dirt_mirror_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Dirt_mirror", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "Dirt_mirror");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_Distance_SCA_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Distance_SCA", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "Distance_SCA");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_EPSILON_4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "EPSILON_4", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "EPSILON_4");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_EPSILON_5_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "EPSILON_5", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "EPSILON_5");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_Error_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Error", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "Error");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_FieldConfig_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "FieldConfig", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "FieldConfig");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_Flow_type_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Flow_type", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "Flow_type");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_Fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Fluid", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "Fluid");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_GeomEffects_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "GeomEffects", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "GeomEffects");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_GlazingIntactIn_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "GlazingIntactIn", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "GlazingIntactIn");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_HCE_FieldFrac_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "HCE_FieldFrac", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "HCE_FieldFrac");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_HDR_rough_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HDR_rough", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "HDR_rough");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_IAM_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "IAM_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "IAM_matrix");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_I_bn_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_bn_des", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "I_bn_des");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_L_SCA_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "L_SCA", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "L_SCA");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_L_aperture_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "L_aperture", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "L_aperture");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_P_a_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "P_a", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "P_a");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_P_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_ref", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "P_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_Pipe_hl_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Pipe_hl_coef", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "Pipe_hl_coef");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_Rho_mirror_clean_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Rho_mirror_clean", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "Rho_mirror_clean");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_Rough_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Rough", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "Rough");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_Row_Distance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Row_Distance", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "Row_Distance");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_SCADefocusArray_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "SCADefocusArray", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "SCADefocusArray");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_SCAInfoArray_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "SCAInfoArray", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "SCAInfoArray");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_SCA_drives_elec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SCA_drives_elec", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "SCA_drives_elec");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_Shadowing_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Shadowing", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "Shadowing");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_T_fp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_fp", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "T_fp");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_T_loop_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_in_des", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "T_loop_in_des");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_T_loop_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_out", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "T_loop_out");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_T_startup_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_startup", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "T_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_Tau_envelope_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Tau_envelope", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "Tau_envelope");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_TrackingError_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "TrackingError", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "TrackingError");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_V_hdr_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_max", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "V_hdr_max");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_V_hdr_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_hdr_min", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "V_hdr_min");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_W_aperture_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_aperture", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "W_aperture");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_accept_init_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "accept_init", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "accept_init");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_accept_loc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "accept_loc", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "accept_loc");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_accept_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "accept_mode", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "accept_mode");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_alpha_abs_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "alpha_abs", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "alpha_abs");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_alpha_env_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "alpha_env", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "alpha_env");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_11_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_11", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_11");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_12_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_12", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_12");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_13_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_13", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_13");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_14_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_14", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_14");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_21_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_21", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_21");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_22_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_22", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_22");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_23_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_23", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_23");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_24_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_24", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_24");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_31_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_31", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_31");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_32_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_32", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_32");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_33_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_33", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_33");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_34_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_34", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_34");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_41_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_41", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_41");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_42_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_42", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_42");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_43_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_43", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_43");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_SolarField_epsilon_3_44_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "epsilon_3_44", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "epsilon_3_44");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_eta_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pump", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "eta_pump");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_fthrctrl_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fthrctrl", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "fthrctrl");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_fthrok_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fthrok", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "fthrok");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_m_dot_htfmax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmax", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "m_dot_htfmax");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_m_dot_htfmin_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htfmin", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "m_dot_htfmin");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_mc_bal_cold_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_cold", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "mc_bal_cold");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_mc_bal_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_hot", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "mc_bal_hot");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_mc_bal_sca_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_bal_sca", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "mc_bal_sca");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_nColt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nColt", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "nColt");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_nHCEVar_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nHCEVar", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "nHCEVar");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_nHCEt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nHCEt", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "nHCEt");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_nLoops_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nLoops", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "nLoops");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_nSCA_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nSCA", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "nSCA");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_solar_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_mult", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "solar_mult");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_theta_dep_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_dep", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "theta_dep");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_theta_stow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_stow", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "theta_stow");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_washing_frequency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "washing_frequency", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "washing_frequency");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_SolarField_water_usage_per_wash_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "water_usage_per_wash", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "water_usage_per_wash");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_T_set_aux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_set_aux", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "T_set_aux");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_T_tank_cold_ini_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_tank_cold_ini", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "T_tank_cold_ini");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_V_tank_hot_ini_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tank_hot_ini", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "V_tank_hot_ini");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_W_pb_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_pb_design", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "W_pb_design");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Controller_aux_array_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "aux_array", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "aux_array");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Controller_bop_array_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "bop_array", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "bop_array");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_Thtr", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "cold_tank_Thtr");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_cycle_cutoff_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_cutoff_frac", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "cycle_cutoff_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_cycle_max_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_max_frac", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "cycle_max_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_dt_cold_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_cold", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "dt_cold");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_dt_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_hot", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "dt_hot");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_f_tc_cold_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_tc_cold", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "f_tc_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Controller_ffrac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ffrac", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "ffrac");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Controller_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "field_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "field_fl_props");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_fossil_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fossil_mode", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "fossil_mode");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_h_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "h_tank");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_h_tank_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_min", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "h_tank_min");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_Thtr", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "hot_tank_Thtr");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_hx_config_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hx_config", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "hx_config");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_is_hx_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_hx", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "is_hx");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_nodes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nodes", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "nodes");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_pb_fixed_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_fixed_par", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "pb_fixed_par");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_pb_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_pump_coef", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "pb_pump_coef");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_q_max_aux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_max_aux", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "q_max_aux");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_q_pb_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_pb_design", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "q_pb_design");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_q_sby_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_sby_frac", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "q_sby_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_sf_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sf_type", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "sf_type");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Controller_store_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "store_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "store_fl_props");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_store_fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "store_fluid", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "store_fluid");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_t_ch_out_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_ch_out_max", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "t_ch_out_max");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_t_dis_out_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_dis_out_min", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "t_dis_out_min");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_t_standby_reset_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_standby_reset", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "t_standby_reset");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_max_heat", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "tank_max_heat");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_tank_pairs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_pairs", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "tank_pairs");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_tc_fill_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tc_fill", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "tc_fill");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_tc_void_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tc_void", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "tc_void");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_tes_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pump_coef", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "tes_pump_coef");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_tes_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_type", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "tes_type");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_tshours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "tshours");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Controller_tslogic_a_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tslogic_a", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "tslogic_a");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Controller_tslogic_b_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tslogic_b", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "tslogic_b");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Controller_tslogic_c_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tslogic_c", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "tslogic_c");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_u_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_tank", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "u_tank");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Controller_vol_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vol_tank", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "vol_tank");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_TouTranslator_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekday_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "weekday_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_TouTranslator_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekend_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "weekend_schedule");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_CT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "CT");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Powerblock_F_wc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "F_wc", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "F_wc");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_P_boil_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_boil", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "P_boil");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_P_cond_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_min", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "P_cond_min");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_P_cond_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_ratio", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "P_cond_ratio");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_T_ITD_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ITD_des", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "T_ITD_des");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_T_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_approach", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "T_approach");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_dT_cw_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_cw_ref", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "dT_cw_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_eta_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ref", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "eta_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_n_pl_inc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_pl_inc", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "n_pl_inc");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_pb_bd_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_bd_frac", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "pb_bd_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_pc_config_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_config", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "pc_config");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_q_sby_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_sby_frac", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "q_sby_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_startup_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_frac", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "startup_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_startup_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_time", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "startup_time");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Powerblock_tech_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tech_type", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "tech_type");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_T_amb_des", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "ud_T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_T_amb_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_T_amb_high", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "ud_T_amb_high");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_T_amb_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_T_amb_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "ud_T_amb_ind_od");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_T_amb_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_T_amb_low", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "ud_T_amb_low");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_T_htf_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_T_htf_high", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "ud_T_htf_high");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_T_htf_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_T_htf_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "ud_T_htf_ind_od");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_T_htf_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_T_htf_low", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "ud_T_htf_low");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_f_W_dot_cool_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_f_W_dot_cool_des", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "ud_f_W_dot_cool_des");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "ud_ind_od");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_m_dot_htf_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_m_dot_htf_high", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "ud_m_dot_htf_high");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_m_dot_htf_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_m_dot_htf_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "ud_m_dot_htf_ind_od");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_m_dot_htf_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_m_dot_htf_low", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "ud_m_dot_htf_low");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_UserDefinedPC_ud_m_dot_water_cool_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_m_dot_water_cool_des", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "ud_m_dot_water_cool_des");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Enet_eta_lhv_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_lhv", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "eta_lhv");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Enet_eta_tes_htr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_tes_htr", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "eta_tes_htr");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_P_cooling_tower_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cooling_tower_tot", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "P_cooling_tower_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_P_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_fixed", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "P_fixed");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_P_out_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_out_net", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "P_out_net");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_P_plant_balance_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_plant_balance_tot", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "P_plant_balance_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_P_tower_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_tower_pump", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "P_tower_pump");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_Q_thermal_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_thermal", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "Q_thermal");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_cold", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "T_tes_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_hot", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "T_tes_hot");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Outputs_annual_W_cycle_gross_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_W_cycle_gross", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "annual_W_cycle_gross");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "annual_energy");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Outputs_annual_total_water_use_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_total_water_use", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "annual_total_water_use");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "beam");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Outputs_conversion_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conversion_factor", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "conversion_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_defocus_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "defocus", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "defocus");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Outputs_disp_iter_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_iter_ann", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_iter_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_obj_relax_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_obj_relax", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_obj_relax");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_objective_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_objective", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_objective");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Outputs_disp_objective_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_objective_ann", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_objective_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_pceff_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_pceff_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_pceff_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_presolve_nconstr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_presolve_nconstr", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_presolve_nconstr");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Outputs_disp_presolve_nconstr_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_presolve_nconstr_ann", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_presolve_nconstr_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_presolve_nvar_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_presolve_nvar", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_presolve_nvar");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Outputs_disp_presolve_nvar_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_presolve_nvar_ann", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_presolve_nvar_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_qpbsu_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qpbsu_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_qpbsu_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_qsf_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsf_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_qsf_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_qsfprod_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsfprod_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_qsfprod_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_qsfsu_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsfsu_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_qsfsu_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_rev_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_rev_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_rev_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_solve_iter_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_iter", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_solve_iter");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_solve_state_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_state", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_solve_state");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_solve_time_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_time", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_solve_time");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Outputs_disp_solve_time_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_solve_time_ann", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_solve_time_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_tes_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_tes_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_tes_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_thermeff_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_thermeff_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_thermeff_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_disp_wpb_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_wpb_expected", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "disp_wpb_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_ch_tes", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "e_ch_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_eta_map_out_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "eta_map_out", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "eta_map_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_flux_maps_out_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "flux_maps_out", nrows, ncols);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "flux_maps_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "htf_pump_power", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "htf_pump_power");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_is_pc_sb_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_sb_allowed", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "is_pc_sb_allowed");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_is_pc_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_su_allowed", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "is_pc_su_allowed");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_is_rec_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_rec_su_allowed", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "is_rec_su_allowed");
	});
	return result;
}



SAM_EXPORT double SAM_TroughPhysicalCspSolver_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_TroughPhysicalCspSolver", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_balance", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "m_dot_balance");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_m_dot_pc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pc", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "m_dot_pc");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_m_dot_rec_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_rec", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "m_dot_rec");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_m_dot_tes_ch_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_ch", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "m_dot_tes_ch");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_m_dot_tes_dc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_dc", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "m_dot_tes_dc");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "n_op_modes", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "n_op_modes");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_1", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "op_mode_1");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_2", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "op_mode_2");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_3", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "op_mode_3");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_operating_modes_a_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_a", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "operating_modes_a");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_operating_modes_b_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_b", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "operating_modes_b");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_operating_modes_c_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_c", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "operating_modes_c");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_pparasi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pparasi", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "pparasi");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_pricing_mult_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pricing_mult", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "pricing_mult");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_balance", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "q_balance");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_q_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_ch_tes", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "q_ch_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_q_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dc_tes", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "q_dc_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_q_dot_est_cr_on_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_cr_on", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "q_dot_est_cr_on");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_q_dot_est_cr_su_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_cr_su", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "q_dot_est_cr_su");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_q_dot_est_tes_ch_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_tes_ch", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "q_dot_est_tes_ch");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_q_dot_est_tes_dc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_tes_dc", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "q_dot_est_tes_dc");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_q_dot_pc_max_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_max", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "q_dot_pc_max");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_q_dot_pc_min_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_min", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "q_dot_pc_min");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_q_dot_pc_sb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_sb", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "q_dot_pc_sb");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_q_dot_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_startup", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "q_dot_pc_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_q_dot_pc_target_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_target", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "q_dot_pc_target");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_q_heater_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_heater", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "q_heater");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_q_pb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pb", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "q_pb");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_q_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pc_startup", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "q_pc_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solzen", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "solzen");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_tank_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tank_losses", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "tank_losses");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "time_hr", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "time_hr");
	});
	return result;
}



SAM_EXPORT double* SAM_TroughPhysicalCspSolver_Outputs_tou_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tou_value", length);
	if (!result)
		make_access_error("SAM_TroughPhysicalCspSolver", "tou_value");
	});
	return result;
}




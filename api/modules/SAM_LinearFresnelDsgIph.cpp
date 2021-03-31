#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_LinearFresnelDsgIph.h"

SAM_EXPORT int SAM_LinearFresnelDsgIph_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("linear_fresnel_dsg_iph", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_LinearFresnelDsgIph_Weather_file_name_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "file_name", str);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Weather_solar_resource_data_tset(SAM_table ptr, SAM_table tab, SAM_error *err){
	SAM_table_set_table(ptr, "solar_resource_data", tab, err);
}



SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_A_aperture_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "A_aperture", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_AbsorberMaterial_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "AbsorberMaterial", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_AnnulusGas_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "AnnulusGas", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_ColAz_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ColAz", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_D_2_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_2", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_D_3_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_3", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_D_4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_4", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_D_5_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_5", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_D_p_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "D_p", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_Design_loss_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Design_loss", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_Dirt_HCE_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Dirt_HCE", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_EPSILON_4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "EPSILON_4", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_Flow_type_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Flow_type", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_GeomEffects_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "GeomEffects", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_GlazingIntactIn_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "GlazingIntactIn", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_HCE_FieldFrac_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "HCE_FieldFrac", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_HLCharType_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "HLCharType", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_HL_W_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "HL_W", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_HL_dT_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "HL_dT", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_IAM_L_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "IAM_L", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_IAM_T_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "IAM_T", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_I_bn_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_bn_des", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_L_col_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "L_col", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_OptCharType_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "OptCharType", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_P_a_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "P_a", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_P_turb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_turb_des", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_Pipe_hl_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Pipe_hl_coef", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_Rough_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Rough", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_SCA_drives_elec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SCA_drives_elec", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_Shadowing_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Shadowing", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_T_amb_des_sf_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des_sf", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_T_fp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_fp", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_Tau_envelope_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "Tau_envelope", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_TrackingError_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "TrackingError", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_V_wind_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_wind_max", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_alpha_abs_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "alpha_abs", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_alpha_env_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "alpha_env", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_b_OpticalTable_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "b_OpticalTable", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE1_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "b_eps_HCE1", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE2_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "b_eps_HCE2", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE3_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "b_eps_HCE3", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "b_eps_HCE4", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_dirt_mirror_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dirt_mirror", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_e_startup_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "e_startup", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_error_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "error", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_eta_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pump", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_fP_hdr_c_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fP_hdr_c", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_fP_hdr_h_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fP_hdr_h", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_fP_sf_boil_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fP_sf_boil", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_nLoops_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nLoops", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_nModBoil_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nModBoil", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_q_pb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_pb_des", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_rho_mirror_clean_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "rho_mirror_clean", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_sh_OpticalTable_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sh_OpticalTable", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE1_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sh_eps_HCE1", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE2_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sh_eps_HCE2", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE3_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sh_eps_HCE3", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sh_eps_HCE4", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_theta_dep_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_dep", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_theta_stow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_stow", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Solarfield_x_b_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "x_b_des", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Powerblock_T_cold_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_cold_ref", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Powerblock_T_hot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_hot", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Heliostat_csp_lf_sf_washes_per_year_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.lf.sf.washes_per_year", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_Heliostat_csp_lf_sf_water_per_wash_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.lf.sf.water_per_wash", number);
	});
}

SAM_EXPORT void SAM_LinearFresnelDsgIph_HeatSink_heat_sink_dP_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heat_sink_dP_frac", number);
	});
}

SAM_EXPORT const char* SAM_LinearFresnelDsgIph_Weather_file_name_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "file_name");
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "file_name");
	});
	return result;
}



SAM_EXPORT SAM_table SAM_LinearFresnelDsgIph_Weather_solar_resource_data_tget(SAM_table ptr, SAM_error *err){
	SAM_table result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_table(ptr, "solar_resource_data");
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "solar_resource_data");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_A_aperture_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "A_aperture", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "A_aperture");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_AbsorberMaterial_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "AbsorberMaterial", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "AbsorberMaterial");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_AnnulusGas_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "AnnulusGas", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "AnnulusGas");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_ColAz_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ColAz", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "ColAz");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_D_2_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_2", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "D_2");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_D_3_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_3", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "D_3");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_D_4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_4", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "D_4");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_D_5_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_5", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "D_5");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_D_p_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "D_p", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "D_p");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_Design_loss_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Design_loss", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "Design_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_Dirt_HCE_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Dirt_HCE", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "Dirt_HCE");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_EPSILON_4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "EPSILON_4", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "EPSILON_4");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_Flow_type_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Flow_type", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "Flow_type");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_GeomEffects_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "GeomEffects", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "GeomEffects");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_GlazingIntactIn_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "GlazingIntactIn", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "GlazingIntactIn");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_HCE_FieldFrac_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "HCE_FieldFrac", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "HCE_FieldFrac");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_HLCharType_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "HLCharType", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "HLCharType");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_HL_W_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "HL_W", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "HL_W");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_HL_dT_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "HL_dT", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "HL_dT");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_IAM_L_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "IAM_L", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "IAM_L");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_IAM_T_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "IAM_T", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "IAM_T");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_I_bn_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_bn_des", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "I_bn_des");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_L_col_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "L_col", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "L_col");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_OptCharType_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "OptCharType", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "OptCharType");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_P_a_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "P_a", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "P_a");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_P_turb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_turb_des", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "P_turb_des");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_Pipe_hl_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Pipe_hl_coef", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "Pipe_hl_coef");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_Rough_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Rough", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "Rough");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_SCA_drives_elec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SCA_drives_elec", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "SCA_drives_elec");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_Shadowing_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Shadowing", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "Shadowing");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_T_amb_des_sf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des_sf", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "T_amb_des_sf");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_T_fp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_fp", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "T_fp");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_Tau_envelope_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "Tau_envelope", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "Tau_envelope");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_TrackingError_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "TrackingError", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "TrackingError");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_V_wind_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_wind_max", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "V_wind_max");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_alpha_abs_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "alpha_abs", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "alpha_abs");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_alpha_env_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "alpha_env", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "alpha_env");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_b_OpticalTable_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "b_OpticalTable", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "b_OpticalTable");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE1_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "b_eps_HCE1", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "b_eps_HCE1");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE2_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "b_eps_HCE2", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "b_eps_HCE2");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE3_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "b_eps_HCE3", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "b_eps_HCE3");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "b_eps_HCE4", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "b_eps_HCE4");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_dirt_mirror_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dirt_mirror", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "dirt_mirror");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_e_startup_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "e_startup", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "e_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_error_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "error", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "error");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_eta_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pump", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "eta_pump");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_fP_hdr_c_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fP_hdr_c", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "fP_hdr_c");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_fP_hdr_h_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fP_hdr_h", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "fP_hdr_h");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_fP_sf_boil_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fP_sf_boil", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "fP_sf_boil");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_nLoops_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nLoops", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "nLoops");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_nModBoil_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nModBoil", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "nModBoil");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_q_pb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_pb_des", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "q_pb_des");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_rho_mirror_clean_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "rho_mirror_clean", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "rho_mirror_clean");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_sh_OpticalTable_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sh_OpticalTable", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "sh_OpticalTable");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE1_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sh_eps_HCE1", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "sh_eps_HCE1");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE2_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sh_eps_HCE2", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "sh_eps_HCE2");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE3_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sh_eps_HCE3", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "sh_eps_HCE3");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sh_eps_HCE4", nrows, ncols);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "sh_eps_HCE4");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_theta_dep_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_dep", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "theta_dep");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_theta_stow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_stow", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "theta_stow");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_x_b_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "x_b_des", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "x_b_des");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Powerblock_T_cold_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_cold_ref", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "T_cold_ref");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Powerblock_T_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_hot", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "T_hot");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Heliostat_csp_lf_sf_washes_per_year_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.lf.sf.washes_per_year", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "csp.lf.sf.washes_per_year");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Heliostat_csp_lf_sf_water_per_wash_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.lf.sf.water_per_wash", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "csp.lf.sf.water_per_wash");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_HeatSink_heat_sink_dP_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heat_sink_dP_frac", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "heat_sink_dP_frac");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_T_field_cold_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_field_cold_in", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "T_field_cold_in");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_T_field_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_field_hot_out", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "T_field_hot_out");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_T_rec_cold_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_cold_in", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "T_rec_cold_in");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_T_rec_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_hot_out", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "T_rec_hot_out");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_W_dot_field_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_field_pump", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "W_dot_field_pump");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_W_dot_heat_sink_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_heat_sink_pump", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "W_dot_heat_sink_pump");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_W_dot_parasitic_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_parasitic_tot", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "W_dot_parasitic_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_W_dot_sca_track_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_sca_track", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "W_dot_sca_track");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Outputs_annual_electricity_consumption_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_electricity_consumption", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "annual_electricity_consumption");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "annual_energy");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Outputs_annual_field_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_field_energy", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "annual_field_energy");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Outputs_annual_thermal_consumption_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_thermal_consumption", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "annual_thermal_consumption");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Outputs_annual_total_water_use_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_total_water_use", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "annual_total_water_use");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "beam");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_defocus_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "defocus", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "defocus");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_deltaP_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "deltaP_field", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "deltaP_field");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_e_dot_field_int_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_dot_field_int_energy", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "e_dot_field_int_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_eta_opt_ave_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_opt_ave", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "eta_opt_ave");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_hour_day_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour_day", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "hour_day");
	});
	return result;
}



SAM_EXPORT double SAM_LinearFresnelDsgIph_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_LinearFresnelDsgIph", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_m_dot_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_field", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "m_dot_field");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_m_dot_loop_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_loop", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "m_dot_loop");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "month");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_1", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "op_mode_1");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_2", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "op_mode_2");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_3", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "op_mode_3");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_pres_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pres", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "pres");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_q_dot_freeze_prot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_freeze_prot", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "q_dot_freeze_prot");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_q_dot_piping_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_piping_loss", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "q_dot_piping_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_q_dot_rec_abs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_abs", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "q_dot_rec_abs");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_q_dot_rec_inc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_inc", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "q_dot_rec_inc");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_q_dot_rec_thermal_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_thermal_loss", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "q_dot_rec_thermal_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_q_dot_sf_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_sf_out", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "q_dot_sf_out");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_q_dot_to_heat_sink_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_to_heat_sink", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "q_dot_to_heat_sink");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_q_inc_sf_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_inc_sf_tot", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "q_inc_sf_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_solazi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solazi", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "solazi");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solzen", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "solzen");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "tdry");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_theta_longitudinal_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "theta_longitudinal", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "theta_longitudinal");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_theta_traverse_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "theta_traverse", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "theta_traverse");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "time_hr", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "time_hr");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "twet");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "wspd");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_x_field_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "x_field_hot_out", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "x_field_hot_out");
	});
	return result;
}



SAM_EXPORT double* SAM_LinearFresnelDsgIph_Outputs_x_rec_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "x_rec_hot_out", length);
	if (!result)
		make_access_error("SAM_LinearFresnelDsgIph", "x_rec_hot_out");
	});
	return result;
}




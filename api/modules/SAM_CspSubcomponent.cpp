#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_CspSubcomponent.h"

SAM_EXPORT int SAM_CspSubcomponent_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("csp_subcomponent", data, verbosity, err);
}

SAM_EXPORT void SAM_CspSubcomponent_System_solar_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "solar_mult", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_System_t_step_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "t_step", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Weather_T_amb_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "T_amb", arr, length);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_T_sink_out_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "T_sink_out", arr, length);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_T_src_out_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "T_src_out", arr, length);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_T_tank_cold_ini_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_tank_cold_ini", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_T_tank_hot_ini_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_tank_hot_ini", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_d_tank_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "d_tank_in", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_dt_hot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_hot", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_h_tank_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_in", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_min", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_hot_tank_bypassed_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "hot_tank_bypassed", arr, length);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "init_hot_htf_percent", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_is_h_tank_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_h_tank_fixed", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_mdot_sink_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "mdot_sink", arr, length);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_mdot_src_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "mdot_src", arr, length);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_store_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "store_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_store_fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "store_fluid", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_pairs", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_cyl_piston_loss_poly_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "tes_cyl_piston_loss_poly", arr, length);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_cyl_tank_cp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_cyl_tank_cp", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_cyl_tank_dens_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_cyl_tank_dens", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_cyl_tank_insul_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_cyl_tank_insul_percent", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_cyl_tank_thick_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_cyl_tank_thick", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_n_tsteps_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_n_tsteps", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_T_charge_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_T_charge_min", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_T_cold_delta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_T_cold_delta", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_T_grad_ini_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "tes_pb_T_grad_ini", arr, length);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_T_hot_delta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_T_hot_delta", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_cp_solid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_cp_solid", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_dens_solid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_dens_solid", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_f_oversize_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_f_oversize", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_k_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_k_eff", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_n_xsteps_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_n_xsteps", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_void_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pb_void_frac", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tes_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_type", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_tshours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tshours", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_TES_u_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Powerblock_P_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_ref", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Powerblock_eta_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_ref", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Powerblock_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_pump_coef", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_SolarField_Fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Fluid", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_SolarField_HDR_rough_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HDR_rough", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_SolarField_T_loop_in_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_in_des", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_SolarField_T_loop_out_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_loop_out", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_SolarField_calc_design_pipe_vals_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calc_design_pipe_vals", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_SolarField_eta_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pump", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_SolarField_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "field_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Controller_DP_SGS_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "DP_SGS", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Controller_T_tank_hot_inlet_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_tank_hot_inlet_min", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Controller_V_tes_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_tes_des", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Controller_custom_tes_p_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "custom_tes_p_loss", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Controller_custom_tes_pipe_sizes_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "custom_tes_pipe_sizes", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Controller_has_hot_tank_bypass_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "has_hot_tank_bypass", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Controller_k_tes_loss_coeffs_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "k_tes_loss_coeffs", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Controller_tanks_in_parallel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tanks_in_parallel", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Controller_tes_diams_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "tes_diams", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Controller_tes_lengths_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "tes_lengths", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Controller_tes_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_pump_coef", number);
	});
}

SAM_EXPORT void SAM_CspSubcomponent_Controller_tes_wallthicks_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "tes_wallthicks", mat, nrows, ncols);
	});
}

SAM_EXPORT double SAM_CspSubcomponent_System_solar_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solar_mult", &result))
		make_access_error("SAM_CspSubcomponent", "solar_mult");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_System_t_step_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_step", &result))
		make_access_error("SAM_CspSubcomponent", "t_step");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Weather_T_amb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_amb", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "T_amb");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_TES_T_sink_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_sink_out", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "T_sink_out");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_TES_T_src_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_src_out", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "T_src_out");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_T_tank_cold_ini_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_tank_cold_ini", &result))
		make_access_error("SAM_CspSubcomponent", "T_tank_cold_ini");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_T_tank_hot_ini_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_tank_hot_ini", &result))
		make_access_error("SAM_CspSubcomponent", "T_tank_hot_ini");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_Thtr", &result))
		make_access_error("SAM_CspSubcomponent", "cold_tank_Thtr");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_max_heat", &result))
		make_access_error("SAM_CspSubcomponent", "cold_tank_max_heat");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_d_tank_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_tank_in", &result))
		make_access_error("SAM_CspSubcomponent", "d_tank_in");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_dt_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_hot", &result))
		make_access_error("SAM_CspSubcomponent", "dt_hot");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_h_tank_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_in", &result))
		make_access_error("SAM_CspSubcomponent", "h_tank_in");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_h_tank_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_min", &result))
		make_access_error("SAM_CspSubcomponent", "h_tank_min");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_Thtr", &result))
		make_access_error("SAM_CspSubcomponent", "hot_tank_Thtr");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_TES_hot_tank_bypassed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hot_tank_bypassed", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "hot_tank_bypassed");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_max_heat", &result))
		make_access_error("SAM_CspSubcomponent", "hot_tank_max_heat");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "init_hot_htf_percent", &result))
		make_access_error("SAM_CspSubcomponent", "init_hot_htf_percent");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_is_h_tank_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_h_tank_fixed", &result))
		make_access_error("SAM_CspSubcomponent", "is_h_tank_fixed");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_TES_mdot_sink_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mdot_sink", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "mdot_sink");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_TES_mdot_src_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mdot_src", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "mdot_src");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_TES_store_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "store_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "store_fl_props");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_store_fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "store_fluid", &result))
		make_access_error("SAM_CspSubcomponent", "store_fluid");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tank_pairs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_pairs", &result))
		make_access_error("SAM_CspSubcomponent", "tank_pairs");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_TES_tes_cyl_piston_loss_poly_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_cyl_piston_loss_poly", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_cyl_piston_loss_poly");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tes_cyl_tank_cp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_cyl_tank_cp", &result))
		make_access_error("SAM_CspSubcomponent", "tes_cyl_tank_cp");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tes_cyl_tank_dens_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_cyl_tank_dens", &result))
		make_access_error("SAM_CspSubcomponent", "tes_cyl_tank_dens");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tes_cyl_tank_insul_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_cyl_tank_insul_percent", &result))
		make_access_error("SAM_CspSubcomponent", "tes_cyl_tank_insul_percent");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tes_cyl_tank_thick_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_cyl_tank_thick", &result))
		make_access_error("SAM_CspSubcomponent", "tes_cyl_tank_thick");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tes_n_tsteps_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_n_tsteps", &result))
		make_access_error("SAM_CspSubcomponent", "tes_n_tsteps");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_T_charge_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_T_charge_min", &result))
		make_access_error("SAM_CspSubcomponent", "tes_pb_T_charge_min");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_T_cold_delta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_T_cold_delta", &result))
		make_access_error("SAM_CspSubcomponent", "tes_pb_T_cold_delta");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_TES_tes_pb_T_grad_ini_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_pb_T_grad_ini", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_pb_T_grad_ini");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_T_hot_delta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_T_hot_delta", &result))
		make_access_error("SAM_CspSubcomponent", "tes_pb_T_hot_delta");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_cp_solid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_cp_solid", &result))
		make_access_error("SAM_CspSubcomponent", "tes_pb_cp_solid");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_dens_solid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_dens_solid", &result))
		make_access_error("SAM_CspSubcomponent", "tes_pb_dens_solid");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_f_oversize_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_f_oversize", &result))
		make_access_error("SAM_CspSubcomponent", "tes_pb_f_oversize");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_k_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_k_eff", &result))
		make_access_error("SAM_CspSubcomponent", "tes_pb_k_eff");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_n_xsteps_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_n_xsteps", &result))
		make_access_error("SAM_CspSubcomponent", "tes_pb_n_xsteps");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_void_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pb_void_frac", &result))
		make_access_error("SAM_CspSubcomponent", "tes_pb_void_frac");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tes_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_type", &result))
		make_access_error("SAM_CspSubcomponent", "tes_type");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_tshours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours", &result))
		make_access_error("SAM_CspSubcomponent", "tshours");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_TES_u_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_tank", &result))
		make_access_error("SAM_CspSubcomponent", "u_tank");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_Powerblock_P_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_ref", &result))
		make_access_error("SAM_CspSubcomponent", "P_ref");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_Powerblock_eta_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ref", &result))
		make_access_error("SAM_CspSubcomponent", "eta_ref");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_Powerblock_pb_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_pump_coef", &result))
		make_access_error("SAM_CspSubcomponent", "pb_pump_coef");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_SolarField_Fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Fluid", &result))
		make_access_error("SAM_CspSubcomponent", "Fluid");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_SolarField_HDR_rough_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HDR_rough", &result))
		make_access_error("SAM_CspSubcomponent", "HDR_rough");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_SolarField_T_loop_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_in_des", &result))
		make_access_error("SAM_CspSubcomponent", "T_loop_in_des");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_SolarField_T_loop_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_loop_out", &result))
		make_access_error("SAM_CspSubcomponent", "T_loop_out");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_SolarField_calc_design_pipe_vals_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calc_design_pipe_vals", &result))
		make_access_error("SAM_CspSubcomponent", "calc_design_pipe_vals");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_SolarField_eta_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pump", &result))
		make_access_error("SAM_CspSubcomponent", "eta_pump");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_SolarField_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "field_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "field_fl_props");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_Controller_DP_SGS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "DP_SGS", &result))
		make_access_error("SAM_CspSubcomponent", "DP_SGS");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_Controller_T_tank_hot_inlet_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_tank_hot_inlet_min", &result))
		make_access_error("SAM_CspSubcomponent", "T_tank_hot_inlet_min");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_Controller_V_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tes_des", &result))
		make_access_error("SAM_CspSubcomponent", "V_tes_des");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_Controller_custom_tes_p_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "custom_tes_p_loss", &result))
		make_access_error("SAM_CspSubcomponent", "custom_tes_p_loss");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_Controller_custom_tes_pipe_sizes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "custom_tes_pipe_sizes", &result))
		make_access_error("SAM_CspSubcomponent", "custom_tes_pipe_sizes");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_Controller_has_hot_tank_bypass_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "has_hot_tank_bypass", &result))
		make_access_error("SAM_CspSubcomponent", "has_hot_tank_bypass");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Controller_k_tes_loss_coeffs_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "k_tes_loss_coeffs", nrows, ncols);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "k_tes_loss_coeffs");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_Controller_tanks_in_parallel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tanks_in_parallel", &result))
		make_access_error("SAM_CspSubcomponent", "tanks_in_parallel");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Controller_tes_diams_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "tes_diams", nrows, ncols);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_diams");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Controller_tes_lengths_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "tes_lengths", nrows, ncols);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_lengths");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_Controller_tes_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_pump_coef", &result))
		make_access_error("SAM_CspSubcomponent", "tes_pump_coef");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Controller_tes_wallthicks_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "tes_wallthicks", nrows, ncols);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_wallthicks");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_T_grad_final_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "T_grad_final", nrows, ncols);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "T_grad_final");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_T_sink_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_sink_in", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "T_sink_in");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_T_src_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_src_in", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "T_src_in");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_T_tank_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tank_cold", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "T_tank_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_T_tank_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tank_hot", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "T_tank_hot");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_hot_tank_mass_perc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hot_tank_mass_perc", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "hot_tank_mass_perc");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_hot_tank_vol_frac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hot_tank_vol_frac", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "hot_tank_vol_frac");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_piston_frac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "piston_frac", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "piston_frac");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_piston_loc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "piston_loc", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "piston_loc");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_q_ch_from_htf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_ch_from_htf", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "q_ch_from_htf");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_q_dc_to_htf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dc_to_htf", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "q_dc_to_htf");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_q_dot_ch_from_htf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_ch_from_htf", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "q_dot_ch_from_htf");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_q_dot_dc_to_htf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_dc_to_htf", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "q_dot_dc_to_htf");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_E_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_E_cold", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_E_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_E_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_E_hot", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_E_hot");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_V_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_V_cold", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_V_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_V_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_V_hot", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_V_hot");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_Outputs_tes_diameter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_diameter", &result))
		make_access_error("SAM_CspSubcomponent", "tes_diameter");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_error_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_error", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_error");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_error_corrected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_error_corrected", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_error_corrected");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_error_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_error_percent", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_error_percent");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_exp_length_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_exp_length", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_exp_length");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_exp_wall_mass_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_exp_wall_mass", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_exp_wall_mass");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_Outputs_tes_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_height", &result))
		make_access_error("SAM_CspSubcomponent", "tes_height");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_leak_error_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_leak_error", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_leak_error");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_mass_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_mass_cold", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_mass_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_mass_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_mass_hot", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_mass_hot");
	});
	return result;
}

SAM_EXPORT double SAM_CspSubcomponent_Outputs_tes_radius_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_radius", &result))
		make_access_error("SAM_CspSubcomponent", "tes_radius");
	});
	return result;
}

SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_wall_error_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_wall_error", length);
	if (!result)
		make_access_error("SAM_CspSubcomponent", "tes_wall_error");
	});
	return result;
}


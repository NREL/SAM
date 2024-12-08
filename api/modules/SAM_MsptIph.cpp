#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_MsptIph.h"

SAM_EXPORT int SAM_MsptIph_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("mspt_iph", data, verbosity, err);
}

SAM_EXPORT void SAM_MsptIph_SolarResource_solar_resource_data_tset(SAM_table ptr, SAM_table tab, SAM_error *err){
	SAM_table_set_table(ptr, "solar_resource_data", tab, err);
}



SAM_EXPORT void SAM_MsptIph_SolarResource_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "solar_resource_file", str);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_aux_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "aux_par", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_aux_par_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "aux_par_0", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_aux_par_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "aux_par_1", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_aux_par_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "aux_par_2", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_aux_par_f_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "aux_par_f", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_bop_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_bop_par_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_0", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_bop_par_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_1", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_bop_par_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_2", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_bop_par_f_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_f", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_disp_frequency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_frequency", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_disp_horizon_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_horizon", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_disp_hsu_cost_rel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_hsu_cost_rel", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_disp_inventory_incentive_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_inventory_incentive", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_disp_max_iter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_max_iter", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_disp_mip_gap_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_mip_gap", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_disp_reporting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_reporting", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_disp_rsu_cost_rel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_rsu_cost_rel", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_disp_spec_bb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_bb", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_disp_spec_presolve_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_presolve", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_disp_spec_scaling_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_scaling", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_disp_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_steps_per_hour", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_disp_time_weighting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_time_weighting", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_disp_timeout_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_timeout", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_f_turb_tou_periods_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "f_turb_tou_periods", arr, length);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_is_dispatch_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_dispatch", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_is_parallel_htr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_parallel_htr", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_is_timestep_load_fractions_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_timestep_load_fractions", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_is_tod_pc_target_also_pc_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_tod_pc_target_also_pc_max", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_fixed_par", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_q_rec_heattrace_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_rec_heattrace", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_q_rec_standby_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_rec_standby", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sim_type", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_time_start_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_start", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_time_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_steps_per_hour", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_time_stop_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_stop", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_timestep_load_fractions_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "timestep_load_fractions", arr, length);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_vacuum_arrays_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "vacuum_arrays", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekday_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemControl_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekend_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialModel_csp_financial_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp_financial_model", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemDesign_T_htf_cold_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_cold_des", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemDesign_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_hot_des", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemDesign_dni_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dni_des", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemDesign_q_pb_design_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_pb_design", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemDesign_sf_excess_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sf_excess", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemDesign_solarm_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "solarm", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemDesign_tshours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tshours", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_A_sf_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "A_sf_in", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_N_hel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "N_hel", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_c_atm_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_0", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_c_atm_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_1", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_c_atm_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_2", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_c_atm_3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_3", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_cant_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cant_type", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_check_max_flux_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "check_max_flux", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_csp_pt_sf_fixed_land_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.sf.fixed_land_area", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_csp_pt_sf_land_overhead_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.sf.land_overhead_factor", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_dens_mirror_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dens_mirror", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_eta_map_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "eta_map", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_eta_map_aod_format_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_map_aod_format", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_field_model_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "field_model_type", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_flux_maps_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "flux_maps", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_focus_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "focus_type", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_hel_stow_deploy_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hel_stow_deploy", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_helio_active_fraction_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_active_fraction", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_helio_aim_points_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "helio_aim_points", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_helio_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_height", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_helio_optical_error_mrad_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_optical_error_mrad", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_helio_positions_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "helio_positions", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_helio_reflectance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_reflectance", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_helio_width_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_width", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_interp_beta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "interp_beta", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_interp_nug_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "interp_nug", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_land_bound_list_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "land_bound_list", arr, length);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_land_bound_table_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "land_bound_table", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_land_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_max", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_land_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_min", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_n_facet_x_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_facet_x", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_n_facet_y_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_facet_y", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_opt_algorithm_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_algorithm", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_opt_conv_tol_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_conv_tol", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_opt_flux_penalty_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_flux_penalty", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_opt_init_step_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_init_step", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_opt_max_iter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_max_iter", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_p_start_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "p_start", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_p_track_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "p_track", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_receiver_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "receiver_type", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_total_land_area_before_rad_cooling_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "total_land_area_before_rad_cooling_in", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_v_wind_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "v_wind_max", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_washing_frequency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "washing_frequency", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeliostatField_water_usage_per_wash_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "water_usage_per_wash", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_D_rec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "D_rec", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_Flow_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Flow_type", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_N_panels_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "N_panels", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_cav_rec_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cav_rec_height", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_cav_rec_passive_abs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cav_rec_passive_abs", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_cav_rec_passive_eps_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cav_rec_passive_eps", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_cav_rec_span_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cav_rec_span", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_cav_rec_width_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cav_rec_width", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_crossover_shift_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "crossover_shift", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_csp_pt_rec_max_oper_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.rec.max_oper_frac", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_d_tube_out_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "d_tube_out", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_delta_flux_hrs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "delta_flux_hrs", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_downc_tm_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "downc_tm_mult", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_epsilon_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epsilon", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_eta_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pump", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_f_rec_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_rec_min", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "field_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_flux_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "flux_max", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_h_tower_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tower", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_heat_trace_power_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heat_trace_power", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_hl_ffact_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hl_ffact", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_is_rec_enforce_min_startup_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_rec_enforce_min_startup", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_is_rec_model_trans_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_rec_model_trans", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_is_rec_startup_from_T_soln_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_rec_startup_from_T_soln", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_is_rec_startup_trans_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_rec_startup_trans", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_mat_tube_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mat_tube", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_min_fill_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "min_fill_time", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_min_preheat_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "min_preheat_time", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_n_cav_rec_panels_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_cav_rec_panels", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_n_flux_days_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_flux_days", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_piping_length_const_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_length_const", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_piping_length_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_length_mult", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_piping_loss_coefficient_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_loss_coefficient", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_preheat_flux_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "preheat_flux", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_rec_absorptance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_absorptance", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_rec_clearsky_dni_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "rec_clearsky_dni", arr, length);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_rec_clearsky_fraction_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_clearsky_fraction", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_rec_clearsky_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_clearsky_model", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_rec_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_height", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_rec_hl_perm2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_hl_perm2", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_rec_htf_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_htf", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_rec_qf_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_qf_delay", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_rec_su_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_su_delay", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_rec_tm_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_tm_mult", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_riser_tm_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "riser_tm_mult", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_startup_ramp_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_ramp_time", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_startup_target_Tdiff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_target_Tdiff", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_th_riser_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "th_riser", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_th_tube_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "th_tube", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TowerAndReceiver_u_riser_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_riser", number);
	});
}

SAM_EXPORT void SAM_MsptIph_ParallelHeater_f_q_dot_des_allowable_su_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_q_dot_des_allowable_su", number);
	});
}

SAM_EXPORT void SAM_MsptIph_ParallelHeater_f_q_dot_heater_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_q_dot_heater_min", number);
	});
}

SAM_EXPORT void SAM_MsptIph_ParallelHeater_heater_efficiency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heater_efficiency", number);
	});
}

SAM_EXPORT void SAM_MsptIph_ParallelHeater_heater_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heater_mult", number);
	});
}

SAM_EXPORT void SAM_MsptIph_ParallelHeater_hrs_startup_at_max_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hrs_startup_at_max_rate", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_allow_heater_no_dispatch_opt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "allow_heater_no_dispatch_opt", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_bop_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_spec_cost", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_contingency_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "contingency_rate", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_cost_sf_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cost_sf_fixed", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_csp_pt_cost_epc_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.fixed", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_csp_pt_cost_epc_per_acre_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.per_acre", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_csp_pt_cost_epc_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.per_watt", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_csp_pt_cost_epc_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.percent", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_csp_pt_cost_plm_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.plm.fixed", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_csp_pt_cost_plm_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.plm.per_watt", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_csp_pt_cost_plm_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.plm.percent", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_heater_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heater_spec_cost", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_heliostat_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heliostat_spec_cost", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_land_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_spec_cost", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_rec_cost_exp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_cost_exp", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_rec_ref_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_ref_area", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_rec_ref_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_ref_cost", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_sales_tax_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_frac", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_site_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "site_spec_cost", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_tes_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_spec_cost", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_tower_exp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tower_exp", number);
	});
}

SAM_EXPORT void SAM_MsptIph_SystemCosts_tower_fixed_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tower_fixed_cost", number);
	});
}

SAM_EXPORT void SAM_MsptIph_ThermalStorage_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_MsptIph_ThermalStorage_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_MsptIph_ThermalStorage_h_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank", number);
	});
}

SAM_EXPORT void SAM_MsptIph_ThermalStorage_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_min", number);
	});
}

SAM_EXPORT void SAM_MsptIph_ThermalStorage_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_MsptIph_ThermalStorage_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_MsptIph_ThermalStorage_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_pairs", number);
	});
}

SAM_EXPORT void SAM_MsptIph_ThermalStorage_tanks_in_parallel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tanks_in_parallel", number);
	});
}

SAM_EXPORT void SAM_MsptIph_ThermalStorage_tes_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_init_hot_htf_percent", number);
	});
}

SAM_EXPORT void SAM_MsptIph_ThermalStorage_u_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeatSink_hs_phys_N_sub_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_phys_N_sub", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeatSink_hs_phys_P_steam_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_phys_P_steam_hot_des", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeatSink_hs_phys_Q_steam_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_phys_Q_steam_hot_des", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeatSink_hs_phys_T_steam_cold_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_phys_T_steam_cold_des", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeatSink_hs_phys_f_mdot_steam_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_phys_f_mdot_steam_max", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeatSink_hs_phys_f_mdot_steam_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_phys_f_mdot_steam_min", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeatSink_hs_phys_tol_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_phys_tol", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeatSink_hs_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hs_type", number);
	});
}

SAM_EXPORT void SAM_MsptIph_HeatSink_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_pump_coef", number);
	});
}

SAM_EXPORT void SAM_MsptIph_TimeOfDeliveryFactors_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_factors_ts", arr, length);
	});
}

SAM_EXPORT void SAM_MsptIph_TimeOfDeliveryFactors_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MsptIph_TimeOfDeliveryFactors_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MsptIph_TimeOfDeliveryFactors_dispatch_tod_factors_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_tod_factors", arr, length);
	});
}

SAM_EXPORT void SAM_MsptIph_TimeOfDeliveryFactors_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_multiplier_model", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialSolutionMode_ppa_soln_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_soln_mode", number);
	});
}

SAM_EXPORT void SAM_MsptIph_Revenue_ppa_price_input_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ppa_price_input_heat_btu", arr, length);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate1", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate2", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate3", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate4", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate5", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months1", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months2", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months3", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months4", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months5", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent1", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent2", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent3", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent4", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent5", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate1", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate2", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate3", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate4", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate5", number);
	});
}

SAM_EXPORT void SAM_MsptIph_FinancialParameters_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_rate", number);
	});
}

SAM_EXPORT void SAM_MsptIph_Deprecated_P_boil_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_boil", number);
	});
}

SAM_EXPORT void SAM_MsptIph_Deprecated_csp_pt_tes_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.tes.init_hot_htf_percent", number);
	});
}

SAM_EXPORT void SAM_MsptIph_Deprecated_disp_csu_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_csu_cost", number);
	});
}

SAM_EXPORT void SAM_MsptIph_Deprecated_disp_pen_delta_w_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_pen_delta_w", number);
	});
}

SAM_EXPORT void SAM_MsptIph_Deprecated_disp_rsu_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_rsu_cost", number);
	});
}

SAM_EXPORT void SAM_MsptIph_Deprecated_piping_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_loss", number);
	});
}

SAM_EXPORT void SAM_MsptIph_AdjustmentFactors_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_constant", number);
	});
}

SAM_EXPORT void SAM_MsptIph_AdjustmentFactors_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_periods", number);
	});
}

SAM_EXPORT void SAM_MsptIph_AdjustmentFactors_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_timeindex", number);
	});
}

SAM_EXPORT void SAM_MsptIph_AdjustmentFactors_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "adjust_periods", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MsptIph_AdjustmentFactors_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "adjust_timeindex", arr, length);
	});
}

SAM_EXPORT void SAM_MsptIph_AdjustmentFactors_sf_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sf_adjust_constant", number);
	});
}

SAM_EXPORT void SAM_MsptIph_AdjustmentFactors_sf_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sf_adjust_en_periods", number);
	});
}

SAM_EXPORT void SAM_MsptIph_AdjustmentFactors_sf_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sf_adjust_en_timeindex", number);
	});
}

SAM_EXPORT void SAM_MsptIph_AdjustmentFactors_sf_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sf_adjust_periods", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MsptIph_AdjustmentFactors_sf_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sf_adjust_timeindex", arr, length);
	});
}

SAM_EXPORT SAM_table SAM_MsptIph_SolarResource_solar_resource_data_tget(SAM_table ptr, SAM_error *err){
	SAM_table result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_table(ptr, "solar_resource_data");
	if (!result)
		make_access_error("SAM_MsptIph", "solar_resource_data");
	});
	return result;
}

SAM_EXPORT const char* SAM_MsptIph_SolarResource_solar_resource_file_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "solar_resource_file");
	if (!result)
		make_access_error("SAM_MsptIph", "solar_resource_file");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_aux_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_par", &result))
		make_access_error("SAM_MsptIph", "aux_par");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_aux_par_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_par_0", &result))
		make_access_error("SAM_MsptIph", "aux_par_0");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_aux_par_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_par_1", &result))
		make_access_error("SAM_MsptIph", "aux_par_1");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_aux_par_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_par_2", &result))
		make_access_error("SAM_MsptIph", "aux_par_2");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_aux_par_f_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_par_f", &result))
		make_access_error("SAM_MsptIph", "aux_par_f");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_bop_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par", &result))
		make_access_error("SAM_MsptIph", "bop_par");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_bop_par_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_0", &result))
		make_access_error("SAM_MsptIph", "bop_par_0");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_bop_par_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_1", &result))
		make_access_error("SAM_MsptIph", "bop_par_1");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_bop_par_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_2", &result))
		make_access_error("SAM_MsptIph", "bop_par_2");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_bop_par_f_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_f", &result))
		make_access_error("SAM_MsptIph", "bop_par_f");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_disp_frequency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_frequency", &result))
		make_access_error("SAM_MsptIph", "disp_frequency");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_disp_horizon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_horizon", &result))
		make_access_error("SAM_MsptIph", "disp_horizon");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_disp_hsu_cost_rel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_hsu_cost_rel", &result))
		make_access_error("SAM_MsptIph", "disp_hsu_cost_rel");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_disp_inventory_incentive_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_inventory_incentive", &result))
		make_access_error("SAM_MsptIph", "disp_inventory_incentive");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_disp_max_iter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_max_iter", &result))
		make_access_error("SAM_MsptIph", "disp_max_iter");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_disp_mip_gap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_mip_gap", &result))
		make_access_error("SAM_MsptIph", "disp_mip_gap");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_disp_reporting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_reporting", &result))
		make_access_error("SAM_MsptIph", "disp_reporting");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_disp_rsu_cost_rel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_rsu_cost_rel", &result))
		make_access_error("SAM_MsptIph", "disp_rsu_cost_rel");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_disp_spec_bb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_bb", &result))
		make_access_error("SAM_MsptIph", "disp_spec_bb");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_disp_spec_presolve_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_presolve", &result))
		make_access_error("SAM_MsptIph", "disp_spec_presolve");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_disp_spec_scaling_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_scaling", &result))
		make_access_error("SAM_MsptIph", "disp_spec_scaling");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_disp_steps_per_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_steps_per_hour", &result))
		make_access_error("SAM_MsptIph", "disp_steps_per_hour");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_disp_time_weighting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_time_weighting", &result))
		make_access_error("SAM_MsptIph", "disp_time_weighting");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_disp_timeout_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_timeout", &result))
		make_access_error("SAM_MsptIph", "disp_timeout");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_SystemControl_f_turb_tou_periods_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "f_turb_tou_periods", length);
	if (!result)
		make_access_error("SAM_MsptIph", "f_turb_tou_periods");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_is_dispatch_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_dispatch", &result))
		make_access_error("SAM_MsptIph", "is_dispatch");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_is_parallel_htr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_parallel_htr", &result))
		make_access_error("SAM_MsptIph", "is_parallel_htr");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_is_timestep_load_fractions_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_timestep_load_fractions", &result))
		make_access_error("SAM_MsptIph", "is_timestep_load_fractions");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_is_tod_pc_target_also_pc_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_tod_pc_target_also_pc_max", &result))
		make_access_error("SAM_MsptIph", "is_tod_pc_target_also_pc_max");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_pb_fixed_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_fixed_par", &result))
		make_access_error("SAM_MsptIph", "pb_fixed_par");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_q_rec_heattrace_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_rec_heattrace", &result))
		make_access_error("SAM_MsptIph", "q_rec_heattrace");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_q_rec_standby_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_rec_standby", &result))
		make_access_error("SAM_MsptIph", "q_rec_standby");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_type", &result))
		make_access_error("SAM_MsptIph", "sim_type");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_time_start_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_start", &result))
		make_access_error("SAM_MsptIph", "time_start");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_time_steps_per_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_steps_per_hour", &result))
		make_access_error("SAM_MsptIph", "time_steps_per_hour");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_time_stop_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_stop", &result))
		make_access_error("SAM_MsptIph", "time_stop");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_SystemControl_timestep_load_fractions_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "timestep_load_fractions", length);
	if (!result)
		make_access_error("SAM_MsptIph", "timestep_load_fractions");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemControl_vacuum_arrays_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vacuum_arrays", &result))
		make_access_error("SAM_MsptIph", "vacuum_arrays");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_SystemControl_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekday_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "weekday_schedule");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_SystemControl_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekend_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "weekend_schedule");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialModel_csp_financial_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp_financial_model", &result))
		make_access_error("SAM_MsptIph", "csp_financial_model");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemDesign_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_cold_des", &result))
		make_access_error("SAM_MsptIph", "T_htf_cold_des");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemDesign_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_hot_des", &result))
		make_access_error("SAM_MsptIph", "T_htf_hot_des");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemDesign_dni_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dni_des", &result))
		make_access_error("SAM_MsptIph", "dni_des");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemDesign_q_pb_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_pb_design", &result))
		make_access_error("SAM_MsptIph", "q_pb_design");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemDesign_sf_excess_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sf_excess", &result))
		make_access_error("SAM_MsptIph", "sf_excess");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemDesign_solarm_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solarm", &result))
		make_access_error("SAM_MsptIph", "solarm");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemDesign_tshours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours", &result))
		make_access_error("SAM_MsptIph", "tshours");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_A_sf_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_sf_in", &result))
		make_access_error("SAM_MsptIph", "A_sf_in");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_N_hel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_hel", &result))
		make_access_error("SAM_MsptIph", "N_hel");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_c_atm_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_0", &result))
		make_access_error("SAM_MsptIph", "c_atm_0");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_c_atm_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_1", &result))
		make_access_error("SAM_MsptIph", "c_atm_1");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_c_atm_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_2", &result))
		make_access_error("SAM_MsptIph", "c_atm_2");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_c_atm_3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_3", &result))
		make_access_error("SAM_MsptIph", "c_atm_3");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_cant_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cant_type", &result))
		make_access_error("SAM_MsptIph", "cant_type");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_check_max_flux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "check_max_flux", &result))
		make_access_error("SAM_MsptIph", "check_max_flux");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_csp_pt_sf_fixed_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.sf.fixed_land_area", &result))
		make_access_error("SAM_MsptIph", "csp.pt.sf.fixed_land_area");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_csp_pt_sf_land_overhead_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.sf.land_overhead_factor", &result))
		make_access_error("SAM_MsptIph", "csp.pt.sf.land_overhead_factor");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_dens_mirror_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dens_mirror", &result))
		make_access_error("SAM_MsptIph", "dens_mirror");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_HeliostatField_eta_map_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "eta_map", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "eta_map");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_eta_map_aod_format_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_map_aod_format", &result))
		make_access_error("SAM_MsptIph", "eta_map_aod_format");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_field_model_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_model_type", &result))
		make_access_error("SAM_MsptIph", "field_model_type");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_HeliostatField_flux_maps_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "flux_maps", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "flux_maps");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_focus_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "focus_type", &result))
		make_access_error("SAM_MsptIph", "focus_type");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_hel_stow_deploy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hel_stow_deploy", &result))
		make_access_error("SAM_MsptIph", "hel_stow_deploy");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_helio_active_fraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_active_fraction", &result))
		make_access_error("SAM_MsptIph", "helio_active_fraction");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_HeliostatField_helio_aim_points_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "helio_aim_points", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "helio_aim_points");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_helio_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_height", &result))
		make_access_error("SAM_MsptIph", "helio_height");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_helio_optical_error_mrad_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_optical_error_mrad", &result))
		make_access_error("SAM_MsptIph", "helio_optical_error_mrad");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_HeliostatField_helio_positions_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "helio_positions", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "helio_positions");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_helio_reflectance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_reflectance", &result))
		make_access_error("SAM_MsptIph", "helio_reflectance");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_helio_width_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_width", &result))
		make_access_error("SAM_MsptIph", "helio_width");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_interp_beta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "interp_beta", &result))
		make_access_error("SAM_MsptIph", "interp_beta");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_interp_nug_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "interp_nug", &result))
		make_access_error("SAM_MsptIph", "interp_nug");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_HeliostatField_land_bound_list_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "land_bound_list", length);
	if (!result)
		make_access_error("SAM_MsptIph", "land_bound_list");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_HeliostatField_land_bound_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "land_bound_table", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "land_bound_table");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_land_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_max", &result))
		make_access_error("SAM_MsptIph", "land_max");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_land_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_min", &result))
		make_access_error("SAM_MsptIph", "land_min");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_n_facet_x_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_facet_x", &result))
		make_access_error("SAM_MsptIph", "n_facet_x");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_n_facet_y_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_facet_y", &result))
		make_access_error("SAM_MsptIph", "n_facet_y");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_opt_algorithm_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_algorithm", &result))
		make_access_error("SAM_MsptIph", "opt_algorithm");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_opt_conv_tol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_conv_tol", &result))
		make_access_error("SAM_MsptIph", "opt_conv_tol");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_opt_flux_penalty_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_flux_penalty", &result))
		make_access_error("SAM_MsptIph", "opt_flux_penalty");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_opt_init_step_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_init_step", &result))
		make_access_error("SAM_MsptIph", "opt_init_step");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_opt_max_iter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_max_iter", &result))
		make_access_error("SAM_MsptIph", "opt_max_iter");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_p_start_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "p_start", &result))
		make_access_error("SAM_MsptIph", "p_start");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_p_track_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "p_track", &result))
		make_access_error("SAM_MsptIph", "p_track");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_receiver_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "receiver_type", &result))
		make_access_error("SAM_MsptIph", "receiver_type");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_total_land_area_before_rad_cooling_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_land_area_before_rad_cooling_in", &result))
		make_access_error("SAM_MsptIph", "total_land_area_before_rad_cooling_in");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_v_wind_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "v_wind_max", &result))
		make_access_error("SAM_MsptIph", "v_wind_max");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_washing_frequency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "washing_frequency", &result))
		make_access_error("SAM_MsptIph", "washing_frequency");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeliostatField_water_usage_per_wash_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "water_usage_per_wash", &result))
		make_access_error("SAM_MsptIph", "water_usage_per_wash");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_D_rec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "D_rec", &result))
		make_access_error("SAM_MsptIph", "D_rec");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_Flow_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Flow_type", &result))
		make_access_error("SAM_MsptIph", "Flow_type");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_N_panels_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_panels", &result))
		make_access_error("SAM_MsptIph", "N_panels");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_cav_rec_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cav_rec_height", &result))
		make_access_error("SAM_MsptIph", "cav_rec_height");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_cav_rec_passive_abs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cav_rec_passive_abs", &result))
		make_access_error("SAM_MsptIph", "cav_rec_passive_abs");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_cav_rec_passive_eps_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cav_rec_passive_eps", &result))
		make_access_error("SAM_MsptIph", "cav_rec_passive_eps");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_cav_rec_span_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cav_rec_span", &result))
		make_access_error("SAM_MsptIph", "cav_rec_span");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_cav_rec_width_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cav_rec_width", &result))
		make_access_error("SAM_MsptIph", "cav_rec_width");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_crossover_shift_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "crossover_shift", &result))
		make_access_error("SAM_MsptIph", "crossover_shift");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_csp_pt_rec_max_oper_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.rec.max_oper_frac", &result))
		make_access_error("SAM_MsptIph", "csp.pt.rec.max_oper_frac");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_d_tube_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_tube_out", &result))
		make_access_error("SAM_MsptIph", "d_tube_out");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_delta_flux_hrs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "delta_flux_hrs", &result))
		make_access_error("SAM_MsptIph", "delta_flux_hrs");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_downc_tm_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "downc_tm_mult", &result))
		make_access_error("SAM_MsptIph", "downc_tm_mult");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_epsilon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epsilon", &result))
		make_access_error("SAM_MsptIph", "epsilon");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_eta_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pump", &result))
		make_access_error("SAM_MsptIph", "eta_pump");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_f_rec_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_rec_min", &result))
		make_access_error("SAM_MsptIph", "f_rec_min");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_TowerAndReceiver_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "field_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "field_fl_props");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_flux_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flux_max", &result))
		make_access_error("SAM_MsptIph", "flux_max");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_h_tower_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tower", &result))
		make_access_error("SAM_MsptIph", "h_tower");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_heat_trace_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heat_trace_power", &result))
		make_access_error("SAM_MsptIph", "heat_trace_power");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_hl_ffact_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hl_ffact", &result))
		make_access_error("SAM_MsptIph", "hl_ffact");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_is_rec_enforce_min_startup_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_rec_enforce_min_startup", &result))
		make_access_error("SAM_MsptIph", "is_rec_enforce_min_startup");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_is_rec_model_trans_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_rec_model_trans", &result))
		make_access_error("SAM_MsptIph", "is_rec_model_trans");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_is_rec_startup_from_T_soln_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_rec_startup_from_T_soln", &result))
		make_access_error("SAM_MsptIph", "is_rec_startup_from_T_soln");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_is_rec_startup_trans_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_rec_startup_trans", &result))
		make_access_error("SAM_MsptIph", "is_rec_startup_trans");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_mat_tube_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mat_tube", &result))
		make_access_error("SAM_MsptIph", "mat_tube");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_min_fill_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "min_fill_time", &result))
		make_access_error("SAM_MsptIph", "min_fill_time");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_min_preheat_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "min_preheat_time", &result))
		make_access_error("SAM_MsptIph", "min_preheat_time");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_n_cav_rec_panels_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_cav_rec_panels", &result))
		make_access_error("SAM_MsptIph", "n_cav_rec_panels");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_n_flux_days_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_flux_days", &result))
		make_access_error("SAM_MsptIph", "n_flux_days");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_piping_length_const_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_length_const", &result))
		make_access_error("SAM_MsptIph", "piping_length_const");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_piping_length_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_length_mult", &result))
		make_access_error("SAM_MsptIph", "piping_length_mult");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_piping_loss_coefficient_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_loss_coefficient", &result))
		make_access_error("SAM_MsptIph", "piping_loss_coefficient");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_preheat_flux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "preheat_flux", &result))
		make_access_error("SAM_MsptIph", "preheat_flux");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_rec_absorptance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_absorptance", &result))
		make_access_error("SAM_MsptIph", "rec_absorptance");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_TowerAndReceiver_rec_clearsky_dni_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rec_clearsky_dni", length);
	if (!result)
		make_access_error("SAM_MsptIph", "rec_clearsky_dni");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_rec_clearsky_fraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_clearsky_fraction", &result))
		make_access_error("SAM_MsptIph", "rec_clearsky_fraction");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_rec_clearsky_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_clearsky_model", &result))
		make_access_error("SAM_MsptIph", "rec_clearsky_model");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_rec_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_height", &result))
		make_access_error("SAM_MsptIph", "rec_height");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_rec_hl_perm2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_hl_perm2", &result))
		make_access_error("SAM_MsptIph", "rec_hl_perm2");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_rec_htf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_htf", &result))
		make_access_error("SAM_MsptIph", "rec_htf");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_rec_qf_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_qf_delay", &result))
		make_access_error("SAM_MsptIph", "rec_qf_delay");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_rec_su_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_su_delay", &result))
		make_access_error("SAM_MsptIph", "rec_su_delay");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_rec_tm_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_tm_mult", &result))
		make_access_error("SAM_MsptIph", "rec_tm_mult");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_riser_tm_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "riser_tm_mult", &result))
		make_access_error("SAM_MsptIph", "riser_tm_mult");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_startup_ramp_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_ramp_time", &result))
		make_access_error("SAM_MsptIph", "startup_ramp_time");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_startup_target_Tdiff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_target_Tdiff", &result))
		make_access_error("SAM_MsptIph", "startup_target_Tdiff");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_th_riser_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "th_riser", &result))
		make_access_error("SAM_MsptIph", "th_riser");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_th_tube_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "th_tube", &result))
		make_access_error("SAM_MsptIph", "th_tube");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TowerAndReceiver_u_riser_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_riser", &result))
		make_access_error("SAM_MsptIph", "u_riser");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_ParallelHeater_f_q_dot_des_allowable_su_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_q_dot_des_allowable_su", &result))
		make_access_error("SAM_MsptIph", "f_q_dot_des_allowable_su");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_ParallelHeater_f_q_dot_heater_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_q_dot_heater_min", &result))
		make_access_error("SAM_MsptIph", "f_q_dot_heater_min");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_ParallelHeater_heater_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heater_efficiency", &result))
		make_access_error("SAM_MsptIph", "heater_efficiency");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_ParallelHeater_heater_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heater_mult", &result))
		make_access_error("SAM_MsptIph", "heater_mult");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_ParallelHeater_hrs_startup_at_max_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hrs_startup_at_max_rate", &result))
		make_access_error("SAM_MsptIph", "hrs_startup_at_max_rate");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_allow_heater_no_dispatch_opt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "allow_heater_no_dispatch_opt", &result))
		make_access_error("SAM_MsptIph", "allow_heater_no_dispatch_opt");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_bop_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_spec_cost", &result))
		make_access_error("SAM_MsptIph", "bop_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_contingency_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_rate", &result))
		make_access_error("SAM_MsptIph", "contingency_rate");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_cost_sf_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_sf_fixed", &result))
		make_access_error("SAM_MsptIph", "cost_sf_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_csp_pt_cost_epc_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.fixed", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.epc.fixed");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_csp_pt_cost_epc_per_acre_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.per_acre", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.epc.per_acre");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_csp_pt_cost_epc_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.per_watt", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.epc.per_watt");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_csp_pt_cost_epc_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.percent", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.epc.percent");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_csp_pt_cost_plm_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.fixed", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.plm.fixed");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_csp_pt_cost_plm_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.per_watt", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.plm.per_watt");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_csp_pt_cost_plm_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.percent", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.plm.percent");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_heater_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heater_spec_cost", &result))
		make_access_error("SAM_MsptIph", "heater_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_heliostat_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heliostat_spec_cost", &result))
		make_access_error("SAM_MsptIph", "heliostat_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_land_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_spec_cost", &result))
		make_access_error("SAM_MsptIph", "land_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_rec_cost_exp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_cost_exp", &result))
		make_access_error("SAM_MsptIph", "rec_cost_exp");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_rec_ref_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_ref_area", &result))
		make_access_error("SAM_MsptIph", "rec_ref_area");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_rec_ref_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_ref_cost", &result))
		make_access_error("SAM_MsptIph", "rec_ref_cost");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_sales_tax_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_frac", &result))
		make_access_error("SAM_MsptIph", "sales_tax_frac");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_site_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "site_spec_cost", &result))
		make_access_error("SAM_MsptIph", "site_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_tes_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_spec_cost", &result))
		make_access_error("SAM_MsptIph", "tes_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_tower_exp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_exp", &result))
		make_access_error("SAM_MsptIph", "tower_exp");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_SystemCosts_tower_fixed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_fixed_cost", &result))
		make_access_error("SAM_MsptIph", "tower_fixed_cost");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_ThermalStorage_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_Thtr", &result))
		make_access_error("SAM_MsptIph", "cold_tank_Thtr");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_ThermalStorage_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_max_heat", &result))
		make_access_error("SAM_MsptIph", "cold_tank_max_heat");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_ThermalStorage_h_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank", &result))
		make_access_error("SAM_MsptIph", "h_tank");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_ThermalStorage_h_tank_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_min", &result))
		make_access_error("SAM_MsptIph", "h_tank_min");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_ThermalStorage_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_Thtr", &result))
		make_access_error("SAM_MsptIph", "hot_tank_Thtr");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_ThermalStorage_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_max_heat", &result))
		make_access_error("SAM_MsptIph", "hot_tank_max_heat");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_ThermalStorage_tank_pairs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_pairs", &result))
		make_access_error("SAM_MsptIph", "tank_pairs");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_ThermalStorage_tanks_in_parallel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tanks_in_parallel", &result))
		make_access_error("SAM_MsptIph", "tanks_in_parallel");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_ThermalStorage_tes_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_init_hot_htf_percent", &result))
		make_access_error("SAM_MsptIph", "tes_init_hot_htf_percent");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_ThermalStorage_u_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_tank", &result))
		make_access_error("SAM_MsptIph", "u_tank");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeatSink_hs_phys_N_sub_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_phys_N_sub", &result))
		make_access_error("SAM_MsptIph", "hs_phys_N_sub");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeatSink_hs_phys_P_steam_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_phys_P_steam_hot_des", &result))
		make_access_error("SAM_MsptIph", "hs_phys_P_steam_hot_des");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeatSink_hs_phys_Q_steam_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_phys_Q_steam_hot_des", &result))
		make_access_error("SAM_MsptIph", "hs_phys_Q_steam_hot_des");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeatSink_hs_phys_T_steam_cold_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_phys_T_steam_cold_des", &result))
		make_access_error("SAM_MsptIph", "hs_phys_T_steam_cold_des");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeatSink_hs_phys_f_mdot_steam_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_phys_f_mdot_steam_max", &result))
		make_access_error("SAM_MsptIph", "hs_phys_f_mdot_steam_max");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeatSink_hs_phys_f_mdot_steam_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_phys_f_mdot_steam_min", &result))
		make_access_error("SAM_MsptIph", "hs_phys_f_mdot_steam_min");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeatSink_hs_phys_tol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_phys_tol", &result))
		make_access_error("SAM_MsptIph", "hs_phys_tol");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeatSink_hs_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hs_type", &result))
		make_access_error("SAM_MsptIph", "hs_type");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_HeatSink_pb_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_pump_coef", &result))
		make_access_error("SAM_MsptIph", "pb_pump_coef");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_TimeOfDeliveryFactors_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_factors_ts", length);
	if (!result)
		make_access_error("SAM_MsptIph", "dispatch_factors_ts");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_TimeOfDeliveryFactors_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "dispatch_sched_weekday");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_TimeOfDeliveryFactors_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "dispatch_sched_weekend");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_TimeOfDeliveryFactors_dispatch_tod_factors_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_tod_factors", length);
	if (!result)
		make_access_error("SAM_MsptIph", "dispatch_tod_factors");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_TimeOfDeliveryFactors_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_multiplier_model", &result))
		make_access_error("SAM_MsptIph", "ppa_multiplier_model");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialSolutionMode_ppa_soln_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_soln_mode", &result))
		make_access_error("SAM_MsptIph", "ppa_soln_mode");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Revenue_ppa_price_input_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ppa_price_input_heat_btu", length);
	if (!result)
		make_access_error("SAM_MsptIph", "ppa_price_input_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate1", &result))
		make_access_error("SAM_MsptIph", "const_per_interest_rate1");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate2", &result))
		make_access_error("SAM_MsptIph", "const_per_interest_rate2");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate3", &result))
		make_access_error("SAM_MsptIph", "const_per_interest_rate3");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate4", &result))
		make_access_error("SAM_MsptIph", "const_per_interest_rate4");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate5", &result))
		make_access_error("SAM_MsptIph", "const_per_interest_rate5");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months1", &result))
		make_access_error("SAM_MsptIph", "const_per_months1");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months2", &result))
		make_access_error("SAM_MsptIph", "const_per_months2");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months3", &result))
		make_access_error("SAM_MsptIph", "const_per_months3");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months4", &result))
		make_access_error("SAM_MsptIph", "const_per_months4");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months5", &result))
		make_access_error("SAM_MsptIph", "const_per_months5");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent1", &result))
		make_access_error("SAM_MsptIph", "const_per_percent1");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent2", &result))
		make_access_error("SAM_MsptIph", "const_per_percent2");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent3", &result))
		make_access_error("SAM_MsptIph", "const_per_percent3");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent4", &result))
		make_access_error("SAM_MsptIph", "const_per_percent4");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent5", &result))
		make_access_error("SAM_MsptIph", "const_per_percent5");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate1", &result))
		make_access_error("SAM_MsptIph", "const_per_upfront_rate1");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate2", &result))
		make_access_error("SAM_MsptIph", "const_per_upfront_rate2");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate3", &result))
		make_access_error("SAM_MsptIph", "const_per_upfront_rate3");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate4", &result))
		make_access_error("SAM_MsptIph", "const_per_upfront_rate4");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate5", &result))
		make_access_error("SAM_MsptIph", "const_per_upfront_rate5");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_FinancialParameters_sales_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_rate", &result))
		make_access_error("SAM_MsptIph", "sales_tax_rate");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Deprecated_P_boil_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_boil", &result))
		make_access_error("SAM_MsptIph", "P_boil");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Deprecated_csp_pt_tes_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.tes.init_hot_htf_percent", &result))
		make_access_error("SAM_MsptIph", "csp.pt.tes.init_hot_htf_percent");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Deprecated_disp_csu_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_csu_cost", &result))
		make_access_error("SAM_MsptIph", "disp_csu_cost");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Deprecated_disp_pen_delta_w_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_pen_delta_w", &result))
		make_access_error("SAM_MsptIph", "disp_pen_delta_w");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Deprecated_disp_rsu_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_rsu_cost", &result))
		make_access_error("SAM_MsptIph", "disp_rsu_cost");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Deprecated_piping_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_loss", &result))
		make_access_error("SAM_MsptIph", "piping_loss");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_AdjustmentFactors_adjust_constant_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_constant", &result))
		make_access_error("SAM_MsptIph", "adjust_constant");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_AdjustmentFactors_adjust_en_periods_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_periods", &result))
		make_access_error("SAM_MsptIph", "adjust_en_periods");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_AdjustmentFactors_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_timeindex", &result))
		make_access_error("SAM_MsptIph", "adjust_en_timeindex");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_AdjustmentFactors_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "adjust_periods", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "adjust_periods");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_AdjustmentFactors_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "adjust_timeindex", length);
	if (!result)
		make_access_error("SAM_MsptIph", "adjust_timeindex");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_AdjustmentFactors_sf_adjust_constant_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sf_adjust_constant", &result))
		make_access_error("SAM_MsptIph", "sf_adjust_constant");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_AdjustmentFactors_sf_adjust_en_periods_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sf_adjust_en_periods", &result))
		make_access_error("SAM_MsptIph", "sf_adjust_en_periods");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_AdjustmentFactors_sf_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sf_adjust_en_timeindex", &result))
		make_access_error("SAM_MsptIph", "sf_adjust_en_timeindex");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_AdjustmentFactors_sf_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sf_adjust_periods", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "sf_adjust_periods");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_AdjustmentFactors_sf_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sf_adjust_timeindex", length);
	if (!result)
		make_access_error("SAM_MsptIph", "sf_adjust_timeindex");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_A_rec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_rec", &result))
		make_access_error("SAM_MsptIph", "A_rec");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_A_sf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_sf", &result))
		make_access_error("SAM_MsptIph", "A_sf");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_D_rec_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "D_rec_calc", &result))
		make_access_error("SAM_MsptIph", "D_rec_calc");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_E_heater_su_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "E_heater_su_des", &result))
		make_access_error("SAM_MsptIph", "E_heater_su_des");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_L_tower_piping_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_tower_piping_calc", &result))
		make_access_error("SAM_MsptIph", "L_tower_piping_calc");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_N_hel_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_hel_calc", &result))
		make_access_error("SAM_MsptIph", "N_hel_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_P_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_fixed", length);
	if (!result)
		make_access_error("SAM_MsptIph", "P_fixed");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_P_plant_balance_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_plant_balance_tot", length);
	if (!result)
		make_access_error("SAM_MsptIph", "P_plant_balance_tot");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_P_rec_heattrace_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_rec_heattrace", length);
	if (!result)
		make_access_error("SAM_MsptIph", "P_rec_heattrace");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_P_tower_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_tower_pump", length);
	if (!result)
		make_access_error("SAM_MsptIph", "P_tower_pump");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_Q_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_tes_des", &result))
		make_access_error("SAM_MsptIph", "Q_tes_des");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_Q_thermal_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_thermal", length);
	if (!result)
		make_access_error("SAM_MsptIph", "Q_thermal");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_Q_thermal_ss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_thermal_ss", length);
	if (!result)
		make_access_error("SAM_MsptIph", "Q_thermal_ss");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_Q_thermal_ss_csky_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_thermal_ss_csky", length);
	if (!result)
		make_access_error("SAM_MsptIph", "Q_thermal_ss_csky");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_T_heat_sink_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_heat_sink_in", length);
	if (!result)
		make_access_error("SAM_MsptIph", "T_heat_sink_in");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_T_heat_sink_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_heat_sink_out", length);
	if (!result)
		make_access_error("SAM_MsptIph", "T_heat_sink_out");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_T_htf_heater_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_htf_heater_in", length);
	if (!result)
		make_access_error("SAM_MsptIph", "T_htf_heater_in");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_T_htf_heater_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_htf_heater_out", length);
	if (!result)
		make_access_error("SAM_MsptIph", "T_htf_heater_out");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_T_panel_out_max_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_panel_out_max", length);
	if (!result)
		make_access_error("SAM_MsptIph", "T_panel_out_max");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_T_rec_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_in", length);
	if (!result)
		make_access_error("SAM_MsptIph", "T_rec_in");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_T_rec_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_out", length);
	if (!result)
		make_access_error("SAM_MsptIph", "T_rec_out");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_T_rec_out_end_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_out_end", length);
	if (!result)
		make_access_error("SAM_MsptIph", "T_rec_out_end");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_T_rec_out_max_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_out_max", length);
	if (!result)
		make_access_error("SAM_MsptIph", "T_rec_out_max");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_cold", length);
	if (!result)
		make_access_error("SAM_MsptIph", "T_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_hot", length);
	if (!result)
		make_access_error("SAM_MsptIph", "T_tes_hot");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_T_wall_downcomer_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_wall_downcomer", length);
	if (!result)
		make_access_error("SAM_MsptIph", "T_wall_downcomer");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_T_wall_rec_inlet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_wall_rec_inlet", length);
	if (!result)
		make_access_error("SAM_MsptIph", "T_wall_rec_inlet");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_T_wall_rec_outlet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_wall_rec_outlet", length);
	if (!result)
		make_access_error("SAM_MsptIph", "T_wall_rec_outlet");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_T_wall_riser_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_wall_riser", length);
	if (!result)
		make_access_error("SAM_MsptIph", "T_wall_riser");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_V_tes_htf_avail_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tes_htf_avail_des", &result))
		make_access_error("SAM_MsptIph", "V_tes_htf_avail_des");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_V_tes_htf_total_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tes_htf_total_des", &result))
		make_access_error("SAM_MsptIph", "V_tes_htf_total_des");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_W_dot_bop_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_bop_design", &result))
		make_access_error("SAM_MsptIph", "W_dot_bop_design");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_W_dot_col_tracking_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_col_tracking_des", &result))
		make_access_error("SAM_MsptIph", "W_dot_col_tracking_des");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_W_dot_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_fixed", &result))
		make_access_error("SAM_MsptIph", "W_dot_fixed");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_W_dot_heater_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_heater", length);
	if (!result)
		make_access_error("SAM_MsptIph", "W_dot_heater");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_W_dot_heater_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_heater_des", &result))
		make_access_error("SAM_MsptIph", "W_dot_heater_des");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_W_dot_parasitic_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_parasitic_tot", length);
	if (!result)
		make_access_error("SAM_MsptIph", "W_dot_parasitic_tot");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_W_dot_pc_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_pc_pump", length);
	if (!result)
		make_access_error("SAM_MsptIph", "W_dot_pc_pump");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_W_dot_rec_pump_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_rec_pump_des", &result))
		make_access_error("SAM_MsptIph", "W_dot_rec_pump_des");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_W_dot_rec_pump_rec_share_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_rec_pump_rec_share_des", &result))
		make_access_error("SAM_MsptIph", "W_dot_rec_pump_rec_share_des");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_W_dot_rec_pump_tower_share_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_rec_pump_tower_share_des", &result))
		make_access_error("SAM_MsptIph", "W_dot_rec_pump_tower_share_des");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_annual_E_tower_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_E_tower_pump", &result))
		make_access_error("SAM_MsptIph", "annual_E_tower_pump");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_annual_electricity_consumption_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_electricity_consumption", &result))
		make_access_error("SAM_MsptIph", "annual_electricity_consumption");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_MsptIph", "annual_energy");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution_time", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "annual_energy_distribution_time");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_annual_energy_heat_btu_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy_heat_btu", &result))
		make_access_error("SAM_MsptIph", "annual_energy_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_annual_eta_rec_th_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_eta_rec_th", &result))
		make_access_error("SAM_MsptIph", "annual_eta_rec_th");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_annual_eta_rec_th_incl_refl_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_eta_rec_th_incl_refl", &result))
		make_access_error("SAM_MsptIph", "annual_eta_rec_th_incl_refl");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_annual_q_defocus_est_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_defocus_est", &result))
		make_access_error("SAM_MsptIph", "annual_q_defocus_est");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_annual_q_piping_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_piping_loss", &result))
		make_access_error("SAM_MsptIph", "annual_q_piping_loss");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_annual_q_rec_htf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_rec_htf", &result))
		make_access_error("SAM_MsptIph", "annual_q_rec_htf");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_annual_q_rec_inc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_rec_inc", &result))
		make_access_error("SAM_MsptIph", "annual_q_rec_inc");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_annual_q_rec_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_rec_loss", &result))
		make_access_error("SAM_MsptIph", "annual_q_rec_loss");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_annual_q_rec_startup_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_rec_startup", &result))
		make_access_error("SAM_MsptIph", "annual_q_rec_startup");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_annual_total_water_use_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_total_water_use", &result))
		make_access_error("SAM_MsptIph", "annual_total_water_use");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_average_attenuation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "average_attenuation", &result))
		make_access_error("SAM_MsptIph", "average_attenuation");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_MsptIph", "beam");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_MsptIph", "capacity_factor");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_cav_panel_width_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cav_panel_width", &result))
		make_access_error("SAM_MsptIph", "cav_panel_width");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_cav_radius_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cav_radius", &result))
		make_access_error("SAM_MsptIph", "cav_radius");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_cav_rec_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cav_rec_area", &result))
		make_access_error("SAM_MsptIph", "cav_rec_area");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_cav_rec_height_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cav_rec_height_calc", &result))
		make_access_error("SAM_MsptIph", "cav_rec_height_calc");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_cav_rec_width_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cav_rec_width_calc", &result))
		make_access_error("SAM_MsptIph", "cav_rec_width_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_clearsky_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "clearsky", length);
	if (!result)
		make_access_error("SAM_MsptIph", "clearsky");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_interest1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest1", &result))
		make_access_error("SAM_MsptIph", "const_per_interest1");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_interest2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest2", &result))
		make_access_error("SAM_MsptIph", "const_per_interest2");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_interest3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest3", &result))
		make_access_error("SAM_MsptIph", "const_per_interest3");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_interest4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest4", &result))
		make_access_error("SAM_MsptIph", "const_per_interest4");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_interest5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest5", &result))
		make_access_error("SAM_MsptIph", "const_per_interest5");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_interest_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_total", &result))
		make_access_error("SAM_MsptIph", "const_per_interest_total");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_percent_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent_total", &result))
		make_access_error("SAM_MsptIph", "const_per_percent_total");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_principal1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal1", &result))
		make_access_error("SAM_MsptIph", "const_per_principal1");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_principal2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal2", &result))
		make_access_error("SAM_MsptIph", "const_per_principal2");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_principal3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal3", &result))
		make_access_error("SAM_MsptIph", "const_per_principal3");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_principal4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal4", &result))
		make_access_error("SAM_MsptIph", "const_per_principal4");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_principal5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal5", &result))
		make_access_error("SAM_MsptIph", "const_per_principal5");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_principal_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal_total", &result))
		make_access_error("SAM_MsptIph", "const_per_principal_total");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_total1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total1", &result))
		make_access_error("SAM_MsptIph", "const_per_total1");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_total2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total2", &result))
		make_access_error("SAM_MsptIph", "const_per_total2");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_total3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total3", &result))
		make_access_error("SAM_MsptIph", "const_per_total3");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_total4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total4", &result))
		make_access_error("SAM_MsptIph", "const_per_total4");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_const_per_total5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total5", &result))
		make_access_error("SAM_MsptIph", "const_per_total5");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_financing_cost", &result))
		make_access_error("SAM_MsptIph", "construction_financing_cost");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_cp_battery_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cp_battery_nameplate", &result))
		make_access_error("SAM_MsptIph", "cp_battery_nameplate");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_cp_system_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cp_system_nameplate", &result))
		make_access_error("SAM_MsptIph", "cp_system_nameplate");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_csp_pt_cost_bop_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.bop", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.bop");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_csp_pt_cost_contingency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.contingency", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.contingency");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_csp_pt_cost_epc_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.total", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.epc.total");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_csp_pt_cost_fossil_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.fossil", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.fossil");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_csp_pt_cost_heliostats_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.heliostats", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.heliostats");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_csp_pt_cost_installed_per_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.installed_per_capacity", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.installed_per_capacity");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_csp_pt_cost_plm_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.total", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.plm.total");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_csp_pt_cost_power_block_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.power_block", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.power_block");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_csp_pt_cost_receiver_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.receiver", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.receiver");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_csp_pt_cost_sales_tax_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.sales_tax.total", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.sales_tax.total");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_csp_pt_cost_site_improvements_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.site_improvements", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.site_improvements");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_csp_pt_cost_storage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.storage", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.storage");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_csp_pt_cost_tower_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.tower", &result))
		make_access_error("SAM_MsptIph", "csp.pt.cost.tower");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_d_tank_tes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_tank_tes", &result))
		make_access_error("SAM_MsptIph", "d_tank_tes");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_defocus_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "defocus", length);
	if (!result)
		make_access_error("SAM_MsptIph", "defocus");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_dens_store_htf_at_T_ave_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dens_store_htf_at_T_ave", &result))
		make_access_error("SAM_MsptIph", "dens_store_htf_at_T_ave");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_obj_relax_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_obj_relax", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_obj_relax");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_objective_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_objective", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_objective");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_pceff_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_pceff_expected", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_pceff_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_presolve_nconstr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_presolve_nconstr", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_presolve_nconstr");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_presolve_nvar_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_presolve_nvar", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_presolve_nvar");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_qpbsu_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qpbsu_expected", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_qpbsu_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_qsf_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsf_expected", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_qsf_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_qsfprod_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsfprod_expected", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_qsfprod_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_qsfsu_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsfsu_expected", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_qsfsu_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_rel_mip_gap_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_rel_mip_gap", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_rel_mip_gap");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_rev_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_rev_expected", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_rev_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_solve_iter_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_iter", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_solve_iter");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_solve_state_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_state", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_solve_state");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_solve_time_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_time", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_solve_time");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_subopt_flag_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_subopt_flag", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_subopt_flag");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_tes_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_tes_expected", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_tes_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_thermeff_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_thermeff_expected", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_thermeff_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_disp_wpb_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_wpb_expected", length);
	if (!result)
		make_access_error("SAM_MsptIph", "disp_wpb_expected");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_ch_tes", length);
	if (!result)
		make_access_error("SAM_MsptIph", "e_ch_tes");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_eta_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_field", length);
	if (!result)
		make_access_error("SAM_MsptIph", "eta_field");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_eta_map_out_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "eta_map_out", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "eta_map_out");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_eta_rec_thermal_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_rec_thermal_des", &result))
		make_access_error("SAM_MsptIph", "eta_rec_thermal_des");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_eta_therm_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_therm", length);
	if (!result)
		make_access_error("SAM_MsptIph", "eta_therm");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_ext_rec_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ext_rec_area", &result))
		make_access_error("SAM_MsptIph", "ext_rec_area");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_ext_rec_aspect_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ext_rec_aspect", &result))
		make_access_error("SAM_MsptIph", "ext_rec_aspect");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_flux_maps_for_import_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "flux_maps_for_import", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "flux_maps_for_import");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_flux_maps_out_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "flux_maps_out", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "flux_maps_out");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_MsptIph", "gen");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_gen_heat_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen_heat", length);
	if (!result)
		make_access_error("SAM_MsptIph", "gen_heat");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_gen_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen_heat_btu", length);
	if (!result)
		make_access_error("SAM_MsptIph", "gen_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_h_rec_input_to_cost_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_rec_input_to_cost_model", &result))
		make_access_error("SAM_MsptIph", "h_rec_input_to_cost_model");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_h_tower_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tower_calc", &result))
		make_access_error("SAM_MsptIph", "h_tower_calc");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_heater_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heater_cost", &result))
		make_access_error("SAM_MsptIph", "heater_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_helio_positions_calc_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "helio_positions_calc", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptIph", "helio_positions_calc");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_heliostat_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heliostat_area", &result))
		make_access_error("SAM_MsptIph", "heliostat_area");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_is_PAR_HTR_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_PAR_HTR_allowed", length);
	if (!result)
		make_access_error("SAM_MsptIph", "is_PAR_HTR_allowed");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_is_pc_sb_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_sb_allowed", length);
	if (!result)
		make_access_error("SAM_MsptIph", "is_pc_sb_allowed");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_is_pc_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_su_allowed", length);
	if (!result)
		make_access_error("SAM_MsptIph", "is_pc_su_allowed");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_is_rec_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_rec_su_allowed", length);
	if (!result)
		make_access_error("SAM_MsptIph", "is_rec_su_allowed");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_MsptIph", "kwh_per_kw");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_land_area_base_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_area_base_calc", &result))
		make_access_error("SAM_MsptIph", "land_area_base_calc");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_land_max_abs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_max_abs", &result))
		make_access_error("SAM_MsptIph", "land_max_abs");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_land_min_abs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_min_abs", &result))
		make_access_error("SAM_MsptIph", "land_min_abs");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_balance", length);
	if (!result)
		make_access_error("SAM_MsptIph", "m_dot_balance");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_m_dot_cr_to_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_cr_to_tes_hot", length);
	if (!result)
		make_access_error("SAM_MsptIph", "m_dot_cr_to_tes_hot");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_m_dot_cycle_to_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_cycle_to_field", length);
	if (!result)
		make_access_error("SAM_MsptIph", "m_dot_cycle_to_field");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_m_dot_field_to_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_field_to_cycle", length);
	if (!result)
		make_access_error("SAM_MsptIph", "m_dot_field_to_cycle");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_m_dot_htf_heat_sink_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_htf_heat_sink", length);
	if (!result)
		make_access_error("SAM_MsptIph", "m_dot_htf_heat_sink");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_m_dot_htf_heater_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_htf_heater", length);
	if (!result)
		make_access_error("SAM_MsptIph", "m_dot_htf_heater");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_m_dot_htf_rec_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_rec_des", &result))
		make_access_error("SAM_MsptIph", "m_dot_htf_rec_des");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_m_dot_htf_rec_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_rec_max", &result))
		make_access_error("SAM_MsptIph", "m_dot_htf_rec_max");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_m_dot_pc_to_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pc_to_tes_cold", length);
	if (!result)
		make_access_error("SAM_MsptIph", "m_dot_pc_to_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_m_dot_rec_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_rec", length);
	if (!result)
		make_access_error("SAM_MsptIph", "m_dot_rec");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_m_dot_tes_cold_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_cold_out", length);
	if (!result)
		make_access_error("SAM_MsptIph", "m_dot_tes_cold_out");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_m_dot_tes_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_hot_out", length);
	if (!result)
		make_access_error("SAM_MsptIph", "m_dot_tes_hot_out");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_mass_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_cold", length);
	if (!result)
		make_access_error("SAM_MsptIph", "mass_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_mass_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_hot", length);
	if (!result)
		make_access_error("SAM_MsptIph", "mass_tes_hot");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "n_op_modes", length);
	if (!result)
		make_access_error("SAM_MsptIph", "n_op_modes");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nameplate", &result))
		make_access_error("SAM_MsptIph", "nameplate");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_od_tube_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "od_tube_calc", &result))
		make_access_error("SAM_MsptIph", "od_tube_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_1", length);
	if (!result)
		make_access_error("SAM_MsptIph", "op_mode_1");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_2", length);
	if (!result)
		make_access_error("SAM_MsptIph", "op_mode_2");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_3", length);
	if (!result)
		make_access_error("SAM_MsptIph", "op_mode_3");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_operating_modes_a_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_a", length);
	if (!result)
		make_access_error("SAM_MsptIph", "operating_modes_a");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_operating_modes_b_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_b", length);
	if (!result)
		make_access_error("SAM_MsptIph", "operating_modes_b");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_operating_modes_c_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_c", length);
	if (!result)
		make_access_error("SAM_MsptIph", "operating_modes_c");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ppa_price_input", length);
	if (!result)
		make_access_error("SAM_MsptIph", "ppa_price_input");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_pparasi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pparasi", length);
	if (!result)
		make_access_error("SAM_MsptIph", "pparasi");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_pricing_mult_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pricing_mult", length);
	if (!result)
		make_access_error("SAM_MsptIph", "pricing_mult");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_balance", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_balance");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_ch_tes", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_ch_tes");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dc_tes", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dc_tes");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dot_elec_to_PAR_HTR_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_elec_to_PAR_HTR", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dot_elec_to_PAR_HTR");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dot_est_cr_on_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_cr_on", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dot_est_cr_on");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dot_est_cr_su_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_cr_su", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dot_est_cr_su");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dot_est_tes_ch_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_tes_ch", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dot_est_tes_ch");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dot_est_tes_dc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_tes_dc", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dot_est_tes_dc");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_q_dot_heater_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_heater_des", &result))
		make_access_error("SAM_MsptIph", "q_dot_heater_des");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dot_heater_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_heater_startup", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dot_heater_startup");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dot_heater_to_htf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_heater_to_htf", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dot_heater_to_htf");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_q_dot_loss_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_loss_tes_des", &result))
		make_access_error("SAM_MsptIph", "q_dot_loss_tes_des");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dot_pc_max_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_max", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dot_pc_max");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dot_pc_min_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_min", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dot_pc_min");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dot_pc_sb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_sb", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dot_pc_sb");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dot_pc_target_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_target", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dot_pc_target");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_q_dot_piping_loss_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_piping_loss_des", &result))
		make_access_error("SAM_MsptIph", "q_dot_piping_loss_des");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_q_dot_rec_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_rec_des", &result))
		make_access_error("SAM_MsptIph", "q_dot_rec_des");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dot_rec_inc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_inc", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dot_rec_inc");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dot_reflection_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_reflection_loss", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dot_reflection_loss");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dot_tes_heater_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_tes_heater", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dot_tes_heater");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_dot_to_heat_sink_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_to_heat_sink", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_dot_to_heat_sink");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_piping_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_piping_losses", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_piping_losses");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_sf_inc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_sf_inc", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_sf_inc");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_startup", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_startup");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_q_thermal_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_thermal_loss", length);
	if (!result)
		make_access_error("SAM_MsptIph", "q_thermal_loss");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_rec_defocus_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rec_defocus", length);
	if (!result)
		make_access_error("SAM_MsptIph", "rec_defocus");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_rec_height_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_height_calc", &result))
		make_access_error("SAM_MsptIph", "rec_height_calc");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_refl_image_error_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "refl_image_error", &result))
		make_access_error("SAM_MsptIph", "refl_image_error");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_rh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rh", length);
	if (!result)
		make_access_error("SAM_MsptIph", "rh");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_sf_adjust_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sf_adjust_out", length);
	if (!result)
		make_access_error("SAM_MsptIph", "sf_adjust_out");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_sim_cpu_run_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_cpu_run_time", &result))
		make_access_error("SAM_MsptIph", "sim_cpu_run_time");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_solaz_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solaz", length);
	if (!result)
		make_access_error("SAM_MsptIph", "solaz");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solzen", length);
	if (!result)
		make_access_error("SAM_MsptIph", "solzen");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_MsptIph", "system_capacity");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_tank_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tank_losses", length);
	if (!result)
		make_access_error("SAM_MsptIph", "tank_losses");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_MsptIph", "tdry");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_tes_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_htf_pump_power", length);
	if (!result)
		make_access_error("SAM_MsptIph", "tes_htf_pump_power");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "time_hr", length);
	if (!result)
		make_access_error("SAM_MsptIph", "time_hr");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_total_direct_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_direct_cost", &result))
		make_access_error("SAM_MsptIph", "total_direct_cost");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_total_indirect_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_indirect_cost", &result))
		make_access_error("SAM_MsptIph", "total_indirect_cost");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_MsptIph", "total_installed_cost");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_total_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_land_area", &result))
		make_access_error("SAM_MsptIph", "total_land_area");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_total_land_area_before_rad_cooling_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_land_area_before_rad_cooling_calc", &result))
		make_access_error("SAM_MsptIph", "total_land_area_before_rad_cooling_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_tou_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tou_value", length);
	if (!result)
		make_access_error("SAM_MsptIph", "tou_value");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_tshours_heater_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours_heater", &result))
		make_access_error("SAM_MsptIph", "tshours_heater");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_tshours_rec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours_rec", &result))
		make_access_error("SAM_MsptIph", "tshours_rec");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_MsptIph", "twet");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_ui_direct_subtotal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ui_direct_subtotal", &result))
		make_access_error("SAM_MsptIph", "ui_direct_subtotal");
	});
	return result;
}

SAM_EXPORT double SAM_MsptIph_Outputs_vel_rec_htf_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vel_rec_htf_des", &result))
		make_access_error("SAM_MsptIph", "vel_rec_htf_des");
	});
	return result;
}

SAM_EXPORT double* SAM_MsptIph_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_MsptIph", "wspd");
	});
	return result;
}


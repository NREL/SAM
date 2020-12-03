#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_TcsmoltenSalt.h"

SAM_EXPORT int SAM_TcsmoltenSalt_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("tcsmolten_salt", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_TcsmoltenSalt_SolarResource_solar_resource_data_tset(SAM_table ptr, SAM_table tab, SAM_error *err){
	SAM_table_set_table(ptr, "solar_resource_data", tab, err);
}



SAM_EXPORT void SAM_TcsmoltenSalt_SolarResource_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "solar_resource_file", str);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor1", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor2", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor3", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor4", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor5", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor6_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor6", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor7_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor7", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor8_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor8", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor9_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor9", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_factors_ts", arr, length);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_multiplier_model", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_A_sf_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "A_sf_in", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_N_hel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "N_hel", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_c_atm_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_0", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_c_atm_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_1", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_c_atm_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_2", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_c_atm_3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_3", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_calc_fluxmaps_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calc_fluxmaps", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_cant_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cant_type", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_check_max_flux_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "check_max_flux", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_csp_pt_sf_fixed_land_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.sf.fixed_land_area", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_csp_pt_sf_land_overhead_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.sf.land_overhead_factor", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_dens_mirror_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dens_mirror", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_eta_map_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "eta_map", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_eta_map_aod_format_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_map_aod_format", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_field_model_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "field_model_type", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_flux_maps_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "flux_maps", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_focus_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "focus_type", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_hel_stow_deploy_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hel_stow_deploy", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_active_fraction_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_active_fraction", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_aim_points_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "helio_aim_points", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_height", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_optical_error_mrad_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_optical_error_mrad", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_positions_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "helio_positions", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_reflectance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_reflectance", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_width_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_width", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_interp_beta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "interp_beta", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_interp_nug_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "interp_nug", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_land_area_base_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_area_base", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_land_bound_list_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "land_bound_list", arr, length);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_land_bound_table_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "land_bound_table", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_land_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_max", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_land_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_min", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_n_facet_x_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_facet_x", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_n_facet_y_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_facet_y", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_opt_algorithm_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_algorithm", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_opt_conv_tol_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_conv_tol", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_opt_flux_penalty_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_flux_penalty", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_opt_init_step_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_init_step", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_opt_max_iter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_max_iter", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_p_start_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "p_start", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_p_track_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "p_track", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_v_wind_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "v_wind_max", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_washing_frequency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "washing_frequency", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_water_usage_per_wash_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "water_usage_per_wash", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_P_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_ref", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_T_htf_cold_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_cold_des", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_hot_des", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_design_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "design_eff", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_dni_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dni_des", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_gross_net_conversion_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gross_net_conversion_factor", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_sf_excess_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sf_excess", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_solarm_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "solarm", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_tshours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tshours", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_D_rec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "D_rec", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_Flow_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Flow_type", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_N_panels_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "N_panels", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_crossover_shift_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "crossover_shift", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_csp_pt_rec_max_oper_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.rec.max_oper_frac", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_d_tube_out_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "d_tube_out", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_delta_flux_hrs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "delta_flux_hrs", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_downc_tm_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "downc_tm_mult", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_epsilon_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epsilon", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_eta_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pump", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_f_rec_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_rec_min", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "field_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_flux_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "flux_max", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_h_tower_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tower", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_heat_trace_power_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heat_trace_power", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_hl_ffact_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hl_ffact", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_enforce_min_startup_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_rec_enforce_min_startup", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_model_trans_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_rec_model_trans", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_startup_from_T_soln_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_rec_startup_from_T_soln", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_startup_trans_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_rec_startup_trans", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_mat_tube_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mat_tube", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_min_fill_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "min_fill_time", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_min_preheat_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "min_preheat_time", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_n_flux_days_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_flux_days", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_piping_length_const_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_length_const", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_piping_length_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_length_mult", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_piping_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_loss", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_preheat_flux_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "preheat_flux", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_absorptance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_absorptance", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_clearsky_dni_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "rec_clearsky_dni", arr, length);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_clearsky_fraction_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_clearsky_fraction", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_clearsky_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_clearsky_model", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_height", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_hl_perm2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_hl_perm2", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_htf_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_htf", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_qf_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_qf_delay", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_su_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_su_delay", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_tm_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_tm_mult", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_riser_tm_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "riser_tm_mult", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_startup_ramp_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_ramp_time", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_startup_target_Tdiff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_target_Tdiff", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_th_riser_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "th_riser", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_th_tube_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "th_tube", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_u_riser_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_riser", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_bop_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_spec_cost", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_contingency_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "contingency_rate", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_cost_sf_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cost_sf_fixed", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.fixed", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_per_acre_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.per_acre", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.per_watt", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.percent", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_plm_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.plm.fixed", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_plm_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.plm.per_watt", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_plm_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.plm.percent", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_fossil_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fossil_spec_cost", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_heliostat_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heliostat_spec_cost", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_land_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_spec_cost", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_plant_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "plant_spec_cost", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_rec_cost_exp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_cost_exp", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_rec_ref_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_ref_area", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_rec_ref_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_ref_cost", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_sales_tax_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_frac", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_site_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "site_spec_cost", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_tes_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_spec_cost", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_tower_exp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tower_exp", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_tower_fixed_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tower_fixed_cost", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate1", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate2", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate3", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate4", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate5", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months1", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months2", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months3", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months4", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months5", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent1", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent2", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent3", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent4", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent5", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate1", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate2", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate3", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate4", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate5", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_rate", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_csp_pt_tes_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.tes.init_hot_htf_percent", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_h_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_min", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_pairs", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_tanks_in_parallel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tanks_in_parallel", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_u_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_D_rad_tubes_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "D_rad_tubes", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_L_rad_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_rad", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_L_rad_sections_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_rad_sections", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_T_ctes_cold_design_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ctes_cold_design", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_T_ctes_cold_ini_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ctes_cold_ini", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_T_ctes_warm_design_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ctes_warm_design", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_T_ctes_warm_ini_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ctes_warm_ini", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_W_rad_tubes_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "W_rad_tubes", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_ctes_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ctes_cost", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_ctes_field_fl_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ctes_field_fl", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_ctes_tankpairs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ctes_tankpairs", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_ctes_tshours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ctes_tshours", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_ctes_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ctes_type", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_epsilon_radHX_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epsilon_radHX", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_epsilon_radbot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epsilon_radbot", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_epsilon_radgrnd_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epsilon_radgrnd", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_epsilon_radtop_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epsilon_radtop", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_f_ctes_warm_ini_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_ctes_warm_ini", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_h_ctes_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_ctes_tank", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_h_ctes_tank_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_ctes_tank_min", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_helio_area_tot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_area_tot", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_k_panel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "k_panel", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_m_dot_radpanel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_radpanel", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_n_rad_tubes_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_rad_tubes", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_rad_multiplier_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rad_multiplier", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_rad_pressuredrop_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rad_pressuredrop", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_radfluid_vol_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "radfluid_vol_ratio", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_radiator_fluidcost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "radiator_fluidcost", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_radiator_installcost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "radiator_installcost", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_radiator_unitcost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "radiator_unitcost", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_th_rad_panel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "th_rad_panel", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_u_ctes_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_ctes_tank", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_PowerCycle_cycle_cutoff_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_cutoff_frac", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_PowerCycle_cycle_max_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_max_frac", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_PowerCycle_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_pump_coef", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_PowerCycle_pc_config_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pc_config", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_PowerCycle_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_sby_frac", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_PowerCycle_startup_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_frac", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_PowerCycle_startup_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_time", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_CT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_P_boil_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_boil", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_P_cond_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_min", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_P_cond_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_ratio", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ITD_des", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_T_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_approach", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_dT_cw_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_cw_ref", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_n_pl_inc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_pl_inc", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_pb_bd_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_bd_frac", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_tech_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tech_type", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_F_wc_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "F_wc", arr, length);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_ampl_data_dir_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "ampl_data_dir", str);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_ampl_exec_call_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "ampl_exec_call", str);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_aux_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "aux_par", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_aux_par_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "aux_par_0", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_aux_par_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "aux_par_1", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_aux_par_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "aux_par_2", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_aux_par_f_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "aux_par_f", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_bop_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_bop_par_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_0", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_bop_par_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_1", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_bop_par_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_2", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_bop_par_f_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_f", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_csu_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_csu_cost", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_frequency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_frequency", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_horizon_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_horizon", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_inventory_incentive_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_inventory_incentive", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_max_iter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_max_iter", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_mip_gap_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_mip_gap", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_pen_delta_w_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_pen_delta_w", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_reporting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_reporting", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_rsu_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_rsu_cost", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_spec_bb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_bb", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_spec_presolve_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_presolve", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_spec_scaling_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_scaling", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_steps_per_hour", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_time_weighting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_time_weighting", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_timeout_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_timeout", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_dispatch_series_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_series", arr, length);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_f_turb_tou_periods_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "f_turb_tou_periods", arr, length);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_is_ampl_engine_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_ampl_engine", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_is_dispatch_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_dispatch", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_is_dispatch_series_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_dispatch_series", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_is_tod_pc_target_also_pc_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_tod_pc_target_also_pc_max", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_is_wlim_series_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_wlim_series", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_is_write_ampl_dat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_write_ampl_dat", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_fixed_par", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_q_rec_heattrace_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_rec_heattrace", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_q_rec_standby_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_rec_standby", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_time_start_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_start", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_time_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_steps_per_hour", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_time_stop_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_stop", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_vacuum_arrays_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "vacuum_arrays", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekday_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekend_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_wlim_series_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "wlim_series", arr, length);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_f_W_dot_cool_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_f_W_dot_cool_des", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_m_dot_water_cool_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_m_dot_water_cool_des", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_P_high_limit_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_high_limit", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_P_high_limit_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "_sco2_P_high_limit", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_P_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "_sco2_P_ref", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "_sco2_T_amb_des", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_T_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "_sco2_T_approach", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "_sco2_T_htf_hot_des", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_deltaT_PHX_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "_sco2_deltaT_PHX", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_design_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "_sco2_design_eff", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_eta_c_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "_sco2_eta_c", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_eta_t_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "_sco2_eta_t", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_recup_eff_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "_sco2_recup_eff_max", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_deltaT_PHX_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "deltaT_PHX", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_eta_c_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_c", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_eta_t_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_t", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_fan_power_perc_net_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fan_power_perc_net", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_is_sco2_preprocess_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_sco2_preprocess", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_recup_eff_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "recup_eff_max", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sco2_T_amb_des", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2_T_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sco2_T_approach", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2_cycle_config_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sco2_cycle_config", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_amb_high_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sco2ud_T_amb_high", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_amb_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sco2ud_T_amb_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_amb_low_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sco2ud_T_amb_low", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_cold_calc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sco2ud_T_htf_cold_calc", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_high_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sco2ud_T_htf_high", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sco2ud_T_htf_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_low_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sco2ud_T_htf_low", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_m_dot_htf_high_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sco2ud_m_dot_htf_high", number);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_m_dot_htf_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sco2ud_m_dot_htf_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_m_dot_htf_low_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sco2ud_m_dot_htf_low", number);
	});
}

SAM_EXPORT SAM_table SAM_TcsmoltenSalt_SolarResource_solar_resource_data_tget(SAM_table ptr, SAM_error *err){
	SAM_table result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_table(ptr, "solar_resource_data");
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "solar_resource_data");
	});
	return result;
}



SAM_EXPORT const char* SAM_TcsmoltenSalt_SolarResource_solar_resource_file_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "solar_resource_file");
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "solar_resource_file");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor1", &result))
		make_access_error("SAM_TcsmoltenSalt", "dispatch_factor1");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor2", &result))
		make_access_error("SAM_TcsmoltenSalt", "dispatch_factor2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor3", &result))
		make_access_error("SAM_TcsmoltenSalt", "dispatch_factor3");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor4", &result))
		make_access_error("SAM_TcsmoltenSalt", "dispatch_factor4");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor5", &result))
		make_access_error("SAM_TcsmoltenSalt", "dispatch_factor5");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor6_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor6", &result))
		make_access_error("SAM_TcsmoltenSalt", "dispatch_factor6");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor7_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor7", &result))
		make_access_error("SAM_TcsmoltenSalt", "dispatch_factor7");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor8_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor8", &result))
		make_access_error("SAM_TcsmoltenSalt", "dispatch_factor8");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor9_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor9", &result))
		make_access_error("SAM_TcsmoltenSalt", "dispatch_factor9");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_factors_ts", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "dispatch_factors_ts");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "dispatch_sched_weekday");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "dispatch_sched_weekend");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_multiplier_model", &result))
		make_access_error("SAM_TcsmoltenSalt", "ppa_multiplier_model");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_A_sf_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_sf_in", &result))
		make_access_error("SAM_TcsmoltenSalt", "A_sf_in");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_N_hel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_hel", &result))
		make_access_error("SAM_TcsmoltenSalt", "N_hel");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_c_atm_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_0", &result))
		make_access_error("SAM_TcsmoltenSalt", "c_atm_0");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_c_atm_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_1", &result))
		make_access_error("SAM_TcsmoltenSalt", "c_atm_1");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_c_atm_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_2", &result))
		make_access_error("SAM_TcsmoltenSalt", "c_atm_2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_c_atm_3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_3", &result))
		make_access_error("SAM_TcsmoltenSalt", "c_atm_3");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_calc_fluxmaps_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calc_fluxmaps", &result))
		make_access_error("SAM_TcsmoltenSalt", "calc_fluxmaps");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_cant_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cant_type", &result))
		make_access_error("SAM_TcsmoltenSalt", "cant_type");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_check_max_flux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "check_max_flux", &result))
		make_access_error("SAM_TcsmoltenSalt", "check_max_flux");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_csp_pt_sf_fixed_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.sf.fixed_land_area", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.sf.fixed_land_area");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_csp_pt_sf_land_overhead_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.sf.land_overhead_factor", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.sf.land_overhead_factor");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_dens_mirror_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dens_mirror", &result))
		make_access_error("SAM_TcsmoltenSalt", "dens_mirror");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_HeliostatField_eta_map_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "eta_map", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "eta_map");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_eta_map_aod_format_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_map_aod_format", &result))
		make_access_error("SAM_TcsmoltenSalt", "eta_map_aod_format");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_field_model_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_model_type", &result))
		make_access_error("SAM_TcsmoltenSalt", "field_model_type");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_HeliostatField_flux_maps_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "flux_maps", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "flux_maps");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_focus_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "focus_type", &result))
		make_access_error("SAM_TcsmoltenSalt", "focus_type");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_hel_stow_deploy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hel_stow_deploy", &result))
		make_access_error("SAM_TcsmoltenSalt", "hel_stow_deploy");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_helio_active_fraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_active_fraction", &result))
		make_access_error("SAM_TcsmoltenSalt", "helio_active_fraction");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_HeliostatField_helio_aim_points_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "helio_aim_points", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "helio_aim_points");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_helio_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_height", &result))
		make_access_error("SAM_TcsmoltenSalt", "helio_height");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_helio_optical_error_mrad_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_optical_error_mrad", &result))
		make_access_error("SAM_TcsmoltenSalt", "helio_optical_error_mrad");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_HeliostatField_helio_positions_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "helio_positions", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "helio_positions");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_helio_reflectance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_reflectance", &result))
		make_access_error("SAM_TcsmoltenSalt", "helio_reflectance");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_helio_width_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_width", &result))
		make_access_error("SAM_TcsmoltenSalt", "helio_width");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_interp_beta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "interp_beta", &result))
		make_access_error("SAM_TcsmoltenSalt", "interp_beta");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_interp_nug_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "interp_nug", &result))
		make_access_error("SAM_TcsmoltenSalt", "interp_nug");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_land_area_base_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_area_base", &result))
		make_access_error("SAM_TcsmoltenSalt", "land_area_base");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_HeliostatField_land_bound_list_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "land_bound_list", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "land_bound_list");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_HeliostatField_land_bound_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "land_bound_table", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "land_bound_table");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_land_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_max", &result))
		make_access_error("SAM_TcsmoltenSalt", "land_max");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_land_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_min", &result))
		make_access_error("SAM_TcsmoltenSalt", "land_min");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_n_facet_x_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_facet_x", &result))
		make_access_error("SAM_TcsmoltenSalt", "n_facet_x");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_n_facet_y_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_facet_y", &result))
		make_access_error("SAM_TcsmoltenSalt", "n_facet_y");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_opt_algorithm_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_algorithm", &result))
		make_access_error("SAM_TcsmoltenSalt", "opt_algorithm");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_opt_conv_tol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_conv_tol", &result))
		make_access_error("SAM_TcsmoltenSalt", "opt_conv_tol");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_opt_flux_penalty_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_flux_penalty", &result))
		make_access_error("SAM_TcsmoltenSalt", "opt_flux_penalty");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_opt_init_step_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_init_step", &result))
		make_access_error("SAM_TcsmoltenSalt", "opt_init_step");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_opt_max_iter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_max_iter", &result))
		make_access_error("SAM_TcsmoltenSalt", "opt_max_iter");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_p_start_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "p_start", &result))
		make_access_error("SAM_TcsmoltenSalt", "p_start");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_p_track_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "p_track", &result))
		make_access_error("SAM_TcsmoltenSalt", "p_track");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_v_wind_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "v_wind_max", &result))
		make_access_error("SAM_TcsmoltenSalt", "v_wind_max");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_washing_frequency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "washing_frequency", &result))
		make_access_error("SAM_TcsmoltenSalt", "washing_frequency");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_water_usage_per_wash_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "water_usage_per_wash", &result))
		make_access_error("SAM_TcsmoltenSalt", "water_usage_per_wash");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_P_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_ref", &result))
		make_access_error("SAM_TcsmoltenSalt", "P_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_cold_des", &result))
		make_access_error("SAM_TcsmoltenSalt", "T_htf_cold_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_hot_des", &result))
		make_access_error("SAM_TcsmoltenSalt", "T_htf_hot_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_design_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "design_eff", &result))
		make_access_error("SAM_TcsmoltenSalt", "design_eff");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_dni_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dni_des", &result))
		make_access_error("SAM_TcsmoltenSalt", "dni_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_gross_net_conversion_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gross_net_conversion_factor", &result))
		make_access_error("SAM_TcsmoltenSalt", "gross_net_conversion_factor");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_sf_excess_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sf_excess", &result))
		make_access_error("SAM_TcsmoltenSalt", "sf_excess");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_solarm_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solarm", &result))
		make_access_error("SAM_TcsmoltenSalt", "solarm");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_tshours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours", &result))
		make_access_error("SAM_TcsmoltenSalt", "tshours");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_D_rec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "D_rec", &result))
		make_access_error("SAM_TcsmoltenSalt", "D_rec");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_Flow_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Flow_type", &result))
		make_access_error("SAM_TcsmoltenSalt", "Flow_type");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_N_panels_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_panels", &result))
		make_access_error("SAM_TcsmoltenSalt", "N_panels");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_crossover_shift_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "crossover_shift", &result))
		make_access_error("SAM_TcsmoltenSalt", "crossover_shift");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_csp_pt_rec_max_oper_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.rec.max_oper_frac", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.rec.max_oper_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_d_tube_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_tube_out", &result))
		make_access_error("SAM_TcsmoltenSalt", "d_tube_out");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_delta_flux_hrs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "delta_flux_hrs", &result))
		make_access_error("SAM_TcsmoltenSalt", "delta_flux_hrs");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_downc_tm_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "downc_tm_mult", &result))
		make_access_error("SAM_TcsmoltenSalt", "downc_tm_mult");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_epsilon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epsilon", &result))
		make_access_error("SAM_TcsmoltenSalt", "epsilon");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_eta_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pump", &result))
		make_access_error("SAM_TcsmoltenSalt", "eta_pump");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_f_rec_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_rec_min", &result))
		make_access_error("SAM_TcsmoltenSalt", "f_rec_min");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_TowerAndReceiver_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "field_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "field_fl_props");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_flux_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flux_max", &result))
		make_access_error("SAM_TcsmoltenSalt", "flux_max");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_h_tower_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tower", &result))
		make_access_error("SAM_TcsmoltenSalt", "h_tower");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_heat_trace_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heat_trace_power", &result))
		make_access_error("SAM_TcsmoltenSalt", "heat_trace_power");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_hl_ffact_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hl_ffact", &result))
		make_access_error("SAM_TcsmoltenSalt", "hl_ffact");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_enforce_min_startup_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_rec_enforce_min_startup", &result))
		make_access_error("SAM_TcsmoltenSalt", "is_rec_enforce_min_startup");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_model_trans_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_rec_model_trans", &result))
		make_access_error("SAM_TcsmoltenSalt", "is_rec_model_trans");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_startup_from_T_soln_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_rec_startup_from_T_soln", &result))
		make_access_error("SAM_TcsmoltenSalt", "is_rec_startup_from_T_soln");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_startup_trans_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_rec_startup_trans", &result))
		make_access_error("SAM_TcsmoltenSalt", "is_rec_startup_trans");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_mat_tube_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mat_tube", &result))
		make_access_error("SAM_TcsmoltenSalt", "mat_tube");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_min_fill_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "min_fill_time", &result))
		make_access_error("SAM_TcsmoltenSalt", "min_fill_time");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_min_preheat_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "min_preheat_time", &result))
		make_access_error("SAM_TcsmoltenSalt", "min_preheat_time");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_n_flux_days_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_flux_days", &result))
		make_access_error("SAM_TcsmoltenSalt", "n_flux_days");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_piping_length_const_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_length_const", &result))
		make_access_error("SAM_TcsmoltenSalt", "piping_length_const");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_piping_length_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_length_mult", &result))
		make_access_error("SAM_TcsmoltenSalt", "piping_length_mult");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_piping_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_loss", &result))
		make_access_error("SAM_TcsmoltenSalt", "piping_loss");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_preheat_flux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "preheat_flux", &result))
		make_access_error("SAM_TcsmoltenSalt", "preheat_flux");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_absorptance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_absorptance", &result))
		make_access_error("SAM_TcsmoltenSalt", "rec_absorptance");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_TowerAndReceiver_rec_clearsky_dni_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rec_clearsky_dni", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "rec_clearsky_dni");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_clearsky_fraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_clearsky_fraction", &result))
		make_access_error("SAM_TcsmoltenSalt", "rec_clearsky_fraction");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_clearsky_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_clearsky_model", &result))
		make_access_error("SAM_TcsmoltenSalt", "rec_clearsky_model");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_height", &result))
		make_access_error("SAM_TcsmoltenSalt", "rec_height");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_hl_perm2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_hl_perm2", &result))
		make_access_error("SAM_TcsmoltenSalt", "rec_hl_perm2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_htf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_htf", &result))
		make_access_error("SAM_TcsmoltenSalt", "rec_htf");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_qf_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_qf_delay", &result))
		make_access_error("SAM_TcsmoltenSalt", "rec_qf_delay");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_su_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_su_delay", &result))
		make_access_error("SAM_TcsmoltenSalt", "rec_su_delay");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_tm_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_tm_mult", &result))
		make_access_error("SAM_TcsmoltenSalt", "rec_tm_mult");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_riser_tm_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "riser_tm_mult", &result))
		make_access_error("SAM_TcsmoltenSalt", "riser_tm_mult");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_startup_ramp_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_ramp_time", &result))
		make_access_error("SAM_TcsmoltenSalt", "startup_ramp_time");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_startup_target_Tdiff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_target_Tdiff", &result))
		make_access_error("SAM_TcsmoltenSalt", "startup_target_Tdiff");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_th_riser_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "th_riser", &result))
		make_access_error("SAM_TcsmoltenSalt", "th_riser");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_th_tube_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "th_tube", &result))
		make_access_error("SAM_TcsmoltenSalt", "th_tube");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_u_riser_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_riser", &result))
		make_access_error("SAM_TcsmoltenSalt", "u_riser");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_bop_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_spec_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "bop_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_contingency_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_rate", &result))
		make_access_error("SAM_TcsmoltenSalt", "contingency_rate");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_cost_sf_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_sf_fixed", &result))
		make_access_error("SAM_TcsmoltenSalt", "cost_sf_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.fixed", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.epc.fixed");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_per_acre_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.per_acre", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.epc.per_acre");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.per_watt", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.epc.per_watt");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.percent", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.epc.percent");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_plm_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.fixed", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.plm.fixed");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_plm_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.per_watt", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.plm.per_watt");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_plm_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.percent", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.plm.percent");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_fossil_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fossil_spec_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "fossil_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_heliostat_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heliostat_spec_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "heliostat_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_land_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_spec_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "land_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_plant_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plant_spec_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "plant_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_rec_cost_exp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_cost_exp", &result))
		make_access_error("SAM_TcsmoltenSalt", "rec_cost_exp");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_rec_ref_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_ref_area", &result))
		make_access_error("SAM_TcsmoltenSalt", "rec_ref_area");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_rec_ref_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_ref_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "rec_ref_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_sales_tax_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_frac", &result))
		make_access_error("SAM_TcsmoltenSalt", "sales_tax_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_site_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "site_spec_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "site_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_tes_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_spec_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "tes_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_tower_exp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_exp", &result))
		make_access_error("SAM_TcsmoltenSalt", "tower_exp");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_tower_fixed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_fixed_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "tower_fixed_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate1", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_interest_rate1");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate2", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_interest_rate2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate3", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_interest_rate3");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate4", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_interest_rate4");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate5", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_interest_rate5");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months1", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_months1");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months2", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_months2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months3", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_months3");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months4", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_months4");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months5", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_months5");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent1", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_percent1");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent2", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_percent2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent3", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_percent3");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent4", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_percent4");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent5", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_percent5");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate1", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_upfront_rate1");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate2", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_upfront_rate2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate3", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_upfront_rate3");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate4", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_upfront_rate4");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate5", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_upfront_rate5");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_sales_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_rate", &result))
		make_access_error("SAM_TcsmoltenSalt", "sales_tax_rate");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_Thtr", &result))
		make_access_error("SAM_TcsmoltenSalt", "cold_tank_Thtr");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_max_heat", &result))
		make_access_error("SAM_TcsmoltenSalt", "cold_tank_max_heat");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_csp_pt_tes_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.tes.init_hot_htf_percent", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.tes.init_hot_htf_percent");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_h_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank", &result))
		make_access_error("SAM_TcsmoltenSalt", "h_tank");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_h_tank_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_min", &result))
		make_access_error("SAM_TcsmoltenSalt", "h_tank_min");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_Thtr", &result))
		make_access_error("SAM_TcsmoltenSalt", "hot_tank_Thtr");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_max_heat", &result))
		make_access_error("SAM_TcsmoltenSalt", "hot_tank_max_heat");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_tank_pairs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_pairs", &result))
		make_access_error("SAM_TcsmoltenSalt", "tank_pairs");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_tanks_in_parallel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tanks_in_parallel", &result))
		make_access_error("SAM_TcsmoltenSalt", "tanks_in_parallel");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_u_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_tank", &result))
		make_access_error("SAM_TcsmoltenSalt", "u_tank");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_D_rad_tubes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "D_rad_tubes", &result))
		make_access_error("SAM_TcsmoltenSalt", "D_rad_tubes");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_L_rad_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_rad", &result))
		make_access_error("SAM_TcsmoltenSalt", "L_rad");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_L_rad_sections_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_rad_sections", &result))
		make_access_error("SAM_TcsmoltenSalt", "L_rad_sections");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_T_ctes_cold_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ctes_cold_design", &result))
		make_access_error("SAM_TcsmoltenSalt", "T_ctes_cold_design");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_T_ctes_cold_ini_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ctes_cold_ini", &result))
		make_access_error("SAM_TcsmoltenSalt", "T_ctes_cold_ini");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_T_ctes_warm_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ctes_warm_design", &result))
		make_access_error("SAM_TcsmoltenSalt", "T_ctes_warm_design");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_T_ctes_warm_ini_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ctes_warm_ini", &result))
		make_access_error("SAM_TcsmoltenSalt", "T_ctes_warm_ini");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_W_rad_tubes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_rad_tubes", &result))
		make_access_error("SAM_TcsmoltenSalt", "W_rad_tubes");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_ctes_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ctes_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "ctes_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_ctes_field_fl_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ctes_field_fl", &result))
		make_access_error("SAM_TcsmoltenSalt", "ctes_field_fl");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_ctes_tankpairs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ctes_tankpairs", &result))
		make_access_error("SAM_TcsmoltenSalt", "ctes_tankpairs");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_ctes_tshours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ctes_tshours", &result))
		make_access_error("SAM_TcsmoltenSalt", "ctes_tshours");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_ctes_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ctes_type", &result))
		make_access_error("SAM_TcsmoltenSalt", "ctes_type");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_epsilon_radHX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epsilon_radHX", &result))
		make_access_error("SAM_TcsmoltenSalt", "epsilon_radHX");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_epsilon_radbot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epsilon_radbot", &result))
		make_access_error("SAM_TcsmoltenSalt", "epsilon_radbot");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_epsilon_radgrnd_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epsilon_radgrnd", &result))
		make_access_error("SAM_TcsmoltenSalt", "epsilon_radgrnd");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_epsilon_radtop_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epsilon_radtop", &result))
		make_access_error("SAM_TcsmoltenSalt", "epsilon_radtop");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_f_ctes_warm_ini_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_ctes_warm_ini", &result))
		make_access_error("SAM_TcsmoltenSalt", "f_ctes_warm_ini");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_h_ctes_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_ctes_tank", &result))
		make_access_error("SAM_TcsmoltenSalt", "h_ctes_tank");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_h_ctes_tank_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_ctes_tank_min", &result))
		make_access_error("SAM_TcsmoltenSalt", "h_ctes_tank_min");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_helio_area_tot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_area_tot", &result))
		make_access_error("SAM_TcsmoltenSalt", "helio_area_tot");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_k_panel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "k_panel", &result))
		make_access_error("SAM_TcsmoltenSalt", "k_panel");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_m_dot_radpanel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_radpanel", &result))
		make_access_error("SAM_TcsmoltenSalt", "m_dot_radpanel");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_n_rad_tubes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_rad_tubes", &result))
		make_access_error("SAM_TcsmoltenSalt", "n_rad_tubes");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_rad_multiplier_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rad_multiplier", &result))
		make_access_error("SAM_TcsmoltenSalt", "rad_multiplier");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_rad_pressuredrop_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rad_pressuredrop", &result))
		make_access_error("SAM_TcsmoltenSalt", "rad_pressuredrop");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_radfluid_vol_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "radfluid_vol_ratio", &result))
		make_access_error("SAM_TcsmoltenSalt", "radfluid_vol_ratio");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_radiator_fluidcost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "radiator_fluidcost", &result))
		make_access_error("SAM_TcsmoltenSalt", "radiator_fluidcost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_radiator_installcost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "radiator_installcost", &result))
		make_access_error("SAM_TcsmoltenSalt", "radiator_installcost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_radiator_unitcost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "radiator_unitcost", &result))
		make_access_error("SAM_TcsmoltenSalt", "radiator_unitcost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_th_rad_panel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "th_rad_panel", &result))
		make_access_error("SAM_TcsmoltenSalt", "th_rad_panel");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_u_ctes_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_ctes_tank", &result))
		make_access_error("SAM_TcsmoltenSalt", "u_ctes_tank");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_PowerCycle_cycle_cutoff_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_cutoff_frac", &result))
		make_access_error("SAM_TcsmoltenSalt", "cycle_cutoff_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_PowerCycle_cycle_max_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_max_frac", &result))
		make_access_error("SAM_TcsmoltenSalt", "cycle_max_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_PowerCycle_pb_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_pump_coef", &result))
		make_access_error("SAM_TcsmoltenSalt", "pb_pump_coef");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_PowerCycle_pc_config_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_config", &result))
		make_access_error("SAM_TcsmoltenSalt", "pc_config");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_PowerCycle_q_sby_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_sby_frac", &result))
		make_access_error("SAM_TcsmoltenSalt", "q_sby_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_PowerCycle_startup_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_frac", &result))
		make_access_error("SAM_TcsmoltenSalt", "startup_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_PowerCycle_startup_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_time", &result))
		make_access_error("SAM_TcsmoltenSalt", "startup_time");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_CT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT", &result))
		make_access_error("SAM_TcsmoltenSalt", "CT");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_P_boil_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_boil", &result))
		make_access_error("SAM_TcsmoltenSalt", "P_boil");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_P_cond_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_min", &result))
		make_access_error("SAM_TcsmoltenSalt", "P_cond_min");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_P_cond_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_ratio", &result))
		make_access_error("SAM_TcsmoltenSalt", "P_cond_ratio");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_T_ITD_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ITD_des", &result))
		make_access_error("SAM_TcsmoltenSalt", "T_ITD_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_TcsmoltenSalt", "T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_T_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_approach", &result))
		make_access_error("SAM_TcsmoltenSalt", "T_approach");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_dT_cw_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_cw_ref", &result))
		make_access_error("SAM_TcsmoltenSalt", "dT_cw_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_n_pl_inc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_pl_inc", &result))
		make_access_error("SAM_TcsmoltenSalt", "n_pl_inc");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_pb_bd_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_bd_frac", &result))
		make_access_error("SAM_TcsmoltenSalt", "pb_bd_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_tech_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tech_type", &result))
		make_access_error("SAM_TcsmoltenSalt", "tech_type");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_SystemControl_F_wc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "F_wc", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "F_wc");
	});
	return result;
}



SAM_EXPORT const char* SAM_TcsmoltenSalt_SystemControl_ampl_data_dir_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "ampl_data_dir");
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "ampl_data_dir");
	});
	return result;
}



SAM_EXPORT const char* SAM_TcsmoltenSalt_SystemControl_ampl_exec_call_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "ampl_exec_call");
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "ampl_exec_call");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_aux_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_par", &result))
		make_access_error("SAM_TcsmoltenSalt", "aux_par");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_aux_par_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_par_0", &result))
		make_access_error("SAM_TcsmoltenSalt", "aux_par_0");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_aux_par_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_par_1", &result))
		make_access_error("SAM_TcsmoltenSalt", "aux_par_1");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_aux_par_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_par_2", &result))
		make_access_error("SAM_TcsmoltenSalt", "aux_par_2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_aux_par_f_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_par_f", &result))
		make_access_error("SAM_TcsmoltenSalt", "aux_par_f");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_bop_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par", &result))
		make_access_error("SAM_TcsmoltenSalt", "bop_par");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_bop_par_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_0", &result))
		make_access_error("SAM_TcsmoltenSalt", "bop_par_0");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_bop_par_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_1", &result))
		make_access_error("SAM_TcsmoltenSalt", "bop_par_1");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_bop_par_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_2", &result))
		make_access_error("SAM_TcsmoltenSalt", "bop_par_2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_bop_par_f_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_f", &result))
		make_access_error("SAM_TcsmoltenSalt", "bop_par_f");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_csu_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_csu_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_csu_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_frequency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_frequency", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_frequency");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_horizon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_horizon", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_horizon");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_inventory_incentive_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_inventory_incentive", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_inventory_incentive");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_max_iter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_max_iter", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_max_iter");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_mip_gap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_mip_gap", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_mip_gap");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_pen_delta_w_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_pen_delta_w", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_pen_delta_w");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_reporting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_reporting", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_reporting");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_rsu_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_rsu_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_rsu_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_spec_bb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_bb", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_spec_bb");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_spec_presolve_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_presolve", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_spec_presolve");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_spec_scaling_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_scaling", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_spec_scaling");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_steps_per_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_steps_per_hour", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_steps_per_hour");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_time_weighting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_time_weighting", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_time_weighting");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_timeout_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_timeout", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_timeout");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_SystemControl_dispatch_series_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_series", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "dispatch_series");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_SystemControl_f_turb_tou_periods_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "f_turb_tou_periods", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "f_turb_tou_periods");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_is_ampl_engine_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_ampl_engine", &result))
		make_access_error("SAM_TcsmoltenSalt", "is_ampl_engine");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_is_dispatch_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_dispatch", &result))
		make_access_error("SAM_TcsmoltenSalt", "is_dispatch");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_is_dispatch_series_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_dispatch_series", &result))
		make_access_error("SAM_TcsmoltenSalt", "is_dispatch_series");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_is_tod_pc_target_also_pc_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_tod_pc_target_also_pc_max", &result))
		make_access_error("SAM_TcsmoltenSalt", "is_tod_pc_target_also_pc_max");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_is_wlim_series_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_wlim_series", &result))
		make_access_error("SAM_TcsmoltenSalt", "is_wlim_series");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_is_write_ampl_dat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_write_ampl_dat", &result))
		make_access_error("SAM_TcsmoltenSalt", "is_write_ampl_dat");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_pb_fixed_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_fixed_par", &result))
		make_access_error("SAM_TcsmoltenSalt", "pb_fixed_par");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_q_rec_heattrace_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_rec_heattrace", &result))
		make_access_error("SAM_TcsmoltenSalt", "q_rec_heattrace");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_q_rec_standby_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_rec_standby", &result))
		make_access_error("SAM_TcsmoltenSalt", "q_rec_standby");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_time_start_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_start", &result))
		make_access_error("SAM_TcsmoltenSalt", "time_start");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_time_steps_per_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_steps_per_hour", &result))
		make_access_error("SAM_TcsmoltenSalt", "time_steps_per_hour");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_time_stop_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_stop", &result))
		make_access_error("SAM_TcsmoltenSalt", "time_stop");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_vacuum_arrays_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vacuum_arrays", &result))
		make_access_error("SAM_TcsmoltenSalt", "vacuum_arrays");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_SystemControl_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekday_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "weekday_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_SystemControl_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekend_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "weekend_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_SystemControl_wlim_series_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wlim_series", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "wlim_series");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_f_W_dot_cool_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_f_W_dot_cool_des", &result))
		make_access_error("SAM_TcsmoltenSalt", "ud_f_W_dot_cool_des");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "ud_ind_od");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_m_dot_water_cool_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_m_dot_water_cool_des", &result))
		make_access_error("SAM_TcsmoltenSalt", "ud_m_dot_water_cool_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_P_high_limit_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_high_limit", &result))
		make_access_error("SAM_TcsmoltenSalt", "P_high_limit");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_P_high_limit_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "_sco2_P_high_limit", &result))
		make_access_error("SAM_TcsmoltenSalt", "_sco2_P_high_limit");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_P_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "_sco2_P_ref", &result))
		make_access_error("SAM_TcsmoltenSalt", "_sco2_P_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "_sco2_T_amb_des", &result))
		make_access_error("SAM_TcsmoltenSalt", "_sco2_T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_T_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "_sco2_T_approach", &result))
		make_access_error("SAM_TcsmoltenSalt", "_sco2_T_approach");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "_sco2_T_htf_hot_des", &result))
		make_access_error("SAM_TcsmoltenSalt", "_sco2_T_htf_hot_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_deltaT_PHX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "_sco2_deltaT_PHX", &result))
		make_access_error("SAM_TcsmoltenSalt", "_sco2_deltaT_PHX");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_design_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "_sco2_design_eff", &result))
		make_access_error("SAM_TcsmoltenSalt", "_sco2_design_eff");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_eta_c_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "_sco2_eta_c", &result))
		make_access_error("SAM_TcsmoltenSalt", "_sco2_eta_c");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_eta_t_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "_sco2_eta_t", &result))
		make_access_error("SAM_TcsmoltenSalt", "_sco2_eta_t");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_recup_eff_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "_sco2_recup_eff_max", &result))
		make_access_error("SAM_TcsmoltenSalt", "_sco2_recup_eff_max");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_deltaT_PHX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "deltaT_PHX", &result))
		make_access_error("SAM_TcsmoltenSalt", "deltaT_PHX");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_eta_c_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_c", &result))
		make_access_error("SAM_TcsmoltenSalt", "eta_c");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_eta_t_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_t", &result))
		make_access_error("SAM_TcsmoltenSalt", "eta_t");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_fan_power_perc_net_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fan_power_perc_net", &result))
		make_access_error("SAM_TcsmoltenSalt", "fan_power_perc_net");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_is_sco2_preprocess_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_sco2_preprocess", &result))
		make_access_error("SAM_TcsmoltenSalt", "is_sco2_preprocess");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_recup_eff_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "recup_eff_max", &result))
		make_access_error("SAM_TcsmoltenSalt", "recup_eff_max");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sco2_T_amb_des", &result))
		make_access_error("SAM_TcsmoltenSalt", "sco2_T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2_T_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sco2_T_approach", &result))
		make_access_error("SAM_TcsmoltenSalt", "sco2_T_approach");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2_cycle_config_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sco2_cycle_config", &result))
		make_access_error("SAM_TcsmoltenSalt", "sco2_cycle_config");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_amb_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sco2ud_T_amb_high", &result))
		make_access_error("SAM_TcsmoltenSalt", "sco2ud_T_amb_high");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_amb_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sco2ud_T_amb_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "sco2ud_T_amb_ind_od");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_amb_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sco2ud_T_amb_low", &result))
		make_access_error("SAM_TcsmoltenSalt", "sco2ud_T_amb_low");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_cold_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sco2ud_T_htf_cold_calc", &result))
		make_access_error("SAM_TcsmoltenSalt", "sco2ud_T_htf_cold_calc");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sco2ud_T_htf_high", &result))
		make_access_error("SAM_TcsmoltenSalt", "sco2ud_T_htf_high");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sco2ud_T_htf_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "sco2ud_T_htf_ind_od");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sco2ud_T_htf_low", &result))
		make_access_error("SAM_TcsmoltenSalt", "sco2ud_T_htf_low");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_m_dot_htf_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sco2ud_m_dot_htf_high", &result))
		make_access_error("SAM_TcsmoltenSalt", "sco2ud_m_dot_htf_high");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_m_dot_htf_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sco2ud_m_dot_htf_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "sco2ud_m_dot_htf_ind_od");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_m_dot_htf_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sco2ud_m_dot_htf_low", &result))
		make_access_error("SAM_TcsmoltenSalt", "sco2ud_m_dot_htf_low");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_A_radfield_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_radfield", &result))
		make_access_error("SAM_TcsmoltenSalt", "A_radfield");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_A_sf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_sf", &result))
		make_access_error("SAM_TcsmoltenSalt", "A_sf");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_cond_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cond", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "P_cond");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_cooling_tower_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cooling_tower_tot", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "P_cooling_tower_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cycle", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "P_cycle");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_fixed", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "P_fixed");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_out_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_out_net", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "P_out_net");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_plant_balance_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_plant_balance_tot", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "P_plant_balance_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_rec_heattrace_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_rec_heattrace", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "P_rec_heattrace");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_tower_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_tower_pump", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "P_tower_pump");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_Q_thermal_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_thermal", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "Q_thermal");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_Q_thermal_ss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_thermal_ss", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "Q_thermal_ss");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_Q_thermal_ss_csky_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_thermal_ss_csky", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "Q_thermal_ss_csky");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_cold", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_cond_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_cond_out", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_cond_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_panel_out_max_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_panel_out_max", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_panel_out_max");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_pc_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pc_in", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_pc_in");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_pc_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pc_out", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_pc_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_rad_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rad_out", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_rad_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_rec_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_in", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_rec_in");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_rec_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_out", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_rec_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_rec_out_end_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_out_end", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_rec_out_end");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_rec_out_max_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rec_out_max", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_rec_out_max");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_cold", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_tes_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_hot", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_tes_hot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_wall_downcomer_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_wall_downcomer", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_wall_downcomer");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_wall_rec_inlet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_wall_rec_inlet", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_wall_rec_inlet");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_wall_rec_outlet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_wall_rec_outlet", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_wall_rec_outlet");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_wall_riser_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_wall_riser", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_wall_riser");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_warm_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_warm", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "T_warm");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_W_cooling_tower_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_W_cooling_tower", &result))
		make_access_error("SAM_TcsmoltenSalt", "annual_W_cooling_tower");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_W_cycle_gross_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_W_cycle_gross", &result))
		make_access_error("SAM_TcsmoltenSalt", "annual_W_cycle_gross");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_TcsmoltenSalt", "annual_energy");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_eta_rec_th_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_eta_rec_th", &result))
		make_access_error("SAM_TcsmoltenSalt", "annual_eta_rec_th");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_eta_rec_th_incl_refl_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_eta_rec_th_incl_refl", &result))
		make_access_error("SAM_TcsmoltenSalt", "annual_eta_rec_th_incl_refl");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_q_rec_inc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_rec_inc", &result))
		make_access_error("SAM_TcsmoltenSalt", "annual_q_rec_inc");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_q_rec_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_rec_loss", &result))
		make_access_error("SAM_TcsmoltenSalt", "annual_q_rec_loss");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_total_water_use_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_total_water_use", &result))
		make_access_error("SAM_TcsmoltenSalt", "annual_total_water_use");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "beam");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_TcsmoltenSalt", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_clearsky_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "clearsky", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "clearsky");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_interest1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest1", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_interest1");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_interest2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest2", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_interest2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_interest3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest3", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_interest3");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_interest4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest4", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_interest4");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_interest5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest5", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_interest5");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_interest_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_total", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_interest_total");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_percent_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent_total", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_percent_total");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_principal1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal1", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_principal1");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_principal2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal2", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_principal2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_principal3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal3", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_principal3");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_principal4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal4", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_principal4");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_principal5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal5", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_principal5");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_principal_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal_total", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_principal_total");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_total1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total1", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_total1");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_total2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total2", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_total2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_total3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total3", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_total3");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_total4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total4", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_total4");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_total5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total5", &result))
		make_access_error("SAM_TcsmoltenSalt", "const_per_total5");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_financing_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "construction_financing_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_conversion_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conversion_factor", &result))
		make_access_error("SAM_TcsmoltenSalt", "conversion_factor");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_bop_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.bop", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.bop");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_contingency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.contingency", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.contingency");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_epc_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.total", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.epc.total");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_fossil_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.fossil", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.fossil");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_heliostats_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.heliostats", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.heliostats");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_installed_per_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.installed_per_capacity", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.installed_per_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_plm_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.total", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.plm.total");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_power_block_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.power_block", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.power_block");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_rad_field_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.rad_field", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.rad_field");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_rad_fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.rad_fluid", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.rad_fluid");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_rad_storage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.rad_storage", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.rad_storage");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_receiver_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.receiver", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.receiver");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_sales_tax_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.sales_tax.total", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.sales_tax.total");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_site_improvements_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.site_improvements", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.site_improvements");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_storage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.storage", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.storage");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_total_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.total_land_area", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.total_land_area");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_tower_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.tower", &result))
		make_access_error("SAM_TcsmoltenSalt", "csp.pt.cost.tower");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_defocus_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "defocus", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "defocus");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_disp_iter_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_iter_ann", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_iter_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_obj_relax_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_obj_relax", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_obj_relax");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_objective_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_objective", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_objective");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_disp_objective_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_objective_ann", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_objective_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_pceff_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_pceff_expected", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_pceff_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_presolve_nconstr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_presolve_nconstr", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_presolve_nconstr");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_disp_presolve_nconstr_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_presolve_nconstr_ann", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_presolve_nconstr_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_presolve_nvar_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_presolve_nvar", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_presolve_nvar");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_disp_presolve_nvar_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_presolve_nvar_ann", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_presolve_nvar_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_qpbsu_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qpbsu_expected", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_qpbsu_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_qsf_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsf_expected", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_qsf_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_qsfprod_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsfprod_expected", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_qsfprod_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_qsfsu_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsfsu_expected", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_qsfsu_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_rev_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_rev_expected", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_rev_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_solve_iter_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_iter", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_solve_iter");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_solve_state_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_state", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_solve_state");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_solve_time_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_time", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_solve_time");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_disp_solve_time_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_solve_time_ann", &result))
		make_access_error("SAM_TcsmoltenSalt", "disp_solve_time_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_tes_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_tes_expected", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_tes_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_thermeff_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_thermeff_expected", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_thermeff_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_wpb_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_wpb_expected", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "disp_wpb_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_ch_tes", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "e_ch_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_eta_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "eta");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_eta_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_field", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "eta_field");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_eta_map_out_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "eta_map_out", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "eta_map_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_eta_therm_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_therm", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "eta_therm");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_flux_maps_for_import_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "flux_maps_for_import", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "flux_maps_for_import");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_flux_maps_out_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "flux_maps_out", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "flux_maps_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "htf_pump_power", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "htf_pump_power");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_is_pc_sb_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_sb_allowed", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "is_pc_sb_allowed");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_is_pc_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_su_allowed", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "is_pc_su_allowed");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_is_rec_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_rec_su_allowed", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "is_rec_su_allowed");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_TcsmoltenSalt", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_cold", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "m_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_balance", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "m_dot_balance");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_cr_to_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_cr_to_tes_hot", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "m_dot_cr_to_tes_hot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_cycle_to_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_cycle_to_field", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "m_dot_cycle_to_field");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_field_to_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_field_to_cycle", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "m_dot_field_to_cycle");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_pc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pc", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "m_dot_pc");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_pc_to_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pc_to_tes_cold", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "m_dot_pc_to_tes_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_rec_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_rec", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "m_dot_rec");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_tes_cold_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_cold_out", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "m_dot_tes_cold_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_tes_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_hot_out", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "m_dot_tes_hot_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_water_pc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_water_pc", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "m_dot_water_pc");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_warm_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_warm", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "m_warm");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_mass_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_cold", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "mass_tes_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_mass_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_hot", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "mass_tes_hot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "n_op_modes", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "n_op_modes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_1", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "op_mode_1");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_2", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "op_mode_2");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_3", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "op_mode_3");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_operating_modes_a_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_a", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "operating_modes_a");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_operating_modes_b_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_b", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "operating_modes_b");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_operating_modes_c_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_c", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "operating_modes_c");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_pparasi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pparasi", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "pparasi");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_pricing_mult_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pricing_mult", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "pricing_mult");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_balance", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_balance");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_ch_tes", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_ch_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dc_tes", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_dc_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_est_cr_on_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_cr_on", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_dot_est_cr_on");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_est_cr_su_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_cr_su", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_dot_est_cr_su");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_est_tes_ch_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_tes_ch", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_dot_est_tes_ch");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_est_tes_dc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_tes_dc", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_dot_est_tes_dc");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_pc_max_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_max", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_dot_pc_max");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_pc_min_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_min", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_dot_pc_min");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_pc_sb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_sb", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_dot_pc_sb");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_startup", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_dot_pc_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_pc_target_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_target", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_dot_pc_target");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_rec_inc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_inc", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_dot_rec_inc");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_heater_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_heater", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_heater");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_pb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pb", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_pb");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pc_startup", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_pc_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_piping_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_piping_losses", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_piping_losses");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_sf_inc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_sf_inc", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_sf_inc");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_startup", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_thermal_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_thermal_loss", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "q_thermal_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_radcool_control_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "radcool_control", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "radcool_control");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_rh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rh", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "rh");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_sco2_preprocess_table_out_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sco2_preprocess_table_out", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "sco2_preprocess_table_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_sf_adjust_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sf_adjust_out", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "sf_adjust_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_solaz_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solaz", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "solaz");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solzen", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "solzen");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_tank_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tank_losses", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "tank_losses");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "tdry");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "time_hr", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "time_hr");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_total_direct_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_direct_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "total_direct_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_total_indirect_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_indirect_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "total_indirect_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_TcsmoltenSalt", "total_installed_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_tou_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tou_value", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "tou_value");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "twet");
	});
	return result;
}



SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_ui_direct_subtotal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ui_direct_subtotal", &result))
		make_access_error("SAM_TcsmoltenSalt", "ui_direct_subtotal");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_TcsmoltenSalt", "wspd");
	});
	return result;
}




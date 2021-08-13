#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_TcsdirectSteam.h"

SAM_EXPORT int SAM_TcsdirectSteam_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("tcsdirect_steam", data, verbosity, err);
}

SAM_EXPORT void SAM_TcsdirectSteam_Weather_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "solar_resource_file", str);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DirectSteamTower_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_TouTranslator_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekday_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_TouTranslator_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekend_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_N_hel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "N_hel", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_bop_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_spec_cost", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_c_atm_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_0", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_c_atm_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_1", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_c_atm_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_2", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_c_atm_3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_3", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_calc_fluxmaps_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calc_fluxmaps", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_cant_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cant_type", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_check_max_flux_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "check_max_flux", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_contingency_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "contingency_rate", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_cost_sf_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cost_sf_fixed", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.fixed", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_per_acre_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.per_acre", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.per_watt", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.percent", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.plm.fixed", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_per_acre_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.plm.per_acre", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.plm.per_watt", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.plm.percent", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_sf_fixed_land_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.sf.fixed_land_area", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_sf_land_overhead_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.sf.land_overhead_factor", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_delta_flux_hrs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "delta_flux_hrs", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_dens_mirror_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dens_mirror", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_dni_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dni_des", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_eta_map_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "eta_map", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_flux_maps_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "flux_maps", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_flux_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "flux_max", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_flux_positions_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "flux_positions", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_focus_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "focus_type", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_fossil_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fossil_spec_cost", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_hel_stow_deploy_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hel_stow_deploy", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_helio_active_fraction_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_active_fraction", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_helio_aim_points_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "helio_aim_points", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_helio_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_height", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_helio_optical_error_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_optical_error", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_helio_positions_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "helio_positions", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_helio_reflectance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_reflectance", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_helio_width_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_width", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_heliostat_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heliostat_spec_cost", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_interp_beta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "interp_beta", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_interp_nug_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "interp_nug", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_is_optimize_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_optimize", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_land_bound_list_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "land_bound_list", arr, length);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_land_bound_table_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "land_bound_table", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_land_bound_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_bound_type", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_land_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_max", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_land_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_min", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_land_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_spec_cost", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_n_facet_x_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_facet_x", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_n_facet_y_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_facet_y", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_n_flux_days_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_flux_days", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_n_flux_x_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_flux_x", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_n_flux_y_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_flux_y", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_opt_algorithm_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_algorithm", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_opt_conv_tol_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_conv_tol", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_opt_flux_penalty_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_flux_penalty", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_opt_init_step_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_init_step", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_opt_max_iter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_max_iter", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_p_start_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "p_start", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_p_track_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "p_track", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_plant_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "plant_spec_cost", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_q_design_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_design", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_rec_absorptance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_absorptance", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_rec_aspect_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_aspect", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_rec_cost_exp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_cost_exp", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_rec_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_height", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_rec_hl_perm2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_hl_perm2", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_rec_ref_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_ref_area", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_rec_ref_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_ref_cost", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_run_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "run_type", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_sales_tax_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_frac", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_rate", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_site_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "site_spec_cost", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_tes_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_spec_cost", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_total_installed_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "total_installed_cost", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_tower_exp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tower_exp", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_tower_fixed_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tower_fixed_cost", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_v_wind_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "v_wind_max", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_washing_frequency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "washing_frequency", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_water_usage_per_wash_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "water_usage_per_wash", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Receiver_H_rec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "H_rec", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Receiver_THT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "THT", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_A_sf_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "A_sf", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_P_b_in_init_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_b_in_init", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_P_cond_init_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_init", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_P_hp_in_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_hp_in_des", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_P_hp_out_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_hp_out", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_P_hp_out_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_hp_out_des", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ITD_des", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_approach", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_fw_init_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_fw_init", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_hp_out_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_hp_out", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_rh_out_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_rh_out_des", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_rh_target_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_rh_target", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_sh_out_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_sh_out_des", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_ct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ct", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_cycle_max_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_max_frac", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_dT_cw_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_cw_ref", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_d_rec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "d_rec", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_d_rh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "d_rh", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_d_sh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "d_sh", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_d_t_boiler_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "d_t_boiler", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_eta_rec_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_rec_pump", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_f_mdot_rh_init_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_mdot_rh_init", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_f_mdotrh_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_mdotrh_des", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_f_pb_cutoff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_pb_cutoff", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_f_pb_sb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_pb_sb", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_f_rec_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_rec_min", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_ffrac_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ffrac", arr, length);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_flowtype_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "flowtype", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_fossil_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fossil_mode", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_h_boiler_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_boiler", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_h_rh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_rh", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_h_sh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_sh", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_h_tower_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tower", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_hl_ffact_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hl_ffact", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_lhv_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "lhv_eff", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_mat_boiler_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mat_boiler", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_mat_rh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mat_rh", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_mat_sh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mat_sh", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_n_panels_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_panels", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_p_cycle_design_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "p_cycle_design", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_q_aux_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_aux_max", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_q_pb_design_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_pb_design", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_q_rec_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_rec_des", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_rec_absorptance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_absorptance", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_rec_emis_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_emis", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_rec_qf_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_qf_delay", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_rec_su_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_su_delay", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_t_standby_ini_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "t_standby_ini", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_th_rh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "th_rh", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_th_sh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "th_sh", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_th_t_boiler_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "th_t_boiler", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_DsgController_x_b_target_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "x_b_target", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_F_wc_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "F_wc", arr, length);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_P_boil_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_boil_des", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_P_cond_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_min", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_P_cond_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_ratio", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_P_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_ref", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_P_rh_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_rh_ref", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ITD_des", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_T_cold_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_cold_ref", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_T_hot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_hot", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_T_hot_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_hot_ref", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_dT_cw_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_cw_ref", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_eta_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_ref", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_n_pl_inc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_pl_inc", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_pb_bd_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_bd_frac", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_sby_frac", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_rh_frac_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rh_frac_ref", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_startup_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_frac", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_startup_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_time", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_Design_power_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Design_power", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_Piping_length_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Piping_length", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_Piping_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Piping_loss", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_aux_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "aux_par", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_aux_par_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "aux_par_0", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_aux_par_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "aux_par_1", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_aux_par_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "aux_par_2", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_aux_par_f_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "aux_par_f", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_bop_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_bop_par_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_0", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_bop_par_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_1", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_bop_par_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_2", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_bop_par_f_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_f", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_design_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "design_eff", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_fixed_par", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_piping_length_add_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_length_add", number);
	});
}

SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_piping_length_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_length_mult", number);
	});
}

SAM_EXPORT const char* SAM_TcsdirectSteam_Weather_solar_resource_file_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "solar_resource_file");
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "solar_resource_file");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DirectSteamTower_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_TcsdirectSteam", "system_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_TouTranslator_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekday_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "weekday_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_TouTranslator_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekend_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "weekend_schedule");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_N_hel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_hel", &result))
		make_access_error("SAM_TcsdirectSteam", "N_hel");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_bop_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_spec_cost", &result))
		make_access_error("SAM_TcsdirectSteam", "bop_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_c_atm_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_0", &result))
		make_access_error("SAM_TcsdirectSteam", "c_atm_0");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_c_atm_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_1", &result))
		make_access_error("SAM_TcsdirectSteam", "c_atm_1");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_c_atm_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_2", &result))
		make_access_error("SAM_TcsdirectSteam", "c_atm_2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_c_atm_3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_3", &result))
		make_access_error("SAM_TcsdirectSteam", "c_atm_3");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_calc_fluxmaps_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calc_fluxmaps", &result))
		make_access_error("SAM_TcsdirectSteam", "calc_fluxmaps");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_cant_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cant_type", &result))
		make_access_error("SAM_TcsdirectSteam", "cant_type");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_check_max_flux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "check_max_flux", &result))
		make_access_error("SAM_TcsdirectSteam", "check_max_flux");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_contingency_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_rate", &result))
		make_access_error("SAM_TcsdirectSteam", "contingency_rate");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_cost_sf_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_sf_fixed", &result))
		make_access_error("SAM_TcsdirectSteam", "cost_sf_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.fixed", &result))
		make_access_error("SAM_TcsdirectSteam", "csp.pt.cost.epc.fixed");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_per_acre_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.per_acre", &result))
		make_access_error("SAM_TcsdirectSteam", "csp.pt.cost.epc.per_acre");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.per_watt", &result))
		make_access_error("SAM_TcsdirectSteam", "csp.pt.cost.epc.per_watt");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.percent", &result))
		make_access_error("SAM_TcsdirectSteam", "csp.pt.cost.epc.percent");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.fixed", &result))
		make_access_error("SAM_TcsdirectSteam", "csp.pt.cost.plm.fixed");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_per_acre_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.per_acre", &result))
		make_access_error("SAM_TcsdirectSteam", "csp.pt.cost.plm.per_acre");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.per_watt", &result))
		make_access_error("SAM_TcsdirectSteam", "csp.pt.cost.plm.per_watt");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.percent", &result))
		make_access_error("SAM_TcsdirectSteam", "csp.pt.cost.plm.percent");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_csp_pt_sf_fixed_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.sf.fixed_land_area", &result))
		make_access_error("SAM_TcsdirectSteam", "csp.pt.sf.fixed_land_area");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_csp_pt_sf_land_overhead_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.sf.land_overhead_factor", &result))
		make_access_error("SAM_TcsdirectSteam", "csp.pt.sf.land_overhead_factor");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_delta_flux_hrs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "delta_flux_hrs", &result))
		make_access_error("SAM_TcsdirectSteam", "delta_flux_hrs");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_dens_mirror_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dens_mirror", &result))
		make_access_error("SAM_TcsdirectSteam", "dens_mirror");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_dni_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dni_des", &result))
		make_access_error("SAM_TcsdirectSteam", "dni_des");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Heliostat_eta_map_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "eta_map", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "eta_map");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Heliostat_flux_maps_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "flux_maps", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "flux_maps");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_flux_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flux_max", &result))
		make_access_error("SAM_TcsdirectSteam", "flux_max");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Heliostat_flux_positions_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "flux_positions", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "flux_positions");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_focus_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "focus_type", &result))
		make_access_error("SAM_TcsdirectSteam", "focus_type");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_fossil_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fossil_spec_cost", &result))
		make_access_error("SAM_TcsdirectSteam", "fossil_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_hel_stow_deploy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hel_stow_deploy", &result))
		make_access_error("SAM_TcsdirectSteam", "hel_stow_deploy");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_helio_active_fraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_active_fraction", &result))
		make_access_error("SAM_TcsdirectSteam", "helio_active_fraction");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Heliostat_helio_aim_points_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "helio_aim_points", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "helio_aim_points");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_helio_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_height", &result))
		make_access_error("SAM_TcsdirectSteam", "helio_height");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_helio_optical_error_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_optical_error", &result))
		make_access_error("SAM_TcsdirectSteam", "helio_optical_error");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Heliostat_helio_positions_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "helio_positions", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "helio_positions");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_helio_reflectance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_reflectance", &result))
		make_access_error("SAM_TcsdirectSteam", "helio_reflectance");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_helio_width_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_width", &result))
		make_access_error("SAM_TcsdirectSteam", "helio_width");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_heliostat_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heliostat_spec_cost", &result))
		make_access_error("SAM_TcsdirectSteam", "heliostat_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_interp_beta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "interp_beta", &result))
		make_access_error("SAM_TcsdirectSteam", "interp_beta");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_interp_nug_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "interp_nug", &result))
		make_access_error("SAM_TcsdirectSteam", "interp_nug");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_is_optimize_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_optimize", &result))
		make_access_error("SAM_TcsdirectSteam", "is_optimize");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Heliostat_land_bound_list_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "land_bound_list", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "land_bound_list");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Heliostat_land_bound_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "land_bound_table", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "land_bound_table");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_land_bound_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_bound_type", &result))
		make_access_error("SAM_TcsdirectSteam", "land_bound_type");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_land_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_max", &result))
		make_access_error("SAM_TcsdirectSteam", "land_max");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_land_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_min", &result))
		make_access_error("SAM_TcsdirectSteam", "land_min");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_land_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_spec_cost", &result))
		make_access_error("SAM_TcsdirectSteam", "land_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_n_facet_x_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_facet_x", &result))
		make_access_error("SAM_TcsdirectSteam", "n_facet_x");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_n_facet_y_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_facet_y", &result))
		make_access_error("SAM_TcsdirectSteam", "n_facet_y");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_n_flux_days_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_flux_days", &result))
		make_access_error("SAM_TcsdirectSteam", "n_flux_days");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_n_flux_x_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_flux_x", &result))
		make_access_error("SAM_TcsdirectSteam", "n_flux_x");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_n_flux_y_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_flux_y", &result))
		make_access_error("SAM_TcsdirectSteam", "n_flux_y");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_opt_algorithm_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_algorithm", &result))
		make_access_error("SAM_TcsdirectSteam", "opt_algorithm");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_opt_conv_tol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_conv_tol", &result))
		make_access_error("SAM_TcsdirectSteam", "opt_conv_tol");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_opt_flux_penalty_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_flux_penalty", &result))
		make_access_error("SAM_TcsdirectSteam", "opt_flux_penalty");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_opt_init_step_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_init_step", &result))
		make_access_error("SAM_TcsdirectSteam", "opt_init_step");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_opt_max_iter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_max_iter", &result))
		make_access_error("SAM_TcsdirectSteam", "opt_max_iter");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_p_start_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "p_start", &result))
		make_access_error("SAM_TcsdirectSteam", "p_start");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_p_track_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "p_track", &result))
		make_access_error("SAM_TcsdirectSteam", "p_track");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_plant_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plant_spec_cost", &result))
		make_access_error("SAM_TcsdirectSteam", "plant_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_q_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_design", &result))
		make_access_error("SAM_TcsdirectSteam", "q_design");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_rec_absorptance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_absorptance", &result))
		make_access_error("SAM_TcsdirectSteam", "rec_absorptance");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_rec_aspect_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_aspect", &result))
		make_access_error("SAM_TcsdirectSteam", "rec_aspect");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_rec_cost_exp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_cost_exp", &result))
		make_access_error("SAM_TcsdirectSteam", "rec_cost_exp");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_rec_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_height", &result))
		make_access_error("SAM_TcsdirectSteam", "rec_height");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_rec_hl_perm2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_hl_perm2", &result))
		make_access_error("SAM_TcsdirectSteam", "rec_hl_perm2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_rec_ref_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_ref_area", &result))
		make_access_error("SAM_TcsdirectSteam", "rec_ref_area");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_rec_ref_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_ref_cost", &result))
		make_access_error("SAM_TcsdirectSteam", "rec_ref_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_run_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "run_type", &result))
		make_access_error("SAM_TcsdirectSteam", "run_type");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_sales_tax_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_frac", &result))
		make_access_error("SAM_TcsdirectSteam", "sales_tax_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_sales_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_rate", &result))
		make_access_error("SAM_TcsdirectSteam", "sales_tax_rate");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_site_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "site_spec_cost", &result))
		make_access_error("SAM_TcsdirectSteam", "site_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_tes_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_spec_cost", &result))
		make_access_error("SAM_TcsdirectSteam", "tes_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_TcsdirectSteam", "total_installed_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_tower_exp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_exp", &result))
		make_access_error("SAM_TcsdirectSteam", "tower_exp");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_tower_fixed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_fixed_cost", &result))
		make_access_error("SAM_TcsdirectSteam", "tower_fixed_cost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_v_wind_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "v_wind_max", &result))
		make_access_error("SAM_TcsdirectSteam", "v_wind_max");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_washing_frequency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "washing_frequency", &result))
		make_access_error("SAM_TcsdirectSteam", "washing_frequency");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Heliostat_water_usage_per_wash_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "water_usage_per_wash", &result))
		make_access_error("SAM_TcsdirectSteam", "water_usage_per_wash");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Receiver_H_rec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "H_rec", &result))
		make_access_error("SAM_TcsdirectSteam", "H_rec");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Receiver_THT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "THT", &result))
		make_access_error("SAM_TcsdirectSteam", "THT");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_A_sf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_sf", &result))
		make_access_error("SAM_TcsdirectSteam", "A_sf");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_P_b_in_init_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_b_in_init", &result))
		make_access_error("SAM_TcsdirectSteam", "P_b_in_init");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_P_cond_init_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_init", &result))
		make_access_error("SAM_TcsdirectSteam", "P_cond_init");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_P_hp_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_hp_in_des", &result))
		make_access_error("SAM_TcsdirectSteam", "P_hp_in_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_P_hp_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_hp_out", &result))
		make_access_error("SAM_TcsdirectSteam", "P_hp_out");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_P_hp_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_hp_out_des", &result))
		make_access_error("SAM_TcsdirectSteam", "P_hp_out_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_T_ITD_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ITD_des", &result))
		make_access_error("SAM_TcsdirectSteam", "T_ITD_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_TcsdirectSteam", "T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_T_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_approach", &result))
		make_access_error("SAM_TcsdirectSteam", "T_approach");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_T_fw_init_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_fw_init", &result))
		make_access_error("SAM_TcsdirectSteam", "T_fw_init");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_T_hp_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_hp_out", &result))
		make_access_error("SAM_TcsdirectSteam", "T_hp_out");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_T_rh_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_rh_out_des", &result))
		make_access_error("SAM_TcsdirectSteam", "T_rh_out_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_T_rh_target_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_rh_target", &result))
		make_access_error("SAM_TcsdirectSteam", "T_rh_target");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_T_sh_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_sh_out_des", &result))
		make_access_error("SAM_TcsdirectSteam", "T_sh_out_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_ct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ct", &result))
		make_access_error("SAM_TcsdirectSteam", "ct");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_cycle_max_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_max_frac", &result))
		make_access_error("SAM_TcsdirectSteam", "cycle_max_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_dT_cw_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_cw_ref", &result))
		make_access_error("SAM_TcsdirectSteam", "dT_cw_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_d_rec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_rec", &result))
		make_access_error("SAM_TcsdirectSteam", "d_rec");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_d_rh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_rh", &result))
		make_access_error("SAM_TcsdirectSteam", "d_rh");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_d_sh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_sh", &result))
		make_access_error("SAM_TcsdirectSteam", "d_sh");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_d_t_boiler_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_t_boiler", &result))
		make_access_error("SAM_TcsdirectSteam", "d_t_boiler");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_eta_rec_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_rec_pump", &result))
		make_access_error("SAM_TcsdirectSteam", "eta_rec_pump");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_f_mdot_rh_init_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_mdot_rh_init", &result))
		make_access_error("SAM_TcsdirectSteam", "f_mdot_rh_init");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_f_mdotrh_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_mdotrh_des", &result))
		make_access_error("SAM_TcsdirectSteam", "f_mdotrh_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_f_pb_cutoff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_pb_cutoff", &result))
		make_access_error("SAM_TcsdirectSteam", "f_pb_cutoff");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_f_pb_sb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_pb_sb", &result))
		make_access_error("SAM_TcsdirectSteam", "f_pb_sb");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_f_rec_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_rec_min", &result))
		make_access_error("SAM_TcsdirectSteam", "f_rec_min");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_DsgController_ffrac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ffrac", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "ffrac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_flowtype_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flowtype", &result))
		make_access_error("SAM_TcsdirectSteam", "flowtype");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_fossil_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fossil_mode", &result))
		make_access_error("SAM_TcsdirectSteam", "fossil_mode");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_h_boiler_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_boiler", &result))
		make_access_error("SAM_TcsdirectSteam", "h_boiler");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_h_rh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_rh", &result))
		make_access_error("SAM_TcsdirectSteam", "h_rh");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_h_sh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_sh", &result))
		make_access_error("SAM_TcsdirectSteam", "h_sh");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_h_tower_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tower", &result))
		make_access_error("SAM_TcsdirectSteam", "h_tower");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_hl_ffact_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hl_ffact", &result))
		make_access_error("SAM_TcsdirectSteam", "hl_ffact");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_lhv_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lhv_eff", &result))
		make_access_error("SAM_TcsdirectSteam", "lhv_eff");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_mat_boiler_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mat_boiler", &result))
		make_access_error("SAM_TcsdirectSteam", "mat_boiler");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_mat_rh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mat_rh", &result))
		make_access_error("SAM_TcsdirectSteam", "mat_rh");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_mat_sh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mat_sh", &result))
		make_access_error("SAM_TcsdirectSteam", "mat_sh");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_n_panels_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_panels", &result))
		make_access_error("SAM_TcsdirectSteam", "n_panels");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_p_cycle_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "p_cycle_design", &result))
		make_access_error("SAM_TcsdirectSteam", "p_cycle_design");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_q_aux_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_aux_max", &result))
		make_access_error("SAM_TcsdirectSteam", "q_aux_max");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_q_pb_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_pb_design", &result))
		make_access_error("SAM_TcsdirectSteam", "q_pb_design");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_q_rec_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_rec_des", &result))
		make_access_error("SAM_TcsdirectSteam", "q_rec_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_rec_absorptance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_absorptance", &result))
		make_access_error("SAM_TcsdirectSteam", "rec_absorptance");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_rec_emis_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_emis", &result))
		make_access_error("SAM_TcsdirectSteam", "rec_emis");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_rec_qf_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_qf_delay", &result))
		make_access_error("SAM_TcsdirectSteam", "rec_qf_delay");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_rec_su_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_su_delay", &result))
		make_access_error("SAM_TcsdirectSteam", "rec_su_delay");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_t_standby_ini_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_standby_ini", &result))
		make_access_error("SAM_TcsdirectSteam", "t_standby_ini");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_th_rh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "th_rh", &result))
		make_access_error("SAM_TcsdirectSteam", "th_rh");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_th_sh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "th_sh", &result))
		make_access_error("SAM_TcsdirectSteam", "th_sh");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_th_t_boiler_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "th_t_boiler", &result))
		make_access_error("SAM_TcsdirectSteam", "th_t_boiler");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_DsgController_x_b_target_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "x_b_target", &result))
		make_access_error("SAM_TcsdirectSteam", "x_b_target");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Powerblock_F_wc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "F_wc", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "F_wc");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_P_boil_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_boil_des", &result))
		make_access_error("SAM_TcsdirectSteam", "P_boil_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_P_cond_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_min", &result))
		make_access_error("SAM_TcsdirectSteam", "P_cond_min");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_P_cond_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_ratio", &result))
		make_access_error("SAM_TcsdirectSteam", "P_cond_ratio");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_P_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_ref", &result))
		make_access_error("SAM_TcsdirectSteam", "P_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_P_rh_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_rh_ref", &result))
		make_access_error("SAM_TcsdirectSteam", "P_rh_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_T_ITD_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ITD_des", &result))
		make_access_error("SAM_TcsdirectSteam", "T_ITD_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_TcsdirectSteam", "T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_T_cold_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_cold_ref", &result))
		make_access_error("SAM_TcsdirectSteam", "T_cold_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_T_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_hot", &result))
		make_access_error("SAM_TcsdirectSteam", "T_hot");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_T_hot_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_hot_ref", &result))
		make_access_error("SAM_TcsdirectSteam", "T_hot_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_dT_cw_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_cw_ref", &result))
		make_access_error("SAM_TcsdirectSteam", "dT_cw_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_eta_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ref", &result))
		make_access_error("SAM_TcsdirectSteam", "eta_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_n_pl_inc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_pl_inc", &result))
		make_access_error("SAM_TcsdirectSteam", "n_pl_inc");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_pb_bd_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_bd_frac", &result))
		make_access_error("SAM_TcsdirectSteam", "pb_bd_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_q_sby_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_sby_frac", &result))
		make_access_error("SAM_TcsdirectSteam", "q_sby_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_rh_frac_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rh_frac_ref", &result))
		make_access_error("SAM_TcsdirectSteam", "rh_frac_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_startup_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_frac", &result))
		make_access_error("SAM_TcsdirectSteam", "startup_frac");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Powerblock_startup_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_time", &result))
		make_access_error("SAM_TcsdirectSteam", "startup_time");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_Design_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Design_power", &result))
		make_access_error("SAM_TcsdirectSteam", "Design_power");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_Piping_length_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Piping_length", &result))
		make_access_error("SAM_TcsdirectSteam", "Piping_length");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_Piping_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Piping_loss", &result))
		make_access_error("SAM_TcsdirectSteam", "Piping_loss");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_aux_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_par", &result))
		make_access_error("SAM_TcsdirectSteam", "aux_par");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_aux_par_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_par_0", &result))
		make_access_error("SAM_TcsdirectSteam", "aux_par_0");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_aux_par_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_par_1", &result))
		make_access_error("SAM_TcsdirectSteam", "aux_par_1");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_aux_par_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_par_2", &result))
		make_access_error("SAM_TcsdirectSteam", "aux_par_2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_aux_par_f_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "aux_par_f", &result))
		make_access_error("SAM_TcsdirectSteam", "aux_par_f");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_bop_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par", &result))
		make_access_error("SAM_TcsdirectSteam", "bop_par");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_bop_par_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_0", &result))
		make_access_error("SAM_TcsdirectSteam", "bop_par_0");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_bop_par_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_1", &result))
		make_access_error("SAM_TcsdirectSteam", "bop_par_1");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_bop_par_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_2", &result))
		make_access_error("SAM_TcsdirectSteam", "bop_par_2");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_bop_par_f_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_f", &result))
		make_access_error("SAM_TcsdirectSteam", "bop_par_f");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_design_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "design_eff", &result))
		make_access_error("SAM_TcsdirectSteam", "design_eff");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_pb_fixed_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_fixed_par", &result))
		make_access_error("SAM_TcsdirectSteam", "pb_fixed_par");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_piping_length_add_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_length_add", &result))
		make_access_error("SAM_TcsdirectSteam", "piping_length_add");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Parasitics_piping_length_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_length_mult", &result))
		make_access_error("SAM_TcsdirectSteam", "piping_length_mult");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_P_b_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_b_in", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "P_b_in");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_P_b_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_b_out", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "P_b_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_P_cond_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cond", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "P_cond");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_P_cooling_tower_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cooling_tower_tot", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "P_cooling_tower_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_P_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cycle", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "P_cycle");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_P_drop_b_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_drop_b", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "P_drop_b");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_P_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_fixed", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "P_fixed");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_P_out_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_out_net", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "P_out_net");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_P_parasitics_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_parasitics", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "P_parasitics");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_P_piping_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_piping_tot", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "P_piping_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_P_plant_balance_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_plant_balance_tot", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "P_plant_balance_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_P_rh_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_rh_in", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "P_rh_in");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_P_rh_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_rh_out", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "P_rh_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_P_sh_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_sh_out", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "P_sh_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_T_b_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_b_in", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "T_b_in");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_T_boiling_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_boiling", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "T_boiling");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_T_fw_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_fw", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "T_fw");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_T_max_b_surf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_max_b_surf", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "T_max_b_surf");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_T_max_rh_surf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_max_rh_surf", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "T_max_rh_surf");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_T_max_sh_surf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_max_sh_surf", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "T_max_sh_surf");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_T_rh_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rh_in", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "T_rh_in");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_T_rh_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_rh_out", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "T_rh_out");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_W_dot_boost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_boost", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "W_dot_boost");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Outputs_annual_W_cycle_gross_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_W_cycle_gross", &result))
		make_access_error("SAM_TcsdirectSteam", "annual_W_cycle_gross");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_TcsdirectSteam", "annual_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution_time", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "annual_energy_distribution_time");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Outputs_annual_fuel_usage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_fuel_usage", &result))
		make_access_error("SAM_TcsdirectSteam", "annual_fuel_usage");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Outputs_annual_total_water_use_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_total_water_use", &result))
		make_access_error("SAM_TcsdirectSteam", "annual_total_water_use");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "beam");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_TcsdirectSteam", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Outputs_conversion_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conversion_factor", &result))
		make_access_error("SAM_TcsdirectSteam", "conversion_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_dP_rh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dP_rh", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "dP_rh");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_dP_sh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dP_sh", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "dP_sh");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_defocus_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "defocus", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "defocus");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_eta_b_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_b", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "eta_b");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_eta_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_field", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "eta_field");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_eta_rec_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_rec", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "eta_rec");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_eta_rh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_rh", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "eta_rh");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_eta_sh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_sh", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "eta_sh");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_f_bays_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "f_bays", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "f_bays");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_f_mdot_rh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "f_mdot_rh", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "f_mdot_rh");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_hour_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "hour");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_TcsdirectSteam", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_m_dot_aux_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_aux", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "m_dot_aux");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_m_dot_makeup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_makeup", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "m_dot_makeup");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_m_dot_sh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_sh", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "m_dot_sh");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "month");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_pparasi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pparasi", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "pparasi");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_pres_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pres", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "pres");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_abs_rec_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_abs_rec", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_abs_rec");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_aux_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_aux", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_aux");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_aux_fuel_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_aux_fuel", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_aux_fuel");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_b_abs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_b_abs", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_b_abs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_b_conv_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_b_conv", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_b_conv");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_b_rad_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_b_rad", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_b_rad");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_conv_rec_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_conv_rec", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_conv_rec");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_inc_full_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_inc_full", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_inc_full");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_rad_rec_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_rad_rec", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_rad_rec");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_rh_abs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_rh_abs", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_rh_abs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_rh_conv_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_rh_conv", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_rh_conv");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_rh_rad_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_rh_rad", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_rh_rad");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_sh_abs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_sh_abs", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_sh_abs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_sh_conv_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_sh_conv", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_sh_conv");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_sh_rad_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_sh_rad", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_sh_rad");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_q_therm_in_rec_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_therm_in_rec", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "q_therm_in_rec");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_solazi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solazi", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "solazi");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solzen", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "solzen");
	});
	return result;
}



SAM_EXPORT double SAM_TcsdirectSteam_Outputs_system_heat_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_heat_rate", &result))
		make_access_error("SAM_TcsdirectSteam", "system_heat_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "tdry");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_tou_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tou_value", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "tou_value");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "twet");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_v_rh_max_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "v_rh_max", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "v_rh_max");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_v_sh_max_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "v_sh_max", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "v_sh_max");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsdirectSteam_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_TcsdirectSteam", "wspd");
	});
	return result;
}




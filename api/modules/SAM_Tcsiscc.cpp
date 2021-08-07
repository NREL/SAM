#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Tcsiscc.h"

SAM_EXPORT int SAM_Tcsiscc_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("tcsiscc", data, verbosity, err);
}

SAM_EXPORT void SAM_Tcsiscc_Weather_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "solar_resource_file", str);
	});
}

SAM_EXPORT void SAM_Tcsiscc_MoltenSaltTower_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_N_hel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "N_hel", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_bop_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_spec_cost", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_c_atm_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_0", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_c_atm_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_1", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_c_atm_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_2", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_c_atm_3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_3", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_calc_fluxmaps_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calc_fluxmaps", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_cant_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cant_type", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_check_max_flux_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "check_max_flux", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_contingency_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "contingency_rate", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_cost_sf_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cost_sf_fixed", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.fixed", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_per_acre_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.per_acre", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.per_watt", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.epc.percent", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.plm.fixed", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_per_acre_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.plm.per_acre", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.plm.per_watt", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.cost.plm.percent", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_sf_fixed_land_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.sf.fixed_land_area", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_sf_land_overhead_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.sf.land_overhead_factor", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_delta_flux_hrs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "delta_flux_hrs", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_dens_mirror_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dens_mirror", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_dni_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dni_des", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_eta_map_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "eta_map", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_flux_maps_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "flux_maps", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_flux_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "flux_max", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_flux_positions_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "flux_positions", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_focus_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "focus_type", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_fossil_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fossil_spec_cost", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_h_tower_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tower", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_hel_stow_deploy_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hel_stow_deploy", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_helio_active_fraction_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_active_fraction", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_helio_aim_points_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "helio_aim_points", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_helio_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_height", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_helio_optical_error_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_optical_error", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_helio_positions_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "helio_positions", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_helio_reflectance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_reflectance", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_helio_width_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_width", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_heliostat_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heliostat_spec_cost", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_interp_beta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "interp_beta", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_interp_nug_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "interp_nug", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_is_optimize_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_optimize", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_land_bound_list_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "land_bound_list", arr, length);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_land_bound_table_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "land_bound_table", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_land_bound_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_bound_type", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_land_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_max", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_land_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_min", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_land_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_spec_cost", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_n_facet_x_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_facet_x", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_n_facet_y_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_facet_y", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_n_flux_days_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_flux_days", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_n_flux_x_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_flux_x", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_n_flux_y_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_flux_y", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_opt_algorithm_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_algorithm", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_opt_conv_tol_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_conv_tol", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_opt_flux_penalty_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_flux_penalty", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_opt_init_step_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_init_step", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_opt_max_iter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_max_iter", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_p_start_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "p_start", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_p_track_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "p_track", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_plant_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "plant_spec_cost", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_q_design_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_design", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_rec_absorptance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_absorptance", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_rec_aspect_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_aspect", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_rec_cost_exp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_cost_exp", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_rec_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_height", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_rec_hl_perm2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_hl_perm2", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_rec_ref_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_ref_area", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_rec_ref_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_ref_cost", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_run_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "run_type", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_sales_tax_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_frac", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_rate", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_site_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "site_spec_cost", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_tes_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_spec_cost", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_total_installed_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "total_installed_cost", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_tower_exp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tower_exp", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_tower_fixed_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tower_fixed_cost", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Heliostat_v_wind_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "v_wind_max", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_A_sf_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "A_sf", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_D_rec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "D_rec", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_Flow_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Flow_type", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_H_rec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "H_rec", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_N_panels_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "N_panels", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_Q_rec_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Q_rec_des", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_THT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "THT", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_T_htf_cold_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_cold_des", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_hot_des", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_crossover_shift_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "crossover_shift", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_d_tube_out_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "d_tube_out", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_epsilon_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epsilon", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_eta_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pump", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_f_rec_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_rec_min", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "field_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_hl_ffact_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hl_ffact", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_m_dot_htf_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htf_max", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_mat_tube_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mat_tube", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_rec_htf_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_htf", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_rec_qf_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_qf_delay", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_rec_su_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_su_delay", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_receiver_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "receiver_type", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Receiver_th_tube_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "th_tube", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Powerblock_elev_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "elev", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Powerblock_ngcc_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ngcc_model", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Powerblock_pinch_point_coldside_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pinch_point_coldside", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Powerblock_pinch_point_hotside_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pinch_point_hotside", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Powerblock_q_pb_design_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_pb_design", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Parasitics_Q_rec_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Q_rec_des", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Parasitics_W_dot_solar_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "W_dot_solar_des", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Parasitics_bop_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Parasitics_bop_par_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_0", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Parasitics_bop_par_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_1", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Parasitics_bop_par_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_2", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Parasitics_bop_par_f_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_f", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Parasitics_fossil_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fossil_output", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Parasitics_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_fixed_par", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Parasitics_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_pump_coef", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Parasitics_piping_length_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_length", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Parasitics_piping_length_const_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_length_const", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Parasitics_piping_length_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_length_mult", number);
	});
}

SAM_EXPORT void SAM_Tcsiscc_Parasitics_piping_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_loss", number);
	});
}

SAM_EXPORT const char* SAM_Tcsiscc_Weather_solar_resource_file_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "solar_resource_file");
	if (!result)
		make_access_error("SAM_Tcsiscc", "solar_resource_file");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_MoltenSaltTower_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_Tcsiscc", "system_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_N_hel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_hel", &result))
		make_access_error("SAM_Tcsiscc", "N_hel");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_bop_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_spec_cost", &result))
		make_access_error("SAM_Tcsiscc", "bop_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_c_atm_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_0", &result))
		make_access_error("SAM_Tcsiscc", "c_atm_0");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_c_atm_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_1", &result))
		make_access_error("SAM_Tcsiscc", "c_atm_1");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_c_atm_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_2", &result))
		make_access_error("SAM_Tcsiscc", "c_atm_2");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_c_atm_3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_3", &result))
		make_access_error("SAM_Tcsiscc", "c_atm_3");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_calc_fluxmaps_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calc_fluxmaps", &result))
		make_access_error("SAM_Tcsiscc", "calc_fluxmaps");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_cant_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cant_type", &result))
		make_access_error("SAM_Tcsiscc", "cant_type");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_check_max_flux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "check_max_flux", &result))
		make_access_error("SAM_Tcsiscc", "check_max_flux");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_contingency_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_rate", &result))
		make_access_error("SAM_Tcsiscc", "contingency_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_cost_sf_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_sf_fixed", &result))
		make_access_error("SAM_Tcsiscc", "cost_sf_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.fixed", &result))
		make_access_error("SAM_Tcsiscc", "csp.pt.cost.epc.fixed");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_per_acre_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.per_acre", &result))
		make_access_error("SAM_Tcsiscc", "csp.pt.cost.epc.per_acre");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.per_watt", &result))
		make_access_error("SAM_Tcsiscc", "csp.pt.cost.epc.per_watt");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.percent", &result))
		make_access_error("SAM_Tcsiscc", "csp.pt.cost.epc.percent");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.fixed", &result))
		make_access_error("SAM_Tcsiscc", "csp.pt.cost.plm.fixed");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_per_acre_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.per_acre", &result))
		make_access_error("SAM_Tcsiscc", "csp.pt.cost.plm.per_acre");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.per_watt", &result))
		make_access_error("SAM_Tcsiscc", "csp.pt.cost.plm.per_watt");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.percent", &result))
		make_access_error("SAM_Tcsiscc", "csp.pt.cost.plm.percent");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_sf_fixed_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.sf.fixed_land_area", &result))
		make_access_error("SAM_Tcsiscc", "csp.pt.sf.fixed_land_area");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_sf_land_overhead_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.sf.land_overhead_factor", &result))
		make_access_error("SAM_Tcsiscc", "csp.pt.sf.land_overhead_factor");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_delta_flux_hrs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "delta_flux_hrs", &result))
		make_access_error("SAM_Tcsiscc", "delta_flux_hrs");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_dens_mirror_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dens_mirror", &result))
		make_access_error("SAM_Tcsiscc", "dens_mirror");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_dni_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dni_des", &result))
		make_access_error("SAM_Tcsiscc", "dni_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Heliostat_eta_map_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "eta_map", nrows, ncols);
	if (!result)
		make_access_error("SAM_Tcsiscc", "eta_map");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Heliostat_flux_maps_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "flux_maps", nrows, ncols);
	if (!result)
		make_access_error("SAM_Tcsiscc", "flux_maps");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_flux_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flux_max", &result))
		make_access_error("SAM_Tcsiscc", "flux_max");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Heliostat_flux_positions_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "flux_positions", nrows, ncols);
	if (!result)
		make_access_error("SAM_Tcsiscc", "flux_positions");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_focus_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "focus_type", &result))
		make_access_error("SAM_Tcsiscc", "focus_type");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_fossil_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fossil_spec_cost", &result))
		make_access_error("SAM_Tcsiscc", "fossil_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_h_tower_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tower", &result))
		make_access_error("SAM_Tcsiscc", "h_tower");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_hel_stow_deploy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hel_stow_deploy", &result))
		make_access_error("SAM_Tcsiscc", "hel_stow_deploy");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_helio_active_fraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_active_fraction", &result))
		make_access_error("SAM_Tcsiscc", "helio_active_fraction");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Heliostat_helio_aim_points_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "helio_aim_points", nrows, ncols);
	if (!result)
		make_access_error("SAM_Tcsiscc", "helio_aim_points");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_helio_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_height", &result))
		make_access_error("SAM_Tcsiscc", "helio_height");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_helio_optical_error_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_optical_error", &result))
		make_access_error("SAM_Tcsiscc", "helio_optical_error");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Heliostat_helio_positions_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "helio_positions", nrows, ncols);
	if (!result)
		make_access_error("SAM_Tcsiscc", "helio_positions");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_helio_reflectance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_reflectance", &result))
		make_access_error("SAM_Tcsiscc", "helio_reflectance");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_helio_width_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_width", &result))
		make_access_error("SAM_Tcsiscc", "helio_width");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_heliostat_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heliostat_spec_cost", &result))
		make_access_error("SAM_Tcsiscc", "heliostat_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_interp_beta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "interp_beta", &result))
		make_access_error("SAM_Tcsiscc", "interp_beta");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_interp_nug_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "interp_nug", &result))
		make_access_error("SAM_Tcsiscc", "interp_nug");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_is_optimize_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_optimize", &result))
		make_access_error("SAM_Tcsiscc", "is_optimize");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Heliostat_land_bound_list_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "land_bound_list", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "land_bound_list");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Heliostat_land_bound_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "land_bound_table", nrows, ncols);
	if (!result)
		make_access_error("SAM_Tcsiscc", "land_bound_table");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_land_bound_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_bound_type", &result))
		make_access_error("SAM_Tcsiscc", "land_bound_type");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_land_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_max", &result))
		make_access_error("SAM_Tcsiscc", "land_max");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_land_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_min", &result))
		make_access_error("SAM_Tcsiscc", "land_min");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_land_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_spec_cost", &result))
		make_access_error("SAM_Tcsiscc", "land_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_n_facet_x_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_facet_x", &result))
		make_access_error("SAM_Tcsiscc", "n_facet_x");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_n_facet_y_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_facet_y", &result))
		make_access_error("SAM_Tcsiscc", "n_facet_y");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_n_flux_days_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_flux_days", &result))
		make_access_error("SAM_Tcsiscc", "n_flux_days");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_n_flux_x_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_flux_x", &result))
		make_access_error("SAM_Tcsiscc", "n_flux_x");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_n_flux_y_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_flux_y", &result))
		make_access_error("SAM_Tcsiscc", "n_flux_y");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_opt_algorithm_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_algorithm", &result))
		make_access_error("SAM_Tcsiscc", "opt_algorithm");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_opt_conv_tol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_conv_tol", &result))
		make_access_error("SAM_Tcsiscc", "opt_conv_tol");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_opt_flux_penalty_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_flux_penalty", &result))
		make_access_error("SAM_Tcsiscc", "opt_flux_penalty");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_opt_init_step_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_init_step", &result))
		make_access_error("SAM_Tcsiscc", "opt_init_step");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_opt_max_iter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_max_iter", &result))
		make_access_error("SAM_Tcsiscc", "opt_max_iter");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_p_start_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "p_start", &result))
		make_access_error("SAM_Tcsiscc", "p_start");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_p_track_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "p_track", &result))
		make_access_error("SAM_Tcsiscc", "p_track");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_plant_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plant_spec_cost", &result))
		make_access_error("SAM_Tcsiscc", "plant_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_q_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_design", &result))
		make_access_error("SAM_Tcsiscc", "q_design");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_rec_absorptance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_absorptance", &result))
		make_access_error("SAM_Tcsiscc", "rec_absorptance");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_rec_aspect_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_aspect", &result))
		make_access_error("SAM_Tcsiscc", "rec_aspect");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_rec_cost_exp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_cost_exp", &result))
		make_access_error("SAM_Tcsiscc", "rec_cost_exp");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_rec_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_height", &result))
		make_access_error("SAM_Tcsiscc", "rec_height");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_rec_hl_perm2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_hl_perm2", &result))
		make_access_error("SAM_Tcsiscc", "rec_hl_perm2");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_rec_ref_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_ref_area", &result))
		make_access_error("SAM_Tcsiscc", "rec_ref_area");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_rec_ref_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_ref_cost", &result))
		make_access_error("SAM_Tcsiscc", "rec_ref_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_run_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "run_type", &result))
		make_access_error("SAM_Tcsiscc", "run_type");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_sales_tax_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_frac", &result))
		make_access_error("SAM_Tcsiscc", "sales_tax_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_sales_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_rate", &result))
		make_access_error("SAM_Tcsiscc", "sales_tax_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_site_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "site_spec_cost", &result))
		make_access_error("SAM_Tcsiscc", "site_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_tes_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_spec_cost", &result))
		make_access_error("SAM_Tcsiscc", "tes_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_Tcsiscc", "total_installed_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_tower_exp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_exp", &result))
		make_access_error("SAM_Tcsiscc", "tower_exp");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_tower_fixed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_fixed_cost", &result))
		make_access_error("SAM_Tcsiscc", "tower_fixed_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Heliostat_v_wind_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "v_wind_max", &result))
		make_access_error("SAM_Tcsiscc", "v_wind_max");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_A_sf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_sf", &result))
		make_access_error("SAM_Tcsiscc", "A_sf");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_D_rec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "D_rec", &result))
		make_access_error("SAM_Tcsiscc", "D_rec");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_Flow_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Flow_type", &result))
		make_access_error("SAM_Tcsiscc", "Flow_type");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_H_rec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "H_rec", &result))
		make_access_error("SAM_Tcsiscc", "H_rec");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_N_panels_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_panels", &result))
		make_access_error("SAM_Tcsiscc", "N_panels");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_Q_rec_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_rec_des", &result))
		make_access_error("SAM_Tcsiscc", "Q_rec_des");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_THT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "THT", &result))
		make_access_error("SAM_Tcsiscc", "THT");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_cold_des", &result))
		make_access_error("SAM_Tcsiscc", "T_htf_cold_des");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_hot_des", &result))
		make_access_error("SAM_Tcsiscc", "T_htf_hot_des");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_crossover_shift_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "crossover_shift", &result))
		make_access_error("SAM_Tcsiscc", "crossover_shift");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_d_tube_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_tube_out", &result))
		make_access_error("SAM_Tcsiscc", "d_tube_out");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_epsilon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epsilon", &result))
		make_access_error("SAM_Tcsiscc", "epsilon");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_eta_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pump", &result))
		make_access_error("SAM_Tcsiscc", "eta_pump");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_f_rec_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_rec_min", &result))
		make_access_error("SAM_Tcsiscc", "f_rec_min");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Receiver_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "field_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_Tcsiscc", "field_fl_props");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_hl_ffact_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hl_ffact", &result))
		make_access_error("SAM_Tcsiscc", "hl_ffact");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_m_dot_htf_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_max", &result))
		make_access_error("SAM_Tcsiscc", "m_dot_htf_max");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_mat_tube_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mat_tube", &result))
		make_access_error("SAM_Tcsiscc", "mat_tube");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_rec_htf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_htf", &result))
		make_access_error("SAM_Tcsiscc", "rec_htf");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_rec_qf_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_qf_delay", &result))
		make_access_error("SAM_Tcsiscc", "rec_qf_delay");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_rec_su_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_su_delay", &result))
		make_access_error("SAM_Tcsiscc", "rec_su_delay");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_receiver_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "receiver_type", &result))
		make_access_error("SAM_Tcsiscc", "receiver_type");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Receiver_th_tube_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "th_tube", &result))
		make_access_error("SAM_Tcsiscc", "th_tube");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Powerblock_elev_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "elev", &result))
		make_access_error("SAM_Tcsiscc", "elev");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Powerblock_ngcc_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ngcc_model", &result))
		make_access_error("SAM_Tcsiscc", "ngcc_model");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Powerblock_pinch_point_coldside_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pinch_point_coldside", &result))
		make_access_error("SAM_Tcsiscc", "pinch_point_coldside");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Powerblock_pinch_point_hotside_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pinch_point_hotside", &result))
		make_access_error("SAM_Tcsiscc", "pinch_point_hotside");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Powerblock_q_pb_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_pb_design", &result))
		make_access_error("SAM_Tcsiscc", "q_pb_design");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Parasitics_Q_rec_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_rec_des", &result))
		make_access_error("SAM_Tcsiscc", "Q_rec_des");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Parasitics_W_dot_solar_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_solar_des", &result))
		make_access_error("SAM_Tcsiscc", "W_dot_solar_des");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Parasitics_bop_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par", &result))
		make_access_error("SAM_Tcsiscc", "bop_par");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Parasitics_bop_par_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_0", &result))
		make_access_error("SAM_Tcsiscc", "bop_par_0");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Parasitics_bop_par_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_1", &result))
		make_access_error("SAM_Tcsiscc", "bop_par_1");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Parasitics_bop_par_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_2", &result))
		make_access_error("SAM_Tcsiscc", "bop_par_2");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Parasitics_bop_par_f_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_f", &result))
		make_access_error("SAM_Tcsiscc", "bop_par_f");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Parasitics_fossil_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fossil_output", &result))
		make_access_error("SAM_Tcsiscc", "fossil_output");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Parasitics_pb_fixed_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_fixed_par", &result))
		make_access_error("SAM_Tcsiscc", "pb_fixed_par");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Parasitics_pb_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_pump_coef", &result))
		make_access_error("SAM_Tcsiscc", "pb_pump_coef");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Parasitics_piping_length_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_length", &result))
		make_access_error("SAM_Tcsiscc", "piping_length");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Parasitics_piping_length_const_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_length_const", &result))
		make_access_error("SAM_Tcsiscc", "piping_length_const");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Parasitics_piping_length_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_length_mult", &result))
		make_access_error("SAM_Tcsiscc", "piping_length_mult");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Parasitics_piping_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_loss", &result))
		make_access_error("SAM_Tcsiscc", "piping_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_P_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_fixed", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "P_fixed");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_P_plant_balance_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_plant_balance_tot", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "P_plant_balance_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_Q_dot_max_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_dot_max", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "Q_dot_max");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_Q_solar_total_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_solar_total", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "Q_solar_total");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_Q_thermal_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_thermal", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "Q_thermal");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_T_htf_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_htf_cold", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "T_htf_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_T_salt_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_salt_hot", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "T_salt_hot");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_T_st_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_st_cold", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "T_st_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_T_st_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_st_hot", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "T_st_hot");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_W_dot_pc_fossil_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_pc_fossil", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "W_dot_pc_fossil");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_W_dot_pc_hybrid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_pc_hybrid", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "W_dot_pc_hybrid");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_W_dot_plant_fossil_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_plant_fossil", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "W_dot_plant_fossil");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_W_dot_plant_hybrid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_plant_hybrid", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "W_dot_plant_hybrid");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_W_dot_plant_solar_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_plant_solar", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "W_dot_plant_solar");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_W_dot_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_pump", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "W_dot_pump");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_Tcsiscc", "annual_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution_time", nrows, ncols);
	if (!result)
		make_access_error("SAM_Tcsiscc", "annual_energy_distribution_time");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Outputs_annual_fuel_usage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_fuel_usage", &result))
		make_access_error("SAM_Tcsiscc", "annual_fuel_usage");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "beam");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_Tcsiscc", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_eta_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_field", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "eta_field");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_eta_fuel_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_fuel", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "eta_fuel");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_eta_solar_use_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_solar_use", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "eta_solar_use");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_eta_therm_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_therm", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "eta_therm");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_f_timestep_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "f_timestep", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "f_timestep");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_field_eff_adj_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "field_eff_adj", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "field_eff_adj");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_fuel_use_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuel_use", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "fuel_use");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_hour_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "hour");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_Tcsiscc", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_m_dot_salt_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_salt_tot", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "m_dot_salt_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_m_dot_ss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_ss", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "m_dot_ss");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_m_dot_steam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_steam", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "m_dot_steam");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "month");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_pparasi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pparasi", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "pparasi");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_pres_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pres", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "pres");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_q_conv_sum_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_conv_sum", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "q_conv_sum");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_q_rad_sum_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_rad_sum", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "q_rad_sum");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_q_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_startup", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "q_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_solar_fraction_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solar_fraction", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "solar_fraction");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_solazi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solazi", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "solazi");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solzen", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "solzen");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsiscc_Outputs_system_heat_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_heat_rate", &result))
		make_access_error("SAM_Tcsiscc", "system_heat_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "tdry");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "twet");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsiscc_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_Tcsiscc", "wspd");
	});
	return result;
}




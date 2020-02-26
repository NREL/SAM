#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Solarpilot.h"

SAM_EXPORT SAM_Solarpilot SAM_Solarpilot_construct(const char* def, SAM_error* err){
	SAM_Solarpilot result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_Solarpilot_execute(SAM_Solarpilot data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("solarpilot", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Solarpilot_destruct(SAM_Solarpilot system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_c_atm_0_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_0", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_c_atm_1_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_1", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_c_atm_2_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_2", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_c_atm_3_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_atm_3", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_calc_fluxmaps_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calc_fluxmaps", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_cant_type_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cant_type", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_check_max_flux_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "check_max_flux", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_contingency_rate_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "contingency_rate", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_cost_sf_fixed_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cost_sf_fixed", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_delta_flux_hrs_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "delta_flux_hrs", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_dens_mirror_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dens_mirror", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_dni_des_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dni_des", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_flux_max_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "flux_max", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_focus_type_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "focus_type", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_h_tower_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tower", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_helio_active_fraction_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_active_fraction", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_helio_height_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_height", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_helio_optical_error_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_optical_error", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_helio_positions_in_mset(SAM_Solarpilot ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "helio_positions_in", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_helio_reflectance_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_reflectance", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_helio_width_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "helio_width", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_heliostat_spec_cost_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heliostat_spec_cost", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_is_optimize_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_optimize", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_land_max_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_max", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_land_min_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_min", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_land_spec_cost_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_spec_cost", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_n_facet_x_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_facet_x", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_n_facet_y_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_facet_y", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_n_flux_days_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_flux_days", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_n_flux_x_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_flux_x", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_n_flux_y_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_flux_y", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_opt_algorithm_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_algorithm", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_opt_conv_tol_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_conv_tol", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_opt_flux_penalty_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_flux_penalty", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_opt_init_step_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_init_step", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_opt_max_iter_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "opt_max_iter", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_q_design_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_design", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_rec_absorptance_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_absorptance", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_rec_aspect_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_aspect", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_rec_cost_exp_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_cost_exp", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_rec_height_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_height", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_rec_hl_perm2_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_hl_perm2", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_rec_ref_area_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_ref_area", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_rec_ref_cost_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_ref_cost", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_sales_tax_frac_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_frac", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_sales_tax_rate_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_rate", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_site_spec_cost_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "site_spec_cost", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_solar_resource_file_sset(SAM_Solarpilot ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "solar_resource_file", str);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_tower_exp_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tower_exp", number);
	});
}

SAM_EXPORT void SAM_Solarpilot_SolarPILOT_tower_fixed_cost_nset(SAM_Solarpilot ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tower_fixed_cost", number);
	});
}

SAM_EXPORT double SAM_Solarpilot_SolarPILOT_c_atm_0_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_0", &result))
		make_access_error("SAM_Solarpilot", "c_atm_0");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_c_atm_1_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_1", &result))
		make_access_error("SAM_Solarpilot", "c_atm_1");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_c_atm_2_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_2", &result))
		make_access_error("SAM_Solarpilot", "c_atm_2");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_c_atm_3_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_atm_3", &result))
		make_access_error("SAM_Solarpilot", "c_atm_3");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_calc_fluxmaps_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calc_fluxmaps", &result))
		make_access_error("SAM_Solarpilot", "calc_fluxmaps");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_cant_type_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cant_type", &result))
		make_access_error("SAM_Solarpilot", "cant_type");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_check_max_flux_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "check_max_flux", &result))
		make_access_error("SAM_Solarpilot", "check_max_flux");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_contingency_rate_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_rate", &result))
		make_access_error("SAM_Solarpilot", "contingency_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_cost_sf_fixed_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_sf_fixed", &result))
		make_access_error("SAM_Solarpilot", "cost_sf_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_delta_flux_hrs_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "delta_flux_hrs", &result))
		make_access_error("SAM_Solarpilot", "delta_flux_hrs");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_dens_mirror_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dens_mirror", &result))
		make_access_error("SAM_Solarpilot", "dens_mirror");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_dni_des_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dni_des", &result))
		make_access_error("SAM_Solarpilot", "dni_des");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_flux_max_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flux_max", &result))
		make_access_error("SAM_Solarpilot", "flux_max");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_focus_type_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "focus_type", &result))
		make_access_error("SAM_Solarpilot", "focus_type");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_h_tower_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tower", &result))
		make_access_error("SAM_Solarpilot", "h_tower");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_helio_active_fraction_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_active_fraction", &result))
		make_access_error("SAM_Solarpilot", "helio_active_fraction");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_helio_height_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_height", &result))
		make_access_error("SAM_Solarpilot", "helio_height");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_helio_optical_error_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_optical_error", &result))
		make_access_error("SAM_Solarpilot", "helio_optical_error");
	});
	return result;
}



SAM_EXPORT double* SAM_Solarpilot_SolarPILOT_helio_positions_in_mget(SAM_Solarpilot ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "helio_positions_in", nrows, ncols);
	if (!result)
		make_access_error("SAM_Solarpilot", "helio_positions_in");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_helio_reflectance_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_reflectance", &result))
		make_access_error("SAM_Solarpilot", "helio_reflectance");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_helio_width_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "helio_width", &result))
		make_access_error("SAM_Solarpilot", "helio_width");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_heliostat_spec_cost_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heliostat_spec_cost", &result))
		make_access_error("SAM_Solarpilot", "heliostat_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_is_optimize_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_optimize", &result))
		make_access_error("SAM_Solarpilot", "is_optimize");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_land_max_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_max", &result))
		make_access_error("SAM_Solarpilot", "land_max");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_land_min_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_min", &result))
		make_access_error("SAM_Solarpilot", "land_min");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_land_spec_cost_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_spec_cost", &result))
		make_access_error("SAM_Solarpilot", "land_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_n_facet_x_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_facet_x", &result))
		make_access_error("SAM_Solarpilot", "n_facet_x");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_n_facet_y_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_facet_y", &result))
		make_access_error("SAM_Solarpilot", "n_facet_y");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_n_flux_days_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_flux_days", &result))
		make_access_error("SAM_Solarpilot", "n_flux_days");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_n_flux_x_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_flux_x", &result))
		make_access_error("SAM_Solarpilot", "n_flux_x");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_n_flux_y_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_flux_y", &result))
		make_access_error("SAM_Solarpilot", "n_flux_y");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_opt_algorithm_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_algorithm", &result))
		make_access_error("SAM_Solarpilot", "opt_algorithm");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_opt_conv_tol_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_conv_tol", &result))
		make_access_error("SAM_Solarpilot", "opt_conv_tol");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_opt_flux_penalty_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_flux_penalty", &result))
		make_access_error("SAM_Solarpilot", "opt_flux_penalty");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_opt_init_step_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_init_step", &result))
		make_access_error("SAM_Solarpilot", "opt_init_step");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_opt_max_iter_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "opt_max_iter", &result))
		make_access_error("SAM_Solarpilot", "opt_max_iter");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_q_design_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_design", &result))
		make_access_error("SAM_Solarpilot", "q_design");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_rec_absorptance_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_absorptance", &result))
		make_access_error("SAM_Solarpilot", "rec_absorptance");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_rec_aspect_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_aspect", &result))
		make_access_error("SAM_Solarpilot", "rec_aspect");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_rec_cost_exp_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_cost_exp", &result))
		make_access_error("SAM_Solarpilot", "rec_cost_exp");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_rec_height_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_height", &result))
		make_access_error("SAM_Solarpilot", "rec_height");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_rec_hl_perm2_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_hl_perm2", &result))
		make_access_error("SAM_Solarpilot", "rec_hl_perm2");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_rec_ref_area_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_ref_area", &result))
		make_access_error("SAM_Solarpilot", "rec_ref_area");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_rec_ref_cost_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_ref_cost", &result))
		make_access_error("SAM_Solarpilot", "rec_ref_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_sales_tax_frac_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_frac", &result))
		make_access_error("SAM_Solarpilot", "sales_tax_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_sales_tax_rate_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_rate", &result))
		make_access_error("SAM_Solarpilot", "sales_tax_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_site_spec_cost_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "site_spec_cost", &result))
		make_access_error("SAM_Solarpilot", "site_spec_cost");
	});
	return result;
}



SAM_EXPORT const char* SAM_Solarpilot_SolarPILOT_solar_resource_file_sget(SAM_Solarpilot ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "solar_resource_file");
	if (!result)
		make_access_error("SAM_Solarpilot", "solar_resource_file");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_tower_exp_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_exp", &result))
		make_access_error("SAM_Solarpilot", "tower_exp");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_SolarPILOT_tower_fixed_cost_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_fixed_cost", &result))
		make_access_error("SAM_Solarpilot", "tower_fixed_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_Outputs_area_sf_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "area_sf", &result))
		make_access_error("SAM_Solarpilot", "area_sf");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_Outputs_base_land_area_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "base_land_area", &result))
		make_access_error("SAM_Solarpilot", "base_land_area");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_Outputs_cost_land_tot_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_land_tot", &result))
		make_access_error("SAM_Solarpilot", "cost_land_tot");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_Outputs_cost_rec_tot_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_rec_tot", &result))
		make_access_error("SAM_Solarpilot", "cost_rec_tot");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_Outputs_cost_sf_tot_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_sf_tot", &result))
		make_access_error("SAM_Solarpilot", "cost_sf_tot");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_Outputs_cost_site_tot_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_site_tot", &result))
		make_access_error("SAM_Solarpilot", "cost_site_tot");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_Outputs_cost_tower_tot_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_tower_tot", &result))
		make_access_error("SAM_Solarpilot", "cost_tower_tot");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_Outputs_flux_max_observed_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flux_max_observed", &result))
		make_access_error("SAM_Solarpilot", "flux_max_observed");
	});
	return result;
}



SAM_EXPORT double* SAM_Solarpilot_Outputs_flux_table_mget(SAM_Solarpilot ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "flux_table", nrows, ncols);
	if (!result)
		make_access_error("SAM_Solarpilot", "flux_table");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_Outputs_h_tower_opt_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tower_opt", &result))
		make_access_error("SAM_Solarpilot", "h_tower_opt");
	});
	return result;
}



SAM_EXPORT double* SAM_Solarpilot_Outputs_heliostat_positions_mget(SAM_Solarpilot ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "heliostat_positions", nrows, ncols);
	if (!result)
		make_access_error("SAM_Solarpilot", "heliostat_positions");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_Outputs_land_area_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_area", &result))
		make_access_error("SAM_Solarpilot", "land_area");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_Outputs_number_heliostats_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "number_heliostats", &result))
		make_access_error("SAM_Solarpilot", "number_heliostats");
	});
	return result;
}



SAM_EXPORT double* SAM_Solarpilot_Outputs_opteff_table_mget(SAM_Solarpilot ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "opteff_table", nrows, ncols);
	if (!result)
		make_access_error("SAM_Solarpilot", "opteff_table");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_Outputs_rec_aspect_opt_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_aspect_opt", &result))
		make_access_error("SAM_Solarpilot", "rec_aspect_opt");
	});
	return result;
}



SAM_EXPORT double SAM_Solarpilot_Outputs_rec_height_opt_nget(SAM_Solarpilot ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_height_opt", &result))
		make_access_error("SAM_Solarpilot", "rec_height_opt");
	});
	return result;
}




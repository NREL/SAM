#ifndef SAM_SOLARPILOT_H_
#define SAM_SOLARPILOT_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Solarpilot Technology Model
	//

	/** 
	 * Create a Solarpilot variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Solarpilot;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Solarpilot_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SolarPILOT parameters
	//

	/**
	 * Set c_atm_0: Attenuation coefficient 0
	 * options: None
	 * constraints: None
	 * required if: ?=0.006789
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_c_atm_0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set c_atm_1: Attenuation coefficient 1
	 * options: None
	 * constraints: None
	 * required if: ?=0.1046
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_c_atm_1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set c_atm_2: Attenuation coefficient 2
	 * options: None
	 * constraints: None
	 * required if: ?=-0.0107
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_c_atm_2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set c_atm_3: Attenuation coefficient 3
	 * options: None
	 * constraints: None
	 * required if: ?=0.002845
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_c_atm_3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set calc_fluxmaps: Include fluxmap calculations
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_calc_fluxmaps_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cant_type: Heliostat cant method
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_cant_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cav_rec_height: Cavity receiver height [m]
	 * options: None
	 * constraints: None
	 * required if: receiver_type=1
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_cav_rec_height_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cav_rec_span: Cavity receiver span angle [deg]
	 * options: None
	 * constraints: None
	 * required if: receiver_type=1
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_cav_rec_span_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cav_rec_width: Cavity receiver width [m]
	 * options: None
	 * constraints: None
	 * required if: receiver_type=1
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_cav_rec_width_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set check_max_flux: Check max flux at design point
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_check_max_flux_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set contingency_rate: Contingency for cost overrun [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_contingency_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cost_sf_fixed: Soalr field fixed cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_cost_sf_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set delta_flux_hrs: Hourly frequency in flux map lookup
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_delta_flux_hrs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dens_mirror: Ratio of reflective area to profile [frac]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_dens_mirror_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dni_des: Design-point DNI [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_dni_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set flux_max: Maximum allowable flux
	 * options: None
	 * constraints: None
	 * required if: ?=1000
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_flux_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set focus_type: Heliostat focus method
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_focus_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tower: Tower height [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_h_tower_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set helio_active_fraction: Active fraction of reflective area [frac]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_helio_active_fraction_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set helio_height: Heliostat height [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_helio_height_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set helio_optical_error: Optical error [rad]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_helio_optical_error_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set helio_positions_in: Heliostat position table
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_helio_positions_in_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set helio_reflectance: Mirror reflectance [frac]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_helio_reflectance_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set helio_width: Heliostat width [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_helio_width_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set heliostat_spec_cost: Heliostat field cost [$/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_heliostat_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_optimize: Do SolarPILOT optimization
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_is_optimize_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_max: Max heliostat-dist-to-tower-height ratio
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_land_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_min: Min heliostat-dist-to-tower-height ratio
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_land_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_spec_cost: Total land area cost [$/acre]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_land_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_cav_rec_panels: Cavity receiver number of panels
	 * options: None
	 * constraints: None
	 * required if: receiver_type=1
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_n_cav_rec_panels_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_facet_x: Number of heliostat facets - X
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_n_facet_x_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_facet_y: Number of heliostat facets - Y
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_n_facet_y_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_flux_days: No. days in flux map lookup
	 * options: None
	 * constraints: None
	 * required if: ?=8
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_n_flux_days_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_flux_x: Flux map X resolution
	 * options: None
	 * constraints: None
	 * required if: ?=12
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_n_flux_x_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_flux_y: Flux map Y resolution
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_n_flux_y_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set opt_algorithm: Optimization algorithm
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_opt_algorithm_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set opt_conv_tol: Optimization convergence tol
	 * options: None
	 * constraints: None
	 * required if: ?=0.001
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_opt_conv_tol_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set opt_flux_penalty: Optimization flux overage penalty
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_opt_flux_penalty_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set opt_init_step: Optimization initial step size
	 * options: None
	 * constraints: None
	 * required if: ?=0.05
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_opt_init_step_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set opt_max_iter: Max. number iteration steps
	 * options: None
	 * constraints: None
	 * required if: ?=200
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_opt_max_iter_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_design: Receiver thermal design power [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_q_design_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_absorptance: Absorptance [frac]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_rec_absorptance_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_aspect: External receiver aspect ratio (H/W) [frac]
	 * options: None
	 * constraints: None
	 * required if: receiver_type=0
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_rec_aspect_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_cost_exp: Receiver cost scaling exponent
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_rec_cost_exp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_height: External receiver height [m]
	 * options: None
	 * constraints: None
	 * required if: receiver_type=0
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_rec_height_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_hl_perm2: Receiver design heat loss [kW/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_rec_hl_perm2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_ref_area: Receiver reference area for cost scale
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_rec_ref_area_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_ref_cost: Receiver reference cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_rec_ref_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set receiver_type: 0: external (default), 1; cavity
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_receiver_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sales_tax_frac: Percent of cost to which sales tax applies [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_sales_tax_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sales_tax_rate: Sales tax rate [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set site_spec_cost: Site improvement cost [$/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_site_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set solar_resource_file: Solar weather data file
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set tower_exp: Tower cost scaling exponent
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_tower_exp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tower_fixed_cost: Tower fixed cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Solarpilot_SolarPILOT_tower_fixed_cost_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * SolarPILOT Getters
	 */

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_c_atm_0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_c_atm_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_c_atm_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_c_atm_3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_calc_fluxmaps_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_cant_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_cav_rec_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_cav_rec_span_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_cav_rec_width_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_check_max_flux_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_contingency_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_cost_sf_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_delta_flux_hrs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_dens_mirror_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_dni_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_flux_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_focus_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_h_tower_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_helio_active_fraction_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_helio_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_helio_optical_error_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Solarpilot_SolarPILOT_helio_positions_in_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_helio_reflectance_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_helio_width_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_heliostat_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_is_optimize_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_land_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_land_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_land_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_n_cav_rec_panels_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_n_facet_x_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_n_facet_y_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_n_flux_days_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_n_flux_x_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_n_flux_y_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_opt_algorithm_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_opt_conv_tol_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_opt_flux_penalty_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_opt_init_step_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_opt_max_iter_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_q_design_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_rec_absorptance_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_rec_aspect_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_rec_cost_exp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_rec_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_rec_hl_perm2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_rec_ref_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_rec_ref_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_receiver_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_sales_tax_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_sales_tax_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_site_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Solarpilot_SolarPILOT_solar_resource_file_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_tower_exp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_SolarPILOT_tower_fixed_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Solarpilot_Outputs_area_sf_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_Outputs_base_land_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_Outputs_cav_rec_aper_width_opt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_Outputs_cost_land_tot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_Outputs_cost_rec_tot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_Outputs_cost_sf_tot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_Outputs_cost_site_tot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_Outputs_cost_tower_tot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_Outputs_flux_max_observed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Solarpilot_Outputs_flux_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_Outputs_h_tower_opt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Solarpilot_Outputs_heliostat_positions_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_Outputs_land_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_Outputs_number_heliostats_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Solarpilot_Outputs_opteff_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_Outputs_rec_aspect_opt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Solarpilot_Outputs_rec_height_opt_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
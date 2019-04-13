#ifndef SAM_TCSDIRECTSTEAM_H_
#define SAM_TCSDIRECTSTEAM_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// TcsdirectSteam Technology Model
	//

	/** 
	 * Create a TcsdirectSteam variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_TcsdirectSteam;

	SAM_EXPORT SAM_TcsdirectSteam SAM_TcsdirectSteam_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_TcsdirectSteam_execute(SAM_TcsdirectSteam data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_TcsdirectSteam_destruct(SAM_TcsdirectSteam system);


	//
	// Weather parameters
	//

	/**
	 * Set solar_resource_file: local weather file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Weather_solar_resource_file_sset(SAM_TcsdirectSteam ptr, const char* str, SAM_error *err);


	//
	// DirectSteamTower parameters
	//

	/**
	 * Set system_capacity: Nameplate capacity [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DirectSteamTower_system_capacity_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);


	//
	// TouTranslator parameters
	//

	/**
	 * Set weekday_schedule: 12x24 Time of Use Values for week days
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_TouTranslator_weekday_schedule_mset(SAM_TcsdirectSteam ptr, float* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set weekend_schedule: 12x24 Time of Use Values for week end days
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_TouTranslator_weekend_schedule_mset(SAM_TcsdirectSteam ptr, float* mat, int nrows, int ncols, SAM_error *err);


	//
	// Heliostat parameters
	//

	/**
	 * Set N_hel: Number of heliostats
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_N_hel_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set bop_spec_cost: BOS specific cost [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_bop_spec_cost_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set c_atm_0: Attenuation coefficient 0
	 * options: None
	 * constraints: None
	 * required if: ?=0.006789
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_c_atm_0_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set c_atm_1: Attenuation coefficient 1
	 * options: None
	 * constraints: None
	 * required if: ?=0.1046
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_c_atm_1_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set c_atm_2: Attenuation coefficient 2
	 * options: None
	 * constraints: None
	 * required if: ?=-0.0107
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_c_atm_2_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set c_atm_3: Attenuation coefficient 3
	 * options: None
	 * constraints: None
	 * required if: ?=0.002845
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_c_atm_3_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set calc_fluxmaps: Include fluxmap calculations
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_calc_fluxmaps_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set cant_type: Heliostat cant method
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_cant_type_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set check_max_flux: Check max flux at design point
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_check_max_flux_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set contingency_rate: Contingency for cost overrun [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_contingency_rate_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set cost_sf_fixed: Solar field fixed cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_cost_sf_fixed_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set csp.pt.cost.epc.fixed: EPC fixed [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_fixed_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set csp.pt.cost.epc.per_acre: EPC cost per acre [$/acre]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_per_acre_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set csp.pt.cost.epc.per_watt: EPC cost per watt [$/W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_per_watt_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set csp.pt.cost.epc.percent: EPC cost percent of direct
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_percent_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set csp.pt.cost.plm.fixed: PLM fixed [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_fixed_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set csp.pt.cost.plm.per_acre: PLM cost per acre [$/acre]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_per_acre_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set csp.pt.cost.plm.per_watt: PLM cost per watt [$/W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_per_watt_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set csp.pt.cost.plm.percent: PLM cost percent of direct
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_percent_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set csp.pt.sf.fixed_land_area: Fixed land area [acre]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_sf_fixed_land_area_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set csp.pt.sf.land_overhead_factor: Land overhead factor
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_csp_pt_sf_land_overhead_factor_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set delta_flux_hrs: Hourly frequency in flux map lookup
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_delta_flux_hrs_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set dens_mirror: Ratio of reflective area to profile
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_dens_mirror_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set dni_des: Design-point DNI [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_dni_des_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set eta_map: Field efficiency array
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_eta_map_mset(SAM_TcsdirectSteam ptr, float* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set flux_maps: Flux map intensities
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_flux_maps_mset(SAM_TcsdirectSteam ptr, float* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set flux_max: Maximum allowable flux
	 * options: None
	 * constraints: None
	 * required if: ?=1000
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_flux_max_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set flux_positions: Flux map sun positions [deg]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_flux_positions_mset(SAM_TcsdirectSteam ptr, float* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set focus_type: Heliostat focus method
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_focus_type_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set fossil_spec_cost: Fossil system specific cost [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_fossil_spec_cost_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set hel_stow_deploy: Stow/deploy elevation [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_hel_stow_deploy_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set helio_active_fraction: Heliostat active frac.
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_helio_active_fraction_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set helio_aim_points: Heliostat aim point table [m]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_helio_aim_points_mset(SAM_TcsdirectSteam ptr, float* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set helio_height: Heliostat height [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_helio_height_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set helio_optical_error: Heliostat optical error [rad]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_helio_optical_error_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set helio_positions: Heliostat position table [m]
	 * options: None
	 * constraints: None
	 * required if: run_type=1
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_helio_positions_mset(SAM_TcsdirectSteam ptr, float* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set helio_reflectance: Heliostat reflectance
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_helio_reflectance_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set helio_width: Heliostat width [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_helio_width_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set heliostat_spec_cost: Heliostat field cost [$/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_heliostat_spec_cost_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set interp_beta: Interpolation beta coef.
	 * options: None
	 * constraints: None
	 * required if: ?=1.99
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_interp_beta_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set interp_nug: Interpolation nugget
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_interp_nug_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set is_optimize: Do SolarPILOT optimization
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_is_optimize_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set land_bound_list: Boundary table listing
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_land_bound_list_aset(SAM_TcsdirectSteam ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set land_bound_table: Land boundary table [m]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_land_bound_table_mset(SAM_TcsdirectSteam ptr, float* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set land_bound_type: Land boundary type
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_land_bound_type_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set land_max: Land max boundary [-ORm]
	 * options: None
	 * constraints: None
	 * required if: ?=7.5
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_land_max_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set land_min: Land min boundary [-ORm]
	 * options: None
	 * constraints: None
	 * required if: ?=0.75
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_land_min_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set land_spec_cost: Total land area cost [$/acre]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_land_spec_cost_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set n_facet_x: Number of heliostat facets - X
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_n_facet_x_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set n_facet_y: Number of heliostat facets - Y
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_n_facet_y_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set n_flux_days: No. days in flux map lookup
	 * options: None
	 * constraints: None
	 * required if: ?=8
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_n_flux_days_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set n_flux_x: Flux map X resolution
	 * options: None
	 * constraints: None
	 * required if: ?=12
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_n_flux_x_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set n_flux_y: Flux map Y resolution
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_n_flux_y_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set opt_algorithm: Optimization algorithm
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_opt_algorithm_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set opt_conv_tol: Optimization convergence tol
	 * options: None
	 * constraints: None
	 * required if: ?=0.001
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_opt_conv_tol_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set opt_flux_penalty: Optimization flux overage penalty
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_opt_flux_penalty_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set opt_init_step: Optimization initial step size
	 * options: None
	 * constraints: None
	 * required if: ?=0.05
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_opt_init_step_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set opt_max_iter: Max. number iteration steps
	 * options: None
	 * constraints: None
	 * required if: ?=200
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_opt_max_iter_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set p_start: Heliostat startup energy [kWe-hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_p_start_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set p_track: Heliostat tracking energy [kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_p_track_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set plant_spec_cost: Power cycle specific cost [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_plant_spec_cost_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set q_design: Receiver thermal design power [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_q_design_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set rec_absorptance: Receiver absorptance
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_rec_absorptance_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set rec_aspect: Receiver aspect ratio
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_rec_aspect_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set rec_cost_exp: Receiver cost scaling exponent
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_rec_cost_exp_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set rec_height: Receiver height [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_rec_height_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set rec_hl_perm2: Receiver design heatloss [kW/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_rec_hl_perm2_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set rec_ref_area: Receiver reference area for cost scale
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_rec_ref_area_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set rec_ref_cost: Receiver reference cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_rec_ref_cost_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set run_type: Run type [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_run_type_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set sales_tax_frac: Percent of cost to which sales tax applies [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_sales_tax_frac_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set sales_tax_rate: Sales tax rate [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_sales_tax_rate_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set site_spec_cost: Site improvement cost [$/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_site_spec_cost_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set tes_spec_cost: Thermal energy storage cost [$/kWht]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_tes_spec_cost_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set total_installed_cost: Total installed cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_total_installed_cost_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set tower_exp: Tower cost scaling exponent
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_tower_exp_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set tower_fixed_cost: Tower fixed cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_tower_fixed_cost_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set v_wind_max: Max. wind velocity [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_v_wind_max_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set washing_frequency: Mirror washing frequency
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_washing_frequency_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set water_usage_per_wash: Water usage per wash [L/m2_aper]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Heliostat_water_usage_per_wash_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);


	//
	// Receiver parameters
	//

	/**
	 * Set H_rec: The height of the receiver [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Receiver_H_rec_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set THT: The height of the tower (hel. pivot to rec equator) [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Receiver_THT_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);


	//
	// DsgController parameters
	//

	/**
	 * Set A_sf: Solar field area [m^2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_A_sf_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set P_b_in_init: Initial Boiler inlet pressure [bar]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_P_b_in_init_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set P_cond_init: Condenser pressure [Pa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_P_cond_init_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set P_hp_in_des: Design HP Turbine Inlet Pressure [bar]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_P_hp_in_des_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set P_hp_out: HP turbine outlet pressure [bar]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_P_hp_out_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set P_hp_out_des: Design HP Turbine Outlet Pressure [bar]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_P_hp_out_des_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set T_ITD_des: Approach temperature for dry cooling [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_ITD_des_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set T_amb_des: Design ambient temperature (power cycle) [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_amb_des_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set T_approach: Approach temperature for wet cooling [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_approach_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set T_fw_init: Initial Feedwater outlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_fw_init_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set T_hp_out: HP turbine outlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_hp_out_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set T_rh_out_des: Target reheater outlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_rh_out_des_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set T_rh_target: Target reheater outlet temp. [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_rh_target_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set T_sh_out_des: Target superheater outlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_T_sh_out_des_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set ct: Cooling Type [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_ct_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set cycle_max_frac: Cycle maximum overdesign fraction [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_cycle_max_frac_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set dT_cw_ref: Reference condenser water dT [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_dT_cw_ref_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set d_rec: Diameter of Receiver [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_d_rec_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set d_rh: O.D. of reheater tubes [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_d_rh_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set d_sh: O.D. of superheater tubes [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_d_sh_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set d_t_boiler: O.D. of boiler tubes [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_d_t_boiler_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set eta_rec_pump: Feedwater pump efficiency [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_eta_rec_pump_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set f_mdot_rh_init: Reheat mass flow rate fraction [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_f_mdot_rh_init_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set f_mdotrh_des: Design reheat mass flow rate fraction [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_f_mdotrh_des_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set f_pb_cutoff: Cycle cut-off fraction [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_f_pb_cutoff_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set f_pb_sb: Cycle minimum standby fraction [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_f_pb_sb_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set f_rec_min: Minimum receiver absorbed power fraction [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_f_rec_min_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set ffrac: Fossil dispatch logic [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_ffrac_aset(SAM_TcsdirectSteam ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set flowtype: Code for flow pattern through rec. [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_flowtype_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set fossil_mode: Fossil model: 1=Normal, 2=Supplemental [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_fossil_mode_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set h_boiler: Height of boiler [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_h_boiler_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set h_rh: Height of reheater [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_h_rh_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set h_sh: Height of superheater [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_h_sh_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set h_tower: Tower Height [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_h_tower_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set hl_ffact: Heat Loss Fudge FACTor [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_hl_ffact_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set lhv_eff: Aux Heater lower heating value efficiency [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_lhv_eff_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set mat_boiler: Numerical code for tube material [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_mat_boiler_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set mat_rh: Numerical code for reheater material [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_mat_rh_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set mat_sh: Numerical code for superheater material [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_mat_sh_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set n_panels: Number of panels [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_n_panels_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set p_cycle_design: Design Cycle Power [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_p_cycle_design_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set q_aux_max: Maximum heat rate of auxiliary heater [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_q_aux_max_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set q_pb_design: Heat rate into powerblock at design [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_q_pb_design_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set q_rec_des: Design-point thermal power [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_q_rec_des_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set rec_absorptance: Absorptance of receiver tubes [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_rec_absorptance_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set rec_emis: Emissivity of receiver tubes [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_rec_emis_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set rec_qf_delay: Receiver start-up delay fraction of thermal energy of receiver running at design for 1 hour [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_rec_qf_delay_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set rec_su_delay: Receiver start-up delay time [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_rec_su_delay_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set t_standby_ini: Power block standby time [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_t_standby_ini_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set th_rh: Thickness of reheater tubes [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_th_rh_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set th_sh: Thickness of superheater tubes [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_th_sh_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set th_t_boiler: Thickness of boiler tubes [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_th_t_boiler_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set x_b_target: Target boiler outlet quality [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_DsgController_x_b_target_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);


	//
	// Powerblock parameters
	//

	/**
	 * Set F_wc: Fraction indicating wet cooling use for hybrid system [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_F_wc_aset(SAM_TcsdirectSteam ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set P_boil_des: Boiler operating pressure @ design [bar]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_P_boil_des_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set P_cond_min: Minimum condenser pressure [inHg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_P_cond_min_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set P_cond_ratio: Condenser pressure ratio [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_P_cond_ratio_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set P_ref: Reference output electric power at design condition [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_P_ref_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set P_rh_ref: Reheater operating pressure at design [bar]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_P_rh_ref_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set T_ITD_des: ITD at design for dry system [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_T_ITD_des_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set T_amb_des: Reference ambient temperature at design point [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_T_amb_des_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set T_cold_ref: Reference HTF outlet temperature at design [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_T_cold_ref_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set T_hot: Hot HTF inlet temperature, from storage tank [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_T_hot_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set T_hot_ref: Reference HTF inlet temperature at design [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_T_hot_ref_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set dT_cw_ref: Reference condenser cooling water inlet/outlet T diff [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_dT_cw_ref_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set eta_ref: Reference conversion efficiency at design condition [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_eta_ref_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set n_pl_inc: Number of part-load increments for the heat rejection system [none]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_n_pl_inc_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set pb_bd_frac: Power block blowdown steam fraction  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_pb_bd_frac_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set q_sby_frac: Fraction of thermal power required for standby mode [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_q_sby_frac_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set rh_frac_ref: Reheater flow fraction at design [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_rh_frac_ref_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set startup_frac: Fraction of design thermal power needed for startup [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_startup_frac_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set startup_time: Time needed for power block startup [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Powerblock_startup_time_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);


	//
	// Parasitics parameters
	//

	/**
	 * Set Design_power: Power production at design conditions [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_Design_power_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set Piping_length: Total length of exposed piping [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_Piping_length_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set Piping_loss: Thermal loss per meter of piping [Wt/m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_Piping_loss_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set aux_par: Aux heater, boiler parasitic [MWe/MWcap]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_aux_par_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set aux_par_0: Aux heater, boiler parasitic - constant coefficient [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_aux_par_0_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set aux_par_1: Aux heater, boiler parasitic - linear coefficient [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_aux_par_1_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set aux_par_2: Aux heater, boiler parasitic - quadratic coefficient [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_aux_par_2_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set aux_par_f: Aux heater, boiler parasitic - multiplying fraction [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_aux_par_f_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set bop_par: Balance of plant parasitic power fraction [MWe/MWcap]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_bop_par_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set bop_par_0: Balance of plant parasitic power fraction - const coeff [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_bop_par_0_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set bop_par_1: Balance of plant parasitic power fraction - linear coeff [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_bop_par_1_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set bop_par_2: Balance of plant parasitic power fraction - quadratic coeff [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_bop_par_2_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set bop_par_f: Balance of plant parasitic power fraction - mult frac [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_bop_par_f_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set design_eff: Power cycle efficiency at design [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_design_eff_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set pb_fixed_par: Fixed parasitic load - runs at all times [MWe/MWcap]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_pb_fixed_par_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set piping_length_add: Piping constant length [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_piping_length_add_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);

	/**
	 * Set piping_length_mult: Piping length multiplier
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsdirectSteam_Parasitics_piping_length_mult_fset(SAM_TcsdirectSteam ptr, float number, SAM_error *err);


	/**
	 * Weather Getters
	 */

	SAM_EXPORT const char* SAM_TcsdirectSteam_Weather_solar_resource_file_sget(SAM_TcsdirectSteam ptr, SAM_error *err);


	/**
	 * DirectSteamTower Getters
	 */

	SAM_EXPORT float SAM_TcsdirectSteam_DirectSteamTower_system_capacity_fget(SAM_TcsdirectSteam ptr, SAM_error *err);


	/**
	 * TouTranslator Getters
	 */

	SAM_EXPORT float* SAM_TcsdirectSteam_TouTranslator_weekday_schedule_mget(SAM_TcsdirectSteam ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_TouTranslator_weekend_schedule_mget(SAM_TcsdirectSteam ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Heliostat Getters
	 */

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_N_hel_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_bop_spec_cost_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_c_atm_0_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_c_atm_1_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_c_atm_2_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_c_atm_3_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_calc_fluxmaps_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_cant_type_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_check_max_flux_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_contingency_rate_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_cost_sf_fixed_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_fixed_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_per_acre_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_per_watt_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_csp_pt_cost_epc_percent_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_fixed_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_per_acre_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_per_watt_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_csp_pt_cost_plm_percent_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_csp_pt_sf_fixed_land_area_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_csp_pt_sf_land_overhead_factor_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_delta_flux_hrs_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_dens_mirror_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_dni_des_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Heliostat_eta_map_mget(SAM_TcsdirectSteam ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Heliostat_flux_maps_mget(SAM_TcsdirectSteam ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_flux_max_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Heliostat_flux_positions_mget(SAM_TcsdirectSteam ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_focus_type_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_fossil_spec_cost_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_hel_stow_deploy_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_helio_active_fraction_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Heliostat_helio_aim_points_mget(SAM_TcsdirectSteam ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_helio_height_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_helio_optical_error_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Heliostat_helio_positions_mget(SAM_TcsdirectSteam ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_helio_reflectance_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_helio_width_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_heliostat_spec_cost_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_interp_beta_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_interp_nug_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_is_optimize_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Heliostat_land_bound_list_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Heliostat_land_bound_table_mget(SAM_TcsdirectSteam ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_land_bound_type_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_land_max_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_land_min_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_land_spec_cost_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_n_facet_x_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_n_facet_y_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_n_flux_days_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_n_flux_x_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_n_flux_y_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_opt_algorithm_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_opt_conv_tol_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_opt_flux_penalty_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_opt_init_step_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_opt_max_iter_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_p_start_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_p_track_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_plant_spec_cost_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_q_design_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_rec_absorptance_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_rec_aspect_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_rec_cost_exp_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_rec_height_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_rec_hl_perm2_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_rec_ref_area_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_rec_ref_cost_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_run_type_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_sales_tax_frac_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_sales_tax_rate_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_site_spec_cost_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_tes_spec_cost_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_total_installed_cost_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_tower_exp_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_tower_fixed_cost_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_v_wind_max_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_washing_frequency_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Heliostat_water_usage_per_wash_fget(SAM_TcsdirectSteam ptr, SAM_error *err);


	/**
	 * Receiver Getters
	 */

	SAM_EXPORT float SAM_TcsdirectSteam_Receiver_H_rec_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Receiver_THT_fget(SAM_TcsdirectSteam ptr, SAM_error *err);


	/**
	 * DsgController Getters
	 */

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_A_sf_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_P_b_in_init_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_P_cond_init_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_P_hp_in_des_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_P_hp_out_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_P_hp_out_des_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_T_ITD_des_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_T_amb_des_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_T_approach_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_T_fw_init_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_T_hp_out_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_T_rh_out_des_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_T_rh_target_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_T_sh_out_des_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_ct_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_cycle_max_frac_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_dT_cw_ref_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_d_rec_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_d_rh_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_d_sh_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_d_t_boiler_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_eta_rec_pump_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_f_mdot_rh_init_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_f_mdotrh_des_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_f_pb_cutoff_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_f_pb_sb_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_f_rec_min_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_DsgController_ffrac_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_flowtype_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_fossil_mode_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_h_boiler_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_h_rh_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_h_sh_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_h_tower_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_hl_ffact_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_lhv_eff_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_mat_boiler_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_mat_rh_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_mat_sh_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_n_panels_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_p_cycle_design_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_q_aux_max_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_q_pb_design_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_q_rec_des_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_rec_absorptance_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_rec_emis_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_rec_qf_delay_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_rec_su_delay_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_t_standby_ini_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_th_rh_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_th_sh_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_th_t_boiler_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_DsgController_x_b_target_fget(SAM_TcsdirectSteam ptr, SAM_error *err);


	/**
	 * Powerblock Getters
	 */

	SAM_EXPORT float* SAM_TcsdirectSteam_Powerblock_F_wc_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_P_boil_des_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_P_cond_min_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_P_cond_ratio_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_P_ref_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_P_rh_ref_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_T_ITD_des_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_T_amb_des_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_T_cold_ref_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_T_hot_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_T_hot_ref_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_dT_cw_ref_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_eta_ref_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_n_pl_inc_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_pb_bd_frac_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_q_sby_frac_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_rh_frac_ref_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_startup_frac_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Powerblock_startup_time_fget(SAM_TcsdirectSteam ptr, SAM_error *err);


	/**
	 * Parasitics Getters
	 */

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_Design_power_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_Piping_length_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_Piping_loss_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_aux_par_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_aux_par_0_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_aux_par_1_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_aux_par_2_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_aux_par_f_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_bop_par_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_bop_par_0_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_bop_par_1_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_bop_par_2_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_bop_par_f_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_design_eff_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_pb_fixed_par_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_piping_length_add_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Parasitics_piping_length_mult_fget(SAM_TcsdirectSteam ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_P_b_in_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_P_b_out_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_P_cond_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_P_cooling_tower_tot_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_P_cycle_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_P_drop_b_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_P_fixed_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_P_out_net_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_P_parasitics_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_P_piping_tot_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_P_plant_balance_tot_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_P_rh_in_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_P_rh_out_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_P_sh_out_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_T_b_in_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_T_boiling_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_T_fw_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_T_max_b_surf_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_T_max_rh_surf_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_T_max_sh_surf_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_T_rh_in_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_T_rh_out_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_W_dot_boost_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Outputs_annual_W_cycle_gross_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Outputs_annual_energy_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Outputs_annual_fuel_usage_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Outputs_annual_total_water_use_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_beam_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Outputs_capacity_factor_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Outputs_conversion_factor_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_dP_rh_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_dP_sh_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_defocus_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_eta_b_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_eta_field_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_eta_rec_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_eta_rh_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_eta_sh_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_f_bays_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_f_mdot_rh_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_gen_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_hour_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Outputs_kwh_per_kw_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_m_dot_aux_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_m_dot_makeup_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_m_dot_sh_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_month_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_pparasi_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_pres_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_abs_rec_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_aux_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_aux_fuel_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_b_abs_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_b_conv_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_b_rad_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_conv_rec_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_inc_full_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_rad_rec_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_rh_abs_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_rh_conv_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_rh_rad_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_sh_abs_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_sh_conv_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_sh_rad_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_q_therm_in_rec_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_solazi_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_solzen_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_TcsdirectSteam_Outputs_system_heat_rate_fget(SAM_TcsdirectSteam ptr, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_tdry_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_tou_value_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_twet_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_v_rh_max_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_v_sh_max_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_TcsdirectSteam_Outputs_wspd_aget(SAM_TcsdirectSteam ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
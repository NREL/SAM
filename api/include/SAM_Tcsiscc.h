#ifndef SAM_TCSISCC_H_
#define SAM_TCSISCC_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Tcsiscc Technology Model
//

/**
 * Create a Tcsiscc variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_Tcsiscc;

SAM_EXPORT SAM_Tcsiscc SAM_Tcsiscc_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_Tcsiscc_execute(SAM_Tcsiscc data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_Tcsiscc_destruct(SAM_Tcsiscc system);


//
// Weather parameters
//

/**
 * Set solar_resource_file: local weather file path
 * options: None
 * constraints: LOCAL_FILE
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Weather_solar_resource_file_sset(SAM_Tcsiscc ptr, const char *str, SAM_error *err);


//
// MoltenSaltTower parameters
//

/**
 * Set system_capacity: Nameplate capacity [kW]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_MoltenSaltTower_system_capacity_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);


//
// Heliostat parameters
//

/**
 * Set N_hel: Number of heliostats [-]
 * options: None
 * constraints: None
 * required if: ?
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_N_hel_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set bop_spec_cost: BOS specific cost [$/kWe]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_bop_spec_cost_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set c_atm_0: Attenuation coefficient 0
 * options: None
 * constraints: None
 * required if: ?=0.006789
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_c_atm_0_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set c_atm_1: Attenuation coefficient 1
 * options: None
 * constraints: None
 * required if: ?=0.1046
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_c_atm_1_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set c_atm_2: Attenuation coefficient 2
 * options: None
 * constraints: None
 * required if: ?=-0.0107
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_c_atm_2_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set c_atm_3: Attenuation coefficient 3
 * options: None
 * constraints: None
 * required if: ?=0.002845
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_c_atm_3_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set calc_fluxmaps: Include fluxmap calculations
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_calc_fluxmaps_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set cant_type: Heliostat cant method
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_cant_type_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set check_max_flux: Check max flux at design point
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_check_max_flux_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set contingency_rate: Contingency for cost overrun [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_contingency_rate_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set cost_sf_fixed: Solar field fixed cost [$]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_cost_sf_fixed_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set csp.pt.cost.epc.fixed: EPC fixed [$]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_fixed_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set csp.pt.cost.epc.per_acre: EPC cost per acre [$/acre]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_per_acre_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set csp.pt.cost.epc.per_watt: EPC cost per watt [$/W]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_per_watt_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set csp.pt.cost.epc.percent: EPC cost percent of direct
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_percent_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set csp.pt.cost.plm.fixed: PLM fixed [$]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_fixed_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set csp.pt.cost.plm.per_acre: PLM cost per acre [$/acre]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_per_acre_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set csp.pt.cost.plm.per_watt: PLM cost per watt [$/W]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_per_watt_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set csp.pt.cost.plm.percent: PLM cost percent of direct
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_percent_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set csp.pt.sf.fixed_land_area: Fixed land area [acre]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_csp_pt_sf_fixed_land_area_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set csp.pt.sf.land_overhead_factor: Land overhead factor
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Tcsiscc_Heliostat_csp_pt_sf_land_overhead_factor_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set delta_flux_hrs: Hourly frequency in flux map lookup
 * options: None
 * constraints: None
 * required if: ?=1
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_delta_flux_hrs_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set dens_mirror: Ratio of Reflective Area to Profile [-]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_dens_mirror_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set dni_des: Design-point DNI [W/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_dni_des_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set eta_map: Field efficiency array [-]
 * options: None
 * constraints: None
 * required if: ?
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_eta_map_mset(SAM_Tcsiscc ptr, double *mat, int nrows, int ncols, SAM_error *err);

/**
 * Set flux_maps: Flux map intensities [-]
 * options: None
 * constraints: None
 * required if: ?
 */
SAM_EXPORT void
SAM_Tcsiscc_Heliostat_flux_maps_mset(SAM_Tcsiscc ptr, double *mat, int nrows, int ncols, SAM_error *err);

/**
 * Set flux_max: Maximum allowable flux
 * options: None
 * constraints: None
 * required if: ?=1000
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_flux_max_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set flux_positions: Flux map sun positions [deg]
 * options: None
 * constraints: None
 * required if: ?
 */
SAM_EXPORT void
SAM_Tcsiscc_Heliostat_flux_positions_mset(SAM_Tcsiscc ptr, double *mat, int nrows, int ncols, SAM_error *err);

/**
 * Set focus_type: Heliostat focus method
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_focus_type_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set fossil_spec_cost: Fossil system specific cost [$/kWe]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_fossil_spec_cost_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set h_tower: Tower height [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_h_tower_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set hel_stow_deploy: Stow/deploy elevation [deg]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_hel_stow_deploy_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set helio_active_fraction: Heliostat active frac. [-]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_helio_active_fraction_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set helio_aim_points: Heliostat aim point table [m]
 * options: None
 * constraints: None
 * required if: ?
 */
SAM_EXPORT void
SAM_Tcsiscc_Heliostat_helio_aim_points_mset(SAM_Tcsiscc ptr, double *mat, int nrows, int ncols, SAM_error *err);

/**
 * Set helio_height: Heliostat height [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_helio_height_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set helio_optical_error: Heliostat optical error [rad]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_helio_optical_error_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set helio_positions: Heliostat position table [m]
 * options: None
 * constraints: None
 * required if: run_type=1
 */
SAM_EXPORT void
SAM_Tcsiscc_Heliostat_helio_positions_mset(SAM_Tcsiscc ptr, double *mat, int nrows, int ncols, SAM_error *err);

/**
 * Set helio_reflectance: Heliostat reflectance [-]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_helio_reflectance_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set helio_width: Heliostat width [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_helio_width_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set heliostat_spec_cost: Heliostat field cost [$/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_heliostat_spec_cost_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set interp_beta: Interpolation beta coef. [-]
 * options: None
 * constraints: None
 * required if: ?=1.99
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_interp_beta_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set interp_nug: Interpolation nugget [-]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_interp_nug_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set is_optimize: Do SolarPILOT optimization
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_is_optimize_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set land_bound_list: Boundary table listing [-]
 * options: None
 * constraints: None
 * required if: ?
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_land_bound_list_aset(SAM_Tcsiscc ptr, double *arr, int length, SAM_error *err);

/**
 * Set land_bound_table: Land boundary table [m]
 * options: None
 * constraints: None
 * required if: ?
 */
SAM_EXPORT void
SAM_Tcsiscc_Heliostat_land_bound_table_mset(SAM_Tcsiscc ptr, double *mat, int nrows, int ncols, SAM_error *err);

/**
 * Set land_bound_type: Land boundary type [-]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_land_bound_type_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set land_max: Land max boundary [-ORm]
 * options: None
 * constraints: None
 * required if: ?=7.5
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_land_max_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set land_min: Land min boundary [-ORm]
 * options: None
 * constraints: None
 * required if: ?=0.75
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_land_min_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set land_spec_cost: Total land area cost [$/acre]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_land_spec_cost_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set n_facet_x: Number of heliostat facets - X
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_n_facet_x_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set n_facet_y: Number of heliostat facets - Y
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_n_facet_y_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set n_flux_days: No. days in flux map lookup
 * options: None
 * constraints: None
 * required if: ?=8
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_n_flux_days_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set n_flux_x: Flux map X resolution [-]
 * options: None
 * constraints: None
 * required if: ?=12
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_n_flux_x_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set n_flux_y: Flux map Y resolution [-]
 * options: None
 * constraints: None
 * required if: ?=1
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_n_flux_y_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set opt_algorithm: Optimization algorithm
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_opt_algorithm_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set opt_conv_tol: Optimization convergence tol
 * options: None
 * constraints: None
 * required if: ?=0.001
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_opt_conv_tol_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set opt_flux_penalty: Optimization flux overage penalty
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_opt_flux_penalty_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set opt_init_step: Optimization initial step size
 * options: None
 * constraints: None
 * required if: ?=0.05
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_opt_init_step_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set opt_max_iter: Max. number iteration steps
 * options: None
 * constraints: None
 * required if: ?=200
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_opt_max_iter_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set p_start: Heliostat startup energy [kWe-hr]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_p_start_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set p_track: Heliostat tracking energy [kWe]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_p_track_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set plant_spec_cost: Power cycle specific cost [$/kWe]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_plant_spec_cost_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set q_design: Receiver thermal design power [MW]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_q_design_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set rec_absorptance: Receiver absorptance [-]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_rec_absorptance_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set rec_aspect: Receiver aspect ratio [-]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_rec_aspect_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set rec_cost_exp: Receiver cost scaling exponent
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_rec_cost_exp_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set rec_height: Receiver height [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_rec_height_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set rec_hl_perm2: Receiver design heatloss [kW/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_rec_hl_perm2_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set rec_ref_area: Receiver reference area for cost scale
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_rec_ref_area_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set rec_ref_cost: Receiver reference cost [$]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_rec_ref_cost_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set run_type: Run type [-]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_run_type_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set sales_tax_frac: Percent of cost to which sales tax applies [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_sales_tax_frac_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set sales_tax_rate: Sales tax rate [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_sales_tax_rate_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set site_spec_cost: Site improvement cost [$/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_site_spec_cost_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set tes_spec_cost: Thermal energy storage cost [$/kWht]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_tes_spec_cost_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set total_installed_cost: Total installed cost [$]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_total_installed_cost_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set tower_exp: Tower cost scaling exponent
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_tower_exp_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set tower_fixed_cost: Tower fixed cost [$]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_tower_fixed_cost_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set v_wind_max: Max. wind velocity [m/s]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Heliostat_v_wind_max_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);


//
// Receiver parameters
//

/**
 * Set A_sf: Solar Field Area [m^2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_A_sf_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set D_rec: The overall outer diameter of the receiver [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_D_rec_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set Flow_type: A flag indicating which flow pattern is used
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_Flow_type_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set H_rec: The height of the receiver [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_H_rec_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set N_panels: Number of individual panels on the receiver
 * options: None
 * constraints: INTEGER
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_N_panels_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set Q_rec_des: Design-point receiver thermal power output [MWt]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_Q_rec_des_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set THT: The height of the tower (hel. pivot to rec equator) [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_THT_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set T_htf_cold_des: Cold HTF inlet temperature at design conditions [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_T_htf_cold_des_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set T_htf_hot_des: Hot HTF outlet temperature at design conditions [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_T_htf_hot_des_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set crossover_shift: No. panels shift in receiver crossover position
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_crossover_shift_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set d_tube_out: The outer diameter of an individual receiver tube [mm]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_d_tube_out_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set epsilon: The emissivity of the receiver surface coating
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_epsilon_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set eta_pump: Receiver HTF pump efficiency
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_eta_pump_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set f_rec_min: Minimum receiver mass flow rate turn down fraction
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_f_rec_min_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set field_fl_props: User defined field fluid property data [-]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Tcsiscc_Receiver_field_fl_props_mset(SAM_Tcsiscc ptr, double *mat, int nrows, int ncols, SAM_error *err);

/**
 * Set hl_ffact: The heat loss factor (thermal loss fudge factor)
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_hl_ffact_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set m_dot_htf_max: Maximum receiver mass flow rate [kg/hr]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_m_dot_htf_max_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set mat_tube: The material name of the receiver tubes
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_mat_tube_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set rec_htf: The name of the HTF used in the receiver
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_rec_htf_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set rec_qf_delay: Energy-based rcvr startup delay (fraction of rated thermal power)
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_rec_qf_delay_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set rec_su_delay: Fixed startup delay time for the receiver [hr]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_rec_su_delay_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set receiver_type: External=0, Cavity=1
 * options: None
 * constraints: INTEGER
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_receiver_type_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set th_tube: The wall thickness of a single receiver tube [mm]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Receiver_th_tube_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);


//
// Powerblock parameters
//

/**
 * Set elev: Plant elevation [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Powerblock_elev_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set ngcc_model: 1: NREL, 2: GE
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Powerblock_ngcc_model_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set pinch_point_coldside: Cold side HX pinch point [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Powerblock_pinch_point_coldside_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set pinch_point_hotside: Hot side temperature HX temperature difference [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Powerblock_pinch_point_hotside_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set q_pb_design: Design point power block thermal power [MWt]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Powerblock_q_pb_design_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);


//
// Parasitics parameters
//

/**
 * Set Q_rec_des: Design point solar field thermal output [MW]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Parasitics_Q_rec_des_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set W_dot_solar_des: Solar contribution to cycle output at design [MWe]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Parasitics_W_dot_solar_des_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set bop_par: Balance of plant parasitic power fraction [MWe/MWcap]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Parasitics_bop_par_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set bop_par_0: Balance of plant parasitic power fraction - const coeff [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Parasitics_bop_par_0_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set bop_par_1: Balance of plant parasitic power fraction - linear coeff [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Parasitics_bop_par_1_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set bop_par_2: Balance of plant parasitic power fraction - quadratic coeff [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Parasitics_bop_par_2_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set bop_par_f: Balance of plant parasitic power fraction - mult frac [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Parasitics_bop_par_f_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set fossil_output: Fossil-only cycle output at design [MWe]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Parasitics_fossil_output_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set pb_fixed_par: Fixed parasitic load - runs at all times [MWe/MWcap]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Parasitics_pb_fixed_par_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set pb_pump_coef: Required pumping power for HTF through power block [kJ/kg]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Parasitics_pb_pump_coef_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set piping_length: Total length of exposed piping [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Parasitics_piping_length_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set piping_length_const: Piping constant length [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Parasitics_piping_length_const_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set piping_length_mult: Piping length multiplier
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Parasitics_piping_length_mult_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);

/**
 * Set piping_loss: Thermal loss per meter of piping [Wt/m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Tcsiscc_Parasitics_piping_loss_nset(SAM_Tcsiscc ptr, double number, SAM_error *err);


/**
 * Weather Getters
 */

SAM_EXPORT const char *SAM_Tcsiscc_Weather_solar_resource_file_sget(SAM_Tcsiscc ptr, SAM_error *err);


/**
 * MoltenSaltTower Getters
 */

SAM_EXPORT double SAM_Tcsiscc_MoltenSaltTower_system_capacity_nget(SAM_Tcsiscc ptr, SAM_error *err);


/**
 * Heliostat Getters
 */

SAM_EXPORT double SAM_Tcsiscc_Heliostat_N_hel_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_bop_spec_cost_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_c_atm_0_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_c_atm_1_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_c_atm_2_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_c_atm_3_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_calc_fluxmaps_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_cant_type_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_check_max_flux_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_contingency_rate_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_cost_sf_fixed_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_fixed_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_per_acre_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_per_watt_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_percent_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_fixed_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_per_acre_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_per_watt_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_percent_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_sf_fixed_land_area_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_csp_pt_sf_land_overhead_factor_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_delta_flux_hrs_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_dens_mirror_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_dni_des_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Heliostat_eta_map_mget(SAM_Tcsiscc ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Heliostat_flux_maps_mget(SAM_Tcsiscc ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_flux_max_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Heliostat_flux_positions_mget(SAM_Tcsiscc ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_focus_type_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_fossil_spec_cost_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_h_tower_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_hel_stow_deploy_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_helio_active_fraction_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Heliostat_helio_aim_points_mget(SAM_Tcsiscc ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_helio_height_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_helio_optical_error_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Heliostat_helio_positions_mget(SAM_Tcsiscc ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_helio_reflectance_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_helio_width_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_heliostat_spec_cost_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_interp_beta_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_interp_nug_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_is_optimize_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Heliostat_land_bound_list_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Heliostat_land_bound_table_mget(SAM_Tcsiscc ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_land_bound_type_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_land_max_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_land_min_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_land_spec_cost_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_n_facet_x_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_n_facet_y_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_n_flux_days_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_n_flux_x_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_n_flux_y_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_opt_algorithm_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_opt_conv_tol_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_opt_flux_penalty_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_opt_init_step_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_opt_max_iter_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_p_start_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_p_track_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_plant_spec_cost_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_q_design_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_rec_absorptance_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_rec_aspect_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_rec_cost_exp_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_rec_height_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_rec_hl_perm2_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_rec_ref_area_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_rec_ref_cost_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_run_type_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_sales_tax_frac_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_sales_tax_rate_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_site_spec_cost_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_tes_spec_cost_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_total_installed_cost_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_tower_exp_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_tower_fixed_cost_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Heliostat_v_wind_max_nget(SAM_Tcsiscc ptr, SAM_error *err);


/**
 * Receiver Getters
 */

SAM_EXPORT double SAM_Tcsiscc_Receiver_A_sf_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_D_rec_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_Flow_type_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_H_rec_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_N_panels_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_Q_rec_des_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_THT_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_T_htf_cold_des_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_T_htf_hot_des_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_crossover_shift_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_d_tube_out_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_epsilon_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_eta_pump_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_f_rec_min_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Receiver_field_fl_props_mget(SAM_Tcsiscc ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_hl_ffact_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_m_dot_htf_max_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_mat_tube_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_rec_htf_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_rec_qf_delay_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_rec_su_delay_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_receiver_type_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Receiver_th_tube_nget(SAM_Tcsiscc ptr, SAM_error *err);


/**
 * Powerblock Getters
 */

SAM_EXPORT double SAM_Tcsiscc_Powerblock_elev_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Powerblock_ngcc_model_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Powerblock_pinch_point_coldside_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Powerblock_pinch_point_hotside_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Powerblock_q_pb_design_nget(SAM_Tcsiscc ptr, SAM_error *err);


/**
 * Parasitics Getters
 */

SAM_EXPORT double SAM_Tcsiscc_Parasitics_Q_rec_des_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Parasitics_W_dot_solar_des_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Parasitics_bop_par_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Parasitics_bop_par_0_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Parasitics_bop_par_1_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Parasitics_bop_par_2_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Parasitics_bop_par_f_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Parasitics_fossil_output_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Parasitics_pb_fixed_par_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Parasitics_pb_pump_coef_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Parasitics_piping_length_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Parasitics_piping_length_const_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Parasitics_piping_length_mult_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Parasitics_piping_loss_nget(SAM_Tcsiscc ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double *SAM_Tcsiscc_Outputs_P_fixed_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_P_plant_balance_tot_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_Q_dot_max_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_Q_solar_total_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_Q_thermal_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_T_htf_cold_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_T_salt_hot_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_T_st_cold_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_T_st_hot_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_W_dot_pc_fossil_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_W_dot_pc_hybrid_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_W_dot_plant_fossil_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_W_dot_plant_hybrid_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_W_dot_plant_solar_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_W_dot_pump_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Outputs_annual_energy_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Outputs_annual_fuel_usage_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_beam_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Outputs_capacity_factor_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_eta_field_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_eta_fuel_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_eta_solar_use_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_eta_therm_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_f_timestep_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_field_eff_adj_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_fuel_use_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_gen_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_hour_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Outputs_kwh_per_kw_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_m_dot_salt_tot_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_m_dot_ss_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_m_dot_steam_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_month_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_pparasi_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_pres_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_q_conv_sum_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_q_rad_sum_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_q_startup_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_solar_fraction_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_solazi_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_solzen_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Tcsiscc_Outputs_system_heat_rate_nget(SAM_Tcsiscc ptr, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_tdry_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_twet_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Tcsiscc_Outputs_wspd_aget(SAM_Tcsiscc ptr, int *length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif

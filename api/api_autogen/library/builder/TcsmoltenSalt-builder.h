#ifndef SAM_TCSMOLTENSALT_FUNCTIONS_H_
#define SAM_TCSMOLTENSALT_FUNCTIONS_H_

#include "TcsmoltenSalt-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a LocationAndResource variable table for a MSPTSingleOwner system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsmoltenSalt_LocationAndResource SAM_TcsmoltenSalt_LocationAndResource_create(const char* def, SAM_error* err);


	/**
	 * Set solar_data_file_name: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_LocationAndResource_solar_data_file_name_set(SAM_TcsmoltenSalt_LocationAndResource ptr, const char* string, SAM_error* err);

	/**
	 * Set user_specified_weather_file: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_LocationAndResource_user_specified_weather_file_set(SAM_TcsmoltenSalt_LocationAndResource ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_TcsmoltenSalt_LocationAndResource_solar_data_file_name_get(SAM_TcsmoltenSalt_LocationAndResource ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_LocationAndResource_user_specified_weather_file_get(SAM_TcsmoltenSalt_LocationAndResource ptr, SAM_error* err);



	/** 
	 * Create a SystemDesign variable table for a MSPTSingleOwner system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsmoltenSalt_SystemDesign SAM_TcsmoltenSalt_SystemDesign_create(const char* def, SAM_error* err);


	/**
	 * Set P_ref: Reference output electric power at design condition
	 * type: numeric
	 * units: MW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_P_ref_set(SAM_TcsmoltenSalt_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set T_htf_cold_des: Cold HTF inlet temperature at design conditions
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_T_htf_cold_des_set(SAM_TcsmoltenSalt_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set T_htf_hot_des: Hot HTF outlet temperature at design conditions
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_T_htf_hot_des_set(SAM_TcsmoltenSalt_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set design_eff: Power cycle efficiency at design
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_design_eff_set(SAM_TcsmoltenSalt_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set dni_des: Design-point DNI
	 * type: numeric
	 * units: W/m2
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_dni_des_set(SAM_TcsmoltenSalt_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set gross_net_conversion_factor: Estimated gross to net conversion factor
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_gross_net_conversion_factor_set(SAM_TcsmoltenSalt_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set solarm: Solar Multiple
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_solarm_set(SAM_TcsmoltenSalt_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set tshours: Equivalent full-load thermal storage hours
	 * type: numeric
	 * units: hr
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_tshours_set(SAM_TcsmoltenSalt_SystemDesign ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemDesign_P_ref_get(SAM_TcsmoltenSalt_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemDesign_T_htf_cold_des_get(SAM_TcsmoltenSalt_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemDesign_T_htf_hot_des_get(SAM_TcsmoltenSalt_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemDesign_design_eff_get(SAM_TcsmoltenSalt_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemDesign_dni_des_get(SAM_TcsmoltenSalt_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemDesign_gross_net_conversion_factor_get(SAM_TcsmoltenSalt_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemDesign_solarm_get(SAM_TcsmoltenSalt_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemDesign_tshours_get(SAM_TcsmoltenSalt_SystemDesign ptr, SAM_error* err);



	/** 
	 * Create a HeliostatField variable table for a MSPTSingleOwner system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsmoltenSalt_HeliostatField SAM_TcsmoltenSalt_HeliostatField_create(const char* def, SAM_error* err);


	/**
	 * Set c_atm_0: Attenuation coefficient 0
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=0.006789
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_c_atm_0_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set c_atm_1: Attenuation coefficient 1
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=0.1046
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_c_atm_1_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set c_atm_2: Attenuation coefficient 2
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=-0.0107
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_c_atm_2_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set c_atm_3: Attenuation coefficient 3
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=0.002845
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_c_atm_3_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set cant_type: Heliostat cant method
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_cant_type_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set check_max_flux: Check max flux at design point
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_check_max_flux_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set csp.pt.sf.fixed_land_area: Fixed land area
	 * type: numeric
	 * units: acre
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_csp.pt.sf.fixed_land_area_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set csp.pt.sf.land_overhead_factor: Land overhead factor
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_csp.pt.sf.land_overhead_factor_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set dens_mirror: Ratio of Reflective Area to Profile
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_dens_mirror_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set focus_type: Heliostat focus method
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_focus_type_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set helio_active_fraction: Heliostat active frac.
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_active_fraction_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set helio_height: Heliostat height
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_height_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set helio_optical_error_mrad: Heliostat optical error
	 * type: numeric
	 * units: mrad
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_optical_error_mrad_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set helio_positions: Heliostat position table
	 * type: matrix
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_positions_set(SAM_TcsmoltenSalt_HeliostatField ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set helio_reflectance: Heliostat reflectance
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_reflectance_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set helio_width: Heliostat width
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_width_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set is_optimize: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_is_optimize_set(SAM_TcsmoltenSalt_HeliostatField ptr, const char* string, SAM_error* err);

	/**
	 * Set land_max: Land max boundary
	 * type: numeric
	 * units: -ORm
	 * options: None
	 * constraints: None
	 * required if: ?=7.5
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_land_max_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set land_min: Land min boundary
	 * type: numeric
	 * units: -ORm
	 * options: None
	 * constraints: None
	 * required if: ?=0.75
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_land_min_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set n_facet_x: Number of heliostat facets - X
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_n_facet_x_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set n_facet_y: Number of heliostat facets - Y
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_n_facet_y_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set opt_algorithm: Optimization algorithm
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_opt_algorithm_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set opt_conv_tol: Optimization convergence tol
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=0.001
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_opt_conv_tol_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set opt_flux_penalty: Optimization flux overage penalty
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_opt_flux_penalty_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set opt_init_step: Optimization initial step size
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=0.05
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_opt_init_step_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);

	/**
	 * Set opt_max_iter: Max. number iteration steps
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=200
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_opt_max_iter_set(SAM_TcsmoltenSalt_HeliostatField ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_c_atm_0_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_c_atm_1_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_c_atm_2_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_c_atm_3_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_cant_type_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_check_max_flux_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_csp.pt.sf.fixed_land_area_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_csp.pt.sf.land_overhead_factor_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_dens_mirror_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_focus_type_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_helio_active_fraction_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_helio_height_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_helio_optical_error_mrad_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_HeliostatField_helio_positions_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_helio_reflectance_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_helio_width_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_HeliostatField_is_optimize_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_land_max_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_land_min_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_n_facet_x_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_n_facet_y_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_opt_algorithm_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_opt_conv_tol_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_opt_flux_penalty_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_opt_init_step_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_HeliostatField_opt_max_iter_get(SAM_TcsmoltenSalt_HeliostatField ptr, SAM_error* err);



	/** 
	 * Create a TowerAndReceiver variable table for a MSPTSingleOwner system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsmoltenSalt_TowerAndReceiver SAM_TcsmoltenSalt_TowerAndReceiver_create(const char* def, SAM_error* err);


	/**
	 * Set D_rec: The overall outer diameter of the receiver
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_D_rec_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set N_panels: Number of individual panels on the receiver
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_N_panels_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set csp.pt.rec.htf_type: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_csp.pt.rec.htf_type_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, const char* string, SAM_error* err);

	/**
	 * Set csp.pt.rec.max_oper_frac: Maximum receiver mass flow rate fraction
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_csp.pt.rec.max_oper_frac_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set delta_flux_hrs: Hourly frequency in flux map lookup
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_delta_flux_hrs_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set field_fl_props: User defined field fluid property data
	 * type: matrix
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_field_fl_props_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set flux_max: Maximum allowable flux
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=1000
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_flux_max_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set h_tower: Tower height
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_h_tower_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set n_flux_days: No. days in flux map lookup
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=8
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_n_flux_days_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set piping_length_const: Piping constant length
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_piping_length_const_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set piping_length_mult: Piping length multiplier
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_piping_length_mult_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set piping_loss: Thermal loss per meter of piping
	 * type: numeric
	 * units: Wt/m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_piping_loss_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set rec_absorptance: Receiver absorptance
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_absorptance_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set rec_d_spec: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_d_spec_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, const char* string, SAM_error* err);

	/**
	 * Set rec_height: Receiver height
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_height_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set rec_hl_perm2: Receiver design heatloss
	 * type: numeric
	 * units: kW/m2
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_hl_perm2_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set receiver_type: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_receiver_type_set(SAM_TcsmoltenSalt_TowerAndReceiver ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsmoltenSalt_TowerAndReceiver_D_rec_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_TowerAndReceiver_N_panels_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_TowerAndReceiver_csp.pt.rec.htf_type_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_TowerAndReceiver_csp.pt.rec.max_oper_frac_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_TowerAndReceiver_delta_flux_hrs_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_TowerAndReceiver_field_fl_props_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_TowerAndReceiver_flux_max_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_TowerAndReceiver_h_tower_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_TowerAndReceiver_n_flux_days_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_TowerAndReceiver_piping_length_const_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_TowerAndReceiver_piping_length_mult_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_TowerAndReceiver_piping_loss_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_TowerAndReceiver_rec_absorptance_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_TowerAndReceiver_rec_d_spec_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_TowerAndReceiver_rec_height_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_TowerAndReceiver_rec_hl_perm2_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_TowerAndReceiver_receiver_type_get(SAM_TcsmoltenSalt_TowerAndReceiver ptr, SAM_error* err);



	/** 
	 * Create a RankineCycle variable table for a MSPTSingleOwner system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsmoltenSalt_RankineCycle SAM_TcsmoltenSalt_RankineCycle_create(const char* def, SAM_error* err);


	/**
	 * Set T_amb_des: Reference ambient temperature at design point
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_T_amb_des_set(SAM_TcsmoltenSalt_RankineCycle ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsmoltenSalt_RankineCycle_T_amb_des_get(SAM_TcsmoltenSalt_RankineCycle ptr, SAM_error* err);



	/** 
	 * Create a UserDefinedPowerCycle variable table for a MSPTSingleOwner system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsmoltenSalt_UserDefinedPowerCycle SAM_TcsmoltenSalt_UserDefinedPowerCycle_create(const char* def, SAM_error* err);


	/**
	 * Set ud_T_amb_des: Ambient temperature at user-defined power cycle design point
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_T_amb_des_set(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set ud_T_amb_high: High level ambient temperature for HTF mass flow rate parametric
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_T_amb_high_set(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set ud_T_amb_low: Low level ambient temperature for HTF mass flow rate parametric
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_T_amb_low_set(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set ud_T_htf_high: High level HTF inlet temperature for T_amb parametric
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_T_htf_high_set(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set ud_T_htf_low: Low level HTF inlet temperature for T_amb parametric
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_T_htf_low_set(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set ud_f_W_dot_cool_des: Percent of user-defined power cycle design gross output consumed by cooling
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_f_W_dot_cool_des_set(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set ud_m_dot_htf_high: High level normalized HTF mass flow rate for T_HTF parametric
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_m_dot_htf_high_set(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set ud_m_dot_htf_low: Low level normalized HTF mass flow rate for T_HTF parametric
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_m_dot_htf_low_set(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_T_amb_des_get(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_T_amb_high_get(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_T_amb_low_get(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_T_htf_high_get(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_T_htf_low_get(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_f_W_dot_cool_des_get(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_m_dot_htf_high_get(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_m_dot_htf_low_get(SAM_TcsmoltenSalt_UserDefinedPowerCycle ptr, SAM_error* err);



	/** 
	 * Create a SupercriticalCarbonDioxidePowerCycle variable table for a MSPTSingleOwner system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsmoltenSalt_SupercriticalCarbonDioxidePowerCycle SAM_TcsmoltenSalt_SupercriticalCarbonDioxidePowerCycle_create(const char* def, SAM_error* err);


	/**
	 * Set P_high_limit: Upper pressure limit in cycle
	 * type: numeric
	 * units: MPa
	 * options: None
	 * constraints: None
	 * required if: pc_config=2
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SupercriticalCarbonDioxidePowerCycle_P_high_limit_set(SAM_TcsmoltenSalt_SupercriticalCarbonDioxidePowerCycle ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsmoltenSalt_SupercriticalCarbonDioxidePowerCycle_P_high_limit_get(SAM_TcsmoltenSalt_SupercriticalCarbonDioxidePowerCycle ptr, SAM_error* err);



	/** 
	 * Create a ThermalStorage variable table for a MSPTSingleOwner system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsmoltenSalt_ThermalStorage SAM_TcsmoltenSalt_ThermalStorage_create(const char* def, SAM_error* err);


	/**
	 * Set h_tank: Total height of tank (height of HTF when tank is full
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_h_tank_set(SAM_TcsmoltenSalt_ThermalStorage ptr, float number, SAM_error* err);

	/**
	 * Set h_tank_min: Minimum allowable HTF height in storage tank
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_h_tank_min_set(SAM_TcsmoltenSalt_ThermalStorage ptr, float number, SAM_error* err);

	/**
	 * Set tank_pairs: Number of equivalent tank pairs
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_tank_pairs_set(SAM_TcsmoltenSalt_ThermalStorage ptr, float number, SAM_error* err);

	/**
	 * Set u_tank: Loss coefficient from the tank
	 * type: numeric
	 * units: W/m2-K
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_u_tank_set(SAM_TcsmoltenSalt_ThermalStorage ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsmoltenSalt_ThermalStorage_h_tank_get(SAM_TcsmoltenSalt_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_ThermalStorage_h_tank_min_get(SAM_TcsmoltenSalt_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_ThermalStorage_tank_pairs_get(SAM_TcsmoltenSalt_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_ThermalStorage_u_tank_get(SAM_TcsmoltenSalt_ThermalStorage ptr, SAM_error* err);



	/** 
	 * Create a SystemControl variable table for a MSPTSingleOwner system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsmoltenSalt_SystemControl SAM_TcsmoltenSalt_SystemControl_create(const char* def, SAM_error* err);


	/**
	 * Set aux_par: Aux heater, boiler parasitic
	 * type: numeric
	 * units: MWe/MWcap
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_aux_par_set(SAM_TcsmoltenSalt_SystemControl ptr, float number, SAM_error* err);

	/**
	 * Set aux_par_0: Aux heater, boiler parasitic - constant coefficient
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_aux_par_0_set(SAM_TcsmoltenSalt_SystemControl ptr, float number, SAM_error* err);

	/**
	 * Set aux_par_1: Aux heater, boiler parasitic - linear coefficient
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_aux_par_1_set(SAM_TcsmoltenSalt_SystemControl ptr, float number, SAM_error* err);

	/**
	 * Set aux_par_2: Aux heater, boiler parasitic - quadratic coefficient
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_aux_par_2_set(SAM_TcsmoltenSalt_SystemControl ptr, float number, SAM_error* err);

	/**
	 * Set aux_par_f: Aux heater, boiler parasitic - multiplying fraction
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_aux_par_f_set(SAM_TcsmoltenSalt_SystemControl ptr, float number, SAM_error* err);

	/**
	 * Set bop_par: Balance of plant parasitic power fraction
	 * type: numeric
	 * units: MWe/MWcap
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_bop_par_set(SAM_TcsmoltenSalt_SystemControl ptr, float number, SAM_error* err);

	/**
	 * Set bop_par_0: Balance of plant parasitic power fraction - const coeff
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_bop_par_0_set(SAM_TcsmoltenSalt_SystemControl ptr, float number, SAM_error* err);

	/**
	 * Set bop_par_1: Balance of plant parasitic power fraction - linear coeff
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_bop_par_1_set(SAM_TcsmoltenSalt_SystemControl ptr, float number, SAM_error* err);

	/**
	 * Set bop_par_2: Balance of plant parasitic power fraction - quadratic coeff
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_bop_par_2_set(SAM_TcsmoltenSalt_SystemControl ptr, float number, SAM_error* err);

	/**
	 * Set bop_par_f: Balance of plant parasitic power fraction - mult frac
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_bop_par_f_set(SAM_TcsmoltenSalt_SystemControl ptr, float number, SAM_error* err);

	/**
	 * Set is_dispatch: Allow dispatch optimization?
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_is_dispatch_set(SAM_TcsmoltenSalt_SystemControl ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemControl_aux_par_get(SAM_TcsmoltenSalt_SystemControl ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemControl_aux_par_0_get(SAM_TcsmoltenSalt_SystemControl ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemControl_aux_par_1_get(SAM_TcsmoltenSalt_SystemControl ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemControl_aux_par_2_get(SAM_TcsmoltenSalt_SystemControl ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemControl_aux_par_f_get(SAM_TcsmoltenSalt_SystemControl ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemControl_bop_par_get(SAM_TcsmoltenSalt_SystemControl ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemControl_bop_par_0_get(SAM_TcsmoltenSalt_SystemControl ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemControl_bop_par_1_get(SAM_TcsmoltenSalt_SystemControl ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemControl_bop_par_2_get(SAM_TcsmoltenSalt_SystemControl ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemControl_bop_par_f_get(SAM_TcsmoltenSalt_SystemControl ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemControl_is_dispatch_get(SAM_TcsmoltenSalt_SystemControl ptr, SAM_error* err);



	/** 
	 * Create a SystemCosts variable table for a MSPTSingleOwner system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsmoltenSalt_SystemCosts SAM_TcsmoltenSalt_SystemCosts_create(const char* def, SAM_error* err);


	/**
	 * Set bop_spec_cost: BOS specific cost
	 * type: numeric
	 * units: $/kWe
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_bop_spec_cost_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set contingency_rate: Contingency for cost overrun
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_contingency_rate_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set cost_sf_fixed: Solar field fixed cost
	 * type: numeric
	 * units: $
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_cost_sf_fixed_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set csp.pt.cost.epc.fixed: EPC fixed
	 * type: numeric
	 * units: $
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp.pt.cost.epc.fixed_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set csp.pt.cost.epc.per_acre: EPC cost per acre
	 * type: numeric
	 * units: $/acre
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp.pt.cost.epc.per_acre_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set csp.pt.cost.epc.per_watt: EPC cost per watt
	 * type: numeric
	 * units: $/W
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp.pt.cost.epc.per_watt_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set csp.pt.cost.epc.percent: EPC cost percent of direct
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp.pt.cost.epc.percent_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set csp.pt.cost.plm.fixed: PLM fixed
	 * type: numeric
	 * units: $
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp.pt.cost.plm.fixed_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set csp.pt.cost.plm.per_watt: PLM cost per watt
	 * type: numeric
	 * units: $/W
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp.pt.cost.plm.per_watt_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set csp.pt.cost.plm.percent: PLM cost percent of direct
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp.pt.cost.plm.percent_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set fossil_spec_cost: Fossil system specific cost
	 * type: numeric
	 * units: $/kWe
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_fossil_spec_cost_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set heliostat_spec_cost: Heliostat field cost
	 * type: numeric
	 * units: $/m2
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_heliostat_spec_cost_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set land_spec_cost: Total land area cost
	 * type: numeric
	 * units: $/acre
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_land_spec_cost_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set plant_spec_cost: Power cycle specific cost
	 * type: numeric
	 * units: $/kWe
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_plant_spec_cost_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set rec_cost_exp: Receiver cost scaling exponent
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_rec_cost_exp_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set rec_ref_area: Receiver reference area for cost scale
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_rec_ref_area_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set rec_ref_cost: Receiver reference cost
	 * type: numeric
	 * units: $
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_rec_ref_cost_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set sales_tax_frac: Percent of cost to which sales tax applies
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_sales_tax_frac_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set site_spec_cost: Site improvement cost
	 * type: numeric
	 * units: $/m2
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_site_spec_cost_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set tes_spec_cost: Thermal energy storage cost
	 * type: numeric
	 * units: $/kWht
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_tes_spec_cost_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set tower_exp: Tower cost scaling exponent
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_tower_exp_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);

	/**
	 * Set tower_fixed_cost: Tower fixed cost
	 * type: numeric
	 * units: $
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_tower_fixed_cost_set(SAM_TcsmoltenSalt_SystemCosts ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_bop_spec_cost_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_contingency_rate_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_cost_sf_fixed_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_csp.pt.cost.epc.fixed_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_csp.pt.cost.epc.per_acre_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_csp.pt.cost.epc.per_watt_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_csp.pt.cost.epc.percent_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_csp.pt.cost.plm.fixed_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_csp.pt.cost.plm.per_watt_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_csp.pt.cost.plm.percent_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_fossil_spec_cost_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_heliostat_spec_cost_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_land_spec_cost_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_plant_spec_cost_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_rec_cost_exp_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_rec_ref_area_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_rec_ref_cost_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_sales_tax_frac_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_site_spec_cost_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_tes_spec_cost_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_tower_exp_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_SystemCosts_tower_fixed_cost_get(SAM_TcsmoltenSalt_SystemCosts ptr, SAM_error* err);



	/** 
	 * Create a FinancialParameters variable table for a MSPTSingleOwner system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsmoltenSalt_FinancialParameters SAM_TcsmoltenSalt_FinancialParameters_create(const char* def, SAM_error* err);


	/**
	 * Set const_per_interest_rate1: Interest rate, loan 1
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate1_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_interest_rate2: Interest rate, loan 2
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate2_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_interest_rate3: Interest rate, loan 3
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate3_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_interest_rate4: Interest rate, loan 4
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate4_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_interest_rate5: Interest rate, loan 5
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate5_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_months1: Months prior to operation, loan 1
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_months1_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_months2: Months prior to operation, loan 2
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_months2_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_months3: Months prior to operation, loan 3
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_months3_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_months4: Months prior to operation, loan 4
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_months4_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_months5: Months prior to operation, loan 5
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_months5_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_percent1: Percent of tot. installed cost, loan 1
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_percent1_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_percent2: Percent of tot. installed cost, loan 2
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_percent2_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_percent3: Percent of tot. installed cost, loan 3
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_percent3_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_percent4: Percent of tot. installed cost, loan 4
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_percent4_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_percent5: Percent of tot. installed cost, loan 5
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_percent5_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_upfront_rate1: Upfront fee on principal, loan 1
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate1_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_upfront_rate2: Upfront fee on principal, loan 2
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate2_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_upfront_rate3: Upfront fee on principal, loan 3
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate3_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_upfront_rate4: Upfront fee on principal, loan 4
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate4_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set const_per_upfront_rate5: Upfront fee on principal, loan 5
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate5_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set debt_option: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_debt_option_set(SAM_TcsmoltenSalt_FinancialParameters ptr, const char* string, SAM_error* err);

	/**
	 * Set debt_percent: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_debt_percent_set(SAM_TcsmoltenSalt_FinancialParameters ptr, const char* string, SAM_error* err);

	/**
	 * Set dscr: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_dscr_set(SAM_TcsmoltenSalt_FinancialParameters ptr, const char* string, SAM_error* err);

	/**
	 * Set equip1_reserve_cost: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_equip1_reserve_cost_set(SAM_TcsmoltenSalt_FinancialParameters ptr, const char* string, SAM_error* err);

	/**
	 * Set equip2_reserve_cost: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_equip2_reserve_cost_set(SAM_TcsmoltenSalt_FinancialParameters ptr, const char* string, SAM_error* err);

	/**
	 * Set equip3_reserve_cost: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_equip3_reserve_cost_set(SAM_TcsmoltenSalt_FinancialParameters ptr, const char* string, SAM_error* err);

	/**
	 * Set federal_tax_rate: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_federal_tax_rate_set(SAM_TcsmoltenSalt_FinancialParameters ptr, const char* string, SAM_error* err);

	/**
	 * Set inflation_rate: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_inflation_rate_set(SAM_TcsmoltenSalt_FinancialParameters ptr, const char* string, SAM_error* err);

	/**
	 * Set ppa_soln_mode: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_ppa_soln_mode_set(SAM_TcsmoltenSalt_FinancialParameters ptr, const char* string, SAM_error* err);

	/**
	 * Set prop_tax_cost_assessed_percent: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_prop_tax_cost_assessed_percent_set(SAM_TcsmoltenSalt_FinancialParameters ptr, const char* string, SAM_error* err);

	/**
	 * Set real_discount_rate: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_real_discount_rate_set(SAM_TcsmoltenSalt_FinancialParameters ptr, const char* string, SAM_error* err);

	/**
	 * Set sales_tax_rate: Sales tax rate
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_sales_tax_rate_set(SAM_TcsmoltenSalt_FinancialParameters ptr, float number, SAM_error* err);

	/**
	 * Set salvage_percentage: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_salvage_percentage_set(SAM_TcsmoltenSalt_FinancialParameters ptr, const char* string, SAM_error* err);

	/**
	 * Set state_tax_rate: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_state_tax_rate_set(SAM_TcsmoltenSalt_FinancialParameters ptr, const char* string, SAM_error* err);

	/**
	 * Set term_int_rate: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_term_int_rate_set(SAM_TcsmoltenSalt_FinancialParameters ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate1_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate2_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate3_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate4_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate5_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_months1_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_months2_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_months3_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_months4_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_months5_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_percent1_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_percent2_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_percent3_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_percent4_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_percent5_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate1_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate2_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate3_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate4_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate5_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_FinancialParameters_debt_option_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_FinancialParameters_debt_percent_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_FinancialParameters_dscr_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_FinancialParameters_equip1_reserve_cost_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_FinancialParameters_equip2_reserve_cost_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_FinancialParameters_equip3_reserve_cost_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_FinancialParameters_federal_tax_rate_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_FinancialParameters_inflation_rate_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_FinancialParameters_ppa_soln_mode_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_FinancialParameters_prop_tax_cost_assessed_percent_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_FinancialParameters_real_discount_rate_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_FinancialParameters_sales_tax_rate_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_FinancialParameters_salvage_percentage_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_FinancialParameters_state_tax_rate_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_FinancialParameters_term_int_rate_get(SAM_TcsmoltenSalt_FinancialParameters ptr, SAM_error* err);



	/** 
	 * Create a TimeOfDeliveryFactors variable table for a MSPTSingleOwner system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsmoltenSalt_TimeOfDeliveryFactors SAM_TcsmoltenSalt_TimeOfDeliveryFactors_create(const char* def, SAM_error* err);


	/**
	 * Set lib_dispatch_sched_weekday: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_lib_dispatch_sched_weekday_set(SAM_TcsmoltenSalt_TimeOfDeliveryFactors ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_dispatch_sched_weekend: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_lib_dispatch_sched_weekend_set(SAM_TcsmoltenSalt_TimeOfDeliveryFactors ptr, const char* string, SAM_error* err);

	/**
	 * Set ppa_multiplier_model: PPA multiplier model
	 * type: numeric
	 * units: 0/1
	 * options: 0=diurnal,1=timestep
	 * constraints: INTEGER,MIN=0
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_ppa_multiplier_model_set(SAM_TcsmoltenSalt_TimeOfDeliveryFactors ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_TcsmoltenSalt_TimeOfDeliveryFactors_lib_dispatch_sched_weekday_get(SAM_TcsmoltenSalt_TimeOfDeliveryFactors ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_TimeOfDeliveryFactors_lib_dispatch_sched_weekend_get(SAM_TcsmoltenSalt_TimeOfDeliveryFactors ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_TimeOfDeliveryFactors_ppa_multiplier_model_get(SAM_TcsmoltenSalt_TimeOfDeliveryFactors ptr, SAM_error* err);



	/** 
	 * Create a Depreciation variable table for a MSPTSingleOwner system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsmoltenSalt_Depreciation SAM_TcsmoltenSalt_Depreciation_create(const char* def, SAM_error* err);


	/**
	 * Set depr_alloc_custom_percent: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Depreciation_depr_alloc_custom_percent_set(SAM_TcsmoltenSalt_Depreciation ptr, const char* string, SAM_error* err);

	/**
	 * Set depr_alloc_macrs_15_percent: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Depreciation_depr_alloc_macrs_15_percent_set(SAM_TcsmoltenSalt_Depreciation ptr, const char* string, SAM_error* err);

	/**
	 * Set depr_alloc_macrs_5_percent: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Depreciation_depr_alloc_macrs_5_percent_set(SAM_TcsmoltenSalt_Depreciation ptr, const char* string, SAM_error* err);

	/**
	 * Set depr_alloc_sl_15_percent: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Depreciation_depr_alloc_sl_15_percent_set(SAM_TcsmoltenSalt_Depreciation ptr, const char* string, SAM_error* err);

	/**
	 * Set depr_alloc_sl_20_percent: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Depreciation_depr_alloc_sl_20_percent_set(SAM_TcsmoltenSalt_Depreciation ptr, const char* string, SAM_error* err);

	/**
	 * Set depr_alloc_sl_39_percent: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Depreciation_depr_alloc_sl_39_percent_set(SAM_TcsmoltenSalt_Depreciation ptr, const char* string, SAM_error* err);

	/**
	 * Set depr_alloc_sl_5_percent: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Depreciation_depr_alloc_sl_5_percent_set(SAM_TcsmoltenSalt_Depreciation ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Depreciation_depr_alloc_custom_percent_get(SAM_TcsmoltenSalt_Depreciation ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Depreciation_depr_alloc_macrs_15_percent_get(SAM_TcsmoltenSalt_Depreciation ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Depreciation_depr_alloc_macrs_5_percent_get(SAM_TcsmoltenSalt_Depreciation ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Depreciation_depr_alloc_sl_15_percent_get(SAM_TcsmoltenSalt_Depreciation ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Depreciation_depr_alloc_sl_20_percent_get(SAM_TcsmoltenSalt_Depreciation ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Depreciation_depr_alloc_sl_39_percent_get(SAM_TcsmoltenSalt_Depreciation ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Depreciation_depr_alloc_sl_5_percent_get(SAM_TcsmoltenSalt_Depreciation ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a MSPTSingleOwner system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsmoltenSalt_Common SAM_TcsmoltenSalt_Common_create(const char* def, SAM_error* err);


	/**
	 * Set A_sf_in: Solar Field Area
	 * type: numeric
	 * units: m^2
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_A_sf_in_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set N_hel: Number of heliostats
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_N_hel_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set add_om_num_types: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_add_om_num_types_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_adjust:constant_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_adjust:hourly_set(SAM_TcsmoltenSalt_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_adjust:periods_set(SAM_TcsmoltenSalt_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set ampl_data_dir: AMPL data file directory
	 * type: string
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=''
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_ampl_data_dir_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set ampl_exec_call: System command to run AMPL code
	 * type: string
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?='ampl sdk_solution.run'
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_ampl_exec_call_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set annual_fuel_usage: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_annual_fuel_usage_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set batt_bank_replacement: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_batt_bank_replacement_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set batt_computed_bank_capacity: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_batt_computed_bank_capacity_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set batt_meter_position: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_batt_meter_position_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set batt_replacement_option: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_batt_replacement_option_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set batt_replacement_schedule: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_batt_replacement_schedule_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set battery_per_kWh: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_battery_per_kWh_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set calc_fluxmaps: Include fluxmap calculations
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_calc_fluxmaps_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set crossover_shift: No. panels shift in receiver crossover position
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_crossover_shift_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set disp_reporting: Dispatch optimization reporting level
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_disp_reporting_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set disp_spec_bb: Dispatch optimization B&B heuristic
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_disp_spec_bb_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set disp_spec_presolve: Dispatch optimization presolve heuristic
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_disp_spec_presolve_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set disp_spec_scaling: Dispatch optimization scaling heuristic
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_disp_spec_scaling_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set disp_steps_per_hour: Time steps per hour for dispatch optimization calculations
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_disp_steps_per_hour_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set en_batt: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_en_batt_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set en_fuelcell: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_en_fuelcell_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set eta_map: Field efficiency array
	 * type: matrix
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_eta_map_set(SAM_TcsmoltenSalt_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set eta_map_aod_format: Use 3D AOD format field efficiency array-
	 * type: numeric
	 * units: None
	 * options: heliostat
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_eta_map_aod_format_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set flux_maps: Flux map intensities
	 * type: matrix
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_flux_maps_set(SAM_TcsmoltenSalt_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set fuelcell_computed_bank_capacity: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_fuelcell_computed_bank_capacity_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set fuelcell_per_kWh: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_fuelcell_per_kWh_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set fuelcell_replacement: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_fuelcell_replacement_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set fuelcell_replacement_option: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_fuelcell_replacement_option_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set fuelcell_replacement_schedule: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_fuelcell_replacement_schedule_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set gen: Total electric power to grid w/ avail. derate
	 * type: array
	 * units: kWe
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_gen_set(SAM_TcsmoltenSalt_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set grid_to_batt: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_grid_to_batt_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set helio_aim_points: Heliostat aim point table
	 * type: matrix
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_helio_aim_points_set(SAM_TcsmoltenSalt_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set interp_beta: Interpolation beta coef.
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=1.99
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_interp_beta_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set interp_nug: Interpolation nugget
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_interp_nug_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set is_ampl_engine: Run dispatch optimization with external AMPL engine
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_is_ampl_engine_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set is_write_ampl_dat: Write AMPL data files for dispatch run
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_is_write_ampl_dat_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set land_bound_list: Boundary table listing
	 * type: array
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_land_bound_list_set(SAM_TcsmoltenSalt_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set land_bound_table: Land boundary table
	 * type: matrix
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_land_bound_table_set(SAM_TcsmoltenSalt_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set om_capacity1: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_capacity1_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_capacity1_nameplate: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_capacity1_nameplate_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_capacity2: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_capacity2_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_capacity2_nameplate: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_capacity2_nameplate_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_fixed1: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_fixed1_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_fixed2: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_fixed2_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_opt_fuel_1_cost: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_opt_fuel_1_cost_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_opt_fuel_1_cost_escal: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_opt_fuel_1_cost_escal_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_opt_fuel_1_usage: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_opt_fuel_1_usage_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_opt_fuel_2_cost: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_opt_fuel_2_cost_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_opt_fuel_2_cost_escal: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_opt_fuel_2_cost_escal_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_opt_fuel_2_usage: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_opt_fuel_2_usage_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_production1: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_production1_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_production1_values: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_production1_values_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_production2: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_production2_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_production2_values: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_production2_values_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_replacement_cost2: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_replacement_cost2_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set ppa_soln_max: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_ppa_soln_max_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set ppa_soln_max_iterations: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_ppa_soln_max_iterations_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set ppa_soln_min: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_ppa_soln_min_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set ppa_soln_tolerance: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_ppa_soln_tolerance_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set q_rec_heattrace: Receiver heat trace energy consumption during startup
	 * type: numeric
	 * units: kWe-hr
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_q_rec_heattrace_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set q_rec_standby: Receiver standby energy consumption
	 * type: numeric
	 * units: kWt
	 * options: None
	 * constraints: None
	 * required if: ?=9e99
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_q_rec_standby_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set sf_adjust:constant: SF Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_sf_adjust:constant_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set sf_adjust:hourly: SF Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_sf_adjust:hourly_set(SAM_TcsmoltenSalt_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set sf_adjust:periods: SF Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_sf_adjust:periods_set(SAM_TcsmoltenSalt_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set solar_resource_data: solar resouce data in memory
	 * type: table
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_solar_resource_data_set(SAM_TcsmoltenSalt_Common ptr, var_table vt, SAM_error* err);

	/**
	 * Set system_heat_rate: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_system_heat_rate_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set system_lifetime_recapitalize: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_system_lifetime_recapitalize_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set system_recapitalization_cost: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_system_recapitalization_cost_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set system_recapitalization_escalation: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_system_recapitalization_escalation_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set time_steps_per_hour: Number of simulation time steps per hour
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_time_steps_per_hour_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set utility_bill_w_sys: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_utility_bill_w_sys_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set vacuum_arrays: Allocate arrays for only the required number of steps
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_vacuum_arrays_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_A_sf_in_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_N_hel_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_add_om_num_types_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_adjust:constant_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_adjust:hourly_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_adjust:periods_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_ampl_data_dir_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_ampl_exec_call_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_annual_fuel_usage_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_batt_bank_replacement_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_batt_computed_bank_capacity_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_batt_meter_position_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_batt_replacement_option_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_batt_replacement_schedule_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_battery_per_kWh_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_calc_fluxmaps_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_crossover_shift_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_disp_reporting_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_disp_spec_bb_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_disp_spec_presolve_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_disp_spec_scaling_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_disp_steps_per_hour_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_en_batt_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_en_fuelcell_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_eta_map_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_eta_map_aod_format_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_flux_maps_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_fuelcell_computed_bank_capacity_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_fuelcell_per_kWh_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_fuelcell_replacement_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_fuelcell_replacement_option_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_fuelcell_replacement_schedule_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_gen_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_grid_to_batt_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_helio_aim_points_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_interp_beta_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_interp_nug_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_is_ampl_engine_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_is_write_ampl_dat_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_land_bound_list_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_land_bound_table_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_capacity1_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_capacity1_nameplate_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_capacity2_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_capacity2_nameplate_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_fixed1_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_fixed2_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_opt_fuel_1_cost_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_opt_fuel_1_cost_escal_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_opt_fuel_1_usage_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_opt_fuel_2_cost_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_opt_fuel_2_cost_escal_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_opt_fuel_2_usage_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_production1_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_production1_values_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_production2_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_production2_values_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_replacement_cost2_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_ppa_soln_max_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_ppa_soln_max_iterations_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_ppa_soln_min_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_ppa_soln_tolerance_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_q_rec_heattrace_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_q_rec_standby_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_sf_adjust:constant_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_sf_adjust:hourly_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_sf_adjust:periods_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT var_table SAM_TcsmoltenSalt_Common_solar_resource_data_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_system_heat_rate_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_system_lifetime_recapitalize_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_system_recapitalization_cost_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_system_recapitalization_escalation_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_time_steps_per_hour_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_utility_bill_w_sys_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_vacuum_arrays_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a MSPTSingleOwner system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsmoltenSalt_Common SAM_TcsmoltenSalt_Common_create(const char* def, SAM_error* err);


	/**
	 * Set A_sf_in: Solar Field Area
	 * type: numeric
	 * units: m^2
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_A_sf_in_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set N_hel: Number of heliostats
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_N_hel_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set add_om_num_types: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_add_om_num_types_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_adjust:constant_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_adjust:hourly_set(SAM_TcsmoltenSalt_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_adjust:periods_set(SAM_TcsmoltenSalt_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set ampl_data_dir: AMPL data file directory
	 * type: string
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=''
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_ampl_data_dir_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set ampl_exec_call: System command to run AMPL code
	 * type: string
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?='ampl sdk_solution.run'
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_ampl_exec_call_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set annual_fuel_usage: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_annual_fuel_usage_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set batt_bank_replacement: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_batt_bank_replacement_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set batt_computed_bank_capacity: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_batt_computed_bank_capacity_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set batt_meter_position: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_batt_meter_position_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set batt_replacement_option: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_batt_replacement_option_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set batt_replacement_schedule: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_batt_replacement_schedule_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set battery_per_kWh: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_battery_per_kWh_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set calc_fluxmaps: Include fluxmap calculations
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_calc_fluxmaps_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set crossover_shift: No. panels shift in receiver crossover position
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_crossover_shift_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set disp_reporting: Dispatch optimization reporting level
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_disp_reporting_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set disp_spec_bb: Dispatch optimization B&B heuristic
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_disp_spec_bb_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set disp_spec_presolve: Dispatch optimization presolve heuristic
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_disp_spec_presolve_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set disp_spec_scaling: Dispatch optimization scaling heuristic
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_disp_spec_scaling_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set disp_steps_per_hour: Time steps per hour for dispatch optimization calculations
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_disp_steps_per_hour_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set en_batt: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_en_batt_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set en_fuelcell: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_en_fuelcell_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set eta_map: Field efficiency array
	 * type: matrix
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_eta_map_set(SAM_TcsmoltenSalt_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set eta_map_aod_format: Use 3D AOD format field efficiency array-
	 * type: numeric
	 * units: None
	 * options: heliostat
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_eta_map_aod_format_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set flux_maps: Flux map intensities
	 * type: matrix
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_flux_maps_set(SAM_TcsmoltenSalt_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set fuelcell_computed_bank_capacity: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_fuelcell_computed_bank_capacity_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set fuelcell_per_kWh: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_fuelcell_per_kWh_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set fuelcell_replacement: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_fuelcell_replacement_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set fuelcell_replacement_option: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_fuelcell_replacement_option_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set fuelcell_replacement_schedule: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_fuelcell_replacement_schedule_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set gen: Total electric power to grid w/ avail. derate
	 * type: array
	 * units: kWe
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_gen_set(SAM_TcsmoltenSalt_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set grid_to_batt: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_grid_to_batt_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set helio_aim_points: Heliostat aim point table
	 * type: matrix
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_helio_aim_points_set(SAM_TcsmoltenSalt_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set interp_beta: Interpolation beta coef.
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=1.99
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_interp_beta_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set interp_nug: Interpolation nugget
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_interp_nug_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set is_ampl_engine: Run dispatch optimization with external AMPL engine
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_is_ampl_engine_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set is_write_ampl_dat: Write AMPL data files for dispatch run
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_is_write_ampl_dat_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set land_bound_list: Boundary table listing
	 * type: array
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_land_bound_list_set(SAM_TcsmoltenSalt_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set land_bound_table: Land boundary table
	 * type: matrix
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_land_bound_table_set(SAM_TcsmoltenSalt_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set om_capacity1: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_capacity1_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_capacity1_nameplate: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_capacity1_nameplate_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_capacity2: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_capacity2_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_capacity2_nameplate: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_capacity2_nameplate_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_fixed1: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_fixed1_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_fixed2: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_fixed2_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_opt_fuel_1_cost: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_opt_fuel_1_cost_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_opt_fuel_1_cost_escal: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_opt_fuel_1_cost_escal_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_opt_fuel_1_usage: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_opt_fuel_1_usage_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_opt_fuel_2_cost: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_opt_fuel_2_cost_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_opt_fuel_2_cost_escal: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_opt_fuel_2_cost_escal_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_opt_fuel_2_usage: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_opt_fuel_2_usage_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_production1: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_production1_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_production1_values: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_production1_values_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_production2: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_production2_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_production2_values: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_production2_values_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set om_replacement_cost2: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_om_replacement_cost2_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set ppa_soln_max: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_ppa_soln_max_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set ppa_soln_max_iterations: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_ppa_soln_max_iterations_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set ppa_soln_min: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_ppa_soln_min_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set ppa_soln_tolerance: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_ppa_soln_tolerance_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set q_rec_heattrace: Receiver heat trace energy consumption during startup
	 * type: numeric
	 * units: kWe-hr
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_q_rec_heattrace_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set q_rec_standby: Receiver standby energy consumption
	 * type: numeric
	 * units: kWt
	 * options: None
	 * constraints: None
	 * required if: ?=9e99
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_q_rec_standby_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set sf_adjust:constant: SF Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_sf_adjust:constant_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set sf_adjust:hourly: SF Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_sf_adjust:hourly_set(SAM_TcsmoltenSalt_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set sf_adjust:periods: SF Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_sf_adjust:periods_set(SAM_TcsmoltenSalt_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set solar_resource_data: solar resouce data in memory
	 * type: table
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_solar_resource_data_set(SAM_TcsmoltenSalt_Common ptr, var_table vt, SAM_error* err);

	/**
	 * Set system_heat_rate: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_system_heat_rate_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set system_lifetime_recapitalize: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_system_lifetime_recapitalize_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set system_recapitalization_cost: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_system_recapitalization_cost_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set system_recapitalization_escalation: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_system_recapitalization_escalation_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set time_steps_per_hour: Number of simulation time steps per hour
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_time_steps_per_hour_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);

	/**
	 * Set utility_bill_w_sys: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_utility_bill_w_sys_set(SAM_TcsmoltenSalt_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set vacuum_arrays: Allocate arrays for only the required number of steps
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_Common_vacuum_arrays_set(SAM_TcsmoltenSalt_Common ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_A_sf_in_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_N_hel_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_add_om_num_types_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_adjust:constant_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_adjust:hourly_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_adjust:periods_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_ampl_data_dir_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_ampl_exec_call_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_annual_fuel_usage_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_batt_bank_replacement_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_batt_computed_bank_capacity_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_batt_meter_position_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_batt_replacement_option_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_batt_replacement_schedule_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_battery_per_kWh_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_calc_fluxmaps_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_crossover_shift_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_disp_reporting_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_disp_spec_bb_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_disp_spec_presolve_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_disp_spec_scaling_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_disp_steps_per_hour_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_en_batt_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_en_fuelcell_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_eta_map_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_eta_map_aod_format_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_flux_maps_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_fuelcell_computed_bank_capacity_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_fuelcell_per_kWh_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_fuelcell_replacement_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_fuelcell_replacement_option_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_fuelcell_replacement_schedule_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_gen_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_grid_to_batt_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_helio_aim_points_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_interp_beta_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_interp_nug_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_is_ampl_engine_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_is_write_ampl_dat_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_land_bound_list_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_land_bound_table_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_capacity1_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_capacity1_nameplate_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_capacity2_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_capacity2_nameplate_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_fixed1_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_fixed2_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_opt_fuel_1_cost_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_opt_fuel_1_cost_escal_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_opt_fuel_1_usage_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_opt_fuel_2_cost_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_opt_fuel_2_cost_escal_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_opt_fuel_2_usage_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_production1_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_production1_values_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_production2_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_production2_values_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_om_replacement_cost2_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_ppa_soln_max_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_ppa_soln_max_iterations_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_ppa_soln_min_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_ppa_soln_tolerance_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_q_rec_heattrace_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_q_rec_standby_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_sf_adjust:constant_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_sf_adjust:hourly_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsmoltenSalt_Common_sf_adjust:periods_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT var_table SAM_TcsmoltenSalt_Common_solar_resource_data_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_system_heat_rate_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_system_lifetime_recapitalize_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_system_recapitalization_cost_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_system_recapitalization_escalation_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_time_steps_per_hour_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_Common_utility_bill_w_sys_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsmoltenSalt_Common_vacuum_arrays_get(SAM_TcsmoltenSalt_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
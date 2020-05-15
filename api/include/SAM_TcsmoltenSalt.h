#ifndef SAM_TCSMOLTENSALT_H_
#define SAM_TCSMOLTENSALT_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// TcsmoltenSalt Technology Model
	//

	/** 
	 * Create a TcsmoltenSalt variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_TcsmoltenSalt;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_TcsmoltenSalt_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SolarResource parameters
	//

	/**
	 * Set solar_resource_data: Weather resource data in memory
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SolarResource_solar_resource_data_tset(SAM_table ptr, SAM_table tab, SAM_error *err);

	/**
	 * Set solar_resource_file: Local weather file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SolarResource_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err);


	//
	// TimeOfDeliveryFactors parameters
	//

	/**
	 * Set dispatch_factor1: Dispatch payment factor 1
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor2: Dispatch payment factor 2
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor3: Dispatch payment factor 3
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor4: Dispatch payment factor 4
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor5: Dispatch payment factor 5
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor6: Dispatch payment factor 6
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor6_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor7: Dispatch payment factor 7
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor7_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor8: Dispatch payment factor 8
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor8_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor9: Dispatch payment factor 9
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor9_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factors_ts: Dispatch payment factor array
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_sched_weekday: PPA pricing weekday schedule, 12x24
	 * options: None
	 * constraints: None
	 * required if: ?=[[1]]
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_sched_weekend: PPA pricing weekend schedule, 12x24
	 * options: None
	 * constraints: None
	 * required if: ?=[[1]]
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ppa_multiplier_model: PPA multiplier model [0/1]
	 * options: 0=diurnal,1=timestep
	 * constraints: INTEGER,MIN=0
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TimeOfDeliveryFactors_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// HeliostatField parameters
	//

	/**
	 * Set A_sf_in: Solar field area [m^2]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_A_sf_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set N_hel: Number of heliostats
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_N_hel_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set c_atm_0: Attenuation coefficient 0
	 * options: None
	 * constraints: None
	 * required if: ?=0.006789
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_c_atm_0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set c_atm_1: Attenuation coefficient 1
	 * options: None
	 * constraints: None
	 * required if: ?=0.1046
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_c_atm_1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set c_atm_2: Attenuation coefficient 2
	 * options: None
	 * constraints: None
	 * required if: ?=-0.0107
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_c_atm_2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set c_atm_3: Attenuation coefficient 3
	 * options: None
	 * constraints: None
	 * required if: ?=0.002845
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_c_atm_3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set calc_fluxmaps: Include fluxmap calculations
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_calc_fluxmaps_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cant_type: Heliostat canting method
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_cant_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set check_max_flux: Check max flux at design point
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_check_max_flux_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.pt.sf.fixed_land_area: Fixed land area [acre]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_csp_pt_sf_fixed_land_area_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.pt.sf.land_overhead_factor: Land overhead factor
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_csp_pt_sf_land_overhead_factor_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dens_mirror: Ratio of heliostat reflective area to profile
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_dens_mirror_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_map: Field efficiency array
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_eta_map_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set eta_map_aod_format: Use 3D AOD format field efficiency array
	 * options: heliostat
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_eta_map_aod_format_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set field_model_type: 0=design field and tower/receiver geometry, 1=design field, 2=user specified field, 3=user performance maps vs solar position
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_field_model_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set flux_maps: Flux map intensities
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_flux_maps_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set focus_type: Heliostat focus method
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_focus_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hel_stow_deploy: Stow/deploy elevation angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_hel_stow_deploy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set helio_active_fraction: Heliostat active fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_active_fraction_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set helio_aim_points: Heliostat aim point table [m]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_aim_points_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set helio_height: Heliostat height [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_height_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set helio_optical_error_mrad: Heliostat optical error [mrad]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_optical_error_mrad_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set helio_positions: Heliostat position table
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_positions_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set helio_reflectance: Heliostat reflectance
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_reflectance_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set helio_width: Heliostat width [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_helio_width_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set interp_beta: Interpolation beta coef. [-]
	 * options: None
	 * constraints: None
	 * required if: ?=1.99
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_interp_beta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set interp_nug: Interpolation nugget [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_interp_nug_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_area_base: Base land area occupied by heliostats [acre]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_land_area_base_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_bound_list: Land boundary table listing
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_land_bound_list_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set land_bound_table: Land boundary table [m]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_land_bound_table_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set land_max: Land max boundary [-ORm]
	 * options: None
	 * constraints: None
	 * required if: ?=7.5
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_land_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_min: Land min boundary [-ORm]
	 * options: None
	 * constraints: None
	 * required if: ?=0.75
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_land_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_facet_x: Number of heliostat facets - X
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_n_facet_x_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_facet_y: Number of heliostat facets - Y
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_n_facet_y_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set opt_algorithm: Optimization algorithm
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_opt_algorithm_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set opt_conv_tol: Optimization convergence tolerance
	 * options: None
	 * constraints: None
	 * required if: ?=0.001
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_opt_conv_tol_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set opt_flux_penalty: Optimization flux overage penalty
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_opt_flux_penalty_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set opt_init_step: Optimization initial step size
	 * options: None
	 * constraints: None
	 * required if: ?=0.05
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_opt_init_step_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set opt_max_iter: Max number iteration steps
	 * options: None
	 * constraints: None
	 * required if: ?=200
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_opt_max_iter_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set p_start: Heliostat startup energy [kWe-hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_p_start_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set p_track: Heliostat tracking energy [kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_p_track_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set v_wind_max: Heliostat max wind velocity [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_v_wind_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set washing_frequency: Mirror washing frequency [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_washing_frequency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set water_usage_per_wash: Water usage per wash [L/m2_aper]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_HeliostatField_water_usage_per_wash_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SystemDesign parameters
	//

	/**
	 * Set P_ref: Reference output electric power at design condition [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_P_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_cold_des: Cold HTF inlet temperature at design conditions [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_T_htf_cold_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_hot_des: Hot HTF outlet temperature at design conditions [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set design_eff: Power cycle efficiency at design [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_design_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dni_des: Design-point DNI [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_dni_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set gross_net_conversion_factor: Estimated gross to net conversion factor
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_gross_net_conversion_factor_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sf_excess: Heliostat field multiple
	 * options: None
	 * constraints: None
	 * required if: ?=1.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_sf_excess_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set solarm: Solar multiple [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_solarm_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tshours: Equivalent full-load thermal storage hours [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemDesign_tshours_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// TowerAndReceiver parameters
	//

	/**
	 * Set D_rec: The overall outer diameter of the receiver [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_D_rec_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Flow_type: Receiver flow pattern: see figure on SAM Receiver page
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_Flow_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set N_panels: Number of individual panels on the receiver
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_N_panels_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set crossover_shift: Number of panels shift in receiver crossover position
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_crossover_shift_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.pt.rec.max_oper_frac: Maximum receiver mass flow rate fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_csp_pt_rec_max_oper_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set d_tube_out: The outer diameter of an individual receiver tube [mm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_d_tube_out_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set delta_flux_hrs: Hourly frequency in flux map lookup
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_delta_flux_hrs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set downc_tm_mult: Downcomer thermal mass multiplier
	 * options: None
	 * constraints: None
	 * required if: ?=1.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_downc_tm_mult_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epsilon: The emissivity of the receiver surface coating
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_epsilon_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_pump: Receiver HTF pump efficiency
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_eta_pump_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set f_rec_min: Minimum receiver mass flow rate turn down fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_f_rec_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set field_fl_props: User defined field fluid property data [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set flux_max: Maximum allowable flux
	 * options: None
	 * constraints: None
	 * required if: ?=1000
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_flux_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tower: Tower height [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_h_tower_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set heat_trace_power: Riser/downcomer heat trace power during startup [kW/m]
	 * options: None
	 * constraints: None
	 * required if: ?=500.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_heat_trace_power_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hl_ffact: The heat loss factor (thermal loss fudge factor)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_hl_ffact_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_rec_enforce_min_startup: Always enforce minimum startup time
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_enforce_min_startup_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_rec_model_trans: Formulate receiver model as transient?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_model_trans_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_rec_startup_from_T_soln: Begin receiver startup from solved temperature profiles?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_startup_from_T_soln_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_rec_startup_trans: Formulate receiver startup model as transient?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_startup_trans_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mat_tube: Receiver tube material, 2=Stainless AISI316
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_mat_tube_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_flux_days: Number of days in flux map lookup
	 * options: None
	 * constraints: None
	 * required if: ?=8
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_n_flux_days_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set piping_length_const: Piping constant length [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_piping_length_const_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set piping_length_mult: Piping length multiplier
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_piping_length_mult_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set piping_loss: Thermal loss per meter of piping [Wt/m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_piping_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set piping_loss_coeff: Wetted loss coefficient for riser or downcomer [W/m2/K]
	 * options: None
	 * constraints: None
	 * required if: ?=5.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_piping_loss_coeff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set preheat_flux: Tube absorbed solar flux during preheat [kW/m2]
	 * options: None
	 * constraints: None
	 * required if: ?=50.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_preheat_flux_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set preheat_target_Tdiff: Target tube T at end of preheat - design pt cold HTF temperature [C]
	 * options: None
	 * constraints: None
	 * required if: ?=25.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_preheat_target_Tdiff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_absorptance: Receiver absorptance
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_absorptance_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_height: Receiver height [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_height_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_hl_perm2: Receiver design heatloss [kW/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_hl_perm2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_htf: Receiver HTF, 17=Salt (60% NaNO3, 40% KNO3) 10=Salt (46.5% LiF 11.5% NaF 42% KF) 50=Lookup tables
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_htf_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_qf_delay: Energy-based receiver startup delay (fraction of rated thermal power)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_qf_delay_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_su_delay: Fixed startup delay time for the receiver [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_su_delay_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_tm_mult: Receiver thermal mass multiplier
	 * options: None
	 * constraints: None
	 * required if: ?=1.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_rec_tm_mult_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set riser_tm_mult: Riser thermal mass multiplier
	 * options: None
	 * constraints: None
	 * required if: ?=1.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_riser_tm_mult_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set startup_ramp_time: Time required to reach full flux during receiver startup [hr]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_startup_ramp_time_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set startup_target_Tdiff: Target HTF T at end of startup - design pt hot HTF temperature [C]
	 * options: None
	 * constraints: None
	 * required if: ?=-5.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_startup_target_Tdiff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set th_riser: Riser or downcomer tube wall thickness [mm]
	 * options: None
	 * constraints: None
	 * required if: ?=15.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_th_riser_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set th_tube: The wall thickness of a single receiver tube [mm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_th_tube_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set u_riser: Design point HTF velocity in riser [m/s]
	 * options: None
	 * constraints: None
	 * required if: ?=4.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_TowerAndReceiver_u_riser_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SystemCosts parameters
	//

	/**
	 * Set bop_spec_cost: BOS specific cost [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_bop_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set contingency_rate: Contingency for cost overrun [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_contingency_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cost_sf_fixed: Solar field fixed cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_cost_sf_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.pt.cost.epc.fixed: EPC fixed [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.pt.cost.epc.per_acre: EPC cost per acre [$/acre]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_per_acre_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.pt.cost.epc.per_watt: EPC cost per watt [$/W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_per_watt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.pt.cost.epc.percent: EPC cost percent of direct [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.pt.cost.plm.fixed: PLM fixed [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_plm_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.pt.cost.plm.per_watt: PLM cost per watt [$/W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_plm_per_watt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.pt.cost.plm.percent: PLM cost percent of direct [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_plm_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fossil_spec_cost: Fossil system specific cost [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_fossil_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set heliostat_spec_cost: Heliostat field cost [$/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_heliostat_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_spec_cost: Total land area cost [$/acre]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_land_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set plant_spec_cost: Power cycle specific cost [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_plant_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_cost_exp: Receiver cost scaling exponent
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_rec_cost_exp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_ref_area: Receiver reference area for cost scale
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_rec_ref_area_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_ref_cost: Receiver reference cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_rec_ref_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sales_tax_frac: Percent of cost to which sales tax applies [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_sales_tax_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set site_spec_cost: Site improvement cost [$/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_site_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_spec_cost: Thermal energy storage cost [$/kWht]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_tes_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tower_exp: Tower cost scaling exponent
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_tower_exp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tower_fixed_cost: Tower fixed cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemCosts_tower_fixed_cost_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// FinancialParameters parameters
	//

	/**
	 * Set const_per_interest_rate1: Interest rate, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate2: Interest rate, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate3: Interest rate, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate4: Interest rate, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate5: Interest rate, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months1: Months prior to operation, loan 1
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months2: Months prior to operation, loan 2
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months3: Months prior to operation, loan 3
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months4: Months prior to operation, loan 4
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months5: Months prior to operation, loan 5
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent1: Percent of total installed cost, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent2: Percent of total installed cost, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent3: Percent of total installed cost, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent4: Percent of total installed cost, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent5: Percent of total installed cost, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate1: Upfront fee on principal, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate2: Upfront fee on principal, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate3: Upfront fee on principal, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate4: Upfront fee on principal, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate5: Upfront fee on principal, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sales_tax_rate: Sales tax rate [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_FinancialParameters_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// ThermalStorage parameters
	//

	/**
	 * Set cold_tank_Thtr: Minimum allowable cold tank HTF temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cold_tank_max_heat: Rated heater capacity for cold tank heating [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.pt.tes.init_hot_htf_percent: Initial fraction of available volume that is hot [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_csp_pt_tes_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank: Total height of tank (height of HTF when tank is full) [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_h_tank_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank_min: Minimum allowable HTF height in storage tank [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_Thtr: Minimum allowable hot tank HTF temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_max_heat: Rated heater capacity for hot tank heating [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tank_pairs: Number of equivalent tank pairs
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tanks_in_parallel: Tanks are in parallel, not in series, with solar field [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_tanks_in_parallel_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set u_tank: Loss coefficient from the tank [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_ThermalStorage_u_tank_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// RADCOOL parameters
	//

	/**
	 * Set D_rad_tubes: Inner diameter of tubes in radiator panel [m]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_D_rad_tubes_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set L_rad: Length of radiator panel row [m]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_L_rad_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set L_rad_sections: Length of individual radiator panel [m]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_L_rad_sections_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_ctes_cold_design: Design value of cooled water to power block [C]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_T_ctes_cold_design_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_ctes_cold_ini: Initial value of cold tank [C]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_T_ctes_cold_ini_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_ctes_warm_design: Design value of warm water returning from power block [C]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_T_ctes_warm_design_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_ctes_warm_ini: Initial value of warm tank [C]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_T_ctes_warm_ini_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set W_rad_tubes: Center-to-center distance between tubes in radiator panel [m]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_W_rad_tubes_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ctes_cost: Cost of cold storage construction [$/L]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_ctes_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ctes_field_fl: Fluid in radiator field. 3=liquid water. Other = Glycol. [-]
	 * options: None
	 * constraints: None
	 * required if: ?=3
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_ctes_field_fl_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ctes_tankpairs: Number of equivalent tank pairs [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_ctes_tankpairs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ctes_tshours: Equivalent full load storage hours [hr]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_ctes_tshours_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ctes_type: Type of cold storage (2=two tank, 3= three node) [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_ctes_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epsilon_radHX: Effectiveness of HX between radiative field and cold storage [-]
	 * options: None
	 * constraints: None
	 * required if: ?=.8
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_epsilon_radHX_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epsilon_radbot: Emmissivity of top of radiator panel bottom (facing ground) [-]
	 * options: None
	 * constraints: None
	 * required if: ?=.07
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_epsilon_radbot_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epsilon_radgrnd: Emmissivity of ground underneath radiator panel [-]
	 * options: None
	 * constraints: None
	 * required if: ?=.90
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_epsilon_radgrnd_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epsilon_radtop: Emmissivity of top of radiator panel [-]
	 * options: None
	 * constraints: None
	 * required if: ?=.95
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_epsilon_radtop_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set f_ctes_warm_ini: Initial fraction of avail. volume that is warm [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_f_ctes_warm_ini_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_ctes_tank: Total height of cold storage tank when full [m]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_h_ctes_tank_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_ctes_tank_min: Minimum allowable water height in storage tank [m]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_h_ctes_tank_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set helio_area_tot: Heliostat total reflective area [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_helio_area_tot_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set k_panel: Thermal conductivity of radiator panel material [W/m-K]
	 * options: None
	 * constraints: None
	 * required if: ?=235
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_k_panel_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set m_dot_radpanel: Mass flow rate through single radiator panel [kg/sec]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_m_dot_radpanel_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_rad_tubes: Number of parallel tubes in single radiator panel [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_n_rad_tubes_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rad_multiplier: Ratio of radiator field area to solar aperature area [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_rad_multiplier_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rad_pressuredrop: Average pressure drop through a radiative panel & distribution [kPa]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_rad_pressuredrop_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set radfluid_vol_ratio: Ratio of fluid in distribution to fluid in panels [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_radfluid_vol_ratio_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set radiator_fluidcost: Cost of circulating fluid in radiative panels [$/L]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_radiator_fluidcost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set radiator_installcost: Installation cost of radiative panels [$/m^2]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_radiator_installcost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set radiator_unitcost: Cost of radiative panels [$/m^2]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_radiator_unitcost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set th_rad_panel: Thickness of radiator panel [m]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_th_rad_panel_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set u_ctes_tank: Loss coefficient from cold storage tank [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RADCOOL_u_ctes_tank_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// PowerCycle parameters
	//

	/**
	 * Set cycle_cutoff_frac: Minimum turbine operation fraction before shutdown
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_PowerCycle_cycle_cutoff_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cycle_max_frac: Maximum turbine over design operation fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_PowerCycle_cycle_max_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_pump_coef: Pumping power to move 1kg of HTF through PB loop [kW/kg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_PowerCycle_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pc_config: PC configuration 0=Steam Rankine (224), 1=user defined, 2=sCO2 Recompression (424)
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_PowerCycle_pc_config_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_sby_frac: Fraction of thermal power required for standby
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_PowerCycle_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set startup_frac: Fraction of design thermal power needed for startup [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_PowerCycle_startup_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set startup_time: Time needed for power block startup [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_PowerCycle_startup_time_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// RankineCycle parameters
	//

	/**
	 * Set CT: Condensor type: 1=evaporative, 2=air, 3=hybrid
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_CT_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_boil: Boiler operating pressure [bar]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_P_boil_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_cond_min: Minimum condenser pressure [inHg]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_P_cond_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_cond_ratio: Condenser pressure ratio
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_P_cond_ratio_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_ITD_des: ITD at design for dry system [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_amb_des: Reference ambient temperature at design point [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_approach: Cooling tower approach temperature [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_T_approach_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dT_cw_ref: Reference condenser cooling water inlet/outlet temperature difference [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_dT_cw_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_pl_inc: Number of part-load increments for the heat rejection system [none]
	 * options: None
	 * constraints: INTEGER
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_n_pl_inc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_bd_frac: Power block blowdown steam fraction
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_pb_bd_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tech_type: Turbine inlet pressure control 1=Fixed, 3=Sliding
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_RankineCycle_tech_type_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SystemControl parameters
	//

	/**
	 * Set F_wc: TOU array of fractions indicating wet cooling share for hybrid cooling
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_F_wc_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ampl_data_dir: AMPL data file directory
	 * options: None
	 * constraints: None
	 * required if: ?=''
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_ampl_data_dir_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set ampl_exec_call: System command to run AMPL code
	 * options: None
	 * constraints: None
	 * required if: ?='ampl sdk_solution.run'
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_ampl_exec_call_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set aux_par: Aux heater, boiler parasitic [MWe/MWcap]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_aux_par_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set aux_par_0: Aux heater, boiler parasitic - constant coefficient
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_aux_par_0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set aux_par_1: Aux heater, boiler parasitic - linear coefficient
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_aux_par_1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set aux_par_2: Aux heater, boiler parasitic - quadratic coefficient
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_aux_par_2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set aux_par_f: Aux heater, boiler parasitic - multiplying fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_aux_par_f_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par: Balance of plant parasitic power fraction [MWe/MWcap]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_bop_par_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_0: Balance of plant parasitic power fraction - const coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_bop_par_0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_1: Balance of plant parasitic power fraction - linear coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_bop_par_1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_2: Balance of plant parasitic power fraction - quadratic coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_bop_par_2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_f: Balance of plant parasitic power fraction - mult frac
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_bop_par_f_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_csu_cost: Cycle startup cost [$]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_csu_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_frequency: Frequency for dispatch optimization calculations [hour]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_frequency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_horizon: Time horizon for dispatch optimization [hour]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_horizon_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_inventory_incentive: Dispatch storage terminal inventory incentive multiplier
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_inventory_incentive_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_max_iter: Max number of dispatch optimization iterations
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_max_iter_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_mip_gap: Dispatch optimization solution tolerance
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_mip_gap_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_pen_delta_w: Dispatch cycle production change penalty [$/kWe-change]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_pen_delta_w_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_reporting: Dispatch optimization reporting level
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_reporting_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_rsu_cost: Receiver startup cost [$]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_rsu_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_bb: Dispatch optimization B&B heuristic
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_spec_bb_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_presolve: Dispatch optimization presolve heuristic
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_spec_presolve_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_scaling: Dispatch optimization scaling heuristic
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_spec_scaling_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_steps_per_hour: Time steps per hour for dispatch optimization calculations
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_time_weighting: Dispatch optimization future time discounting factor
	 * options: None
	 * constraints: None
	 * required if: ?=0.99
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_time_weighting_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_timeout: Max dispatch optimization solve duration [s]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_disp_timeout_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_series: Time series dispatch factors
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_dispatch_series_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set f_turb_tou_periods: Dispatch logic for turbine load fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_f_turb_tou_periods_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set is_ampl_engine: Run dispatch optimization with external AMPL engine
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_is_ampl_engine_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_dispatch: Allow dispatch optimization?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_is_dispatch_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_dispatch_series: Use time-series dispatch factors
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_is_dispatch_series_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_tod_pc_target_also_pc_max: Is the TOD target cycle heat input also the max cycle heat input?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_is_tod_pc_target_also_pc_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_wlim_series: Use time-series net electricity generation limits
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_is_wlim_series_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_write_ampl_dat: Write AMPL data files for dispatch run
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_is_write_ampl_dat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_fixed_par: Fixed parasitic load - runs at all times [MWe/MWcap]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_rec_heattrace: Receiver heat trace energy consumption during startup [kWe-hr]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_q_rec_heattrace_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_rec_standby: Receiver standby energy consumption [kWt]
	 * options: None
	 * constraints: None
	 * required if: ?=9e99
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_q_rec_standby_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set time_start: Simulation start time [s]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_time_start_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set time_steps_per_hour: Number of simulation time steps per hour
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_time_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set time_stop: Simulation stop time [s]
	 * options: None
	 * constraints: None
	 * required if: ?=31536000
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_time_stop_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set vacuum_arrays: Allocate arrays for only the required number of steps
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_vacuum_arrays_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set weekday_schedule: 12x24 CSP operation Time-of-Use Weekday schedule
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set weekend_schedule: 12x24 CSP operation Time-of-Use Weekend schedule
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set wlim_series: Time series net electicity generation limits [kWe]
	 * options: None
	 * constraints: None
	 * required if: is_wlim_series=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SystemControl_wlim_series_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// UserDefinedPowerCycle parameters
	//

	/**
	 * Set ud_f_W_dot_cool_des: Percent of user-defined power cycle design gross output consumed by cooling [%]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_f_W_dot_cool_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ud_ind_od: Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ud_m_dot_water_cool_des: Mass flow rate of water required at user-defined power cycle design point [kg/s]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_m_dot_water_cool_des_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SCO2Cycle parameters
	//

	/**
	 * Set P_high_limit: Upper pressure limit in cycle [MPa]
	 * options: None
	 * constraints: None
	 * required if: pc_config=2
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_P_high_limit_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set _sco2_P_high_limit: Preprocess input: upper pressure limit [MPa]
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_P_high_limit_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set _sco2_P_ref: Preprocess input: gross power output [MWe]
	 * options: sco2_pc_pre
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_P_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set _sco2_T_amb_des: Preprocess input: design ambient temperature [C]
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set _sco2_T_approach: Preprocess input: compressor approach temperature [C]
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_T_approach_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set _sco2_T_htf_hot_des: Preprocess input: HTF hot temperature [C]
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set _sco2_deltaT_PHX: Preprocess input: PHX approach temperature [C]
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_deltaT_PHX_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set _sco2_design_eff: Preprocess input: cycle thermal efficiency
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_design_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set _sco2_eta_c: Preprocess input: compressor isentropic efficiency
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_eta_c_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set _sco2_eta_t: Preprocess input: turbine isentropic efficiency
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_eta_t_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set _sco2_recup_eff_max: Preprocess input: max recuperator effectiveness
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle__sco2_recup_eff_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set deltaT_PHX: Design temperature difference in PHX [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=2
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_deltaT_PHX_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_c: Isentropic efficiency of compressor(s)
	 * options: None
	 * constraints: None
	 * required if: pc_config=2
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_eta_c_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_t: Isentropic efficiency of turbine
	 * options: None
	 * constraints: None
	 * required if: pc_config=2
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_eta_t_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fan_power_perc_net: Percent of net cycle output used for fan power at design [%]
	 * options: None
	 * constraints: None
	 * required if: pc_config=2
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_fan_power_perc_net_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_sco2_preprocess: Is sco2 off-design performance preprocessed? 1=yes
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_is_sco2_preprocess_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set recup_eff_max: Maximum recuperator effectiveness
	 * options: None
	 * constraints: None
	 * required if: pc_config=2
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_recup_eff_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sco2_T_amb_des: Ambient temperature at design point [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=2
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sco2_T_approach: Temperature difference between main compressor CO2 inlet and ambient air [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=2
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2_T_approach_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sco2_cycle_config: SCO2 cycle configuration, 1=recompression, 2=partial cooling
	 * options: None
	 * constraints: None
	 * required if: pc_config=2
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2_cycle_config_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sco2ud_T_amb_high: High level ambient temperature for HTF mass flow rate parametric [C]
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_amb_high_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sco2ud_T_amb_ind_od: Off design table of user-defined power cycle performance formed from parametric on T_amb [C]
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_amb_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sco2ud_T_amb_low: Low level ambient temperature for HTF mass flow rate parametric [C]
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_amb_low_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sco2ud_T_htf_cold_calc: HTF cold temperature from sCO2 cycle des, may be different than T_htf_cold_des [C]
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_cold_calc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sco2ud_T_htf_high: High level HTF inlet temperature for T_amb parametric [C]
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_high_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sco2ud_T_htf_ind_od: Off design table of user-defined power cycle performance formed from parametric on T_htf_hot [C]
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sco2ud_T_htf_low: Low level HTF inlet temperature for T_amb parametric [C]
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_low_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sco2ud_m_dot_htf_high: High level normalized HTF mass flow rate for T_HTF parametric
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_m_dot_htf_high_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sco2ud_m_dot_htf_ind_od: Off design table of user-defined power cycle performance formed from parametric on m_dot_htf [ND]
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_m_dot_htf_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sco2ud_m_dot_htf_low: Low level normalized HTF mass flow rate for T_HTF parametric
	 * options: None
	 * constraints: None
	 * required if: is_sco2_preprocess=1
	 */
	SAM_EXPORT void SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_m_dot_htf_low_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * SolarResource Getters
	 */

	SAM_EXPORT SAM_table SAM_TcsmoltenSalt_SolarResource_solar_resource_data_tget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_SolarResource_solar_resource_file_sget(SAM_table ptr, SAM_error *err);


	/**
	 * TimeOfDeliveryFactors Getters
	 */

	SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor6_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor7_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor8_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factor9_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_TimeOfDeliveryFactors_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TimeOfDeliveryFactors_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err);


	/**
	 * HeliostatField Getters
	 */

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_A_sf_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_N_hel_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_c_atm_0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_c_atm_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_c_atm_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_c_atm_3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_calc_fluxmaps_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_cant_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_check_max_flux_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_csp_pt_sf_fixed_land_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_csp_pt_sf_land_overhead_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_dens_mirror_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_HeliostatField_eta_map_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_eta_map_aod_format_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_field_model_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_HeliostatField_flux_maps_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_focus_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_hel_stow_deploy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_helio_active_fraction_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_HeliostatField_helio_aim_points_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_helio_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_helio_optical_error_mrad_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_HeliostatField_helio_positions_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_helio_reflectance_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_helio_width_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_interp_beta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_interp_nug_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_land_area_base_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_HeliostatField_land_bound_list_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_HeliostatField_land_bound_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_land_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_land_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_n_facet_x_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_n_facet_y_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_opt_algorithm_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_opt_conv_tol_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_opt_flux_penalty_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_opt_init_step_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_opt_max_iter_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_p_start_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_p_track_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_v_wind_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_washing_frequency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_HeliostatField_water_usage_per_wash_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemDesign Getters
	 */

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_P_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_design_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_dni_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_gross_net_conversion_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_sf_excess_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_solarm_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemDesign_tshours_nget(SAM_table ptr, SAM_error *err);


	/**
	 * TowerAndReceiver Getters
	 */

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_D_rec_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_Flow_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_N_panels_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_crossover_shift_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_csp_pt_rec_max_oper_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_d_tube_out_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_delta_flux_hrs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_downc_tm_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_epsilon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_eta_pump_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_f_rec_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_TowerAndReceiver_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_flux_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_h_tower_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_heat_trace_power_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_hl_ffact_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_enforce_min_startup_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_model_trans_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_startup_from_T_soln_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_is_rec_startup_trans_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_mat_tube_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_n_flux_days_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_piping_length_const_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_piping_length_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_piping_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_piping_loss_coeff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_preheat_flux_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_preheat_target_Tdiff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_absorptance_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_hl_perm2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_htf_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_qf_delay_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_su_delay_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_rec_tm_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_riser_tm_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_startup_ramp_time_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_startup_target_Tdiff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_th_riser_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_th_tube_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_TowerAndReceiver_u_riser_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemCosts Getters
	 */

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_bop_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_contingency_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_cost_sf_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_per_acre_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_per_watt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_epc_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_plm_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_plm_per_watt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_csp_pt_cost_plm_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_fossil_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_heliostat_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_land_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_plant_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_rec_cost_exp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_rec_ref_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_rec_ref_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_sales_tax_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_site_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_tes_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_tower_exp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemCosts_tower_fixed_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * FinancialParameters Getters
	 */

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_FinancialParameters_sales_tax_rate_nget(SAM_table ptr, SAM_error *err);


	/**
	 * ThermalStorage Getters
	 */

	SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_csp_pt_tes_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_h_tank_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_h_tank_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_tank_pairs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_tanks_in_parallel_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_ThermalStorage_u_tank_nget(SAM_table ptr, SAM_error *err);


	/**
	 * RADCOOL Getters
	 */

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_D_rad_tubes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_L_rad_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_L_rad_sections_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_T_ctes_cold_design_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_T_ctes_cold_ini_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_T_ctes_warm_design_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_T_ctes_warm_ini_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_W_rad_tubes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_ctes_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_ctes_field_fl_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_ctes_tankpairs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_ctes_tshours_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_ctes_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_epsilon_radHX_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_epsilon_radbot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_epsilon_radgrnd_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_epsilon_radtop_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_f_ctes_warm_ini_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_h_ctes_tank_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_h_ctes_tank_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_helio_area_tot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_k_panel_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_m_dot_radpanel_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_n_rad_tubes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_rad_multiplier_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_rad_pressuredrop_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_radfluid_vol_ratio_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_radiator_fluidcost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_radiator_installcost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_radiator_unitcost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_th_rad_panel_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RADCOOL_u_ctes_tank_nget(SAM_table ptr, SAM_error *err);


	/**
	 * PowerCycle Getters
	 */

	SAM_EXPORT double SAM_TcsmoltenSalt_PowerCycle_cycle_cutoff_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_PowerCycle_cycle_max_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_PowerCycle_pb_pump_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_PowerCycle_pc_config_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_PowerCycle_q_sby_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_PowerCycle_startup_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_PowerCycle_startup_time_nget(SAM_table ptr, SAM_error *err);


	/**
	 * RankineCycle Getters
	 */

	SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_CT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_P_boil_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_P_cond_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_P_cond_ratio_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_T_ITD_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_T_amb_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_T_approach_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_dT_cw_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_n_pl_inc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_pb_bd_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_RankineCycle_tech_type_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemControl Getters
	 */

	SAM_EXPORT double* SAM_TcsmoltenSalt_SystemControl_F_wc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_SystemControl_ampl_data_dir_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_TcsmoltenSalt_SystemControl_ampl_exec_call_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_aux_par_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_aux_par_0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_aux_par_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_aux_par_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_aux_par_f_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_bop_par_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_bop_par_0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_bop_par_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_bop_par_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_bop_par_f_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_csu_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_frequency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_horizon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_inventory_incentive_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_max_iter_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_mip_gap_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_pen_delta_w_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_reporting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_rsu_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_spec_bb_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_spec_presolve_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_spec_scaling_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_steps_per_hour_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_time_weighting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_disp_timeout_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_SystemControl_dispatch_series_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_SystemControl_f_turb_tou_periods_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_is_ampl_engine_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_is_dispatch_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_is_dispatch_series_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_is_tod_pc_target_also_pc_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_is_wlim_series_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_is_write_ampl_dat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_pb_fixed_par_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_q_rec_heattrace_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_q_rec_standby_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_time_start_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_time_steps_per_hour_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_time_stop_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SystemControl_vacuum_arrays_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_SystemControl_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_SystemControl_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_SystemControl_wlim_series_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * UserDefinedPowerCycle Getters
	 */

	SAM_EXPORT double SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_f_W_dot_cool_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_UserDefinedPowerCycle_ud_m_dot_water_cool_des_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SCO2Cycle Getters
	 */

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_P_high_limit_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_P_high_limit_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_P_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_T_amb_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_T_approach_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_deltaT_PHX_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_design_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_eta_c_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_eta_t_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle__sco2_recup_eff_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_deltaT_PHX_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_eta_c_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_eta_t_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_fan_power_perc_net_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_is_sco2_preprocess_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_recup_eff_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2_T_amb_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2_T_approach_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2_cycle_config_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_amb_high_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_amb_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_amb_low_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_cold_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_high_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_T_htf_low_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_m_dot_htf_high_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_m_dot_htf_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_SCO2Cycle_sco2ud_m_dot_htf_low_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_A_radfield_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_A_sf_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_cond_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_cooling_tower_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_out_net_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_plant_balance_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_rec_heattrace_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_P_tower_pump_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_Q_thermal_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_cond_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_panel_out_max_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_pc_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_pc_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_rad_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_rec_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_rec_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_rec_out_end_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_rec_out_max_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_wall_downcomer_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_wall_rec_inlet_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_wall_rec_outlet_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_wall_riser_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_T_warm_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_W_cooling_tower_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_W_cycle_gross_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_eta_rec_th_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_eta_rec_th_incl_refl_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_q_rec_inc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_q_rec_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_annual_total_water_use_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_interest1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_interest2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_interest3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_interest4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_interest5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_interest_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_percent_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_principal1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_principal2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_principal3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_principal4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_principal5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_principal_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_total1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_total2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_total3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_total4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_const_per_total5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_conversion_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_bop_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_contingency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_epc_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_fossil_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_heliostats_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_installed_per_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_plm_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_power_block_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_rad_field_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_rad_fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_rad_storage_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_receiver_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_sales_tax_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_site_improvements_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_storage_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_total_land_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_csp_pt_cost_tower_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_defocus_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_disp_iter_ann_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_obj_relax_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_objective_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_disp_objective_ann_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_pceff_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_presolve_nconstr_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_disp_presolve_nconstr_ann_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_presolve_nvar_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_disp_presolve_nvar_ann_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_qpbsu_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_qsf_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_qsfprod_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_qsfsu_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_rev_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_solve_iter_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_solve_state_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_solve_time_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_disp_solve_time_ann_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_tes_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_thermeff_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_disp_wpb_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_eta_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_eta_field_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_eta_map_out_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_eta_therm_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_flux_maps_for_import_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_flux_maps_out_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_is_pc_sb_allowed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_is_pc_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_is_rec_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_cr_to_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_cycle_to_field_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_field_to_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_pc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_pc_to_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_rec_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_tes_cold_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_tes_hot_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_dot_water_pc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_m_warm_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_mass_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_mass_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_operating_modes_a_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_operating_modes_b_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_operating_modes_c_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_pparasi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_pricing_mult_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_est_cr_on_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_est_cr_su_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_est_tes_ch_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_est_tes_dc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_pc_max_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_pc_min_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_pc_sb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_pc_target_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_dot_rec_inc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_heater_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_pb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_piping_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_sf_inc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_startup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_q_thermal_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_radcool_control_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_rh_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_sco2_preprocess_table_out_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_sf_adjust_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_solaz_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_tank_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_total_direct_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_total_indirect_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_tou_value_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcsmoltenSalt_Outputs_ui_direct_subtotal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcsmoltenSalt_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
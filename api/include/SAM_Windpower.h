#ifndef SAM_WINDPOWER_H_
#define SAM_WINDPOWER_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Windpower Technology Model
	//

	/** 
	 * Create a Windpower variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Windpower;

	SAM_EXPORT SAM_Windpower SAM_Windpower_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Windpower_execute(SAM_Windpower data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Windpower_destruct(SAM_Windpower system);


	//
	// Resource parameters
	//

	/**
	 * Set weibull_k_factor: Weibull K factor for wind resource
	 * options: None
	 * constraints: None
	 * required if: wind_resource_model_choice=1
	 */
	SAM_EXPORT void SAM_Windpower_Resource_weibull_k_factor_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set weibull_reference_height: Reference height for Weibull wind speed [m]
	 * options: None
	 * constraints: MIN=0
	 * required if: ?=50
	 */
	SAM_EXPORT void SAM_Windpower_Resource_weibull_reference_height_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set weibull_wind_speed: Average wind speed for Weibull model
	 * options: None
	 * constraints: MIN=0
	 * required if: wind_resource_model_choice=1
	 */
	SAM_EXPORT void SAM_Windpower_Resource_weibull_wind_speed_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set wind_resource_data: Wind resouce data in memory
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Windpower_Resource_wind_resource_data_tset(SAM_Windpower ptr, SAM_table tab, SAM_error *err);

	/**
	 * Set wind_resource_distribution: Wind Speed x Dir Distribution as 2-D PDF [m/s,deg]
	 * options: [(speed, direction, prob)]
	 * constraints: None
	 * required if: wind_resource_model_choice=2
	 */
	SAM_EXPORT void SAM_Windpower_Resource_wind_resource_distribution_mset(SAM_Windpower ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set wind_resource_filename: Local wind data file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Windpower_Resource_wind_resource_filename_sset(SAM_Windpower ptr, const char* str, SAM_error *err);

	/**
	 * Set wind_resource_model_choice: Hourly or Weibull model [0/1]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_Resource_wind_resource_model_choice_nset(SAM_Windpower ptr, double number, SAM_error *err);


	//
	// Turbine parameters
	//

	/**
	 * Set wind_resource_shear: Shear exponent
	 * options: None
	 * constraints: MIN=0
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_Turbine_wind_resource_shear_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set wind_turbine_hub_ht: Hub height [m]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_Turbine_wind_turbine_hub_ht_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set wind_turbine_max_cp: Max Coefficient of Power
	 * options: None
	 * constraints: MIN=0
	 * required if: wind_resource_model_choice=1
	 */
	SAM_EXPORT void SAM_Windpower_Turbine_wind_turbine_max_cp_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set wind_turbine_powercurve_powerout: Power curve turbine output array [kW]
	 * options: None
	 * constraints: LENGTH_EQUAL=wind_turbine_powercurve_windspeeds
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_Turbine_wind_turbine_powercurve_powerout_aset(SAM_Windpower ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set wind_turbine_powercurve_windspeeds: Power curve wind speed array [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_Turbine_wind_turbine_powercurve_windspeeds_aset(SAM_Windpower ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set wind_turbine_rotor_diameter: Rotor diameter [m]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_Turbine_wind_turbine_rotor_diameter_nset(SAM_Windpower ptr, double number, SAM_error *err);


	//
	// Farm parameters
	//

	/**
	 * Set system_capacity: Nameplate capacity [kW]
	 * options: None
	 * constraints: MIN=0
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_Farm_system_capacity_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set wind_farm_wake_model: Wake Model [Simple, Park, EV, Constant] [0/1/2/3]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_Farm_wind_farm_wake_model_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set wind_farm_xCoordinates: Turbine X coordinates [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_Farm_wind_farm_xCoordinates_aset(SAM_Windpower ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set wind_farm_yCoordinates: Turbine Y coordinates [m]
	 * options: None
	 * constraints: LENGTH_EQUAL=wind_farm_xCoordinates
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_Farm_wind_farm_yCoordinates_aset(SAM_Windpower ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set wind_resource_turbulence_coeff: Turbulence coefficient [%]
	 * options: None
	 * constraints: MIN=0
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_Farm_wind_resource_turbulence_coeff_nset(SAM_Windpower ptr, double number, SAM_error *err);


	//
	// Losses parameters
	//

	/**
	 * Set avail_bop_loss: Balance-of-plant availability loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_avail_bop_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set avail_grid_loss: Grid availability loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_avail_grid_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set avail_turb_loss: Turbine availabaility loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_avail_turb_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set elec_eff_loss: Electrical efficiency loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_elec_eff_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set en_icing_cutoff: Enable Icing Cutoff [0/1]
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_en_icing_cutoff_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set en_low_temp_cutoff: Enable Low Temperature Cutoff [0/1]
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_en_low_temp_cutoff_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set env_degrad_loss: Environmental Degradation loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_env_degrad_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set env_exposure_loss: Environmental Exposure loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_env_exposure_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set env_ext_loss: Environmental External Conditions loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_env_ext_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set env_icing_loss: Environmental Icing loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_env_icing_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set icing_cutoff_rh: Icing Cutoff Relative Humidity [%]
	 * options: 'rh' required in wind_resource_data
	 * constraints: MIN=0
	 * required if: en_icing_cutoff=1
	 */
	SAM_EXPORT void SAM_Windpower_Losses_icing_cutoff_rh_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set icing_cutoff_temp: Icing Cutoff Temperature [C]
	 * options: None
	 * constraints: None
	 * required if: en_icing_cutoff=1
	 */
	SAM_EXPORT void SAM_Windpower_Losses_icing_cutoff_temp_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set low_temp_cutoff: Low Temperature Cutoff [C]
	 * options: None
	 * constraints: None
	 * required if: en_low_temp_cutoff=1
	 */
	SAM_EXPORT void SAM_Windpower_Losses_low_temp_cutoff_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set ops_env_loss: Environmental/Permit Curtailment loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_ops_env_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set ops_grid_loss: Grid curtailment loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_ops_grid_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set ops_load_loss: Load curtailment loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_ops_load_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set ops_strategies_loss: Operational strategies loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_ops_strategies_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set turb_generic_loss: Turbine Generic Powercurve loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_turb_generic_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set turb_hysteresis_loss: Turbine High Wind Hysteresis loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_turb_hysteresis_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set turb_perf_loss: Turbine Sub-optimal performance loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_turb_perf_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set turb_specific_loss: Turbine Site-specific Powercurve loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Losses_turb_specific_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);

	/**
	 * Set wake_loss: Wake effects loss percent [%]
	 * options: None
	 * constraints: None
	 * required if: wind_farm_wake_model=3
	 */
	SAM_EXPORT void SAM_Windpower_Losses_wake_loss_nset(SAM_Windpower ptr, double number, SAM_error *err);


	/**
	 * Resource Getters
	 */

	SAM_EXPORT double SAM_Windpower_Resource_weibull_k_factor_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Resource_weibull_reference_height_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Resource_weibull_wind_speed_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT SAM_table SAM_Windpower_Resource_wind_resource_data_tget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Windpower_Resource_wind_resource_distribution_mget(SAM_Windpower ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT const char* SAM_Windpower_Resource_wind_resource_filename_sget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Resource_wind_resource_model_choice_nget(SAM_Windpower ptr, SAM_error *err);


	/**
	 * Turbine Getters
	 */

	SAM_EXPORT double SAM_Windpower_Turbine_wind_resource_shear_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Turbine_wind_turbine_hub_ht_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Turbine_wind_turbine_max_cp_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Windpower_Turbine_wind_turbine_powercurve_powerout_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Windpower_Turbine_wind_turbine_powercurve_windspeeds_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Turbine_wind_turbine_rotor_diameter_nget(SAM_Windpower ptr, SAM_error *err);


	/**
	 * Farm Getters
	 */

	SAM_EXPORT double SAM_Windpower_Farm_system_capacity_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Farm_wind_farm_wake_model_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Windpower_Farm_wind_farm_xCoordinates_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Windpower_Farm_wind_farm_yCoordinates_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Farm_wind_resource_turbulence_coeff_nget(SAM_Windpower ptr, SAM_error *err);


	/**
	 * Losses Getters
	 */

	SAM_EXPORT double SAM_Windpower_Losses_avail_bop_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_avail_grid_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_avail_turb_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_elec_eff_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_en_icing_cutoff_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_en_low_temp_cutoff_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_env_degrad_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_env_exposure_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_env_ext_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_env_icing_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_icing_cutoff_rh_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_icing_cutoff_temp_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_low_temp_cutoff_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_ops_env_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_ops_grid_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_ops_load_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_ops_strategies_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_turb_generic_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_turb_hysteresis_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_turb_perf_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_turb_specific_loss_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Losses_wake_loss_nget(SAM_Windpower ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Windpower_Outputs_annual_energy_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Outputs_annual_gross_energy_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Outputs_capacity_factor_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Outputs_cutoff_losses_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Windpower_Outputs_gen_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Windpower_Outputs_kwh_per_kw_nget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Windpower_Outputs_monthly_energy_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Windpower_Outputs_pressure_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Windpower_Outputs_temp_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Windpower_Outputs_turbine_output_by_windspeed_bin_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Windpower_Outputs_wind_direction_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Windpower_Outputs_wind_speed_aget(SAM_Windpower ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
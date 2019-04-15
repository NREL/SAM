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
	// WindResourceFile parameters
	//

	/**
	 * Set wind_resource_data: wind resouce data in memory
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Windpower_WindResourceFile_wind_resource_data_tset(SAM_Windpower ptr, SAM_table tab, SAM_error *err);

	/**
	 * Set wind_resource_filename: local wind data file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Windpower_WindResourceFile_wind_resource_filename_sset(SAM_Windpower ptr, const char* str, SAM_error *err);


	//
	// WindTurbine parameters
	//

	/**
	 * Set wind_resource_shear: Shear exponent
	 * options: None
	 * constraints: MIN=0
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_resource_shear_fset(SAM_Windpower ptr, float number, SAM_error *err);

	/**
	 * Set wind_turbine_hub_ht: Hub height [m]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_hub_ht_fset(SAM_Windpower ptr, float number, SAM_error *err);

	/**
	 * Set wind_turbine_max_cp: Max cp
	 * options: None
	 * constraints: MIN=0
	 * required if: wind_resource_model_choice=1
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_max_cp_fset(SAM_Windpower ptr, float number, SAM_error *err);

	/**
	 * Set wind_turbine_powercurve_powerout: Power curve turbine output array [kW]
	 * options: None
	 * constraints: LENGTH_EQUAL=wind_turbine_powercurve_windspeeds
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_powercurve_powerout_aset(SAM_Windpower ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set wind_turbine_powercurve_windspeeds: Power curve wind speed array [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_powercurve_windspeeds_aset(SAM_Windpower ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set wind_turbine_rotor_diameter: Rotor diameter [m]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_rotor_diameter_fset(SAM_Windpower ptr, float number, SAM_error *err);


	//
	// WindFarm parameters
	//

	/**
	 * Set system_capacity: Nameplate capacity [kW]
	 * options: None
	 * constraints: MIN=0
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_system_capacity_fset(SAM_Windpower ptr, float number, SAM_error *err);

	/**
	 * Set wind_farm_losses_percent: Percentage losses [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_wind_farm_losses_percent_fset(SAM_Windpower ptr, float number, SAM_error *err);

	/**
	 * Set wind_farm_wake_model: Wake Model [0/1/2]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_wind_farm_wake_model_fset(SAM_Windpower ptr, float number, SAM_error *err);

	/**
	 * Set wind_farm_xCoordinates: Turbine X coordinates [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_wind_farm_xCoordinates_aset(SAM_Windpower ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set wind_farm_yCoordinates: Turbine Y coordinates [m]
	 * options: None
	 * constraints: LENGTH_EQUAL=wind_farm_xCoordinates
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_wind_farm_yCoordinates_aset(SAM_Windpower ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set wind_resource_turbulence_coeff: Turbulence coefficient [%]
	 * options: None
	 * constraints: MIN=0
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_wind_resource_turbulence_coeff_fset(SAM_Windpower ptr, float number, SAM_error *err);


	//
	// WindPower parameters
	//

	/**
	 * Set en_icing_cutoff: Enable Icing Cutoff [0/1]
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_WindPower_en_icing_cutoff_fset(SAM_Windpower ptr, float number, SAM_error *err);

	/**
	 * Set en_low_temp_cutoff: Enable Low Temperature Cutoff [0/1]
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_WindPower_en_low_temp_cutoff_fset(SAM_Windpower ptr, float number, SAM_error *err);

	/**
	 * Set icing_cutoff_rh: Icing Cutoff Relative Humidity [%]
	 * options: None
	 * constraints: MIN=0
	 * required if: en_icing_cutoff=1
	 */
	SAM_EXPORT void SAM_Windpower_WindPower_icing_cutoff_rh_fset(SAM_Windpower ptr, float number, SAM_error *err);

	/**
	 * Set icing_cutoff_temp: Icing Cutoff Temperature [C]
	 * options: None
	 * constraints: None
	 * required if: en_icing_cutoff=1
	 */
	SAM_EXPORT void SAM_Windpower_WindPower_icing_cutoff_temp_fset(SAM_Windpower ptr, float number, SAM_error *err);

	/**
	 * Set low_temp_cutoff: Low Temperature Cutoff [C]
	 * options: None
	 * constraints: None
	 * required if: en_low_temp_cutoff=1
	 */
	SAM_EXPORT void SAM_Windpower_WindPower_low_temp_cutoff_fset(SAM_Windpower ptr, float number, SAM_error *err);

	/**
	 * Set wind_resource_model_choice: Hourly or Weibull model [0/1]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Windpower_WindPower_wind_resource_model_choice_fset(SAM_Windpower ptr, float number, SAM_error *err);


	//
	// WindSpeedWeibullDistribution parameters
	//

	/**
	 * Set weibull_k_factor: Weibull K factor for wind resource
	 * options: None
	 * constraints: None
	 * required if: wind_resource_model_choice=1
	 */
	SAM_EXPORT void SAM_Windpower_WindSpeedWeibullDistribution_weibull_k_factor_fset(SAM_Windpower ptr, float number, SAM_error *err);

	/**
	 * Set weibull_reference_height: Reference height for Weibull wind speed [m]
	 * options: None
	 * constraints: MIN=0
	 * required if: ?=50
	 */
	SAM_EXPORT void SAM_Windpower_WindSpeedWeibullDistribution_weibull_reference_height_fset(SAM_Windpower ptr, float number, SAM_error *err);

	/**
	 * Set weibull_wind_speed: Average wind speed for Weibull model
	 * options: None
	 * constraints: MIN=0
	 * required if: wind_resource_model_choice=1
	 */
	SAM_EXPORT void SAM_Windpower_WindSpeedWeibullDistribution_weibull_wind_speed_fset(SAM_Windpower ptr, float number, SAM_error *err);


	/**
	 * WindResourceFile Getters
	 */

	SAM_EXPORT SAM_table SAM_Windpower_WindResourceFile_wind_resource_data_tget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Windpower_WindResourceFile_wind_resource_filename_sget(SAM_Windpower ptr, SAM_error *err);


	/**
	 * WindTurbine Getters
	 */

	SAM_EXPORT float SAM_Windpower_WindTurbine_wind_resource_shear_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_WindTurbine_wind_turbine_hub_ht_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_WindTurbine_wind_turbine_max_cp_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float* SAM_Windpower_WindTurbine_wind_turbine_powercurve_powerout_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Windpower_WindTurbine_wind_turbine_powercurve_windspeeds_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_WindTurbine_wind_turbine_rotor_diameter_fget(SAM_Windpower ptr, SAM_error *err);


	/**
	 * WindFarm Getters
	 */

	SAM_EXPORT float SAM_Windpower_WindFarm_system_capacity_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_WindFarm_wind_farm_losses_percent_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_WindFarm_wind_farm_wake_model_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float* SAM_Windpower_WindFarm_wind_farm_xCoordinates_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Windpower_WindFarm_wind_farm_yCoordinates_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_WindFarm_wind_resource_turbulence_coeff_fget(SAM_Windpower ptr, SAM_error *err);


	/**
	 * WindPower Getters
	 */

	SAM_EXPORT float SAM_Windpower_WindPower_en_icing_cutoff_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_WindPower_en_low_temp_cutoff_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_WindPower_icing_cutoff_rh_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_WindPower_icing_cutoff_temp_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_WindPower_low_temp_cutoff_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_WindPower_wind_resource_model_choice_fget(SAM_Windpower ptr, SAM_error *err);


	/**
	 * WindSpeedWeibullDistribution Getters
	 */

	SAM_EXPORT float SAM_Windpower_WindSpeedWeibullDistribution_weibull_k_factor_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_WindSpeedWeibullDistribution_weibull_reference_height_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_WindSpeedWeibullDistribution_weibull_wind_speed_fget(SAM_Windpower ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT float SAM_Windpower_Outputs_annual_energy_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_Outputs_capacity_factor_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_Outputs_cutoff_losses_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float* SAM_Windpower_Outputs_gen_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_Windpower_Outputs_kwh_per_kw_fget(SAM_Windpower ptr, SAM_error *err);

	SAM_EXPORT float* SAM_Windpower_Outputs_monthly_energy_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Windpower_Outputs_pressure_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Windpower_Outputs_temp_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Windpower_Outputs_turbine_output_by_windspeed_bin_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Windpower_Outputs_wind_direction_aget(SAM_Windpower ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Windpower_Outputs_wind_speed_aget(SAM_Windpower ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
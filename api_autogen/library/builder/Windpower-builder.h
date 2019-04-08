#ifndef SAM_WINDPOWER_FUNCTIONS_H_
#define SAM_WINDPOWER_FUNCTIONS_H_

#include "Windpower-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a WindSpeedWeibullDistribution variable table for a WindPowerNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Windpower_WindSpeedWeibullDistribution SAM_Windpower_WindSpeedWeibullDistribution_create(const char* def, SAM_error* err);


	/**
	 * Set weibull_k_factor: Weibull K factor for wind resource
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: wind_resource_model_choice=1
	 */
	SAM_EXPORT void SAM_Windpower_WindSpeedWeibullDistribution_weibull_k_factor_set(SAM_Windpower_WindSpeedWeibullDistribution ptr, float number, SAM_error* err);

	/**
	 * Set weibull_reference_height: Reference height for Weibull wind speed
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: MIN=0
	 * required if: ?=50
	 */
	SAM_EXPORT void SAM_Windpower_WindSpeedWeibullDistribution_weibull_reference_height_set(SAM_Windpower_WindSpeedWeibullDistribution ptr, float number, SAM_error* err);

	/**
	 * Set weibull_wind_speed: Average wind speed for Weibull model
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: MIN=0
	 * required if: wind_resource_model_choice=1
	 */
	SAM_EXPORT void SAM_Windpower_WindSpeedWeibullDistribution_weibull_wind_speed_set(SAM_Windpower_WindSpeedWeibullDistribution ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Windpower_WindSpeedWeibullDistribution_weibull_k_factor_get(SAM_Windpower_WindSpeedWeibullDistribution ptr, SAM_error* err);

	SAM_EXPORT float SAM_Windpower_WindSpeedWeibullDistribution_weibull_reference_height_get(SAM_Windpower_WindSpeedWeibullDistribution ptr, SAM_error* err);

	SAM_EXPORT float SAM_Windpower_WindSpeedWeibullDistribution_weibull_wind_speed_get(SAM_Windpower_WindSpeedWeibullDistribution ptr, SAM_error* err);



	/** 
	 * Create a WindTurbine variable table for a WindPowerNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Windpower_WindTurbine SAM_Windpower_WindTurbine_create(const char* def, SAM_error* err);


	/**
	 * Set wind.turbine.drive_train: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind.turbine.drive_train_set(SAM_Windpower_WindTurbine ptr, const char* string, SAM_error* err);

	/**
	 * Set wind.turbine.elevation: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind.turbine.elevation_set(SAM_Windpower_WindTurbine ptr, const char* string, SAM_error* err);

	/**
	 * Set wind.turbine.max_tip_speed: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind.turbine.max_tip_speed_set(SAM_Windpower_WindTurbine ptr, const char* string, SAM_error* err);

	/**
	 * Set wind.turbine.max_tspeed_ratio: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind.turbine.max_tspeed_ratio_set(SAM_Windpower_WindTurbine ptr, const char* string, SAM_error* err);

	/**
	 * Set wind.turbine.radio_list_or_design: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind.turbine.radio_list_or_design_set(SAM_Windpower_WindTurbine ptr, const char* string, SAM_error* err);

	/**
	 * Set wind.turbine.region2nhalf_slope: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind.turbine.region2nhalf_slope_set(SAM_Windpower_WindTurbine ptr, const char* string, SAM_error* err);

	/**
	 * Set wind_resource_shear: Shear exponent
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: MIN=0
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_resource_shear_set(SAM_Windpower_WindTurbine ptr, float number, SAM_error* err);

	/**
	 * Set wind_turbine_cut_out: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_cut_out_set(SAM_Windpower_WindTurbine ptr, const char* string, SAM_error* err);

	/**
	 * Set wind_turbine_cutin: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_cutin_set(SAM_Windpower_WindTurbine ptr, const char* string, SAM_error* err);

	/**
	 * Set wind_turbine_hub_ht: Hub height
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: POSITIVE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_hub_ht_set(SAM_Windpower_WindTurbine ptr, float number, SAM_error* err);

	/**
	 * Set wind_turbine_kw_rating_from_lib: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_kw_rating_from_lib_set(SAM_Windpower_WindTurbine ptr, const char* string, SAM_error* err);

	/**
	 * Set wind_turbine_kw_rating_input: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_kw_rating_input_set(SAM_Windpower_WindTurbine ptr, const char* string, SAM_error* err);

	/**
	 * Set wind_turbine_max_cp: Max cp
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: MIN=0
	 * required if: wind_resource_model_choice=1
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_max_cp_set(SAM_Windpower_WindTurbine ptr, float number, SAM_error* err);

	/**
	 * Set wind_turbine_powercurve_powerout_from_lib: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_powercurve_powerout_from_lib_set(SAM_Windpower_WindTurbine ptr, const char* string, SAM_error* err);

	/**
	 * Set wind_turbine_powercurve_windspeeds_from_lib: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_powercurve_windspeeds_from_lib_set(SAM_Windpower_WindTurbine ptr, const char* string, SAM_error* err);

	/**
	 * Set wind_turbine_rotor_diameter_from_lib: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_rotor_diameter_from_lib_set(SAM_Windpower_WindTurbine ptr, const char* string, SAM_error* err);

	/**
	 * Set wind_turbine_rotor_diameter_input: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_rotor_diameter_input_set(SAM_Windpower_WindTurbine ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_Windpower_WindTurbine_wind.turbine.drive_train_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindTurbine_wind.turbine.elevation_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindTurbine_wind.turbine.max_tip_speed_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindTurbine_wind.turbine.max_tspeed_ratio_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindTurbine_wind.turbine.radio_list_or_design_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindTurbine_wind.turbine.region2nhalf_slope_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT float SAM_Windpower_WindTurbine_wind_resource_shear_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindTurbine_wind_turbine_cut_out_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindTurbine_wind_turbine_cutin_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT float SAM_Windpower_WindTurbine_wind_turbine_hub_ht_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindTurbine_wind_turbine_kw_rating_from_lib_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindTurbine_wind_turbine_kw_rating_input_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT float SAM_Windpower_WindTurbine_wind_turbine_max_cp_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindTurbine_wind_turbine_powercurve_powerout_from_lib_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindTurbine_wind_turbine_powercurve_windspeeds_from_lib_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindTurbine_wind_turbine_rotor_diameter_from_lib_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindTurbine_wind_turbine_rotor_diameter_input_get(SAM_Windpower_WindTurbine ptr, SAM_error* err);



	/** 
	 * Create a WindFarm variable table for a WindPowerNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Windpower_WindFarm SAM_Windpower_WindFarm_create(const char* def, SAM_error* err);


	/**
	 * Set desired_farm_size: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_desired_farm_size_set(SAM_Windpower_WindFarm ptr, const char* string, SAM_error* err);

	/**
	 * Set wind_farm_sizing_mode: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_wind_farm_sizing_mode_set(SAM_Windpower_WindFarm ptr, const char* string, SAM_error* err);

	/**
	 * Set wind_farm_xCoord_file: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_wind_farm_xCoord_file_set(SAM_Windpower_WindFarm ptr, const char* string, SAM_error* err);

	/**
	 * Set wind_farm_yCoord_file: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_wind_farm_yCoord_file_set(SAM_Windpower_WindFarm ptr, const char* string, SAM_error* err);

	/**
	 * Set windfarm.farm.layout_angle: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_windfarm.farm.layout_angle_set(SAM_Windpower_WindFarm ptr, const char* string, SAM_error* err);

	/**
	 * Set windfarm.farm.number_of_rows: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_windfarm.farm.number_of_rows_set(SAM_Windpower_WindFarm ptr, const char* string, SAM_error* err);

	/**
	 * Set windfarm.farm.offset: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_windfarm.farm.offset_set(SAM_Windpower_WindFarm ptr, const char* string, SAM_error* err);

	/**
	 * Set windfarm.farm.offset_type: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_windfarm.farm.offset_type_set(SAM_Windpower_WindFarm ptr, const char* string, SAM_error* err);

	/**
	 * Set windfarm.farm.row_spacing: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_windfarm.farm.row_spacing_set(SAM_Windpower_WindFarm ptr, const char* string, SAM_error* err);

	/**
	 * Set windfarm.farm.shape: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_windfarm.farm.shape_set(SAM_Windpower_WindFarm ptr, const char* string, SAM_error* err);

	/**
	 * Set windfarm.farm.turbine_spacing: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_windfarm.farm.turbine_spacing_set(SAM_Windpower_WindFarm ptr, const char* string, SAM_error* err);

	/**
	 * Set windfarm.farm.turbines_per_row: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_windfarm.farm.turbines_per_row_set(SAM_Windpower_WindFarm ptr, const char* string, SAM_error* err);

	/**
	 * Set windfarm.layout.file_or_controls: local wind data file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_WindFarm_windfarm.layout.file_or_controls_set(SAM_Windpower_WindFarm ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_Windpower_WindFarm_desired_farm_size_get(SAM_Windpower_WindFarm ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindFarm_wind_farm_sizing_mode_get(SAM_Windpower_WindFarm ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindFarm_wind_farm_xCoord_file_get(SAM_Windpower_WindFarm ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindFarm_wind_farm_yCoord_file_get(SAM_Windpower_WindFarm ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindFarm_windfarm.farm.layout_angle_get(SAM_Windpower_WindFarm ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindFarm_windfarm.farm.number_of_rows_get(SAM_Windpower_WindFarm ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindFarm_windfarm.farm.offset_get(SAM_Windpower_WindFarm ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindFarm_windfarm.farm.offset_type_get(SAM_Windpower_WindFarm ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindFarm_windfarm.farm.row_spacing_get(SAM_Windpower_WindFarm ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindFarm_windfarm.farm.shape_get(SAM_Windpower_WindFarm ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindFarm_windfarm.farm.turbine_spacing_get(SAM_Windpower_WindFarm ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindFarm_windfarm.farm.turbines_per_row_get(SAM_Windpower_WindFarm ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Windpower_WindFarm_windfarm.layout.file_or_controls_get(SAM_Windpower_WindFarm ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a WindPowerNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Windpower_Common SAM_Windpower_Common_create(const char* def, SAM_error* err);


	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_Common_adjust:constant_set(SAM_Windpower_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_Common_adjust:hourly_set(SAM_Windpower_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_Common_adjust:periods_set(SAM_Windpower_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set en_icing_cutoff: Enable Icing Cutoff
	 * type: numeric
	 * units: 0/1
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Common_en_icing_cutoff_set(SAM_Windpower_Common ptr, float number, SAM_error* err);

	/**
	 * Set en_low_temp_cutoff: Enable Low Temperature Cutoff
	 * type: numeric
	 * units: 0/1
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Windpower_Common_en_low_temp_cutoff_set(SAM_Windpower_Common ptr, float number, SAM_error* err);

	/**
	 * Set icing_cutoff_rh: Icing Cutoff Relative Humidity
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MIN=0
	 * required if: en_icing_cutoff=1
	 */
	SAM_EXPORT void SAM_Windpower_Common_icing_cutoff_rh_set(SAM_Windpower_Common ptr, float number, SAM_error* err);

	/**
	 * Set icing_cutoff_temp: Icing Cutoff Temperature
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: en_icing_cutoff=1
	 */
	SAM_EXPORT void SAM_Windpower_Common_icing_cutoff_temp_set(SAM_Windpower_Common ptr, float number, SAM_error* err);

	/**
	 * Set low_temp_cutoff: Low Temperature Cutoff
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: en_low_temp_cutoff=1
	 */
	SAM_EXPORT void SAM_Windpower_Common_low_temp_cutoff_set(SAM_Windpower_Common ptr, float number, SAM_error* err);

	/**
	 * Set wind_resource_data: wind resouce data in memory
	 * type: table
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Windpower_Common_wind_resource_data_set(SAM_Windpower_Common ptr, var_table vt, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Windpower_Common_adjust:constant_get(SAM_Windpower_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Windpower_Common_adjust:hourly_get(SAM_Windpower_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Windpower_Common_adjust:periods_get(SAM_Windpower_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Windpower_Common_en_icing_cutoff_get(SAM_Windpower_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Windpower_Common_en_low_temp_cutoff_get(SAM_Windpower_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Windpower_Common_icing_cutoff_rh_get(SAM_Windpower_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Windpower_Common_icing_cutoff_temp_get(SAM_Windpower_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Windpower_Common_low_temp_cutoff_get(SAM_Windpower_Common ptr, SAM_error* err);

	SAM_EXPORT var_table SAM_Windpower_Common_wind_resource_data_get(SAM_Windpower_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
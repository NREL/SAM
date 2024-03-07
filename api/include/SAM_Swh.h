#ifndef SAM_SWH_H_
#define SAM_SWH_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Swh Technology Model
	//

	/** 
	 * Create a Swh variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Swh;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Swh_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SolarResource parameters
	//

	/**
	 * Set solar_resource_data: Weather data
	 * options: dn,df,tdry,wspd,lat,lon,tz
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Swh_SolarResource_solar_resource_data_tset(SAM_table ptr, SAM_table tab, SAM_error *err);

	/**
	 * Set solar_resource_file: local weather file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Swh_SolarResource_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err);


	//
	// SWH parameters
	//

	/**
	 * Set FRUL: FRUL
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_FRUL_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set FRta: FRta
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_FRta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_room: Temperature around solar tank [C]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_T_room_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_set: Set temperature [C]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_T_set_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_tank_max: Max temperature in solar tank [C]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_T_tank_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set U_tank: Solar tank heat loss coefficient [W/m2K]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_U_tank_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set V_tank: Solar tank volume [m3]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_V_tank_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set albedo: Ground reflectance factor [0..1]
	 * options: None
	 * constraints: FACTOR
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_albedo_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set area_coll: Single collector area [m2]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_area_coll_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set azimuth: Collector azimuth [deg]
	 * options: 90=E,180=S
	 * constraints: MIN=0,MAX=360
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_azimuth_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set custom_mains: Custom mains [C]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_custom_mains_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set custom_set: Custom set points [C]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_custom_set_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set fluid: Working fluid in system
	 * options: Water,Glycol
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hx_eff: Heat exchanger effectiveness [0..1]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_hx_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set iam: Incidence angle modifier
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_iam_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set irrad_mode: Irradiance input mode [0/1/2]
	 * options: Beam+Diff,Global+Beam,Global+Diff
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Swh_SWH_irrad_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set load: Electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_SWH_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set load_escalation: Annual load escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Swh_SWH_load_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set mdot: Total system mass flow rate [kg/s]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_mdot_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ncoll: Number of collectors
	 * options: None
	 * constraints: POSITIVE,INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_ncoll_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pipe_diam: Pipe diameter [m]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_pipe_diam_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pipe_insul: Pipe insulation thickness [m]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_pipe_insul_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pipe_k: Pipe insulation conductivity [W/m-C]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_pipe_k_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pipe_length: Length of piping in system [m]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_pipe_length_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pump_eff: Pumping efficiency [%]
	 * options: None
	 * constraints: PERCENT
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_pump_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pump_power: Pump power [W]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_pump_power_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set scaled_draw: Hot water draw [kg/hr]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_scaled_draw_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set sky_model: Tilted surface irradiance model [0/1/2]
	 * options: Isotropic,HDKR,Perez
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Swh_SWH_sky_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_capacity: Nameplate capacity [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_system_capacity_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tank_h2d_ratio: Solar tank height to diameter ratio
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_tank_h2d_ratio_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set test_flow: Flow rate used in collector test [kg/s]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_test_flow_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set test_fluid: Fluid used in collector test
	 * options: Water,Glycol
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_test_fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tilt: Collector tilt [deg]
	 * options: None
	 * constraints: MIN=0,MAX=90
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_tilt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set use_custom_mains: Use custom mains [%]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_use_custom_mains_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set use_custom_set: Use custom set points [%]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_use_custom_set_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Shading parameters
	//

	/**
	 * Set shading_azal: Azimuth x altitude beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Swh_Shading_shading_azal_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set shading_diff: Diffuse shading loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Swh_Shading_shading_diff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading_en_azal: Enable azimuth x altitude beam shading losses [0/1]
	 * options: 0=false,1=true
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Swh_Shading_shading_en_azal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading_en_diff: Enable diffuse shading loss [0/1]
	 * options: 0=false,1=true
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Swh_Shading_shading_en_diff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading_en_mxh: Enable month x Hour beam shading losses [0/1]
	 * options: 0=false,1=true
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Swh_Shading_shading_en_mxh_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading_en_string_option: Enable shading string option [0/1]
	 * options: 0=false,1=true
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Swh_Shading_shading_en_string_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading_en_timestep: Enable timestep beam shading losses [0/1]
	 * options: 0=false,1=true
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Swh_Shading_shading_en_timestep_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading_mxh: Month x Hour beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Swh_Shading_shading_mxh_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set shading_string_option: Shading string option
	 * options: 0=shadingdb,1=average,2=maximum,3=minimum
	 * constraints: INTEGER,MIN=-1,MAX=4
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_Swh_Shading_shading_string_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading_timestep: Timestep beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Swh_Shading_shading_timestep_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// AdjustmentFactors parameters
	//

	/**
	 * Set adjust_constant: Constant loss adjustment [%]
	 * options: 'adjust' and 'constant' separated by _ instead of : after SAM 2022.12.21
	 * constraints: MAX=100
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Swh_AdjustmentFactors_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_en_periods: Enable period-based adjustment factors [0/1]
	 * options: 'adjust' and 'en_periods' separated by _ instead of : after SAM 2022.12.21
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Swh_AdjustmentFactors_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_en_timeindex: Enable lifetime adjustment factors [0/1]
	 * options: 'adjust' and 'en_timeindex' separated by _ instead of : after SAM 2022.12.21
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Swh_AdjustmentFactors_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_periods: Period-based adjustment factors [%]
	 * options: Syntax: n x 3 matrix [ start, end, loss ]; Version upgrade: 'adjust' and 'periods' separated by _ instead of : after SAM 2022.12.21
	 * constraints: COLS=3
	 * required if: adjust_en_periods=1
	 */
	SAM_EXPORT void SAM_Swh_AdjustmentFactors_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set adjust_timeindex: Lifetime adjustment factors [%]
	 * options: 'adjust' and 'timeindex' separated by _ instead of : after SAM 2022.12.21
	 * constraints: None
	 * required if: adjust_en_timeindex=1
	 */
	SAM_EXPORT void SAM_Swh_AdjustmentFactors_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	/**
	 * SolarResource Getters
	 */

	SAM_EXPORT SAM_table SAM_Swh_SolarResource_solar_resource_data_tget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Swh_SolarResource_solar_resource_file_sget(SAM_table ptr, SAM_error *err);


	/**
	 * SWH Getters
	 */

	SAM_EXPORT double SAM_Swh_SWH_FRUL_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_FRta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_T_room_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_T_set_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_T_tank_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_U_tank_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_V_tank_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_albedo_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_area_coll_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_azimuth_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_SWH_custom_mains_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_SWH_custom_set_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_hx_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_iam_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_irrad_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_SWH_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_SWH_load_escalation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_mdot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_ncoll_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_pipe_diam_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_pipe_insul_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_pipe_k_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_pipe_length_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_pump_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_pump_power_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_SWH_scaled_draw_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_sky_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_tank_h2d_ratio_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_test_flow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_test_fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_tilt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_use_custom_mains_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_use_custom_set_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Shading Getters
	 */

	SAM_EXPORT double* SAM_Swh_Shading_shading_azal_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Shading_shading_diff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Shading_shading_en_azal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Shading_shading_en_diff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Shading_shading_en_mxh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Shading_shading_en_string_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Shading_shading_en_timestep_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Shading_shading_mxh_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Shading_shading_string_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Shading_shading_timestep_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * AdjustmentFactors Getters
	 */

	SAM_EXPORT double SAM_Swh_AdjustmentFactors_adjust_constant_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_AdjustmentFactors_adjust_en_periods_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_AdjustmentFactors_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_AdjustmentFactors_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_AdjustmentFactors_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Swh_Outputs_I_incident_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_I_transmitted_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_P_pump_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_Q_aux_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_Q_auxonly_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_Q_deliv_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_Q_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_Q_transmitted_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_Q_useful_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_T_amb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_T_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_T_deliv_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_T_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_T_mains_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_T_tank_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_V_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_V_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_annual_Q_aux_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_annual_Q_auxonly_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_annual_Q_deliv_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_diffuse_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_draw_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_mode_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_monthly_Q_aux_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_monthly_Q_auxonly_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_monthly_Q_deliv_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_shading_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_solar_fraction_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_ts_shift_hours_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
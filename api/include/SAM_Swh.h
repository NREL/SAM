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

	SAM_EXPORT SAM_Swh SAM_Swh_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Swh_execute(SAM_Swh data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Swh_destruct(SAM_Swh system);


	//
	// Weather parameters
	//

	/**
	 * Set solar_resource_file: local weather file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_Weather_solar_resource_file_sset(SAM_Swh ptr, const char* str, SAM_error *err);


	//
	// SWH parameters
	//

	/**
	 * Set FRUL: FRUL
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_FRUL_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set FRta: FRta
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_FRta_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set T_room: Temperature around solar tank [C]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_T_room_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set T_set: Set temperature [C]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_T_set_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set T_tank_max: Max temperature in solar tank [C]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_T_tank_max_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set U_tank: Solar tank heat loss coefficient [W/m2K]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_U_tank_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set V_tank: Solar tank volume [m3]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_V_tank_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set albedo: Ground reflectance factor [0..1]
	 * options: None
	 * constraints: FACTOR
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_albedo_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set area_coll: Single collector area [m2]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_area_coll_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set azimuth: Collector azimuth [deg]
	 * options: 90=E,180=S
	 * constraints: MIN=0,MAX=360
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_azimuth_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set custom_mains: Custom mains [C]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_custom_mains_aset(SAM_Swh ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set custom_set: Custom set points [C]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_custom_set_aset(SAM_Swh ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set fluid: Working fluid in system
	 * options: Water,Glycol
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_fluid_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set hx_eff: Heat exchanger effectiveness [0..1]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_hx_eff_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set iam: Incidence angle modifier
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_iam_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set irrad_mode: Irradiance input mode [0/1/2]
	 * options: Beam+Diff,Global+Beam,Global+Diff
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Swh_SWH_irrad_mode_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set load: Electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_SWH_load_aset(SAM_Swh ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set mdot: Total system mass flow rate [kg/s]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_mdot_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set ncoll: Number of collectors
	 * options: None
	 * constraints: POSITIVE,INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_ncoll_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set pipe_diam: Pipe diameter [m]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_pipe_diam_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set pipe_insul: Pipe insulation thickness [m]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_pipe_insul_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set pipe_k: Pipe insulation conductivity [W/m2.C]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_pipe_k_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set pipe_length: Length of piping in system [m]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_pipe_length_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set pump_eff: Pumping efficiency [%]
	 * options: None
	 * constraints: PERCENT
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_pump_eff_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set pump_power: Pump power [W]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_pump_power_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set scaled_draw: Hot water draw [kg/hr]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_scaled_draw_aset(SAM_Swh ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set shading:azal: Azimuth x altitude beam shading loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Swh_SWH_shading_azal_mset(SAM_Swh ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set shading:diff: Diffuse shading loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Swh_SWH_shading_diff_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set shading:mxh: Month x Hour beam shading loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Swh_SWH_shading_mxh_mset(SAM_Swh ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set shading:timestep: Time step beam shading loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Swh_SWH_shading_timestep_mset(SAM_Swh ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sky_model: Tilted surface irradiance model [0/1/2]
	 * options: Isotropic,HDKR,Perez
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Swh_SWH_sky_model_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set system_capacity: Nameplate capacity [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_system_capacity_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set tank_h2d_ratio: Solar tank height to diameter ratio
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_tank_h2d_ratio_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set test_flow: Flow rate used in collector test [kg/s]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_test_flow_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set test_fluid: Fluid used in collector test
	 * options: Water,Glycol
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_test_fluid_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set tilt: Collector tilt [deg]
	 * options: None
	 * constraints: MIN=0,MAX=90
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_tilt_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set use_custom_mains: Use custom mains [%]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_use_custom_mains_nset(SAM_Swh ptr, double number, SAM_error *err);

	/**
	 * Set use_custom_set: Use custom set points [%]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Swh_SWH_use_custom_set_nset(SAM_Swh ptr, double number, SAM_error *err);


	/**
	 * Weather Getters
	 */

	SAM_EXPORT const char* SAM_Swh_Weather_solar_resource_file_sget(SAM_Swh ptr, SAM_error *err);


	/**
	 * SWH Getters
	 */

	SAM_EXPORT double SAM_Swh_SWH_FRUL_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_FRta_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_T_room_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_T_set_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_T_tank_max_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_U_tank_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_V_tank_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_albedo_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_area_coll_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_azimuth_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_SWH_custom_mains_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_SWH_custom_set_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_fluid_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_hx_eff_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_iam_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_irrad_mode_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_SWH_load_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_mdot_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_ncoll_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_pipe_diam_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_pipe_insul_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_pipe_k_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_pipe_length_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_pump_eff_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_pump_power_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_SWH_scaled_draw_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_SWH_shading_azal_mget(SAM_Swh ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_shading_diff_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_SWH_shading_mxh_mget(SAM_Swh ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_SWH_shading_timestep_mget(SAM_Swh ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_sky_model_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_system_capacity_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_tank_h2d_ratio_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_test_flow_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_test_fluid_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_tilt_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_use_custom_mains_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_SWH_use_custom_set_nget(SAM_Swh ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Swh_Outputs_I_incident_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_I_transmitted_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_P_pump_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_Q_aux_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_Q_auxonly_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_Q_deliv_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_Q_loss_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_Q_transmitted_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_Q_useful_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_T_amb_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_T_cold_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_T_deliv_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_T_hot_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_T_mains_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_T_tank_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_V_cold_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_V_hot_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_annual_Q_aux_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_annual_Q_auxonly_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_annual_Q_deliv_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_annual_energy_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_beam_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_capacity_factor_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_diffuse_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_draw_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_gen_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_kwh_per_kw_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_mode_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_monthly_Q_aux_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_monthly_Q_auxonly_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_monthly_Q_deliv_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_monthly_energy_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Swh_Outputs_shading_loss_aget(SAM_Swh ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_solar_fraction_nget(SAM_Swh ptr, SAM_error *err);

	SAM_EXPORT double SAM_Swh_Outputs_ts_shift_hours_nget(SAM_Swh ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
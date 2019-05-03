#ifndef SAM_TCSMSLF_FUNCTIONS_H_
#define SAM_TCSMSLF_FUNCTIONS_H_

#include "TcsMSLF-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a SolarField variable table for a MSLFNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsMSLF_SolarField SAM_TcsMSLF_SolarField_create(const char* def, SAM_error* err);


	/**
	 * Set HTF_data: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_SolarField_HTF_data_set(SAM_TcsMSLF_SolarField ptr, const char* string, SAM_error* err);

	/**
	 * Set I_bn_des: Solar irradiation at design
	 * type: numeric
	 * units: W/m2
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_SolarField_I_bn_des_set(SAM_TcsMSLF_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set T_amb_sf_des: Ambient design-point temperature for the solar field
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_SolarField_T_amb_sf_des_set(SAM_TcsMSLF_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set T_loop_in_des: Design loop inlet temperature
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_SolarField_T_loop_in_des_set(SAM_TcsMSLF_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set T_loop_out: Target loop outlet temperature
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_SolarField_T_loop_out_set(SAM_TcsMSLF_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set a_field: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_SolarField_a_field_set(SAM_TcsMSLF_SolarField ptr, const char* string, SAM_error* err);

	/**
	 * Set csp.mslf.sf.Fluid: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_SolarField_csp.mslf.sf.Fluid_set(SAM_TcsMSLF_SolarField ptr, const char* string, SAM_error* err);

	/**
	 * Set csp.mslf.sf.sm_or_area: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_SolarField_csp.mslf.sf.sm_or_area_set(SAM_TcsMSLF_SolarField ptr, const char* string, SAM_error* err);

	/**
	 * Set fthrok: Flag to allow partial defocusing of the collectors
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_SolarField_fthrok_set(SAM_TcsMSLF_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set nMod: Number of collector modules in a loop
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_SolarField_nMod_set(SAM_TcsMSLF_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set solar_mult_spec: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_SolarField_solar_mult_spec_set(SAM_TcsMSLF_SolarField ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_TcsMSLF_SolarField_HTF_data_get(SAM_TcsMSLF_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_SolarField_I_bn_des_get(SAM_TcsMSLF_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_SolarField_T_amb_sf_des_get(SAM_TcsMSLF_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_SolarField_T_loop_in_des_get(SAM_TcsMSLF_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_SolarField_T_loop_out_get(SAM_TcsMSLF_SolarField ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsMSLF_SolarField_a_field_get(SAM_TcsMSLF_SolarField ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsMSLF_SolarField_csp.mslf.sf.Fluid_get(SAM_TcsMSLF_SolarField ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsMSLF_SolarField_csp.mslf.sf.sm_or_area_get(SAM_TcsMSLF_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_SolarField_fthrok_get(SAM_TcsMSLF_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_SolarField_nMod_get(SAM_TcsMSLF_SolarField ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcsMSLF_SolarField_solar_mult_spec_get(SAM_TcsMSLF_SolarField ptr, SAM_error* err);



	/** 
	 * Create a CollectorAndReceiver variable table for a MSLFNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsMSLF_CollectorAndReceiver SAM_TcsMSLF_CollectorAndReceiver_create(const char* def, SAM_error* err);


	/**
	 * Set A_aperture: Reflective aperture area of the collector
	 * type: numeric
	 * units: m2
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_CollectorAndReceiver_A_aperture_set(SAM_TcsMSLF_CollectorAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set DP_nominal: Pressure drop across a single collector assembly at design
	 * type: numeric
	 * units: bar
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_CollectorAndReceiver_DP_nominal_set(SAM_TcsMSLF_CollectorAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set Dirt_mirror: User-defined dirt on mirror derate
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_CollectorAndReceiver_Dirt_mirror_set(SAM_TcsMSLF_CollectorAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set Error: User-defined general optical error derate
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_CollectorAndReceiver_Error_set(SAM_TcsMSLF_CollectorAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set GeomEffects: Geometry effects derate
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_CollectorAndReceiver_GeomEffects_set(SAM_TcsMSLF_CollectorAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set L_mod: The length of the collector module
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_CollectorAndReceiver_L_mod_set(SAM_TcsMSLF_CollectorAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set TrackingError: Tracking error derate
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_CollectorAndReceiver_TrackingError_set(SAM_TcsMSLF_CollectorAndReceiver ptr, float number, SAM_error* err);

	/**
	 * Set reflectivity: Solar-weighted mirror reflectivity value
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_CollectorAndReceiver_reflectivity_set(SAM_TcsMSLF_CollectorAndReceiver ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsMSLF_CollectorAndReceiver_A_aperture_get(SAM_TcsMSLF_CollectorAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_CollectorAndReceiver_DP_nominal_get(SAM_TcsMSLF_CollectorAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_CollectorAndReceiver_Dirt_mirror_get(SAM_TcsMSLF_CollectorAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_CollectorAndReceiver_Error_get(SAM_TcsMSLF_CollectorAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_CollectorAndReceiver_GeomEffects_get(SAM_TcsMSLF_CollectorAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_CollectorAndReceiver_L_mod_get(SAM_TcsMSLF_CollectorAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_CollectorAndReceiver_TrackingError_get(SAM_TcsMSLF_CollectorAndReceiver ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_CollectorAndReceiver_reflectivity_get(SAM_TcsMSLF_CollectorAndReceiver ptr, SAM_error* err);



	/** 
	 * Create a PowerCycleCommon variable table for a MSLFNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsMSLF_PowerCycleCommon SAM_TcsMSLF_PowerCycleCommon_create(const char* def, SAM_error* err);


	/**
	 * Set P_ref: Label
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_PowerCycleCommon_P_ref_set(SAM_TcsMSLF_PowerCycleCommon ptr, float number, SAM_error* err);

	/**
	 * Set eta_lhv: Label
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_PowerCycleCommon_eta_lhv_set(SAM_TcsMSLF_PowerCycleCommon ptr, float number, SAM_error* err);

	/**
	 * Set eta_ref: Cycle thermal efficiency at design point
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_PowerCycleCommon_eta_ref_set(SAM_TcsMSLF_PowerCycleCommon ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsMSLF_PowerCycleCommon_P_ref_get(SAM_TcsMSLF_PowerCycleCommon ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_PowerCycleCommon_eta_lhv_get(SAM_TcsMSLF_PowerCycleCommon ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_PowerCycleCommon_eta_ref_get(SAM_TcsMSLF_PowerCycleCommon ptr, SAM_error* err);



	/** 
	 * Create a UserDefinedPowerCycle variable table for a MSLFNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsMSLF_UserDefinedPowerCycle SAM_TcsMSLF_UserDefinedPowerCycle_create(const char* def, SAM_error* err);


	/**
	 * Set ud_T_amb_des: Ambient temperature at user-defined power cycle design point
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsMSLF_UserDefinedPowerCycle_ud_T_amb_des_set(SAM_TcsMSLF_UserDefinedPowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set ud_T_amb_high: High level ambient temperature for HTF mass flow rate parametric
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsMSLF_UserDefinedPowerCycle_ud_T_amb_high_set(SAM_TcsMSLF_UserDefinedPowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set ud_T_amb_low: Low level ambient temperature for HTF mass flow rate parametric
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsMSLF_UserDefinedPowerCycle_ud_T_amb_low_set(SAM_TcsMSLF_UserDefinedPowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set ud_T_htf_high: High level HTF inlet temperature for T_amb parametric
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsMSLF_UserDefinedPowerCycle_ud_T_htf_high_set(SAM_TcsMSLF_UserDefinedPowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set ud_T_htf_low: Low level HTF inlet temperature for T_amb parametric
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsMSLF_UserDefinedPowerCycle_ud_T_htf_low_set(SAM_TcsMSLF_UserDefinedPowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set ud_f_W_dot_cool_des: Percent of user-defined power cycle design gross output consumed by cooling
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsMSLF_UserDefinedPowerCycle_ud_f_W_dot_cool_des_set(SAM_TcsMSLF_UserDefinedPowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set ud_m_dot_htf_high: High level normalized HTF mass flow rate for T_HTF parametric
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsMSLF_UserDefinedPowerCycle_ud_m_dot_htf_high_set(SAM_TcsMSLF_UserDefinedPowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set ud_m_dot_htf_low: Low level normalized HTF mass flow rate for T_HTF parametric
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcsMSLF_UserDefinedPowerCycle_ud_m_dot_htf_low_set(SAM_TcsMSLF_UserDefinedPowerCycle ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsMSLF_UserDefinedPowerCycle_ud_T_amb_des_get(SAM_TcsMSLF_UserDefinedPowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_UserDefinedPowerCycle_ud_T_amb_high_get(SAM_TcsMSLF_UserDefinedPowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_UserDefinedPowerCycle_ud_T_amb_low_get(SAM_TcsMSLF_UserDefinedPowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_UserDefinedPowerCycle_ud_T_htf_high_get(SAM_TcsMSLF_UserDefinedPowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_UserDefinedPowerCycle_ud_T_htf_low_get(SAM_TcsMSLF_UserDefinedPowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_UserDefinedPowerCycle_ud_f_W_dot_cool_des_get(SAM_TcsMSLF_UserDefinedPowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_UserDefinedPowerCycle_ud_m_dot_htf_high_get(SAM_TcsMSLF_UserDefinedPowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_UserDefinedPowerCycle_ud_m_dot_htf_low_get(SAM_TcsMSLF_UserDefinedPowerCycle ptr, SAM_error* err);



	/** 
	 * Create a ThermalStorage variable table for a MSLFNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsMSLF_ThermalStorage SAM_TcsMSLF_ThermalStorage_create(const char* def, SAM_error* err);


	/**
	 * Set csp.mslf.control.store_fluid: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_ThermalStorage_csp.mslf.control.store_fluid_set(SAM_TcsMSLF_ThermalStorage ptr, const char* string, SAM_error* err);

	/**
	 * Set dt_hot: Label
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_ThermalStorage_dt_hot_set(SAM_TcsMSLF_ThermalStorage ptr, float number, SAM_error* err);

	/**
	 * Set h_tank: Label
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_ThermalStorage_h_tank_set(SAM_TcsMSLF_ThermalStorage ptr, float number, SAM_error* err);

	/**
	 * Set h_tank_min: Label
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_ThermalStorage_h_tank_min_set(SAM_TcsMSLF_ThermalStorage ptr, float number, SAM_error* err);

	/**
	 * Set store_fl_props: Label
	 * type: matrix
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_ThermalStorage_store_fl_props_set(SAM_TcsMSLF_ThermalStorage ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set tank_pairs: Label
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_ThermalStorage_tank_pairs_set(SAM_TcsMSLF_ThermalStorage ptr, float number, SAM_error* err);

	/**
	 * Set tshours: Label
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_ThermalStorage_tshours_set(SAM_TcsMSLF_ThermalStorage ptr, float number, SAM_error* err);

	/**
	 * Set u_tank: Label
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_ThermalStorage_u_tank_set(SAM_TcsMSLF_ThermalStorage ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_TcsMSLF_ThermalStorage_csp.mslf.control.store_fluid_get(SAM_TcsMSLF_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_ThermalStorage_dt_hot_get(SAM_TcsMSLF_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_ThermalStorage_h_tank_get(SAM_TcsMSLF_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_ThermalStorage_h_tank_min_get(SAM_TcsMSLF_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsMSLF_ThermalStorage_store_fl_props_get(SAM_TcsMSLF_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_ThermalStorage_tank_pairs_get(SAM_TcsMSLF_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_ThermalStorage_tshours_get(SAM_TcsMSLF_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_ThermalStorage_u_tank_get(SAM_TcsMSLF_ThermalStorage ptr, SAM_error* err);



	/** 
	 * Create a Parasitics variable table for a MSLFNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsMSLF_Parasitics SAM_TcsMSLF_Parasitics_create(const char* def, SAM_error* err);


	/**
	 * Set SCA_drives_elec: Tracking power in Watts per SCA drive
	 * type: numeric
	 * units: W/module
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_Parasitics_SCA_drives_elec_set(SAM_TcsMSLF_Parasitics ptr, float number, SAM_error* err);

	/**
	 * Set pb_fixed_par: Label
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_Parasitics_pb_fixed_par_set(SAM_TcsMSLF_Parasitics ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsMSLF_Parasitics_SCA_drives_elec_get(SAM_TcsMSLF_Parasitics ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_Parasitics_pb_fixed_par_get(SAM_TcsMSLF_Parasitics ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a MSLFNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsMSLF_Common SAM_TcsMSLF_Common_create(const char* def, SAM_error* err);


	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_Common_adjust:constant_set(SAM_TcsMSLF_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_Common_adjust:hourly_set(SAM_TcsMSLF_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsMSLF_Common_adjust:periods_set(SAM_TcsMSLF_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set nRecVar: Number of receiver variantions
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=4
	 */
	SAM_EXPORT void SAM_TcsMSLF_Common_nRecVar_set(SAM_TcsMSLF_Common ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsMSLF_Common_adjust:constant_get(SAM_TcsMSLF_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsMSLF_Common_adjust:hourly_get(SAM_TcsMSLF_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsMSLF_Common_adjust:periods_get(SAM_TcsMSLF_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsMSLF_Common_nRecVar_get(SAM_TcsMSLF_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
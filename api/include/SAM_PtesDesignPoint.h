#ifndef SAM_PTESDESIGNPOINT_H_
#define SAM_PTESDESIGNPOINT_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// PtesDesignPoint Technology Model
	//

	/** 
	 * Create a PtesDesignPoint variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_PtesDesignPoint;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_PtesDesignPoint_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Common parameters
	//

	/**
	 * Set P0: Ambient Pressure [Pa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_P0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P1: Lowest Pressure in cycle [Pa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_P1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T0: Ambient Temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_T0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_compressor_inlet: Charging compressor inlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_T_compressor_inlet_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_compressor_outlet: Charging compressor outlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_T_compressor_outlet_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set alpha: Ratio of mdot cp     AIR/WF
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_alpha_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set charge_time_hr: charging time [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_charge_time_hr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cold_fluid_id: Cold Reservoir Fluid ID
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_cold_fluid_id_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cold_ud_fluid_props: User Defined Cold Resevior Fluid Properties
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_cold_ud_fluid_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set discharge_time_hr: discharge time [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_discharge_time_hr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta: polytropic efficiency of compressors and expanders
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_eta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_pump: polytropic efficiency of air pump
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_eta_pump_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set gen_eff: Generator Efficiency
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_gen_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_fluid_id: Hot Reservoir Fluid ID
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_hot_fluid_id_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_ud_fluid_props: User Defined Hot Resevior Fluid Properties
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_hot_ud_fluid_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set hx_eff: hx effectiveness
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_hx_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set motor_eff: Motor Efficiency
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_motor_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ploss_air: Fractional pressure loss (air)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_ploss_air_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ploss_liquid: Fractional pressure loss (liquid)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_ploss_liquid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ploss_working: Fractional pressure loss in each heat exchanger
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_ploss_working_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set power_output: Power Output [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_power_output_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set working_fluid_type: Working Fluid Type
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_PtesDesignPoint_Common_working_fluid_type_sset(SAM_table ptr, const char* str, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_PtesDesignPoint_Common_P0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_P1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_T0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_T_compressor_inlet_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_T_compressor_outlet_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_alpha_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_charge_time_hr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_cold_fluid_id_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_PtesDesignPoint_Common_cold_ud_fluid_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_discharge_time_hr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_eta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_eta_pump_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_gen_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_hot_fluid_id_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_PtesDesignPoint_Common_hot_ud_fluid_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_hx_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_motor_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_ploss_air_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_ploss_liquid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_ploss_working_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Common_power_output_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_PtesDesignPoint_Common_working_fluid_type_sget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_PtesDesignPoint_Outputs_N_pts_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Outputs_N_pts_discharge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Outputs_Tc_cold_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Outputs_Tc_hot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Outputs_Th_cold_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Outputs_Th_hot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Outputs_cycle_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Outputs_hp_COP_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Outputs_hp_cold_pump_power_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Outputs_hp_hot_pump_power_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Outputs_hp_parasitic_fraction_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Outputs_pc_cold_pump_power_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Outputs_pc_hot_pump_power_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_PtesDesignPoint_Outputs_pc_parasitic_fraction_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_PtesDesignPoint_Outputs_s_series_charge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_PtesDesignPoint_Outputs_s_series_discharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_PtesDesignPoint_Outputs_temp_series_charge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_PtesDesignPoint_Outputs_temp_series_discharge_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
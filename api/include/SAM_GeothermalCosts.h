#ifndef SAM_GEOTHERMALCOSTS_H_
#define SAM_GEOTHERMALCOSTS_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// GeothermalCosts Technology Model
	//

	/** 
	 * Create a GeothermalCosts variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_GeothermalCosts;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_GeothermalCosts_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// GeoHourly parameters
	//

	/**
	 * Set GF_flowrate: GF Flow Rate [lb/h]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_GF_flowrate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set condensate_pump_power: hp
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_condensate_pump_power_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set conversion_type: Conversion Type
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_conversion_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cw_pump_head: Cooling Water Pump Head [lb/h]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_cw_pump_head_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cw_pump_work: CW Pump Work [kW]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_cw_pump_work_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cwflow: Cooling Water Flow [lb/h]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_cwflow_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set design_temp: Power block design temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_design_temp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eff_secondlaw: Second Law Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_eff_secondlaw_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set flash_count: Flash Count [(1 -2)]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_flash_count_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set gross_output: Gross output from GETEM [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_gross_output_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hp_flash_pressure: HP Flash Pressure [psia]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_hp_flash_pressure_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set lp_flash_pressure: LP Flash Pressure [psia]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_lp_flash_pressure_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ncg_condensate_pump: Condensate Pump Work [kW]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_ncg_condensate_pump_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pressure_ratio_1: Suction Steam Ratio 1
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_pressure_ratio_1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pressure_ratio_2: Suction Steam Ratio 2
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_pressure_ratio_2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pressure_ratio_3: Suction Steam Ratio 3
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_pressure_ratio_3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set qCondenser: Condenser Heat Rejected [btu/h]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_qCondenser_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set qRejectByStage_1: Heat Rejected by NCG Condenser Stage 1 [BTU/hr]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_qRejectByStage_1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set qRejectByStage_2: Heat Rejected by NCG Condenser Stage 2 [BTU/hr]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_qRejectByStage_2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set qRejectByStage_3: Heat Rejected by NCG Condenser Stage 3 [BTU/hr]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_qRejectByStage_3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set qRejectTotal: Total Rejected Heat [btu/h]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_qRejectTotal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spec_vol: Specific Volume [cft/lb]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_spec_vol_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spec_vol_lp: LP Specific Volume [cft/lb]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_spec_vol_lp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set v_stage_1: Vacumm Pump Stage 1 [kW]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_v_stage_1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set v_stage_2: Vacumm Pump Stage 2 [kW]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_v_stage_2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set v_stage_3: Vacumm Pump Stage 3 [kW]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_v_stage_3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set x_hp: HP Mass Fraction [%]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_x_hp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set x_lp: LP Mass Fraction [%]
	 * options: None
	 * constraints: None
	 * required if: conversion_type=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_x_lp_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * GeoHourly Getters
	 */

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_GF_flowrate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_condensate_pump_power_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_conversion_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_cw_pump_head_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_cw_pump_work_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_cwflow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_design_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_eff_secondlaw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_flash_count_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_gross_output_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_hp_flash_pressure_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_lp_flash_pressure_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_ncg_condensate_pump_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_pressure_ratio_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_pressure_ratio_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_pressure_ratio_3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_qCondenser_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_qRejectByStage_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_qRejectByStage_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_qRejectByStage_3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_qRejectTotal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_spec_vol_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_spec_vol_lp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_v_stage_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_v_stage_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_v_stage_3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_x_hp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_x_lp_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_baseline_cost_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
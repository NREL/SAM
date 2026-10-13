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
	 * Set calc_drill_costs: Calculate drill costs [0/1]
	 * options: 0=LargerDiameter,1=SmallerDiameter
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_calc_drill_costs_nset(SAM_table ptr, double number, SAM_error *err);

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
	 * Set dt_prod_well: Temperature loss in production well [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_dt_prod_well_nset(SAM_table ptr, double number, SAM_error *err);

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
	 * Set geotherm.cost.conf_multiplier: Confirmation cost multiplier
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_conf_multiplier_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.conf_non_drill: Confirmation non drilling costs [$]
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_conf_non_drill_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.conf_num_wells: Number of confirmation wells
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_conf_num_wells_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.contingency_percent: Contingency percent [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_contingency_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.drilling.amount_specified: Absolute drilling cost input [$]
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_drilling_amount_specified_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.drilling.calc: 0: user specified absolute drilling cost, 1: calculated [$]
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_drilling_calc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.epc.fixed: EPC fixed cost [$]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_epc_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.epc.percent: EPC percent [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_epc_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.expl_lump_sum: Exploration cost lump sum
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_lump_sum_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.expl_multiplier: Exploration cost multiplier
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_multiplier_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.expl_non_drill: Exploration non drilling costs [$]
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_non_drill_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.expl_num_wells: Number of exploration wells
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_num_wells_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.indirect.amount_specified: Absolute indirect cost input [$]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_indirect_amount_specified_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.indirect.calc: 0: user specified absolute indirect cost, 1: calculated
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_indirect_calc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.inj_cost_curve: Injection well diameter type [0/1]
	 * options: 0=LargerDiameter,1=SmallerDiameter
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_inj_cost_curve_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.inj_cost_curve_welldiam: Injection well diameter type [0/1]
	 * options: 0=LargerDiameter,1=SmallerDiameter
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_inj_cost_curve_welldiam_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.inj_cost_curve_welltype: Injection well type [0/1]
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_inj_cost_curve_welltype_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.plant_auto_estimate: 0: use user input cost; 1: use getem calcs [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_plant_auto_estimate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.plant_per_kW_input: user input for relative plant cost [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_plant_per_kW_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.plant_total.calc: 0: user specificed absolute plant cost, 1: calculated
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_plant_total_calc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.plm.fixed: Project-land-misc fixed cost [$]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_plm_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.plm.percent: Project-land-misc percent [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_plm_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.prod_cost_curve: Production well diameter type [0/1]
	 * options: 0=LargerDiameter,1=SmallerDiameter
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_prod_cost_curve_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.prod_cost_curve_welldiam: Production well diameter type [0/1]
	 * options: 0=LargerDiameter,1=SmallerDiameter
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_prod_cost_curve_welldiam_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.prod_cost_curve_welltype: Production well type [0/1]
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_prod_cost_curve_welltype_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.prod_inj_non_drill: Non drilling cost for prod and inj well [$]
	 * options: 0=LargerDiameter,1=SmallerDiameter
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_prod_inj_non_drill_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.pump_casing_cost: Pump casing cost per foot [$/ft]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_pump_casing_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.pump_fixed: Fixed pump workover and casing cost [$]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_pump_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.pump_per_foot: Pump cost per foot [$/ft]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_pump_per_foot_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.pumping.amount_specified: Absolute pump cost input [$]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_pumping_amount_specified_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.pumping.calc: 0: user specified absolute pump cost, 1: calculated
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_pumping_calc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.recap_specified: Absolute recap cost input [$]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_recap_specified_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.recap_use_calc: 0: user specified absolute recap cost, 1: calculated
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_recap_use_calc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.sales_tax.percent: Percent of direct cost to which sales tax is applied [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_sales_tax_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.stim_non_drill: Stimulation non drilling costs [$]
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_stim_non_drill_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set gross_cost_output: Gross output from GETEM for cost calculations [kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_gross_cost_output_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set gross_output: Gross output from GETEM [MWe]
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
	 * Set inj_pump_hp: Injection pump power [hp]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_inj_pump_hp_nset(SAM_table ptr, double number, SAM_error *err);

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
	 * Set net_plant_output: Net plant power to grid; net sales in GETEM [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_net_plant_output_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set num_wells_getem: Number of production wells required
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_num_wells_getem_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set num_wells_getem_inj_drilled: Number of drilled injection wells [0/1]
	 * options: 0=LargerDiameter,1=SmallerDiameter
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_num_wells_getem_inj_drilled_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set num_wells_getem_prod_drilled: Number of drilled production wells
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_num_wells_getem_prod_drilled_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set num_wells_getem_prod_failed: Number of failed production wells
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_num_wells_getem_prod_failed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set num_wells_getem_prod_inj_sum: Sum of production and injection drilled wells
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_num_wells_getem_prod_inj_sum_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ppi_base_year: PPI Base Year
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_ppi_base_year_nset(SAM_table ptr, double number, SAM_error *err);

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
	 * Set pump_depth_ft: Pump depth [ft]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_pump_depth_ft_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pump_size_hp: Production pump power [hp]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_pump_size_hp_nset(SAM_table ptr, double number, SAM_error *err);

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
	 * Set resource_depth: Resource Depth [m]
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_resource_depth_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sales_tax_rate: Sales tax rate [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err);

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
	 * Set stimulation_type: Which wells are stimulated [0/1/2/3]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_stimulation_type_nset(SAM_table ptr, double number, SAM_error *err);

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


	//
	// FinancialModel parameters
	//

	/**
	 * Set geo_financial_model:  [1-8]
	 * options: None
	 * constraints: INTEGER,MIN=0
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_GeothermalCosts_FinancialModel_geo_financial_model_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Common parameters
	//

	/**
	 * Set geotherm.cost.plant_total.amount_specified: Absolute plant cost input [$]
	 * options: GeoHourly
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GeothermalCosts_Common_geotherm_cost_plant_total_amount_specified_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * GeoHourly Getters
	 */

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_GF_flowrate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_calc_drill_costs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_condensate_pump_power_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_conversion_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_cw_pump_head_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_cw_pump_work_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_cwflow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_design_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_dt_prod_well_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_eff_secondlaw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_flash_count_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_conf_multiplier_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_conf_non_drill_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_conf_num_wells_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_contingency_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_drilling_amount_specified_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_drilling_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_epc_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_epc_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_lump_sum_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_multiplier_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_non_drill_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_num_wells_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_indirect_amount_specified_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_indirect_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_inj_cost_curve_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_inj_cost_curve_welldiam_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_inj_cost_curve_welltype_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_plant_auto_estimate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_plant_per_kW_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_plant_total_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_plm_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_plm_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_prod_cost_curve_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_prod_cost_curve_welldiam_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_prod_cost_curve_welltype_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_prod_inj_non_drill_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_pump_casing_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_pump_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_pump_per_foot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_pumping_amount_specified_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_pumping_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_recap_specified_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_recap_use_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_sales_tax_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_stim_non_drill_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_gross_cost_output_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_gross_output_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_hp_flash_pressure_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_inj_pump_hp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_lp_flash_pressure_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_ncg_condensate_pump_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_net_plant_output_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_num_wells_getem_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_num_wells_getem_inj_drilled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_num_wells_getem_prod_drilled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_num_wells_getem_prod_failed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_num_wells_getem_prod_inj_sum_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_ppi_base_year_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_pressure_ratio_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_pressure_ratio_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_pressure_ratio_3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_pump_depth_ft_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_pump_size_hp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_qCondenser_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_qRejectByStage_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_qRejectByStage_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_qRejectByStage_3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_qRejectTotal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_resource_depth_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_sales_tax_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_spec_vol_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_spec_vol_lp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_stimulation_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_v_stage_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_v_stage_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_v_stage_3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_x_hp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_x_lp_nget(SAM_table ptr, SAM_error *err);


	/**
	 * FinancialModel Getters
	 */

	SAM_EXPORT double SAM_GeothermalCosts_FinancialModel_geo_financial_model_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_GeothermalCosts_Common_geotherm_cost_plant_total_amount_specified_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_atb_drilling_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_atb_exploration_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_atb_plant_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_baseline_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_conf_drilling_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_conf_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_contingency_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_engineering_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_epc_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_expl_drilling_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_expl_per_well_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_expl_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_field_gathering_num_wells_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_indirect_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_indirect_pump_gathering_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_inj_num_pumps_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_inj_pump_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_inj_pump_cost_per_pump_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_inj_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_inj_well_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_installed_cost_per_kW_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_piping_cost_per_well_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_plm_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_prod_inj_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_prod_pump_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_prod_pump_cost_per_well_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_prod_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_prod_well_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_pump_cost_install_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_pump_only_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_sales_tax_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_stim_cost_non_drill_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_stim_cost_per_well_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_stim_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_sum_prod_inj_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_system_recapitalization_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_capital_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_direct_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_drilling_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_drilling_cost_used_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_drilling_permitting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_expl_permitting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_gathering_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_plant_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_plant_cost_used_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_pump_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_pump_gathering_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_pump_gathering_cost_used_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_surface_equipment_cost_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
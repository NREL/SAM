#ifndef SAM_GEOTHERMAL_H_
#define SAM_GEOTHERMAL_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Geothermal Technology Model
	//

	/** 
	 * Create a Geothermal variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Geothermal;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Geothermal_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SystemControl parameters
	//

	/**
	 * Set sim_type: 1 (default): timeseries, 2: design only
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Geothermal_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// FinancialModel parameters
	//

	/**
	 * Set geo_financial_model:  [1-8]
	 * options: None
	 * constraints: INTEGER,MIN=0
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialModel_geo_financial_model_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// GeoHourly parameters
	//

	/**
	 * Set CT: Condenser type (Wet, Dry,Hybrid) [(1-3)]
	 * options: None
	 * constraints: INTEGER
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_CT_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_boil: Design Boiler Pressure [bar]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_P_boil_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_cond_min: Minimum condenser pressure [in Hg]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_P_cond_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_cond_ratio: Condenser pressure ratio
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_P_cond_ratio_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_ITD_des: Design ITD for dry system [C]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_amb_des: Design ambient temperature [C]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_approach: Approach Temperature [C]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_T_approach_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_cold_ref: Outlet design temp [C]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_T_htf_cold_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set allow_reservoir_replacements: Allow reservoir replacements
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_allow_reservoir_replacements_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ambient_pressure: Ambient pressure [psi]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_ambient_pressure_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set analysis_type: Analysis Type
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_analysis_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set calc_drill_costs: Calculate drill costs [0/1]
	 * options: 0=LargerDiameter,1=SmallerDiameter
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_calc_drill_costs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set conversion_subtype: Conversion Subtype
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_conversion_subtype_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set conversion_type: Conversion Type (0: binary; 1: flash)
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_conversion_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dT_cw_ref: Design condenser cooling water inlet/outlet T diff [C]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_dT_cw_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set decline_type: Temp decline Type (0: enter rate; 1 calc rate)
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_decline_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set delta_pressure_equip: Delta pressure across surface equipment [psi]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_delta_pressure_equip_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set drilling_success_rate: Drilling success rate [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_drilling_success_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dt_prod_well: Temperature loss in production well [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_dt_prod_well_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_ref: Desgin conversion efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_eta_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set excess_pressure_pump: Excess pressure @ pump suction [psi]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_excess_pressure_pump_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set failed_prod_flow_ratio: Failed production well flow ratio
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_failed_prod_flow_ratio_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set file_name: local weather file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_file_name_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set fracture_angle: Fracture angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_fracture_angle_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fracture_aperature: Fracture aperature [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_fracture_aperature_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fracture_length: Fracture length [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_fracture_length_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fracture_spacing: Fracture spacing [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_fracture_spacing_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fracture_width: Fracture width [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_fracture_width_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.conf_multiplier: Confirmation cost multiplier
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_conf_multiplier_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.conf_non_drill: Confirmation non drilling costs [$]
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_conf_non_drill_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.conf_num_wells: Number of confirmation wells
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_conf_num_wells_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.confirm_wells_percent: % of Confirmation Wells Used for Production
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_confirm_wells_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.contingency_percent: Contingency percent [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_contingency_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.drilling.amount_specified: Absolute drilling cost input [$]
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_drilling_amount_specified_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.drilling.calc: 0: user specified absolute drilling cost, 1: calculated [$]
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_drilling_calc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.epc.fixed: EPC fixed cost [$]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_epc_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.epc.percent: EPC percent [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_epc_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.expl_lump_sum: Exploration cost lump sum
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_expl_lump_sum_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.expl_multiplier: Exploration cost multiplier
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_expl_multiplier_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.expl_non_drill: Exploration non drilling costs [$]
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_expl_non_drill_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.expl_num_wells: Number of exploration wells
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_expl_num_wells_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.indirect.amount_specified: Absolute indirect cost input [$]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_indirect_amount_specified_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.indirect.calc: 0: user specified absolute indirect cost, 1: calculated
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_indirect_calc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.inj_cost_curve: Injection well diameter type [0/1]
	 * options: 0=LargerDiameter,1=SmallerDiameter
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_inj_cost_curve_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.inj_cost_curve_welldiam: Injection well diameter type [0/1]
	 * options: 0=LargerDiameter,1=SmallerDiameter
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_inj_cost_curve_welldiam_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.inj_cost_curve_welltype: Injection well type [0/1]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_inj_cost_curve_welltype_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.inj_prod_well_ratio: Ratio of injection wells to production wells
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_inj_prod_well_ratio_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.plant_auto_estimate: 0: use user input cost; 1: use getem calcs [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_plant_auto_estimate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.plant_per_kW_input: user input for relative plant cost [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_plant_per_kW_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.plant_total.calc: 0: user specificed absolute plant cost, 1: calculated
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_plant_total_calc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.plm.fixed: Project-land-misc fixed cost [$]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_plm_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.plm.percent: Project-land-misc percent [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_plm_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.prod_cost_curve: Production well diameter type [0/1]
	 * options: 0=LargerDiameter,1=SmallerDiameter
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_prod_cost_curve_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.prod_cost_curve_welldiam: Production well diameter type [0/1]
	 * options: 0=LargerDiameter,1=SmallerDiameter
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_prod_cost_curve_welldiam_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.prod_cost_curve_welltype: Production well type [0/1]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_prod_cost_curve_welltype_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.prod_inj_non_drill: Non drilling cost for prod and inj well [$]
	 * options: 0=LargerDiameter,1=SmallerDiameter
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_prod_inj_non_drill_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.pump_casing_cost: Pump casing cost per foot [$/ft]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_pump_casing_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.pump_fixed: Fixed pump workover and casing cost [$]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_pump_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.pump_per_foot: Pump cost per foot [$/ft]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_pump_per_foot_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.pumping.amount_specified: Absolute pump cost input [$]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_pumping_amount_specified_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.pumping.calc: 0: user specified absolute pump cost, 1: calculated
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_pumping_calc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.recap_specified: Absolute recap cost input [$]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_recap_specified_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.recap_use_calc: 0: user specified absolute recap cost, 1: calculated
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_recap_use_calc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.sales_tax.percent: Percent of direct cost to which sales tax is applied [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_sales_tax_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set geotherm.cost.stim_non_drill: Stimulation non drilling costs [$]
	 * options: None
	 * constraints: None
	 * required if: calc_drill_costs=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_stim_non_drill_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hc_ctl1: HC Control 1
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hc_ctl2: HC Control 2
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hc_ctl3: HC Control 3
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hc_ctl4: HC Control 4
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hc_ctl5: HC Control 5
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hc_ctl6: HC Control 6
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl6_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hc_ctl7: HC Control 7
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl7_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hc_ctl8: HC Control 8
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl8_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hc_ctl9: HC Control 9
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl9_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hr_pl_nlev: # part-load increments [(0-9)]
	 * options: None
	 * constraints: INTEGER
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_hr_pl_nlev_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hybrid_dispatch_schedule: Daily dispatch schedule
	 * options: None
	 * constraints: TOUSCHED
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_hybrid_dispatch_schedule_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set inj_prod_well_distance: Distance from injection to production wells [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_inj_prod_well_distance_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set injectivity_index: Injectivity index [lb/hr-psi]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_injectivity_index_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set model_choice: Which model to run (0,1,2)
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_model_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nameplate: Desired plant output [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_nameplate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set num_fractures: Number of fractures
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_num_fractures_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set num_wells: Number of Wells
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_num_wells_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_bd_frac: Blowdown steam fraction [%]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_pb_bd_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set plant_efficiency_input: Plant efficiency
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_plant_efficiency_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ppi_base_year: PPI Base Year
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_ppi_base_year_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set prod_well_choice: Temperature loss in production well choice [0/1]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_prod_well_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pump_efficiency: Pump efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_pump_efficiency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_sby_frac: % thermal power for standby mode [%]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set reservoir_height: Reservoir height [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_reservoir_height_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set reservoir_model_inputs: Reservoir temperatures over time
	 * options: None
	 * constraints: None
	 * required if: reservoir_pressure_change_type=3
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_reservoir_model_inputs_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set reservoir_permeability: Reservoir Permeability [darcys]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_reservoir_permeability_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set reservoir_pressure_change: Pressure change [psi-h/1000lb]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_reservoir_pressure_change_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set reservoir_pressure_change_type: Reservoir pressure change type
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_reservoir_pressure_change_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set reservoir_width: Reservoir width [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_reservoir_width_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set resource_depth: Resource Depth [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_resource_depth_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set resource_potential: Resource Potential [MW]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_resource_potential_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set resource_temp: Resource Temperature [C]
	 * options: None
	 * constraints: MAX=373
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_resource_temp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set resource_type: Type of Resource (O = hydrothermal; 1 = EGS)
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_resource_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rock_density: Rock density [kg/m^3]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_rock_density_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rock_specific_heat: Rock specific heat [J/kg-C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_rock_specific_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rock_thermal_conductivity: Rock thermal conductivity [J/m-day-C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_rock_thermal_conductivity_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sales_tax_rate: Sales tax rate [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set specified_pump_work_amount: Pump work specified by user [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_specified_pump_work_amount_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set specify_pump_work: Did user specify pump work? [0 or 1]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_specify_pump_work_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set start_day_of_year: Start day of year for TOD periods [0..6]
	 * options: 0=Monday, 6=Sunday
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_start_day_of_year_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set startup_frac: % thermal power for startup [%]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_startup_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set startup_time: Hours to start power block [hours]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_startup_time_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set stim_success_rate: Stimulation success rate [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_stim_success_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set stimulation_type: Which wells are stimulated [0/1/2/3]
	 * options: 0=Injection,1=Production,2=Both,3=Neither
	 * constraints: None
	 * required if: ?=3
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_stimulation_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subsurface_water_loss: Subsurface water loss [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_subsurface_water_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set temp_decline_max: Maximum temperature decline [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_temp_decline_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set temp_decline_rate: Temperature decline rate [%/yr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_temp_decline_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set use_weather_file_conditions: Use weather file ambient temperature [0/1]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_use_weather_file_conditions_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set well_flow_rate: Production flow rate per well [kg/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_well_flow_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set wet_bulb_temp: Wet Bulb Temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Geothermal_GeoHourly_wet_bulb_temp_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// AdjustmentFactors parameters
	//

	/**
	 * Set adjust_constant: Constant loss adjustment [%]
	 * options: 'adjust' and 'constant' separated by _ instead of : after SAM 2022.12.21
	 * constraints: MAX=100
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Geothermal_AdjustmentFactors_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_en_periods: Enable period-based adjustment factors [0/1]
	 * options: 'adjust' and 'en_periods' separated by _ instead of : after SAM 2022.12.21
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Geothermal_AdjustmentFactors_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_en_timeindex: Enable lifetime adjustment factors [0/1]
	 * options: 'adjust' and 'en_timeindex' separated by _ instead of : after SAM 2022.12.21
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Geothermal_AdjustmentFactors_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_periods: Period-based adjustment factors [%]
	 * options: Syntax: n x 3 matrix [ start, end, loss ]; Version upgrade: 'adjust' and 'periods' separated by _ instead of : after SAM 2022.12.21
	 * constraints: COLS=3
	 * required if: adjust_en_periods=1
	 */
	SAM_EXPORT void SAM_Geothermal_AdjustmentFactors_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set adjust_timeindex: Lifetime adjustment factors [%]
	 * options: 'adjust' and 'timeindex' separated by _ instead of : after SAM 2022.12.21
	 * constraints: None
	 * required if: adjust_en_timeindex=1
	 */
	SAM_EXPORT void SAM_Geothermal_AdjustmentFactors_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// FinancialParameters parameters
	//

	/**
	 * Set analysis_period: Analyis period [years]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_analysis_period_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Costs parameters
	//

	/**
	 * Set geotherm.cost.plant_total.amount_specified: Absolute plant cost input [$]
	 * options: GeoHourly
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_Costs_geotherm_cost_plant_total_amount_specified_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// FinancialParameters parameters
	//

	/**
	 * Set const_per_interest_rate1: Interest rate, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate2: Interest rate, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate3: Interest rate, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate4: Interest rate, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate5: Interest rate, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months1: Months prior to operation, loan 1
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months2: Months prior to operation, loan 2
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months3: Months prior to operation, loan 3
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months4: Months prior to operation, loan 4
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months5: Months prior to operation, loan 5
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent1: Percent of tot. installed cost, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent2: Percent of tot. installed cost, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent3: Percent of tot. installed cost, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent4: Percent of tot. installed cost, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent5: Percent of tot. installed cost, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate1: Upfront fee on principal, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate2: Upfront fee on principal, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate3: Upfront fee on principal, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate4: Upfront fee on principal, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate5: Upfront fee on principal, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * SystemControl Getters
	 */

	SAM_EXPORT double SAM_Geothermal_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err);


	/**
	 * FinancialModel Getters
	 */

	SAM_EXPORT double SAM_Geothermal_FinancialModel_geo_financial_model_nget(SAM_table ptr, SAM_error *err);


	/**
	 * GeoHourly Getters
	 */

	SAM_EXPORT double SAM_Geothermal_GeoHourly_CT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_P_boil_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_P_cond_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_P_cond_ratio_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_T_ITD_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_T_amb_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_T_approach_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_T_htf_cold_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_allow_reservoir_replacements_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_ambient_pressure_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_analysis_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_calc_drill_costs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_conversion_subtype_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_conversion_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_dT_cw_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_decline_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_delta_pressure_equip_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_drilling_success_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_dt_prod_well_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_eta_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_excess_pressure_pump_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_failed_prod_flow_ratio_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Geothermal_GeoHourly_file_name_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_fracture_angle_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_fracture_aperature_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_fracture_length_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_fracture_spacing_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_fracture_width_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_conf_multiplier_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_conf_non_drill_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_conf_num_wells_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_confirm_wells_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_contingency_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_drilling_amount_specified_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_drilling_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_epc_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_epc_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_expl_lump_sum_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_expl_multiplier_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_expl_non_drill_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_expl_num_wells_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_indirect_amount_specified_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_indirect_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_inj_cost_curve_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_inj_cost_curve_welldiam_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_inj_cost_curve_welltype_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_inj_prod_well_ratio_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_plant_auto_estimate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_plant_per_kW_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_plant_total_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_plm_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_plm_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_prod_cost_curve_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_prod_cost_curve_welldiam_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_prod_cost_curve_welltype_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_prod_inj_non_drill_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_pump_casing_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_pump_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_pump_per_foot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_pumping_amount_specified_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_pumping_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_recap_specified_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_recap_use_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_sales_tax_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_stim_non_drill_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl6_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl7_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl8_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl9_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_hr_pl_nlev_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Geothermal_GeoHourly_hybrid_dispatch_schedule_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_inj_prod_well_distance_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_injectivity_index_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_model_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_num_fractures_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_num_wells_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_pb_bd_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_plant_efficiency_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_ppi_base_year_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_prod_well_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_pump_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_q_sby_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_reservoir_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_GeoHourly_reservoir_model_inputs_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_reservoir_permeability_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_reservoir_pressure_change_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_reservoir_pressure_change_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_reservoir_width_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_resource_depth_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_resource_potential_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_resource_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_resource_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_rock_density_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_rock_specific_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_rock_thermal_conductivity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_sales_tax_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_specified_pump_work_amount_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_specify_pump_work_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_start_day_of_year_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_startup_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_startup_time_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_stim_success_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_stimulation_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_subsurface_water_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_temp_decline_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_temp_decline_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_use_weather_file_conditions_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_well_flow_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_GeoHourly_wet_bulb_temp_nget(SAM_table ptr, SAM_error *err);


	/**
	 * AdjustmentFactors Getters
	 */

	SAM_EXPORT double SAM_Geothermal_AdjustmentFactors_adjust_constant_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_AdjustmentFactors_adjust_en_periods_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_AdjustmentFactors_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_AdjustmentFactors_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_AdjustmentFactors_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * FinancialParameters Getters
	 */

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_analysis_period_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Costs Getters
	 */

	SAM_EXPORT double SAM_Geothermal_Costs_geotherm_cost_plant_total_amount_specified_nget(SAM_table ptr, SAM_error *err);


	/**
	 * FinancialParameters Getters
	 */

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Geothermal_Outputs_GF_flowrate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_atb_drilling_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_atb_exploration_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_atb_plant_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_baseline_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_bottom_hole_pressure_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_brine_effectiveness_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_condensate_pump_power_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_conf_drilling_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_conf_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_interest1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_interest2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_interest3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_interest4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_interest5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_interest_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_percent_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_principal1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_principal2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_principal3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_principal4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_principal5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_principal_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_total1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_total2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_total3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_total4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_const_per_total5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_contingency_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_cp_battery_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_cp_system_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_cw_pump_head_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_cw_pump_work_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_cwflow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_Outputs_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_design_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_dt_rock_well_head_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_eff_secondlaw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_engineering_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_epc_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_expl_drilling_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_expl_per_well_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_expl_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_field_gathering_num_wells_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_first_year_output_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_flash_count_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_geothermal_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_gross_cost_output_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_gross_output_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_hp_flash_pressure_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_indirect_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_indirect_pump_gathering_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_inj_num_pumps_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_inj_pump_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_inj_pump_cost_per_pump_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_inj_pump_hp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_inj_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_inj_well_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_installed_cost_per_kW_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_lifetime_output_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_lp_flash_pressure_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_max_brine_effectiveness_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_Outputs_monthly_energy_lifetime_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_Outputs_monthly_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_Outputs_monthly_resource_temperature_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_ncg_condensate_pump_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_net_plant_output_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_num_confirm_wells_to_production_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_num_wells_getem_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_num_wells_getem_inj_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_num_wells_getem_inj_drilled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_num_wells_getem_output_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_num_wells_getem_prod_drilled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_num_wells_getem_prod_failed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_piping_cost_per_well_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_plant_brine_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_plm_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_pressure_ratio_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_pressure_ratio_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_pressure_ratio_3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_prod_inj_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_prod_pump_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_prod_pump_cost_per_well_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_prod_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_prod_well_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_pump_cost_install_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_pump_depth_ft_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_pump_hp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_pump_only_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_pump_size_hp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_pump_watthr_per_lb_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_pump_work_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_pumpwork_inj_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_pumpwork_prod_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_qCondenser_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_qRejectByStage_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_qRejectByStage_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_qRejectByStage_3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_qRejectTotal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_reservoir_avg_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_reservoir_pressure_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_sales_tax_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_spec_vol_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_spec_vol_lp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_stim_cost_non_drill_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_stim_cost_per_well_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_stim_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_sum_prod_inj_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_Outputs_system_lifetime_recapitalize_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_system_recapitalization_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_system_use_recapitalization_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_Outputs_timestep_dry_bulb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_Outputs_timestep_pressure_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_Outputs_timestep_resource_temperature_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_Outputs_timestep_test_values_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Geothermal_Outputs_timestep_wet_bulb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_total_capital_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_total_direct_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_total_drilling_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_total_drilling_cost_used_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_total_drilling_permitting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_total_expl_permitting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_total_gathering_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_total_getem_om_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_total_plant_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_total_plant_cost_used_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_total_pump_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_total_pump_gathering_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_total_pump_gathering_cost_used_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_total_surface_equipment_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_v_stage_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_v_stage_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_v_stage_3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_x_hp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Geothermal_Outputs_x_lp_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
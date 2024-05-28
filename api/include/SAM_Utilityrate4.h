#ifndef SAM_UTILITYRATE4_H_
#define SAM_UTILITYRATE4_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Utilityrate4 Technology Model
	//

	/** 
	 * Create a Utilityrate4 variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Utilityrate4;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Utilityrate4_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Common parameters
	//

	/**
	 * Set analysis_period: Number of years in analysis [years]
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_analysis_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set load_escalation: Annual load escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_load_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set rate_escalation: Annual electricity rate escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_rate_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Lifetime hourly system outputs [0/1]
	 * options: 0=hourly first year,1=hourly lifetime
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_annual_min_charge: Annual minimum charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_ur_annual_min_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_dc_enable: Enable demand charge [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_ur_dc_enable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_dc_flat_mat: Demand rates (flat) table
	 * options: None
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_ur_dc_flat_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekday: Demand charge weekday schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_ur_dc_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekend: Demand charge weekend schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_ur_dc_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_tou_mat: Demand rates (TOU) table
	 * options: None
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_ur_dc_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekday: Energy charge weekday schedule
	 * options: 12x24
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_ur_ec_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekend: Energy charge weekend schedule
	 * options: 12x24
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_ur_ec_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_tou_mat: Energy rates table
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_ur_ec_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_metering_option: Metering options [0=Single meter with monthly rollover credits in kWh,1=Single meter with monthly rollover credits in $,2=Single meter with no monthly rollover credits,3=Two meters with all generation sold and all load purchased]
	 * options: Net metering monthly excess
	 * constraints: INTEGER
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_ur_metering_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_monthly_fixed_charge: Monthly fixed charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_ur_monthly_fixed_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_monthly_min_charge: Monthly minimum charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_ur_monthly_min_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_yearend_sell_rate: Year end sell rate [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_ur_nm_yearend_sell_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_sell_eq_buy: Set sell rate equal to buy rate [0/1]
	 * options: Optional override
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate4_Common_ur_sell_eq_buy_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// TimeSeries parameters
	//

	/**
	 * Set gen: System power generated [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate4_TimeSeries_gen_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set load: Electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate4_TimeSeries_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// Financials parameters
	//

	/**
	 * Set inflation_rate: Inflation rate [%]
	 * options: None
	 * constraints: MIN=-99
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate4_Financials_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// AnnualOutput parameters
	//

	/**
	 * Set degradation: Annual energy degradation [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate4_AnnualOutput_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_Utilityrate4_Common_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Common_load_escalation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Common_rate_escalation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate4_Common_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate4_Common_ur_annual_min_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate4_Common_ur_dc_enable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Common_ur_dc_flat_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Common_ur_dc_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Common_ur_dc_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Common_ur_dc_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Common_ur_ec_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Common_ur_ec_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Common_ur_ec_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate4_Common_ur_metering_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate4_Common_ur_monthly_fixed_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate4_Common_ur_monthly_min_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate4_Common_ur_nm_yearend_sell_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate4_Common_ur_sell_eq_buy_nget(SAM_table ptr, SAM_error *err);


	/**
	 * TimeSeries Getters
	 */

	SAM_EXPORT double* SAM_Utilityrate4_TimeSeries_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_TimeSeries_load_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Financials Getters
	 */

	SAM_EXPORT double SAM_Utilityrate4_Financials_inflation_rate_nget(SAM_table ptr, SAM_error *err);


	/**
	 * AnnualOutput Getters
	 */

	SAM_EXPORT double* SAM_Utilityrate4_AnnualOutput_degradation_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_annual_electric_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_annual_energy_value_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_dc_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_dc_fixed_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_dc_tou_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_dc_tou_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_apr_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_aug_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_dec_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_feb_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_jan_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_jul_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_jun_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_mar_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_may_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_nov_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_oct_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_sep_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_fixed_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_minimum_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_minimum_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_dc_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_dc_fixed_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_dc_tou_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_dc_tou_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_apr_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_aug_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_dec_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_feb_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_jan_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_jul_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_jun_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_mar_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_may_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_nov_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_oct_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_sep_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_fixed_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_minimum_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_minimum_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_elec_cost_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate4_Outputs_elec_cost_with_system_year1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_elec_cost_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate4_Outputs_elec_cost_without_system_year1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_apr_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_aug_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_dec_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_feb_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_jan_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_jul_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_jun_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_mar_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_may_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_nov_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_oct_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_sep_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_apr_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_aug_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_dec_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_feb_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_jan_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_jul_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_jun_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_mar_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_may_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_nov_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_oct_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_sep_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_lifetime_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate4_Outputs_savings_year1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_apr_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_aug_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_dec_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_feb_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_jan_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_jul_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_jun_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_mar_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_may_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_nov_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_oct_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_sep_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_utility_bill_w_sys_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_utility_bill_w_sys_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_utility_bill_wo_sys_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_utility_bill_wo_sys_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate4_Outputs_year1_electric_load_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_dc_peak_per_period_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_dc_tou_schedule_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_dc_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_dc_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_e_fromgrid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_e_tofromgrid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_e_togrid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_ec_tou_schedule_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_ec_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_ec_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_p_system_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_p_tofromgrid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_salespurchases_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_salespurchases_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_system_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_cumulative_excess_dollars_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_cumulative_excess_generation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_dc_fixed_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_dc_fixed_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_dc_tou_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_dc_tou_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_ec_charge_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_ec_charge_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_electricity_to_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_fixed_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_fixed_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_minimum_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_minimum_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_peak_w_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_peak_wo_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_use_w_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_use_wo_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_utility_bill_w_sys_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_utility_bill_wo_sys_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
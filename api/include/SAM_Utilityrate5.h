#ifndef SAM_UTILITYRATE5_H_
#define SAM_UTILITYRATE5_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Utilityrate5 Technology Model
	//

	/** 
	 * Create a Utilityrate5 variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Utilityrate5;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Utilityrate5_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Lifetime parameters
	//

	/**
	 * Set analysis_period: Number of years in analysis [years]
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate5_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inflation_rate: Inflation rate [%]
	 * options: None
	 * constraints: MIN=-99
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate5_Lifetime_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Lifetime hourly system outputs [0/1]
	 * options: 0=hourly first year,1=hourly lifetime
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate5_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// ElectricityRates parameters
	//

	/**
	 * Set TOU_demand_single_peak: Use single monthly peak for TOU demand charge [0/1]
	 * options: 0=use TOU peak,1=use flat peak
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_TOU_demand_single_peak_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set en_electricity_rates: Optionally enable/disable electricity_rate [years]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_en_electricity_rates_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rate_escalation: Annual electricity rate escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_rate_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_annual_min_charge: Annual minimum charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_annual_min_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_billing_demand_lookback_percentages: Billing demand lookback percentages by month and consider actual peak demand
	 * options: 12x2
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_billing_demand_lookback_percentages_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_billing_demand_lookback_period: Billing demand lookback period [mn]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=12
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_billing_demand_lookback_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_billing_demand_minimum: Minimum billing demand
	 * options: None
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_billing_demand_minimum_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_dc_billing_demand_periods: Billing demand applicability to a given demand charge time of use period
	 * options: None
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_dc_billing_demand_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_enable: Enable demand charge [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_dc_enable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_dc_flat_mat: Demand rates (flat) table
	 * options: None
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_dc_flat_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekday: Demand charge weekday schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_dc_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekend: Demand charge weekend schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_dc_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_tou_mat: Demand rates (TOU) table
	 * options: None
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_dc_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekday: Energy charge weekday schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_ec_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekend: Energy charge weekend schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_ec_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_tou_mat: Energy rates table
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_ec_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_en_ts_buy_rate: Enable time step buy rates [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_en_ts_buy_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_en_ts_sell_rate: Enable time step sell rates [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_en_ts_sell_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_enable_billing_demand: Enable billing demand ratchets [0/1]
	 * options: 0=disable,1=enable
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_enable_billing_demand_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_metering_option: Metering options [0=net energy metering,1=net energy metering with $ credits,2=net billing,3=net billing with carryover to next month,4=buy all - sell all]
	 * options: Net metering monthly excess
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_metering_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_monthly_fixed_charge: Monthly fixed charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_monthly_fixed_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_monthly_min_charge: Monthly minimum charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_monthly_min_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_credit_month: Month of year end payout (true-up) [mn]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=11
	 * required if: ?=11
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_nm_credit_month_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_credit_rollover: Apply net metering true-up credits to future bills [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_nm_credit_rollover_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_yearend_sell_rate: Net metering true-up credit sell rate [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_nm_yearend_sell_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_sell_eq_buy: Set sell rate equal to buy rate [0/1]
	 * options: Optional override
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_sell_eq_buy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_ts_buy_rate: Time step buy rates [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_ts_buy_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_ts_sell_rate: Time step sell rates [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_ts_sell_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_yearzero_usage_peaks: Peak usage by month for year zero
	 * options: 12
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricityRates_ur_yearzero_usage_peaks_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// SystemOutput parameters
	//

	/**
	 * Set degradation: Annual energy degradation [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate5_SystemOutput_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set gen: System power generated [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate5_SystemOutput_gen_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// Load parameters
	//

	/**
	 * Set load: Electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate5_Load_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set load_escalation: Annual load escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_Load_load_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double SAM_Utilityrate5_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_Lifetime_inflation_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err);


	/**
	 * ElectricityRates Getters
	 */

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_TOU_demand_single_peak_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_en_electricity_rates_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_ElectricityRates_rate_escalation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_ur_annual_min_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_ElectricityRates_ur_billing_demand_lookback_percentages_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_ur_billing_demand_lookback_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_ur_billing_demand_minimum_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_ElectricityRates_ur_dc_billing_demand_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_ur_dc_enable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_ElectricityRates_ur_dc_flat_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_ElectricityRates_ur_dc_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_ElectricityRates_ur_dc_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_ElectricityRates_ur_dc_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_ElectricityRates_ur_ec_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_ElectricityRates_ur_ec_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_ElectricityRates_ur_ec_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_ur_en_ts_buy_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_ur_en_ts_sell_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_ur_enable_billing_demand_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_ur_metering_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_ur_monthly_fixed_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_ur_monthly_min_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_ur_nm_credit_month_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_ur_nm_credit_rollover_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_ur_nm_yearend_sell_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_ElectricityRates_ur_sell_eq_buy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_ElectricityRates_ur_ts_buy_rate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_ElectricityRates_ur_ts_sell_rate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_ElectricityRates_ur_yearzero_usage_peaks_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * SystemOutput Getters
	 */

	SAM_EXPORT double* SAM_Utilityrate5_SystemOutput_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_SystemOutput_gen_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Load Getters
	 */

	SAM_EXPORT double* SAM_Utilityrate5_Load_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Load_load_escalation_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_annual_electric_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_annual_energy_value_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_bill_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_billing_demand_w_sys_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_billing_demand_wo_sys_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_dc_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_dc_fixed_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_dc_tou_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_dc_tou_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_apr_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_aug_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_dec_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_feb_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_gross_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_jan_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_jul_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_jun_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_mar_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_may_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_nov_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_oct_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_sep_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_fixed_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_minimum_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_minimum_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_dc_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_dc_fixed_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_dc_tou_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_dc_tou_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_apr_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_aug_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_dec_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_feb_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_jan_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_jul_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_jun_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_mar_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_may_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_nov_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_oct_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_sep_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_fixed_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_minimum_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_minimum_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_elec_cost_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_Outputs_elec_cost_with_system_year1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_elec_cost_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_Outputs_elec_cost_without_system_year1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_apr_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_aug_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_dec_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_feb_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_jan_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_jul_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_jun_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_mar_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_may_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_nov_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_oct_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_sep_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_apr_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_aug_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_dec_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_feb_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_jan_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_jul_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_jun_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_mar_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_may_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_nov_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_oct_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_sep_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_excess_kwhs_earned_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_lifetime_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_monthly_tou_demand_charge_w_sys_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_monthly_tou_demand_charge_wo_sys_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_monthly_tou_demand_peak_w_sys_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_monthly_tou_demand_peak_wo_sys_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_net_billing_credits_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_nm_dollars_applied_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_Outputs_savings_year1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_apr_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_aug_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_dec_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_feb_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_jan_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_jul_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_jun_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_mar_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_may_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_nov_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_oct_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_sep_tp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_true_up_credits_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_two_meter_sales_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_utility_bill_w_sys_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_utility_bill_w_sys_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_utility_bill_wo_sys_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_utility_bill_wo_sys_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_billing_demand_w_sys_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_billing_demand_wo_sys_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_Outputs_year1_electric_load_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_excess_kwhs_earned_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_dc_peak_per_period_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_dc_tou_schedule_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_dc_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_dc_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_e_fromgrid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_e_tofromgrid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_e_togrid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_ec_tou_schedule_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_ec_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_ec_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_p_system_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_p_tofromgrid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_salespurchases_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_salespurchases_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_system_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_cumulative_excess_generation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_dc_fixed_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_dc_fixed_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_dc_tou_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_dc_tou_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_ec_charge_gross_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_ec_charge_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_ec_charge_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_electricity_to_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_fixed_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_fixed_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_minimum_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_minimum_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_peak_w_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_peak_wo_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_use_w_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_use_wo_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_utility_bill_w_sys_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_utility_bill_wo_sys_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_net_billing_credits_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_nm_dollars_applied_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_true_up_credits_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_two_meter_sales_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
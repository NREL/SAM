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

	SAM_EXPORT SAM_Utilityrate5 SAM_Utilityrate5_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Utilityrate5_execute(SAM_Utilityrate5 data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Utilityrate5_destruct(SAM_Utilityrate5 system);


	//
	// Common parameters
	//

	/**
	 * Set TOU_demand_single_peak: Use single monthly peak for TOU demand charge [0/1]
	 * options: 0=use TOU peak,1=use flat peak
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_Common_TOU_demand_single_peak_nset(SAM_Utilityrate5 ptr, double number, SAM_error *err);

	/**
	 * Set en_electricity_rates: Optionally enable/disable electricity_rate [years]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate5_Common_en_electricity_rates_nset(SAM_Utilityrate5 ptr, double number, SAM_error *err);

	/**
	 * Set ur_sell_eq_buy: Set sell rate equal to buy rate [0/1]
	 * options: Optional override
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_Common_ur_sell_eq_buy_nset(SAM_Utilityrate5 ptr, double number, SAM_error *err);


	//
	// Lifetime parameters
	//

	/**
	 * Set analysis_period: Number of years in analysis [years]
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate5_Lifetime_analysis_period_nset(SAM_Utilityrate5 ptr, double number, SAM_error *err);

	/**
	 * Set inflation_rate: Inflation rate [%]
	 * options: None
	 * constraints: MIN=-99
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate5_Lifetime_inflation_rate_nset(SAM_Utilityrate5 ptr, double number, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Lifetime hourly system outputs [0/1]
	 * options: 0=hourly first year,1=hourly lifetime
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate5_Lifetime_system_use_lifetime_output_nset(SAM_Utilityrate5 ptr, double number, SAM_error *err);


	//
	// SystemOutput parameters
	//

	/**
	 * Set degradation: Annual energy degradation [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate5_SystemOutput_degradation_aset(SAM_Utilityrate5 ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set gen: System power generated [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate5_SystemOutput_gen_aset(SAM_Utilityrate5 ptr, double* arr, int length, SAM_error *err);


	//
	// TimeSeries parameters
	//

	/**
	 * Set load: Electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate5_TimeSeries_load_aset(SAM_Utilityrate5 ptr, double* arr, int length, SAM_error *err);


	//
	// ElectricLoad parameters
	//

	/**
	 * Set load_escalation: Annual load escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_ElectricLoad_load_escalation_aset(SAM_Utilityrate5 ptr, double* arr, int length, SAM_error *err);


	//
	// UtilityRateFlat parameters
	//

	/**
	 * Set rate_escalation: Annual electricity rate escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateFlat_rate_escalation_aset(SAM_Utilityrate5 ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_annual_min_charge: Annual minimum charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateFlat_ur_annual_min_charge_nset(SAM_Utilityrate5 ptr, double number, SAM_error *err);

	/**
	 * Set ur_en_ts_sell_rate: Enable time step sell rates [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateFlat_ur_en_ts_sell_rate_nset(SAM_Utilityrate5 ptr, double number, SAM_error *err);

	/**
	 * Set ur_metering_option: Metering options [0=Single meter with monthly rollover credits in kWh,1=Single meter with monthly rollover credits in $,2=Single meter with no monthly rollover credits (Net Billing),3=Single meter with monthly rollover credits in $ (Net Billing $),4=Two meters with all generation sold and all load purchased]
	 * options: Net metering monthly excess
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateFlat_ur_metering_option_nset(SAM_Utilityrate5 ptr, double number, SAM_error *err);

	/**
	 * Set ur_monthly_fixed_charge: Monthly fixed charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateFlat_ur_monthly_fixed_charge_nset(SAM_Utilityrate5 ptr, double number, SAM_error *err);

	/**
	 * Set ur_monthly_min_charge: Monthly minimum charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateFlat_ur_monthly_min_charge_nset(SAM_Utilityrate5 ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_yearend_sell_rate: Year end sell rate [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateFlat_ur_nm_yearend_sell_rate_nset(SAM_Utilityrate5 ptr, double number, SAM_error *err);

	/**
	 * Set ur_ts_buy_rate: Time step buy rates [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateFlat_ur_ts_buy_rate_aset(SAM_Utilityrate5 ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_ts_sell_rate: Time step sell rates [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateFlat_ur_ts_sell_rate_aset(SAM_Utilityrate5 ptr, double* arr, int length, SAM_error *err);


	//
	// UtilityRateEnergyCharge parameters
	//

	/**
	 * Set ur_ec_sched_weekday: Energy charge weekday schedule
	 * options: 12x24
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateEnergyCharge_ur_ec_sched_weekday_mset(SAM_Utilityrate5 ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekend: Energy charge weekend schedule
	 * options: 12x24
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateEnergyCharge_ur_ec_sched_weekend_mset(SAM_Utilityrate5 ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_tou_mat: Energy rates table
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateEnergyCharge_ur_ec_tou_mat_mset(SAM_Utilityrate5 ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// UtilityRateDemandCharge parameters
	//

	/**
	 * Set ur_dc_enable: Enable demand charge [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_enable_nset(SAM_Utilityrate5 ptr, double number, SAM_error *err);

	/**
	 * Set ur_dc_flat_mat: Demand rates (flat) table
	 * options: None
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_flat_mat_mset(SAM_Utilityrate5 ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekday: Demand charge weekday schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_sched_weekday_mset(SAM_Utilityrate5 ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekend: Demand charge weekend schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_sched_weekend_mset(SAM_Utilityrate5 ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_tou_mat: Demand rates (TOU) table
	 * options: None
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_tou_mat_mset(SAM_Utilityrate5 ptr, double* mat, int nrows, int ncols, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_Utilityrate5_Common_TOU_demand_single_peak_nget(SAM_Utilityrate5 ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_Common_en_electricity_rates_nget(SAM_Utilityrate5 ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_Common_ur_sell_eq_buy_nget(SAM_Utilityrate5 ptr, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double SAM_Utilityrate5_Lifetime_analysis_period_nget(SAM_Utilityrate5 ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_Lifetime_inflation_rate_nget(SAM_Utilityrate5 ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_Lifetime_system_use_lifetime_output_nget(SAM_Utilityrate5 ptr, SAM_error *err);


	/**
	 * SystemOutput Getters
	 */

	SAM_EXPORT double* SAM_Utilityrate5_SystemOutput_degradation_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_SystemOutput_gen_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);


	/**
	 * TimeSeries Getters
	 */

	SAM_EXPORT double* SAM_Utilityrate5_TimeSeries_load_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);


	/**
	 * ElectricLoad Getters
	 */

	SAM_EXPORT double* SAM_Utilityrate5_ElectricLoad_load_escalation_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);


	/**
	 * UtilityRateFlat Getters
	 */

	SAM_EXPORT double* SAM_Utilityrate5_UtilityRateFlat_rate_escalation_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_UtilityRateFlat_ur_annual_min_charge_nget(SAM_Utilityrate5 ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_UtilityRateFlat_ur_en_ts_sell_rate_nget(SAM_Utilityrate5 ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_UtilityRateFlat_ur_metering_option_nget(SAM_Utilityrate5 ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_UtilityRateFlat_ur_monthly_fixed_charge_nget(SAM_Utilityrate5 ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_UtilityRateFlat_ur_monthly_min_charge_nget(SAM_Utilityrate5 ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_UtilityRateFlat_ur_nm_yearend_sell_rate_nget(SAM_Utilityrate5 ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_UtilityRateFlat_ur_ts_buy_rate_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_UtilityRateFlat_ur_ts_sell_rate_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);


	/**
	 * UtilityRateEnergyCharge Getters
	 */

	SAM_EXPORT double* SAM_Utilityrate5_UtilityRateEnergyCharge_ur_ec_sched_weekday_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_UtilityRateEnergyCharge_ur_ec_sched_weekend_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_UtilityRateEnergyCharge_ur_ec_tou_mat_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * UtilityRateDemandCharge Getters
	 */

	SAM_EXPORT double SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_enable_nget(SAM_Utilityrate5 ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_flat_mat_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_sched_weekday_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_sched_weekend_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_tou_mat_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_annual_electric_load_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_annual_energy_value_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_bill_load_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_dc_fixed_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_dc_fixed_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_dc_tou_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_dc_tou_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_apr_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_aug_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_dec_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_feb_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_gross_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_jan_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_jul_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_jun_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_mar_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_may_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_nov_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_oct_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_sep_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_ec_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_fixed_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_fixed_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_minimum_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_w_sys_minimum_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_dc_fixed_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_dc_fixed_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_dc_tou_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_dc_tou_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_apr_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_aug_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_dec_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_feb_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_jan_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_jul_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_jun_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_mar_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_may_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_nov_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_oct_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_sep_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_ec_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_fixed_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_fixed_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_minimum_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_charge_wo_sys_minimum_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_elec_cost_with_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_Outputs_elec_cost_with_system_year1_nget(SAM_Utilityrate5 ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_elec_cost_without_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_Outputs_elec_cost_without_system_year1_nget(SAM_Utilityrate5 ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_apr_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_aug_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_dec_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_feb_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_jan_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_jul_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_jun_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_mar_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_may_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_nov_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_oct_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_w_sys_ec_sep_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_apr_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_aug_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_dec_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_feb_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_jan_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_jul_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_jun_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_mar_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_may_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_nov_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_oct_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_energy_wo_sys_ec_sep_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_excess_dollars_applied_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_excess_dollars_earned_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_excess_kwhs_applied_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_excess_kwhs_earned_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_lifetime_load_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_monthly_tou_demand_charge_w_sys_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_monthly_tou_demand_charge_wo_sys_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_monthly_tou_demand_peak_w_sys_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_monthly_tou_demand_peak_wo_sys_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_Outputs_savings_year1_nget(SAM_Utilityrate5 ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_apr_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_aug_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_dec_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_feb_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_jan_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_jul_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_jun_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_mar_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_may_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_nov_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_oct_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_surplus_w_sys_ec_sep_tp_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_utility_bill_w_sys_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_utility_bill_w_sys_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_utility_bill_wo_sys_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_utility_bill_wo_sys_ym_mget(SAM_Utilityrate5 ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrate5_Outputs_year1_electric_load_nget(SAM_Utilityrate5 ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_excess_dollars_applied_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_excess_dollars_earned_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_excess_kwhs_applied_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_excess_kwhs_earned_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_dc_peak_per_period_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_dc_tou_schedule_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_dc_with_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_dc_without_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_e_fromgrid_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_e_tofromgrid_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_e_togrid_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_ec_tou_schedule_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_ec_with_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_ec_without_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_p_system_to_load_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_p_tofromgrid_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_salespurchases_with_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_salespurchases_without_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_hourly_system_to_load_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_cumulative_excess_dollars_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_cumulative_excess_generation_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_dc_fixed_with_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_dc_fixed_without_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_dc_tou_with_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_dc_tou_without_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_ec_charge_gross_with_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_ec_charge_with_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_ec_charge_without_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_electricity_to_grid_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_fixed_with_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_fixed_without_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_load_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_minimum_with_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_minimum_without_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_peak_w_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_peak_wo_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_use_w_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_use_wo_system_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_utility_bill_w_sys_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrate5_Outputs_year1_monthly_utility_bill_wo_sys_aget(SAM_Utilityrate5 ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
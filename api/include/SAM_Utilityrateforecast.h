#ifndef SAM_UTILITYRATEFORECAST_H_
#define SAM_UTILITYRATEFORECAST_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Utilityrateforecast Technology Model
	//

	/** 
	 * Create a Utilityrateforecast variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Utilityrateforecast;

	SAM_EXPORT SAM_Utilityrateforecast SAM_Utilityrateforecast_setup(SAM_table data, SAM_error* err);


	//
	// ElectricityRates parameters
	//

	/**
	 * Set en_electricity_rates: Optionally enable/disable electricity_rate [years]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_en_electricity_rates_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set gen: Lifetime generation forecast
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_gen_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set grid_power: Electricity to/from grid
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_grid_power_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set load: Lifetime load forecast
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set rate_escalation: Annual electricity rate escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_rate_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_annual_min_charge: Annual minimum charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_annual_min_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_billing_demand_lookback_percentages: Billing demand lookback percentages by month and consider actual peak demand
	 * options: 12x2
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_billing_demand_lookback_percentages_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_billing_demand_lookback_period: Billing demand lookback period [mn]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=12
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_billing_demand_lookback_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_billing_demand_minimum: Minimum billing demand
	 * options: None
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_billing_demand_minimum_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_dc_billing_demand_periods: Billing demand applicability to a given demand charge time of use period
	 * options: None
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_dc_billing_demand_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_enable: Enable demand charge [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_dc_enable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_dc_flat_mat: Demand rates (flat) table
	 * options: None
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_dc_flat_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_peaks: Peak demand by month and period
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_dc_peaks_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekday: Demand charge weekday schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_dc_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekend: Demand charge weekend schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_dc_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_tou_mat: Demand rates (TOU) table
	 * options: None
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_dc_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekday: Energy charge weekday schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_ec_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekend: Energy charge weekend schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_ec_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_tou_mat: Energy rates table
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_ec_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_en_ts_buy_rate: Enable time step buy rates [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_en_ts_buy_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_en_ts_sell_rate: Enable time step sell rates [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_en_ts_sell_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_enable_billing_demand: Enable billing demand ratchets [0/1]
	 * options: 0=disable,1=enable
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_enable_billing_demand_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_energy_use: Energy use or surplus by month and period
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_energy_use_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_metering_option: Metering options [0=net energy metering,1=net energy metering with $ credits,2=net billing,3=net billing with carryover to next month,4=buy all - sell all]
	 * options: Net metering monthly excess
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_metering_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_monthly_fixed_charge: Monthly fixed charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_monthly_fixed_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_monthly_min_charge: Monthly minimum charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_monthly_min_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_credit_month: Month of year end payout (true-up) [mn]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=11
	 * required if: ?=11
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_nm_credit_month_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_credit_rollover: Apply net metering true-up credits to future bills [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_nm_credit_rollover_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_yearend_sell_rate: Net metering true-up credit sell rate [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_nm_yearend_sell_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_sell_eq_buy: Set sell rate equal to buy rate [0/1]
	 * options: Optional override
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_sell_eq_buy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_ts_buy_rate: Time step buy rates [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_ts_buy_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_ts_sell_rate: Time step sell rates [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_ts_sell_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_yearzero_usage_peaks: Peak usage by month for year zero
	 * options: 12
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_yearzero_usage_peaks_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// Lifetime parameters
	//

	/**
	 * Set analysis_period: Number of years in escalation and forecast [years]
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inflation_rate: Inflation rate [%]
	 * options: None
	 * constraints: MIN=-99
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_Lifetime_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Controls parameters
	//

	/**
	 * Set idx: Starting index (lifetime)
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_Controls_idx_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set steps_per_hour: Steps per hour [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Utilityrateforecast_Controls_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * ElectricityRates Getters
	 */

	SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_en_electricity_rates_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_grid_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_rate_escalation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_annual_min_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_billing_demand_lookback_percentages_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_billing_demand_lookback_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_billing_demand_minimum_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_dc_billing_demand_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_dc_enable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_dc_flat_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_dc_peaks_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_dc_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_dc_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_dc_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_ec_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_ec_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_ec_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_en_ts_buy_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_en_ts_sell_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_enable_billing_demand_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_energy_use_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_metering_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_monthly_fixed_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_monthly_min_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_nm_credit_month_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_nm_credit_rollover_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_nm_yearend_sell_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_sell_eq_buy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_ts_buy_rate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_ts_sell_rate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_yearzero_usage_peaks_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double SAM_Utilityrateforecast_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_Lifetime_inflation_rate_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Controls Getters
	 */

	SAM_EXPORT double SAM_Utilityrateforecast_Controls_idx_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_Controls_steps_per_hour_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Utilityrateforecast_Outputs_ur_price_series_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Utilityrateforecast_Outputs_ur_total_bill_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
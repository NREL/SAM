#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Utilityrateforecast.h"

SAM_EXPORT SAM_Utilityrateforecast SAM_Utilityrateforecast_setup(SAM_table data, SAM_error* err){
	return SAM_stateful_module_setup("utilityrateforecast", data, err);
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_en_electricity_rates_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_electricity_rates", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_gen_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "gen", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_grid_power_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "grid_power", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "load", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_rate_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "rate_escalation", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_annual_min_charge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_annual_min_charge", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_billing_demand_lookback_percentages_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_billing_demand_lookback_percentages", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_billing_demand_lookback_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_billing_demand_lookback_period", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_billing_demand_minimum_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_billing_demand_minimum", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_dc_billing_demand_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_billing_demand_periods", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_dc_enable_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_enable", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_dc_flat_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_flat_mat", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_dc_peaks_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_peaks", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_dc_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_dc_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_dc_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_tou_mat", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_ec_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_ec_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_ec_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_tou_mat", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_en_ts_buy_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_en_ts_buy_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_en_ts_sell_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_en_ts_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_enable_billing_demand_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_enable_billing_demand", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_energy_use_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_energy_use", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_metering_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_metering_option", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_monthly_fixed_charge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_monthly_fixed_charge", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_monthly_min_charge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_monthly_min_charge", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_nm_credit_month_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_nm_credit_month", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_nm_credit_rollover_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_nm_credit_rollover", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_nm_yearend_sell_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_nm_yearend_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_sell_eq_buy_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_sell_eq_buy", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_ts_buy_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ur_ts_buy_rate", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_ts_sell_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ur_ts_sell_rate", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_ElectricityRates_ur_yearzero_usage_peaks_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ur_yearzero_usage_peaks", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_Lifetime_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inflation_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_Controls_idx_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "idx", number);
	});
}

SAM_EXPORT void SAM_Utilityrateforecast_Controls_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "steps_per_hour", number);
	});
}

SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_en_electricity_rates_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_electricity_rates", &result))
		make_access_error("SAM_Utilityrateforecast", "en_electricity_rates");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_grid_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "grid_power", length);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "grid_power");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "load", length);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "load");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_rate_escalation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rate_escalation", length);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "rate_escalation");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_annual_min_charge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_annual_min_charge", &result))
		make_access_error("SAM_Utilityrateforecast", "ur_annual_min_charge");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_billing_demand_lookback_percentages_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_billing_demand_lookback_percentages", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "ur_billing_demand_lookback_percentages");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_billing_demand_lookback_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_billing_demand_lookback_period", &result))
		make_access_error("SAM_Utilityrateforecast", "ur_billing_demand_lookback_period");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_billing_demand_minimum_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_billing_demand_minimum", &result))
		make_access_error("SAM_Utilityrateforecast", "ur_billing_demand_minimum");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_dc_billing_demand_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_billing_demand_periods", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "ur_dc_billing_demand_periods");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_dc_enable_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_enable", &result))
		make_access_error("SAM_Utilityrateforecast", "ur_dc_enable");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_dc_flat_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_flat_mat", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "ur_dc_flat_mat");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_dc_peaks_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_peaks", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "ur_dc_peaks");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_dc_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "ur_dc_sched_weekday");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_dc_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "ur_dc_sched_weekend");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_dc_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_tou_mat", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "ur_dc_tou_mat");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_ec_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "ur_ec_sched_weekday");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_ec_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "ur_ec_sched_weekend");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_ec_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_tou_mat", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "ur_ec_tou_mat");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_en_ts_buy_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_en_ts_buy_rate", &result))
		make_access_error("SAM_Utilityrateforecast", "ur_en_ts_buy_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_en_ts_sell_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_en_ts_sell_rate", &result))
		make_access_error("SAM_Utilityrateforecast", "ur_en_ts_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_enable_billing_demand_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_enable_billing_demand", &result))
		make_access_error("SAM_Utilityrateforecast", "ur_enable_billing_demand");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_energy_use_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_energy_use", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "ur_energy_use");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_metering_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_metering_option", &result))
		make_access_error("SAM_Utilityrateforecast", "ur_metering_option");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_monthly_fixed_charge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_monthly_fixed_charge", &result))
		make_access_error("SAM_Utilityrateforecast", "ur_monthly_fixed_charge");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_monthly_min_charge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_monthly_min_charge", &result))
		make_access_error("SAM_Utilityrateforecast", "ur_monthly_min_charge");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_nm_credit_month_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_nm_credit_month", &result))
		make_access_error("SAM_Utilityrateforecast", "ur_nm_credit_month");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_nm_credit_rollover_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_nm_credit_rollover", &result))
		make_access_error("SAM_Utilityrateforecast", "ur_nm_credit_rollover");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_nm_yearend_sell_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_nm_yearend_sell_rate", &result))
		make_access_error("SAM_Utilityrateforecast", "ur_nm_yearend_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_ElectricityRates_ur_sell_eq_buy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_sell_eq_buy", &result))
		make_access_error("SAM_Utilityrateforecast", "ur_sell_eq_buy");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_ts_buy_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ur_ts_buy_rate", length);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "ur_ts_buy_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_ts_sell_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ur_ts_sell_rate", length);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "ur_ts_sell_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_ElectricityRates_ur_yearzero_usage_peaks_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ur_yearzero_usage_peaks", length);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "ur_yearzero_usage_peaks");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_Utilityrateforecast", "analysis_period");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_Lifetime_inflation_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inflation_rate", &result))
		make_access_error("SAM_Utilityrateforecast", "inflation_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_Controls_idx_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "idx", &result))
		make_access_error("SAM_Utilityrateforecast", "idx");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_Controls_steps_per_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "steps_per_hour", &result))
		make_access_error("SAM_Utilityrateforecast", "steps_per_hour");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrateforecast_Outputs_ur_price_series_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ur_price_series", length);
	if (!result)
		make_access_error("SAM_Utilityrateforecast", "ur_price_series");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrateforecast_Outputs_ur_total_bill_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_total_bill", &result))
		make_access_error("SAM_Utilityrateforecast", "ur_total_bill");
	});
	return result;
}




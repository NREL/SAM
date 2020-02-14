#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Utilityrate4.h"

SAM_EXPORT SAM_Utilityrate4 SAM_Utilityrate4_construct(const char* def, SAM_error* err){
	SAM_Utilityrate4 result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_Utilityrate4_execute(SAM_Utilityrate4 data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("utilityrate4", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Utilityrate4_destruct(SAM_Utilityrate4 system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_Utilityrate4_Common_analysis_period_nset(SAM_Utilityrate4 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_load_escalation_aset(SAM_Utilityrate4 ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "load_escalation", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_rate_escalation_aset(SAM_Utilityrate4 ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "rate_escalation", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_system_use_lifetime_output_nset(SAM_Utilityrate4 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_use_lifetime_output", number);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_ur_annual_min_charge_nset(SAM_Utilityrate4 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_annual_min_charge", number);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_ur_dc_enable_nset(SAM_Utilityrate4 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_enable", number);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_ur_dc_flat_mat_mset(SAM_Utilityrate4 ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_flat_mat", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_ur_dc_sched_weekday_mset(SAM_Utilityrate4 ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_ur_dc_sched_weekend_mset(SAM_Utilityrate4 ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_ur_dc_tou_mat_mset(SAM_Utilityrate4 ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_tou_mat", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_ur_ec_sched_weekday_mset(SAM_Utilityrate4 ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_ur_ec_sched_weekend_mset(SAM_Utilityrate4 ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_ur_ec_tou_mat_mset(SAM_Utilityrate4 ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_tou_mat", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_ur_metering_option_nset(SAM_Utilityrate4 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_metering_option", number);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_ur_monthly_fixed_charge_nset(SAM_Utilityrate4 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_monthly_fixed_charge", number);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_ur_monthly_min_charge_nset(SAM_Utilityrate4 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_monthly_min_charge", number);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_ur_nm_yearend_sell_rate_nset(SAM_Utilityrate4 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_nm_yearend_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Common_ur_sell_eq_buy_nset(SAM_Utilityrate4 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_sell_eq_buy", number);
	});
}

SAM_EXPORT void SAM_Utilityrate4_TimeSeries_gen_aset(SAM_Utilityrate4 ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "gen", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate4_TimeSeries_load_aset(SAM_Utilityrate4 ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "load", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate4_Financials_inflation_rate_nset(SAM_Utilityrate4 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inflation_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate4_AnnualOutput_degradation_aset(SAM_Utilityrate4 ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "degradation", arr, length);
	});
}

SAM_EXPORT double SAM_Utilityrate4_Common_analysis_period_nget(SAM_Utilityrate4 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_Utilityrate4", "analysis_period");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Common_load_escalation_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "load_escalation", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "load_escalation");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Common_rate_escalation_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rate_escalation", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "rate_escalation");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate4_Common_system_use_lifetime_output_nget(SAM_Utilityrate4 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_Utilityrate4", "system_use_lifetime_output");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate4_Common_ur_annual_min_charge_nget(SAM_Utilityrate4 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_annual_min_charge", &result))
		make_access_error("SAM_Utilityrate4", "ur_annual_min_charge");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate4_Common_ur_dc_enable_nget(SAM_Utilityrate4 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_enable", &result))
		make_access_error("SAM_Utilityrate4", "ur_dc_enable");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Common_ur_dc_flat_mat_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_flat_mat", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "ur_dc_flat_mat");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Common_ur_dc_sched_weekday_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "ur_dc_sched_weekday");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Common_ur_dc_sched_weekend_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "ur_dc_sched_weekend");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Common_ur_dc_tou_mat_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_tou_mat", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "ur_dc_tou_mat");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Common_ur_ec_sched_weekday_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "ur_ec_sched_weekday");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Common_ur_ec_sched_weekend_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "ur_ec_sched_weekend");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Common_ur_ec_tou_mat_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_tou_mat", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "ur_ec_tou_mat");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate4_Common_ur_metering_option_nget(SAM_Utilityrate4 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_metering_option", &result))
		make_access_error("SAM_Utilityrate4", "ur_metering_option");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate4_Common_ur_monthly_fixed_charge_nget(SAM_Utilityrate4 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_monthly_fixed_charge", &result))
		make_access_error("SAM_Utilityrate4", "ur_monthly_fixed_charge");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate4_Common_ur_monthly_min_charge_nget(SAM_Utilityrate4 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_monthly_min_charge", &result))
		make_access_error("SAM_Utilityrate4", "ur_monthly_min_charge");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate4_Common_ur_nm_yearend_sell_rate_nget(SAM_Utilityrate4 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_nm_yearend_sell_rate", &result))
		make_access_error("SAM_Utilityrate4", "ur_nm_yearend_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate4_Common_ur_sell_eq_buy_nget(SAM_Utilityrate4 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_sell_eq_buy", &result))
		make_access_error("SAM_Utilityrate4", "ur_sell_eq_buy");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_TimeSeries_gen_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_TimeSeries_load_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "load", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "load");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate4_Financials_inflation_rate_nget(SAM_Utilityrate4 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inflation_rate", &result))
		make_access_error("SAM_Utilityrate4", "inflation_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_AnnualOutput_degradation_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "degradation", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "degradation");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_annual_electric_load_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_electric_load", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "annual_electric_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_annual_energy_value_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_energy_value", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "annual_energy_value");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_dc_fixed_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_w_sys_dc_fixed", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_dc_fixed");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_dc_fixed_ym_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_dc_fixed_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_dc_fixed_ym");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_dc_tou_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_w_sys_dc_tou", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_dc_tou");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_dc_tou_ym_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_dc_tou_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_dc_tou_ym");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_w_sys_ec", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_ec");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_apr_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_apr_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_ec_apr_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_aug_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_aug_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_ec_aug_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_dec_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_dec_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_ec_dec_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_feb_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_feb_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_ec_feb_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_jan_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_jan_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_ec_jan_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_jul_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_jul_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_ec_jul_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_jun_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_jun_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_ec_jun_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_mar_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_mar_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_ec_mar_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_may_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_may_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_ec_may_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_nov_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_nov_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_ec_nov_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_oct_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_oct_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_ec_oct_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_sep_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_sep_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_ec_sep_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_ec_ym_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_ec_ym");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_fixed_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_w_sys_fixed", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_fixed");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_fixed_ym_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_fixed_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_fixed_ym");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_minimum_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_w_sys_minimum", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_minimum");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_w_sys_minimum_ym_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_minimum_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_w_sys_minimum_ym");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_dc_fixed_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_wo_sys_dc_fixed", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_dc_fixed");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_dc_fixed_ym_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_dc_fixed_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_dc_fixed_ym");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_dc_tou_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_wo_sys_dc_tou", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_dc_tou");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_dc_tou_ym_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_dc_tou_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_dc_tou_ym");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_wo_sys_ec", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_ec");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_apr_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_ec_apr_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_ec_apr_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_aug_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_ec_aug_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_ec_aug_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_dec_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_ec_dec_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_ec_dec_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_feb_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_ec_feb_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_ec_feb_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_jan_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_ec_jan_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_ec_jan_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_jul_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_ec_jul_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_ec_jul_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_jun_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_ec_jun_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_ec_jun_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_mar_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_ec_mar_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_ec_mar_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_may_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_ec_may_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_ec_may_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_nov_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_ec_nov_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_ec_nov_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_oct_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_ec_oct_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_ec_oct_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_sep_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_ec_sep_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_ec_sep_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_ec_ym_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_ec_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_ec_ym");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_fixed_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_wo_sys_fixed", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_fixed");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_fixed_ym_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_fixed_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_fixed_ym");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_minimum_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_wo_sys_minimum", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_minimum");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_charge_wo_sys_minimum_ym_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_wo_sys_minimum_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "charge_wo_sys_minimum_ym");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_elec_cost_with_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "elec_cost_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "elec_cost_with_system");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate4_Outputs_elec_cost_with_system_year1_nget(SAM_Utilityrate4 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "elec_cost_with_system_year1", &result))
		make_access_error("SAM_Utilityrate4", "elec_cost_with_system_year1");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_elec_cost_without_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "elec_cost_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "elec_cost_without_system");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate4_Outputs_elec_cost_without_system_year1_nget(SAM_Utilityrate4 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "elec_cost_without_system_year1", &result))
		make_access_error("SAM_Utilityrate4", "elec_cost_without_system_year1");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_apr_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_w_sys_ec_apr_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_w_sys_ec_apr_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_aug_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_w_sys_ec_aug_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_w_sys_ec_aug_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_dec_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_w_sys_ec_dec_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_w_sys_ec_dec_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_feb_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_w_sys_ec_feb_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_w_sys_ec_feb_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_jan_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_w_sys_ec_jan_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_w_sys_ec_jan_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_jul_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_w_sys_ec_jul_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_w_sys_ec_jul_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_jun_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_w_sys_ec_jun_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_w_sys_ec_jun_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_mar_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_w_sys_ec_mar_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_w_sys_ec_mar_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_may_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_w_sys_ec_may_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_w_sys_ec_may_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_nov_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_w_sys_ec_nov_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_w_sys_ec_nov_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_oct_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_w_sys_ec_oct_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_w_sys_ec_oct_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_w_sys_ec_sep_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_w_sys_ec_sep_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_w_sys_ec_sep_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_apr_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_wo_sys_ec_apr_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_wo_sys_ec_apr_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_aug_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_wo_sys_ec_aug_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_wo_sys_ec_aug_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_dec_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_wo_sys_ec_dec_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_wo_sys_ec_dec_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_feb_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_wo_sys_ec_feb_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_wo_sys_ec_feb_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_jan_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_wo_sys_ec_jan_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_wo_sys_ec_jan_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_jul_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_wo_sys_ec_jul_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_wo_sys_ec_jul_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_jun_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_wo_sys_ec_jun_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_wo_sys_ec_jun_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_mar_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_wo_sys_ec_mar_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_wo_sys_ec_mar_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_may_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_wo_sys_ec_may_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_wo_sys_ec_may_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_nov_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_wo_sys_ec_nov_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_wo_sys_ec_nov_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_oct_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_wo_sys_ec_oct_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_wo_sys_ec_oct_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_energy_wo_sys_ec_sep_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "energy_wo_sys_ec_sep_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "energy_wo_sys_ec_sep_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_lifetime_load_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "lifetime_load", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "lifetime_load");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate4_Outputs_savings_year1_nget(SAM_Utilityrate4 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "savings_year1", &result))
		make_access_error("SAM_Utilityrate4", "savings_year1");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_apr_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "surplus_w_sys_ec_apr_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "surplus_w_sys_ec_apr_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_aug_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "surplus_w_sys_ec_aug_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "surplus_w_sys_ec_aug_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_dec_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "surplus_w_sys_ec_dec_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "surplus_w_sys_ec_dec_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_feb_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "surplus_w_sys_ec_feb_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "surplus_w_sys_ec_feb_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_jan_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "surplus_w_sys_ec_jan_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "surplus_w_sys_ec_jan_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_jul_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "surplus_w_sys_ec_jul_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "surplus_w_sys_ec_jul_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_jun_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "surplus_w_sys_ec_jun_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "surplus_w_sys_ec_jun_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_mar_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "surplus_w_sys_ec_mar_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "surplus_w_sys_ec_mar_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_may_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "surplus_w_sys_ec_may_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "surplus_w_sys_ec_may_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_nov_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "surplus_w_sys_ec_nov_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "surplus_w_sys_ec_nov_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_oct_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "surplus_w_sys_ec_oct_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "surplus_w_sys_ec_oct_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_surplus_w_sys_ec_sep_tp_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "surplus_w_sys_ec_sep_tp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "surplus_w_sys_ec_sep_tp");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_utility_bill_w_sys_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "utility_bill_w_sys", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "utility_bill_w_sys");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_utility_bill_w_sys_ym_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "utility_bill_w_sys_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "utility_bill_w_sys_ym");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_utility_bill_wo_sys_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "utility_bill_wo_sys", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "utility_bill_wo_sys");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_utility_bill_wo_sys_ym_mget(SAM_Utilityrate4 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "utility_bill_wo_sys_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate4", "utility_bill_wo_sys_ym");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate4_Outputs_year1_electric_load_nget(SAM_Utilityrate4 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "year1_electric_load", &result))
		make_access_error("SAM_Utilityrate4", "year1_electric_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_dc_peak_per_period_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_dc_peak_per_period", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_hourly_dc_peak_per_period");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_dc_tou_schedule_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_dc_tou_schedule", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_hourly_dc_tou_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_dc_with_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_dc_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_hourly_dc_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_dc_without_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_dc_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_hourly_dc_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_e_fromgrid_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_e_fromgrid", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_hourly_e_fromgrid");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_e_tofromgrid_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_e_tofromgrid", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_hourly_e_tofromgrid");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_e_togrid_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_e_togrid", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_hourly_e_togrid");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_ec_tou_schedule_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_ec_tou_schedule", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_hourly_ec_tou_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_ec_with_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_ec_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_hourly_ec_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_ec_without_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_ec_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_hourly_ec_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_p_system_to_load_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_p_system_to_load", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_hourly_p_system_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_p_tofromgrid_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_p_tofromgrid", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_hourly_p_tofromgrid");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_salespurchases_with_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_salespurchases_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_hourly_salespurchases_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_salespurchases_without_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_salespurchases_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_hourly_salespurchases_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_hourly_system_to_load_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_system_to_load", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_hourly_system_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_cumulative_excess_dollars_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_cumulative_excess_dollars", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_cumulative_excess_dollars");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_cumulative_excess_generation_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_cumulative_excess_generation", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_cumulative_excess_generation");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_dc_fixed_with_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_dc_fixed_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_dc_fixed_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_dc_fixed_without_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_dc_fixed_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_dc_fixed_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_dc_tou_with_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_dc_tou_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_dc_tou_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_dc_tou_without_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_dc_tou_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_dc_tou_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_ec_charge_with_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_ec_charge_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_ec_charge_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_ec_charge_without_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_ec_charge_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_ec_charge_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_electricity_to_grid_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_electricity_to_grid", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_electricity_to_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_fixed_with_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_fixed_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_fixed_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_fixed_without_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_fixed_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_fixed_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_load_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_load", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_minimum_with_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_minimum_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_minimum_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_minimum_without_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_minimum_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_minimum_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_peak_w_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_peak_w_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_peak_w_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_peak_wo_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_peak_wo_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_peak_wo_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_use_w_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_use_w_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_use_w_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_use_wo_system_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_use_wo_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_use_wo_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_utility_bill_w_sys_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_utility_bill_w_sys", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_utility_bill_w_sys");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate4_Outputs_year1_monthly_utility_bill_wo_sys_aget(SAM_Utilityrate4 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_utility_bill_wo_sys", length);
	if (!result)
		make_access_error("SAM_Utilityrate4", "year1_monthly_utility_bill_wo_sys");
	});
	return result;
}




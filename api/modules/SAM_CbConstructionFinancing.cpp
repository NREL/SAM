#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_CbConstructionFinancing.h"

SAM_EXPORT int SAM_CbConstructionFinancing_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("cb_construction_financing", data, verbosity, err);
}

SAM_EXPORT void SAM_CbConstructionFinancing_SystemCosts_total_installed_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "total_installed_cost", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate1", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate2", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate3", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate4", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate5", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months1", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months2", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months3", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months4", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months5", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent1", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent2", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent3", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent4", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent5", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate1", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate2", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate3", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate4", number);
	});
}

SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate5", number);
	});
}

SAM_EXPORT double SAM_CbConstructionFinancing_SystemCosts_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_CbConstructionFinancing", "total_installed_cost");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate1", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_interest_rate1");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate2", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_interest_rate2");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate3", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_interest_rate3");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate4", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_interest_rate4");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate5", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_interest_rate5");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months1", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_months1");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months2", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_months2");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months3", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_months3");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months4", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_months4");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months5", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_months5");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent1", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_percent1");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent2", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_percent2");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent3", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_percent3");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent4", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_percent4");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent5", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_percent5");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate1", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_upfront_rate1");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate2", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_upfront_rate2");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate3", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_upfront_rate3");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate4", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_upfront_rate4");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate5", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_upfront_rate5");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_interest1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest1", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_interest1");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_interest2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest2", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_interest2");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_interest3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest3", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_interest3");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_interest4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest4", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_interest4");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_interest5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest5", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_interest5");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_interest_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_total", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_interest_total");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_percent_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent_total", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_percent_total");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_principal1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal1", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_principal1");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_principal2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal2", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_principal2");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_principal3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal3", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_principal3");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_principal4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal4", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_principal4");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_principal5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal5", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_principal5");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_principal_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal_total", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_principal_total");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_total1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total1", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_total1");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_total2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total2", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_total2");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_total3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total3", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_total3");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_total4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total4", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_total4");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_total5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total5", &result))
		make_access_error("SAM_CbConstructionFinancing", "const_per_total5");
	});
	return result;
}



SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_financing_cost", &result))
		make_access_error("SAM_CbConstructionFinancing", "construction_financing_cost");
	});
	return result;
}




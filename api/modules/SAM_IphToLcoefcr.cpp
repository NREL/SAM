#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_IphToLcoefcr.h"

SAM_EXPORT int SAM_IphToLcoefcr_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("iph_to_lcoefcr", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_IphToLcoefcr_IPHLCOH_annual_electricity_consumption_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "annual_electricity_consumption", number);
	});
}

SAM_EXPORT void SAM_IphToLcoefcr_IPHLCOH_electricity_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "electricity_rate", number);
	});
}

SAM_EXPORT void SAM_IphToLcoefcr_SimpleLCOE_fixed_operating_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fixed_operating_cost", number);
	});
}

SAM_EXPORT double SAM_IphToLcoefcr_IPHLCOH_annual_electricity_consumption_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_electricity_consumption", &result))
		make_access_error("SAM_IphToLcoefcr", "annual_electricity_consumption");
	});
	return result;
}



SAM_EXPORT double SAM_IphToLcoefcr_IPHLCOH_electricity_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "electricity_rate", &result))
		make_access_error("SAM_IphToLcoefcr", "electricity_rate");
	});
	return result;
}



SAM_EXPORT double SAM_IphToLcoefcr_SimpleLCOE_fixed_operating_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fixed_operating_cost", &result))
		make_access_error("SAM_IphToLcoefcr", "fixed_operating_cost");
	});
	return result;
}




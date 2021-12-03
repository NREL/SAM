#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_TestUdPowerCycle.h"

SAM_EXPORT int SAM_TestUdPowerCycle_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("test_ud_power_cycle", data, verbosity, err);
}

SAM_EXPORT void SAM_TestUdPowerCycle_Common_q_pb_design_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_pb_design", number);
	});
}

SAM_EXPORT double SAM_TestUdPowerCycle_Common_q_pb_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_pb_design", &result))
		make_access_error("SAM_TestUdPowerCycle", "q_pb_design");
	});
	return result;
}



SAM_EXPORT double* SAM_TestUdPowerCycle_Outputs_udpc_table_out_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "udpc_table_out", nrows, ncols);
	if (!result)
		make_access_error("SAM_TestUdPowerCycle", "udpc_table_out");
	});
	return result;
}




#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Wfcheck.h"

SAM_EXPORT int SAM_Wfcheck_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("wfcheck", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Wfcheck_WeatherFileChecker_input_file_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "input_file", str);
	});
}

SAM_EXPORT const char* SAM_Wfcheck_WeatherFileChecker_input_file_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "input_file");
	if (!result)
		make_access_error("SAM_Wfcheck", "input_file");
	});
	return result;
}




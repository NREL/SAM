#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_HybridSteps.h"

SAM_EXPORT int SAM_HybridSteps_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("hybrid_steps", data, verbosity, err);
}

SAM_EXPORT void SAM_HybridSteps_Common_input_tset(SAM_table ptr, SAM_table tab, SAM_error *err){
	SAM_table_set_table(ptr, "input", tab, err);
}



SAM_EXPORT SAM_table SAM_HybridSteps_Common_input_tget(SAM_table ptr, SAM_error *err){
	SAM_table result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_table(ptr, "input");
	if (!result)
		make_access_error("SAM_HybridSteps", "input");
	});
	return result;
}

SAM_EXPORT SAM_table SAM_HybridSteps_Outputs_output_tget(SAM_table ptr, SAM_error *err){
	SAM_table result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_table(ptr, "output");
	if (!result)
		make_access_error("SAM_HybridSteps", "output");
	});
	return result;
}


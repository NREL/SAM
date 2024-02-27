#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Hybrid.h"

SAM_EXPORT int SAM_Hybrid_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("hybrid", data, verbosity, err);
}

SAM_EXPORT void SAM_Hybrid_Common_input_tset(SAM_table ptr, SAM_table tab, SAM_error *err){
	SAM_table_set_table(ptr, "input", tab, err);
}



SAM_EXPORT SAM_table SAM_Hybrid_Common_input_tget(SAM_table ptr, SAM_error *err){
	SAM_table result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_table(ptr, "input");
	if (!result)
		make_access_error("SAM_Hybrid", "input");
	});
	return result;
}

SAM_EXPORT SAM_table SAM_Hybrid_Outputs_output_tget(SAM_table ptr, SAM_error *err){
	SAM_table result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_table(ptr, "output");
	if (!result)
		make_access_error("SAM_Hybrid", "output");
	});
	return result;
}


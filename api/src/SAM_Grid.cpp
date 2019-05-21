#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Grid.h"

SAM_EXPORT SAM_Grid SAM_Grid_construct(const char* def, SAM_error* err){
	SAM_Grid result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_Grid_execute(SAM_Grid data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("grid", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Grid_destruct(SAM_Grid system)
{
	ssc_data_free(system);
}


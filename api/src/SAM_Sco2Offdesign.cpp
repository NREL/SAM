#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Sco2Offdesign.h"

SAM_EXPORT SAM_Sco2Offdesign SAM_Sco2Offdesign_construct(const char* def, SAM_error* err){
	SAM_Sco2Offdesign result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_Sco2Offdesign_execute(SAM_Sco2Offdesign data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("sco2_offdesign", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Sco2Offdesign_destruct(SAM_Sco2Offdesign system)
{
	ssc_data_free(system);
}


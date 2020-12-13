#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Iec61853interp.h"

SAM_EXPORT int SAM_Iec61853interp_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("iec61853interp", data, verbosity, err);
}

SAM_EXPORT void SAM_Iec61853interp_IEC61853_input_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "input", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Iec61853interp_IEC61853_param_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "param", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Iec61853interp_SingleDiodeModel_I_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I", number);
	});
}

SAM_EXPORT void SAM_Iec61853interp_SingleDiodeModel_T_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T", number);
	});
}

SAM_EXPORT double* SAM_Iec61853interp_IEC61853_input_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "input", nrows, ncols);
	if (!result)
		make_access_error("SAM_Iec61853interp", "input");
	});
	return result;
}



SAM_EXPORT double* SAM_Iec61853interp_IEC61853_param_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "param", nrows, ncols);
	if (!result)
		make_access_error("SAM_Iec61853interp", "param");
	});
	return result;
}



SAM_EXPORT double SAM_Iec61853interp_SingleDiodeModel_I_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I", &result))
		make_access_error("SAM_Iec61853interp", "I");
	});
	return result;
}



SAM_EXPORT double SAM_Iec61853interp_SingleDiodeModel_T_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T", &result))
		make_access_error("SAM_Iec61853interp", "T");
	});
	return result;
}



SAM_EXPORT double SAM_Iec61853interp_Outputs_Il_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Il", &result))
		make_access_error("SAM_Iec61853interp", "Il");
	});
	return result;
}



SAM_EXPORT double SAM_Iec61853interp_Outputs_Io_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Io", &result))
		make_access_error("SAM_Iec61853interp", "Io");
	});
	return result;
}



SAM_EXPORT double SAM_Iec61853interp_Outputs_Rs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Rs", &result))
		make_access_error("SAM_Iec61853interp", "Rs");
	});
	return result;
}



SAM_EXPORT double SAM_Iec61853interp_Outputs_Rsh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Rsh", &result))
		make_access_error("SAM_Iec61853interp", "Rsh");
	});
	return result;
}



SAM_EXPORT double SAM_Iec61853interp_Outputs_a_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "a", &result))
		make_access_error("SAM_Iec61853interp", "a");
	});
	return result;
}




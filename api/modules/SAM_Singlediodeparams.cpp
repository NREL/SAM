#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Singlediodeparams.h"

SAM_EXPORT int SAM_Singlediodeparams_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("singlediodeparams", data, verbosity, err);
}

SAM_EXPORT void SAM_Singlediodeparams_SingleDiodeModel_Adj_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Adj_ref", number);
	});
}

SAM_EXPORT void SAM_Singlediodeparams_SingleDiodeModel_I_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I", number);
	});
}

SAM_EXPORT void SAM_Singlediodeparams_SingleDiodeModel_Il_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Il_ref", number);
	});
}

SAM_EXPORT void SAM_Singlediodeparams_SingleDiodeModel_Io_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Io_ref", number);
	});
}

SAM_EXPORT void SAM_Singlediodeparams_SingleDiodeModel_Rs_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Rs_ref", number);
	});
}

SAM_EXPORT void SAM_Singlediodeparams_SingleDiodeModel_Rsh_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Rsh_ref", number);
	});
}

SAM_EXPORT void SAM_Singlediodeparams_SingleDiodeModel_T_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T", number);
	});
}

SAM_EXPORT void SAM_Singlediodeparams_SingleDiodeModel_a_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "a_ref", number);
	});
}

SAM_EXPORT void SAM_Singlediodeparams_SingleDiodeModel_alpha_isc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "alpha_isc", number);
	});
}

SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_Adj_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Adj_ref", &result))
		make_access_error("SAM_Singlediodeparams", "Adj_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_I_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I", &result))
		make_access_error("SAM_Singlediodeparams", "I");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_Il_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Il_ref", &result))
		make_access_error("SAM_Singlediodeparams", "Il_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_Io_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Io_ref", &result))
		make_access_error("SAM_Singlediodeparams", "Io_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_Rs_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Rs_ref", &result))
		make_access_error("SAM_Singlediodeparams", "Rs_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_Rsh_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Rsh_ref", &result))
		make_access_error("SAM_Singlediodeparams", "Rsh_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_T_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T", &result))
		make_access_error("SAM_Singlediodeparams", "T");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_a_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "a_ref", &result))
		make_access_error("SAM_Singlediodeparams", "a_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_alpha_isc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "alpha_isc", &result))
		make_access_error("SAM_Singlediodeparams", "alpha_isc");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediodeparams_Outputs_Il_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Il", &result))
		make_access_error("SAM_Singlediodeparams", "Il");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediodeparams_Outputs_Io_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Io", &result))
		make_access_error("SAM_Singlediodeparams", "Io");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediodeparams_Outputs_Rs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Rs", &result))
		make_access_error("SAM_Singlediodeparams", "Rs");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediodeparams_Outputs_Rsh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Rsh", &result))
		make_access_error("SAM_Singlediodeparams", "Rsh");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediodeparams_Outputs_a_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "a", &result))
		make_access_error("SAM_Singlediodeparams", "a");
	});
	return result;
}




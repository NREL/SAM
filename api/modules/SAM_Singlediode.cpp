#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Singlediode.h"

SAM_EXPORT SAM_Singlediode SAM_Singlediode_construct(const char* def, SAM_error* err){
	SAM_Singlediode result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_Singlediode_execute(SAM_Singlediode data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("singlediode", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Singlediode_destruct(SAM_Singlediode system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_Singlediode_SingleDiodeModel_Il_nset(SAM_Singlediode ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Il", number);
	});
}

SAM_EXPORT void SAM_Singlediode_SingleDiodeModel_Io_nset(SAM_Singlediode ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Io", number);
	});
}

SAM_EXPORT void SAM_Singlediode_SingleDiodeModel_Rs_nset(SAM_Singlediode ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Rs", number);
	});
}

SAM_EXPORT void SAM_Singlediode_SingleDiodeModel_Rsh_nset(SAM_Singlediode ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Rsh", number);
	});
}

SAM_EXPORT void SAM_Singlediode_SingleDiodeModel_Vop_nset(SAM_Singlediode ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Vop", number);
	});
}

SAM_EXPORT void SAM_Singlediode_SingleDiodeModel_a_nset(SAM_Singlediode ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "a", number);
	});
}

SAM_EXPORT double SAM_Singlediode_SingleDiodeModel_Il_nget(SAM_Singlediode ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Il", &result))
		make_access_error("SAM_Singlediode", "Il");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediode_SingleDiodeModel_Io_nget(SAM_Singlediode ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Io", &result))
		make_access_error("SAM_Singlediode", "Io");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediode_SingleDiodeModel_Rs_nget(SAM_Singlediode ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Rs", &result))
		make_access_error("SAM_Singlediode", "Rs");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediode_SingleDiodeModel_Rsh_nget(SAM_Singlediode ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Rsh", &result))
		make_access_error("SAM_Singlediode", "Rsh");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediode_SingleDiodeModel_Vop_nget(SAM_Singlediode ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Vop", &result))
		make_access_error("SAM_Singlediode", "Vop");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediode_SingleDiodeModel_a_nget(SAM_Singlediode ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "a", &result))
		make_access_error("SAM_Singlediode", "a");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediode_Outputs_I_nget(SAM_Singlediode ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I", &result))
		make_access_error("SAM_Singlediode", "I");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediode_Outputs_Isc_nget(SAM_Singlediode ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Isc", &result))
		make_access_error("SAM_Singlediode", "Isc");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediode_Outputs_V_nget(SAM_Singlediode ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V", &result))
		make_access_error("SAM_Singlediode", "V");
	});
	return result;
}



SAM_EXPORT double SAM_Singlediode_Outputs_Voc_nget(SAM_Singlediode ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Voc", &result))
		make_access_error("SAM_Singlediode", "Voc");
	});
	return result;
}




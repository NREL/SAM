#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Timeseq.h"

SAM_EXPORT SAM_Timeseq SAM_Timeseq_construct(const char* def, SAM_error* err){
	SAM_Timeseq result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_Timeseq_execute(SAM_Timeseq data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("timeseq", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Timeseq_destruct(SAM_Timeseq system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_Timeseq_TimeSequence_end_time_nset(SAM_Timeseq ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "end_time", number);
	});
}

SAM_EXPORT void SAM_Timeseq_TimeSequence_start_time_nset(SAM_Timeseq ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "start_time", number);
	});
}

SAM_EXPORT void SAM_Timeseq_TimeSequence_time_step_nset(SAM_Timeseq ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_step", number);
	});
}

SAM_EXPORT double SAM_Timeseq_TimeSequence_end_time_nget(SAM_Timeseq ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "end_time", &result))
		make_access_error("SAM_Timeseq", "end_time");
	});
	return result;
}



SAM_EXPORT double SAM_Timeseq_TimeSequence_start_time_nget(SAM_Timeseq ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "start_time", &result))
		make_access_error("SAM_Timeseq", "start_time");
	});
	return result;
}



SAM_EXPORT double SAM_Timeseq_TimeSequence_time_step_nget(SAM_Timeseq ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_step", &result))
		make_access_error("SAM_Timeseq", "time_step");
	});
	return result;
}



SAM_EXPORT double* SAM_Timeseq_Outputs_day_aget(SAM_Timeseq ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "day", length);
	if (!result)
		make_access_error("SAM_Timeseq", "day");
	});
	return result;
}



SAM_EXPORT double* SAM_Timeseq_Outputs_hour_aget(SAM_Timeseq ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour", length);
	if (!result)
		make_access_error("SAM_Timeseq", "hour");
	});
	return result;
}



SAM_EXPORT double* SAM_Timeseq_Outputs_minute_aget(SAM_Timeseq ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "minute", length);
	if (!result)
		make_access_error("SAM_Timeseq", "minute");
	});
	return result;
}



SAM_EXPORT double* SAM_Timeseq_Outputs_month_aget(SAM_Timeseq ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_Timeseq", "month");
	});
	return result;
}



SAM_EXPORT double* SAM_Timeseq_Outputs_time_aget(SAM_Timeseq ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "time", length);
	if (!result)
		make_access_error("SAM_Timeseq", "time");
	});
	return result;
}



SAM_EXPORT double* SAM_Timeseq_Outputs_timehr_aget(SAM_Timeseq ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "timehr", length);
	if (!result)
		make_access_error("SAM_Timeseq", "timehr");
	});
	return result;
}




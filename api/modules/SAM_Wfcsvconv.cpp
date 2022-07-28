#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Wfcsvconv.h"

SAM_EXPORT int SAM_Wfcsvconv_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("wfcsvconv", data, verbosity, err);
}

SAM_EXPORT void SAM_Wfcsvconv_WeatherFileConverter_input_file_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "input_file", str);
	});
}

SAM_EXPORT void SAM_Wfcsvconv_WeatherFileConverter_output_file_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "output_file", str);
	});
}

SAM_EXPORT void SAM_Wfcsvconv_WeatherFileConverter_output_filename_format_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "output_filename_format", str);
	});
}

SAM_EXPORT void SAM_Wfcsvconv_WeatherFileConverter_output_folder_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "output_folder", str);
	});
}

SAM_EXPORT const char* SAM_Wfcsvconv_WeatherFileConverter_input_file_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "input_file");
	if (!result)
		make_access_error("SAM_Wfcsvconv", "input_file");
	});
	return result;
}



SAM_EXPORT const char* SAM_Wfcsvconv_WeatherFileConverter_output_file_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "output_file");
	if (!result)
		make_access_error("SAM_Wfcsvconv", "output_file");
	});
	return result;
}



SAM_EXPORT const char* SAM_Wfcsvconv_WeatherFileConverter_output_filename_format_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "output_filename_format");
	if (!result)
		make_access_error("SAM_Wfcsvconv", "output_filename_format");
	});
	return result;
}



SAM_EXPORT const char* SAM_Wfcsvconv_WeatherFileConverter_output_folder_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "output_folder");
	if (!result)
		make_access_error("SAM_Wfcsvconv", "output_folder");
	});
	return result;
}




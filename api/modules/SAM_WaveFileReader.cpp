#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_WaveFileReader.h"

SAM_EXPORT int SAM_WaveFileReader_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("wave_file_reader", data, verbosity, err);
}

SAM_EXPORT void SAM_WaveFileReader_WeatherReader_use_specific_wf_wave_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "use_specific_wf_wave", number);
	});
}

SAM_EXPORT void SAM_WaveFileReader_WeatherReader_wave_resource_filename_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "wave_resource_filename", str);
	});
}

SAM_EXPORT void SAM_WaveFileReader_WeatherReader_wave_resource_filename_ts_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "wave_resource_filename_ts", str);
	});
}

SAM_EXPORT void SAM_WaveFileReader_WeatherReader_wave_resource_model_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wave_resource_model_choice", number);
	});
}

SAM_EXPORT double SAM_WaveFileReader_WeatherReader_use_specific_wf_wave_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "use_specific_wf_wave", &result))
		make_access_error("SAM_WaveFileReader", "use_specific_wf_wave");
	});
	return result;
}



SAM_EXPORT const char* SAM_WaveFileReader_WeatherReader_wave_resource_filename_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "wave_resource_filename");
	if (!result)
		make_access_error("SAM_WaveFileReader", "wave_resource_filename");
	});
	return result;
}



SAM_EXPORT const char* SAM_WaveFileReader_WeatherReader_wave_resource_filename_ts_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "wave_resource_filename_ts");
	if (!result)
		make_access_error("SAM_WaveFileReader", "wave_resource_filename_ts");
	});
	return result;
}



SAM_EXPORT double SAM_WaveFileReader_WeatherReader_wave_resource_model_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wave_resource_model_choice", &result))
		make_access_error("SAM_WaveFileReader", "wave_resource_model_choice");
	});
	return result;
}



SAM_EXPORT double SAM_WaveFileReader_Outputs_average_power_flux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "average_power_flux", &result))
		make_access_error("SAM_WaveFileReader", "average_power_flux");
	});
	return result;
}



SAM_EXPORT const char* SAM_WaveFileReader_Outputs_bathymetry_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "bathymetry");
	if (!result)
		make_access_error("SAM_WaveFileReader", "bathymetry");
	});
	return result;
}



SAM_EXPORT const char* SAM_WaveFileReader_Outputs_city_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "city");
	if (!result)
		make_access_error("SAM_WaveFileReader", "city");
	});
	return result;
}



SAM_EXPORT const char* SAM_WaveFileReader_Outputs_country_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "country");
	if (!result)
		make_access_error("SAM_WaveFileReader", "country");
	});
	return result;
}



SAM_EXPORT const char* SAM_WaveFileReader_Outputs_data_source_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "data_source");
	if (!result)
		make_access_error("SAM_WaveFileReader", "data_source");
	});
	return result;
}



SAM_EXPORT double* SAM_WaveFileReader_Outputs_day_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "day", length);
	if (!result)
		make_access_error("SAM_WaveFileReader", "day");
	});
	return result;
}



SAM_EXPORT double SAM_WaveFileReader_Outputs_distance_to_shore_file_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "distance_to_shore_file", &result))
		make_access_error("SAM_WaveFileReader", "distance_to_shore_file");
	});
	return result;
}



SAM_EXPORT double* SAM_WaveFileReader_Outputs_energy_period_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "energy_period", length);
	if (!result)
		make_access_error("SAM_WaveFileReader", "energy_period");
	});
	return result;
}



SAM_EXPORT double* SAM_WaveFileReader_Outputs_hour_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour", length);
	if (!result)
		make_access_error("SAM_WaveFileReader", "hour");
	});
	return result;
}



SAM_EXPORT double SAM_WaveFileReader_Outputs_lat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lat", &result))
		make_access_error("SAM_WaveFileReader", "lat");
	});
	return result;
}



SAM_EXPORT double SAM_WaveFileReader_Outputs_location_id_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "location_id", &result))
		make_access_error("SAM_WaveFileReader", "location_id");
	});
	return result;
}



SAM_EXPORT double SAM_WaveFileReader_Outputs_lon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lon", &result))
		make_access_error("SAM_WaveFileReader", "lon");
	});
	return result;
}



SAM_EXPORT double* SAM_WaveFileReader_Outputs_minute_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "minute", length);
	if (!result)
		make_access_error("SAM_WaveFileReader", "minute");
	});
	return result;
}



SAM_EXPORT double* SAM_WaveFileReader_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_WaveFileReader", "month");
	});
	return result;
}



SAM_EXPORT const char* SAM_WaveFileReader_Outputs_name_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "name");
	if (!result)
		make_access_error("SAM_WaveFileReader", "name");
	});
	return result;
}



SAM_EXPORT const char* SAM_WaveFileReader_Outputs_nearby_buoy_number_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "nearby_buoy_number");
	if (!result)
		make_access_error("SAM_WaveFileReader", "nearby_buoy_number");
	});
	return result;
}



SAM_EXPORT const char* SAM_WaveFileReader_Outputs_notes_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "notes");
	if (!result)
		make_access_error("SAM_WaveFileReader", "notes");
	});
	return result;
}



SAM_EXPORT double SAM_WaveFileReader_Outputs_number_hours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "number_hours", &result))
		make_access_error("SAM_WaveFileReader", "number_hours");
	});
	return result;
}



SAM_EXPORT double SAM_WaveFileReader_Outputs_number_records_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "number_records", &result))
		make_access_error("SAM_WaveFileReader", "number_records");
	});
	return result;
}



SAM_EXPORT const char* SAM_WaveFileReader_Outputs_sea_bed_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "sea_bed");
	if (!result)
		make_access_error("SAM_WaveFileReader", "sea_bed");
	});
	return result;
}



SAM_EXPORT double* SAM_WaveFileReader_Outputs_significant_wave_height_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "significant_wave_height", length);
	if (!result)
		make_access_error("SAM_WaveFileReader", "significant_wave_height");
	});
	return result;
}



SAM_EXPORT const char* SAM_WaveFileReader_Outputs_state_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "state");
	if (!result)
		make_access_error("SAM_WaveFileReader", "state");
	});
	return result;
}



SAM_EXPORT double SAM_WaveFileReader_Outputs_tz_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tz", &result))
		make_access_error("SAM_WaveFileReader", "tz");
	});
	return result;
}



SAM_EXPORT double SAM_WaveFileReader_Outputs_water_depth_file_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "water_depth_file", &result))
		make_access_error("SAM_WaveFileReader", "water_depth_file");
	});
	return result;
}



SAM_EXPORT double* SAM_WaveFileReader_Outputs_wave_resource_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "wave_resource_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_WaveFileReader", "wave_resource_matrix");
	});
	return result;
}



SAM_EXPORT double* SAM_WaveFileReader_Outputs_year_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year", length);
	if (!result)
		make_access_error("SAM_WaveFileReader", "year");
	});
	return result;
}




#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_TidalFileReader.h"

SAM_EXPORT int SAM_TidalFileReader_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("tidal_file_reader", data, verbosity, err);
}

SAM_EXPORT void SAM_TidalFileReader_WeatherReader_tidal_resource_filename_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "tidal_resource_filename", str);
	});
}

SAM_EXPORT void SAM_TidalFileReader_WeatherReader_tidal_resource_model_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tidal_resource_model_choice", number);
	});
}

SAM_EXPORT const char* SAM_TidalFileReader_WeatherReader_tidal_resource_filename_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "tidal_resource_filename");
	if (!result)
		make_access_error("SAM_TidalFileReader", "tidal_resource_filename");
	});
	return result;
}

SAM_EXPORT double SAM_TidalFileReader_WeatherReader_tidal_resource_model_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tidal_resource_model_choice", &result))
		make_access_error("SAM_TidalFileReader", "tidal_resource_model_choice");
	});
	return result;
}

SAM_EXPORT double SAM_TidalFileReader_Outputs_average_power_flux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "average_power_flux", &result))
		make_access_error("SAM_TidalFileReader", "average_power_flux");
	});
	return result;
}

SAM_EXPORT const char* SAM_TidalFileReader_Outputs_bathymetry_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "bathymetry");
	if (!result)
		make_access_error("SAM_TidalFileReader", "bathymetry");
	});
	return result;
}

SAM_EXPORT const char* SAM_TidalFileReader_Outputs_city_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "city");
	if (!result)
		make_access_error("SAM_TidalFileReader", "city");
	});
	return result;
}

SAM_EXPORT const char* SAM_TidalFileReader_Outputs_country_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "country");
	if (!result)
		make_access_error("SAM_TidalFileReader", "country");
	});
	return result;
}

SAM_EXPORT const char* SAM_TidalFileReader_Outputs_data_source_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "data_source");
	if (!result)
		make_access_error("SAM_TidalFileReader", "data_source");
	});
	return result;
}

SAM_EXPORT double* SAM_TidalFileReader_Outputs_day_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "day", length);
	if (!result)
		make_access_error("SAM_TidalFileReader", "day");
	});
	return result;
}

SAM_EXPORT double SAM_TidalFileReader_Outputs_distance_to_shore_file_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "distance_to_shore_file", &result))
		make_access_error("SAM_TidalFileReader", "distance_to_shore_file");
	});
	return result;
}

SAM_EXPORT double* SAM_TidalFileReader_Outputs_hour_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour", length);
	if (!result)
		make_access_error("SAM_TidalFileReader", "hour");
	});
	return result;
}

SAM_EXPORT double SAM_TidalFileReader_Outputs_lat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lat", &result))
		make_access_error("SAM_TidalFileReader", "lat");
	});
	return result;
}

SAM_EXPORT const char* SAM_TidalFileReader_Outputs_location_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "location");
	if (!result)
		make_access_error("SAM_TidalFileReader", "location");
	});
	return result;
}

SAM_EXPORT const char* SAM_TidalFileReader_Outputs_location_id_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "location_id");
	if (!result)
		make_access_error("SAM_TidalFileReader", "location_id");
	});
	return result;
}

SAM_EXPORT double SAM_TidalFileReader_Outputs_lon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lon", &result))
		make_access_error("SAM_TidalFileReader", "lon");
	});
	return result;
}

SAM_EXPORT double* SAM_TidalFileReader_Outputs_minute_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "minute", length);
	if (!result)
		make_access_error("SAM_TidalFileReader", "minute");
	});
	return result;
}

SAM_EXPORT double* SAM_TidalFileReader_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_TidalFileReader", "month");
	});
	return result;
}

SAM_EXPORT const char* SAM_TidalFileReader_Outputs_name_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "name");
	if (!result)
		make_access_error("SAM_TidalFileReader", "name");
	});
	return result;
}

SAM_EXPORT const char* SAM_TidalFileReader_Outputs_nearby_buoy_number_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "nearby_buoy_number");
	if (!result)
		make_access_error("SAM_TidalFileReader", "nearby_buoy_number");
	});
	return result;
}

SAM_EXPORT const char* SAM_TidalFileReader_Outputs_notes_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "notes");
	if (!result)
		make_access_error("SAM_TidalFileReader", "notes");
	});
	return result;
}

SAM_EXPORT double SAM_TidalFileReader_Outputs_number_hours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "number_hours", &result))
		make_access_error("SAM_TidalFileReader", "number_hours");
	});
	return result;
}

SAM_EXPORT double SAM_TidalFileReader_Outputs_number_records_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "number_records", &result))
		make_access_error("SAM_TidalFileReader", "number_records");
	});
	return result;
}

SAM_EXPORT const char* SAM_TidalFileReader_Outputs_sea_bed_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "sea_bed");
	if (!result)
		make_access_error("SAM_TidalFileReader", "sea_bed");
	});
	return result;
}

SAM_EXPORT double* SAM_TidalFileReader_Outputs_significant_wave_height_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "significant_wave_height", length);
	if (!result)
		make_access_error("SAM_TidalFileReader", "significant_wave_height");
	});
	return result;
}

SAM_EXPORT const char* SAM_TidalFileReader_Outputs_state_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "state");
	if (!result)
		make_access_error("SAM_TidalFileReader", "state");
	});
	return result;
}

SAM_EXPORT double* SAM_TidalFileReader_Outputs_tidal_velocity_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tidal_velocity", length);
	if (!result)
		make_access_error("SAM_TidalFileReader", "tidal_velocity");
	});
	return result;
}

SAM_EXPORT double SAM_TidalFileReader_Outputs_tz_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tz", &result))
		make_access_error("SAM_TidalFileReader", "tz");
	});
	return result;
}

SAM_EXPORT double SAM_TidalFileReader_Outputs_water_depth_file_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "water_depth_file", &result))
		make_access_error("SAM_TidalFileReader", "water_depth_file");
	});
	return result;
}

SAM_EXPORT double* SAM_TidalFileReader_Outputs_year_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year", length);
	if (!result)
		make_access_error("SAM_TidalFileReader", "year");
	});
	return result;
}


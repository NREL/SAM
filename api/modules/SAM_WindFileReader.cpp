#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_WindFileReader.h"

SAM_EXPORT int SAM_WindFileReader_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("wind_file_reader", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_WindFileReader_WeatherReader_file_name_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "file_name", str);
	});
}

SAM_EXPORT void SAM_WindFileReader_WeatherReader_interpolate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "interpolate", number);
	});
}

SAM_EXPORT void SAM_WindFileReader_WeatherReader_requested_ht_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "requested_ht", number);
	});
}

SAM_EXPORT void SAM_WindFileReader_WeatherReader_scan_header_only_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "scan_header_only", number);
	});
}

SAM_EXPORT const char* SAM_WindFileReader_WeatherReader_file_name_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "file_name");
	if (!result)
		make_access_error("SAM_WindFileReader", "file_name");
	});
	return result;
}



SAM_EXPORT double SAM_WindFileReader_WeatherReader_interpolate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "interpolate", &result))
		make_access_error("SAM_WindFileReader", "interpolate");
	});
	return result;
}



SAM_EXPORT double SAM_WindFileReader_WeatherReader_requested_ht_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "requested_ht", &result))
		make_access_error("SAM_WindFileReader", "requested_ht");
	});
	return result;
}



SAM_EXPORT double SAM_WindFileReader_WeatherReader_scan_header_only_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "scan_header_only", &result))
		make_access_error("SAM_WindFileReader", "scan_header_only");
	});
	return result;
}



SAM_EXPORT const char* SAM_WindFileReader_Outputs_city_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "city");
	if (!result)
		make_access_error("SAM_WindFileReader", "city");
	});
	return result;
}



SAM_EXPORT double SAM_WindFileReader_Outputs_closest_dir_meas_ht_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "closest_dir_meas_ht", &result))
		make_access_error("SAM_WindFileReader", "closest_dir_meas_ht");
	});
	return result;
}



SAM_EXPORT double SAM_WindFileReader_Outputs_closest_speed_meas_ht_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "closest_speed_meas_ht", &result))
		make_access_error("SAM_WindFileReader", "closest_speed_meas_ht");
	});
	return result;
}



SAM_EXPORT const char* SAM_WindFileReader_Outputs_country_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "country");
	if (!result)
		make_access_error("SAM_WindFileReader", "country");
	});
	return result;
}



SAM_EXPORT const char* SAM_WindFileReader_Outputs_description_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "description");
	if (!result)
		make_access_error("SAM_WindFileReader", "description");
	});
	return result;
}



SAM_EXPORT double SAM_WindFileReader_Outputs_elev_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "elev", &result))
		make_access_error("SAM_WindFileReader", "elev");
	});
	return result;
}



SAM_EXPORT double SAM_WindFileReader_Outputs_lat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lat", &result))
		make_access_error("SAM_WindFileReader", "lat");
	});
	return result;
}



SAM_EXPORT const char* SAM_WindFileReader_Outputs_location_id_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "location_id");
	if (!result)
		make_access_error("SAM_WindFileReader", "location_id");
	});
	return result;
}



SAM_EXPORT double SAM_WindFileReader_Outputs_lon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lon", &result))
		make_access_error("SAM_WindFileReader", "lon");
	});
	return result;
}



SAM_EXPORT double* SAM_WindFileReader_Outputs_pressure_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pressure", length);
	if (!result)
		make_access_error("SAM_WindFileReader", "pressure");
	});
	return result;
}



SAM_EXPORT const char* SAM_WindFileReader_Outputs_state_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "state");
	if (!result)
		make_access_error("SAM_WindFileReader", "state");
	});
	return result;
}



SAM_EXPORT double* SAM_WindFileReader_Outputs_temperature_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "temperature", length);
	if (!result)
		make_access_error("SAM_WindFileReader", "temperature");
	});
	return result;
}



SAM_EXPORT double* SAM_WindFileReader_Outputs_wind_direction_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wind_direction", length);
	if (!result)
		make_access_error("SAM_WindFileReader", "wind_direction");
	});
	return result;
}



SAM_EXPORT double* SAM_WindFileReader_Outputs_wind_speed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wind_speed", length);
	if (!result)
		make_access_error("SAM_WindFileReader", "wind_speed");
	});
	return result;
}



SAM_EXPORT double SAM_WindFileReader_Outputs_year_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "year", &result))
		make_access_error("SAM_WindFileReader", "year");
	});
	return result;
}




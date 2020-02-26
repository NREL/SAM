#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Irradproc.h"

SAM_EXPORT SAM_Irradproc SAM_Irradproc_construct(const char* def, SAM_error* err){
	SAM_Irradproc result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_Irradproc_execute(SAM_Irradproc data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("irradproc", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Irradproc_destruct(SAM_Irradproc system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_albedo_aset(SAM_Irradproc ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "albedo", arr, length);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_albedo_const_nset(SAM_Irradproc ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "albedo_const", number);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_azimuth_nset(SAM_Irradproc ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "azimuth", number);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_backtrack_nset(SAM_Irradproc ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "backtrack", number);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_beam_aset(SAM_Irradproc ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "beam", arr, length);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_day_aset(SAM_Irradproc ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "day", arr, length);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_diffuse_aset(SAM_Irradproc ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "diffuse", arr, length);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_gcr_nset(SAM_Irradproc ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gcr", number);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_global_aset(SAM_Irradproc ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "global", arr, length);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_hour_aset(SAM_Irradproc ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "hour", arr, length);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_irrad_mode_nset(SAM_Irradproc ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "irrad_mode", number);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_lat_nset(SAM_Irradproc ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "lat", number);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_lon_nset(SAM_Irradproc ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "lon", number);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_minute_aset(SAM_Irradproc ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "minute", arr, length);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_month_aset(SAM_Irradproc ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "month", arr, length);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_rotlim_nset(SAM_Irradproc ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rotlim", number);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_sky_model_nset(SAM_Irradproc ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sky_model", number);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_tilt_nset(SAM_Irradproc ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tilt", number);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_track_mode_nset(SAM_Irradproc ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "track_mode", number);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_tz_nset(SAM_Irradproc ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tz", number);
	});
}

SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_year_aset(SAM_Irradproc ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "year", arr, length);
	});
}

SAM_EXPORT double* SAM_Irradproc_IrradianceProcessor_albedo_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "albedo", length);
	if (!result)
		make_access_error("SAM_Irradproc", "albedo");
	});
	return result;
}



SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_albedo_const_nget(SAM_Irradproc ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "albedo_const", &result))
		make_access_error("SAM_Irradproc", "albedo_const");
	});
	return result;
}



SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_azimuth_nget(SAM_Irradproc ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "azimuth", &result))
		make_access_error("SAM_Irradproc", "azimuth");
	});
	return result;
}



SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_backtrack_nget(SAM_Irradproc ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "backtrack", &result))
		make_access_error("SAM_Irradproc", "backtrack");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_IrradianceProcessor_beam_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_Irradproc", "beam");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_IrradianceProcessor_day_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "day", length);
	if (!result)
		make_access_error("SAM_Irradproc", "day");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_IrradianceProcessor_diffuse_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "diffuse", length);
	if (!result)
		make_access_error("SAM_Irradproc", "diffuse");
	});
	return result;
}



SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_gcr_nget(SAM_Irradproc ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gcr", &result))
		make_access_error("SAM_Irradproc", "gcr");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_IrradianceProcessor_global_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "global", length);
	if (!result)
		make_access_error("SAM_Irradproc", "global");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_IrradianceProcessor_hour_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour", length);
	if (!result)
		make_access_error("SAM_Irradproc", "hour");
	});
	return result;
}



SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_irrad_mode_nget(SAM_Irradproc ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "irrad_mode", &result))
		make_access_error("SAM_Irradproc", "irrad_mode");
	});
	return result;
}



SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_lat_nget(SAM_Irradproc ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lat", &result))
		make_access_error("SAM_Irradproc", "lat");
	});
	return result;
}



SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_lon_nget(SAM_Irradproc ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lon", &result))
		make_access_error("SAM_Irradproc", "lon");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_IrradianceProcessor_minute_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "minute", length);
	if (!result)
		make_access_error("SAM_Irradproc", "minute");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_IrradianceProcessor_month_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_Irradproc", "month");
	});
	return result;
}



SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_rotlim_nget(SAM_Irradproc ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rotlim", &result))
		make_access_error("SAM_Irradproc", "rotlim");
	});
	return result;
}



SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_sky_model_nget(SAM_Irradproc ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sky_model", &result))
		make_access_error("SAM_Irradproc", "sky_model");
	});
	return result;
}



SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_tilt_nget(SAM_Irradproc ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tilt", &result))
		make_access_error("SAM_Irradproc", "tilt");
	});
	return result;
}



SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_track_mode_nget(SAM_Irradproc ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "track_mode", &result))
		make_access_error("SAM_Irradproc", "track_mode");
	});
	return result;
}



SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_tz_nget(SAM_Irradproc ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tz", &result))
		make_access_error("SAM_Irradproc", "tz");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_IrradianceProcessor_year_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year", length);
	if (!result)
		make_access_error("SAM_Irradproc", "year");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_Outputs_axis_rotation_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "axis_rotation", length);
	if (!result)
		make_access_error("SAM_Irradproc", "axis_rotation");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_Outputs_bt_diff_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "bt_diff", length);
	if (!result)
		make_access_error("SAM_Irradproc", "bt_diff");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_Outputs_incidence_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "incidence", length);
	if (!result)
		make_access_error("SAM_Irradproc", "incidence");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_Outputs_poa_beam_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_beam", length);
	if (!result)
		make_access_error("SAM_Irradproc", "poa_beam");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_Outputs_poa_gnddiff_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_gnddiff", length);
	if (!result)
		make_access_error("SAM_Irradproc", "poa_gnddiff");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_Outputs_poa_skydiff_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_skydiff", length);
	if (!result)
		make_access_error("SAM_Irradproc", "poa_skydiff");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_Outputs_poa_skydiff_cir_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_skydiff_cir", length);
	if (!result)
		make_access_error("SAM_Irradproc", "poa_skydiff_cir");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_Outputs_poa_skydiff_hor_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_skydiff_hor", length);
	if (!result)
		make_access_error("SAM_Irradproc", "poa_skydiff_hor");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_Outputs_poa_skydiff_iso_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_skydiff_iso", length);
	if (!result)
		make_access_error("SAM_Irradproc", "poa_skydiff_iso");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_Outputs_sun_azm_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sun_azm", length);
	if (!result)
		make_access_error("SAM_Irradproc", "sun_azm");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_Outputs_sun_dec_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sun_dec", length);
	if (!result)
		make_access_error("SAM_Irradproc", "sun_dec");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_Outputs_sun_elv_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sun_elv", length);
	if (!result)
		make_access_error("SAM_Irradproc", "sun_elv");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_Outputs_sun_zen_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sun_zen", length);
	if (!result)
		make_access_error("SAM_Irradproc", "sun_zen");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_Outputs_surf_azm_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "surf_azm", length);
	if (!result)
		make_access_error("SAM_Irradproc", "surf_azm");
	});
	return result;
}



SAM_EXPORT double* SAM_Irradproc_Outputs_surf_tilt_aget(SAM_Irradproc ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "surf_tilt", length);
	if (!result)
		make_access_error("SAM_Irradproc", "surf_tilt");
	});
	return result;
}




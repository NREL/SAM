#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Snowmodel.h"

SAM_EXPORT int SAM_Snowmodel_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("snowmodel", data, verbosity, err);
}

SAM_EXPORT void SAM_Snowmodel_PVSnowModel_snowdepth_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "snowdepth", arr, length);
	});
}

SAM_EXPORT void SAM_Snowmodel_PVSnowModel_subarray1_nmody_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_nmody", number);
	});
}

SAM_EXPORT void SAM_Snowmodel_PVSnowModel_subarray1_poa_shaded_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subarray1_poa_shaded", arr, length);
	});
}

SAM_EXPORT void SAM_Snowmodel_PVSnowModel_subarray1_surf_tilt_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subarray1_surf_tilt", arr, length);
	});
}

SAM_EXPORT void SAM_Snowmodel_PVSnowModel_subarray1_tilt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_tilt", number);
	});
}

SAM_EXPORT void SAM_Snowmodel_PVSnowModel_subarray1_track_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_track_mode", number);
	});
}

SAM_EXPORT void SAM_Snowmodel_PVSnowModel_tdry_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "tdry", arr, length);
	});
}

SAM_EXPORT void SAM_Snowmodel_PVSnowModel_wspd_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "wspd", arr, length);
	});
}

SAM_EXPORT void SAM_Snowmodel_TimeSeries_hourly_gen_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "hourly_gen", arr, length);
	});
}

SAM_EXPORT void SAM_Snowmodel_TimeSeries_sunup_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sunup", arr, length);
	});
}

SAM_EXPORT double* SAM_Snowmodel_PVSnowModel_snowdepth_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "snowdepth", length);
	if (!result)
		make_access_error("SAM_Snowmodel", "snowdepth");
	});
	return result;
}



SAM_EXPORT double SAM_Snowmodel_PVSnowModel_subarray1_nmody_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_nmody", &result))
		make_access_error("SAM_Snowmodel", "subarray1_nmody");
	});
	return result;
}



SAM_EXPORT double* SAM_Snowmodel_PVSnowModel_subarray1_poa_shaded_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_poa_shaded", length);
	if (!result)
		make_access_error("SAM_Snowmodel", "subarray1_poa_shaded");
	});
	return result;
}



SAM_EXPORT double* SAM_Snowmodel_PVSnowModel_subarray1_surf_tilt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_surf_tilt", length);
	if (!result)
		make_access_error("SAM_Snowmodel", "subarray1_surf_tilt");
	});
	return result;
}



SAM_EXPORT double SAM_Snowmodel_PVSnowModel_subarray1_tilt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_tilt", &result))
		make_access_error("SAM_Snowmodel", "subarray1_tilt");
	});
	return result;
}



SAM_EXPORT double SAM_Snowmodel_PVSnowModel_subarray1_track_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_track_mode", &result))
		make_access_error("SAM_Snowmodel", "subarray1_track_mode");
	});
	return result;
}



SAM_EXPORT double* SAM_Snowmodel_PVSnowModel_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_Snowmodel", "tdry");
	});
	return result;
}



SAM_EXPORT double* SAM_Snowmodel_PVSnowModel_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_Snowmodel", "wspd");
	});
	return result;
}



SAM_EXPORT double* SAM_Snowmodel_TimeSeries_hourly_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hourly_gen", length);
	if (!result)
		make_access_error("SAM_Snowmodel", "hourly_gen");
	});
	return result;
}



SAM_EXPORT double* SAM_Snowmodel_TimeSeries_sunup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sunup", length);
	if (!result)
		make_access_error("SAM_Snowmodel", "sunup");
	});
	return result;
}



SAM_EXPORT double SAM_Snowmodel_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_Snowmodel", "annual_energy");
	});
	return result;
}



SAM_EXPORT double SAM_Snowmodel_Outputs_annual_energy_before_snow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy_before_snow", &result))
		make_access_error("SAM_Snowmodel", "annual_energy_before_snow");
	});
	return result;
}



SAM_EXPORT double* SAM_Snowmodel_Outputs_hourly_energy_before_snow_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hourly_energy_before_snow", length);
	if (!result)
		make_access_error("SAM_Snowmodel", "hourly_energy_before_snow");
	});
	return result;
}



SAM_EXPORT double* SAM_Snowmodel_Outputs_hourly_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hourly_gen", length);
	if (!result)
		make_access_error("SAM_Snowmodel", "hourly_gen");
	});
	return result;
}



SAM_EXPORT double* SAM_Snowmodel_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_Snowmodel", "monthly_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Snowmodel_Outputs_monthly_energy_before_snow_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy_before_snow", length);
	if (!result)
		make_access_error("SAM_Snowmodel", "monthly_energy_before_snow");
	});
	return result;
}




#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Belpe.h"

SAM_EXPORT SAM_Belpe SAM_Belpe_construct(const char* def, SAM_error* err){
	SAM_Belpe result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_Belpe_execute(SAM_Belpe data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("belpe", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Belpe_destruct(SAM_Belpe system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_Monthly_util_aset(SAM_Belpe ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Monthly_util", arr, length);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_Occ_Schedule_aset(SAM_Belpe ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Occ_Schedule", arr, length);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_Occupants_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Occupants", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_Retrofits_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Retrofits", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_Stories_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Stories", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_TCool_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TCool", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_TCoolSB_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TCoolSB", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_THeat_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "THeat", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_THeatSB_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "THeatSB", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_T_Sched_aset(SAM_Belpe ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "T_Sched", arr, length);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_YrBuilt_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "YrBuilt", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_belpe_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_belpe", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_cool_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_cool", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_dish_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_dish", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_dry_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_dry", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_fridge_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_fridge", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_heat_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_heat", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_mels_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_mels", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_range_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_range", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_wash_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_wash", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_floor_area_fset(SAM_Belpe ptr, float number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "floor_area", number);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_load_aset(SAM_Belpe ptr, float* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "load", arr, length);
	});
}

SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_solar_resource_file_sset(SAM_Belpe ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "solar_resource_file", str);
	});
}

SAM_EXPORT float* SAM_Belpe_LoadProfileEstimator_Monthly_util_aget(SAM_Belpe ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Monthly_util", length);
	if (!result)
		make_access_error("SAM_Belpe", "Monthly_util");
	});
	return result;
}



SAM_EXPORT float* SAM_Belpe_LoadProfileEstimator_Occ_Schedule_aget(SAM_Belpe ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Occ_Schedule", length);
	if (!result)
		make_access_error("SAM_Belpe", "Occ_Schedule");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_Occupants_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Occupants", &result))
		make_access_error("SAM_Belpe", "Occupants");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_Retrofits_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Retrofits", &result))
		make_access_error("SAM_Belpe", "Retrofits");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_Stories_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Stories", &result))
		make_access_error("SAM_Belpe", "Stories");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_TCool_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TCool", &result))
		make_access_error("SAM_Belpe", "TCool");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_TCoolSB_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TCoolSB", &result))
		make_access_error("SAM_Belpe", "TCoolSB");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_THeat_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "THeat", &result))
		make_access_error("SAM_Belpe", "THeat");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_THeatSB_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "THeatSB", &result))
		make_access_error("SAM_Belpe", "THeatSB");
	});
	return result;
}



SAM_EXPORT float* SAM_Belpe_LoadProfileEstimator_T_Sched_aget(SAM_Belpe ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_Sched", length);
	if (!result)
		make_access_error("SAM_Belpe", "T_Sched");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_YrBuilt_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "YrBuilt", &result))
		make_access_error("SAM_Belpe", "YrBuilt");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_en_belpe_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_belpe", &result))
		make_access_error("SAM_Belpe", "en_belpe");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_en_cool_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_cool", &result))
		make_access_error("SAM_Belpe", "en_cool");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_en_dish_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_dish", &result))
		make_access_error("SAM_Belpe", "en_dish");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_en_dry_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_dry", &result))
		make_access_error("SAM_Belpe", "en_dry");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_en_fridge_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_fridge", &result))
		make_access_error("SAM_Belpe", "en_fridge");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_en_heat_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_heat", &result))
		make_access_error("SAM_Belpe", "en_heat");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_en_mels_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_mels", &result))
		make_access_error("SAM_Belpe", "en_mels");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_en_range_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_range", &result))
		make_access_error("SAM_Belpe", "en_range");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_en_wash_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_wash", &result))
		make_access_error("SAM_Belpe", "en_wash");
	});
	return result;
}



SAM_EXPORT float SAM_Belpe_LoadProfileEstimator_floor_area_fget(SAM_Belpe ptr, SAM_error *err){
	float result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "floor_area", &result))
		make_access_error("SAM_Belpe", "floor_area");
	});
	return result;
}



SAM_EXPORT float* SAM_Belpe_LoadProfileEstimator_load_aget(SAM_Belpe ptr, int* length, SAM_error *err){
	float* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "load", length);
	if (!result)
		make_access_error("SAM_Belpe", "load");
	});
	return result;
}



SAM_EXPORT const char* SAM_Belpe_LoadProfileEstimator_solar_resource_file_sget(SAM_Belpe ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "solar_resource_file");
	if (!result)
		make_access_error("SAM_Belpe", "solar_resource_file");
	});
	return result;
}




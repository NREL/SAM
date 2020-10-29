#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Pvwattsv51ts.h"

SAM_EXPORT int SAM_Pvwattsv51ts_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("pvwattsv5_1ts", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_alb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "alb", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_beam_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "beam", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_day_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "day", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_diffuse_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "diffuse", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_dry_temperature_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dry_temperature", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_elevation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "elevation", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hour", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_lat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "lat", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_lon_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "lon", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_minute_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "minute", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_month_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "month", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_poa_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "poa", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_pressure_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pressure", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_shaded_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "shaded_percent", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_tamb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tamb", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_tcell_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tcell", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_time_step_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_step", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_tz_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tz", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_wspd_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wspd", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_year_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "year", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_array_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "array_type", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_azimuth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "azimuth", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_dc_ac_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dc_ac_ratio", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_gcr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gcr", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_inv_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_eff", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_losses_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "losses", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_module_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "module_type", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_tilt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tilt", number);
	});
}

SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_alb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "alb", &result))
		make_access_error("SAM_Pvwattsv51ts", "alb");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_beam_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "beam", &result))
		make_access_error("SAM_Pvwattsv51ts", "beam");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_day_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "day", &result))
		make_access_error("SAM_Pvwattsv51ts", "day");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_diffuse_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "diffuse", &result))
		make_access_error("SAM_Pvwattsv51ts", "diffuse");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_dry_temperature_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dry_temperature", &result))
		make_access_error("SAM_Pvwattsv51ts", "dry_temperature");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_elevation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "elevation", &result))
		make_access_error("SAM_Pvwattsv51ts", "elevation");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hour", &result))
		make_access_error("SAM_Pvwattsv51ts", "hour");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_lat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lat", &result))
		make_access_error("SAM_Pvwattsv51ts", "lat");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_lon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lon", &result))
		make_access_error("SAM_Pvwattsv51ts", "lon");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_minute_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "minute", &result))
		make_access_error("SAM_Pvwattsv51ts", "minute");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_month_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "month", &result))
		make_access_error("SAM_Pvwattsv51ts", "month");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_poa_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "poa", &result))
		make_access_error("SAM_Pvwattsv51ts", "poa");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_pressure_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pressure", &result))
		make_access_error("SAM_Pvwattsv51ts", "pressure");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_shaded_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "shaded_percent", &result))
		make_access_error("SAM_Pvwattsv51ts", "shaded_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_tamb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tamb", &result))
		make_access_error("SAM_Pvwattsv51ts", "tamb");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_tcell_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tcell", &result))
		make_access_error("SAM_Pvwattsv51ts", "tcell");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_time_step_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_step", &result))
		make_access_error("SAM_Pvwattsv51ts", "time_step");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_tz_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tz", &result))
		make_access_error("SAM_Pvwattsv51ts", "tz");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_wspd_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wspd", &result))
		make_access_error("SAM_Pvwattsv51ts", "wspd");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_year_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "year", &result))
		make_access_error("SAM_Pvwattsv51ts", "year");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_array_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "array_type", &result))
		make_access_error("SAM_Pvwattsv51ts", "array_type");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_azimuth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "azimuth", &result))
		make_access_error("SAM_Pvwattsv51ts", "azimuth");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_dc_ac_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dc_ac_ratio", &result))
		make_access_error("SAM_Pvwattsv51ts", "dc_ac_ratio");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_gcr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gcr", &result))
		make_access_error("SAM_Pvwattsv51ts", "gcr");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_inv_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_eff", &result))
		make_access_error("SAM_Pvwattsv51ts", "inv_eff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_losses_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "losses", &result))
		make_access_error("SAM_Pvwattsv51ts", "losses");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_module_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "module_type", &result))
		make_access_error("SAM_Pvwattsv51ts", "module_type");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_Pvwattsv51ts", "system_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_tilt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tilt", &result))
		make_access_error("SAM_Pvwattsv51ts", "tilt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_Outputs_ac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ac", &result))
		make_access_error("SAM_Pvwattsv51ts", "ac");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv51ts_Outputs_dc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dc", &result))
		make_access_error("SAM_Pvwattsv51ts", "dc");
	});
	return result;
}




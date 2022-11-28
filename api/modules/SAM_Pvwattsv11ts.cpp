#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Pvwattsv11ts.h"

SAM_EXPORT int SAM_Pvwattsv11ts_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("pvwattsv1_1ts", data, verbosity, err);
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_azimuth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "azimuth", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_beam_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "beam", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_day_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "day", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_derate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "derate", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_diffuse_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "diffuse", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_elevation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "elevation", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_fd_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fd", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_gamma_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gamma", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hour", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_i_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "i_ref", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_inv_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_eff", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_lat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "lat", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_lon_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "lon", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_minute_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "minute", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_month_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "month", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_poa_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "poa", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_poa_cutin_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "poa_cutin", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_pressure_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pressure", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_rotlim_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rotlim", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_snow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snow", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_system_size_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_size", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_t_noct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "t_noct", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_t_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "t_ref", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_tamb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tamb", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_tcell_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tcell", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_tilt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tilt", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_time_step_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_step", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_track_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "track_mode", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_tz_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tz", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_w_stow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "w_stow", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_wspd_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wspd", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_year_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "year", number);
	});
}

SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_azimuth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "azimuth", &result))
		make_access_error("SAM_Pvwattsv11ts", "azimuth");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_beam_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "beam", &result))
		make_access_error("SAM_Pvwattsv11ts", "beam");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_day_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "day", &result))
		make_access_error("SAM_Pvwattsv11ts", "day");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_derate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "derate", &result))
		make_access_error("SAM_Pvwattsv11ts", "derate");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_diffuse_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "diffuse", &result))
		make_access_error("SAM_Pvwattsv11ts", "diffuse");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_elevation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "elevation", &result))
		make_access_error("SAM_Pvwattsv11ts", "elevation");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_fd_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fd", &result))
		make_access_error("SAM_Pvwattsv11ts", "fd");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_gamma_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gamma", &result))
		make_access_error("SAM_Pvwattsv11ts", "gamma");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hour", &result))
		make_access_error("SAM_Pvwattsv11ts", "hour");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_i_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "i_ref", &result))
		make_access_error("SAM_Pvwattsv11ts", "i_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_inv_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_eff", &result))
		make_access_error("SAM_Pvwattsv11ts", "inv_eff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_lat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lat", &result))
		make_access_error("SAM_Pvwattsv11ts", "lat");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_lon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lon", &result))
		make_access_error("SAM_Pvwattsv11ts", "lon");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_minute_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "minute", &result))
		make_access_error("SAM_Pvwattsv11ts", "minute");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_month_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "month", &result))
		make_access_error("SAM_Pvwattsv11ts", "month");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_poa_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "poa", &result))
		make_access_error("SAM_Pvwattsv11ts", "poa");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_poa_cutin_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "poa_cutin", &result))
		make_access_error("SAM_Pvwattsv11ts", "poa_cutin");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_pressure_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pressure", &result))
		make_access_error("SAM_Pvwattsv11ts", "pressure");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_rotlim_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rotlim", &result))
		make_access_error("SAM_Pvwattsv11ts", "rotlim");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_snow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snow", &result))
		make_access_error("SAM_Pvwattsv11ts", "snow");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_system_size_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_size", &result))
		make_access_error("SAM_Pvwattsv11ts", "system_size");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_t_noct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_noct", &result))
		make_access_error("SAM_Pvwattsv11ts", "t_noct");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_t_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_ref", &result))
		make_access_error("SAM_Pvwattsv11ts", "t_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_tamb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tamb", &result))
		make_access_error("SAM_Pvwattsv11ts", "tamb");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_tcell_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tcell", &result))
		make_access_error("SAM_Pvwattsv11ts", "tcell");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_tilt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tilt", &result))
		make_access_error("SAM_Pvwattsv11ts", "tilt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_time_step_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_step", &result))
		make_access_error("SAM_Pvwattsv11ts", "time_step");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_track_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "track_mode", &result))
		make_access_error("SAM_Pvwattsv11ts", "track_mode");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_tz_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tz", &result))
		make_access_error("SAM_Pvwattsv11ts", "tz");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_w_stow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "w_stow", &result))
		make_access_error("SAM_Pvwattsv11ts", "w_stow");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_wspd_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wspd", &result))
		make_access_error("SAM_Pvwattsv11ts", "wspd");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_year_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "year", &result))
		make_access_error("SAM_Pvwattsv11ts", "year");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_Outputs_ac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ac", &result))
		make_access_error("SAM_Pvwattsv11ts", "ac");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv11ts_Outputs_dc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dc", &result))
		make_access_error("SAM_Pvwattsv11ts", "dc");
	});
	return result;
}



